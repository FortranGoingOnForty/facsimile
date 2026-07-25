! Remember completions so the same question is not asked twice.
!
! This exists because backspace is a trigger key. Deleting four characters
! fires four requests -- roughly 1.2 s of GPU -- every one of which is thrown
! away, and then a fifth fires when the user retypes what they just deleted.
! The answer to that fifth request was computed 400 ms earlier and discarded.
!
! Two caches, because a rejection is as worth remembering as a success:
!
!   positive  keyed on the prompt window, holding the SANITIZED text, so a
!             hit skips the entire validation pipeline as well as the request
!   negative  prompt hashes that produced a rejection, so the token bucket is
!             not burned repeatedly asking a question the model keeps
!             answering badly
!
! Entries are keyed on the prompt itself rather than on cursor position: the
! same prefix and suffix deserve the same answer wherever the caret happens to
! be, and position-keyed entries would miss on exactly the retype case this is
! for.
module completion_cache_module
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: completion_cache_t
    public :: cache_lookup, cache_store, cache_store_rejection, cache_clear
    public :: cache_hit_rate_percent, cache_key_of

    integer, parameter :: CACHE_SLOTS = 64
    integer, parameter :: MAX_ENTRY_BYTES = 2048
    ! A rejection is remembered for a while, but not forever: models are
    ! sampled, so an identical prompt can legitimately do better later.
    integer, parameter :: NEGATIVE_TTL_MS = 30000

    type :: cache_entry_t
        integer(int64) :: key = 0
        logical :: occupied = .false.
        logical :: rejected = .false.      ! a remembered failure, not a result
        integer(int64) :: stored_ms = 0
        integer(int64) :: used_ms = 0      ! for LRU eviction
        character(len=:), allocatable :: text
    end type cache_entry_t

    type :: completion_cache_t
        type(cache_entry_t) :: slots(CACHE_SLOTS)
        integer :: hits = 0
        integer :: misses = 0
        integer :: saved_requests = 0
    end type completion_cache_t

contains

    ! FNV-1a over the material that decides the answer. Anything that would
    ! change the completion must be in here, or the cache will serve a result
    ! computed for a different question.
    function cache_key_of(model, prefix, suffix, num_predict) result(key)
        character(len=*), intent(in) :: model, prefix, suffix
        integer, intent(in) :: num_predict
        integer(int64) :: key

        ! The FNV-1a 64-bit offset basis is 14695981039346656037, which does
        ! not fit in a signed int64. Fortran has no unsigned integers, so use
        ! the two's-complement equivalent -- identical bit pattern, and the
        ! wraparound arithmetic below behaves the same either way.
        key = -3750763034362895579_int64
        ! Each field's length is mixed in before the field itself. Without
        ! that the fields stream together and ('ab','c') hashes the same as
        ! ('a','bc') -- the same document with the caret somewhere else, which
        ! is an entirely different question with an entirely different answer.
        call fnv_mix_int(key, len(model))
        call fnv_mix(key, model)
        call fnv_mix_int(key, len(prefix))
        call fnv_mix(key, prefix)
        call fnv_mix_int(key, len(suffix))
        call fnv_mix(key, suffix)
        call fnv_mix_int(key, num_predict)
    end function cache_key_of

    subroutine fnv_mix(h, s)
        integer(int64), intent(inout) :: h
        character(len=*), intent(in) :: s
        integer :: i

        do i = 1, len(s)
            h = ieor(h, int(iachar(s(i:i)), int64))
            h = h * 1099511628211_int64
        end do
    end subroutine fnv_mix

    subroutine fnv_mix_int(h, v)
        integer(int64), intent(inout) :: h
        integer, intent(in) :: v

        h = ieor(h, int(v, int64))
        h = h * 1099511628211_int64
    end subroutine fnv_mix_int

    ! found  : a usable completion was cached (text is set)
    ! known_bad : this prompt was rejected recently; do not ask again
    subroutine cache_lookup(cache, key, now_ms, text, found, known_bad)
        type(completion_cache_t), intent(inout) :: cache
        integer(int64), intent(in) :: key, now_ms
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found, known_bad
        integer :: i

        text = ''
        found = .false.
        known_bad = .false.

        do i = 1, CACHE_SLOTS
            if (.not. cache%slots(i)%occupied) cycle
            if (cache%slots(i)%key /= key) cycle

            if (cache%slots(i)%rejected) then
                if (now_ms - cache%slots(i)%stored_ms > int(NEGATIVE_TTL_MS, int64)) then
                    cache%slots(i)%occupied = .false.   ! expired; ask again
                    exit
                end if
                known_bad = .true.
                cache%slots(i)%used_ms = now_ms
                cache%hits = cache%hits + 1
                cache%saved_requests = cache%saved_requests + 1
                return
            end if

            text = cache%slots(i)%text
            found = .true.
            cache%slots(i)%used_ms = now_ms
            cache%hits = cache%hits + 1
            cache%saved_requests = cache%saved_requests + 1
            return
        end do

        cache%misses = cache%misses + 1
    end subroutine cache_lookup

    subroutine cache_store(cache, key, now_ms, text)
        type(completion_cache_t), intent(inout) :: cache
        integer(int64), intent(in) :: key, now_ms
        character(len=*), intent(in) :: text
        integer :: slot

        if (len(text) == 0 .or. len(text) > MAX_ENTRY_BYTES) return
        slot = slot_for(cache, key)
        cache%slots(slot)%key = key
        cache%slots(slot)%occupied = .true.
        cache%slots(slot)%rejected = .false.
        cache%slots(slot)%stored_ms = now_ms
        cache%slots(slot)%used_ms = now_ms
        cache%slots(slot)%text = text
    end subroutine cache_store

    subroutine cache_store_rejection(cache, key, now_ms)
        type(completion_cache_t), intent(inout) :: cache
        integer(int64), intent(in) :: key, now_ms
        integer :: slot

        slot = slot_for(cache, key)
        cache%slots(slot)%key = key
        cache%slots(slot)%occupied = .true.
        cache%slots(slot)%rejected = .true.
        cache%slots(slot)%stored_ms = now_ms
        cache%slots(slot)%used_ms = now_ms
        if (allocated(cache%slots(slot)%text)) deallocate(cache%slots(slot)%text)
    end subroutine cache_store_rejection

    ! Reuse the same key, else a free slot, else the least recently used.
    function slot_for(cache, key) result(slot)
        type(completion_cache_t), intent(in) :: cache
        integer(int64), intent(in) :: key
        integer :: slot, i
        integer(int64) :: oldest

        do i = 1, CACHE_SLOTS
            if (cache%slots(i)%occupied .and. cache%slots(i)%key == key) then
                slot = i
                return
            end if
        end do

        do i = 1, CACHE_SLOTS
            if (.not. cache%slots(i)%occupied) then
                slot = i
                return
            end if
        end do

        slot = 1
        oldest = cache%slots(1)%used_ms
        do i = 2, CACHE_SLOTS
            if (cache%slots(i)%used_ms < oldest) then
                oldest = cache%slots(i)%used_ms
                slot = i
            end if
        end do
    end function slot_for

    ! Called on anything that invalidates the premise: a different file, a
    ! different model, an undo. Counters survive so the status line can still
    ! report how the session has gone.
    subroutine cache_clear(cache)
        type(completion_cache_t), intent(inout) :: cache
        integer :: i

        do i = 1, CACHE_SLOTS
            cache%slots(i)%occupied = .false.
            cache%slots(i)%rejected = .false.
            cache%slots(i)%key = 0
            if (allocated(cache%slots(i)%text)) deallocate(cache%slots(i)%text)
        end do
    end subroutine cache_clear

    function cache_hit_rate_percent(cache) result(pct)
        type(completion_cache_t), intent(in) :: cache
        integer :: pct, total

        total = cache%hits + cache%misses
        if (total <= 0) then
            pct = 0
        else
            pct = (cache%hits * 100) / total
        end if
    end function cache_hit_rate_percent

end module completion_cache_module
