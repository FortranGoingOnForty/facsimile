program test_completion_cache
    ! The completion cache exists to stop the editor paying ~300 ms of GPU
    ! twice for the same question. Backspace is a trigger key, so deleting
    ! four characters and retyping them asks five questions of which four
    ! were already answered.
    !
    ! What matters here is not that it stores things -- it is that it never
    ! serves an answer computed for a *different* question. Every test below
    ! is really asking one of two things: does a genuine repeat hit, and does
    ! anything that changes the answer miss?
    use iso_fortran_env, only: int64
    use completion_cache_module
    implicit none

    integer :: nfail

    nfail = 0

    call test_miss_on_empty()
    call test_round_trip()
    call test_key_separates_questions()
    call test_negative_cache()
    call test_negative_expires()
    call test_lru_eviction()
    call test_reuses_slot_for_same_key()
    call test_clear()
    call test_counters()
    call test_oversize_not_stored()
    call test_sanitized_text_survives_verbatim()

    if (nfail == 0) then
        print '(a)', 'test_completion_cache: all passed'
    else
        print '(a,i0,a)', 'test_completion_cache: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine check(cond, label)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label

        if (.not. cond) then
            print '(a)', '  FAIL: ' // label
            nfail = nfail + 1
        end if
    end subroutine check

    subroutine test_miss_on_empty()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad

        call cache_lookup(c, cache_key_of('m', 'pre', 'suf', 24), 1000_int64, text, hit, bad)
        call check(.not. hit, 'empty cache does not hit')
        call check(.not. bad, 'empty cache reports nothing known-bad')
        call check(len(text) == 0, 'miss yields empty text')
    end subroutine test_miss_on_empty

    subroutine test_round_trip()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('qwen', 'int add(int a, int b) {', '}', 24)
        call cache_store(c, k, 1000_int64, 'return a + b;')
        call cache_lookup(c, k, 1010_int64, text, hit, bad)
        call check(hit, 'a stored completion is found again')
        call check(.not. bad, 'a stored completion is not known-bad')
        call check(text == 'return a + b;', 'the exact text comes back')
    end subroutine test_round_trip

    ! The whole safety argument for this cache is that the key covers
    ! everything that decides the answer. If any of these collided, the user
    ! would be shown a suggestion computed for other code.
    subroutine test_key_separates_questions()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: base

        base = cache_key_of('qwen', 'prefix', 'suffix', 24)
        call cache_store(c, base, 1000_int64, 'answer')

        call cache_lookup(c, cache_key_of('other', 'prefix', 'suffix', 24), &
                          1000_int64, text, hit, bad)
        call check(.not. hit, 'a different model is a different question')

        call cache_lookup(c, cache_key_of('qwen', 'prefiy', 'suffix', 24), &
                          1000_int64, text, hit, bad)
        call check(.not. hit, 'a different prefix is a different question')

        call cache_lookup(c, cache_key_of('qwen', 'prefix', 'suffiy', 24), &
                          1000_int64, text, hit, bad)
        call check(.not. hit, 'a different suffix is a different question')

        call cache_lookup(c, cache_key_of('qwen', 'prefix', 'suffix', 96), &
                          1000_int64, text, hit, bad)
        call check(.not. hit, 'a different token budget is a different question')

        ! The suffix and prefix must not be interchangeable -- code before the
        ! caret and code after it are emphatically not the same prompt.
        call check(cache_key_of('m', 'ab', 'cd', 8) /= cache_key_of('m', 'cd', 'ab', 8), &
                   'prefix and suffix are not interchangeable')

        ! ...nor may concatenation collide across the boundary.
        call check(cache_key_of('m', 'ab', 'c', 8) /= cache_key_of('m', 'a', 'bc', 8), &
                   'the prefix/suffix split is part of the key')

        call cache_lookup(c, base, 1000_int64, text, hit, bad)
        call check(hit .and. text == 'answer', 'the original entry is undisturbed')
    end subroutine test_key_separates_questions

    subroutine test_negative_cache()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('qwen', 'p', 's', 24)
        call cache_store_rejection(c, k, 1000_int64)
        call cache_lookup(c, k, 1100_int64, text, hit, bad)
        call check(bad, 'a rejected prompt is remembered as bad')
        call check(.not. hit, 'known-bad is not reported as a usable hit')
        call check(len(text) == 0, 'known-bad yields no text')
    end subroutine test_negative_cache

    ! Sampling is not deterministic, so a prompt that produced garbage once
    ! may not the next time. Remembering the failure forever would silently
    ! blind the feature at that spot for the rest of the session.
    subroutine test_negative_expires()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('qwen', 'p', 's', 24)
        call cache_store_rejection(c, k, 1000_int64)

        call cache_lookup(c, k, 20000_int64, text, hit, bad)
        call check(bad, 'still known-bad well inside the TTL')

        call cache_lookup(c, k, 1000_int64 + 30001_int64, text, hit, bad)
        call check(.not. bad, 'the rejection expires and we are willing to ask again')
        call check(.not. hit, 'an expired rejection is not a hit either')
    end subroutine test_negative_expires

    ! Bounded, or a long session in a large file grows without limit.
    subroutine test_lru_eviction()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer :: i
        integer(int64) :: keep

        keep = cache_key_of('m', 'keep', 's', 24)
        call cache_store(c, keep, 1_int64, 'kept')

        ! Touch it so it is the most recently *used*, not the oldest stored.
        call cache_lookup(c, keep, 5_int64, text, hit, bad)
        call check(hit, 'the entry to keep is present before the flood')

        do i = 1, 200
            call cache_store(c, cache_key_of('m', 'filler' // int_str(i), 's', 24), &
                             int(100 + i, int64), 'x')
        end do

        call cache_lookup(c, keep, 9000_int64, text, hit, bad)
        call check(.not. hit, 'the cache is bounded -- 200 entries do not all fit')

        ! The most recent fillers must still be there, or eviction is evicting
        ! the wrong end and the cache would never hit on real usage.
        call cache_lookup(c, cache_key_of('m', 'filler200', 's', 24), 9000_int64, text, hit, bad)
        call check(hit, 'the most recent entry survives eviction')
    end subroutine test_lru_eviction

    ! Re-answering the same prompt must overwrite, not consume a second slot.
    subroutine test_reuses_slot_for_same_key()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k
        integer :: i

        k = cache_key_of('m', 'p', 's', 24)
        do i = 1, 500
            call cache_store(c, k, int(i, int64), 'v' // int_str(i))
        end do

        call cache_lookup(c, k, 1000_int64, text, hit, bad)
        call check(hit, 'repeated stores of one key still hit')
        call check(text == 'v500', 'the newest value wins')

        ! Nothing else was displaced: 63 slots must still be free.
        do i = 1, 63
            call cache_store(c, cache_key_of('m', 'x' // int_str(i), 's', 24), &
                             1000_int64, 'y')
        end do
        call cache_lookup(c, k, 1000_int64, text, hit, bad)
        call check(hit, 'one key occupied exactly one slot')
    end subroutine test_reuses_slot_for_same_key

    ! A rejection must be able to overwrite a previous success for the same
    ! key, and vice versa -- otherwise a stale answer outlives its refutation.
    subroutine test_clear()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('m', 'p', 's', 24)
        call cache_store(c, k, 1000_int64, 'v')
        call cache_store_rejection(c, k, 1000_int64)
        call cache_lookup(c, k, 1000_int64, text, hit, bad)
        call check(bad .and. .not. hit, 'a rejection replaces an earlier success')

        call cache_store(c, k, 2000_int64, 'w')
        call cache_lookup(c, k, 2000_int64, text, hit, bad)
        call check(hit .and. text == 'w', 'a success replaces an earlier rejection')

        call cache_clear(c)
        call cache_lookup(c, k, 2000_int64, text, hit, bad)
        call check(.not. hit .and. .not. bad, 'clear empties the cache')
        call check(c%hits > 0, 'clear keeps the session counters')
    end subroutine test_clear

    ! The counters are the only evidence the cache is worth its complexity,
    ! so they have to be right.
    subroutine test_counters()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('m', 'p', 's', 24)
        call check(cache_hit_rate_percent(c) == 0, 'no traffic reports 0%, not a divide by zero')

        call cache_lookup(c, k, 1_int64, text, hit, bad)      ! miss
        call cache_store(c, k, 1_int64, 'v')
        call cache_lookup(c, k, 2_int64, text, hit, bad)      ! hit
        call cache_lookup(c, k, 3_int64, text, hit, bad)      ! hit
        call cache_lookup(c, k, 4_int64, text, hit, bad)      ! hit

        call check(c%hits == 3 .and. c%misses == 1, 'hits and misses are counted separately')
        call check(c%saved_requests == 3, 'three requests were not sent')
        call check(cache_hit_rate_percent(c) == 75, 'hit rate is hits over total')

        ! A known-bad answer also saves a request, and must count as one.
        call cache_store_rejection(c, cache_key_of('m', 'q', 's', 24), 5_int64)
        call cache_lookup(c, cache_key_of('m', 'q', 's', 24), 6_int64, text, hit, bad)
        call check(bad .and. c%saved_requests == 4, 'a suppressed retry counts as saved')
    end subroutine test_counters

    ! A pathological reply should not be pinned in memory for the session.
    subroutine test_oversize_not_stored()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text
        logical :: hit, bad
        integer(int64) :: k

        k = cache_key_of('m', 'p', 's', 24)
        call cache_store(c, k, 1_int64, repeat('x', 4096))
        call cache_lookup(c, k, 2_int64, text, hit, bad)
        call check(.not. hit, 'an oversized completion is not cached')

        call cache_store(c, k, 1_int64, '')
        call cache_lookup(c, k, 2_int64, text, hit, bad)
        call check(.not. hit, 'an empty completion is not cached')
    end subroutine test_oversize_not_stored

    ! The stored value skips the sanitizer on the way back out, so it must
    ! come back byte-identical -- including the newlines of a block.
    subroutine test_sanitized_text_survives_verbatim()
        type(completion_cache_t) :: c
        character(len=:), allocatable :: text, block
        logical :: hit, bad
        integer(int64) :: k

        block = 'if (n < 0) {' // achar(10) // '    return -1;' // achar(10) // '}'
        k = cache_key_of('m', 'p', 's', 96)
        call cache_store(c, k, 1_int64, block)
        call cache_lookup(c, k, 2_int64, text, hit, bad)
        call check(hit, 'a multi-line block caches')
        call check(text == block, 'a multi-line block comes back byte-identical')
        call check(len(text) == len(block), 'no trailing-blank padding on the way out')
    end subroutine test_sanitized_text_survives_verbatim

    function int_str(v) result(t)
        integer, intent(in) :: v
        character(len=:), allocatable :: t
        character(len=16) :: b

        write(b, '(i0)') v
        t = trim(b)
    end function int_str

end program test_completion_cache
