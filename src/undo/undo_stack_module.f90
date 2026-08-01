!> Undo and redo, as a log of DIFFS.
!>
!> Each entry is one edit: where it happened, what was there, and what
!> replaced it. Undoing puts `removed` back where `inserted` is; redoing does
!> the reverse. Both directions are the same operation with the arguments
!> swapped, which is what makes redo a true inverse of undo rather than a
!> separate mechanism that has to be kept in step.
!>
!> It used to keep a hundred FULL copies of the buffer. That is what made a
!> deep history expensive on a large file, and it also meant an entry knew
!> what the document looked like but not what had CHANGED -- so there was
!> nothing to scroll to, and undoing an edit that had scrolled off screen
!> looked like nothing happening at all.
!>
!> Capturing the diff does not require instrumenting every mutation. One
!> baseline copy is held for the unit currently being edited, and when the
!> unit closes the baseline is compared with the buffer. Trim the common
!> prefix and suffix and what is left, by construction, is the change.
module undo_stack_module
    use iso_fortran_env, only: int64
    use text_buffer_module, only: buffer_t, buffer_insert, buffer_delete, &
                                  buffer_to_string
    use editor_state_module, only: cursor_t
    implicit none
    private

    public :: undo_stack_t, init_undo_stack, cleanup_undo_stack
    public :: perform_undo, perform_redo, can_undo, can_redo
    public :: undo_note_edit, undo_flush, undo_discard_open
    public :: undo_depth, undo_redo_depth
    public :: EDIT_INSERT, EDIT_DELETE, EDIT_STRUCTURAL

    integer, parameter :: MAX_UNDO_LEVELS = 500

    !> What kind of edit this is. A run of one kind is one undo; changing kind
    !> ends the run, so typing and then backspacing is two undos rather than
    !> one -- which is what it feels like it should be.
    integer, parameter :: EDIT_INSERT = 1
    integer, parameter :: EDIT_DELETE = 2
    integer, parameter :: EDIT_STRUCTURAL = 3   ! cut, paste, comment, ...

    !> A pause is a thought boundary. Long enough that ordinary typing stays
    !> one undo, short enough that coming back to a line starts a new one.
    integer(int64), parameter :: UNDO_IDLE_MS = 600_int64

    type :: undo_edit_t
        integer :: pos = 1                       !< byte position of the change
        character(len=:), allocatable :: removed
        character(len=:), allocatable :: inserted
        integer :: line_before = 1, col_before = 1
        integer :: line_after = 1, col_after = 1
    end type undo_edit_t

    type :: undo_stack_t
        type(undo_edit_t), allocatable :: edits(:)
        integer :: n = 0            !< entries recorded
        integer :: pos = 0          !< how many are currently applied
        ! The unit being edited right now, if any.
        logical :: open = .false.
        integer :: kind = 0
        integer(int64) :: last_ms = 0
        character(len=:), allocatable :: baseline
        integer :: base_line = 1, base_col = 1
    end type undo_stack_t

contains

    subroutine init_undo_stack(stack)
        type(undo_stack_t), intent(out) :: stack

        allocate(stack%edits(MAX_UNDO_LEVELS))
        stack%n = 0
        stack%pos = 0
        stack%open = .false.
        stack%kind = 0
        stack%last_ms = 0
    end subroutine init_undo_stack

    subroutine cleanup_undo_stack(stack)
        type(undo_stack_t), intent(inout) :: stack
        integer :: i

        if (allocated(stack%edits)) then
            do i = 1, size(stack%edits)
                if (allocated(stack%edits(i)%removed)) deallocate(stack%edits(i)%removed)
                if (allocated(stack%edits(i)%inserted)) deallocate(stack%edits(i)%inserted)
            end do
            deallocate(stack%edits)
        end if
        if (allocated(stack%baseline)) deallocate(stack%baseline)
        stack%n = 0
        stack%pos = 0
        stack%open = .false.
    end subroutine cleanup_undo_stack

    integer function undo_depth(stack)
        type(undo_stack_t), intent(in) :: stack
        undo_depth = stack%pos
    end function undo_depth

    integer function undo_redo_depth(stack)
        type(undo_stack_t), intent(in) :: stack
        undo_redo_depth = stack%n - stack%pos
    end function undo_redo_depth

    logical function can_undo(stack)
        type(undo_stack_t), intent(in) :: stack
        can_undo = stack%pos > 0 .or. stack%open
    end function can_undo

    logical function can_redo(stack)
        type(undo_stack_t), intent(in) :: stack
        can_redo = stack%n > stack%pos
    end function can_redo

    !> An edit of `kind` is about to happen. Decide whether it continues the
    !> run in progress or starts a new one.
    !>
    !> `gap_ms` is how long since the last edit; the caller owns the clock so
    !> this stays testable without one.
    subroutine undo_note_edit(stack, buffer, cursor, kind, now_ms, broke)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer, intent(in) :: kind
        integer(int64), intent(in) :: now_ms
        !> .true. when something outside this module already decided the run
        !> is over -- the caret moved, a different command ran.
        logical, intent(in) :: broke
        logical :: split

        split = .false.
        if (.not. stack%open) then
            split = .true.
        else if (broke) then
            split = .true.
        else if (kind /= stack%kind) then
            split = .true.
        else if (kind == EDIT_STRUCTURAL) then
            split = .true.              ! never merges, in either direction
        else if (now_ms - stack%last_ms >= UNDO_IDLE_MS) then
            split = .true.
        end if

        if (split) then
            call undo_flush(stack, buffer, cursor)
            call open_unit(stack, buffer, cursor, kind)
        end if
        stack%kind = kind
        stack%last_ms = now_ms
    end subroutine undo_note_edit

    !> Close the unit in progress, recording it if anything actually changed.
    subroutine undo_flush(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor
        character(len=:), allocatable :: now
        integer :: p, s, lo, ln

        if (.not. stack%open) return
        stack%open = .false.
        if (.not. allocated(stack%baseline)) return

        now = buffer_to_string(buffer)
        lo = len(stack%baseline)
        ln = len(now)

        ! Common prefix, then common suffix that does not run back into it.
        p = 0
        do while (p < min(lo, ln))
            if (stack%baseline(p + 1:p + 1) /= now(p + 1:p + 1)) exit
            p = p + 1
        end do
        s = 0
        do while (s < min(lo, ln) - p)
            if (stack%baseline(lo - s:lo - s) /= now(ln - s:ln - s)) exit
            s = s + 1
        end do

        if (lo - s < p .and. ln - s < p) return      ! nothing changed
        if (lo - s - p <= 0 .and. ln - s - p <= 0) return

        call push_edit(stack, p + 1, stack%baseline(p + 1:lo - s), &
                       now(p + 1:ln - s), cursor)
        if (allocated(stack%baseline)) deallocate(stack%baseline)
    end subroutine undo_flush

    !> Throw away the unit in progress without recording it. For a caller that
    !> has replaced the buffer wholesale and whose baseline is meaningless.
    subroutine undo_discard_open(stack)
        type(undo_stack_t), intent(inout) :: stack

        stack%open = .false.
        if (allocated(stack%baseline)) deallocate(stack%baseline)
    end subroutine undo_discard_open

    subroutine open_unit(stack, buffer, cursor, kind)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer, intent(in) :: kind

        stack%baseline = buffer_to_string(buffer)
        stack%base_line = cursor%line
        stack%base_col = cursor%column
        stack%kind = kind
        stack%open = .true.
    end subroutine open_unit

    subroutine push_edit(stack, pos, removed, inserted, cursor)
        type(undo_stack_t), intent(inout) :: stack
        integer, intent(in) :: pos
        character(len=*), intent(in) :: removed, inserted
        type(cursor_t), intent(in) :: cursor
        integer :: i

        ! Anything ahead of the pointer was undone and is now unreachable: a
        ! new edit is a new branch of history.
        do i = stack%pos + 1, stack%n
            if (allocated(stack%edits(i)%removed)) deallocate(stack%edits(i)%removed)
            if (allocated(stack%edits(i)%inserted)) deallocate(stack%edits(i)%inserted)
        end do
        stack%n = stack%pos

        if (stack%n >= MAX_UNDO_LEVELS) then
            ! Drop the oldest to make room.
            if (allocated(stack%edits(1)%removed)) deallocate(stack%edits(1)%removed)
            if (allocated(stack%edits(1)%inserted)) deallocate(stack%edits(1)%inserted)
            do i = 1, MAX_UNDO_LEVELS - 1
                call move_edit(stack%edits(i), stack%edits(i + 1))
            end do
            stack%n = MAX_UNDO_LEVELS - 1
            stack%pos = stack%n
        end if

        stack%n = stack%n + 1
        stack%pos = stack%n
        stack%edits(stack%n)%pos = pos
        stack%edits(stack%n)%removed = removed
        stack%edits(stack%n)%inserted = inserted
        stack%edits(stack%n)%line_before = stack%base_line
        stack%edits(stack%n)%col_before = stack%base_col
        stack%edits(stack%n)%line_after = cursor%line
        stack%edits(stack%n)%col_after = cursor%column
    end subroutine push_edit

    subroutine move_edit(dst, src)
        type(undo_edit_t), intent(inout) :: dst, src

        dst%pos = src%pos
        if (allocated(dst%removed)) deallocate(dst%removed)
        if (allocated(dst%inserted)) deallocate(dst%inserted)
        if (allocated(src%removed)) call move_alloc(src%removed, dst%removed)
        if (allocated(src%inserted)) call move_alloc(src%inserted, dst%inserted)
        dst%line_before = src%line_before
        dst%col_before = src%col_before
        dst%line_after = src%line_after
        dst%col_after = src%col_after
    end subroutine move_edit

    !> Replace `count` bytes at `pos` with `text`. The one operation both
    !> directions are built from.
    subroutine splice(buffer, pos, count, text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: pos, count
        character(len=*), intent(in) :: text

        if (count > 0) call buffer_delete(buffer, pos, count)
        if (len(text) > 0) call buffer_insert(buffer, pos, text)
        buffer%modified = .true.
    end subroutine splice

    subroutine perform_undo(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor

        ! Whatever is being typed right now counts as the most recent edit.
        call undo_flush(stack, buffer, cursor)
        if (stack%pos <= 0) return

        associate(e => stack%edits(stack%pos))
            call splice(buffer, e%pos, len(e%inserted), e%removed)
            ! The caret goes where the edit was, which is what gives the
            ! viewport something real to scroll to. Undoing a change that has
            ! scrolled off screen used to leave the caret where it was and so
            ! looked like nothing had happened.
            cursor%line = e%line_before
            cursor%column = e%col_before
        end associate
        cursor%desired_column = cursor%column
        cursor%has_selection = .false.
        stack%pos = stack%pos - 1
    end subroutine perform_undo

    subroutine perform_redo(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor

        call undo_flush(stack, buffer, cursor)
        if (stack%n <= stack%pos) return

        stack%pos = stack%pos + 1
        associate(e => stack%edits(stack%pos))
            call splice(buffer, e%pos, len(e%removed), e%inserted)
            cursor%line = e%line_after
            cursor%column = e%col_after
        end associate
        cursor%desired_column = cursor%column
        cursor%has_selection = .false.
    end subroutine perform_redo

end module undo_stack_module
