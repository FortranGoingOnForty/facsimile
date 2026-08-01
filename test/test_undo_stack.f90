program test_undo_stack
    ! The arithmetic under undo. Every entry is a diff -- a position, the bytes
    ! that were there, and the bytes that replaced them -- so a wrong diff does
    ! not merely misbehave, it loses text. That is worth checking directly
    ! rather than only through a terminal.
    !
    ! The property that matters: applying an edit and then inverting it must
    ! give back the ORIGINAL BYTES, exactly. Everything else here is a case
    ! that could plausibly break it.
    use iso_fortran_env, only: int64
    use text_buffer_module
    use editor_state_module, only: cursor_t
    use undo_stack_module
    implicit none

    integer :: nfail
    nfail = 0

    call round_trip('insert in the middle', 'abcdef', 3, 0, 'XY')
    call round_trip('insert at the start',  'abcdef', 1, 0, 'XY')
    call round_trip('insert at the very end', 'abcdef', 7, 0, 'XY')
    call round_trip('delete in the middle', 'abcdef', 3, 2, '')
    call round_trip('delete to the end',    'abcdef', 4, 3, '')
    call round_trip('delete everything',    'abcdef', 1, 6, '')
    call round_trip('replace a run',        'abcdef', 2, 3, 'ZZZZ')
    call round_trip('across a newline',     'aa'//nl()//'bb'//nl()//'cc', 3, 3, 'X')
    call round_trip('a whole line',         'aa'//nl()//'bb'//nl()//'cc', 4, 3, '')
    ! Multi-byte: the diff works in BYTES, so a change inside a character is
    ! still applied correctly even though the trimmed span is not a character
    ! boundary. That is exactly why positions here never meet cursor columns.
    call round_trip('utf-8 replaced',       'a'//char(195)//char(169)//'b', 2, 2, 'e')
    call round_trip('utf-8 inserted',       'abc', 2, 0, char(195)//char(169))
    call round_trip('no change at all',     'abcdef', 3, 0, '')

    call test_redo_steps_one_at_a_time()
    call test_a_new_edit_drops_the_redo_tail()
    call test_undo_lands_on_the_edit()

    if (nfail == 0) then
        print *, 'test_undo_stack: all passed'
    else
        print *, 'test_undo_stack: FAILED', nfail
        stop 1
    end if

contains

    function nl() result(c)
        character(len=1) :: c
        c = char(10)
    end function nl

    subroutine check(ok, name)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name

        if (ok) then
            print *, 'ok   ', name
        else
            print *, 'FAIL ', name
            nfail = nfail + 1
        end if
    end subroutine check

    subroutine fresh(buffer, stack, cursor, text)
        type(buffer_t), intent(out) :: buffer
        type(undo_stack_t), intent(out) :: stack
        type(cursor_t), intent(out) :: cursor
        character(len=*), intent(in) :: text

        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        call init_undo_stack(stack)
        cursor%line = 1
        cursor%column = 1
        cursor%has_selection = .false.
    end subroutine fresh

    !> Make one edit, record it, undo it, redo it, and check the bytes at
    !> every step.
    subroutine round_trip(name, start_text, pos, del, ins)
        character(len=*), intent(in) :: name, start_text, ins
        integer, intent(in) :: pos, del
        type(buffer_t) :: buffer
        type(undo_stack_t) :: stack
        type(cursor_t) :: cursor
        character(len=:), allocatable :: before, after, undone, redone

        call fresh(buffer, stack, cursor, start_text)
        before = buffer_to_string(buffer)

        call undo_note_edit(stack, buffer, cursor, EDIT_STRUCTURAL, 0_int64, .true.)
        if (del > 0) call buffer_delete(buffer, pos, del)
        if (len(ins) > 0) call buffer_insert(buffer, pos, ins)
        after = buffer_to_string(buffer)
        call undo_flush(stack, buffer, cursor)

        call perform_undo(stack, buffer, cursor)
        undone = buffer_to_string(buffer)
        call check(undone == before, name // ': undo restores the original bytes')

        call perform_redo(stack, buffer, cursor)
        redone = buffer_to_string(buffer)
        call check(redone == after, name // ': redo restores the edited bytes')

        call cleanup_undo_stack(stack)
        call cleanup_buffer(buffer)
    end subroutine round_trip

    !> One press, one entry. Redo used to jump straight to the newest state,
    !> so three undos were followed by a single redo that put all three back.
    subroutine test_redo_steps_one_at_a_time()
        type(buffer_t) :: buffer
        type(undo_stack_t) :: stack
        type(cursor_t) :: cursor
        integer :: i

        call fresh(buffer, stack, cursor, 'abc')
        do i = 1, 3
            call undo_note_edit(stack, buffer, cursor, EDIT_STRUCTURAL, 0_int64, .true.)
            call buffer_insert(buffer, buffer_size_of(buffer) + 1, char(48 + i))
            call undo_flush(stack, buffer, cursor)
        end do
        call check(buffer_to_string(buffer) == 'abc123', 'three edits applied')

        call perform_undo(stack, buffer, cursor)
        call perform_undo(stack, buffer, cursor)
        call perform_undo(stack, buffer, cursor)
        call check(buffer_to_string(buffer) == 'abc', 'three undos peel them all off')

        call perform_redo(stack, buffer, cursor)
        call check(buffer_to_string(buffer) == 'abc1', 'the first redo restores ONE')
        call perform_redo(stack, buffer, cursor)
        call check(buffer_to_string(buffer) == 'abc12', 'the second restores the next')
        call perform_redo(stack, buffer, cursor)
        call check(buffer_to_string(buffer) == 'abc123', 'and the third the last')
        call perform_redo(stack, buffer, cursor)
        call check(buffer_to_string(buffer) == 'abc123', 'a fourth does nothing')

        call cleanup_undo_stack(stack)
        call cleanup_buffer(buffer)
    end subroutine test_redo_steps_one_at_a_time

    subroutine test_a_new_edit_drops_the_redo_tail()
        type(buffer_t) :: buffer
        type(undo_stack_t) :: stack
        type(cursor_t) :: cursor

        call fresh(buffer, stack, cursor, 'abc')
        call undo_note_edit(stack, buffer, cursor, EDIT_STRUCTURAL, 0_int64, .true.)
        call buffer_insert(buffer, 4, 'X')
        call undo_flush(stack, buffer, cursor)
        call perform_undo(stack, buffer, cursor)
        call check(can_redo(stack), 'there is something to redo')

        call undo_note_edit(stack, buffer, cursor, EDIT_STRUCTURAL, 0_int64, .true.)
        call buffer_insert(buffer, 4, 'Y')
        call undo_flush(stack, buffer, cursor)
        call check(.not. can_redo(stack), 'a new edit makes the undone branch unreachable')
        call check(buffer_to_string(buffer) == 'abcY', 'and the new edit stands')

        call cleanup_undo_stack(stack)
        call cleanup_buffer(buffer)
    end subroutine test_a_new_edit_drops_the_redo_tail

    !> The caret must land where the edit was, or the viewport has nothing to
    !> scroll to and an off-screen undo looks like nothing happening.
    subroutine test_undo_lands_on_the_edit()
        type(buffer_t) :: buffer
        type(undo_stack_t) :: stack
        type(cursor_t) :: cursor

        call fresh(buffer, stack, cursor, 'aaa'//nl()//'bbb'//nl()//'ccc')
        cursor%line = 1
        cursor%column = 4
        call undo_note_edit(stack, buffer, cursor, EDIT_STRUCTURAL, 0_int64, .true.)
        call buffer_insert(buffer, 4, 'X')
        cursor%column = 5
        call undo_flush(stack, buffer, cursor)

        ! Wander far away, as scrolling would.
        cursor%line = 3
        cursor%column = 1
        call perform_undo(stack, buffer, cursor)
        call check(cursor%line == 1, 'undo brings the caret back to the edit line')
        call check(cursor%column == 4, 'and to the column it was made at')

        call cleanup_undo_stack(stack)
        call cleanup_buffer(buffer)
    end subroutine test_undo_lands_on_the_edit

    integer function buffer_size_of(buffer)
        type(buffer_t), intent(in) :: buffer
        buffer_size_of = len(buffer_to_string(buffer))
    end function buffer_size_of

end program test_undo_stack
