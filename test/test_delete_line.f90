program test_delete_line
    ! ctrl-shift-k (delete line).
    !
    ! ctrl-shift-k removes lines outright. Everything else that removes a line
    ! (ctrl-x, ctrl-k) captures the text first; this one must leave both the
    ! clipboard and the yank stack alone.
    use editor_state_module
    use text_buffer_module
    use command_handler_module, only: handle_key_command, init_command_handler
    implicit none

    character(len=1), parameter :: NL = char(10)
    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    logical :: quit
    integer :: nfail

    nfail = 0
    call init_command_handler()

    ! ==================== ctrl-shift-k ====================

    call setup('aa' // NL // 'bb' // NL // 'cc')
    call cursor_at(2, 1)
    call key('ctrl-shift-k')
    call check_text('aa' // NL // 'cc', 'deletes the caret line')
    call check_cursor(2, 1, 'caret moves onto the following line')

    call setup('aa' // NL // 'bb')
    call cursor_at(1, 1)
    call key('ctrl-shift-k')
    call check_text('bb', 'deletes the first line')

    call setup('aa' // NL // 'bb')
    call cursor_at(2, 1)
    call key('ctrl-shift-k')
    call check_text('aa', 'deletes the last line without leaving a blank')

    call setup('aa' // NL // 'bb' // NL // 'cc')
    call cursor_at(1, 1)
    call key('ctrl-shift-k')
    call key('ctrl-shift-k')
    call check_text('cc', 'repeated presses walk down the file')

    call setup('only')
    call cursor_at(1, 1)
    call key('ctrl-shift-k')
    call check_text('', 'deleting the only line empties the buffer')

    ! --- A selection spans whole lines, column-1 end excluded ---
    call setup('aa' // NL // 'bb' // NL // 'cc' // NL // 'dd')
    call select_range(1, 1, 2, 3)
    call key('ctrl-shift-k')
    call check_text('cc' // NL // 'dd', 'selection deletes every line it touches')

    call setup('aa' // NL // 'bb' // NL // 'cc')
    call select_range(1, 1, 2, 1)
    call key('ctrl-shift-k')
    call check_text('bb' // NL // 'cc', 'a selection ending at column 1 stops short')

    ! --- Multiple cursors delete each of their lines ---
    call setup('aa' // NL // 'bb' // NL // 'cc' // NL // 'dd')
    call cursors_at(1, 1, 3, 1)
    call key('ctrl-shift-k')
    call check_text('bb' // NL // 'dd', 'each cursor deletes its own line')

    ! --- Undo brings it back ---
    call setup('aa' // NL // 'bb' // NL // 'cc')
    call cursor_at(2, 1)
    call key('ctrl-shift-k')
    call key('ctrl-z')
    call check_text('aa' // NL // 'bb' // NL // 'cc', 'undo restores the line')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All delete-line tests passed'

contains

    subroutine setup(text)
        character(len=*), intent(in) :: text

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_command_handler()
        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'line_edit_test.c')
    end subroutine setup

    subroutine cursor_at(l, c)
        integer, intent(in) :: l, c

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(1))
        editor%cursors(1)%line = l
        editor%cursors(1)%column = c
        editor%active_cursor = 1
    end subroutine cursor_at

    subroutine cursors_at(l1, c1, l2, c2)
        integer, intent(in) :: l1, c1, l2, c2

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(2))
        editor%cursors(1)%line = l1
        editor%cursors(1)%column = c1
        editor%cursors(2)%line = l2
        editor%cursors(2)%column = c2
        editor%active_cursor = 1
    end subroutine cursors_at

    subroutine select_range(al, ac, cl, cc)
        integer, intent(in) :: al, ac, cl, cc

        call cursor_at(cl, cc)
        editor%cursors(1)%has_selection = .true.
        editor%cursors(1)%selection_start_line = al
        editor%cursors(1)%selection_start_col = ac
    end subroutine select_range

    subroutine key(k)
        character(len=*), intent(in) :: k

        quit = .false.
        call handle_key_command(k, editor, buffer, quit)
    end subroutine key

    subroutine check_text(expected, name)
        character(len=*), intent(in) :: expected, name
        character(len=:), allocatable :: got

        got = buffer_to_string(buffer)
        call check(got == expected, name, '"' // got // '"')
    end subroutine check_text

    subroutine check_cursor(l, c, name)
        integer, intent(in) :: l, c
        character(len=*), intent(in) :: name

        call check_cursor_n(1, l, c, name)
    end subroutine check_cursor

    subroutine check_cursor_n(idx, l, c, name)
        integer, intent(in) :: idx, l, c
        character(len=*), intent(in) :: name
        character(len=64) :: got

        if (idx > size(editor%cursors)) then
            call check(.false., name, 'cursor index out of range')
            return
        end if
        write(got, '(a,i0,a,i0,a)') '(', editor%cursors(idx)%line, ',', &
            editor%cursors(idx)%column, ')'
        call check(editor%cursors(idx)%line == l .and. &
                   editor%cursors(idx)%column == c, name, trim(got))
    end subroutine check_cursor_n

    subroutine check(ok, name, got)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name, got

        if (ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: ' // trim(got) // ')'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_delete_line
