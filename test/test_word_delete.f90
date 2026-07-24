program test_word_delete
    ! alt-backspace across line breaks.
    !
    ! alt-backspace used to stop dead at column 1: there was no word left on
    ! the line, so it deleted nothing and a blank line could never be cleared
    ! with it. It now takes the line break instead, so one press eats one
    ! blank line.
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

    ! ==================== alt-backspace ====================

    ! --- The reported case: a blank line above the caret ---
    call setup('aa' // NL // '' // NL // 'bb')
    call cursor_at(3, 1)
    call key('alt-backspace')
    call check_text('aa' // NL // 'bb', 'blank line above is eaten')
    call check_cursor(2, 1, 'caret lands where the blank line was')

    ! --- At column 1 with text above, the line break goes ---
    call setup('aa' // NL // 'bb')
    call cursor_at(2, 1)
    call key('alt-backspace')
    call check_text('aabb', 'column 1 joins with the previous line')
    call check_cursor(1, 3, 'caret at the seam')

    ! --- Word, then the break, then the previous word ---
    call setup('aa' // NL // 'bb')
    call cursor_at(2, 3)
    call key('alt-backspace')
    call check_text('aa' // NL // '', 'first press takes the word')
    call key('alt-backspace')
    call check_text('aa', 'second press takes the line break')
    call key('alt-backspace')
    call check_text('', 'third press takes the word above')

    ! --- A run of blank lines goes one press at a time ---
    call setup('aa' // NL // '' // NL // '' // NL // 'bb')
    call cursor_at(4, 1)
    call key('alt-backspace')
    call key('alt-backspace')
    call check_text('aa' // NL // 'bb', 'two blank lines, two presses')

    ! --- Nothing to delete at the very start of the buffer ---
    call setup('aa')
    call cursor_at(1, 1)
    call key('alt-backspace')
    call check_text('aa', 'start of buffer is a no-op')

    ! --- Ordinary word deletion is unchanged ---
    call setup('foo bar')
    call cursor_at(1, 8)
    call key('alt-backspace')
    call check_text('foo ', 'word delete within a line still works')

    ! --- Indented line: the whole indent goes, then the break ---
    call setup('aa' // NL // '    ')
    call cursor_at(2, 5)
    call key('alt-backspace')
    call check_text('aa' // NL // '', 'indent is deleted as whitespace')
    call key('alt-backspace')
    call check_text('aa', 'then the now-empty line goes')

    ! --- Multiple cursors: the one below must follow the join ---
    ! Cursor 1 joins lines 1 and 2; cursor 2 then deletes the word 'ccc' from
    ! what is now line 2. That it acted on line 2 at all is the point -- it
    ! started on line 3 and had to be re-homed by the join.
    call setup('aa' // NL // 'bb' // NL // 'ccc ddd')
    call cursors_at(2, 1, 3, 5)
    call key('alt-backspace')
    call check_text('aabb' // NL // 'ddd', 'both cursors acted')
    call check_cursor_n(2, 2, 1, 'lower cursor followed the line join')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All word-delete tests passed'

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

end program test_word_delete
