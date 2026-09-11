program test_remove_surrounding
    ! Structural unwrap is driven through the real command dispatcher. The
    ! terminal decoder names Ctrl+Alt+Backspace `alt-ctrl-backspace`, so that
    ! spelling is the primary path exercised here.
    use editor_state_module
    use text_buffer_module
    use command_handler_module, only: handle_key_command, init_command_handler
    implicit none

    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    logical :: quit
    integer :: nfail

    nfail = 0
    call init_command_handler()

    call setup('(paren)', 1, 3)
    call unwrap()
    call check_text('paren', 'parentheses unwrap')
    call check_cursor(1, 2, 'caret stays on the same interior character')

    call setup('[square]', 1, 4)
    call unwrap()
    call check_text('square', 'square brackets unwrap')

    call setup('{brace}', 1, 4)
    call unwrap()
    call check_text('brace', 'braces unwrap')

    call setup('"double"', 1, 4)
    call unwrap()
    call check_text('double', 'double quotes unwrap')

    call setup("'single'", 1, 4)
    call unwrap()
    call check_text('single', 'single quotes unwrap')

    call setup('`ticks`', 1, 4)
    call unwrap()
    call check_text('ticks', 'backticks unwrap')

    call setup('((core))', 1, 4)
    call unwrap()
    call check_text('(core)', 'first invocation removes the innermost pair')
    call check_cursor(1, 3, 'first nested unwrap preserves caret position')
    call unwrap()
    call check_text('core', 'second invocation removes the outer pair')
    call check_cursor(1, 2, 'second nested unwrap preserves caret position')

    call setup('(undo me)', 1, 3)
    call unwrap()
    call key('ctrl-z')
    call check_text('(undo me)', 'unwrap is one undoable structural edit')

    ! The [] pair is complete before the caret. It must be skipped so the
    ! enclosing parentheses are removed instead of pairing `[` with nothing.
    call setup('([x] + y)', 1, 8)
    call unwrap()
    call check_text('[x] + y', 'completed inner pair does not hide its outer pair')
    call check_cursor(1, 7, 'outer unwrap retains the logical caret')

    call setup('"a\"b"', 1, 5)
    call unwrap()
    call check_text('a\"b', 'escaped quote is not mistaken for a delimiter')
    call check_cursor(1, 4, 'escaped-quote unwrap retains the caret')

    call setup('"""block string"""', 1, 7)
    call unwrap()
    call check_text('"""block string"""', &
                    'triple-quoted strings are not partially unwrapped')

    call setup('(x)', 1, 1)
    call unwrap()
    call check_text('(x)', 'caret on opening delimiter is a no-op')
    call setup('(x)', 1, 3)
    call unwrap()
    call check_text('(x)', 'caret on closing delimiter is a no-op')

    call setup('(alpha' // new_line('a') // 'beta)', 2, 2)
    call unwrap()
    call check_text('alpha' // new_line('a') // 'beta', &
                    'brackets can unwrap across lines')
    call check_cursor_at(2, 2, 'multiline unwrap retains the caret')

    call setup('(caf' // achar(195) // achar(169) // ')', 1, 4)
    call unwrap()
    call check_text('caf' // achar(195) // achar(169), &
                    'UTF-8 content is not damaged')
    call check_cursor(1, 3, 'UTF-8 unwrap uses character columns')

    call setup('plain text', 1, 4)
    call unwrap()
    call check_text('plain text', 'no enclosing pair is a no-op')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All remove-surrounding tests passed'

contains

    subroutine setup(text, line, column)
        character(len=*), intent(in) :: text
        integer, intent(in) :: line, column

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_command_handler()
        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'remove_surrounding_test.c')
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(1))
        editor%cursors(1)%line = line
        editor%cursors(1)%column = column
        editor%cursors(1)%desired_column = column
        editor%active_cursor = 1
    end subroutine setup

    subroutine unwrap()
        call key('alt-ctrl-backspace')
    end subroutine unwrap

    subroutine key(name)
        character(len=*), intent(in) :: name

        quit = .false.
        call handle_key_command(name, editor, buffer, quit)
    end subroutine key

    subroutine check_text(expected, name)
        character(len=*), intent(in) :: expected, name
        character(len=:), allocatable :: got

        got = buffer_to_string(buffer)
        call check(got == expected, name, '"' // got // '"')
    end subroutine check_text

    subroutine check_cursor(line, column, name)
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: name
        call check_cursor_at(line, column, name)
    end subroutine check_cursor

    subroutine check_cursor_at(line, column, name)
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: name
        character(len=32) :: got

        write(got, '(i0,a,i0)') editor%cursors(1)%line, ':', &
                                editor%cursors(1)%column
        call check(editor%cursors(1)%line == line .and. &
                   editor%cursors(1)%column == column, name, trim(got))
    end subroutine check_cursor_at

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

end program test_remove_surrounding
