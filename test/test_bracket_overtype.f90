program test_bracket_overtype
    ! Typing the closing bracket that auto-close already inserted should step
    ! over it instead of leaving a duplicate behind ("f(a))" from muscle
    ! memory). Only closers auto-close put there are stepped over -- a ')'
    ! already in the text must still take a typed ')' as a new character,
    ! otherwise the feature eats input.
    !
    ! Tests drive handle_key_command, the same path real keystrokes take.
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

    ! --- The reported annoyance ---
    call setup('')
    call typed('f(a)')
    call check_text('f(a)', 'typed closer steps over the auto-closed one')
    call check_cursor(1, 5, 'caret ends past the pair')

    ! --- Nesting, closed by hand all the way out ---
    call setup('')
    call typed('f((a))')
    call check_text('f((a))', 'nested pairs close without duplicates')

    call setup('')
    call typed('f([a])')
    call check_text('f([a])', 'mixed bracket kinds')

    call setup('')
    call typed('((()))')
    call check_text('((()))', 'three deep, every closer typed')

    ! --- A quote is both opener and closer; the second one must close ---
    call setup('')
    call typed('"hi"')
    call check_text('"hi"', 'quotes overtype rather than opening a new pair')

    call setup('')
    call typed('x = ''a''')
    call check_text('x = ''a''', 'single quotes too')

    ! --- Closing immediately after opening ---
    call setup('')
    call typed('()')
    call check_text('()', 'empty pair closed straight away')
    call check_cursor(1, 3, 'caret past the pair')

    ! --- Auto-close itself is unchanged ---
    call setup('')
    call typed('f(')
    call check_text('f()', 'opener still auto-closes')
    call check_cursor(1, 3, 'caret still parked inside the pair')

    call setup('')
    call typed('f(abc')
    call check_text('f(abc)', 'typing inside the pair keeps the closer ahead')

    ! --- Must NOT overtype: this ')' was never auto-inserted ---
    call setup('ab)')
    call cursor_at(1, 3)
    call typed(')')
    call check_text('ab))', 'a closer you wrote yourself is not stepped over')

    call setup('()')
    call cursor_at(1, 2)
    call typed(')')
    call check_text('())', 'pre-existing pair in the file is not stepped over')

    ! --- Moving the caret breaks the association ---
    call setup('')
    call typed('f(a')
    call key('left')
    call key('right')
    call typed(')')
    call check_text('f(a))', 'a cursor move invalidates the pending closer')

    ! --- ...as does any non-typing command ---
    call setup('')
    call typed('f(a')
    call key('enter')
    call typed(')')
    call check_text('f(a' // char(10) // '))', 'enter invalidates it too')

    ! --- Overtyping consumes the pending closer exactly once ---
    call setup('')
    call typed('f(a)')
    call typed(')')
    call check_text('f(a))', 'the second typed closer inserts normally')

    ! --- Non-pair characters are untouched ---
    call setup('')
    call typed('a>b')
    call check_text('a>b', 'unpaired punctuation is inserted as typed')

    ! --- Multiple cursors step over in lockstep ---
    call setup('xx yy')
    call cursors_at(1, 3, 1, 6)
    call typed('(a)')
    call check_text('xx(a) yy(a)', 'every cursor overtypes its own closer')

    ! --- ...but not when only some cursors face a pending closer ---
    call setup('p) q')
    call cursors_at(1, 2, 1, 5)
    call typed(')')
    call check_text('p)) q)', 'no overtype unless every cursor is in front of one')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All bracket-overtype tests passed'

contains

    subroutine setup(text)
        character(len=*), intent(in) :: text

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_command_handler()
        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'overtype_test.c')
        call cursor_at(1, len(text) + 1)
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

    ! Feed a string one keystroke at a time, exactly as the terminal would
    subroutine typed(text)
        character(len=*), intent(in) :: text
        integer :: i

        do i = 1, len(text)
            call key(text(i:i))
        end do
    end subroutine typed

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

    subroutine check_cursor(idx, c, name)
        integer, intent(in) :: idx, c
        character(len=*), intent(in) :: name
        character(len=32) :: got

        write(got, '(i0)') editor%cursors(idx)%column
        call check(editor%cursors(idx)%column == c, name, trim(got))
    end subroutine check_cursor

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

end program test_bracket_overtype
