program test_comment_toggle
    ! Regression tests for ctrl-/ (toggle line comment).
    !
    ! The interesting parts are not "insert // at column 1": they are the
    ! indent baseline (a whole block shifts by the SHALLOWEST indent, so
    ! relative nesting survives), the whole-line rule for partial selections,
    ! the column-1 end rule, the half-commented -> fully-commented direction,
    ! and keeping every cursor glued to the text it was sitting on.
    ! Tests drive handle_key_command, the same path a real ctrl-/ takes.
    use editor_state_module
    use text_buffer_module
    use command_handler_module, only: handle_key_command, init_command_handler, &
                                     save_initial_state_for_undo
    implicit none

    character(len=1), parameter :: NL = char(10)
    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    logical :: quit
    integer :: nfail

    nfail = 0
    call init_command_handler()

    ! --- Single line, no selection ---
    call setup('x.py', 'print(1)')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('# print(1)', 'python single line commented')
    call check_cursor(1, 1, 3, 'cursor stayed on the "p" it was in front of')
    call key('ctrl-/')
    call check_text('print(1)', 'python single line uncommented')
    call check_cursor(1, 1, 1, 'cursor came back with the text')

    ! --- Token is per language ---
    call setup('x.c', 'int a;')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('// int a;', 'c uses //')

    call setup('x.f90', 'call foo()')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('! call foo()', 'fortran uses !')

    call setup('CMakeLists.txt', 'project(x)')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('# project(x)', 'extensionless-style names matched by basename')

    ! --- Indentation baseline: the shallowest indent wins for the block ---
    call setup('x.py', 'def f():' // NL // '    a = 1' // NL // '        b = 2')
    call select_range(2, 1, 3, 14)
    call key('ctrl-/')
    ! The token goes at column 5 on BOTH lines -- the deeper line keeps its
    ! extra indent *inside* the comment, so the block's shape is unchanged.
    call check_text('def f():' // NL // '    # a = 1' // NL // '    #     b = 2', &
                    'tokens land at the shallowest indent, nesting preserved')
    call key('ctrl-/')
    call check_text('def f():' // NL // '    a = 1' // NL // '        b = 2', &
                    'uncomment restores both indents exactly')

    ! --- The token never lands at column 1 when the block is indented ---
    call setup('x.py', '        deep = 1')
    call cursor_at(1, 3)
    call key('ctrl-/')
    call check_text('        # deep = 1', 'lone indented line keeps its indent')

    ! --- Tab indentation is respected the same way ---
    call setup('x.go', char(9) // 'x := 1' // NL // char(9) // char(9) // 'y := 2')
    call select_range(1, 1, 2, 10)
    call key('ctrl-/')
    call check_text(char(9) // '// x := 1' // NL // char(9) // '// ' // char(9) // 'y := 2', &
                    'tab indent baseline is the shallowest tab run')

    ! --- Partial lines: a selection that starts and ends mid-line still
    !     comments whole lines ---
    call setup('x.py', 'alpha = 1' // NL // 'beta = 2' // NL // 'gamma = 3')
    call select_range(1, 4, 3, 4)
    call key('ctrl-/')
    call check_text('# alpha = 1' // NL // '# beta = 2' // NL // '# gamma = 3', &
                    'partial selection comments whole lines')

    ! --- A selection ending in column 1 does not drag that line in ---
    call setup('x.py', 'a = 1' // NL // 'b = 2' // NL // 'c = 3')
    call select_range(1, 1, 3, 1)
    call key('ctrl-/')
    call check_text('# a = 1' // NL // '# b = 2' // NL // 'c = 3', &
                    'selection ending at column 1 excludes that line')

    ! --- Backwards selection (anchor below the caret) behaves identically ---
    call setup('x.py', 'a = 1' // NL // 'b = 2')
    call select_range(2, 6, 1, 1)
    call key('ctrl-/')
    call check_text('# a = 1' // NL // '# b = 2', 'backwards selection commented')

    ! --- Half-commented block goes fully commented, not toggled per line ---
    call setup('x.py', '# a = 1' // NL // 'b = 2')
    call select_range(1, 1, 2, 6)
    call key('ctrl-/')
    call check_text('# # a = 1' // NL // '# b = 2', &
                    'partially commented block becomes fully commented')
    call key('ctrl-/')
    call check_text('# a = 1' // NL // 'b = 2', 'and unwinds in one step')

    ! --- Blank lines inside a range are left alone ---
    call setup('x.py', 'a = 1' // NL // '' // NL // 'b = 2')
    call select_range(1, 1, 3, 6)
    call key('ctrl-/')
    call check_text('# a = 1' // NL // '' // NL // '# b = 2', &
                    'blank line inside the range gets no token')
    call key('ctrl-/')
    call check_text('a = 1' // NL // '' // NL // 'b = 2', &
                    'blank line does not block uncommenting')

    ! --- ...but a lone blank line can still be commented ---
    call setup('x.py', '')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('# ', 'empty line still takes a token')

    ! --- A comment with no space after the token uncomments cleanly ---
    call setup('x.py', '#a = 1')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('a = 1', 'token without a trailing space is removed')

    ! --- Deeper indentation inside the comment body survives a round trip ---
    call setup('x.py', '#     a = 1')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('    a = 1', 'only one space after the token is eaten')

    ! --- Multiple cursors on separate lines ---
    call setup('x.py', 'a = 1' // NL // 'b = 2' // NL // 'c = 3' // NL // 'd = 4')
    call cursors_at(1, 1, 4, 1)
    call key('ctrl-/')
    call check_text('# a = 1' // NL // 'b = 2' // NL // 'c = 3' // NL // '# d = 4', &
                    'each cursor comments its own line')
    call check_cursor(1, 1, 3, 'first cursor followed its text')
    call check_cursor(2, 4, 3, 'second cursor followed its text')

    ! --- Adjacent cursors share one indent baseline ---
    call setup('x.py', '    a = 1' // NL // '        b = 2')
    call cursors_at(1, 5, 2, 9)
    call key('ctrl-/')
    call check_text('    # a = 1' // NL // '    #     b = 2', &
                    'adjacent cursors merge into one block')

    ! --- Selection anchor tracks the insertion too ---
    call setup('x.py', 'abcdef')
    call select_range(1, 2, 1, 5)
    call key('ctrl-/')
    call check_text('# abcdef', 'selection on one line commented')
    call check_cursor(1, 1, 7, 'caret shifted by the token')
    call check(editor%cursors(1)%selection_start_col == 4, &
               'selection anchor shifted by the token', &
               int_str(editor%cursors(1)%selection_start_col))

    ! --- Languages with no line comment fall back to block delimiters ---
    call setup('x.html', '<p>hi</p>')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('<!-- <p>hi</p> -->', 'html wraps in a block comment')
    call key('ctrl-/')
    call check_text('<p>hi</p>', 'html block comment unwraps')

    call setup('x.html', '<a>' // NL // '<b>')
    call select_range(1, 1, 2, 4)
    call key('ctrl-/')
    call check_text('<!-- <a>' // NL // '<b> -->', 'html block spans the range')
    call key('ctrl-/')
    call check_text('<a>' // NL // '<b>', 'multi-line html block unwraps')

    ! --- Unknown language: no comment token, so no edit at all ---
    call setup('x.unknownext', 'leave me alone')
    call cursor_at(1, 1)
    call key('ctrl-/')
    call check_text('leave me alone', 'unknown extension is a no-op')

    ! --- UTF-8: columns are characters, not bytes ---
    call setup('x.py', '    caf' // char(195) // char(169) // ' = 1')
    call cursor_at(1, 12)
    call key('ctrl-/')
    call check_text('    # caf' // char(195) // char(169) // ' = 1', &
                    'multibyte line commented at the right column')
    call check_cursor(1, 1, 14, 'cursor shifted by characters, not bytes')

    ! --- Undo puts it all back ---
    call setup('x.py', 'a = 1' // NL // 'b = 2')
    call select_range(1, 1, 2, 6)
    call save_initial_state_for_undo()
    call key('ctrl-/')
    call check_text('# a = 1' // NL // '# b = 2', 'commented before undo')
    call key('ctrl-z')
    call check_text('a = 1' // NL // 'b = 2', 'undo reverts the whole toggle')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All comment-toggle tests passed'

contains

    subroutine setup(filename, text)
        character(len=*), intent(in) :: filename, text

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        ! Fresh undo stack per case; it is module state in the command handler
        call init_command_handler()
        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, filename)
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

    ! Anchor at (al, ac), caret at (cl, cc)
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

    subroutine check_cursor(idx, l, c, name)
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
    end subroutine check_cursor

    function int_str(v) result(s)
        integer, intent(in) :: v
        character(len=16) :: s
        write(s, '(i0)') v
    end function int_str

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

end program test_comment_toggle
