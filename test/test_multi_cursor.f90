program test_multi_cursor
    ! Regression tests for multi-cursor editing. Every per-cursor edit must
    ! shift the OTHER cursors the way the text under them moved. The
    ! original code adjusted nothing, so a second cursor on the same line
    ! inserted one character early (the mouse-multicursor bug), backspace
    ! deleted the wrong character, and line joins stranded cursors.
    ! Tests drive handle_key_command, the same path real keys take.
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

    ! --- Same-line pair: insert (the reported bug) ---
    call setup("mm [1] [2] nn", [7, 11])
    call key("X")
    call check_text("mm [1]X [2]X nn", "same-line pair insert")
    call check_cursor(1, 1, 8,  "first cursor advanced")
    call check_cursor(2, 1, 13, "second cursor advanced past both inserts")

    ! --- Same-line pair: backspace ---
    call setup("mm [1] [2] nn", [7, 11])
    call key("backspace")
    call check_text("mm [1 [2 nn", "same-line pair backspace")
    call check_cursor(1, 1, 6, "first cursor after backspace")
    call check_cursor(2, 1, 9, "second cursor after both deletes")

    ! --- Same-line pair: delete key ---
    call setup("mm [1] [2] nn", [6, 10])
    call key("delete")
    call check_text("mm [1 [2 nn", "same-line pair delete")

    ! --- Cross-line insert stays independent ---
    call setup("aa [1] bb" // char(10) // "cc [22] dd", [0, 0])
    call set_cursors2(1, 7, 2, 8)
    call key("X")
    call check_text("aa [1]X bb" // char(10) // "cc [22]X dd", "cross-line insert")

    ! --- Line joins: two cursors at column 1 backspacing ---
    call setup("aa" // char(10) // "bb" // char(10) // "cc", [0, 0])
    call set_cursors2(2, 1, 3, 1)
    call key("backspace")
    call check_text("aabbcc", "double line join collapses to one line")
    call check_cursor(1, 1, 3, "first join cursor at seam")
    call check_cursor(2, 1, 5, "second join cursor at its seam")

    ! --- Enter: same-line pair splits into three lines ---
    call setup("abcdef", [3, 5])
    call key("enter")
    call check_text("ab" // char(10) // "cd" // char(10) // "ef", &
                    "same-line double enter")
    call check_cursor(1, 2, 1, "first enter cursor on new line")
    call check_cursor(2, 3, 1, "second enter cursor on its new line")

    ! --- Enter keeps auto-indent for both cursors ---
    call setup("    abcd", [7, 8])
    call key("enter")
    call check_text("    ab" // char(10) // "    c" // char(10) // "    d", &
                    "double enter with auto-indent")
    call check_cursor(1, 2, 5, "indented cursor 1")
    call check_cursor(2, 3, 5, "indented cursor 2")

    ! --- Auto-close parity with the single-cursor path ---
    call setup("xx yy", [3, 6])
    call key("(")
    call check_text("xx() yy()", "auto-close pair at both cursors")
    call check_cursor(1, 1, 4, "cursor 1 between brackets")
    call check_cursor(2, 1, 9, "cursor 2 between brackets")

    ! --- Typing over two same-line selections (ctrl-d workflow) ---
    call setup("foo bar foo", [0, 0])
    call set_cursors2(1, 4, 1, 12)
    editor%cursors(1)%has_selection = .true.
    editor%cursors(1)%selection_start_line = 1
    editor%cursors(1)%selection_start_col = 1
    editor%cursors(2)%has_selection = .true.
    editor%cursors(2)%selection_start_line = 1
    editor%cursors(2)%selection_start_col = 9
    call key("Z")
    call check_text("Z bar Z", "type over two selections")

    ! --- Wrapping two same-line selections in brackets ---
    call setup("foo bar foo", [0, 0])
    call set_cursors2(1, 4, 1, 12)
    editor%cursors(1)%has_selection = .true.
    editor%cursors(1)%selection_start_line = 1
    editor%cursors(1)%selection_start_col = 1
    editor%cursors(2)%has_selection = .true.
    editor%cursors(2)%selection_start_line = 1
    editor%cursors(2)%selection_start_col = 9
    call key("(")
    call check_text("(foo) bar (foo)", "wrap two selections")

    ! --- Tab at a same-line pair ---
    ! Tab advances to the next tab stop, so each caret inserts however much
    ! it needs rather than a fixed four: the caret after "a" sits at display
    ! column 1 and lands on 4, then the second caret -- shifted to column 5 by
    ! that insert -- lands on 8. Both end up on a stop, which is the point.
    call setup("abcd", [2, 3])
    call key("tab")
    call check_text("a   b   cd", "tab at same-line pair")

    ! --- Escape collapses to a single cursor ---
    call setup("abcd", [2, 3])
    call key("esc")
    call check(size(editor%cursors) == 1, "escape collapses cursors", "still multi")

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All multi-cursor tests passed'

contains

    ! Fresh editor+buffer with text; cols(i) > 0 places cursor i there on line 1
    subroutine setup(text, cols)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cols(2)

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_buffer(buffer)
        call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'mc_test.txt')
        if (cols(1) > 0) then
            call set_cursors2(1, cols(1), 1, cols(2))
        end if
    end subroutine setup

    subroutine set_cursors2(l1, c1, l2, c2)
        integer, intent(in) :: l1, c1, l2, c2

        ! cursor_t fields have default initializers; allocation resets them
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(2))
        editor%cursors(1)%line = l1
        editor%cursors(1)%column = c1
        editor%cursors(2)%line = l2
        editor%cursors(2)%column = c2
        editor%active_cursor = 1
    end subroutine set_cursors2

    subroutine key(k)
        character(len=*), intent(in) :: k

        quit = .false.
        call handle_key_command(k, editor, buffer, quit)
    end subroutine key

    subroutine check_text(expected, name)
        character(len=*), intent(in) :: expected, name
        character(len=:), allocatable :: got

        got = buffer_to_string(buffer)
        call check(got == expected, name, got)
    end subroutine check_text

    subroutine check_cursor(idx, l, c, name)
        integer, intent(in) :: idx, l, c
        character(len=*), intent(in) :: name
        character(len=64) :: got

        if (idx > size(editor%cursors)) then
            call check(.false., name, "cursor index out of range")
            return
        end if
        write(got, '(a,i0,a,i0,a)') '(', editor%cursors(idx)%line, ',', &
            editor%cursors(idx)%column, ')'
        call check(editor%cursors(idx)%line == l .and. &
                   editor%cursors(idx)%column == c, name, trim(got))
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

end program test_multi_cursor
