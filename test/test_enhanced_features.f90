program test_enhanced_features
    use test_framework
    use test_driver_module
    use text_buffer_module
    use editor_state_module
    implicit none

    type(test_suite) :: suite
    type(test_case), allocatable :: tests(:)

    ! Define test suite
    suite%name = "Enhanced Features Tests"

    ! Allocate and configure tests
    allocate(tests(8))

    tests(1)%name = "Auto-close brackets"
    tests(1)%test_procedure => test_auto_close_brackets

    tests(2)%name = "Auto-close quotes"
    tests(2)%test_procedure => test_auto_close_quotes

    tests(3)%name = "Tab indents selection"
    tests(3)%test_procedure => test_tab_indent

    tests(4)%name = "Shift-Tab dedents selection"
    tests(4)%test_procedure => test_shift_tab_dedent

    tests(5)%name = "Join lines"
    tests(5)%test_procedure => test_join_lines

    tests(6)%name = "Duplicate line down"
    tests(6)%test_procedure => test_duplicate_line

    tests(7)%name = "Kill and yank line"
    tests(7)%test_procedure => test_kill_yank

    tests(8)%name = "Undo and redo"
    tests(8)%test_procedure => test_undo_redo

    suite%tests = tests

    ! Run the test suite
    call run_tests(suite)

contains

    subroutine test_auto_close_brackets()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer)

        ! Test parentheses
        call simulate_typing(editor, buffer, "(")
        call assert_buffer_equals(buffer, "()", "Auto-close parentheses")
        call assert_cursor_at(editor, 1, 2, "Cursor between parens")

        ! Type inside the parens
        call simulate_typing(editor, buffer, "test")
        call assert_buffer_equals(buffer, "(test)", "Text inside parens")

        ! Test square brackets
        call simulate_key(editor, buffer, "end")  ! Move to end
        call simulate_typing(editor, buffer, "[")
        call assert_buffer_equals(buffer, "(test)[]", "Auto-close brackets")

        ! Test curly braces
        call simulate_typing(editor, buffer, "x")  ! Type between brackets
        call simulate_key(editor, buffer, "end")
        call simulate_typing(editor, buffer, "{")
        call assert_buffer_equals(buffer, "(test)[x]{}", "Auto-close braces")

        call destroy_test_editor(editor, buffer)
    end subroutine test_auto_close_brackets

    subroutine test_auto_close_quotes()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer)

        ! Test double quotes
        call simulate_typing(editor, buffer, '"')
        call assert_buffer_equals(buffer, '""', "Auto-close double quotes")
        call assert_cursor_at(editor, 1, 2, "Cursor between quotes")

        call simulate_typing(editor, buffer, "hello")
        call assert_buffer_equals(buffer, '"hello"', "Text in quotes")

        ! Test single quotes
        call simulate_key(editor, buffer, "end")
        call simulate_typing(editor, buffer, " ")
        call simulate_typing(editor, buffer, "'")
        call assert_buffer_equals(buffer, '"hello" ' // "''", "Auto-close single quotes")

        call destroy_test_editor(editor, buffer)
    end subroutine test_auto_close_quotes

    subroutine test_tab_indent()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        ! Create multi-line text
        call create_test_editor(editor, buffer, "Line 1" // char(10) // "Line 2" // char(10) // "Line 3")

        ! Select lines 2 and 3
        editor%cursors(1)%line = 2
        editor%cursors(1)%column = 1
        editor%cursors(1)%has_selection = .true.
        editor%cursors(1)%selection_start_line = 2
        editor%cursors(1)%selection_start_col = 1
        editor%cursors(1)%line = 3
        editor%cursors(1)%column = 7  ! End of "Line 3"

        ! Tab to indent
        call simulate_key(editor, buffer, "tab")

        call assert_line_equals(buffer, 1, "Line 1", "Line 1 unchanged")
        call assert_line_equals(buffer, 2, "    Line 2", "Line 2 indented")
        call assert_line_equals(buffer, 3, "    Line 3", "Line 3 indented")

        call destroy_test_editor(editor, buffer)
    end subroutine test_tab_indent

    subroutine test_shift_tab_dedent()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        ! Create indented text
        call create_test_editor(editor, buffer, "    Line 1" // char(10) // "    Line 2")

        ! Select both lines
        editor%cursors(1)%has_selection = .true.
        editor%cursors(1)%selection_start_line = 1
        editor%cursors(1)%selection_start_col = 1
        editor%cursors(1)%line = 2
        editor%cursors(1)%column = 11

        ! Shift-Tab to dedent
        call simulate_key(editor, buffer, "shift-tab")

        call assert_line_equals(buffer, 1, "Line 1", "Line 1 dedented")
        call assert_line_equals(buffer, 2, "Line 2", "Line 2 dedented")

        call destroy_test_editor(editor, buffer)
    end subroutine test_shift_tab_dedent

    subroutine test_join_lines()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello" // char(10) // "    World")

        ! Join the lines (bound to alt-shift-j; ctrl-j was the old binding)
        call simulate_key(editor, buffer, "alt-shift-j")

        call assert_buffer_equals(buffer, "Hello World", "Lines joined with space")
        call assert_cursor_at(editor, 1, 1, "Cursor position maintained")

        call destroy_test_editor(editor, buffer)
    end subroutine test_join_lines

    subroutine test_duplicate_line()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        ! ctrl-t (old transpose binding) now opens a new tab; exercise
        ! line duplication instead, which is a current editing feature
        call create_test_editor(editor, buffer, "Hello")
        editor%cursors(1)%column = 3

        call simulate_key(editor, buffer, "alt-shift-down")

        call assert_buffer_equals(buffer, "Hello" // char(10) // "Hello", &
                                  "Line duplicated below")
        call assert_cursor_at(editor, 1, 3, "Cursor stays on original line")

        call destroy_test_editor(editor, buffer)
    end subroutine test_duplicate_line

    subroutine test_kill_yank()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello World")
        editor%cursors(1)%column = 6  ! After "Hello"

        ! Kill to end of line
        call simulate_key(editor, buffer, "ctrl-k")
        call assert_buffer_equals(buffer, "Hello", "Kill line forward")

        ! Move to beginning and yank
        call simulate_key(editor, buffer, "home")
        call simulate_key(editor, buffer, "ctrl-y")
        call assert_buffer_equals(buffer, " WorldHello", "Yanked text")

        call destroy_test_editor(editor, buffer)
    end subroutine test_kill_yank

    subroutine test_undo_redo()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer)

        ! Type some text
        call simulate_typing(editor, buffer, "Hello")
        call assert_buffer_equals(buffer, "Hello", "Initial text")

        ! Undo
        call simulate_key(editor, buffer, "ctrl-z")
        call assert_buffer_equals(buffer, "", "Undo typing")

        ! Redo
        call simulate_key(editor, buffer, "ctrl-shift-z")
        call assert_buffer_equals(buffer, "Hello", "Redo typing")

        call destroy_test_editor(editor, buffer)
    end subroutine test_undo_redo

end program test_enhanced_features