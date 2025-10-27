program test_basic_editing
    use test_framework
    use test_driver_module
    use text_buffer_module
    use editor_state_module
    implicit none

    type(test_suite) :: suite
    type(test_case), allocatable :: tests(:)

    ! Define test suite
    suite%name = "Basic Editing Tests"

    ! Allocate and configure tests
    allocate(tests(10))

    tests(1)%name = "Insert characters"
    tests(1)%test_procedure => test_insert_chars

    tests(2)%name = "Backspace"
    tests(2)%test_procedure => test_backspace

    tests(3)%name = "Delete forward"
    tests(3)%test_procedure => test_delete

    tests(4)%name = "Enter creates newline"
    tests(4)%test_procedure => test_enter_newline

    tests(5)%name = "Tab inserts spaces"
    tests(5)%test_procedure => test_tab_spaces

    tests(6)%name = "Cursor movement"
    tests(6)%test_procedure => test_cursor_movement

    tests(7)%name = "Home/End navigation"
    tests(7)%test_procedure => test_home_end

    tests(8)%name = "Word jumping"
    tests(8)%test_procedure => test_word_jump

    tests(9)%name = "Auto-indent on enter"
    tests(9)%test_procedure => test_auto_indent

    tests(10)%name = "Smart home behavior"
    tests(10)%test_procedure => test_smart_home

    suite%tests = tests

    ! Run the test suite
    call run_tests(suite)

contains

    subroutine test_insert_chars()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer)
        call simulate_typing(editor, buffer, "Hello World")
        call assert_buffer_equals(buffer, "Hello World", "Insert text")
        call assert_cursor_at(editor, 1, 12, "Cursor after insert")
        call destroy_test_editor(editor, buffer)
    end subroutine test_insert_chars

    subroutine test_backspace()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello")
        editor%cursors(1)%column = 6  ! Position after "Hello"
        call simulate_key(editor, buffer, "backspace")
        call assert_buffer_equals(buffer, "Hell", "Backspace deletes")
        call assert_cursor_at(editor, 1, 5, "Cursor after backspace")
        call destroy_test_editor(editor, buffer)
    end subroutine test_backspace

    subroutine test_delete()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello")
        editor%cursors(1)%column = 3  ! Position at 'l'
        call simulate_key(editor, buffer, "delete")
        call assert_buffer_equals(buffer, "Helo", "Delete forward")
        call assert_cursor_at(editor, 1, 3, "Cursor stays put")
        call destroy_test_editor(editor, buffer)
    end subroutine test_delete

    subroutine test_enter_newline()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer
        character(len=:), allocatable :: content

        call create_test_editor(editor, buffer, "Hello")
        editor%cursors(1)%column = 3  ! Position after "He"
        call simulate_key(editor, buffer, "enter")
        content = get_buffer_text(buffer)

        ! Check that we have "He\nllo"
        call assert_true(content == "He" // char(10) // "llo", "Enter splits line")
        call assert_cursor_at(editor, 2, 1, "Cursor on new line")
        call destroy_test_editor(editor, buffer)
    end subroutine test_enter_newline

    subroutine test_tab_spaces()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer)
        call simulate_key(editor, buffer, "tab")
        call assert_buffer_equals(buffer, "    ", "Tab inserts 4 spaces")
        call assert_cursor_at(editor, 1, 5, "Cursor after tab")
        call destroy_test_editor(editor, buffer)
    end subroutine test_tab_spaces

    subroutine test_cursor_movement()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello" // char(10) // "World")

        ! Test arrow keys
        call simulate_key(editor, buffer, "right")
        call assert_cursor_at(editor, 1, 2, "Right arrow")

        call simulate_key(editor, buffer, "down")
        call assert_cursor_at(editor, 2, 2, "Down arrow")

        call simulate_key(editor, buffer, "left")
        call assert_cursor_at(editor, 2, 1, "Left arrow")

        call simulate_key(editor, buffer, "up")
        call assert_cursor_at(editor, 1, 1, "Up arrow")

        call destroy_test_editor(editor, buffer)
    end subroutine test_cursor_movement

    subroutine test_home_end()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello World")
        editor%cursors(1)%column = 6  ! Middle of line

        call simulate_key(editor, buffer, "home")
        call assert_cursor_at(editor, 1, 1, "Home key")

        call simulate_key(editor, buffer, "end")
        call assert_cursor_at(editor, 1, 12, "End key")

        call destroy_test_editor(editor, buffer)
    end subroutine test_home_end

    subroutine test_word_jump()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "Hello World Test")

        call simulate_key(editor, buffer, "alt-right")
        call assert_cursor_at(editor, 1, 6, "Jump to end of 'Hello'")

        call simulate_key(editor, buffer, "alt-right")
        call assert_cursor_at(editor, 1, 12, "Jump to end of 'World'")

        call simulate_key(editor, buffer, "alt-left")
        call assert_cursor_at(editor, 1, 7, "Jump back to 'World'")

        call destroy_test_editor(editor, buffer)
    end subroutine test_word_jump

    subroutine test_auto_indent()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "    Hello")
        editor%cursors(1)%column = 10  ! After "Hello"

        call simulate_key(editor, buffer, "enter")
        call simulate_typing(editor, buffer, "World")

        call assert_line_equals(buffer, 1, "    Hello", "First line preserved")
        call assert_line_equals(buffer, 2, "    World", "Auto-indented line")

        call destroy_test_editor(editor, buffer)
    end subroutine test_auto_indent

    subroutine test_smart_home()
        type(editor_state_t) :: editor
        type(buffer_t) :: buffer

        call create_test_editor(editor, buffer, "    Hello")
        editor%cursors(1)%column = 9  ! At 'o'

        ! First press goes to first non-whitespace
        call simulate_key(editor, buffer, "ctrl-a")
        call assert_cursor_at(editor, 1, 5, "Smart home to first char")

        ! Second press goes to column 1
        call simulate_key(editor, buffer, "ctrl-a")
        call assert_cursor_at(editor, 1, 1, "Smart home to column 1")

        ! Third press goes back to first non-whitespace
        call simulate_key(editor, buffer, "ctrl-a")
        call assert_cursor_at(editor, 1, 5, "Smart home toggles")

        call destroy_test_editor(editor, buffer)
    end subroutine test_smart_home

end program test_basic_editing