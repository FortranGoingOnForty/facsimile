module test_driver_module
    use text_buffer_module
    use editor_state_module
    use command_handler_module
    use mock_terminal_module
    implicit none
    private

    public :: create_test_editor, destroy_test_editor
    public :: simulate_typing, simulate_key, simulate_key_sequence
    public :: get_buffer_text, get_cursor_position, get_line_text
    public :: assert_buffer_equals, assert_cursor_at
    public :: assert_line_equals

contains

    subroutine create_test_editor(editor, buffer, initial_text)
        type(editor_state_t), intent(out) :: editor
        type(buffer_t), intent(out) :: buffer
        character(len=*), intent(in), optional :: initial_text
        integer :: i

        ! Initialize buffer
        call init_buffer(buffer)

        ! Add initial text if provided
        if (present(initial_text)) then
            do i = 1, len(initial_text)
                call buffer_insert(buffer, i, initial_text(i:i))
            end do
        end if

        ! Initialize editor state
        editor%screen_rows = 24
        editor%screen_cols = 80
        editor%viewport_line = 1
        editor%viewport_column = 1
        allocate(editor%cursors(1))
        editor%cursors(1)%line = 1
        editor%cursors(1)%column = 1
        editor%cursors(1)%desired_column = 1
        editor%cursors(1)%has_selection = .false.
        editor%active_cursor = 1

        ! Initialize command handler
        call init_command_handler()
    end subroutine create_test_editor

    subroutine destroy_test_editor(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer

        call cleanup_buffer(buffer)
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        if (allocated(editor%filename)) deallocate(editor%filename)
        call cleanup_command_handler()
    end subroutine destroy_test_editor

    subroutine simulate_typing(editor, buffer, text)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: text
        integer :: i
        character(len=16) :: key_str
        logical :: should_quit

        do i = 1, len(text)
            ! Convert character to key string
            write(key_str, '(A)') text(i:i)
            call handle_key_command(trim(key_str), editor, buffer, should_quit)
        end do
    end subroutine simulate_typing

    subroutine simulate_key(editor, buffer, key_name)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: key_name
        logical :: should_quit

        call handle_key_command(key_name, editor, buffer, should_quit)
    end subroutine simulate_key

    subroutine simulate_key_sequence(editor, buffer, keys)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), dimension(:), intent(in) :: keys
        integer :: i

        do i = 1, size(keys)
            call simulate_key(editor, buffer, keys(i))
        end do
    end subroutine simulate_key_sequence

    function get_buffer_text(buffer) result(text)
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text
        integer :: i, pos
        character :: ch

        allocate(character(len=buffer%size) :: text)
        pos = 0

        do i = 1, buffer%size
            ch = buffer_get_char_at(buffer, i)
            pos = pos + 1
            text(pos:pos) = ch
        end do

        text = text(1:pos)
    end function get_buffer_text

    function get_line_text(buffer, line_num) result(text)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        character(len=:), allocatable :: text

        text = buffer_get_line(buffer, line_num)
    end function get_line_text

    subroutine get_cursor_position(editor, line, column)
        type(editor_state_t), intent(in) :: editor
        integer, intent(out) :: line, column

        line = editor%cursors(editor%active_cursor)%line
        column = editor%cursors(editor%active_cursor)%column
    end subroutine get_cursor_position

    subroutine assert_buffer_equals(buffer, expected, test_name)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: expected
        character(len=*), intent(in), optional :: test_name
        character(len=:), allocatable :: actual
        character(len=256) :: msg

        actual = get_buffer_text(buffer)

        if (actual /= expected) then
            if (present(test_name)) then
                write(msg, '(A,A)') trim(test_name), ": Buffer content mismatch"
            else
                msg = "Buffer content mismatch"
            end if
            write(*, '(A)') trim(msg)
            write(*, '(A,A)') "Expected: '", expected, "'"
            write(*, '(A,A)') "Actual:   '", actual, "'"
            error stop "Test failed"
        end if
    end subroutine assert_buffer_equals

    subroutine assert_cursor_at(editor, expected_line, expected_col, test_name)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: expected_line, expected_col
        character(len=*), intent(in), optional :: test_name
        integer :: actual_line, actual_col
        character(len=256) :: msg

        call get_cursor_position(editor, actual_line, actual_col)

        if (actual_line /= expected_line .or. actual_col /= expected_col) then
            if (present(test_name)) then
                write(msg, '(A,A)') trim(test_name), ": Cursor position mismatch"
            else
                msg = "Cursor position mismatch"
            end if
            write(*, '(A)') trim(msg)
            write(*, '(A,I0,A,I0,A)') "Expected: (", expected_line, ",", expected_col, ")"
            write(*, '(A,I0,A,I0,A)') "Actual:   (", actual_line, ",", actual_col, ")"
            error stop "Test failed"
        end if
    end subroutine assert_cursor_at

    subroutine assert_line_equals(buffer, line_num, expected, test_name)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        character(len=*), intent(in) :: expected
        character(len=*), intent(in), optional :: test_name
        character(len=:), allocatable :: actual
        character(len=256) :: msg

        actual = get_line_text(buffer, line_num)

        if (actual /= expected) then
            if (present(test_name)) then
                write(msg, '(A,A,I0)') trim(test_name), ": Line ", line_num, " mismatch"
            else
                write(msg, '(A,I0,A)') "Line ", line_num, " mismatch"
            end if
            write(*, '(A)') trim(msg)
            write(*, '(A,A)') "Expected: '", expected, "'"
            write(*, '(A,A)') "Actual:   '", actual, "'"
            error stop "Test failed"
        end if
    end subroutine assert_line_equals

    function buffer_get_char_at(buffer, position) result(ch)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: position
        character :: ch

        if (position < 1 .or. position > buffer%size) then
            ch = char(0)
            return
        end if

        if (position <= buffer%gap_start) then
            ch = buffer%data(position:position)
        else
            ch = buffer%data(position + (buffer%gap_end - buffer%gap_start - 1):position + (buffer%gap_end - buffer%gap_start - 1))
        end if
    end function buffer_get_char_at

end module test_driver_module