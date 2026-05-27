module goto_prompt_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    implicit none
    private

    public :: show_goto_prompt

contains

    subroutine show_goto_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: input_buffer
        character(len=32) :: prompt
        integer :: input_pos, ch
        integer :: target_line, target_col
        integer :: line_count

        ! Initialize
        input_buffer = ''
        input_pos = 0
        prompt = 'Go to (line:col): '

        ! Get current position for default
        target_line = editor%cursors(editor%active_cursor)%line
        target_col = editor%cursors(editor%active_cursor)%column
        line_count = buffer_get_line_count(buffer)

        ! Display prompt at bottom of screen (clear entire row first)
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(prompt)
        call terminal_show_cursor()

        ! Input loop
        do
            ch = terminal_read_char()

            if (ch == -1) then
                ! No input, continue
                cycle
            else if (ch == 27) then  ! ESC - cancel
                exit
            else if (ch == 10 .or. ch == 13) then  ! Enter - accept
                if (input_pos > 0) then
                    call parse_goto_input(input_buffer(1:input_pos), target_line, target_col)
                end if

                ! Validate and apply
                if (target_line >= 1 .and. target_line <= line_count) then
                    editor%cursors(editor%active_cursor)%line = target_line
                    editor%cursors(editor%active_cursor)%column = max(1, target_col)
                    editor%cursors(editor%active_cursor)%desired_column = &
                        editor%cursors(editor%active_cursor)%column

                    ! Clear any selection
                    editor%cursors(editor%active_cursor)%has_selection = .false.

                    ! Update viewport to center on target
                    call center_viewport_on_cursor(editor)
                end if
                exit
            else if (ch == 127 .or. ch == 8) then  ! Backspace
                if (input_pos > 0) then
                    input_pos = input_pos - 1
                    ! Redraw prompt and input
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(prompt // input_buffer(1:input_pos) // ' ')
                    call terminal_move_cursor(editor%screen_rows, len(prompt) + input_pos + 1)
                end if
            else if (ch >= 32 .and. ch <= 126) then  ! Printable characters
                if (input_pos < 256) then
                    input_pos = input_pos + 1
                    input_buffer(input_pos:input_pos) = char(ch)
                    call terminal_write(char(ch))
                end if
            end if
        end do

        ! Clean up - hide cursor and clear prompt line
        call terminal_hide_cursor()
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))
    end subroutine show_goto_prompt

    subroutine parse_goto_input(input, line, col)
        character(len=*), intent(in) :: input
        integer, intent(out) :: line, col
        integer :: colon_pos, ios
        character(len=:), allocatable :: line_str, col_str

        ! Default to column 1
        col = 1

        ! Find colon separator
        colon_pos = index(input, ':')

        if (colon_pos > 0) then
            ! Parse line:column format
            line_str = input(1:colon_pos-1)
            col_str = input(colon_pos+1:)

            ! Parse line number
            read(line_str, '(i10)', iostat=ios) line
            if (ios /= 0) line = 1

            ! Parse column number if present
            if (len_trim(col_str) > 0) then
                read(col_str, '(i10)', iostat=ios) col
                if (ios /= 0) col = 1
            end if
        else
            ! Just a line number
            read(input, '(i10)', iostat=ios) line
            if (ios /= 0) line = 1
        end if

        ! Ensure positive values
        line = max(1, line)
        col = max(1, col)
    end subroutine parse_goto_input

    subroutine center_viewport_on_cursor(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: cursor_line
        integer :: viewport_height

        cursor_line = editor%cursors(editor%active_cursor)%line
        viewport_height = editor%screen_rows - 2  ! Account for status bar

        ! Center the cursor in the viewport
        editor%viewport_line = max(1, cursor_line - viewport_height / 2)
    end subroutine center_viewport_on_cursor

end module goto_prompt_module