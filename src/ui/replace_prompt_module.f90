module replace_prompt_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    use search_prompt_module, only: find_next_match, center_viewport_on_cursor
    use renderer_module, only: render_screen
    use utf8_module, only: utf8_char_count, utf8_char_to_byte_index, utf8_byte_to_char_index
    implicit none
    private

    ! Column conventions: find_next_match positions are BYTE offsets in
    ! the line; cursor_t columns are UTF-8 CHARACTER indices. Convert at
    ! the boundaries below.

    public :: show_replace_prompt

contains

    subroutine show_replace_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: find_buffer, replace_buffer
        character(len=64) :: prompt
        integer :: input_pos, ch
        logical :: entering_find, entering_replace
        integer :: replace_count
        character(len=:), allocatable :: find_pattern, replace_text

        ! Initialize allocatables to prevent "may be uninitialized" warning
        allocate(character(len=0) :: find_pattern)
        allocate(character(len=0) :: replace_text)

        ! Initialize
        find_buffer = ''
        replace_buffer = ''
        input_pos = 0
        entering_find = .true.
        entering_replace = .false.
        replace_count = 0

        ! Display initial prompt
        prompt = 'Replace: '
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(prompt)
        call terminal_show_cursor()

        ! Input loop for find pattern
        do
            ch = terminal_read_char()

            if (ch == -1) then
                cycle
            else if (ch == 27) then  ! ESC - cancel
                exit
            else if (ch == 10 .or. ch == 13) then  ! Enter - proceed to replacement
                if (input_pos > 0 .and. entering_find) then
                    ! Assignment (re)allocates; find_pattern is already
                    ! allocated at length 0 above
                    find_pattern = find_buffer(1:input_pos)
                    entering_find = .false.
                    entering_replace = .true.
                    input_pos = 0

                    ! Update prompt for replacement
                    prompt = 'With: '
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(repeat(' ', editor%screen_cols))
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(prompt)
                else if (entering_replace) then
                    if (input_pos > 0) then
                        replace_text = replace_buffer(1:input_pos)
                    else
                        replace_text = ''
                    end if
                    exit
                end if
            else if (ch == 127 .or. ch == 8) then  ! Backspace
                if (input_pos > 0) then
                    input_pos = input_pos - 1
                    call terminal_move_cursor(editor%screen_rows, 1)
                    if (entering_find) then
                        call terminal_write(prompt // find_buffer(1:input_pos) // ' ')
                    else
                        call terminal_write(prompt // replace_buffer(1:input_pos) // ' ')
                    end if
                    call terminal_move_cursor(editor%screen_rows, len(prompt) + input_pos + 1)
                    call terminal_flush()
                end if
            else if (ch >= 32 .and. ch <= 126) then  ! Printable characters
                if (input_pos < 256) then
                    input_pos = input_pos + 1
                    if (entering_find) then
                        find_buffer(input_pos:input_pos) = char(ch)
                    else
                        replace_buffer(input_pos:input_pos) = char(ch)
                    end if
                    call terminal_write(char(ch))
                    call terminal_flush()
                end if
            end if
        end do

        ! Clean up prompt line
        call terminal_hide_cursor()
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))

        ! If we have both patterns, show replace options (an empty find
        ! pattern means the prompt was cancelled before completion)
        if (len(find_pattern) > 0 .and. allocated(replace_text)) then
            call execute_replace(editor, buffer, find_pattern, replace_text, replace_count)

            ! Show result
            call terminal_move_cursor(editor%screen_rows, 1)
            if (replace_count > 0) then
                write(prompt, '(a,i0,a)') 'Replaced ', replace_count, ' occurrences'
            else
                prompt = 'No matches found'
            end if
            call terminal_write(trim(prompt))
            call flush(output_unit)

            ! Brief pause to show message
            call sleep_ms(1500)

            ! Clear message
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(repeat(' ', editor%screen_cols))
        end if

        ! Clean up
        if (allocated(find_pattern)) deallocate(find_pattern)
        if (allocated(replace_text)) deallocate(replace_text)
    end subroutine show_replace_prompt

    subroutine execute_replace(editor, buffer, find_pattern, replace_text, replace_count)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: find_pattern, replace_text
        integer, intent(out) :: replace_count
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col
        integer :: start_char, end_char
        character(len=64) :: prompt
        character :: response
        integer :: ch
        logical :: replace_all

        replace_count = 0
        replace_all = .false.
        start_line = 1
        start_col = 1

        ! Find first match
        call find_next_match(buffer, find_pattern, start_line, start_col, &
                           found, found_line, found_col)

        do while (found)
            ! Move cursor to match (char columns; found_col is bytes)
            start_char = line_char_col(buffer, found_line, found_col)
            end_char = line_char_col(buffer, found_line, found_col + len(find_pattern))
            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%desired_column = start_char

            ! Select the match
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = start_char
            editor%cursors(editor%active_cursor)%column = end_char

            ! Update viewport and render
            call center_viewport_on_cursor(editor)
            call render_screen(buffer, editor)

            if (.not. replace_all) then
                ! Ask for confirmation
                call terminal_move_cursor(editor%screen_rows, 1)
                prompt = 'Replace? (y/n/a/q): '
                call terminal_write(trim(prompt))
                call terminal_show_cursor()

                ! Get response
                do
                    ch = terminal_read_char()
                    if (ch > 0) then
                        response = char(ch)
                        exit
                    end if
                end do

                call terminal_hide_cursor()
                call terminal_move_cursor(editor%screen_rows, 1)
                call terminal_write(repeat(' ', editor%screen_cols))

                select case(response)
                case('y', 'Y')
                    ! Replace this occurrence
                    call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                          find_pattern, replace_text)
                    replace_count = replace_count + 1

                case('n', 'N')
                    ! Skip this occurrence

                case('a', 'A')
                    ! Replace all remaining
                    replace_all = .true.
                    call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                          find_pattern, replace_text)
                    replace_count = replace_count + 1

                case('q', 'Q', char(27))  ! q or ESC
                    ! Quit replacing
                    exit

                case default
                    ! Invalid response, skip
                end select
            else
                ! Replace without asking
                call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                      find_pattern, replace_text)
                replace_count = replace_count + 1
            end if

            ! Clear selection
            editor%cursors(editor%active_cursor)%has_selection = .false.

            ! Find next match (starting after the replacement); the scan
            ! position is in bytes, the cursor column in chars
            start_line = editor%cursors(editor%active_cursor)%line
            start_col = line_byte_col(buffer, start_line, &
                                      editor%cursors(editor%active_cursor)%column)

            call find_next_match(buffer, find_pattern, start_line, start_col, &
                               found, found_line, found_col)

            ! Check if we've wrapped around to the beginning
            if (found .and. found_line <= start_line .and. found_col < start_col) then
                exit
            end if
        end do
    end subroutine execute_replace

    subroutine perform_replacement(buffer, cursor, find_pattern, replace_text)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: find_pattern, replace_text
        character(len=:), allocatable :: line, new_line
        integer :: col_char, col, i

        ! Get current line
        line = buffer_get_line(buffer, cursor%line)

        ! Build new line with replacement; slicing works in bytes,
        ! selection_start_col is a char column
        col_char = cursor%selection_start_col
        col = utf8_char_to_byte_index(line, col_char)
        if (col == 0) col = len(line) + 1
        allocate(character(len=len(line) - len(find_pattern) + len(replace_text)) :: new_line)

        ! Copy part before match
        if (col > 1) then
            new_line(1:col-1) = line(1:col-1)
        end if

        ! Insert replacement text
        if (len(replace_text) > 0) then
            new_line(col:col+len(replace_text)-1) = replace_text
        end if

        ! Copy part after match
        if (col + len(find_pattern) <= len(line)) then
            new_line(col+len(replace_text):) = line(col+len(find_pattern):)
        end if

        ! Delete old line content
        cursor%column = 1
        do i = 1, len(line)
            call buffer_delete_at_cursor(buffer, cursor)
        end do

        ! Insert new line content
        do i = 1, len(new_line)
            call buffer_insert_char(buffer, cursor, new_line(i:i))
            cursor%column = cursor%column + 1
        end do

        ! Position cursor after replacement (char column)
        cursor%column = col_char + utf8_char_count(replace_text)
        cursor%desired_column = cursor%column

        buffer%modified = .true.

        if (allocated(line)) deallocate(line)
        if (allocated(new_line)) deallocate(new_line)
    end subroutine perform_replacement

    subroutine buffer_delete_at_cursor(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: pos

        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        if (pos > 0 .and. pos <= get_buffer_content_size(buffer)) then
            call buffer_delete(buffer, pos, 1)
        end if
    end subroutine buffer_delete_at_cursor

    subroutine buffer_insert_char(buffer, cursor, ch)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        character, intent(in) :: ch
        integer :: pos

        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, ch)
    end subroutine buffer_insert_char

    function get_buffer_position(buffer, line, column) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line, column
        integer :: pos
        integer :: current_line, i
        character :: ch

        pos = 1
        current_line = 1

        do i = 1, get_buffer_content_size(buffer)
            if (current_line == line .and. pos == column) then
                return
            end if

            ch = buffer_get_char(buffer, i)
            if (ch == char(10)) then
                if (current_line == line) then
                    return
                end if
                current_line = current_line + 1
                pos = 1
            else if (current_line == line) then
                pos = pos + 1
            end if
        end do

        if (current_line == line) then
            pos = i
        else
            pos = get_buffer_content_size(buffer) + 1
        end if
    end function get_buffer_position

    function get_buffer_content_size(buffer) result(size)
        type(buffer_t), intent(in) :: buffer
        integer :: size

        size = buffer%size - (buffer%gap_end - buffer%gap_start)
    end function get_buffer_content_size

    ! Convert a 1-based char column on a buffer line to a byte column
    function line_byte_col(buffer, line_num, char_col) result(byte_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, char_col
        integer :: byte_col
        character(len=:), allocatable :: line

        if (char_col <= 1) then
            byte_col = char_col
            return
        end if
        line = buffer_get_line(buffer, line_num)
        byte_col = utf8_char_to_byte_index(line, char_col)
        if (byte_col == 0) byte_col = len(line) + 1
    end function line_byte_col

    ! Convert a 1-based byte column on a buffer line to a char column
    function line_char_col(buffer, line_num, byte_col) result(char_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, byte_col
        integer :: char_col
        character(len=:), allocatable :: line

        if (byte_col <= 1) then
            char_col = byte_col
            return
        end if
        line = buffer_get_line(buffer, line_num)
        char_col = utf8_byte_to_char_index(line, byte_col)
        if (char_col == 0) char_col = utf8_char_count(line) + 1
    end function line_char_col

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        integer :: count_rate, count_start, count_end
        real :: elapsed_time

        call system_clock(count_rate=count_rate)
        call system_clock(count=count_start)

        do
            call system_clock(count=count_end)
            elapsed_time = real(count_end - count_start) / real(count_rate) * 1000.0
            if (elapsed_time >= milliseconds) exit
        end do
    end subroutine sleep_ms

end module replace_prompt_module