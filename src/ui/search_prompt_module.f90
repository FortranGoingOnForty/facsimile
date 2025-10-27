module search_prompt_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    implicit none
    private

    public :: show_search_prompt, search_forward, search_backward
    public :: current_search_pattern, clear_search_pattern
    public :: find_next_match, find_prev_match, center_viewport_on_cursor

    ! Module variables for search state
    character(len=:), allocatable :: current_search_pattern
    integer :: last_search_line = 1
    integer :: last_search_col = 1

contains

    subroutine show_search_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: input_buffer
        character(len=32) :: prompt
        integer :: input_pos, ch, ios
        logical :: found
        integer :: found_line, found_col

        ! Initialize
        input_buffer = ''
        input_pos = 0
        prompt = '/'

        ! Display prompt at bottom of screen
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
                ! Clear any existing search pattern
                if (allocated(current_search_pattern)) then
                    deallocate(current_search_pattern)
                end if
                exit
            else if (ch == 10 .or. ch == 13) then  ! Enter - accept
                if (input_pos > 0) then
                    ! Save search pattern
                    if (allocated(current_search_pattern)) then
                        deallocate(current_search_pattern)
                    end if
                    allocate(character(len=input_pos) :: current_search_pattern)
                    current_search_pattern = input_buffer(1:input_pos)

                    ! Search from current position
                    call find_next_match(buffer, current_search_pattern, &
                                        editor%cursors(editor%active_cursor)%line, &
                                        editor%cursors(editor%active_cursor)%column, &
                                        found, found_line, found_col)

                    if (found) then
                        ! Move cursor to match and select it
                        editor%cursors(editor%active_cursor)%line = found_line
                        editor%cursors(editor%active_cursor)%column = found_col
                        editor%cursors(editor%active_cursor)%desired_column = found_col

                        ! Create selection for the match
                        editor%cursors(editor%active_cursor)%has_selection = .true.
                        editor%cursors(editor%active_cursor)%selection_start_line = found_line
                        editor%cursors(editor%active_cursor)%selection_start_col = found_col
                        editor%cursors(editor%active_cursor)%column = found_col + len(current_search_pattern)

                        ! Update last search position
                        last_search_line = found_line
                        last_search_col = found_col

                        ! Center viewport on match
                        call center_viewport_on_cursor(editor)
                    else
                        ! Show "not found" message briefly
                        call terminal_move_cursor(editor%screen_rows, 1)
                        call terminal_write('Pattern not found: ' // current_search_pattern)
                        call flush(output_unit)
                        ! Note: In a real implementation, we'd want a better way to show this
                    end if
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
    end subroutine show_search_prompt

    subroutine search_forward(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col

        if (.not. allocated(current_search_pattern)) return

        ! Search from cursor position
        start_line = editor%cursors(editor%active_cursor)%line
        start_col = editor%cursors(editor%active_cursor)%column

        call find_next_match(buffer, current_search_pattern, &
                            start_line, start_col, &
                            found, found_line, found_col)

        if (found) then
            ! Move cursor to match and select it
            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%column = found_col
            editor%cursors(editor%active_cursor)%desired_column = found_col

            ! Create selection for the match
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = found_col
            editor%cursors(editor%active_cursor)%column = found_col + len(current_search_pattern)

            ! Update last search position
            last_search_line = found_line
            last_search_col = found_col

            ! Update viewport
            call center_viewport_on_cursor(editor)
        end if
    end subroutine search_forward

    subroutine search_backward(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col

        if (.not. allocated(current_search_pattern)) return

        ! Search backward from cursor position
        start_line = editor%cursors(editor%active_cursor)%line
        start_col = max(1, editor%cursors(editor%active_cursor)%column - 1)

        call find_prev_match(buffer, current_search_pattern, &
                            start_line, start_col, &
                            found, found_line, found_col)

        if (found) then
            ! Move cursor to match and select it
            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%column = found_col
            editor%cursors(editor%active_cursor)%desired_column = found_col

            ! Create selection for the match
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = found_col
            editor%cursors(editor%active_cursor)%column = found_col + len(current_search_pattern)

            ! Update last search position
            last_search_line = found_line
            last_search_col = found_col

            ! Update viewport
            call center_viewport_on_cursor(editor)
        end if
    end subroutine search_backward

    subroutine find_next_match(buffer, pattern, start_line, start_col, &
                               found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos
        integer :: search_col

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)

        ! Search from current position to end
        do current_line = start_line, line_count
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                search_col = start_col + 1
            else
                search_col = 1
            end if

            if (search_col <= len(line)) then
                pos = index(line(search_col:), pattern)
                if (pos > 0) then
                    found = .true.
                    found_line = current_line
                    found_col = search_col + pos - 1
                    if (allocated(line)) deallocate(line)
                    return
                end if
            end if
            if (allocated(line)) deallocate(line)
        end do

        ! Wrap around to beginning
        do current_line = 1, start_line
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                ! Search only up to start position
                if (start_col > 1) then
                    pos = index(line(1:start_col-1), pattern)
                else
                    pos = 0
                end if
            else
                pos = index(line, pattern)
            end if

            if (pos > 0) then
                found = .true.
                found_line = current_line
                found_col = pos
                if (allocated(line)) deallocate(line)
                return
            end if

            if (allocated(line)) deallocate(line)
        end do
    end subroutine find_next_match

    subroutine find_prev_match(buffer, pattern, start_line, start_col, &
                               found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line
        integer :: pos, last_pos, check_col

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)

        ! Search backward from current position
        do current_line = start_line, 1, -1
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                check_col = min(start_col, len(line))
            else
                check_col = len(line)
            end if

            ! Find last occurrence before check_col
            last_pos = 0
            pos = 1
            do while (pos <= check_col - len(pattern) + 1)
                if (line(pos:pos+len(pattern)-1) == pattern) then
                    last_pos = pos
                end if
                pos = pos + 1
            end do

            if (last_pos > 0) then
                found = .true.
                found_line = current_line
                found_col = last_pos
                if (allocated(line)) deallocate(line)
                return
            end if

            if (allocated(line)) deallocate(line)
        end do

        ! Wrap around from end
        do current_line = line_count, start_line, -1
            if (current_line <= start_line) exit

            line = buffer_get_line(buffer, current_line)

            ! Find last occurrence in line
            last_pos = 0
            pos = 1
            do while (pos <= len(line) - len(pattern) + 1)
                if (line(pos:pos+len(pattern)-1) == pattern) then
                    last_pos = pos
                end if
                pos = pos + 1
            end do

            if (last_pos > 0) then
                found = .true.
                found_line = current_line
                found_col = last_pos
                if (allocated(line)) deallocate(line)
                return
            end if

            if (allocated(line)) deallocate(line)
        end do
    end subroutine find_prev_match

    subroutine center_viewport_on_cursor(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: cursor_line
        integer :: viewport_height

        cursor_line = editor%cursors(editor%active_cursor)%line
        viewport_height = editor%screen_rows - 2  ! Account for status bar

        ! Center the cursor in the viewport
        editor%viewport_line = max(1, cursor_line - viewport_height / 2)
    end subroutine center_viewport_on_cursor

    subroutine clear_search_pattern()
        if (allocated(current_search_pattern)) then
            deallocate(current_search_pattern)
        end if
        last_search_line = 1
        last_search_col = 1
    end subroutine clear_search_pattern

end module search_prompt_module