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

    ! Search options
    logical :: case_sensitive = .false.
    logical :: whole_word = .false.
    integer :: total_matches = 0
    integer :: current_match_index = 0

contains

    subroutine show_search_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: input_buffer
        character(len=128) :: prompt
        integer :: input_pos, ch
        logical :: found
        integer :: found_line, found_col
        logical :: in_alt_sequence

        ! Initialize
        input_buffer = ''
        input_pos = 0
        in_alt_sequence = .false.

        ! Build prompt with options
        call build_search_prompt(prompt)

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
            else if (ch == 27) then  ! ESC
                ! Check if this is an Alt sequence or standalone ESC
                in_alt_sequence = .true.
                ch = terminal_read_char()

                if (ch == -1 .or. ch == 27) then
                    ! Standalone ESC - cancel search
                    if (allocated(current_search_pattern)) then
                        deallocate(current_search_pattern)
                    end if
                    exit
                else if (ch == iachar('c') .or. ch == iachar('C')) then
                    ! Alt+C - toggle case sensitive
                    case_sensitive = .not. case_sensitive
                    call build_search_prompt(prompt)
                    call terminal_move_cursor(editor%screen_rows, 1)
                    ! Clear the entire line first to avoid duplicate text
                    call terminal_write(repeat(' ', editor%screen_cols))
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(prompt // input_buffer(1:input_pos))
                    call terminal_move_cursor(editor%screen_rows, len_trim(prompt) + input_pos + 1)
                    in_alt_sequence = .false.
                else if (ch == iachar('w') .or. ch == iachar('W')) then
                    ! Alt+W - toggle whole word
                    whole_word = .not. whole_word
                    call build_search_prompt(prompt)
                    call terminal_move_cursor(editor%screen_rows, 1)
                    ! Clear the entire line first to avoid duplicate text
                    call terminal_write(repeat(' ', editor%screen_cols))
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(prompt // input_buffer(1:input_pos))
                    call terminal_move_cursor(editor%screen_rows, len_trim(prompt) + input_pos + 1)
                    in_alt_sequence = .false.
                else
                    ! Unknown Alt sequence, ignore
                    in_alt_sequence = .false.
                end if
            else if (ch == 10 .or. ch == 13) then  ! Enter - accept
                if (input_pos > 0) then
                    ! Save search pattern
                    if (allocated(current_search_pattern)) then
                        deallocate(current_search_pattern)
                    end if
                    allocate(character(len=input_pos) :: current_search_pattern)
                    current_search_pattern = input_buffer(1:input_pos)

                    ! Count all matches first
                    call count_all_matches(buffer, current_search_pattern)

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

                    ! Incremental search with reduced pattern
                    if (input_pos > 0) then
                        call perform_incremental_search(editor, buffer, input_buffer(1:input_pos))
                    end if
                end if
            else if (ch >= 32 .and. ch <= 126) then  ! Printable characters
                if (input_pos < 256) then
                    input_pos = input_pos + 1
                    input_buffer(input_pos:input_pos) = char(ch)
                    call terminal_write(char(ch))

                    ! Incremental search - search as user types
                    if (input_pos > 0) then
                        call perform_incremental_search(editor, buffer, input_buffer(1:input_pos))
                    end if
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

        ! Re-count matches in case search options changed
        call count_all_matches(buffer, current_search_pattern)

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

        ! Re-count matches in case search options changed
        call count_all_matches(buffer, current_search_pattern)

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
                call find_pattern_in_line(line(search_col:), pattern, pos)
                if (pos > 0) then
                    found_col = search_col + pos - 1

                    ! Check whole word constraint if enabled
                    if (whole_word) then
                        if (.not. is_whole_word_match(line, found_col, len(pattern))) then
                            ! Not a whole word match, continue searching
                            search_col = found_col + 1
                            if (search_col <= len(line)) then
                                cycle
                            end if
                        end if
                    end if

                    found = .true.
                    found_line = current_line
                    if (allocated(line)) deallocate(line)
                    ! Update match index
                    call update_match_index(buffer, pattern, found_line, found_col)
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
                    call find_pattern_in_line(line(1:start_col-1), pattern, pos)
                else
                    pos = 0
                end if
            else
                call find_pattern_in_line(line, pattern, pos)
            end if

            if (pos > 0) then
                ! Check whole word constraint if enabled
                if (whole_word) then
                    if (.not. is_whole_word_match(line, pos, len(pattern))) then
                        ! Not a whole word match, skip
                        if (allocated(line)) deallocate(line)
                        cycle
                    end if
                end if

                found = .true.
                found_line = current_line
                found_col = pos
                if (allocated(line)) deallocate(line)
                ! Update match index
                call update_match_index(buffer, pattern, found_line, found_col)
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
                call find_pattern_in_line(line(pos:), pattern, found_col)
                if (found_col > 0 .and. pos + found_col - 1 <= check_col) then
                    ! Check whole word constraint if enabled
                    if (whole_word) then
                        if (is_whole_word_match(line, pos + found_col - 1, len(pattern))) then
                            last_pos = pos + found_col - 1
                        end if
                    else
                        last_pos = pos + found_col - 1
                    end if
                    pos = pos + found_col  ! Skip past this match
                else
                    exit
                end if
            end do

            if (last_pos > 0) then
                found = .true.
                found_line = current_line
                found_col = last_pos
                if (allocated(line)) deallocate(line)
                ! Update match index
                call update_match_index(buffer, pattern, found_line, found_col)
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
                call find_pattern_in_line(line(pos:), pattern, found_col)
                if (found_col > 0) then
                    ! Check whole word constraint if enabled
                    if (whole_word) then
                        if (is_whole_word_match(line, pos + found_col - 1, len(pattern))) then
                            last_pos = pos + found_col - 1
                        end if
                    else
                        last_pos = pos + found_col - 1
                    end if
                    pos = pos + found_col  ! Skip past this match
                else
                    exit
                end if
            end do

            if (last_pos > 0) then
                found = .true.
                found_line = current_line
                found_col = last_pos
                if (allocated(line)) deallocate(line)
                ! Update match index
                call update_match_index(buffer, pattern, found_line, found_col)
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

    subroutine build_search_prompt(prompt)
        character(len=*), intent(out) :: prompt
        character(len=64) :: options
        character(len=32) :: count_str

        options = ''

        ! Build options string
        if (case_sensitive) then
            options = trim(options) // '[Cc]'
        else
            options = trim(options) // '[cc]'
        end if

        if (whole_word) then
            options = trim(options) // '[Ww]'
        else
            options = trim(options) // '[ww]'
        end if

        ! Add match count if we have a pattern and matches
        if (allocated(current_search_pattern) .and. total_matches > 0) then
            write(count_str, '(I0,A,I0)') current_match_index, ' of ', total_matches
            options = trim(options) // ' (' // trim(count_str) // ')'
        end if

        ! Build full prompt with ESC indicator
        if (len_trim(options) > 0) then
            prompt = 'Search ' // trim(options) // ' | ESC:exit: '
        else
            prompt = 'Search | ESC:exit: '
        end if
    end subroutine build_search_prompt

    subroutine find_pattern_in_line(line, pattern, pos)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: pattern
        integer, intent(out) :: pos
        character(len=:), allocatable :: search_line
        character(len=:), allocatable :: search_pattern
        integer :: i

        pos = 0

        if (len(line) == 0 .or. len(pattern) == 0) return
        if (len(pattern) > len(line)) return

        ! Handle case sensitivity
        if (case_sensitive) then
            ! Direct search
            pos = index(line, pattern)
        else
            ! Case-insensitive search - convert both to lowercase
            allocate(character(len=len(line)) :: search_line)
            allocate(character(len=len(pattern)) :: search_pattern)

            ! Convert line to lowercase
            do i = 1, len(line)
                if (iachar(line(i:i)) >= iachar('A') .and. &
                    iachar(line(i:i)) <= iachar('Z')) then
                    search_line(i:i) = char(iachar(line(i:i)) + 32)
                else
                    search_line(i:i) = line(i:i)
                end if
            end do

            ! Convert pattern to lowercase
            do i = 1, len(pattern)
                if (iachar(pattern(i:i)) >= iachar('A') .and. &
                    iachar(pattern(i:i)) <= iachar('Z')) then
                    search_pattern(i:i) = char(iachar(pattern(i:i)) + 32)
                else
                    search_pattern(i:i) = pattern(i:i)
                end if
            end do

            pos = index(search_line, search_pattern)

            deallocate(search_line)
            deallocate(search_pattern)
        end if
    end subroutine find_pattern_in_line

    logical function is_whole_word_match(line, start_pos, pattern_len)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_pos, pattern_len
        logical :: word_start, word_end
        integer :: end_pos

        end_pos = start_pos + pattern_len - 1

        ! Check if match is at word boundary (start)
        if (start_pos == 1) then
            word_start = .true.
        else
            word_start = .not. is_word_char(line(start_pos-1:start_pos-1))
        end if

        ! Check if match is at word boundary (end)
        if (end_pos == len(line)) then
            word_end = .true.
        else
            word_end = .not. is_word_char(line(end_pos+1:end_pos+1))
        end if

        is_whole_word_match = word_start .and. word_end
    end function is_whole_word_match

    logical function is_word_char(ch)
        character(len=1), intent(in) :: ch
        integer :: ascii_val

        ascii_val = iachar(ch)

        ! Check if character is alphanumeric or underscore
        is_word_char = (ascii_val >= iachar('A') .and. ascii_val <= iachar('Z')) .or. &
                      (ascii_val >= iachar('a') .and. ascii_val <= iachar('z')) .or. &
                      (ascii_val >= iachar('0') .and. ascii_val <= iachar('9')) .or. &
                      (ch == '_')
    end function is_word_char

    subroutine count_all_matches(buffer, pattern)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, col
        integer :: match_count

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        ! Go through all lines and count matches
        do current_line = 1, line_count
            line = buffer_get_line(buffer, current_line)
            col = 1

            do while (col <= len(line))
                call find_pattern_in_line(line(col:), pattern, pos)
                if (pos > 0) then
                    pos = col + pos - 1
                    ! Check whole word constraint if enabled
                    if (whole_word) then
                        if (is_whole_word_match(line, pos, len(pattern))) then
                            match_count = match_count + 1
                        end if
                    else
                        match_count = match_count + 1
                    end if
                    col = pos + 1
                else
                    exit
                end if
            end do

            if (allocated(line)) deallocate(line)
        end do

        total_matches = match_count
        current_match_index = 0  ! Will be set when we find first match
    end subroutine count_all_matches

    subroutine perform_incremental_search(editor, buffer, pattern)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col

        ! Search from current cursor position
        start_line = editor%cursors(editor%active_cursor)%line
        start_col = editor%cursors(editor%active_cursor)%column

        ! Find the first match
        call find_next_match(buffer, pattern, start_line, start_col, &
                           found, found_line, found_col)

        if (found) then
            ! Update viewport to show the match (but don't move cursor yet)
            editor%viewport_line = max(1, found_line - editor%screen_rows / 2)

            ! Store the match position for highlighting (could be used for visual feedback)
            last_search_line = found_line
            last_search_col = found_col
        end if
    end subroutine perform_incremental_search

    subroutine update_match_index(buffer, pattern, match_line, match_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: match_line, match_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, col
        integer :: match_count

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        ! Count matches until we reach the current one
        do current_line = 1, line_count
            line = buffer_get_line(buffer, current_line)
            col = 1

            do while (col <= len(line))
                call find_pattern_in_line(line(col:), pattern, pos)
                if (pos > 0) then
                    pos = col + pos - 1
                    ! Check whole word constraint if enabled
                    if (whole_word) then
                        if (is_whole_word_match(line, pos, len(pattern))) then
                            match_count = match_count + 1
                            if (current_line == match_line .and. pos == match_col) then
                                current_match_index = match_count
                                if (allocated(line)) deallocate(line)
                                return
                            end if
                        end if
                    else
                        match_count = match_count + 1
                        if (current_line == match_line .and. pos == match_col) then
                            current_match_index = match_count
                            if (allocated(line)) deallocate(line)
                            return
                        end if
                    end if
                    col = pos + 1
                else
                    exit
                end if
            end do

            if (allocated(line)) deallocate(line)
        end do
    end subroutine update_match_index

end module search_prompt_module