module unified_search_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    use regex_module
    use renderer_module, only: render_screen
    implicit none
    private

    public :: show_unified_search_prompt
    public :: current_search_pattern, clear_search_pattern
    public :: find_next_match, find_prev_match, center_viewport_on_cursor

    ! Module variables for search/replace state
    character(len=:), allocatable :: current_search_pattern
    character(len=:), allocatable :: current_replace_text
    integer :: last_search_line = 1
    integer :: last_search_col = 1

    ! Search options
    logical :: case_sensitive = .false.
    logical :: whole_word = .false.
    logical :: use_regex = .false.
    integer :: total_matches = 0
    integer :: current_match_index = 0

    ! Compiled regex ID (when regex mode is active)
    integer :: compiled_regex_id = -1

    ! Length of last match (needed for regex where match length != pattern length)
    integer :: last_match_length = 0

    ! Active search mode - persists after first search
    logical :: search_mode_active = .false.

    ! Field focus (1 = find, 2 = replace)
    integer :: active_field = 1

contains

    subroutine show_unified_search_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: find_buffer, replace_buffer
        character(len=256) :: prompt
        integer :: find_pos, replace_pos, ch
        logical :: found, in_alt_sequence
        integer :: found_line, found_col

        ! Initialize
        find_buffer = ''
        replace_buffer = ''
        find_pos = 0
        replace_pos = 0
        active_field = 1  ! Start with find field
        in_alt_sequence = .false.

        ! If we have existing patterns, load them
        if (allocated(current_search_pattern)) then
            find_buffer = current_search_pattern
            find_pos = len(current_search_pattern)
        end if
        if (allocated(current_replace_text)) then
            replace_buffer = current_replace_text
            replace_pos = len(current_replace_text)
        end if

        ! Build and display prompt
        call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
        call display_prompt(editor, prompt, find_pos, replace_pos)

        ! Input loop
        do
            ch = terminal_read_char()

            if (ch == -1) then
                cycle
            else if (ch == 27) then  ! ESC or Alt sequence
                in_alt_sequence = .true.
                ch = terminal_read_char()

                if (ch == -1 .or. ch == 27) then
                    ! Standalone ESC - exit search mode
                    search_mode_active = .false.
                    exit
                else if (ch == iachar('[')) then
                    ! Could be mouse event ESC [ < ... - consume and ignore
                    ch = terminal_read_char()
                    if (ch == iachar('<')) then
                        ! Mouse event - consume until 'M' or 'm'
                        do
                            ch = terminal_read_char()
                            if (ch == iachar('M') .or. ch == iachar('m') .or. ch == -1) exit
                        end do
                        in_alt_sequence = .false.
                        cycle
                    end if
                    ! Not a mouse event, fall through
                    in_alt_sequence = .false.
                    cycle
                else if (ch == iachar('c') .or. ch == iachar('C')) then
                    ! Alt+C - toggle case sensitive
                    case_sensitive = .not. case_sensitive
                    call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                    call display_prompt(editor, prompt, find_pos, replace_pos)
                    in_alt_sequence = .false.
                else if (ch == iachar('w') .or. ch == iachar('W')) then
                    ! Alt+W - toggle whole word
                    whole_word = .not. whole_word
                    call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                    call display_prompt(editor, prompt, find_pos, replace_pos)
                    in_alt_sequence = .false.
                else if (ch == iachar('r') .or. ch == iachar('R')) then
                    ! Alt+R - toggle regex mode
                    use_regex = .not. use_regex
                    call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                    call display_prompt(editor, prompt, find_pos, replace_pos)
                    in_alt_sequence = .false.
                else
                    in_alt_sequence = .false.
                end if
            else if (ch == 9) then  ! Tab - switch fields
                if (active_field == 1) then
                    active_field = 2
                else
                    active_field = 1
                end if
                call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                call display_prompt(editor, prompt, find_pos, replace_pos)
            else if (ch == 6) then  ! Ctrl+F - find next
                if (find_pos > 0) then
                    ! Save search pattern
                    if (allocated(current_search_pattern)) deallocate(current_search_pattern)
                    allocate(character(len=find_pos) :: current_search_pattern)
                    current_search_pattern = find_buffer(1:find_pos)

                    if (.not. search_mode_active) then
                        ! First search - count and find
                        search_mode_active = .true.
                        call count_all_matches(buffer, current_search_pattern)
                        call perform_search(editor, buffer, current_search_pattern)
                    else
                        ! Cycle to next match
                        call search_forward(editor, buffer)
                    end if

                    ! Update prompt with match count
                    call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                    call display_prompt(editor, prompt, find_pos, replace_pos)
                end if
            else if (ch == 18) then  ! Ctrl+R - replace current and advance
                if (find_pos > 0 .and. replace_pos >= 0) then
                    ! Save patterns
                    if (allocated(current_search_pattern)) deallocate(current_search_pattern)
                    if (allocated(current_replace_text)) deallocate(current_replace_text)
                    allocate(character(len=find_pos) :: current_search_pattern)
                    allocate(character(len=replace_pos) :: current_replace_text)
                    current_search_pattern = find_buffer(1:find_pos)
                    current_replace_text = replace_buffer(1:replace_pos)

                    call replace_current_and_advance(editor, buffer)

                    ! Update prompt
                    call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                    call display_prompt(editor, prompt, find_pos, replace_pos)
                end if
            else if (ch == 1) then  ! Ctrl+A - replace all
                if (find_pos > 0 .and. replace_pos >= 0) then
                    ! Save patterns
                    if (allocated(current_search_pattern)) deallocate(current_search_pattern)
                    if (allocated(current_replace_text)) deallocate(current_replace_text)
                    allocate(character(len=find_pos) :: current_search_pattern)
                    allocate(character(len=replace_pos) :: current_replace_text)
                    current_search_pattern = find_buffer(1:find_pos)
                    current_replace_text = replace_buffer(1:replace_pos)

                    call replace_all_matches(editor, buffer)

                    ! Exit after replace all
                    search_mode_active = .false.
                    exit
                end if
            else if (ch == 13 .or. ch == 10) then  ! Enter - accept current match and exit
                ! Keep the cursor at the current match position
                ! If there's a selection, keep it (user can clear with ESC or arrow keys)
                exit
            else if (ch == 127 .or. ch == 8) then  ! Backspace
                if (active_field == 1 .and. find_pos > 0) then
                    find_pos = find_pos - 1
                else if (active_field == 2 .and. replace_pos > 0) then
                    replace_pos = replace_pos - 1
                end if
                call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                call display_prompt(editor, prompt, find_pos, replace_pos)
            else if (ch >= 32 .and. ch <= 126) then  ! Printable characters
                if (active_field == 1 .and. find_pos < 256) then
                    find_pos = find_pos + 1
                    find_buffer(find_pos:find_pos) = char(ch)
                else if (active_field == 2 .and. replace_pos < 256) then
                    replace_pos = replace_pos + 1
                    replace_buffer(replace_pos:replace_pos) = char(ch)
                end if
                call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                call display_prompt(editor, prompt, find_pos, replace_pos)
            end if
        end do

        ! Clean up
        call terminal_hide_cursor()
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))
    end subroutine show_unified_search_prompt

    subroutine build_unified_prompt(prompt, find_text, find_len, replace_text, replace_len)
        character(len=*), intent(out) :: prompt
        character(len=*), intent(in) :: find_text, replace_text
        integer, intent(in) :: find_len, replace_len
        character(len=64) :: options, count_str
        character(len=25) :: find_field, replace_field
        character(len=1) :: esc = char(27)
        integer :: i
        integer, parameter :: FIELD_WIDTH = 20  ! Reduced from 30 for narrower terminals

        ! Build options string with all three toggles
        options = ''
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
        if (use_regex) then
            options = trim(options) // '[Rr]'
        else
            options = trim(options) // '[rr]'
        end if

        ! Add match count if available
        if (allocated(current_search_pattern) .and. total_matches > 0) then
            write(count_str, '(A,I0,A,I0,A)') ' (', current_match_index, '/', total_matches, ')'
            options = trim(options) // trim(count_str)
        end if

        ! Build fixed-width fields with padding (20 chars each for compact display)
        find_field = find_text(1:min(find_len, FIELD_WIDTH))
        do i = find_len + 1, FIELD_WIDTH
            find_field(i:i) = ' '
        end do

        replace_field = replace_text(1:min(replace_len, FIELD_WIDTH))
        do i = replace_len + 1, FIELD_WIDTH
            replace_field(i:i) = ' '
        end do

        ! Build unified prompt with reverse video highlighting for active field
        if (active_field == 1) then
            ! Find field active (reverse video)
            write(prompt, '(9A)') &
                esc, '[7m[f]:', find_field, esc, '[27m /[r]:', &
                replace_field, ' ', trim(options), ' RET:go ESC:exit'
        else
            ! Replace field active (reverse video)
            write(prompt, '(10A)') &
                '[f]:', find_field, ' ', esc, '[7m/[r]:', &
                replace_field, esc, '[27m ', trim(options), ' RET:go ESC:exit'
        end if
    end subroutine build_unified_prompt

    subroutine display_prompt(editor, prompt, find_len, replace_len)
        type(editor_state_t), intent(in) :: editor
        character(len=*), intent(in) :: prompt
        integer, intent(in) :: find_len, replace_len
        integer :: cursor_pos

        ! Hide cursor during redraw to prevent flicker
        call terminal_hide_cursor()

        ! Clear the entire status line
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))

        ! Move back to start and write prompt
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(trim(prompt))

        ! Calculate cursor position within the active field
        ! Account for escape sequences which don't take screen space
        ! Compact layout: "[f]: <20 chars> /[r]: <20 chars> ..."
        if (active_field == 1) then
            ! Cursor in find field: "[f]:" = 4 visible chars
            cursor_pos = 4 + find_len + 1
        else
            ! Cursor in replace field
            ! "[f]:" (4) + field (20) + " /[r]:" (6)
            cursor_pos = 4 + 20 + 6 + replace_len + 1
        end if

        ! Position cursor and show it
        call terminal_move_cursor(editor%screen_rows, cursor_pos)
        call terminal_show_cursor()
    end subroutine display_prompt

    subroutine perform_search(editor, buffer, pattern)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: pattern
        logical :: found
        integer :: found_line, found_col

        ! Compile regex if in regex mode
        if (use_regex) then
            ! Free old regex if any
            if (compiled_regex_id >= 0) then
                call regex_free(compiled_regex_id)
            end if
            ! Compile new pattern
            compiled_regex_id = regex_compile(pattern, case_sensitive)
            if (compiled_regex_id < 0) then
                ! Regex compilation failed - could show error, for now just skip
                return
            end if
        end if

        call find_next_match(buffer, pattern, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column, &
                            found, found_line, found_col)

        if (found) then
            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%column = found_col
            editor%cursors(editor%active_cursor)%desired_column = found_col

            ! Create selection
            ! For regex, use the match length from last search
            ! For normal search, use pattern length
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = found_col
            if (use_regex .and. last_match_length > 0) then
                editor%cursors(editor%active_cursor)%column = found_col + last_match_length
            else
                editor%cursors(editor%active_cursor)%column = found_col + len(pattern)
            end if

            last_search_line = found_line
            last_search_col = found_col

            call center_viewport_on_cursor(editor)
            call render_screen(buffer, editor)
        end if
    end subroutine perform_search

    subroutine search_forward(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col

        if (.not. allocated(current_search_pattern)) return

        ! Re-count matches
        call count_all_matches(buffer, current_search_pattern)

        ! Search from cursor position
        start_line = editor%cursors(editor%active_cursor)%line
        start_col = editor%cursors(editor%active_cursor)%column

        call find_next_match(buffer, current_search_pattern, &
                            start_line, start_col, &
                            found, found_line, found_col)

        if (found) then
            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%column = found_col
            editor%cursors(editor%active_cursor)%desired_column = found_col

            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = found_col

            ! Use last_match_length for regex (which was set by find_next_match)
            if (use_regex .and. last_match_length > 0) then
                editor%cursors(editor%active_cursor)%column = found_col + last_match_length
            else
                editor%cursors(editor%active_cursor)%column = found_col + len(current_search_pattern)
            end if

            last_search_line = found_line
            last_search_col = found_col

            call center_viewport_on_cursor(editor)
            call render_screen(buffer, editor)
        end if
    end subroutine search_forward

    subroutine replace_current_and_advance(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer

        if (.not. allocated(current_search_pattern)) return
        if (.not. allocated(current_replace_text)) return

        ! If cursor has selection, replace it
        if (editor%cursors(editor%active_cursor)%has_selection) then
            call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                    current_replace_text, last_match_length)

            ! Clear selection after replacement
            editor%cursors(editor%active_cursor)%has_selection = .false.

            ! Re-count matches after replacement
            call count_all_matches(buffer, current_search_pattern)

            ! Render to show the replacement (without selection)
            call render_screen(buffer, editor)
        end if
    end subroutine replace_current_and_advance

    subroutine replace_all_matches(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col, replace_count
        type(cursor_t) :: temp_cursor

        if (.not. allocated(current_search_pattern)) return
        if (.not. allocated(current_replace_text)) return

        replace_count = 0
        temp_cursor = editor%cursors(editor%active_cursor)

        ! Start from beginning
        temp_cursor%line = 1
        temp_cursor%column = 0

        do
            call find_next_match(buffer, current_search_pattern, &
                                temp_cursor%line, temp_cursor%column, &
                                found, found_line, found_col)

            if (.not. found) exit

            ! Move cursor to match and select it
            temp_cursor%line = found_line
            temp_cursor%column = found_col
            temp_cursor%has_selection = .true.
            temp_cursor%selection_start_line = found_line
            temp_cursor%selection_start_col = found_col
            temp_cursor%column = found_col + last_match_length

            ! Replace
            call perform_replacement(buffer, temp_cursor, current_replace_text, last_match_length)

            replace_count = replace_count + 1

            ! Guard against infinite loop
            if (replace_count > 10000) exit
        end do

        ! Update main cursor
        editor%cursors(editor%active_cursor) = temp_cursor
    end subroutine replace_all_matches

    subroutine perform_replacement(buffer, cursor, replace_text, match_len)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: replace_text
        integer, intent(in) :: match_len
        character(len=:), allocatable :: line, new_line
        integer :: col, i

        ! Get current line
        line = buffer_get_line(buffer, cursor%line)

        ! Build new line with replacement
        col = cursor%selection_start_col
        allocate(character(len=len(line) - match_len + len(replace_text)) :: new_line)

        ! Copy part before match
        if (col > 1) then
            new_line(1:col-1) = line(1:col-1)
        end if

        ! Insert replacement text
        if (len(replace_text) > 0) then
            new_line(col:col+len(replace_text)-1) = replace_text
        end if

        ! Copy part after match
        if (col + match_len <= len(line)) then
            new_line(col+len(replace_text):) = line(col+match_len:)
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

        ! Position cursor after replacement
        cursor%column = col + len(replace_text)
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

    ! Include all helper functions from search_prompt_module
    ! (find_next_match, find_prev_match, count_all_matches, etc.)
    ! For brevity, I'll add a note that these need to be copied over

    subroutine clear_search_pattern()
        if (allocated(current_search_pattern)) deallocate(current_search_pattern)
        if (allocated(current_replace_text)) deallocate(current_replace_text)
        search_mode_active = .false.
        last_search_line = 1
        last_search_col = 1

        ! Free compiled regex if any
        if (compiled_regex_id >= 0) then
            call regex_free(compiled_regex_id)
            compiled_regex_id = -1
        end if
        last_match_length = 0
    end subroutine clear_search_pattern

    ! Search helper functions
    subroutine find_next_match(buffer, pattern, start_line, start_col, found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, search_col
        integer :: match_len

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)
        last_match_length = 0

        ! Search from current position to end
        do current_line = start_line, line_count
            line = buffer_get_line(buffer, current_line)
            if (current_line == start_line) then
                search_col = start_col + 1
            else
                search_col = 1
            end if
            if (search_col <= len(line)) then
                if (use_regex) then
                    call find_regex_in_line(line(search_col:), compiled_regex_id, pos, match_len)
                else
                    call find_pattern_in_line(line(search_col:), pattern, pos)
                    match_len = len(pattern)
                end if
                if (pos > 0) then
                    found_col = search_col + pos - 1
                    if (whole_word) then
                        if (.not. is_whole_word_match(line, found_col, match_len)) then
                            if (allocated(line)) deallocate(line)
                            cycle
                        end if
                    end if
                    found = .true.
                    found_line = current_line
                    last_match_length = match_len
                    if (allocated(line)) deallocate(line)
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
                if (start_col > 1) then
                    if (use_regex) then
                        call find_regex_in_line(line(1:start_col-1), compiled_regex_id, pos, match_len)
                    else
                        call find_pattern_in_line(line(1:start_col-1), pattern, pos)
                        match_len = len(pattern)
                    end if
                else
                    pos = 0
                    match_len = 0
                end if
            else
                if (use_regex) then
                    call find_regex_in_line(line, compiled_regex_id, pos, match_len)
                else
                    call find_pattern_in_line(line, pattern, pos)
                    match_len = len(pattern)
                end if
            end if
            if (pos > 0) then
                if (whole_word) then
                    if (.not. is_whole_word_match(line, pos, match_len)) then
                        if (allocated(line)) deallocate(line)
                        cycle
                    end if
                end if
                found = .true.
                found_line = current_line
                found_col = pos
                last_match_length = match_len
                if (allocated(line)) deallocate(line)
                call update_match_index(buffer, pattern, found_line, found_col)
                return
            end if
            if (allocated(line)) deallocate(line)
        end do
    end subroutine find_next_match

    subroutine find_prev_match(buffer, pattern, start_line, start_col, found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, last_pos, check_col
        integer :: match_len, last_match_len

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)
        last_match_length = 0

        ! Search backward from current position
        do current_line = start_line, 1, -1
            line = buffer_get_line(buffer, current_line)
            if (current_line == start_line) then
                check_col = min(start_col, len(line))
            else
                check_col = len(line)
            end if

            last_pos = 0
            last_match_len = 0
            pos = 1
            do while (pos <= check_col)
                if (use_regex) then
                    call find_regex_in_line(line(pos:), compiled_regex_id, found_col, match_len)
                else
                    call find_pattern_in_line(line(pos:), pattern, found_col)
                    match_len = len(pattern)
                end if
                if (found_col > 0 .and. pos + found_col - 1 <= check_col) then
                    if (whole_word) then
                        if (is_whole_word_match(line, pos + found_col - 1, match_len)) then
                            last_pos = pos + found_col - 1
                            last_match_len = match_len
                        end if
                    else
                        last_pos = pos + found_col - 1
                        last_match_len = match_len
                    end if
                    pos = pos + found_col
                else
                    exit
                end if
            end do

            if (last_pos > 0) then
                found = .true.
                found_line = current_line
                found_col = last_pos
                last_match_length = last_match_len
                if (allocated(line)) deallocate(line)
                call update_match_index(buffer, pattern, found_line, found_col)
                return
            end if
            if (allocated(line)) deallocate(line)
        end do
    end subroutine find_prev_match

    subroutine find_pattern_in_line(line, pattern, pos)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: pattern
        integer, intent(out) :: pos
        character(len=:), allocatable :: search_line, search_pattern
        integer :: i

        pos = 0
        if (len(line) == 0 .or. len(pattern) == 0) return
        if (len(pattern) > len(line)) return

        if (case_sensitive) then
            pos = index(line, pattern)
        else
            allocate(character(len=len(line)) :: search_line)
            allocate(character(len=len(pattern)) :: search_pattern)
            do i = 1, len(line)
                if (iachar(line(i:i)) >= iachar('A') .and. iachar(line(i:i)) <= iachar('Z')) then
                    search_line(i:i) = char(iachar(line(i:i)) + 32)
                else
                    search_line(i:i) = line(i:i)
                end if
            end do
            do i = 1, len(pattern)
                if (iachar(pattern(i:i)) >= iachar('A') .and. iachar(pattern(i:i)) <= iachar('Z')) then
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

    ! Find pattern using regex in a line
    ! Returns position where match starts (1-based), or 0 if not found
    ! Also returns match_len (length of what was matched)
    subroutine find_regex_in_line(line, regex_id, pos, match_len)
        character(len=*), intent(in) :: line
        integer, intent(in) :: regex_id
        integer, intent(out) :: pos, match_len
        logical :: found
        integer :: start_pos, len_matched

        if (regex_id < 0) then
            pos = 0
            match_len = 0
            return
        end if

        found = regex_match(regex_id, line, start_pos, len_matched)
        if (found) then
            pos = start_pos
            match_len = len_matched
        else
            pos = 0
            match_len = 0
        end if
    end subroutine find_regex_in_line

    logical function is_whole_word_match(line, start_pos, pattern_len)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_pos, pattern_len
        logical :: word_start, word_end
        integer :: end_pos

        end_pos = start_pos + pattern_len - 1
        if (start_pos == 1) then
            word_start = .true.
        else
            word_start = .not. is_word_char(line(start_pos-1:start_pos-1))
        end if
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
        is_word_char = (ascii_val >= iachar('A') .and. ascii_val <= iachar('Z')) .or. &
                      (ascii_val >= iachar('a') .and. ascii_val <= iachar('z')) .or. &
                      (ascii_val >= iachar('0') .and. ascii_val <= iachar('9')) .or. &
                      (ch == '_')
    end function is_word_char

    subroutine count_all_matches(buffer, pattern)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, col, match_count
        integer :: match_len

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        do current_line = 1, line_count
            line = buffer_get_line(buffer, current_line)
            col = 1
            do while (col <= len(line))
                if (use_regex) then
                    call find_regex_in_line(line(col:), compiled_regex_id, pos, match_len)
                else
                    call find_pattern_in_line(line(col:), pattern, pos)
                    match_len = len(pattern)
                end if
                if (pos > 0) then
                    pos = col + pos - 1
                    if (whole_word) then
                        if (is_whole_word_match(line, pos, match_len)) then
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
        current_match_index = 0
    end subroutine count_all_matches

    subroutine update_match_index(buffer, pattern, match_line, match_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: match_line, match_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, col, match_count
        integer :: match_len

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        do current_line = 1, line_count
            line = buffer_get_line(buffer, current_line)
            col = 1
            do while (col <= len(line))
                if (use_regex) then
                    call find_regex_in_line(line(col:), compiled_regex_id, pos, match_len)
                else
                    call find_pattern_in_line(line(col:), pattern, pos)
                    match_len = len(pattern)
                end if
                if (pos > 0) then
                    pos = col + pos - 1
                    if (whole_word) then
                        if (is_whole_word_match(line, pos, match_len)) then
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

    subroutine center_viewport_on_cursor(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: cursor_line, viewport_height

        cursor_line = editor%cursors(editor%active_cursor)%line
        viewport_height = editor%screen_rows - 2
        editor%viewport_line = max(1, cursor_line - viewport_height / 2)
    end subroutine center_viewport_on_cursor

end module unified_search_module
