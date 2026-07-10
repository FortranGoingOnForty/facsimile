module unified_search_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t, sync_editor_to_pane
    use text_buffer_module
    use regex_module
    use utf8_module, only: utf8_char_count, utf8_char_to_byte_index, utf8_byte_to_char_index
    implicit none
    private

    public :: show_unified_search_prompt
    public :: current_search_pattern, clear_search_pattern, exit_search_mode
    public :: find_next_match, find_prev_match, center_viewport_on_cursor
    public :: get_matches_on_line, search_mode_active
    public :: search_forward, search_backward

    ! Column conventions: cursor_t columns are 1-based UTF-8 CHARACTER
    ! indices, while the search internals (index(), POSIX regex) work on
    ! BYTE offsets within a line. find_next_match/find_prev_match and
    ! get_matches_on_line speak bytes; conversions happen where cursor or
    ! selection fields are read or written.

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

    ! Track last search parameters to detect changes
    character(len=:), allocatable :: last_search_pattern
    logical :: last_case_sensitive = .false.
    logical :: last_whole_word = .false.
    logical :: last_use_regex = .false.

    ! Field focus (1 = find, 2 = replace)
    integer :: active_field = 1

    ! Search history
    integer, parameter :: MAX_HISTORY = 20
    character(len=256), dimension(MAX_HISTORY) :: search_history
    integer :: history_count = 0
    integer :: history_index = 0  ! Current position when navigating history

    ! Search in selection mode
    logical :: search_in_selection = .false.
    integer :: selection_start_line = 1
    integer :: selection_start_col = 1
    integer :: selection_end_line = 1
    integer :: selection_end_col = 1

contains

    subroutine show_unified_search_prompt(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=256) :: find_buffer, replace_buffer
        character(len=256) :: prompt
        integer :: find_pos, replace_pos, ch
        integer :: temp_line, temp_col
        logical :: in_alt_sequence

        ! Initialize
        find_buffer = ''
        replace_buffer = ''
        find_pos = 0
        replace_pos = 0
        active_field = 1  ! Start with find field
        in_alt_sequence = .false.

        ! Check if there's an active selection for search-in-selection mode
        if (editor%cursors(editor%active_cursor)%has_selection) then
            search_in_selection = .true.
            selection_start_line = editor%cursors(editor%active_cursor)%selection_start_line
            selection_start_col = editor%cursors(editor%active_cursor)%selection_start_col
            selection_end_line = editor%cursors(editor%active_cursor)%line
            selection_end_col = editor%cursors(editor%active_cursor)%column
            ! Ensure start comes before end
            if (selection_start_line > selection_end_line .or. &
                (selection_start_line == selection_end_line .and. selection_start_col > selection_end_col)) then
                ! Swap
                temp_line = selection_start_line
                temp_col = selection_start_col
                selection_start_line = selection_end_line
                selection_start_col = selection_end_col
                selection_end_line = temp_line
                selection_end_col = temp_col
            end if
            ! Bounds are compared against byte match positions
            selection_start_col = line_byte_col(buffer, selection_start_line, selection_start_col)
            selection_end_col = line_byte_col(buffer, selection_end_line, selection_end_col)
        else
            search_in_selection = .false.
        end if

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
                    ! Arrow keys or mouse events
                    ch = terminal_read_char()
                    if (ch == iachar('<')) then
                        ! Mouse event - consume until 'M' or 'm'
                        do
                            ch = terminal_read_char()
                            if (ch == iachar('M') .or. ch == iachar('m') .or. ch == -1) exit
                        end do
                        in_alt_sequence = .false.
                        cycle
                    else if (ch == iachar('A')) then
                        ! Up arrow - navigate history backward (older)
                        if (active_field == 1) then  ! Only in find field
                            call navigate_history_up(find_buffer, find_pos)
                            call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                            call display_prompt(editor, prompt, find_pos, replace_pos)
                        end if
                    else if (ch == iachar('B')) then
                        ! Down arrow - navigate history forward (newer)
                        if (active_field == 1) then  ! Only in find field
                            call navigate_history_down(find_buffer, find_pos)
                            call build_unified_prompt(prompt, find_buffer, find_pos, replace_buffer, replace_pos)
                            call display_prompt(editor, prompt, find_pos, replace_pos)
                        end if
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
                else if (ch == iachar('s') .or. ch == iachar('S')) then
                    ! Alt+S - toggle search in selection
                    search_in_selection = .not. search_in_selection
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

                    ! Add to search history
                    call add_to_search_history(current_search_pattern)

                    ! Check if search parameters changed - if so, reset search mode
                    if (search_mode_active) then
                        if (.not. allocated(last_search_pattern) .or. &
                            current_search_pattern /= last_search_pattern .or. &
                            case_sensitive .neqv. last_case_sensitive .or. &
                            whole_word .neqv. last_whole_word .or. &
                            use_regex .neqv. last_use_regex) then
                            ! Parameters changed - treat as new search
                            search_mode_active = .false.
                        end if
                    end if

                    if (.not. search_mode_active) then
                        ! First search - count and find
                        search_mode_active = .true.
                        call count_all_matches(buffer, current_search_pattern)
                        call perform_search(editor, buffer, current_search_pattern)

                        ! Save current parameters
                        if (allocated(last_search_pattern)) deallocate(last_search_pattern)
                        allocate(character(len=len(current_search_pattern)) :: last_search_pattern)
                        last_search_pattern = current_search_pattern
                        last_case_sensitive = case_sensitive
                        last_whole_word = whole_word
                        last_use_regex = use_regex
                    else
                        ! Cycle to next match
                        call search_forward(editor, buffer)
                    end if

                    ! Note: Screen re-rendering happens in command_handler after search exits
                    ! The match highlighting and cursor position will be visible after exiting search

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

                    ! Perform replacement
                    call replace_current_and_advance(editor, buffer)

                    ! Clear old prompt and re-render everything
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write(repeat(' ', editor%screen_cols))

                    ! Update prompt with new match count
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
            else if (ch == 13 .or. ch == 10) then  ! Enter - find first match and exit
                ! If we have a search pattern, perform search first if needed
                if (find_pos > 0) then
                    ! Save search pattern
                    if (allocated(current_search_pattern)) deallocate(current_search_pattern)
                    allocate(character(len=find_pos) :: current_search_pattern)
                    current_search_pattern = find_buffer(1:find_pos)

                    ! Add to search history
                    call add_to_search_history(current_search_pattern)

                    ! If no selection yet (haven't searched), perform the search
                    if (.not. editor%cursors(editor%active_cursor)%has_selection) then
                        search_mode_active = .true.
                        call count_all_matches(buffer, current_search_pattern)
                        call perform_search(editor, buffer, current_search_pattern)

                        ! Save current parameters
                        if (allocated(last_search_pattern)) deallocate(last_search_pattern)
                        allocate(character(len=len(current_search_pattern)) :: last_search_pattern)
                        last_search_pattern = current_search_pattern
                        last_case_sensitive = case_sensitive
                        last_whole_word = whole_word
                        last_use_regex = use_regex
                    end if
                end if

                ! Move cursor to START of match (not end)
                if (editor%cursors(editor%active_cursor)%has_selection) then
                    editor%cursors(editor%active_cursor)%line = &
                        editor%cursors(editor%active_cursor)%selection_start_line
                    editor%cursors(editor%active_cursor)%column = &
                        editor%cursors(editor%active_cursor)%selection_start_col
                    editor%cursors(editor%active_cursor)%desired_column = &
                        editor%cursors(editor%active_cursor)%selection_start_col
                    ! Clear selection so cursor is at start, not selecting
                    editor%cursors(editor%active_cursor)%has_selection = .false.
                    ! Sync cursor back to pane (important for pane system!)
                    call sync_editor_to_pane(editor)
                end if
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

        ! Clean up - clear the prompt line
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', editor%screen_cols))
        ! Don't hide cursor - let the main render loop handle cursor display
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
        if (search_in_selection) then
            options = trim(options) // '[Ss]'
        else
            options = trim(options) // '[ss]'
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
        integer :: match_len, start_char, end_char

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
                            line_byte_col(buffer, editor%cursors(editor%active_cursor)%line, &
                                          editor%cursors(editor%active_cursor)%column), &
                            found, found_line, found_col)

        if (found) then
            ! For regex, use the match length from last search
            ! For normal search, use pattern length (both in bytes)
            if (use_regex .and. last_match_length > 0) then
                match_len = last_match_length
            else
                match_len = len(pattern)
            end if
            start_char = line_char_col(buffer, found_line, found_col)
            end_char = line_char_col(buffer, found_line, found_col + match_len)

            editor%cursors(editor%active_cursor)%line = found_line
            editor%cursors(editor%active_cursor)%desired_column = start_char

            ! Create selection spanning the match
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = found_line
            editor%cursors(editor%active_cursor)%selection_start_col = start_char
            editor%cursors(editor%active_cursor)%column = end_char

            last_search_line = found_line
            last_search_col = found_col

            ! Center viewport on the found match FIRST
            call center_viewport_on_cursor(editor)

            ! THEN sync cursor and viewport to pane so rendering shows updated state
            call sync_editor_to_pane(editor)
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
        start_col = line_byte_col(buffer, start_line, &
                                  editor%cursors(editor%active_cursor)%column)

        call find_next_match(buffer, current_search_pattern, &
                            start_line, start_col, &
                            found, found_line, found_col)

        if (found) call select_found_match(editor, buffer, found_line, found_col)
    end subroutine search_forward

    subroutine search_backward(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col
        integer :: start_line, start_col, start_char

        if (.not. allocated(current_search_pattern)) return

        call count_all_matches(buffer, current_search_pattern)

        ! Start strictly before the current match (its selection start)
        ! so repeated presses walk backward instead of re-finding it
        start_line = editor%cursors(editor%active_cursor)%line
        if (editor%cursors(editor%active_cursor)%has_selection .and. &
            editor%cursors(editor%active_cursor)%selection_start_line == start_line) then
            start_char = editor%cursors(editor%active_cursor)%selection_start_col
        else
            start_char = editor%cursors(editor%active_cursor)%column
        end if
        start_col = line_byte_col(buffer, start_line, start_char) - 1

        call find_prev_match(buffer, current_search_pattern, &
                            start_line, start_col, &
                            found, found_line, found_col)

        if (found) call select_found_match(editor, buffer, found_line, found_col)
    end subroutine search_backward

    ! Shared tail of search_forward/search_backward: place the cursor and
    ! selection over the match found at (found_line, found_col-in-bytes)
    subroutine select_found_match(editor, buffer, found_line, found_col)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: found_line, found_col
        integer :: match_len, start_char, end_char

        if (use_regex .and. last_match_length > 0) then
            match_len = last_match_length
        else
            match_len = len(current_search_pattern)
        end if
        start_char = line_char_col(buffer, found_line, found_col)
        end_char = line_char_col(buffer, found_line, found_col + match_len)

        editor%cursors(editor%active_cursor)%line = found_line
        editor%cursors(editor%active_cursor)%desired_column = start_char

        editor%cursors(editor%active_cursor)%has_selection = .true.
        editor%cursors(editor%active_cursor)%selection_start_line = found_line
        editor%cursors(editor%active_cursor)%selection_start_col = start_char
        editor%cursors(editor%active_cursor)%column = end_char

        last_search_line = found_line
        last_search_col = found_col

        ! Center viewport on the found match FIRST
        call center_viewport_on_cursor(editor)

        ! THEN sync cursor and viewport to pane
        call sync_editor_to_pane(editor)
    end subroutine select_found_match

    ! Leave search mode (dismiss match highlights, release n/N navigation)
    ! but keep the pattern so the prompt can prefill it next time
    subroutine exit_search_mode()
        search_mode_active = .false.
    end subroutine exit_search_mode

    subroutine replace_current_and_advance(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: match_len
        integer :: tab_i, pane_i

        if (.not. allocated(current_search_pattern)) return
        if (.not. allocated(current_replace_text)) return

        ! Check if cursor has selection
        if (editor%cursors(editor%active_cursor)%has_selection) then
            ! Calculate match length in BYTES (perform_replacement slices
            ! the line); the selection columns are char indices
            if (last_match_length > 0) then
                match_len = last_match_length
            else
                match_len = line_byte_col(buffer, editor%cursors(editor%active_cursor)%line, &
                                          editor%cursors(editor%active_cursor)%column) - &
                            line_byte_col(buffer, editor%cursors(editor%active_cursor)%line, &
                                          editor%cursors(editor%active_cursor)%selection_start_col)
            end if

            ! Perform replacement on parameter buffer
            call perform_replacement(buffer, editor%cursors(editor%active_cursor), &
                                    current_replace_text, match_len)

            ! CRITICAL: Also update the pane's buffer if using panes
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                tab_i = editor%active_tab_index
                if (allocated(editor%tabs(tab_i)%panes)) then
                    pane_i = editor%tabs(tab_i)%active_pane_index
                    if (pane_i > 0 .and. pane_i <= size(editor%tabs(tab_i)%panes)) then
                        ! Copy the modified buffer to the pane's buffer
                        call copy_buffer(editor%tabs(tab_i)%panes(pane_i)%buffer, buffer)
                    end if
                end if
            end if

            ! Create selection highlighting the replacement text
            ! Cursor is already at end of replacement from perform_replacement
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = &
                editor%cursors(editor%active_cursor)%line
            editor%cursors(editor%active_cursor)%selection_start_col = &
                editor%cursors(editor%active_cursor)%column - utf8_char_count(current_replace_text)

            ! Sync cursor to pane (important!)
            call sync_editor_to_pane(editor)

            ! Re-count matches after replacement
            call count_all_matches(buffer, current_search_pattern)

            ! Note: render_screen should be called by the caller
        end if
    end subroutine replace_current_and_advance

    subroutine replace_all_matches(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: found_line, found_col, replace_count
        integer :: search_line, search_col
        type(cursor_t) :: temp_cursor

        if (.not. allocated(current_search_pattern)) return
        if (.not. allocated(current_replace_text)) return

        replace_count = 0
        temp_cursor = editor%cursors(editor%active_cursor)

        ! Scan position for find_next_match, in bytes; start from beginning
        search_line = 1
        search_col = 0

        do
            call find_next_match(buffer, current_search_pattern, &
                                search_line, search_col, &
                                found, found_line, found_col)

            if (.not. found) exit

            ! Move cursor to match and select it (char columns)
            temp_cursor%line = found_line
            temp_cursor%has_selection = .true.
            temp_cursor%selection_start_line = found_line
            temp_cursor%selection_start_col = line_char_col(buffer, found_line, found_col)
            temp_cursor%column = line_char_col(buffer, found_line, found_col + last_match_length)

            ! Replace
            call perform_replacement(buffer, temp_cursor, current_replace_text, last_match_length)

            replace_count = replace_count + 1

            ! Continue searching after the replacement
            search_line = temp_cursor%line
            search_col = line_byte_col(buffer, search_line, temp_cursor%column)

            ! Guard against infinite loop
            if (replace_count > 10000) exit
        end do

        ! Update main cursor (leave it untouched if nothing matched)
        if (replace_count > 0) editor%cursors(editor%active_cursor) = temp_cursor
    end subroutine replace_all_matches

    ! Replace match_len BYTES at the cursor's selection start with
    ! replace_text. Selection/cursor columns are char indices; the line
    ! rebuild below works in bytes (the local buffer position helpers are
    ! byte-based, which stays consistent while the whole line is
    ! deleted and re-inserted byte by byte).
    subroutine perform_replacement(buffer, cursor, replace_text, match_len)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: replace_text
        integer, intent(in) :: match_len
        character(len=:), allocatable :: line, new_line
        integer :: col_char, col, i

        ! Get current line
        line = buffer_get_line(buffer, cursor%line)

        ! Build new line with replacement
        col_char = cursor%selection_start_col
        col = utf8_char_to_byte_index(line, col_char)
        if (col == 0) col = len(line) + 1
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
                pos = i  ! Set pos to buffer position before returning
                return
            end if

            ch = buffer_get_char(buffer, i)
            if (ch == char(10)) then
                if (current_line == line) then
                    pos = i  ! Set pos to buffer position before returning
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

    ! Convert a 1-based char column on a buffer line to a byte column.
    ! Values <= 1 pass through (0 is used as "before line start").
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

    ! Include all helper functions from search_prompt_module
    ! (find_next_match, find_prev_match, count_all_matches, etc.)
    ! For brevity, I'll add a note that these need to be copied over

    subroutine clear_search_pattern()
        if (allocated(current_search_pattern)) deallocate(current_search_pattern)
        if (allocated(current_replace_text)) deallocate(current_replace_text)
        if (allocated(last_search_pattern)) deallocate(last_search_pattern)
        search_mode_active = .false.
        last_search_line = 1
        last_search_col = 1

        ! Reset search parameter tracking
        last_case_sensitive = .false.
        last_whole_word = .false.
        last_use_regex = .false.

        ! Free compiled regex if any
        if (compiled_regex_id >= 0) then
            call regex_free(compiled_regex_id)
            compiled_regex_id = -1
        end if
        last_match_length = 0
    end subroutine clear_search_pattern

    ! Search helper functions
    ! Check if a position is within the search selection bounds
    function is_in_selection_bounds(line_num, col_num) result(in_bounds)
        integer, intent(in) :: line_num, col_num
        logical :: in_bounds

        if (.not. search_in_selection) then
            in_bounds = .true.
            return
        end if

        ! Check if position is within selection
        if (line_num < selection_start_line .or. line_num > selection_end_line) then
            in_bounds = .false.
        else if (line_num == selection_start_line .and. col_num < selection_start_col) then
            in_bounds = .false.
        else if (line_num == selection_end_line .and. col_num > selection_end_col) then
            in_bounds = .false.
        else
            in_bounds = .true.
        end if
    end function is_in_selection_bounds

    subroutine find_next_match(buffer, pattern, start_line, start_col, found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos, search_col
        integer :: match_len
        integer :: end_line

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)
        last_match_length = 0

        ! Determine search range
        if (search_in_selection) then
            end_line = selection_end_line
        else
            end_line = line_count
        end if

        ! Search from current position to end (of selection or file)
        do current_line = start_line, end_line
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
                    ! Check if match is within selection bounds
                    if (search_in_selection .and. .not. is_in_selection_bounds(current_line, found_col)) then
                        if (allocated(line)) deallocate(line)
                        cycle
                    end if
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

        ! Wrap around to beginning (skip if searching in selection)
        if (search_in_selection) return

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
        integer :: begin_line

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)
        last_match_length = 0

        ! Determine search range
        if (search_in_selection) then
            begin_line = selection_start_line
        else
            begin_line = 1
        end if

        ! Search backward from current position
        do current_line = start_line, begin_line, -1
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
                ! Check if match is within selection bounds
                if (search_in_selection .and. .not. is_in_selection_bounds(current_line, last_pos)) then
                    if (allocated(line)) deallocate(line)
                    cycle
                end if
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
        integer :: start_line, end_line

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        ! Determine search range
        if (search_in_selection) then
            start_line = selection_start_line
            end_line = selection_end_line
        else
            start_line = 1
            end_line = line_count
        end if

        do current_line = start_line, end_line
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
                    ! Check if match is within selection bounds
                    if (search_in_selection .and. .not. is_in_selection_bounds(current_line, pos)) then
                        col = pos + 1
                        cycle
                    end if
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
        integer :: start_line, end_line

        match_count = 0
        line_count = buffer_get_line_count(buffer)

        ! Determine search range
        if (search_in_selection) then
            start_line = selection_start_line
            end_line = selection_end_line
        else
            start_line = 1
            end_line = line_count
        end if

        do current_line = start_line, end_line
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
                    ! Check if match is within selection bounds
                    if (search_in_selection .and. .not. is_in_selection_bounds(current_line, pos)) then
                        col = pos + 1
                        cycle
                    end if
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

    ! Add pattern to search history (avoiding duplicates)
    subroutine add_to_search_history(pattern)
        character(len=*), intent(in) :: pattern
        integer :: i

        if (len_trim(pattern) == 0) return

        ! Check if already in history (at position 1 = most recent)
        if (history_count > 0) then
            if (trim(search_history(1)) == trim(pattern)) return
        end if

        ! Shift existing history down
        do i = min(history_count, MAX_HISTORY - 1), 1, -1
            search_history(i + 1) = search_history(i)
        end do

        ! Add new pattern at top
        search_history(1) = pattern
        history_count = min(history_count + 1, MAX_HISTORY)
        history_index = 0  ! Reset navigation position
    end subroutine add_to_search_history

    ! Navigate history up (to older entries)
    subroutine navigate_history_up(buffer, pos)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: pos

        if (history_count == 0) return

        ! Move to next older entry
        if (history_index < history_count) then
            history_index = history_index + 1
            buffer = search_history(history_index)
            pos = len_trim(buffer)
        end if
    end subroutine navigate_history_up

    ! Navigate history down (to newer entries)
    subroutine navigate_history_down(buffer, pos)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: pos

        if (history_count == 0) return

        if (history_index > 1) then
            ! Move to next newer entry
            history_index = history_index - 1
            buffer = search_history(history_index)
            pos = len_trim(buffer)
        else if (history_index == 1) then
            ! Clear to allow new search
            history_index = 0
            buffer = ''
            pos = 0
        end if
    end subroutine navigate_history_down

    ! Get all match positions on a given line
    ! Returns pairs of (start_col, end_col) for each match
    subroutine get_matches_on_line(line, line_num, matches, num_matches)
        character(len=*), intent(in) :: line
        integer, intent(in) :: line_num
        integer, intent(out) :: matches(:,:)  ! Array of (start, end) pairs
        integer, intent(out) :: num_matches
        integer :: col, pos, match_len
        integer :: max_matches

        num_matches = 0
        max_matches = size(matches, 2)  ! Second dimension is number of match slots

        ! Only highlight if search is active and we have a pattern
        if (.not. search_mode_active .or. .not. allocated(current_search_pattern)) return
        if (len_trim(current_search_pattern) == 0) return

        ! Find all matches on this line
        col = 1
        do while (col <= len(line) .and. num_matches < max_matches)
            if (use_regex) then
                call find_regex_in_line(line(col:), compiled_regex_id, pos, match_len)
            else
                call find_pattern_in_line(line(col:), current_search_pattern, pos)
                match_len = len(current_search_pattern)
            end if

            if (pos > 0) then
                pos = col + pos - 1  ! Adjust to line position

                ! Check bounds and whole word if needed
                if (search_in_selection .and. .not. is_in_selection_bounds(line_num, pos)) then
                    col = pos + 1
                    cycle
                end if
                if (whole_word .and. .not. is_whole_word_match(line, pos, match_len)) then
                    col = pos + 1
                    cycle
                end if

                ! Add match
                num_matches = num_matches + 1
                matches(1, num_matches) = pos
                matches(2, num_matches) = pos + match_len - 1
                col = pos + 1
            else
                exit
            end if
        end do
    end subroutine get_matches_on_line

end module unified_search_module
