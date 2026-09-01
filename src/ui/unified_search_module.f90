module unified_search_module
    use iso_fortran_env, only: input_unit, output_unit
    use terminal_io_module
    use editor_state_module, only: editor_state_t, cursor_t, sync_editor_to_pane
    use text_buffer_module
    use regex_module
    use utf8_module, only: utf8_char_count, utf8_char_to_byte_index, utf8_byte_to_char_index
    use theme_module, only: THEME_PANEL_SELECTION, THEME_STATUS, &
                            THEME_STATUS_ACCENT, theme_reset, theme_sgr
    implicit none
    private

    public :: current_search_pattern, clear_search_pattern, exit_search_mode
    public :: find_next_match, find_prev_match, center_viewport_on_cursor
    public :: get_matches_on_line, search_mode_active
    public :: search_forward, search_backward

    ! The find bar, as a panel the main loop drives (see below)
    public :: search_panel_show, search_panel_hide, is_search_panel_visible
    public :: search_panel_handle_key, render_search_panel
    public :: search_panel_key_edits
    public :: active_match_span

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

    ! Field focus (1 = find, 2 = replace)
    integer :: active_field = 1

    ! Search history
    integer, parameter :: MAX_HISTORY = 20
    character(len=256), dimension(MAX_HISTORY) :: search_history
    integer :: history_count = 0
    integer :: history_index = 0  ! Current position when navigating history

    ! ---- Find bar state -------------------------------------------------
    ! Ctrl-F used to run its own blocking read loop, so nothing could
    ! re-render while it was up and the matches it lit only became visible
    ! after it exited. It is now state plus a key handler plus a render, on
    ! the same footing as the group dialog -- which is what lets every match
    ! stay lit while you walk between them.
    logical :: panel_visible = .false.

    ! True while the last key typed into the bar was a character or a
    ! backspace. It decides who owns the two keys that mean something to
    ! both the field and the matches: while you are composing, SPACE and TAB
    ! belong to the field; the moment you navigate, they belong to the
    ! matches. Seeding from the word under the caret opens the bar NOT
    ! composing, so every navigation key is live immediately.
    logical :: composing = .false.

    ! The seeded pattern behaves like selected text: it is there to
    ! search with, and the first character typed replaces it rather
    ! than extending it.
    logical :: seed_fresh = .false.

    character(len=256) :: panel_find = ' '
    character(len=256) :: panel_replace = ' '
    integer :: panel_find_len = 0
    integer :: panel_replace_len = 0

    ! Where an edit to the pattern restarts its search from, so that typing
    ! narrows the match set from one fixed point instead of chasing the
    ! caret forward one match per keystroke.
    integer :: anchor_line = 1
    integer :: anchor_col = 0

    ! The match the caret is on, in BYTE columns on its own line. The
    ! renderer paints this one differently from the rest; 0 means none.
    integer :: active_match_line = 0
    integer :: active_match_sbyte = 0
    integer :: active_match_ebyte = 0

    ! Search in selection mode
    logical :: search_in_selection = .false.
    integer :: selection_start_line = 1
    integer :: selection_start_col = 1
    integer :: selection_end_line = 1
    integer :: selection_end_col = 1

contains

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

        ! Remember which match this is, in bytes, so the renderer can paint
        ! it differently from the others.
        active_match_line = found_line
        active_match_sbyte = found_col
        active_match_ebyte = found_col + match_len - 1

        ! Scroll only if the match is off screen. Re-centring on every jump
        ! throws the page around while you step between two matches you can
        ! already both see, which is most of what walking a search is.
        call reveal_match_in_viewport(editor, found_line)

        ! THEN sync cursor and viewport to pane
        call sync_editor_to_pane(editor)
    end subroutine select_found_match

    subroutine clear_active_match()
        active_match_line = 0
        active_match_sbyte = 0
        active_match_ebyte = 0
    end subroutine clear_active_match

    !> The active match's byte span on `line_num`, for the renderer.
    function active_match_span(line_num, sbyte, ebyte) result(is_active_line)
        integer, intent(in) :: line_num
        integer, intent(out) :: sbyte, ebyte
        logical :: is_active_line

        sbyte = active_match_sbyte
        ebyte = active_match_ebyte
        is_active_line = search_mode_active .and. active_match_line == line_num &
                         .and. active_match_sbyte > 0
    end function active_match_span

    !> Bring `line_num` into view, but leave the page alone if it is already
    !> there. The height is the ACTIVE PANE's when panes are in play: the
    !> screen height would overstate a split pane and leave a match just off
    !> its bottom edge sitting there unscrolled.
    subroutine reveal_match_in_viewport(editor, line_num)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: line_num
        integer :: vh, top, t, p

        vh = max(1, editor%screen_rows - 2)
        top = editor%viewport_line
        t = editor%active_tab_index
        if (t >= 1 .and. t <= size(editor%tabs)) then
            if (allocated(editor%tabs(t)%panes)) then
                p = editor%tabs(t)%active_pane_index
                if (p >= 1 .and. p <= size(editor%tabs(t)%panes)) then
                    vh = max(1, editor%tabs(t)%panes(p)%screen_height)
                    ! The pane's own viewport is the truth once panes exist;
                    ! editor%viewport_line can lag behind a wheel scroll.
                    top = editor%tabs(t)%panes(p)%viewport_line
                    editor%viewport_line = top
                end if
            end if
        end if

        if (line_num < top .or. line_num > top + vh - 1) then
            editor%viewport_line = max(1, line_num - vh / 2)
        end if
    end subroutine reveal_match_in_viewport

    ! Leave search mode (dismiss match highlights, release n/N navigation)
    ! but keep the pattern so the prompt can prefill it next time
    subroutine exit_search_mode()
        search_mode_active = .false.
        panel_visible = .false.
        call clear_active_match()
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
        search_mode_active = .false.
        panel_visible = .false.
        call clear_active_match()
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

    !=====================================================================
    ! The find bar
    !
    ! Every key here follows one rule for the two keys that mean something
    ! to both the field and the match list: WHILE COMPOSING (the last key
    ! was a character or a backspace) space and tab belong to the field; the
    ! moment you navigate they belong to the matches. Opening the bar on a
    ! word seeds the field and does NOT count as composing, so the whole
    ! navigation set is live on the first keypress.
    !=====================================================================

    logical function is_search_panel_visible()
        is_search_panel_visible = panel_visible
    end function is_search_panel_visible

    !> Open the bar, seeded from what the caret is on.
    subroutine search_panel_show(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: seed
        integer :: c, sline, scol, eline, ecol, tl, tc
        integer :: seed_line, seed_byte

        c = editor%active_cursor
        active_field = 1
        composing = .false.

        ! A selection drawn across LINES is a region to confine the search
        ! to. A selection inside one line is a word you want to hunt for --
        ! confining the hunt to it would find exactly one match, which is
        ! never what Ctrl-F on a word means.
        search_in_selection = .false.
        if (editor%cursors(c)%has_selection) then
            sline = editor%cursors(c)%selection_start_line
            scol = editor%cursors(c)%selection_start_col
            eline = editor%cursors(c)%line
            ecol = editor%cursors(c)%column
            if (sline > eline .or. (sline == eline .and. scol > ecol)) then
                tl = sline
                tc = scol
                sline = eline
                scol = ecol
                eline = tl
                ecol = tc
            end if
            if (eline > sline) then
                search_in_selection = .true.
                selection_start_line = sline
                selection_end_line = eline
                selection_start_col = line_byte_col(buffer, sline, scol)
                selection_end_col = line_byte_col(buffer, eline, ecol)
            end if
        end if

        seed = seed_text(editor, buffer, seed_line, seed_byte)

        if (len(seed) > 0) then
            panel_find = ' '
            panel_find_len = min(len(seed), len(panel_find))
            panel_find(1:panel_find_len) = seed(1:panel_find_len)
            seed_fresh = .true.
            ! Anchor one byte BEFORE the seed so the very thing under the
            ! caret is match 1. find_next_match starts at start_col + 1, so
            ! anchoring at the caret itself would skip the word it named.
            anchor_line = seed_line
            anchor_col = max(0, seed_byte - 1)
        else
            ! Nothing under the caret: keep whatever was searched for last,
            ! and hunt forward from where the caret actually is.
            if (allocated(current_search_pattern)) then
                panel_find = ' '
                panel_find_len = min(len(current_search_pattern), len(panel_find))
                panel_find(1:panel_find_len) = current_search_pattern(1:panel_find_len)
                seed_fresh = .true.
            else
                panel_find = ' '
                panel_find_len = 0
                seed_fresh = .false.
                composing = .true.
            end if
            anchor_line = editor%cursors(c)%line
            anchor_col = max(0, line_byte_col(buffer, editor%cursors(c)%line, &
                                              editor%cursors(c)%column) - 1)
        end if

        if (allocated(current_replace_text)) then
            panel_replace = ' '
            panel_replace_len = min(len(current_replace_text), len(panel_replace))
            if (panel_replace_len > 0) &
                panel_replace(1:panel_replace_len) = current_replace_text(1:panel_replace_len)
        else
            panel_replace = ' '
            panel_replace_len = 0
        end if

        panel_visible = .true.
        call apply_pattern(editor, buffer)
    end subroutine search_panel_show

    !> Close the bar. `keep_highlights` distinguishes the two ways out:
    !> Ctrl-F puts the bar away but leaves the search live, so the matches
    !> stay lit and n/N keep walking them; ESC ends the search outright.
    subroutine search_panel_hide(keep_highlights)
        logical, intent(in) :: keep_highlights

        if (.not. panel_visible) return
        panel_visible = .false.
        composing = .false.
        seed_fresh = .false.
        if (panel_find_len > 0) call add_to_search_history(panel_find(1:panel_find_len))
        if (.not. keep_highlights) then
            search_mode_active = .false.
            call clear_active_match()
        end if
    end subroutine search_panel_hide

    !> The text Ctrl-F should start from: a one-line selection verbatim,
    !> otherwise the word the caret sits on. Returns '' when the caret is on
    !> whitespace or past the end of the line, with the byte column the seed
    !> starts at so the caller can anchor the search on it.
    function seed_text(editor, buffer, seed_line, seed_byte) result(seed)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(out) :: seed_line, seed_byte
        character(len=:), allocatable :: seed, line
        integer :: c, sb, eb, wstart, wend, bpos
        integer :: sline, scol, eline, ecol

        seed = ''
        c = editor%active_cursor
        seed_line = editor%cursors(c)%line
        seed_byte = 1

        if (editor%cursors(c)%has_selection) then
            sline = editor%cursors(c)%selection_start_line
            scol = editor%cursors(c)%selection_start_col
            eline = editor%cursors(c)%line
            ecol = editor%cursors(c)%column
            if (sline == eline) then
                if (scol > ecol) then
                    bpos = scol
                    scol = ecol
                    ecol = bpos
                end if
                line = buffer_get_line(buffer, sline)
                sb = line_byte_col(buffer, sline, scol)
                eb = line_byte_col(buffer, sline, ecol) - 1
                if (eb >= sb .and. sb >= 1 .and. eb <= len(line)) then
                    seed = line(sb:eb)
                    seed_line = sline
                    seed_byte = sb
                end if
                return
            end if
            ! A multi-line selection is the region, not the needle.
            return
        end if

        line = buffer_get_line(buffer, seed_line)
        bpos = utf8_char_to_byte_index(line, editor%cursors(c)%column)
        if (bpos == 0) return
        if (bpos > len(line)) return
        call word_bounds(line, bpos, wstart, wend)
        if (wstart > 0 .and. wend >= wstart) then
            seed = line(wstart:wend)
            seed_byte = wstart
        end if
    end function seed_text

    !> Byte extent of the word containing byte `pos`, or 0,0 if that byte is
    !> not a word character.
    subroutine word_bounds(line, pos, word_start, word_end)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: word_start, word_end
        integer :: i

        word_start = 0
        word_end = 0
        if (pos < 1 .or. pos > len(line)) return
        if (.not. is_word_char(line(pos:pos))) return

        word_start = pos
        do i = pos - 1, 1, -1
            if (.not. is_word_char(line(i:i))) exit
            word_start = i
        end do
        word_end = pos
        do i = pos + 1, len(line)
            if (.not. is_word_char(line(i:i))) exit
            word_end = i
        end do
    end subroutine word_bounds

    !> Re-run the search for whatever the find field now holds, from the
    !> anchor rather than from the caret: typing a pattern one letter at a
    !> time would otherwise walk the caret forward one match per keystroke.
    subroutine apply_pattern(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: found
        integer :: fl, fc

        if (panel_find_len == 0) then
            search_mode_active = .false.
            total_matches = 0
            current_match_index = 0
            call clear_active_match()
            if (allocated(current_search_pattern)) deallocate(current_search_pattern)
            return
        end if

        if (allocated(current_search_pattern)) deallocate(current_search_pattern)
        current_search_pattern = panel_find(1:panel_find_len)

        if (use_regex) then
            if (compiled_regex_id >= 0) then
                call regex_free(compiled_regex_id)
                compiled_regex_id = -1
            end if
            compiled_regex_id = regex_compile(current_search_pattern, case_sensitive)
            if (compiled_regex_id < 0) then
                ! An unfinished pattern -- "[a" while still typing -- is the
                ! normal case, not an error worth a message.
                search_mode_active = .false.
                total_matches = 0
                current_match_index = 0
                call clear_active_match()
                return
            end if
        end if

        search_mode_active = .true.
        call count_all_matches(buffer, current_search_pattern)
        if (total_matches == 0) then
            call clear_active_match()
            return
        end if

        call find_next_match(buffer, current_search_pattern, anchor_line, anchor_col, &
                             found, fl, fc)
        if (found) then
            call select_found_match(editor, buffer, fl, fc)
        else
            call clear_active_match()
        end if
    end subroutine apply_pattern

    !> One step through the match list, keeping the bar up.
    subroutine panel_step(editor, buffer, forward)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(in) :: forward

        ! Navigating ends the composing run, but leaves the seed
        ! replaceable: walking the matches is not a decision to keep the
        ! word, so typing after it should still start a fresh pattern.
        composing = .false.
        if (.not. search_mode_active) return
        if (.not. allocated(current_search_pattern)) return
        if (total_matches == 0) return

        if (forward) then
            call search_forward(editor, buffer)
        else
            call search_backward(editor, buffer)
        end if
    end subroutine panel_step

    !> Home/End: the first and last match in the file.
    subroutine panel_edge(editor, buffer, first)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(in) :: first
        logical :: found
        integer :: fl, fc

        composing = .false.
        if (.not. search_mode_active) return
        if (.not. allocated(current_search_pattern)) return
        if (total_matches == 0) return

        if (first) then
            call find_next_match(buffer, current_search_pattern, 1, 0, found, fl, fc)
        else
            call find_prev_match(buffer, current_search_pattern, &
                                 buffer_get_line_count(buffer), huge(1), found, fl, fc)
        end if
        if (found) call select_found_match(editor, buffer, fl, fc)
    end subroutine panel_edge

    subroutine field_insert(ch)
        character(len=1), intent(in) :: ch

        ! The seeded pattern behaves like selected text: the first character
        ! typed replaces it rather than extending it.
        if (seed_fresh .and. active_field == 1) then
            panel_find = ' '
            panel_find_len = 0
            seed_fresh = .false.
        end if
        composing = .true.
        if (active_field == 1) then
            if (panel_find_len >= len(panel_find)) return
            panel_find_len = panel_find_len + 1
            panel_find(panel_find_len:panel_find_len) = ch
        else
            if (panel_replace_len >= len(panel_replace)) return
            panel_replace_len = panel_replace_len + 1
            panel_replace(panel_replace_len:panel_replace_len) = ch
        end if
    end subroutine field_insert

    subroutine field_backspace()
        if (seed_fresh .and. active_field == 1) then
            panel_find = ' '
            panel_find_len = 0
            seed_fresh = .false.
            composing = .true.
            return
        end if
        composing = .true.
        if (active_field == 1) then
            if (panel_find_len > 0) panel_find_len = panel_find_len - 1
        else
            if (panel_replace_len > 0) panel_replace_len = panel_replace_len - 1
        end if
    end subroutine field_backspace

    !> Does this key make the bar change the DOCUMENT rather than the bar?
    !> The caller has to know before dispatching: an undo baseline is a
    !> snapshot of the text as it was, so it can only be taken beforehand.
    logical function search_panel_key_edits(key)
        character(len=*), intent(in) :: key

        search_panel_key_edits = .false.
        if (.not. panel_visible) return
        if (panel_find_len == 0) return
        select case (trim(key))
        case ('ctrl-r', 'ctrl-a')
            search_panel_key_edits = .true.
        end select
    end function search_panel_key_edits

    function search_panel_handle_key(key, editor, buffer) result(handled)
        character(len=*), intent(in) :: key
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical :: handled
        logical :: is_space
        integer :: c

        handled = .false.
        if (.not. panel_visible) return
        handled = .true.

        ! trim() turns a space into '', so select case cannot see it. Same
        ! trap that once stopped the command palette typing a space.
        is_space = .false.
        if (len(key) >= 1) is_space = (len_trim(key) == 0 .and. key(1:1) == ' ')

        if (is_space) then
            if (composing) then
                call field_insert(' ')
                if (active_field == 1) call apply_pattern(editor, buffer)
            else
                call panel_step(editor, buffer, .true.)
            end if
            return
        end if

        select case (trim(key))
        case ('esc')
            call search_panel_hide(.false.)

        case ('ctrl-f')
            call search_panel_hide(.true.)

        case ('down', 'right', 'pagedown', 'enter')
            call panel_step(editor, buffer, .true.)

        case ('up', 'left', 'pageup')
            call panel_step(editor, buffer, .false.)

        ! Shift reverses whatever the key would have done, so every one of
        ! them is prev -- including the two that only exist on terminals
        ! speaking CSI-u, shift-enter and shift-space.
        case ('shift-down', 'shift-right', 'shift-pagedown', 'shift-enter', &
              'shift-up', 'shift-left', 'shift-pageup', 'shift-tab', 'shift-space')
            call panel_step(editor, buffer, .false.)

        case ('tab')
            if (composing) then
                active_field = 3 - active_field
            else
                call panel_step(editor, buffer, .true.)
            end if

        case ('home')
            call panel_edge(editor, buffer, .true.)

        case ('end')
            call panel_edge(editor, buffer, .false.)

        case ('backspace')
            call field_backspace()
            if (active_field == 1) call apply_pattern(editor, buffer)

        ! History moved off up/down, which now walk matches.
        case ('alt-up')
            if (active_field == 1) then
                call navigate_history_up(panel_find, panel_find_len)
                seed_fresh = .false.
                call apply_pattern(editor, buffer)
            end if

        case ('alt-down')
            if (active_field == 1) then
                call navigate_history_down(panel_find, panel_find_len)
                seed_fresh = .false.
                call apply_pattern(editor, buffer)
            end if

        case ('alt-c')
            case_sensitive = .not. case_sensitive
            call apply_pattern(editor, buffer)

        case ('alt-w')
            whole_word = .not. whole_word
            call apply_pattern(editor, buffer)

        case ('alt-r')
            use_regex = .not. use_regex
            call apply_pattern(editor, buffer)

        case ('alt-s')
            search_in_selection = .not. search_in_selection
            call apply_pattern(editor, buffer)

        case ('ctrl-r')
            if (panel_find_len > 0) then
                if (allocated(current_replace_text)) deallocate(current_replace_text)
                current_replace_text = panel_replace(1:panel_replace_len)
                call replace_current_and_advance(editor, buffer)
                call panel_step(editor, buffer, .true.)
            end if

        case ('ctrl-a')
            if (panel_find_len > 0) then
                if (allocated(current_replace_text)) deallocate(current_replace_text)
                current_replace_text = panel_replace(1:panel_replace_len)
                call replace_all_matches(editor, buffer)
                call count_all_matches(buffer, current_search_pattern)
                call clear_active_match()
                call sync_editor_to_pane(editor)
            end if

        case default
            if (len_trim(key) == 1) then
                c = iachar(key(1:1))
                if (c >= 32 .and. c < 127) then
                    call field_insert(key(1:1))
                    if (active_field == 1) call apply_pattern(editor, buffer)
                    return
                end if
            end if
            ! Everything else -- Ctrl chords, function keys, Alt keys the bar
            ! has no meaning for -- belongs to the editor. A bar that
            ! swallowed them would make Ctrl-S look broken while it was up.
            handled = .false.
        end select
    end function search_panel_handle_key

    !> Draw the bar over the status line, and leave the caret in the field.
    !> Called last in the frame, so it sits on top of the status bar it
    !> replaces.
    subroutine render_search_panel(editor)
        type(editor_state_t), intent(in) :: editor
        character(len=:), allocatable :: head, field, tail, flags
        character(len=48) :: num
        integer :: w, fw, caret_col, tail_room, shown

        if (.not. panel_visible) return
        w = editor%screen_cols
        if (w < 8) return

        if (active_field == 1) then
            head = ' find '
        else
            head = ' repl '
        end if

        ! The field shows its TAIL when the pattern outgrows it: what you
        ! just typed is what you need to see. The second clamp is what stops
        ! a very narrow terminal being handed a field wider than the row,
        ! which would wrap the bar onto the document.
        fw = max(10, min(28, w / 3))
        fw = min(fw, max(1, w - len(head) - 2))
        if (active_field == 1) then
            shown = panel_find_len
            if (shown > fw) then
                field = panel_find(shown - fw + 1:shown)
            else
                field = panel_find(1:shown) // repeat(' ', fw - shown)
            end if
        else
            shown = panel_replace_len
            if (shown > fw) then
                field = panel_replace(shown - fw + 1:shown)
            else
                field = panel_replace(1:shown) // repeat(' ', fw - shown)
            end if
        end if

        flags = ''
        if (case_sensitive) flags = trim(flags) // ' Aa'
        if (whole_word) flags = trim(flags) // ' W'
        if (use_regex) flags = trim(flags) // ' .*'
        if (search_in_selection) flags = trim(flags) // ' sel'

        if (panel_find_len == 0) then
            tail = '  type to search'
        else if (.not. search_mode_active) then
            tail = '  bad pattern'
        else if (total_matches == 0) then
            tail = '  no matches'
        else
            write(num, '(a,i0,a,i0)') '  ', current_match_index, ' of ', total_matches
            tail = trim(num)
        end if
        if (len_trim(flags) > 0) tail = tail // '  [' // trim(adjustl(flags)) // ']'

        ! Hints are the first thing to go on a narrow terminal.
        tail_room = w - len(head) - fw - len(tail)
        if (tail_room >= 44) then
            tail = tail // '   arrows/enter next  shift prev  esc close'
        else if (tail_room >= 22) then
            tail = tail // '   enter next  esc close'
        end if
        if (len(tail) > max(0, w - len(head) - fw)) then
            tail = tail(1:max(0, w - len(head) - fw))
        end if

        call terminal_hide_cursor()
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(theme_sgr(THEME_STATUS) // head)
        if (active_field == 1) then
            call terminal_write(theme_sgr(THEME_STATUS_ACCENT))
        else
            call terminal_write(theme_sgr(THEME_PANEL_SELECTION))
        end if
        call terminal_write(field // theme_sgr(THEME_STATUS) // tail)
        caret_col = w - len(head) - fw - len(tail)
        if (caret_col > 0) call terminal_write(repeat(' ', caret_col))
        call terminal_write(theme_reset())

        ! The caret marks where the next character lands.
        caret_col = len(head) + min(shown, fw) + 1
        call terminal_move_cursor(editor%screen_rows, min(caret_col, w))
        call terminal_show_cursor()
    end subroutine render_search_panel

end module unified_search_module
