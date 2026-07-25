module renderer_module
    use iso_fortran_env, only: int32, int64, output_unit
    use terminal_io_module
    use text_buffer_module
    use utf8_module
    use editor_state_module, only: editor_state_t, cursor_t
    use bracket_matching_module
    use file_tree_module
    use file_tree_renderer_module
    use syntax_highlighter_module
    use diagnostics_module, only: diagnostic_t, get_diagnostics_for_line, &
                                   get_diagnostic_at_cursor, &
                                   SEVERITY_ERROR, SEVERITY_WARNING, SEVERITY_INFO, SEVERITY_HINT
    use diagnostics_panel_module, only: render_diagnostics_panel
    use references_panel_module, only: render_references_panel
    use code_actions_panel_module, only: render_code_actions_panel
    use symbols_panel_module, only: render_symbols_panel
    use unified_search_module, only: get_matches_on_line, search_mode_active
    use lsp_server_installer_panel_module, only: render_lsp_server_installer_panel, &
                                                  is_lsp_server_installer_panel_visible
    use terminal_panel_module, only: is_terminal_panel_visible, &
        terminal_panel_render, get_terminal_panel_height
    use completion_popup_module, only: render_completion_popup
    use ghost_text_module, only: ghost_is_active, ghost_suffix
    implicit none
    private

    public :: render_screen, update_viewport, init_renderer, cleanup_renderer
    public :: resize_renderer
    public :: render_status_bar, render_cursor
    public :: set_status_message, clear_status_message, has_status_message
    public :: show_line_numbers, LINE_NUMBER_WIDTH
    public :: clip_to_cells, is_terminal_safe  ! exposed for unit tests
    public :: render_screen_with_tree, render_screen_with_lsp_panel

    ! Transient status-bar message (see set_status_message)
    character(len=:), allocatable :: g_status_message
    public :: tree_state
    public :: update_syntax_highlighter
    public :: render_cursor_only  ! Fast path for cursor-only updates
    public :: fuss_search_buffer, fuss_search_len, fuss_search_last_time
    public :: fuss_fuzzy_jump, fuss_reset_search, get_time_ms
    public :: fuss_git_prefix_active
    public :: display_offset_of, char_col_at_offset

    ! Configuration
    logical :: show_line_numbers = .true.
    logical :: highlight_current_line = .true.
    integer, parameter :: LINE_NUMBER_WIDTH = 5  ! Width for line number display
    integer, parameter :: TAB_WIDTH = 4  ! Columns a tab expands to when rendering

    ! Bracket matching state
    integer :: bracket_line = 0
    integer :: bracket_col = 0
    integer :: matching_bracket_line = 0
    integer :: matching_bracket_col = 0

    ! Screen buffer for double buffering
    type :: screen_buffer_t
        character(len=:), allocatable :: lines(:)
        integer :: rows
        integer :: cols
        logical :: needs_full_redraw
    end type screen_buffer_t

    type(screen_buffer_t) :: screen_buffer

    ! File tree state (for fuss mode)
    type(tree_state_t) :: tree_state

    ! Fuzzy search state for fuss mode (500ms timeout)
    character(len=64) :: fuss_search_buffer = ''
    integer :: fuss_search_len = 0
    integer(int64) :: fuss_search_last_time = 0

    ! Git prefix mode for fuss (Ctrl+g followed by command key)
    logical :: fuss_git_prefix_active = .false.

    ! Syntax highlighting state
    type(syntax_highlighter_t) :: syntax_highlighter
    character(len=512) :: last_highlighted_filename = ""

contains

    subroutine init_renderer(rows, cols, filename)
        integer, intent(in) :: rows, cols
        character(len=*), intent(in), optional :: filename
        integer :: i

        screen_buffer%rows = rows
        screen_buffer%cols = cols
        screen_buffer%needs_full_redraw = .true.

        allocate(character(len=cols) :: screen_buffer%lines(rows))
        do i = 1, rows
            screen_buffer%lines(i) = repeat(' ', cols)
        end do

        ! Initialize syntax highlighter if filename provided
        if (present(filename)) then
            call init_highlighter(syntax_highlighter, filename)
            last_highlighted_filename = trim(filename)
        else
            call init_highlighter(syntax_highlighter)
            last_highlighted_filename = ""
        end if
    end subroutine init_renderer

    subroutine cleanup_renderer()
        if (allocated(screen_buffer%lines)) deallocate(screen_buffer%lines)
        call cleanup_highlighter(syntax_highlighter)
    end subroutine cleanup_renderer

    ! Reallocate the screen buffer for a new terminal size (init_renderer
    ! sized it once at startup; a resize would otherwise leave stale dims)
    subroutine resize_renderer(rows, cols)
        integer, intent(in) :: rows, cols
        integer :: i

        if (rows == screen_buffer%rows .and. cols == screen_buffer%cols) return

        screen_buffer%rows = rows
        screen_buffer%cols = cols
        screen_buffer%needs_full_redraw = .true.

        if (allocated(screen_buffer%lines)) deallocate(screen_buffer%lines)
        allocate(character(len=cols) :: screen_buffer%lines(rows))
        do i = 1, rows
            screen_buffer%lines(i) = repeat(' ', cols)
        end do
    end subroutine resize_renderer

    ! Update syntax highlighter for a new filename/language
    subroutine update_syntax_highlighter(filename)
        character(len=*), intent(in) :: filename

        ! Only update if filename has changed
        if (trim(filename) == trim(last_highlighted_filename)) return

        ! Cleanup old language definition and re-initialize
        call cleanup_highlighter(syntax_highlighter)
        call init_highlighter(syntax_highlighter, filename)
        last_highlighted_filename = trim(filename)
    end subroutine update_syntax_highlighter

    subroutine render_screen(buffer, editor, match_mode_active, match_case_sens)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(inout) :: editor
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        integer :: screen_row, buffer_line, line_count
        character(len=:), allocatable :: line_content
        character(len=1) :: cursor_char
        integer :: content_width
        integer :: start_row, row_offset_val
        character(len=16) :: line_num_str
        logical :: found_match
        type(cursor_t) :: cursor

        ! Auto-update syntax highlighter if filename changed
        if (allocated(editor%filename)) then
            call update_syntax_highlighter(editor%filename)
        end if

        call terminal_hide_cursor()

        ! Render tab bar if there are any tabs
        call render_tab_bar(editor)

        ! Check if cursor is on a bracket and find its match. The cursor
        ! column is a char index; utf8_char_at handles bounds and returns
        ! '' (padded to a space) past EOL. Multibyte chars truncate to
        ! their lead byte, which is never an ASCII bracket.
        cursor = editor%cursors(editor%active_cursor)
        line_content = buffer_get_line(buffer, cursor%line)
        cursor_char = utf8_char_at(line_content, cursor%column)
        if (is_opening_bracket(cursor_char) .or. is_closing_bracket(cursor_char)) then
            bracket_line = cursor%line
            bracket_col = cursor%column
            call find_matching_bracket(buffer, bracket_line, bracket_col, &
                                     found_match, matching_bracket_line, matching_bracket_col)
            if (.not. found_match) then
                matching_bracket_line = 0
                matching_bracket_col = 0
            end if
        else
            bracket_line = 0
            bracket_col = 0
            matching_bracket_line = 0
            matching_bracket_col = 0
        end if
        if (allocated(line_content)) deallocate(line_content)

        ! Get total lines in buffer
        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers)
        if (show_line_numbers) then
            content_width = editor%screen_cols - LINE_NUMBER_WIDTH - 1  ! -1 for separator
        else
            content_width = editor%screen_cols
        end if

        ! Determine starting row based on whether tabs exist
        if (size(editor%tabs) > 0) then
            start_row = 2  ! Tab bar at row 1
            row_offset_val = 2
        else
            start_row = 1  ! No tab bar
            row_offset_val = 1
        end if

        ! Render all panes for the active tab
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            editor%active_tab_index <= size(editor%tabs)) then
            if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                call render_all_panes(editor)
                ! Render status bar after panes
                call render_status_bar(editor, buffer, match_mode_active, match_case_sens)

                ! Render diagnostics panel if visible (for panes path)
                if (allocated(editor%filename)) then
                    block
                        character(len=:), allocatable :: file_uri, cwd
                        character(len=1024) :: cwd_buffer
                        integer :: cwd_len

                        ! Get absolute path for file URI
                        if (editor%filename(1:1) == '/') then
                            ! Already absolute
                            file_uri = 'file:///' // trim(editor%filename)
                        else
                            ! Relative path - get PWD from environment
                            call get_environment_variable("PWD", cwd_buffer, cwd_len)

                            if (cwd_len > 0) then
                                cwd = cwd_buffer(1:cwd_len)
                                file_uri = 'file://' // trim(cwd) // '/' // trim(editor%filename)
                            else
                                file_uri = 'file:///' // trim(editor%filename)
                            end if
                        end if

                        call render_diagnostics_panel(editor%diagnostics_panel, editor%diagnostics, &
                                                     file_uri, editor%screen_rows, editor%screen_cols)
                    end block
                end if

                ! Render references panel if visible (for panes path)
                call render_references_panel(editor%references_panel, 3)

                ! Render code actions menu if visible (for panes path)
                call render_code_actions_panel(editor%code_actions_panel, editor%screen_rows, editor%screen_cols)

                ! Render symbols panel if visible (for panes path)
                call render_symbols_panel(editor%symbols_panel, editor%screen_rows)

                ! Render LSP server installer panel if visible (for panes path)
                if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
                    call render_lsp_server_installer_panel(editor%lsp_installer_panel, &
                        editor%screen_cols)
                end if

                ! Render terminal panel if visible (for panes path)
                block
                    integer :: pane_term_h
                    pane_term_h = get_terminal_panel_height( &
                        editor%terminal_panel)
                    if (pane_term_h > 0) then
                        call terminal_panel_render( &
                            editor%terminal_panel, &
                            editor%screen_rows - pane_term_h, &
                            editor%screen_cols)
                    end if
                end block

                ! Skip editor cursor when a modal or terminal is focused
                if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel) .or. &
                    (is_terminal_panel_visible(editor%terminal_panel) .and. &
                     editor%terminal_panel%focused)) then
                    call terminal_hide_cursor()
                    call terminal_flush()
                else
                    call render_ghost_text(editor, buffer)
                    call render_completion_popup(editor%completion_popup)
                    call render_cursor_for_panes(editor)
                end if
                return  ! Exit after rendering panes
            end if
        end if

        ! Fallback to simple rendering if no tabs/panes
        ! Clear and render each visible line
        block
            integer :: term_h, editor_bottom
            term_h = get_terminal_panel_height(editor%terminal_panel)
            ! Editor content ends before terminal panel + separator + status bar
            if (term_h > 0) then
                editor_bottom = editor%screen_rows - term_h - 1
            else
                editor_bottom = editor%screen_rows - 1
            end if
        do screen_row = start_row, editor_bottom
                buffer_line = editor%viewport_line + screen_row - row_offset_val

                call terminal_move_cursor(screen_row, 1)

                ! Render line number if enabled
                if (show_line_numbers) then
                    if (buffer_line <= line_count) then
                        ! Format line number, right-aligned
                        write(line_num_str, '(i5)') buffer_line

                        if (buffer_line == editor%cursors(editor%active_cursor)%line) then
                            ! Highlight current line number
                            call terminal_write(char(27) // '[1;33m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                              // char(27) // '[0m ')
                        else
                            call terminal_write(char(27) // '[90m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                              // char(27) // '[0m ')
                        end if
                    else
                        ! Empty line number area for lines beyond file
                        call terminal_write(repeat(' ', LINE_NUMBER_WIDTH + 1))
                    end if
                end if

                if (buffer_line <= line_count) then
                    ! Render actual line content with selections
                    call render_line_with_selections(buffer, editor, buffer_line, &
                                                    editor%viewport_column, content_width)
                else
                    ! Beyond file content: '~' only, the ESC[K below clears
                    call terminal_write('~')
                end if
                ! Clear to end of line to prevent stale content when scrolling
                call terminal_write(char(27) // '[K')
            end do

        ! Render terminal panel if visible
        if (term_h > 0) then
            call terminal_panel_render(editor%terminal_panel, &
                editor%screen_rows - term_h, editor%screen_cols)
        end if
        end block

        ! Render status bar
        call render_status_bar(editor, buffer, match_mode_active, match_case_sens)

        ! Render diagnostics panel if visible
        if (allocated(editor%filename)) then
            block
                character(len=:), allocatable :: file_uri
                file_uri = 'file://' // trim(editor%filename)
                call render_diagnostics_panel(editor%diagnostics_panel, editor%diagnostics, &
                                             file_uri, editor%screen_rows, editor%screen_cols)
            end block
        end if

        ! Render references panel if visible
        call render_references_panel(editor%references_panel, 3)

        ! Render code actions menu if visible
        call render_code_actions_panel(editor%code_actions_panel, editor%screen_rows, editor%screen_cols)

        ! Render symbols panel if visible
        call render_symbols_panel(editor%symbols_panel, editor%screen_rows)

        ! Render LSP server installer panel if visible
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
            call render_lsp_server_installer_panel(editor%lsp_installer_panel, &
                editor%screen_cols)
        end if

        ! Skip editor cursor when a modal or terminal is focused
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel) .or. &
            (is_terminal_panel_visible(editor%terminal_panel) .and. &
             editor%terminal_panel%focused)) then
            call terminal_hide_cursor()
            call terminal_flush()
        else
            call render_ghost_text(editor, buffer)
            call render_completion_popup(editor%completion_popup)
            ! Position cursor for panes or regular view
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
                editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                    call render_cursor_for_panes(editor)
                else
                    call render_cursor(editor, buffer)
                end if
            else
                call render_cursor(editor, buffer)
            end if
        end if
    end subroutine render_screen

    subroutine render_line_with_selections(buffer, editor, line_num, start_col, width)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_num, start_col, width
        character(len=:), allocatable :: line, utf8_ch
        integer :: i, char_idx, byte_pos, token_idx, char_count, display_col, char_width
        integer :: sel_start_line, sel_start_col, sel_end_line, sel_end_col
        logical :: in_selection, is_bracket_match, is_current_line, is_search_match
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: token_color
        character(len=:), allocatable :: style, last_style
        integer :: search_matches(2, 50)  ! Up to 50 matches per line (start, end pairs)
        integer :: num_search_matches, match_idx
        integer :: line_byte_len

        line = buffer_get_line(buffer, line_num)
        line_byte_len = len(line)
        char_count = utf8_char_count(line)

        ! Get all search matches on this line (these use byte indices)
        if (search_mode_active) then
            call get_matches_on_line(line, line_num, search_matches, num_search_matches)
        else
            num_search_matches = 0
        end if

        ! Get syntax tokens for this line (tokens use byte indices)
        if (syntax_highlighter%enabled) then
            call tokenize_line(syntax_highlighter, line, tokens)
        else
            allocate(tokens(1))
            tokens(1)%type = TOKEN_PLAIN
            tokens(1)%start_col = 1
            tokens(1)%end_col = max(1, line_byte_len)
        end if

        ! Check if this is the current line
        is_current_line = (line_num == editor%cursors(editor%active_cursor)%line) .and. highlight_current_line

        ! Render each UTF-8 character with selection highlighting
        ! char_idx = 1-based character index (for selection logic)
        ! display_col = screen column position (for width tracking)
        ! byte_pos = byte position in string (for token lookup)
        ! Style codes are emitted only when they CHANGE between characters:
        ! wrapping every char in color+reset made frame size scale with the
        ! terminal area (~7x larger than needed at wide terminals).
        display_col = 0
        char_idx = start_col
        last_style = ''

        do while (char_idx <= char_count .and. display_col < width)
            in_selection = .false.
            is_bracket_match = .false.

            ! Get the UTF-8 character at this position
            utf8_ch = utf8_char_at(line, char_idx)
            char_width = utf8_display_width(utf8_ch)

            ! Expand tabs to spaces so display_col stays exact (a raw tab would
            ! advance the terminal to its own tab stop and desync the count).
            if (utf8_ch == char(9)) then
                char_width = min(TAB_WIDTH - mod(display_col, TAB_WIDTH), &
                                 width - display_col)
                utf8_ch = repeat(' ', char_width)
            end if

            ! Get byte position for token lookup
            byte_pos = utf8_char_to_byte_index(line, char_idx)

            ! Check if this position is in any cursor's selection
            ! (cursor positions are character indices, not byte indices)
            do i = 1, size(editor%cursors)
                if (editor%cursors(i)%has_selection) then
                    ! Determine selection bounds (handle both directions)
                    if (editor%cursors(i)%line < editor%cursors(i)%selection_start_line .or. &
                        (editor%cursors(i)%line == editor%cursors(i)%selection_start_line .and. &
                         editor%cursors(i)%column < editor%cursors(i)%selection_start_col)) then
                        ! Cursor is before selection start (selecting upward)
                        sel_start_line = editor%cursors(i)%line
                        sel_start_col = editor%cursors(i)%column
                        sel_end_line = editor%cursors(i)%selection_start_line
                        sel_end_col = editor%cursors(i)%selection_start_col
                    else
                        ! Cursor is after selection start (selecting downward)
                        sel_start_line = editor%cursors(i)%selection_start_line
                        sel_start_col = editor%cursors(i)%selection_start_col
                        sel_end_line = editor%cursors(i)%line
                        sel_end_col = editor%cursors(i)%column
                    end if

                    ! Check if this position is selected (using char_idx)
                    if (line_num > sel_start_line .and. line_num < sel_end_line) then
                        ! Fully selected line (between start and end)
                        in_selection = .true.
                        exit
                    else if (line_num == sel_start_line .and. line_num == sel_end_line) then
                        ! Single-line selection
                        if (char_idx >= sel_start_col .and. char_idx < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_start_line .and. line_num < sel_end_line) then
                        ! First line of multi-line selection
                        if (char_idx >= sel_start_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_end_line .and. line_num > sel_start_line) then
                        ! Last line of multi-line selection
                        if (char_idx < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    end if
                end if
            end do

            ! Check if this position is a bracket or its match (using char_idx)
            if ((line_num == bracket_line .and. char_idx == bracket_col) .or. &
                (line_num == matching_bracket_line .and. char_idx == matching_bracket_col)) then
                is_bracket_match = .true.
            end if

            ! Check if this position is part of a search match (search uses byte indices)
            is_search_match = .false.
            if (byte_pos > 0) then
                do match_idx = 1, num_search_matches
                    if (byte_pos >= search_matches(1, match_idx) .and. byte_pos <= search_matches(2, match_idx)) then
                        is_search_match = .true.
                        exit
                    end if
                end do
            end if

            ! Find which token this column belongs to (tokens use byte indices)
            token_color = ""
            if (syntax_highlighter%enabled .and. byte_pos > 0) then
                do token_idx = 1, size(tokens)
                    if (byte_pos >= tokens(token_idx)%start_col .and. byte_pos <= tokens(token_idx)%end_col) then
                        token_color = get_token_color(tokens(token_idx)%type)
                        exit
                    end if
                end do
            end if

            ! Determine this character's style (priority order preserved)
            if (in_selection) then
                ! Selected text: reverse video (highest priority)
                style = char(27) // '[7m'
            else if (is_bracket_match) then
                ! Matching brackets: cyan background
                style = char(27) // '[46m'
            else if (is_search_match) then
                ! Search matches: yellow background (+ syntax color)
                style = token_color // char(27) // '[43m'
            else if (is_current_line) then
                ! Current line: subtle background (+ syntax color)
                style = token_color // char(27) // '[48;5;236m'
            else
                ! Syntax color only (empty for plain text)
                style = token_color
            end if

            if (style /= last_style) then
                call terminal_write(char(27) // '[0m')
                if (len(style) > 0) call terminal_write(style)
                last_style = style
            end if
            call terminal_write(utf8_ch)

            display_col = display_col + char_width
            char_idx = char_idx + 1
        end do

        ! Fill remaining width. When the tail needs no styling (no
        ! current-line background, no selection anywhere), skip it: both
        ! callers follow this routine with ESC[K, which clears the same
        ! cells with default attributes at a fraction of the bytes.
        block
            logical :: fill_styled
            fill_styled = is_current_line
            if (.not. fill_styled) then
                do i = 1, size(editor%cursors)
                    if (editor%cursors(i)%has_selection) then
                        fill_styled = .true.
                        exit
                    end if
                end do
            end if
            if (.not. fill_styled) then
                if (len(last_style) > 0) call terminal_write(char(27) // '[0m')
                if (allocated(line)) deallocate(line)
                if (allocated(utf8_ch)) deallocate(utf8_ch)
                return
            end if
        end block
        do while (display_col < width)
            in_selection = .false.

            ! Check if end of line position is in selection
            do i = 1, size(editor%cursors)
                if (editor%cursors(i)%has_selection) then
                    ! Determine selection bounds (handle both directions)
                    if (editor%cursors(i)%line < editor%cursors(i)%selection_start_line .or. &
                        (editor%cursors(i)%line == editor%cursors(i)%selection_start_line .and. &
                         editor%cursors(i)%column < editor%cursors(i)%selection_start_col)) then
                        sel_start_line = editor%cursors(i)%line
                        sel_start_col = editor%cursors(i)%column
                        sel_end_line = editor%cursors(i)%selection_start_line
                        sel_end_col = editor%cursors(i)%selection_start_col
                    else
                        sel_start_line = editor%cursors(i)%selection_start_line
                        sel_start_col = editor%cursors(i)%selection_start_col
                        sel_end_line = editor%cursors(i)%line
                        sel_end_col = editor%cursors(i)%column
                    end if

                    ! Check if this position is selected (multi-line aware)
                    ! Use char_idx which is now past end of line content
                    if (line_num > sel_start_line .and. line_num < sel_end_line) then
                        ! Fully selected line
                        in_selection = .true.
                        exit
                    else if (line_num == sel_start_line .and. line_num == sel_end_line) then
                        ! Single-line selection
                        if (char_idx >= sel_start_col .and. char_idx < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_start_line .and. line_num < sel_end_line) then
                        ! First line of multi-line selection
                        if (char_idx >= sel_start_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_end_line .and. line_num > sel_start_line) then
                        ! Last line of multi-line selection
                        if (char_idx < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    end if
                end if
            end do

            if (in_selection) then
                style = char(27) // '[7m'
            else if (is_current_line) then
                style = char(27) // '[48;5;236m'
            else
                style = ''
            end if

            if (style /= last_style) then
                call terminal_write(char(27) // '[0m')
                if (len(style) > 0) call terminal_write(style)
                last_style = style
            end if
            call terminal_write(' ')

            display_col = display_col + 1
            char_idx = char_idx + 1
        end do

        ! Leave the terminal in a clean state (the caller's ESC[K must not
        ! inherit a lingering background)
        if (len(last_style) > 0) call terminal_write(char(27) // '[0m')

        if (allocated(line)) deallocate(line)
        if (allocated(utf8_ch)) deallocate(utf8_ch)
    end subroutine render_line_with_selections

    ! One-shot status-bar message. Commands used to write straight to the
    ! status row, which the very next render painted over -- so a failure like
    ! "no LSP server with rename support" was on screen for microseconds and
    ! the command looked like it had done nothing at all. Set it here instead
    ! and the status bar carries it until the next keystroke.
    subroutine set_status_message(msg)
        character(len=*), intent(in) :: msg

        if (allocated(g_status_message)) deallocate(g_status_message)
        g_status_message = trim(msg)
    end subroutine set_status_message

    subroutine clear_status_message()
        if (allocated(g_status_message)) deallocate(g_status_message)
    end subroutine clear_status_message

    function has_status_message() result(res)
        logical :: res
        res = .false.
        if (allocated(g_status_message)) res = len_trim(g_status_message) > 0
    end function has_status_message

    subroutine render_status_bar(editor, buffer, match_mode_active, match_case_sens)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        character(len=256) :: status_left, status_center, status_right
        character(len=:), allocatable :: status_bar
        character(len=200) :: fname_disp
        integer :: padding_len, left_pad, right_pad, fname_len
        type(cursor_t) :: cursor
        logical :: show_match_hint

        cursor = editor%cursors(editor%active_cursor)
        show_match_hint = .false.
        if (present(match_mode_active)) show_match_hint = match_mode_active

        ! Move to status bar position
        call terminal_move_cursor(editor%screen_rows, 1)

        ! Prepare status bar content. The filename is truncated to what the
        ! 256-char section buffer can hold: an internal write that overflows
        ! the record is a runtime error, so a very long path must never be
        ! formatted in unbounded.
        if (allocated(editor%filename)) then
            fname_len = len_trim(editor%filename)
            if (fname_len > len(fname_disp)) then
                fname_disp = '...' // &
                    editor%filename(fname_len - len(fname_disp) + 4:fname_len)
            else
                fname_disp = editor%filename
            end if
            write(status_left, '(a,a,a,a)') ' ctrl-b:fuss | ', trim(fname_disp), &
                   merge(' [modified]', '           ', buffer%modified), ' '
        else
            write(status_left, '(a,a,a)') ' ctrl-b:fuss | [No Name]', &
                   merge(' [modified]', '           ', buffer%modified), ' '
        end if

        ! Timed messages and LSP diagnostics take over the whole bar:
        ! always exactly one line, ellipsized to the terminal width.
        block
            type(diagnostic_t), allocatable :: line_diagnostics(:)
            character(len=:), allocatable :: file_uri
            integer(int64) :: now_ms

            ! Check for timed status message (persists ~2 seconds)
            now_ms = get_time_ms()
            if (len_trim(editor%timed_message) > 0 .and. &
                (now_ms - editor%timed_message_ms) < 2000) then
                call write_status_message(editor%screen_cols, ' ' // trim(editor%timed_message))
                return
            else if (len_trim(editor%timed_message) > 0) then
                editor%timed_message = ''  ! Expired, clear it
            end if

            ! Check for diagnostics at cursor position
            if (allocated(editor%filename)) then
                file_uri = 'file://' // trim(editor%filename)
                line_diagnostics = get_diagnostics_for_line(editor%diagnostics, file_uri, cursor%line)
            end if

            if (allocated(line_diagnostics) .and. size(line_diagnostics) > 0) then
                ! Show first diagnostic message (highest severity)
                call write_status_message(editor%screen_cols, &
                    ' ' // trim(line_diagnostics(1)%message))
                deallocate(line_diagnostics)
                return
            end if

            if (show_match_hint .and. present(match_case_sens)) then
                if (match_case_sens) then
                    status_center = '[Cc] alt-c:toggle'
                else
                    status_center = '[cc] alt-c:toggle'
                end if
            else
                status_center = 'ctrl-?:help'
            end if
        end block

        if (size(editor%cursors) > 1) then
            write(status_right, '(a,i0,a,a,i0,a,i0,a)') '[', size(editor%cursors), ' cursors] ', &
                   'Ln ', cursor%line, ', Col ', cursor%column, ' '
        else
            write(status_right, '(a,i0,a,i0,a)') 'Ln ', cursor%line, ', Col ', cursor%column, ' '
        end if

        ! A pending message replaces the left section; the caret position on
        ! the right is still worth keeping visible.
        if (has_status_message()) then
            status_left = ' ' // g_status_message
            status_center = ''
        end if

        ! Create full status bar with center text
        padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_center) - len_trim(status_right)
        if (padding_len > 0) then
            ! Distribute padding around center text
            left_pad = padding_len / 2
            right_pad = padding_len - left_pad
            status_bar = trim(status_left) // repeat(' ', left_pad) // &
                        trim(status_center) // repeat(' ', right_pad) // trim(status_right)
        else
            ! Not enough space for all three sections
            ! If in match mode, prioritize showing the hint by reducing right side info
            if (show_match_hint) then
                ! Show: left + hint + minimal right (just line/col, no cursor count)
                write(status_right, '(a,i0,a,i0,a)') 'Ln ', cursor%line, ',Col ', cursor%column, ' '
                padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_center) - len_trim(status_right)
                if (padding_len > 0) then
                    left_pad = padding_len / 2
                    right_pad = padding_len - left_pad
                    status_bar = trim(status_left) // repeat(' ', left_pad) // &
                                trim(status_center) // repeat(' ', right_pad) // trim(status_right)
                else
                    ! Still not enough space, show hint + right only
                    padding_len = editor%screen_cols - len_trim(status_center) - len_trim(status_right)
                    if (padding_len > 0) then
                        status_bar = repeat(' ', padding_len / 2) // trim(status_center) // &
                                    repeat(' ', padding_len - padding_len / 2) // trim(status_right)
                    else
                        ! Absolute minimum: just show the hint centered
                        padding_len = editor%screen_cols - len_trim(status_center)
                        if (padding_len > 0) then
                            left_pad = padding_len / 2
                            status_bar = repeat(' ', left_pad) // trim(status_center) // &
                                        repeat(' ', padding_len - left_pad)
                        else
                            status_bar = trim(status_center)
                        end if
                    end if
                end if
            else
                ! Normal mode: just show left and right
                padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_right)
                if (padding_len > 0) then
                    status_bar = trim(status_left) // repeat(' ', padding_len) // trim(status_right)
                else
                    status_bar = trim(status_left)
                end if
            end if
        end if

        ! Render with inverse video, clamped to the terminal width
        call write_status_message(editor%screen_cols, trim(status_bar))
    end subroutine render_status_bar

    ! Write one status-bar line in inverse video. The text is forced to
    ! exactly `width` columns: control characters are blanked (LSP
    ! messages can carry newlines/tabs that would wrap the bar onto the
    ! text area) and overlong text is ellipsized, never wrapped.
    subroutine write_status_message(width, text)
        integer, intent(in) :: width
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: line
        integer :: i, cut

        if (width < 1) return

        line = text
        do i = 1, len(line)
            if (iachar(line(i:i)) < 32 .or. iachar(line(i:i)) == 127) line(i:i) = ' '
        end do

        if (len(line) > width) then
            if (width > 3) then
                cut = width - 3
                ! Don't split a UTF-8 sequence at the cut point
                do while (cut > 1 .and. iachar(line(cut+1:cut+1)) >= 128 .and. &
                          iachar(line(cut+1:cut+1)) < 192)
                    cut = cut - 1
                end do
                line = line(1:cut) // '...' // repeat(' ', width - cut - 3)
            else
                line = line(1:width)
            end if
        else if (len(line) < width) then
            line = line // repeat(' ', width - len(line))
        end if

        call terminal_write(char(27) // '[7m')  ! Inverse video
        call terminal_write(line)
        call terminal_write(char(27) // '[0m')  ! Reset attributes
    end subroutine write_status_message

    ! Number of display cells between the viewport's first visible character
    ! (start_col, cell 0) and char_col, matching exactly how the line
    ! renderers advance: UTF-8 display widths, tabs expanded to TAB_WIDTH
    ! stops, one cell per virtual position past end of line. The caret must
    ! be placed with this, not with raw character-index arithmetic, or it
    ! drifts on lines containing tabs or wide characters.
    function display_offset_of(line, start_col, char_col) result(off)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_col, char_col
        integer :: off
        integer :: ci
        character(len=:), allocatable :: ch

        off = 0
        if (char_col <= start_col) return

        ci = start_col
        do while (ci < char_col)
            ch = utf8_char_at(line, ci)
            if (len(ch) == 0) then
                ! Past end of line: every remaining position is one cell
                off = off + (char_col - ci)
                return
            else if (ch == char(9)) then
                off = off + (TAB_WIDTH - mod(off, TAB_WIDTH))
            else
                off = off + utf8_display_width(ch)
            end if
            ci = ci + 1
        end do
    end function display_offset_of

    ! Inverse of display_offset_of: the character position occupying the
    ! display cell `cells` (0-based) right of the viewport start. Maps mouse
    ! clicks back to buffer columns; a click inside a tab's or wide char's
    ! span selects that character.
    function char_col_at_offset(line, start_col, cells) result(char_col)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_col, cells
        integer :: char_col
        integer :: off
        character(len=:), allocatable :: ch

        char_col = start_col
        off = 0
        do while (off < cells)
            ch = utf8_char_at(line, char_col)
            if (len(ch) == 0) then
                ! Past end of line: one cell per virtual position
                char_col = char_col + (cells - off)
                return
            else if (ch == char(9)) then
                off = off + (TAB_WIDTH - mod(off, TAB_WIDTH))
            else
                off = off + utf8_display_width(ch)
            end if
            char_col = char_col + 1
        end do
        ! Overshot: the click landed inside the previous char's span
        if (off > cells) char_col = char_col - 1
    end function char_col_at_offset

    ! True when the active cursor has a selection. During a selection
    ! the hardware cursor is hidden and a hollow-box caret is drawn by
    ! the line renderer instead, so the highlight reads uniformly.
    logical function active_has_sel(editor)
        type(editor_state_t), intent(in) :: editor
        active_has_sel = .false.
        if (allocated(editor%cursors) .and. editor%active_cursor >= 1 .and. &
            editor%active_cursor <= size(editor%cursors)) then
            active_has_sel = editor%cursors(editor%active_cursor)%has_selection
        end if
    end function active_has_sel

    ! Show or hide the hardware cursor based on selection state
    subroutine show_caret_unless_selecting(editor)
        type(editor_state_t), intent(in) :: editor
        if (active_has_sel(editor)) then
            call terminal_hide_cursor()
        else
            call terminal_show_cursor()
        end if
    end subroutine show_caret_unless_selecting

    subroutine render_cursor(editor, buffer)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        type(cursor_t) :: cursor
        integer :: screen_row, screen_col
        integer :: i
        integer :: col_offset, row_offset, min_row
        character(len=:), allocatable :: line
        character(len=:), allocatable :: cursor_char  ! Full UTF-8 char, not one byte

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Account for tab bar offset - when tabs exist, row 1 is tab bar, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2  ! Tab bar takes row 1
            min_row = 2     ! Cursor cannot be in row 1 (tab bar)
        else
            row_offset = 1  ! No tab bar
            min_row = 1     ! Cursor can be in row 1
        end if

        ! For multiple cursors, show them all with block cursor for inactive ones
        if (size(editor%cursors) > 1) then
            ! First draw all inactive cursors
            do i = 1, size(editor%cursors)
                if (i /= editor%active_cursor) then
                    cursor = editor%cursors(i)

                    ! Selection highlight already marks selected cursors; a
                    ! block caret here would highlight the char past the
                    ! selection end (e.g. the '.' after a ctrl-d word select)
                    if (cursor%has_selection) cycle

                    ! Calculate screen position from buffer position
                    screen_row = cursor%line - editor%viewport_line + row_offset

                    ! Screen column in display cells from the viewport start
                    line = buffer_get_line(buffer, cursor%line)
                    screen_col = col_offset + 1 + &
                        display_offset_of(line, editor%viewport_column, cursor%column)

                    ! Ensure cursor is within screen bounds and not in tab bar
                    if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                        screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                        ! Get the character at this cursor position
                        if (cursor%column <= utf8_char_count(line)) then
                            cursor_char = utf8_char_at(line, cursor%column)
                        else
                            cursor_char = ' '  ! End of line
                        end if

                        ! Inactive cursor - draw character with reverse video
                        call terminal_move_cursor(screen_row, screen_col)
                        call terminal_write(char(27) // '[7m' // cursor_char)  ! Inverse video
                        call terminal_write(char(27) // '[0m')   ! Reset
                    end if
                end if
            end do

            ! Then position terminal cursor at active cursor location
            cursor = editor%cursors(editor%active_cursor)
            screen_row = cursor%line - editor%viewport_line + row_offset

            ! Screen column in display cells from the viewport start
            line = buffer_get_line(buffer, cursor%line)
            screen_col = col_offset + 1 + &
                display_offset_of(line, editor%viewport_column, cursor%column)

            if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                call terminal_move_cursor(screen_row, screen_col)
                call show_caret_unless_selecting(editor)
            end if
        else
            ! Single cursor mode
            cursor = editor%cursors(editor%active_cursor)

            ! Calculate screen position from buffer position
            screen_row = cursor%line - editor%viewport_line + row_offset
            line = buffer_get_line(buffer, cursor%line)
            screen_col = col_offset + 1 + &
                display_offset_of(line, editor%viewport_column, cursor%column)

            ! Ensure cursor is within screen bounds and not in tab bar
            if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                call terminal_move_cursor(screen_row, screen_col)
                call show_caret_unless_selecting(editor)
            end if
        end if
    end subroutine render_cursor

    ! Fast path for cursor-only updates - just update cursor and status bar
    ! Use this when only cursor position changed, not buffer content
    subroutine render_cursor_only(buffer, editor, match_mode_active, match_case_sens)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(inout) :: editor
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        integer :: editor_start_col, editor_width

        ! Just render the status bar and position cursor
        call render_status_bar(editor, buffer, match_mode_active, match_case_sens)

        ! Fuss mode splits 30% tree / 70% editor; compute the same split as
        ! render_screen_with_tree (a hardcoded 31/cols-30 here only agreed
        ! with the renderer near 97 columns)
        editor_start_col = editor%screen_cols * 30 / 100 + 2
        editor_width = editor%screen_cols - editor_start_col + 1

        ! Handle panes vs single buffer
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            editor%active_tab_index <= size(editor%tabs)) then
            if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                if (editor%fuss_mode_active) then
                    call render_cursor_for_panes_with_tree(editor, editor_start_col, editor_width)
                else
                    call render_cursor_for_panes(editor)
                end if
                return
            end if
        end if

        ! Single buffer mode
        if (editor%fuss_mode_active) then
            call render_cursor_in_pane(editor, buffer, editor_start_col, editor_width)
        else
            call render_cursor(editor, buffer)
        end if
    end subroutine render_cursor_only

    subroutine update_viewport(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t) :: cursor
        integer :: margin = 3  ! Lines to keep visible above/below cursor
        integer :: v_margin, h_margin
        integer :: tab_idx, pane_idx
        integer :: pane_height, pane_width
        integer :: screen_width, screen_height

        ! If we have panes, update the active pane's viewport
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
            tab_idx = editor%active_tab_index
            if (allocated(editor%tabs(tab_idx)%panes)) then
                pane_idx = editor%tabs(tab_idx)%active_pane_index
                if (pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_idx)%panes)) then

                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors) .and. &
                        editor%tabs(tab_idx)%panes(pane_idx)%active_cursor > 0) then
                        cursor = editor%tabs(tab_idx)%panes(pane_idx)%cursors(&
                                 editor%tabs(tab_idx)%panes(pane_idx)%active_cursor)

                        ! Calculate pane dimensions (account for fuss mode)
                        if (editor%fuss_mode_active) then
                            ! Fuss mode: editor takes ~70% of screen
                            screen_width = editor%screen_cols * 70 / 100
                        else
                            screen_width = editor%screen_cols
                        end if
                        screen_height = editor%screen_rows - 2
                        pane_height = int((editor%tabs(tab_idx)%panes(pane_idx)%y_end - &
                                          editor%tabs(tab_idx)%panes(pane_idx)%y_start) * real(screen_height))
                        pane_width = int((editor%tabs(tab_idx)%panes(pane_idx)%x_end - &
                                         editor%tabs(tab_idx)%panes(pane_idx)%x_start) * real(screen_width))

                        ! Account for line numbers in pane width
                        if (show_line_numbers) then
                            pane_width = pane_width - LINE_NUMBER_WIDTH - 1
                        end if

                        ! The margin must shrink with the pane: with the fixed
                        ! margin the two scroll conditions overlap once the
                        ! pane is shorter than ~2*margin lines, pinning (or
                        ! oscillating) the viewport a few lines away from the
                        ! cursor. The cursor line then fails the renderer's
                        ! bounds check and the caret parks at the pane origin
                        ! while typing continues off-screen.
                        v_margin = min(margin, max(0, (pane_height - 1) / 2))
                        h_margin = min(margin, max(0, (pane_width - 1) / 2))

                        ! Vertical scrolling for pane
                        if (cursor%line < editor%tabs(tab_idx)%panes(pane_idx)%viewport_line + v_margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = max(1, cursor%line - v_margin)
                        else if (cursor%line > editor%tabs(tab_idx)%panes(pane_idx)%viewport_line + pane_height - v_margin - 1) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = cursor%line - pane_height + v_margin + 1
                        end if

                        ! Never scroll past the last buffer line: a stale
                        ! cursor (e.g. restored session state for another
                        ! file) must not blank the whole view.
                        editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = &
                            min(editor%tabs(tab_idx)%panes(pane_idx)%viewport_line, &
                                max(1, buffer_get_line_count(editor%tabs(tab_idx)%panes(pane_idx)%buffer)))

                        ! Horizontal scrolling for pane
                        if (cursor%column < editor%tabs(tab_idx)%panes(pane_idx)%viewport_column + h_margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = max(1, cursor%column - h_margin)
                        else if (cursor%column > editor%tabs(tab_idx)%panes(pane_idx)%viewport_column + pane_width - h_margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = cursor%column - pane_width + h_margin
                        end if

                        ! Hard guarantee, independent of the margin math: the
                        ! cursor cell stays inside the visible window.
                        call clamp_viewport( &
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_line, &
                            cursor%line, pane_height)
                        call clamp_viewport( &
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_column, &
                            cursor%column, pane_width)

                        ! Also update legacy editor viewport for compatibility
                        editor%viewport_line = editor%tabs(tab_idx)%panes(pane_idx)%viewport_line
                        editor%viewport_column = editor%tabs(tab_idx)%panes(pane_idx)%viewport_column
                    end if
                    return
                end if
            end if
        end if

        ! Fallback to original behavior if no panes
        cursor = editor%cursors(editor%active_cursor)

        ! Adaptive margins, as in the pane path above
        screen_height = editor%screen_rows - 2
        v_margin = min(margin, max(0, (screen_height - 1) / 2))

        ! Vertical scrolling
        if (cursor%line < editor%viewport_line + v_margin) then
            editor%viewport_line = max(1, cursor%line - v_margin)
        else if (cursor%line > editor%viewport_line + editor%screen_rows - v_margin - 2) then
            ! -2 for status bar and margin
            editor%viewport_line = cursor%line - editor%screen_rows + v_margin + 2
        end if

        ! Never scroll past the last buffer line (see pane path above)
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            editor%active_tab_index <= size(editor%tabs)) then
            editor%viewport_line = min(editor%viewport_line, &
                max(1, buffer_get_line_count(editor%tabs(editor%active_tab_index)%buffer)))
        end if

        ! Horizontal scrolling (account for fuss mode and line numbers)
        if (editor%fuss_mode_active) then
            screen_width = editor%screen_cols * 70 / 100
        else
            screen_width = editor%screen_cols
        end if

        ! Account for line numbers
        if (show_line_numbers) then
            screen_width = screen_width - LINE_NUMBER_WIDTH - 1
        end if
        h_margin = min(margin, max(0, (screen_width - 1) / 2))

        if (cursor%column < editor%viewport_column + h_margin) then
            editor%viewport_column = max(1, cursor%column - h_margin)
        else if (cursor%column > editor%viewport_column + screen_width - h_margin) then
            editor%viewport_column = cursor%column - screen_width + h_margin
        end if

        call clamp_viewport(editor%viewport_line, cursor%line, screen_height)
        call clamp_viewport(editor%viewport_column, cursor%column, screen_width)
    end subroutine update_viewport

    ! Force the viewport origin so that `pos` falls inside a window of
    ! `extent` cells starting at `viewport`. Backstop for the margin-based
    ! scrolling above: whatever the margins produced, the cursor cell must
    ! be on screen (extent is clamped to at least 1 for degenerate panes).
    subroutine clamp_viewport(viewport, pos, extent)
        integer(int32), intent(inout) :: viewport
        integer(int32), intent(in) :: pos
        integer, intent(in) :: extent

        if (viewport > pos) viewport = pos
        if (viewport < pos - max(1, extent) + 1) then
            viewport = pos - max(1, extent) + 1
        end if
        if (viewport < 1) viewport = 1
    end subroutine clamp_viewport

    ! Render screen with split panes (tree on left, editor on right)
    subroutine render_screen_with_tree(buffer, editor, match_mode_active, match_case_sens)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(inout) :: editor
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        integer :: tree_width, editor_start_col, editor_width
        integer :: separator_col

        call terminal_hide_cursor()

        ! Calculate split: 30% for tree, 70% for editor
        tree_width = editor%screen_cols * 30 / 100
        separator_col = tree_width + 1
        editor_start_col = tree_width + 2
        editor_width = editor%screen_cols - editor_start_col + 1

        ! Clear row 1 left side (tree area on tab bar row, not covered by other components)
        call terminal_move_cursor(1, 1)
        call terminal_write(repeat(' ', tree_width))

        ! Render tab bar if there are any tabs (positioned in editor pane area)
        call render_tab_bar(editor, editor_start_col, editor_width)

        ! Calculate editor area bottom (accounting for terminal panel)
        block
            integer :: term_h, content_bottom

            term_h = get_terminal_panel_height(editor%terminal_panel)
            if (term_h > 0) then
                content_bottom = editor%screen_rows - term_h - 1
            else
                content_bottom = editor%screen_rows - 1
            end if

        ! Render editor FIRST so its ESC[K can't destroy file tree content.
        call render_editor_area_with_tree(editor, editor_start_col, editor_width)

        ! Render file tree in left pane
        call render_file_tree(tree_state, 2, content_bottom, 2, &
            tree_width - 2, editor%fuss_hints_expanded, &
            fuss_git_prefix_active)

        ! Render vertical separator
        call render_vertical_separator(separator_col, 2, &
            content_bottom)

        ! Render terminal panel if visible
        if (term_h > 0) then
            call terminal_panel_render(editor%terminal_panel, &
                editor%screen_rows - term_h, editor%screen_cols)
        end if
        end block

        ! Render status bar (full width)
        call render_status_bar(editor, buffer, match_mode_active, match_case_sens)

        ! Render diagnostics panel if visible
        if (allocated(editor%filename)) then
            block
                character(len=:), allocatable :: file_uri
                file_uri = 'file://' // trim(editor%filename)
                call render_diagnostics_panel(editor%diagnostics_panel, editor%diagnostics, &
                                             file_uri, editor%screen_rows, editor%screen_cols)
            end block
        end if

        ! Render references panel if visible
        call render_references_panel(editor%references_panel, 3)

        ! Render code actions menu if visible
        call render_code_actions_panel(editor%code_actions_panel, editor%screen_rows, editor%screen_cols)

        ! Render symbols panel if visible
        call render_symbols_panel(editor%symbols_panel, editor%screen_rows)

        ! Render LSP server installer panel if visible
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
            call render_lsp_server_installer_panel(editor%lsp_installer_panel, &
                editor%screen_cols)
        end if

        ! Skip editor cursor when a modal or terminal is focused
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel) .or. &
            (is_terminal_panel_visible(editor%terminal_panel) .and. &
             editor%terminal_panel%focused)) then
            call terminal_hide_cursor()
            call terminal_flush()
        else
            ! Position cursor in editor pane
            if (size(editor%tabs(editor%active_tab_index)%panes) > 1) then
                call render_cursor_for_panes_with_tree(editor, editor_start_col, editor_width)
            else
                call render_cursor_in_pane(editor, buffer, editor_start_col, editor_width)
            end if
            call show_caret_unless_selecting(editor)
        end if
    end subroutine render_screen_with_tree

    subroutine render_vertical_separator(col, start_row, end_row)
        integer, intent(in) :: col, start_row, end_row
        integer :: row

        do row = start_row, end_row
            call terminal_move_cursor(row, col)
            call terminal_write(char(27) // '[90m│' // char(27) // '[0m')  ! Gray vertical line
        end do
    end subroutine render_vertical_separator

    subroutine render_editor_area_with_tree(editor, start_col, width)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: start_col, width
        type(pane_t) :: pane
        integer :: i, tab_idx, n_panes
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_height

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) then
            ! No valid tab, render empty
            return
        end if

        if (.not. allocated(editor%tabs(tab_idx)%panes)) then
            ! No panes, render empty
            return
        end if

        n_panes = size(editor%tabs(tab_idx)%panes)
        if (n_panes == 0) return

        screen_height = editor%screen_rows - 2  ! Account for tab bar and status bar

        ! If only one pane, use simple rendering
        if (n_panes == 1) then
            ! Use the pane's buffer, not the passed buffer parameter
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, start_col, width)
            return
        end if

        ! Multiple panes: render each with adjusted coordinates for tree view
        ! Clear the editor area first
        do i = 2, editor%screen_rows - 1
            call terminal_move_cursor(i, start_col)
            call terminal_write(repeat(' ', width))
        end do

        ! Render each pane with coordinates adjusted for tree offset
        do i = 1, n_panes
            pane = editor%tabs(tab_idx)%panes(i)

            ! Calculate pane position relative to editor area (not full screen)
            pane_col = start_col + int(pane%x_start * real(width))
            if (i < n_panes) then
                pane_width = int((pane%x_end - pane%x_start) * real(width)) - 1
            else
                pane_width = int((pane%x_end - pane%x_start) * real(width))
            end if
            pane_row = 2 + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            ! Store the calculated screen coordinates in the pane
            editor%tabs(tab_idx)%panes(i)%screen_col = pane_col
            editor%tabs(tab_idx)%panes(i)%screen_row = pane_row
            editor%tabs(tab_idx)%panes(i)%screen_width = pane_width
            editor%tabs(tab_idx)%panes(i)%screen_height = pane_height

            ! Render the pane content
            call render_single_pane(editor, i, pane_col, pane_row, pane_width, pane_height)

            ! Draw vertical separator between panes
            if (i < n_panes) then
                call render_pane_separator(pane_col + pane_width, pane_row, pane_height)
            end if
        end do
    end subroutine render_editor_area_with_tree

    subroutine render_editor_pane(buffer, editor, start_col, width)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: start_col, width
        integer :: screen_row, buffer_line, line_count
        integer :: adjusted_width, line_num_width
        integer :: start_row
        character(len=16) :: line_num_str

        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers if enabled)
        if (show_line_numbers) then
            line_num_width = LINE_NUMBER_WIDTH + 1
            adjusted_width = width - line_num_width
        else
            line_num_width = 0
            adjusted_width = width
        end if

        ! Determine starting row (account for tab bar)
        if (size(editor%tabs) > 0) then
            start_row = 2  ! Tab bar at row 1
        else
            start_row = 1  ! No tab bar
        end if

        ! Render each visible line in the editor pane
        do screen_row = start_row, editor%screen_rows - 1
            buffer_line = editor%viewport_line + screen_row - start_row

            ! Position cursor at start of this line in the pane
            call terminal_move_cursor(screen_row, start_col)

            ! Render line number if enabled
            if (show_line_numbers) then
                if (buffer_line <= line_count) then
                    write(line_num_str, '(i5)') buffer_line
                    if (buffer_line == editor%cursors(editor%active_cursor)%line) then
                        call terminal_write(char(27) // '[1;33m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                          // char(27) // '[0m ')
                    else
                        call terminal_write(char(27) // '[90m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                          // char(27) // '[0m ')
                    end if
                else
                    call terminal_write(repeat(' ', line_num_width))
                end if
            end if

            ! Render content (render_line_with_selections writes at most
            ! adjusted_width cells; the ESC[K below clears the rest)
            if (buffer_line <= line_count) then
                call render_line_with_selections(buffer, editor, buffer_line, &
                                                editor%viewport_column, adjusted_width)
            else
                ! Empty line beyond file content ('~' only; ESC[K clears)
                call terminal_write('~')
            end if
            ! Clear to end of line to prevent stale content when scrolling
            call terminal_write(char(27) // '[K')
        end do
    end subroutine render_editor_pane

    subroutine render_all_panes(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        type(pane_t) :: pane
        integer :: i, tab_idx, n_panes, active_pane_idx
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_width, screen_height
        character(len=:), allocatable :: line_content
        character(len=1) :: cursor_char
        logical :: found_match
        type(cursor_t) :: active_cursor

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        n_panes = size(editor%tabs(tab_idx)%panes)
        if (n_panes == 0) return

        active_pane_idx = editor%tabs(tab_idx)%active_pane_index

        ! Calculate bracket matching for the active pane's cursor
        bracket_line = 0
        bracket_col = 0
        matching_bracket_line = 0
        matching_bracket_col = 0

        if (active_pane_idx > 0 .and. active_pane_idx <= n_panes) then
            pane = editor%tabs(tab_idx)%panes(active_pane_idx)
            if (allocated(pane%cursors) .and. size(pane%cursors) > 0) then
                active_cursor = pane%cursors(1)  ! Use first cursor for bracket matching
                line_content = buffer_get_line(pane%buffer, active_cursor%line)
                ! Char-index lookup (see render_screen bracket check)
                cursor_char = utf8_char_at(line_content, active_cursor%column)
                if (is_opening_bracket(cursor_char) .or. is_closing_bracket(cursor_char)) then
                    bracket_line = active_cursor%line
                    bracket_col = active_cursor%column
                    call find_matching_bracket(pane%buffer, bracket_line, bracket_col, &
                                             found_match, matching_bracket_line, matching_bracket_col)
                    if (.not. found_match) then
                        matching_bracket_line = 0
                        matching_bracket_col = 0
                    end if
                end if
                if (allocated(line_content)) deallocate(line_content)
            end if
        end if

        ! Get screen dimensions
        screen_width = editor%screen_cols
        screen_height = editor%screen_rows - 2  ! Account for tab bar and status bar

        ! Reduce height if terminal panel is visible
        block
            integer :: tp_h
            tp_h = get_terminal_panel_height(editor%terminal_panel)
            if (tp_h > 0) screen_height = screen_height - tp_h
        end block

        ! Reduce width if diagnostics panel is visible
        if (editor%diagnostics_panel%visible) then
            screen_width = screen_width - editor%diagnostics_panel%width
        end if

        ! Reduce width if references panel is visible
        if (editor%references_panel%visible) then
            screen_width = screen_width - editor%references_panel%width
        end if

        ! If only one pane, render full screen
        if (n_panes == 1) then
            ! Set screen coordinates for the single pane
            editor%tabs(tab_idx)%panes(1)%screen_col = 1
            editor%tabs(tab_idx)%panes(1)%screen_row = 2  ! After tab bar
            editor%tabs(tab_idx)%panes(1)%screen_width = screen_width
            editor%tabs(tab_idx)%panes(1)%screen_height = screen_height

            ! Use the pane's buffer, not the passed buffer parameter
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, 1, screen_width)
            return
        end if

        ! Clear the editor area first with background
        do i = 2, editor%screen_rows - 1
            call terminal_move_cursor(i, 1)
            call terminal_write(repeat(' ', screen_width))
        end do

        ! Render each pane with gaps
        do i = 1, n_panes
            pane = editor%tabs(tab_idx)%panes(i)

            ! Calculate actual screen coordinates with gap consideration
            ! Add 1 column gap on right side of each pane except the last
            pane_col = 1 + int(pane%x_start * real(screen_width))
            if (i < n_panes) then
                ! Reserve 1 column for the border/gap
                pane_width = int((pane%x_end - pane%x_start) * real(screen_width)) - 1
            else
                ! Last pane uses full width
                pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
            end if
            pane_row = 2 + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            ! Store the calculated screen coordinates in the pane
            editor%tabs(tab_idx)%panes(i)%screen_col = pane_col
            editor%tabs(tab_idx)%panes(i)%screen_row = pane_row
            editor%tabs(tab_idx)%panes(i)%screen_width = pane_width
            editor%tabs(tab_idx)%panes(i)%screen_height = pane_height

            ! Render the pane content
            call render_single_pane(editor, i, pane_col, pane_row, pane_width, pane_height)

            ! Draw vertical separator between panes
            if (i < n_panes) then
                call render_pane_separator(pane_col + pane_width, pane_row, pane_height)
            end if
        end do
    end subroutine render_all_panes

    subroutine render_single_pane(editor, pane_idx, col, row, width, height)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_idx, col, row, width, height
        type(pane_t) :: pane
        integer :: screen_row, buffer_line, content_start_row, content_height
        integer :: tab_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(pane_idx)

        ! Draw pane header with filename (if more than one pane exists)
        if (size(editor%tabs(tab_idx)%panes) > 1) then
            call render_pane_header(pane, col, row, width)
            content_start_row = row + 1
            content_height = height - 1
        else
            content_start_row = row
            content_height = height
        end if

        ! Clear the pane area with subtle background for inactive panes
        do screen_row = content_start_row, content_start_row + content_height - 1
            call terminal_move_cursor(screen_row, col)
            if (.not. pane%is_active) then
                ! Subtle dark background for inactive panes
                call terminal_write(char(27) // '[48;5;234m')  ! Very dark gray
            end if
            call terminal_write(repeat(' ', width))
            call terminal_write(char(27) // '[0m')
        end do

        ! Render buffer content with pane's viewport (use pane's own buffer)
        do screen_row = content_start_row, content_start_row + content_height - 1
            buffer_line = pane%viewport_line + (screen_row - content_start_row)
            if (buffer_line > 0 .and. buffer_line <= buffer_get_line_count(pane%buffer)) then
                call render_buffer_line_in_pane(pane%buffer, editor, pane_idx, buffer_line, &
                                               screen_row, col, width)
            else
                ! Render empty line indicator for lines beyond file
                call terminal_move_cursor(screen_row, col)

                ! Render empty line number area if line numbers are enabled
                if (show_line_numbers) then
                    if (.not. pane%is_active) then
                        call terminal_write(char(27) // '[48;5;234m')  ! Dark gray for inactive
                    end if
                    call terminal_write(repeat(' ', LINE_NUMBER_WIDTH + 1))
                end if

                ! Render the ~ indicator
                if (.not. pane%is_active) then
                    call terminal_write(char(27) // '[48;5;234m')  ! Dark gray for inactive
                end if
                call terminal_write('~')

                ! Calculate remaining width accounting for line numbers
                if (show_line_numbers) then
                    if (width > LINE_NUMBER_WIDTH + 2) then
                        call terminal_write(repeat(' ', width - LINE_NUMBER_WIDTH - 2))
                    end if
                else
                    if (width > 1) then
                        call terminal_write(repeat(' ', width - 1))
                    end if
                end if
                call terminal_write(char(27) // '[0m')
            end if
        end do
    end subroutine render_single_pane

    subroutine render_pane_header(pane, col, row, width)
        use editor_state_module, only: pane_t
        type(pane_t), intent(in) :: pane
        integer, intent(in) :: col, row, width
        character(len=:), allocatable :: filename_display, filename_only
        character(len=256) :: temp_display
        integer :: slash_pos, display_len, padding_left, padding_right

        ! Move to header position
        call terminal_move_cursor(row, col)

        ! Extract filename from path
        if (allocated(pane%filename)) then
            ! Find last slash to get just the filename
            slash_pos = index(pane%filename, '/', back=.true.)
            if (slash_pos > 0) then
                filename_only = pane%filename(slash_pos+1:)
            else
                filename_only = pane%filename
            end if

            ! Build display string with brackets
            write(temp_display, '(A,A,A)') ' [', trim(filename_only), '] '
            filename_display = trim(temp_display)
        else
            filename_display = ' [untitled] '
        end if

        ! Calculate padding
        display_len = len(filename_display)
        if (display_len < width) then
            padding_left = (width - display_len) / 2
            padding_right = width - display_len - padding_left
        else
            ! Truncate if too long
            filename_display = filename_display(1:width)
            padding_left = 0
            padding_right = 0
        end if

        ! Draw header with reverse video (like tab bar)
        if (pane%is_active) then
            ! Active pane: bright reverse video
            call terminal_write(char(27) // '[7m')  ! Reverse video
        else
            ! Inactive pane: dimmed reverse video
            call terminal_write(char(27) // '[2;7m')  ! Dim + reverse video
        end if

        ! Draw the header line
        call terminal_write(repeat('─', padding_left))
        call terminal_write(filename_display)
        call terminal_write(repeat('─', padding_right))

        ! Reset attributes
        call terminal_write(char(27) // '[0m')
    end subroutine render_pane_header

    subroutine render_buffer_line_in_pane(buffer, editor, pane_idx, line_num, screen_row, col, width)
        use editor_state_module, only: pane_t
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_idx, line_num, screen_row, col, width
        character(len=:), allocatable :: line, utf8_ch
        type(pane_t) :: pane
        integer :: tab_idx, i, char_idx, char_count, display_col, char_width
        integer :: content_width, content_col
        character(len=5) :: line_num_str
        logical :: is_current_line, in_selection, is_bracket_match
        integer :: sel_start_line, sel_start_col, sel_end_line, sel_end_col
        ! Syntax highlighting support
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: token_color
        character(len=:), allocatable :: style, last_style
        integer :: byte_pos, token_idx, line_byte_len

        tab_idx = editor%active_tab_index
        pane = editor%tabs(tab_idx)%panes(pane_idx)

        ! Move to position
        call terminal_move_cursor(screen_row, col)

        ! Render line number if enabled
        if (show_line_numbers) then
            write(line_num_str, '(i5)') line_num
            ! Check if this line has any cursor
            is_current_line = .false.
            if (allocated(pane%cursors)) then
                do i = 1, size(pane%cursors)
                    if (pane%cursors(i)%line == line_num) then
                        is_current_line = .true.
                        exit
                    end if
                end do
            end if

            if (pane%is_active) then
                ! Active pane: line number on the default background.
                if (is_current_line) then
                    call terminal_write(char(27) // '[1;33m' // &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // char(27) // '[0m ')
                else
                    call terminal_write(char(27) // '[90m' // &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // char(27) // '[0m ')
                end if
            else
                ! Inactive pane: keep the dim background continuous across the
                ! whole gutter (number + separator) so no default-background
                ! stripe shows through, and use a fixed gray (not [90m, whose
                ! shade varies by terminal and can vanish into the background).
                call terminal_write(char(27) // '[48;5;234m' // char(27) // '[38;5;245m' // &
                    adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' ' // char(27) // '[0m')
            end if

            ! Continue with pane background for content
            if (.not. pane%is_active) then
                call terminal_write(char(27) // '[48;5;234m')  ! Dark gray background
            end if

            content_width = width - LINE_NUMBER_WIDTH - 1
            content_col = col + LINE_NUMBER_WIDTH + 1
        else
            content_width = width
            content_col = col
        end if

        ! Get the line content
        line = buffer_get_line(buffer, line_num)
        if (.not. allocated(line)) return

        ! Get character count for UTF-8 iteration
        char_count = utf8_char_count(line)
        line_byte_len = len(line)

        ! Get syntax tokens for this line
        if (syntax_highlighter%enabled) then
            call tokenize_line(syntax_highlighter, line, tokens)
        else
            allocate(tokens(1))
            tokens(1)%type = TOKEN_PLAIN
            tokens(1)%start_col = 1
            tokens(1)%end_col = max(1, line_byte_len)
        end if

        ! Check if this is the current line with a cursor
        is_current_line = .false.
        if (allocated(pane%cursors)) then
            do i = 1, size(pane%cursors)
                if (pane%cursors(i)%line == line_num) then
                    is_current_line = .true.
                    exit
                end if
            end do
        end if

        ! Render the line character by character with selection highlighting
        ! Using UTF-8 aware iteration. Style codes are emitted only when
        ! they change between characters (see render_line_with_selections).
        display_col = 0
        char_idx = pane%viewport_column  ! Start from viewport column (character index)
        last_style = ''

        do while (char_idx <= char_count .and. display_col < content_width)
            in_selection = .false.

            ! Get the UTF-8 character at this position
            utf8_ch = utf8_char_at(line, char_idx)
            char_width = utf8_display_width(utf8_ch)

            ! Expand tabs to spaces so column accounting is exact. Writing a raw
            ! tab would let the terminal advance to its own tab stop (which
            ! depends on the pane's absolute column), desyncing display_col and
            ! spilling the line into the neighbouring pane.
            if (utf8_ch == char(9)) then
                char_width = min(TAB_WIDTH - mod(display_col, TAB_WIDTH), &
                                 content_width - display_col)
                utf8_ch = repeat(' ', char_width)
            end if

            ! Check if this position is in any cursor's selection (use pane's cursors)
            if (allocated(pane%cursors)) then
                do i = 1, size(pane%cursors)
                    if (pane%cursors(i)%has_selection) then
                        ! Determine selection bounds (handle both directions)
                        if (pane%cursors(i)%line < pane%cursors(i)%selection_start_line .or. &
                            (pane%cursors(i)%line == pane%cursors(i)%selection_start_line .and. &
                             pane%cursors(i)%column < pane%cursors(i)%selection_start_col)) then
                            ! Cursor is before selection start (selecting upward)
                            sel_start_line = pane%cursors(i)%line
                            sel_start_col = pane%cursors(i)%column
                            sel_end_line = pane%cursors(i)%selection_start_line
                            sel_end_col = pane%cursors(i)%selection_start_col
                        else
                            ! Cursor is after selection start (selecting downward)
                            sel_start_line = pane%cursors(i)%selection_start_line
                            sel_start_col = pane%cursors(i)%selection_start_col
                            sel_end_line = pane%cursors(i)%line
                            sel_end_col = pane%cursors(i)%column
                        end if

                        ! Check if this position is selected (using char_idx)
                        if (line_num > sel_start_line .and. line_num < sel_end_line) then
                            ! Fully selected line (between start and end)
                            in_selection = .true.
                            exit
                        else if (line_num == sel_start_line .and. line_num == sel_end_line) then
                            ! Single-line selection
                            if (char_idx >= sel_start_col .and. char_idx < sel_end_col) then
                                in_selection = .true.
                                exit
                            end if
                        else if (line_num == sel_start_line .and. line_num < sel_end_line) then
                            ! First line of multi-line selection
                            if (char_idx >= sel_start_col) then
                                in_selection = .true.
                                exit
                            end if
                        else if (line_num == sel_end_line .and. line_num > sel_start_line) then
                            ! Last line of multi-line selection
                            if (char_idx < sel_end_col) then
                                in_selection = .true.
                                exit
                            end if
                        end if
                    end if
                end do
            end if

            ! Check if this position is a bracket or its match (only for active pane)
            is_bracket_match = .false.
            if (pane%is_active) then
                if ((line_num == bracket_line .and. char_idx == bracket_col) .or. &
                    (line_num == matching_bracket_line .and. char_idx == matching_bracket_col)) then
                    is_bracket_match = .true.
                end if
            end if

            ! Get byte position for syntax token lookup
            byte_pos = utf8_char_to_byte_index(line, char_idx)

            ! Find which token this column belongs to (tokens use byte indices)
            token_color = ""
            if (syntax_highlighter%enabled .and. byte_pos > 0) then
                do token_idx = 1, size(tokens)
                    if (byte_pos >= tokens(token_idx)%start_col .and. &
                        byte_pos <= tokens(token_idx)%end_col) then
                        token_color = get_token_color(tokens(token_idx)%type)
                        exit
                    end if
                end do
            end if

            ! Determine this character's style (priority order preserved)
            if (in_selection) then
                ! Selected text: reverse video
                style = char(27) // '[7m'
            else if (is_bracket_match) then
                ! Matching brackets: cyan background
                style = char(27) // '[46m'
            else if (pane%is_active .and. is_current_line) then
                ! Current line background (+ syntax color)
                style = token_color // char(27) // '[48;5;237m'
            else if (.not. pane%is_active) then
                ! Inactive pane background (+ syntax color)
                style = token_color // char(27) // '[48;5;234m'
            else
                ! Syntax color only (empty for plain text)
                style = token_color
            end if

            if (style /= last_style) then
                call terminal_write(char(27) // '[0m')
                if (len(style) > 0) call terminal_write(style)
                last_style = style
            end if
            call terminal_write(utf8_ch)

            display_col = display_col + char_width
            char_idx = char_idx + 1
        end do

        ! Fill remaining width with spaces
        do while (display_col < content_width)
            if (.not. pane%is_active) then
                style = char(27) // '[48;5;234m'
            else if (is_current_line) then
                style = char(27) // '[48;5;237m'
            else
                style = ''
            end if

            if (style /= last_style) then
                call terminal_write(char(27) // '[0m')
                if (len(style) > 0) call terminal_write(style)
                last_style = style
            end if
            call terminal_write(' ')

            display_col = display_col + 1
        end do

        ! Reset attributes
        call terminal_write(char(27) // '[0m')

        if (allocated(utf8_ch)) deallocate(utf8_ch)
    end subroutine render_buffer_line_in_pane

    subroutine render_pane_separator(col, start_row, height)
        integer, intent(in) :: col, start_row, height
        integer :: row

        ! Draw vertical separator with distinct visual
        do row = start_row, start_row + height - 1
            call terminal_move_cursor(row, col)
            ! Use reverse video for a solid separator
            call terminal_write(char(27) // '[7m ')  ! Reverse video space
            call terminal_write(char(27) // '[0m')   ! Reset
        end do
    end subroutine render_pane_separator

    subroutine render_cursor_for_panes(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(in) :: editor
        type(pane_t) :: pane
        type(cursor_t) :: cursor
        integer :: tab_idx, pane_idx, i
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_row, screen_col
        integer :: screen_width, screen_height
        integer :: col_offset
        character(len=:), allocatable :: line
        character(len=:), allocatable :: cursor_char  ! Full UTF-8 char, not one byte

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(pane_idx)
        if (.not. allocated(pane%cursors)) return
        if (pane%active_cursor < 1 .or. pane%active_cursor > size(pane%cursors)) return

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Calculate pane screen coordinates
        screen_width = editor%screen_cols
        screen_height = editor%screen_rows - 2  ! Account for tab bar (row 1) and status bar (last row)

        ! Reduce width if diagnostics panel is visible
        if (editor%diagnostics_panel%visible) then
            screen_width = screen_width - editor%diagnostics_panel%width
        end if

        ! Reduce width if references panel is visible
        if (editor%references_panel%visible) then
            screen_width = screen_width - editor%references_panel%width
        end if

        pane_col = 1 + int(pane%x_start * real(screen_width))
        pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
        if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
            pane_width = pane_width - 1  ! Reserve space for separator
        end if
        pane_row = 2 + int(pane%y_start * real(screen_height))
        pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

        ! Account for pane header when multiple panes exist
        if (size(editor%tabs(tab_idx)%panes) > 1) then
            pane_row = pane_row + 1  ! Content starts after header
            pane_height = pane_height - 1  ! Height reduced by header
        end if

        ! For multiple cursors, render all inactive ones first
        if (size(pane%cursors) > 1) then
            do i = 1, size(pane%cursors)
                if (i /= pane%active_cursor) then
                    cursor = pane%cursors(i)

                    ! Skip the block caret when this cursor has a selection:
                    ! the selection highlight already marks it, and the caret
                    ! sits one past the selection end (e.g. on the '.' after
                    ! a ctrl-d word select), reading as a bogus extra highlight
                    if (cursor%has_selection) cycle

                    ! Calculate cursor position within the pane (display
                    ! cells, so tabs and wide chars line up with the text)
                    line = buffer_get_line(pane%buffer, cursor%line)
                    screen_row = pane_row + (cursor%line - pane%viewport_line)
                    screen_col = pane_col + col_offset + &
                        display_offset_of(line, pane%viewport_column, cursor%column)

                    ! Ensure cursor is within pane boundaries
                    if (screen_row >= pane_row .and. screen_row < pane_row + pane_height .and. &
                        screen_col >= pane_col + col_offset .and. screen_col < pane_col + pane_width) then
                        ! Get the character at this cursor position
                        ! (cursor%column is a character index, not a byte index)
                        if (cursor%column <= utf8_char_count(line)) then
                            cursor_char = utf8_char_at(line, cursor%column)
                        else
                            cursor_char = ' '  ! End of line
                        end if

                        ! Inactive cursor - draw character with reverse video
                        call terminal_move_cursor(screen_row, screen_col)
                        call terminal_write(char(27) // '[7m' // cursor_char)  ! Inverse video
                        call terminal_write(char(27) // '[0m')   ! Reset
                    end if
                end if
            end do
        end if

        ! Now render the active cursor
        cursor = pane%cursors(pane%active_cursor)

        ! Calculate cursor position within the pane, accounting for line
        ! numbers, in display cells (tabs / wide chars)
        line = buffer_get_line(pane%buffer, cursor%line)
        screen_row = pane_row + (cursor%line - pane%viewport_line)
        screen_col = pane_col + col_offset + &
            display_offset_of(line, pane%viewport_column, cursor%column)

        ! Ensure cursor is within pane boundaries
        if (screen_row >= pane_row .and. screen_row < pane_row + pane_height .and. &
            screen_col >= pane_col + col_offset .and. screen_col < pane_col + pane_width) then
            call terminal_move_cursor(screen_row, screen_col)
        else
            ! Cursor is out of view, position at top-left of pane content area
            call terminal_move_cursor(pane_row, pane_col + col_offset)
        end if

        ! Show the caret unless a selection is active (hollow box then)
        call show_caret_unless_selecting(editor)
    end subroutine render_cursor_for_panes

    ! Draw the inline shadow-text suggestion (dim gray) at the active cursor.
    ! Mid-line the rest of the real line is redrawn after the suggestion, so
    ! the line visually opens up for the ghost and closes again when it goes
    ! (every keystroke is a full redraw). Drawn just before cursor placement
    ! each frame. Screen-position math mirrors render_cursor_for_panes /
    ! render_cursor so the ghost stays aligned with the caret.
    subroutine render_ghost_text(editor, buffer)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        type(pane_t) :: pane
        type(cursor_t) :: cursor
        character(len=:), allocatable :: suffix, shown, line
        integer :: tab_idx, pane_idx, col_offset
        integer :: screen_row, screen_col, avail, disp
        integer :: screen_width, screen_height
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: row_offset, min_row, ghost_cells
        logical :: use_panes

        if (.not. ghost_is_active(editor%ghost)) return
        if (editor%completion_popup%visible) return

        suffix = ghost_suffix(editor%ghost)
        if (len(suffix) == 0) return
        ! The suggestion is written straight to the terminal, so a control
        ! byte in it would be interpreted as an escape sequence and corrupt
        ! the screen. Today's sources are filtered to identifiers, but this
        ! keeps the renderer safe whatever produces the text.
        if (.not. is_terminal_safe(suffix)) return

        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1
        else
            col_offset = 0
        end if

        ! Width available to editor content (matches cursor rendering)
        screen_width = editor%screen_cols
        if (editor%diagnostics_panel%visible) then
            screen_width = screen_width - editor%diagnostics_panel%width
        end if
        if (editor%references_panel%visible) then
            screen_width = screen_width - editor%references_panel%width
        end if

        use_panes = .false.
        tab_idx = editor%active_tab_index
        if (size(editor%tabs) > 0 .and. tab_idx >= 1 .and. tab_idx <= size(editor%tabs)) then
            use_panes = allocated(editor%tabs(tab_idx)%panes)
        end if

        if (use_panes) then
            pane_idx = editor%tabs(tab_idx)%active_pane_index
            if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return
            pane = editor%tabs(tab_idx)%panes(pane_idx)
            if (.not. allocated(pane%cursors)) return
            if (size(pane%cursors) /= 1) return
            if (pane%active_cursor < 1 .or. pane%active_cursor > size(pane%cursors)) return
            cursor = pane%cursors(pane%active_cursor)

            ! Suggestion must still be anchored at the cursor
            if (cursor%line /= editor%ghost%anchor_line .or. &
                cursor%column /= editor%ghost%anchor_col) return
            line = buffer_get_line(pane%buffer, cursor%line)

            ! Pane geometry (same formulas as render_cursor_for_panes)
            screen_height = editor%screen_rows - 2
            pane_col = 1 + int(pane%x_start * real(screen_width))
            pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
            if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
                pane_width = pane_width - 1  ! Reserve space for separator
            end if
            pane_row = 2 + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))
            if (size(editor%tabs(tab_idx)%panes) > 1) then
                pane_row = pane_row + 1
                pane_height = pane_height - 1
            end if

            screen_row = pane_row + (cursor%line - pane%viewport_line)
            disp = display_offset_of(line, pane%viewport_column, cursor%column)
            screen_col = pane_col + col_offset + disp
            if (screen_row < pane_row .or. screen_row >= pane_row + pane_height) return
            if (screen_col < pane_col + col_offset .or. screen_col >= pane_col + pane_width) return
            avail = pane_col + pane_width - screen_col
        else
            if (size(editor%cursors) /= 1) return
            cursor = editor%cursors(editor%active_cursor)
            if (cursor%line /= editor%ghost%anchor_line .or. &
                cursor%column /= editor%ghost%anchor_col) return
            line = buffer_get_line(buffer, cursor%line)

            if (size(editor%tabs) > 0) then
                row_offset = 2
                min_row = 2
            else
                row_offset = 1
                min_row = 1
            end if
            screen_row = cursor%line - editor%viewport_line + row_offset
            disp = display_offset_of(line, editor%viewport_column, cursor%column)
            screen_col = col_offset + 1 + disp
            if (screen_row < min_row .or. screen_row >= editor%screen_rows) return
            if (screen_col < 1 .or. screen_col > screen_width) return
            avail = screen_width - screen_col + 1
        end if

        if (avail < 1) return
        ! avail is a count of screen CELLS, so the suggestion has to be
        ! measured and cut the same way. Clipping by byte count would both
        ! overshoot the pane on multibyte text and slice a character in half,
        ! emitting a partial UTF-8 sequence to the terminal.
        call clip_to_cells(suffix, avail, shown, ghost_cells)
        if (len(shown) == 0) return

        call terminal_move_cursor(screen_row, screen_col)
        call terminal_write(char(27) // '[2m' // char(27) // '[90m' // &
                            shown // char(27) // '[0m')

        ! Mid-line: redraw the real text right of the cursor, shifted past
        ! the suggestion, so nothing is hidden while the ghost is up
        if (cursor%column <= utf8_char_count(line)) then
            call render_line_tail_shifted(line, cursor%column, &
                disp + ghost_cells, avail - ghost_cells)
        end if
    end subroutine render_ghost_text

    ! Cut text to at most `cells` display columns, only ever on a character
    ! boundary, and report the width actually used. Wide characters that would
    ! straddle the limit are dropped rather than half-drawn.
    subroutine clip_to_cells(text, cells, clipped, used)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cells
        character(len=:), allocatable, intent(out) :: clipped
        integer, intent(out) :: used
        character(len=:), allocatable :: ch
        integer :: ci, nchars, w, last_byte, start_byte

        used = 0
        last_byte = 0
        nchars = utf8_char_count(text)

        do ci = 1, nchars
            ch = utf8_char_at(text, ci)
            if (len(ch) == 0) exit
            w = utf8_display_width(ch)
            if (used + w > cells) exit
            used = used + w
            start_byte = utf8_char_to_byte_index(text, ci)
            if (start_byte <= 0) exit
            last_byte = start_byte + len(ch) - 1
        end do

        if (last_byte <= 0) then
            clipped = ''
        else
            clipped = text(1:last_byte)
        end if
    end subroutine clip_to_cells

    ! True when every byte can be written to the terminal without being taken
    ! as a control code. Rejects C0 (except tab), DEL, and the C1 range's lead
    ! byte pattern is left to the caller's UTF-8 validation.
    pure function is_terminal_safe(text) result(ok)
        character(len=*), intent(in) :: text
        logical :: ok
        integer :: i, b

        ok = .false.
        do i = 1, len(text)
            b = iachar(text(i:i))
            if (b < 32 .and. b /= 9) return
            if (b == 127) return
        end do
        ok = .true.
    end function is_terminal_safe

    ! Draw buffer_line from start_char onward at the current terminal cursor,
    ! stopping when budget screen cells are used. start_off is the display
    ! offset of the first cell from the line's on-screen start (keeps tab
    ! stops aligned). Colors come from a fresh tokenization of the real line;
    ! the highlighter's multiline scan state is saved and restored so the
    ! frame-sequential state machine is untouched.
    subroutine render_line_tail_shifted(buffer_line, start_char, start_off, budget)
        character(len=*), intent(in) :: buffer_line
        integer, intent(in) :: start_char, start_off, budget
        type(token_t), allocatable :: tokens(:)
        logical :: saved_mc, saved_ms
        character(len=4) :: saved_delim
        character(len=:), allocatable :: ch, style, prev_style
        integer :: ci, off, w, byte_pos, ti

        if (budget < 1) return

        saved_mc = syntax_highlighter%in_multiline_comment
        saved_ms = syntax_highlighter%in_multiline_string
        saved_delim = syntax_highlighter%string_delimiter
        syntax_highlighter%in_multiline_comment = .false.
        syntax_highlighter%in_multiline_string = .false.
        call tokenize_line(syntax_highlighter, buffer_line, tokens)
        syntax_highlighter%in_multiline_comment = saved_mc
        syntax_highlighter%in_multiline_string = saved_ms
        syntax_highlighter%string_delimiter = saved_delim

        prev_style = ''
        off = 0
        ci = start_char
        byte_pos = utf8_char_to_byte_index(buffer_line, start_char)
        do
            ch = utf8_char_at(buffer_line, ci)
            if (len(ch) == 0) exit
            if (ch == char(9)) then
                w = TAB_WIDTH - mod(start_off + off, TAB_WIDTH)
            else
                w = utf8_display_width(ch)
            end if
            if (off + w > budget) exit

            style = ''
            if (syntax_highlighter%enabled) then
                do ti = 1, size(tokens)
                    if (byte_pos >= tokens(ti)%start_col .and. &
                        byte_pos <= tokens(ti)%end_col) then
                        style = get_token_color(tokens(ti)%type)
                        exit
                    end if
                end do
            end if
            if (style /= prev_style) then
                call terminal_write(char(27) // '[0m')
                if (len(style) > 0) call terminal_write(style)
                prev_style = style
            end if

            if (ch == char(9)) then
                call terminal_write(repeat(' ', w))
            else
                call terminal_write(ch)
            end if
            off = off + w
            byte_pos = byte_pos + len(ch)
            ci = ci + 1
        end do
        call terminal_write(char(27) // '[0m')
    end subroutine render_line_tail_shifted

    subroutine render_cursor_for_panes_with_tree(editor, tree_offset, editor_width)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: tree_offset, editor_width
        type(pane_t) :: pane
        type(cursor_t) :: cursor
        integer :: tab_idx, pane_idx
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_row, screen_col, col_offset
        integer :: screen_height

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(pane_idx)
        if (.not. allocated(pane%cursors)) return
        if (pane%active_cursor < 1 .or. pane%active_cursor > size(pane%cursors)) return

        cursor = pane%cursors(pane%active_cursor)

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1
        else
            col_offset = 0
        end if

        ! Calculate pane coordinates (adjusted for tree)
        screen_height = editor%screen_rows - 2
        pane_col = tree_offset + int(pane%x_start * real(editor_width))
        pane_width = int((pane%x_end - pane%x_start) * real(editor_width))
        if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
            pane_width = pane_width - 1
        end if
        pane_row = 2 + int(pane%y_start * real(screen_height))
        pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

        ! Account for pane header
        if (size(editor%tabs(tab_idx)%panes) > 1) then
            pane_row = pane_row + 1
            pane_height = pane_height - 1
        end if

        ! Calculate cursor screen position (display cells)
        screen_row = pane_row + (cursor%line - pane%viewport_line)
        screen_col = pane_col + col_offset + display_offset_of( &
            buffer_get_line(pane%buffer, cursor%line), &
            pane%viewport_column, cursor%column)

        ! Ensure cursor is within pane boundaries
        if (screen_row >= pane_row .and. screen_row < pane_row + pane_height .and. &
            screen_col >= pane_col + col_offset .and. screen_col < pane_col + pane_width) then
            call terminal_move_cursor(screen_row, screen_col)
        else
            ! Cursor out of view, position at top-left of pane
            call terminal_move_cursor(pane_row, pane_col + col_offset)
        end if

        call show_caret_unless_selecting(editor)
    end subroutine render_cursor_for_panes_with_tree

    subroutine render_cursor_in_pane(editor, buffer, pane_start_col, pane_width)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: pane_start_col, pane_width
        type(cursor_t) :: cursor
        integer :: screen_row, screen_col, col_offset, row_offset, min_row

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1
        else
            col_offset = 0
        end if

        ! Account for tab bar offset - when tabs exist, row 1 is tab bar, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2  ! Tab bar takes row 1
            min_row = 2     ! Cursor cannot be in row 1 (tab bar)
        else
            row_offset = 1  ! No tab bar
            min_row = 1     ! Cursor can be in row 1
        end if

        cursor = editor%cursors(editor%active_cursor)

        ! Calculate screen position within the editor pane (display cells)
        screen_row = cursor%line - editor%viewport_line + row_offset
        screen_col = pane_start_col + col_offset + display_offset_of( &
            buffer_get_line(buffer, cursor%line), &
            editor%viewport_column, cursor%column)

        ! Ensure cursor is within pane bounds and not in tab bar
        if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
            screen_col >= pane_start_col .and. screen_col <= pane_start_col + pane_width) then
            call terminal_move_cursor(screen_row, screen_col)
            call show_caret_unless_selecting(editor)
        end if
    end subroutine render_cursor_in_pane

    ! Render tab bar at top of screen
    ! Optional start_col and width parameters for positioning in split view
    subroutine render_tab_bar(editor, start_col, width)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in), optional :: start_col, width
        integer :: i, col, tab_count
        character(len=:), allocatable :: tab_label, filename_only
        character(len=256) :: temp_label
        integer :: slash_pos, last_slash
        character(len=1) :: modified_marker
        integer :: start_column, max_width

        tab_count = size(editor%tabs)
        if (tab_count == 0) return  ! No tabs to display

        ! Use provided start_col and width, or default to full screen
        if (present(start_col)) then
            start_column = start_col
        else
            start_column = 1
        end if

        if (present(width)) then
            max_width = width
        else
            max_width = editor%screen_cols
        end if

        ! Move to top row at starting column and clear the tab bar area
        call terminal_move_cursor(1, start_column)
        call terminal_write(repeat(' ', max_width))

        ! Render each tab
        col = start_column
        do i = 1, tab_count
            ! Extract filename from full path
            filename_only = editor%tabs(i)%filename
            last_slash = 0
            do slash_pos = len(editor%tabs(i)%filename), 1, -1
                if (editor%tabs(i)%filename(slash_pos:slash_pos) == '/') then
                    last_slash = slash_pos
                    exit
                end if
            end do
            if (last_slash > 0 .and. last_slash < len(editor%tabs(i)%filename)) then
                filename_only = editor%tabs(i)%filename(last_slash+1:)
            end if

            ! Add modified marker
            if (editor%tabs(i)%modified) then
                modified_marker = '*'
            else
                modified_marker = ' '
            end if

            ! Build tab label: [1: file.txt*]
            write(temp_label, '(A,I0,A,A,A,A)') '[', i, ': ', trim(filename_only), modified_marker, ']'
            tab_label = trim(temp_label)

            ! Check if we have room for this tab
            if (col + len(tab_label) > start_column + max_width) exit

            ! Position cursor
            call terminal_move_cursor(1, col)

            ! Apply orphan tab styling (gray foreground)
            if (editor%tabs(i)%is_orphan) then
                call terminal_write(char(27) // '[90m')  ! Bright black/gray
            end if

            ! Highlight active tab
            if (i == editor%active_tab_index) then
                call terminal_write(char(27) // '[7m')  ! Reverse video
            end if

            call terminal_write(tab_label)

            ! Reset if we applied any styling
            if (i == editor%active_tab_index .or. editor%tabs(i)%is_orphan) then
                call terminal_write(char(27) // '[0m')  ! Reset
            end if

            col = col + len(tab_label) + 1  ! +1 for space between tabs
        end do
    end subroutine render_tab_bar

    ! UNUSED: Get diagnostic marker and color for a line
    ! Kept for potential future use
    ! subroutine get_diagnostic_marker(diagnostics, marker, color)
    !     type(diagnostic_t), intent(in) :: diagnostics(:)
    !     character(len=3), intent(out) :: marker  ! UTF-8 characters can be up to 3 bytes
    !     character(len=:), allocatable, intent(out) :: color
    !     integer :: i, max_severity
    !
    !     marker = ' '
    !     color = ''
    !
    !     if (size(diagnostics) == 0) return
    !
    !     ! Find highest severity diagnostic
    !     max_severity = SEVERITY_HINT
    !     do i = 1, size(diagnostics)
    !         if (diagnostics(i)%severity < max_severity) then
    !             max_severity = diagnostics(i)%severity
    !         end if
    !     end do
    !
    !     ! Set marker and color based on severity
    !     select case(max_severity)
    !     case(SEVERITY_ERROR)
    !         marker = '●'  ! Filled circle for errors
    !         color = char(27) // '[31m'  ! Red
    !     case(SEVERITY_WARNING)
    !         marker = '▲'  ! Triangle for warnings
    !         color = char(27) // '[33m'  ! Yellow
    !     case(SEVERITY_INFO)
    !         marker = '◆'  ! Diamond for info
    !         color = char(27) // '[36m'  ! Cyan
    !     case(SEVERITY_HINT)
    !         marker = '○'  ! Empty circle for hints
    !         color = char(27) // '[90m'  ! Gray
    !     end select
    ! end subroutine get_diagnostic_marker

    ! Render screen with LSP panel on the right (similar to render_screen_with_tree but for right side)
    subroutine render_screen_with_lsp_panel(buffer, editor, panel_type, match_mode_active, match_case_sens)
        use references_panel_module, only: references_panel_t, is_references_panel_visible
        use symbols_panel_module, only: symbols_panel_t, is_symbols_panel_visible
        use workspace_symbols_panel_module, only: workspace_symbols_panel_t, is_workspace_symbols_panel_visible
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: panel_type  ! "references", "symbols", or "workspace_symbols"
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        integer :: panel_width, editor_end_col, editor_width
        integer :: separator_col, panel_start_col
        integer :: row

        call terminal_hide_cursor()

        ! Clear screen first to avoid artifacts
        do row = 1, editor%screen_rows
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', editor%screen_cols))
        end do

        ! Calculate split: 60% for editor, 40% for LSP panel
        panel_width = editor%screen_cols * 40 / 100
        editor_width = editor%screen_cols - panel_width - 1  ! -1 for separator
        editor_end_col = editor_width
        separator_col = editor_width + 1
        panel_start_col = separator_col + 1

        ! Render tab bar if there are any tabs (positioned in editor pane area)
        call render_tab_bar(editor, 1, editor_width)

        ! Render editor in left pane (check for multiple panes)
        call render_editor_area_for_lsp_panel(editor, 1, editor_width)

        ! Render status bar (full width)
        call render_status_bar(editor, buffer, match_mode_active, match_case_sens)

        ! Render vertical separator (start at row 2 for tab bar)
        call render_vertical_separator(separator_col, 2, editor%screen_rows - 1)

        ! Render appropriate LSP panel on the right
        select case (panel_type)
        case ("references")
            if (is_references_panel_visible(editor%references_panel)) then
                call render_lsp_references_panel(editor%references_panel, panel_start_col, panel_width, &
                                                 2, editor%screen_rows - 1)
            end if
        case ("symbols")
            if (is_symbols_panel_visible(editor%symbols_panel)) then
                call render_lsp_symbols_panel(editor%symbols_panel, editor%screen_rows - 1)
            end if
        case ("workspace_symbols")
            if (is_workspace_symbols_panel_visible(editor%workspace_symbols_panel)) then
                call render_lsp_workspace_symbols_panel(editor%workspace_symbols_panel, editor%screen_rows - 1)
            end if
        end select

        ! Render cursor
        call render_cursor_for_lsp_panel(editor, buffer, 1, editor_width)

        call terminal_show_cursor()
    end subroutine render_screen_with_lsp_panel

    ! Helper to render editor area when LSP panel is on right
    subroutine render_editor_area_for_lsp_panel(editor, start_col, width)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: start_col, width
        type(pane_t) :: pane
        integer :: i, tab_idx, n_panes
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_height

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) then
            return
        end if

        if (.not. allocated(editor%tabs(tab_idx)%panes)) then
            return
        end if

        n_panes = size(editor%tabs(tab_idx)%panes)
        if (n_panes == 0) return

        screen_height = editor%screen_rows - 2  ! Account for tab bar and status bar

        ! If only one pane, use simple rendering
        if (n_panes == 1) then
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, start_col, width)
            return
        end if

        ! Multiple panes: render each with adjusted coordinates
        do i = 2, editor%screen_rows - 1
            call terminal_move_cursor(i, start_col)
            call terminal_write(repeat(' ', width))
        end do

        do i = 1, n_panes
            pane = editor%tabs(tab_idx)%panes(i)

            pane_col = start_col + int(pane%x_start * real(width))
            if (i < n_panes) then
                pane_width = int((pane%x_end - pane%x_start) * real(width)) - 1
            else
                pane_width = int((pane%x_end - pane%x_start) * real(width))
            end if
            pane_row = 2 + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            editor%tabs(tab_idx)%panes(i)%screen_col = pane_col
            editor%tabs(tab_idx)%panes(i)%screen_row = pane_row
            editor%tabs(tab_idx)%panes(i)%screen_width = pane_width
            editor%tabs(tab_idx)%panes(i)%screen_height = pane_height

            call render_single_pane(editor, i, pane_col, pane_row, pane_width, pane_height)

            if (i < n_panes) then
                call render_pane_separator(pane_col + pane_width, pane_row, pane_height)
            end if
        end do
    end subroutine render_editor_area_for_lsp_panel

    ! Helper to render cursor when LSP panel is visible
    subroutine render_cursor_for_lsp_panel(editor, buffer, start_col, width)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: start_col, width
        integer :: tab_idx, n_panes

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        n_panes = size(editor%tabs(tab_idx)%panes)
        if (n_panes == 0) return

        if (n_panes > 1) then
            call render_cursor_for_panes_in_lsp_view(editor)
        else
            call render_cursor_in_pane(editor, buffer, start_col, width)
        end if
    end subroutine render_cursor_for_lsp_panel

    ! Helper to render cursor for multiple panes when LSP panel is visible
    subroutine render_cursor_for_panes_in_lsp_view(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, active_pane
        type(pane_t) :: pane
        integer :: screen_row, screen_col

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return

        active_pane = editor%tabs(tab_idx)%active_pane_index
        if (active_pane < 1 .or. active_pane > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(active_pane)

        ! Calculate cursor position relative to pane
        screen_row = pane%screen_row + (editor%cursors(editor%active_cursor)%line - pane%viewport_line)
        screen_col = pane%screen_col + (editor%cursors(editor%active_cursor)%column - 1)

        if (screen_row >= pane%screen_row .and. &
            screen_row < pane%screen_row + pane%screen_height .and. &
            screen_col >= pane%screen_col .and. &
            screen_col < pane%screen_col + pane%screen_width) then
            call terminal_move_cursor(screen_row, screen_col)
        end if
    end subroutine render_cursor_for_panes_in_lsp_view

    ! Render references panel in offcanvas mode (right side, full height)
    subroutine render_lsp_references_panel(panel, start_col, width, start_row, end_row)
        use references_panel_module, only: references_panel_t
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: start_col, width, start_row, end_row
        integer :: row, i, visible_index, max_visible
        character(len=256) :: line
        character(len=100) :: header, location_str
        character(len=:), allocatable :: filename_display
        character(len=1), parameter :: ESC = achar(27)

        ! Clear panel area
        do row = start_row, end_row
            call terminal_move_cursor(row, start_col)
            call terminal_write(repeat(' ', width))
        end do

        row = start_row

        ! Header with symbol name
        call terminal_move_cursor(row, start_col)
        call terminal_write(ESC // '[48;5;237m')  ! Dark background

        if (allocated(panel%symbol_name)) then
            write(header, '(A,A,A,I0,A)') " References: ", trim(panel%symbol_name), &
                " (", panel%num_references, ") "
        else
            write(header, '(A,I0,A)') " References (", panel%num_references, ") "
        end if

        ! Truncate header if too long
        if (len_trim(header) > width) then
            header = header(1:width-3) // "..."
        end if

        call terminal_write(ESC // '[1m' // trim(header))
        ! Pad rest of header line
        if (len_trim(header) < width) then
            call terminal_write(repeat(' ', width - len_trim(header)))
        end if
        call terminal_write(ESC // '[0m')
        row = row + 1

        ! Separator
        call terminal_move_cursor(row, start_col)
        call terminal_write(ESC // '[48;5;237m' // repeat("─", width) // ESC // '[0m')
        row = row + 1

        ! Legend
        call terminal_move_cursor(row, start_col)
        call terminal_write(ESC // '[90m')
        if (width >= 31) then
            call terminal_write('j/k:nav  enter:jump  esc:close')
        else
            call terminal_write('j/k enter esc')
        end if
        call terminal_write(ESC // '[0m')
        row = row + 1

        ! Separator
        call terminal_move_cursor(row, start_col)
        call terminal_write(ESC // '[90m' // repeat("─", width) // ESC // '[0m')
        row = row + 1

        ! Calculate max visible items
        max_visible = end_row - row + 1

        ! Display references
        if (panel%num_references == 0) then
            call terminal_move_cursor(row, start_col)
            call terminal_write(ESC // '[48;5;235m' // ESC // '[90m')
            call terminal_write(" No references found")
            if (20 < width) then
                call terminal_write(repeat(' ', width - 20))
            end if
            call terminal_write(ESC // '[0m')
        else
            do i = 1, min(max_visible, panel%num_references - panel%scroll_offset)
                visible_index = panel%scroll_offset + i
                if (visible_index > panel%num_references) exit

                call terminal_move_cursor(row, start_col)

                ! Highlight selected item
                if (visible_index == panel%selected_index) then
                    call terminal_write(ESC // '[48;5;240m')  ! Highlight background
                else
                    call terminal_write(ESC // '[48;5;235m')  ! Normal background
                end if

                ! Format location string
                if (allocated(panel%references(visible_index)%filename)) then
                    ! Extract just the filename from full path
                    filename_display = get_basename_str(panel%references(visible_index)%filename)
                    write(location_str, '(A,A,I0,A,I0)') &
                        trim(filename_display), &
                        ":", panel%references(visible_index)%line, &
                        ":", panel%references(visible_index)%column
                else
                    write(location_str, '(I0,A,I0)') &
                        panel%references(visible_index)%line, &
                        ":", panel%references(visible_index)%column
                end if

                ! Build line with location
                line = " " // trim(adjustl(location_str))

                ! Truncate to fit panel width
                if (len_trim(line) > width) then
                    line = line(1:width - 3) // '...'
                end if

                ! Write line and pad to width
                call terminal_write(line(1:min(len_trim(line), width)))
                if (len_trim(line) < width) then
                    call terminal_write(repeat(' ', width - len_trim(line)))
                end if
                call terminal_write(ESC // '[0m')

                row = row + 1
                if (row > end_row) exit
            end do
        end if
    end subroutine render_lsp_references_panel

    ! Render symbols panel in offcanvas mode (right side, full height)
    subroutine render_lsp_symbols_panel(panel, screen_height)
        use symbols_panel_module, only: symbols_panel_t, render_symbols_panel
        type(symbols_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_height

        ! Delegate to the real symbols panel renderer
        ! The panel manages its own positioning via panel_start_col and panel_width
        call render_symbols_panel(panel, screen_height)
    end subroutine render_lsp_symbols_panel

    ! Render workspace symbols panel in offcanvas mode (right side, full height)
    subroutine render_lsp_workspace_symbols_panel(panel, screen_height)
        use workspace_symbols_panel_module, only: workspace_symbols_panel_t, render_workspace_symbols_panel
        type(workspace_symbols_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_height

        ! Delegate to the real workspace symbols panel renderer
        ! The panel manages its own positioning via panel_start_col and panel_width
        call render_workspace_symbols_panel(panel, screen_height)
    end subroutine render_lsp_workspace_symbols_panel

    ! Helper function to extract basename from path
    function get_basename_str(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: basename
        integer :: i, last_slash

        last_slash = 0
        do i = len(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_slash = i
                exit
            end if
        end do

        if (last_slash > 0 .and. last_slash < len(path)) then
            basename = path(last_slash+1:len(path))
        else
            basename = path
        end if
    end function get_basename_str

    ! Get current time in milliseconds (for fuzzy search timeout)
    function get_time_ms() result(ms)
        integer(int64) :: ms
        integer(int64) :: count, count_rate

        call system_clock(count, count_rate)
        if (count_rate > 0) then
            ms = (count * 1000_int64) / count_rate
        else
            ms = 0
        end if
    end function get_time_ms

    ! Reset fuzzy search buffer
    subroutine fuss_reset_search()
        fuss_search_buffer = ''
        fuss_search_len = 0
        fuss_search_last_time = 0
    end subroutine fuss_reset_search

    ! Fuzzy jump to matching entry in fuss mode
    ! Returns true if a match was found and jumped to
    function fuss_fuzzy_jump(search_str) result(found)
        character(len=*), intent(in) :: search_str
        logical :: found
        integer :: i, start_idx, search_len
        character(len=256) :: item_name, search_lower, name_lower

        found = .false.
        search_len = len_trim(search_str)
        if (search_len == 0) return
        if (tree_state%n_selectable == 0) return

        search_lower = to_lower(trim(search_str))

        ! If the current selection still matches the (now longer)
        ! search string, stay put — don't bounce between items
        ! that share a prefix.
        start_idx = tree_state%selected_index
        if (start_idx >= 1 .and. &
            start_idx <= tree_state%n_selectable) then
            if (associated( &
                tree_state%selectable_files(start_idx)%node)) then
                item_name = trim( &
                    tree_state%selectable_files(start_idx)%node%name)
            else
                item_name = trim( &
                    tree_state%selectable_files(start_idx)%path)
            end if
            name_lower = to_lower(trim(item_name))
            if (len_trim(name_lower) >= search_len) then
                if (name_lower(1:search_len) == &
                    search_lower(1:search_len)) then
                    found = .true.
                    return
                end if
            end if
        end if

        ! Current item doesn't match — scan forward, wrapping
        do i = 1, tree_state%n_selectable
            start_idx = start_idx + 1
            if (start_idx > tree_state%n_selectable) start_idx = 1

            if (associated( &
                tree_state%selectable_files(start_idx)%node)) then
                item_name = trim( &
                    tree_state%selectable_files(start_idx)%node%name)
            else
                item_name = trim( &
                    tree_state%selectable_files(start_idx)%path)
            end if

            name_lower = to_lower(trim(item_name))

            if (len_trim(name_lower) >= search_len) then
                if (name_lower(1:search_len) == &
                    search_lower(1:search_len)) then
                    tree_state%selected_index = start_idx
                    found = .true.
                    return
                end if
            end if
        end do
    end function fuss_fuzzy_jump

    ! Convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=256) :: lower_str
        integer :: i, ic

        lower_str = str
        do i = 1, len_trim(str)
            ic = ichar(str(i:i))
            if (ic >= ichar('A') .and. ic <= ichar('Z')) then
                lower_str(i:i) = char(ic + 32)
            end if
        end do
    end function to_lower

end module renderer_module