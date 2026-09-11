module renderer_module
    use iso_fortran_env, only: int32, int64, output_unit
    use terminal_io_module
    use text_buffer_module
    use utf8_module
    use editor_state_module, only: editor_state_t, cursor_t
    use editor_state_module, only: active_pane_of, active_group_id, group_label, &
                                   group_members, group_member_count, group_find
    use bracket_matching_module
    use tab_drag_module, only: drag_is_showing, drag_payload, drag_to_row, &
                              drag_to_slot, drag_has_target, drag_label, &
                              drag_pointer_row, drag_pointer_col, &
                              drag_split_side, drag_split_rect, SPLIT_NONE, &
                              drag_candidate_gid
    use clickable_region_module, only: regions_begin_frame, region_add, REGION_TAB, &
                                       region_at, clickable_region_t, &
                                       REGION_TAB_SCROLL, REGION_NEW_TAB, &
                                       REGION_FUSS_TOGGLE
    use theme_module, only: THEME_ACCENT, THEME_BORDER, THEME_BORDER_FOCUS, THEME_CURRENT_LINE, &
        THEME_EDITOR, THEME_EDITOR_BG, THEME_GHOST, THEME_HINT, &
        THEME_LINE_NUMBER, THEME_LINE_NUMBER_ACTIVE, THEME_MUTED, &
        THEME_PANEL, THEME_PANEL_FOOTER, THEME_PANEL_HEADER, THEME_PANEL_SELECTION, &
        THEME_SEARCH_MATCH, THEME_SEARCH_MATCH_ACTIVE, THEME_SELECTION, &
        THEME_SELECTION_INACTIVE, THEME_STATUS, &
        THEME_STATUS_ACCENT, THEME_TAB_ACTIVE, THEME_TAB_BAR, &
        THEME_TAB_DRAG, THEME_TAB_INACTIVE, THEME_TAB_MODIFIED, THEME_TAB_ORPHAN, &
        theme_background_sgr, theme_foreground_sgr, theme_glyph, theme_paint, &
        theme_reset, theme_sgr
    use context_menu_module, only: render_context_menu, is_context_menu_visible
    use group_picker_module, only: render_group_picker, is_group_picker_visible
    use fortress_navigator_module, only: render_fortress, is_fortress_visible
    use file_tree_module
    use file_tree_renderer_module
    use syntax_highlighter_module
    use diagnostics_module, only: diagnostic_t, get_diagnostics_for_line, &
                                   get_diagnostic_at_cursor, &
                                   SEVERITY_ERROR, SEVERITY_WARNING, SEVERITY_INFO, SEVERITY_HINT
    use diagnostics_panel_module, only: render_diagnostics_panel
    use references_panel_module, only: render_references_panel, render_references_panel_at
    use code_actions_panel_module, only: render_code_actions_panel
    use symbols_panel_module, only: render_symbols_panel
    use unified_search_module, only: get_matches_on_line, search_mode_active, &
                                     active_match_span, render_search_panel, &
                                     is_search_panel_visible
    use lsp_server_installer_panel_module, only: render_lsp_server_installer_panel, &
                                                  is_lsp_server_installer_panel_visible
    use terminal_panel_module, only: is_terminal_panel_visible, &
        terminal_panel_render, get_terminal_panel_height
    use completion_popup_module, only: render_completion_popup
    use ghost_text_module, only: ghost_is_active, ghost_suffix, ghost_is_block, &
                                ghost_block_line
    implicit none
    private

    public :: render_screen, update_viewport, init_renderer, cleanup_renderer
    public :: resize_renderer
    public :: render_status_bar, render_cursor
    public :: set_status_message, clear_status_message, has_status_message
    public :: format_status_line  ! exposed for unit tests
    public :: show_line_numbers, LINE_NUMBER_WIDTH
    ! clip_to_cells now lives in utf8_module; re-exported here so existing
    ! importers (and test_ghost_render_safety) keep resolving it
    public :: clip_to_cells, is_terminal_safe  ! exposed for unit tests
    public :: render_screen_with_tree, render_screen_with_lsp_panel

    ! Transient status-bar message (see set_status_message)
    character(len=:), allocatable :: g_status_message
    public :: tree_state
    public :: update_syntax_highlighter
    public :: render_caret_move  ! Fast path for a plain caret move
    public :: fuss_search_buffer, fuss_search_len, fuss_search_last_time
    public :: fuss_fuzzy_jump, fuss_reset_search, get_time_ms
    public :: fuss_git_prefix_active
    public :: display_offset_of, char_col_at_offset
    public :: text_area_height, tab_bar_height, first_content_row
    public :: strip_entry_t, strip_span_t, strip_layout, STRIP_MAX_ENTRIES
    public :: nudge_tab_scroll, tab_group_hover, tab_group_clear_hover
    public :: tab_group_set_hover
    public :: tabbar_slot_at, tabbar_strip_rows, tabbar_strip2_gid
    public :: tabbar_last_slot, tabbar_hit_at
    public :: tab_group_preview_visible, set_group_preview_enabled  ! rows the document gets; page size must match

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

    ! Whether the terminal is currently reporting bare pointer motion.
    ! Owned here because render_menu_overlay is the one place that sees the
    ! menu's visibility every frame.
    logical, save :: g_motion_tracking = .false.

    ! ---- multi-line lexical-state seeding -------------------------------
    !
    ! Whether a line sits inside a /* */ block depends on every line above it,
    ! and tokenize_line carries that as state from one call to the next. The
    ! renderers walk the VIEWPORT, not the file, so the state arriving at the
    ! top row is whatever the previous frame happened to leave -- correct only
    ! when the viewport moved down one line at a time. A page-down, a wheel
    ! tick or a jump to the end left it wrong, and the rest of a comment
    ! rendered as code. A full repaint did not help: it starts from the same
    ! wrong state.
    !
    ! So the state is established rather than inherited. The anchor caches the
    ! last position it was computed for, so scrolling forward costs one extra
    ! tokenize per new line instead of a rescan.
    integer :: g_hl_next_line = -1          ! line the state is currently correct for
    integer :: g_hl_anchor_line = 1         ! line at whose start the anchor state holds
    logical :: g_hl_anchor_mc = .false.
    logical :: g_hl_anchor_ms = .false.
    character(len=4) :: g_hl_anchor_delim = ''
    logical :: g_hl_anchor_interp = .false.
    integer :: g_hl_anchor_interp_depth = 0
    integer(int64) :: g_hl_key_rev = -1     ! doc revision the anchor was built from
    integer :: g_hl_key_tab = -1
    ! WHICH DOCUMENT the anchor was built from.
    !
    ! A split is several panes inside ONE tab, each able to show a different
    ! file, so the tab and its revision do not identify the text being
    ! scanned. Without this, scrolling a pane whose file is inside a block
    ! comment left in_multiline_comment set, and the next pane resumed from it
    ! -- an unrelated file rendered entirely as a comment.
    character(len=:), allocatable :: g_hl_key_file
    !> The document currently being drawn. Set before each pane's rows.
    character(len=:), allocatable :: g_hl_surface

    ! ---- tab bar strip ---------------------------------------------------
    !
    ! One entry laid out on one row of the bar. Row 1 holds tabs (and, later,
    ! tab groups); a second row will hold a group's members. Both go through
    ! the same layout so the two cannot disagree about where a click landed.
    integer, parameter :: STRIP_MAX_ENTRIES = 256
    integer, parameter :: MAX_ENTRY_CELLS = 24   ! per label, before ellipsis
    ! Kept across frames so a click on a chevron persists.
    ! The active entry each strip was last laid out with. The bar follows the
    ! active entry when it CHANGES -- switching tabs should reveal the tab you
    ! switched to -- and leaves the scroll alone otherwise, so a position
    ! chosen with the chevrons survives the next redraw.
    integer :: g_last_active(2) = 0
    ! And the width it was laid out at. A resize changes what fits, so the
    ! active entry is worth re-revealing then -- otherwise shrinking the
    ! window can leave the tab you are editing behind a chevron.
    integer :: g_last_width(2) = 0

    integer :: g_tab_scroll = 1
    integer :: g_group_scroll = 1
    ! The group under the pointer, 0 for none, and the column window the tab
    ! bar was last drawn in -- with the tree open the bar does not start at
    ! column 1, and the preview must inherit that rather than draw over the
    ! tree.
    integer(int32) :: g_hover_group = 0
    ! Where each drawn entry sits, per strip, recorded as it is drawn.
    ! A drop target is a SLOT -- a position among the entries -- and the
    ! clickable region only carries a payload, so the mapping has to come from
    ! whoever did the placing.
    integer :: g_slot_n(2) = 0
    integer :: g_slot_c0(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_slot_c1(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_slot_idx(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_slot_row(2) = 0
    ! Which group row 2 belongs to. It is the pinned member row while inside a
    ! group and a hover preview otherwise, and a drop needs to know WHICH
    ! group it landed on -- the two cases are the same strip.
    integer(int32) :: g_slot2_gid = 0
    ! Where every entry would be if nothing were being dragged.
    !
    ! A drag needs to ask "what is under the pointer" and get a STABLE answer.
    ! The drawn bar cannot give one: the preview moves the held entry under
    ! the pointer, and suppressing the preview moves it back, so a decision
    ! taken from the drawn layout changes the drawn layout and the two
    ! oscillate frame by frame -- the entry flickering between two places as
    ! the pointer sits still.
    !
    ! So the layout is computed twice: once as it would be with nothing held,
    ! recorded here and used for every decision, and once with the preview
    ! applied, which is what gets drawn.
    integer :: g_hit_n(2) = 0
    integer :: g_hit_row(2) = 0
    integer :: g_hit_c0(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_hit_c1(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_hit_slot(2, STRIP_MAX_ENTRIES) = 0
    integer :: g_hit_payload(2, STRIP_MAX_ENTRIES) = 0

    integer :: g_tabbar_col0 = 1
    integer :: g_tabbar_width = 80
    logical :: g_group_preview_enabled = .true.

    ! Where the preview strip was last actually drawn, so the pointer can move
    ! down into it without dismissing the thing it is moving towards.
    !
    ! Recorded at draw time rather than re-derived: row 2 is the preview strip
    ! only while OUTSIDE a group, and the pinned member row while inside one.
    ! Asking "was a preview drawn there" is a different question from "what is
    ! at row 2", and only the first one should keep a hover alive.
    ! g_preview_row = 0 means no preview is on screen.
    integer :: g_preview_row = 0
    integer :: g_preview_col0 = 0
    integer :: g_preview_col1 = 0

    type :: strip_entry_t
        character(len=192) :: label = ''
        integer :: payload = 0
        logical :: dim = .false.       ! drawn grey (an orphan tab)
        logical :: modified = .false.
    end type strip_entry_t

    type :: strip_span_t
        integer :: idx = 0             ! index into the entry array
        integer :: col0 = 0, col1 = 0  ! inclusive screen columns
    end type strip_span_t


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
        integer :: content_width
        integer :: start_row, row_offset_val
        character(len=16) :: line_num_str

        ! Auto-update syntax highlighter if filename changed
        if (allocated(editor%filename)) then
            call update_syntax_highlighter(editor%filename)
        end if

        call terminal_hide_cursor()
        call regions_begin_frame()

        ! Render tab bar if there are any tabs
        call render_tab_bar(editor)

        call update_bracket_match(buffer, editor)

        ! Get total lines in buffer
        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers)
        if (show_line_numbers) then
            content_width = editor%screen_cols - LINE_NUMBER_WIDTH - 1  ! -1 for separator
        else
            content_width = editor%screen_cols
        end if

        start_row = first_content_row(editor)
        row_offset_val = start_row

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
                call render_references_panel(editor%references_panel, first_content_row(editor))

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
                call render_menu_overlay(editor)
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
                            call terminal_write(theme_paint(THEME_LINE_NUMBER_ACTIVE, &
                                adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                        else
                            call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                                adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                        end if
                    else
                        ! Empty line number area for lines beyond file
                        call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                            repeat(' ', LINE_NUMBER_WIDTH + 1)))
                    end if
                end if

                if (buffer_line <= line_count) then
                    ! Render actual line content with selections
                    call render_line_with_selections(buffer, editor, buffer_line, &
                                                    editor%viewport_column, content_width)
                else
                    call terminal_write(theme_sgr(THEME_EDITOR) // '~')
                end if
                ! Erased cells use the current background, so establish the
                ! editor surface before clearing stale content.
                call terminal_write(theme_sgr(THEME_EDITOR_BG) // &
                    char(27) // '[K' // theme_reset())
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
        call render_references_panel(editor%references_panel, first_content_row(editor))

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
        call render_menu_overlay(editor)
    end subroutine render_screen

    ! Draw the context menu, if any, as the last thing in a frame.
    !
    ! Must come after the caret renderers: they write reverse-video cells at
    ! every inactive multi-cursor position, which would punch holes through
    ! the box. And the frame's only flush is the caret call, so anything
    ! written after it would otherwise sit in the output buffer until the
    ! next frame -- a menu one frame late, and stale bytes on the frame it is
    ! dismissed. Hiding the caret both suppresses a caret blinking behind the
    ! box and flushes.
    ! Recompute which bracket pair, if any, sits under the caret. The four
    ! module variables it sets are read by the line renderers, so a frame that
    ! repaints only some lines must call this too or it will paint a
    ! highlight from the previous caret position.
    !
    ! The cursor column is a char index; utf8_char_at handles bounds and
    ! returns '' (padded to a space) past EOL. Multibyte chars truncate to
    ! their lead byte, which is never an ASCII bracket.
    !> Repaint only what a plain caret move changes, instead of the whole
    !> screen. A full frame is ~3.3 KB on an 80x24-ish terminal; this is a few
    !> hundred bytes, which is the difference between comfortable and
    !> unusable when the terminal is at the far end of an ssh link.
    !>
    !> Four things change when the caret moves without editing: the status
    !> bar's Ln/Col, the caret itself, the line-number gutter (the caret's line
    !> is drawn bright, the one it left goes dim), and the bracket-match
    !> highlight (up to two lines gained, up to two lost). So the affected
    !> lines are known exactly rather than guessed at -- there is no
    !> "probably nothing else moved" in here.
    !>
    !> The caller is responsible for only invoking this when nothing else on
    !> screen could have changed; see the guard in app/main.f90.
    subroutine render_caret_move(buffer, editor, prev_line, match_mode_active, &
                                 match_case_sens)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: prev_line
        logical, intent(in), optional :: match_mode_active
        logical, intent(in), optional :: match_case_sens
        integer :: dirty(6), n_dirty, i, j, tab_idx, pane_idx
        integer :: old_bracket, old_match, line_count, screen_row
        integer :: line_num_width, adjusted_width, start_row
        logical :: seen

        tab_idx = editor%active_tab_index
        if (tab_idx < 1) return
        if (tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1) return
        if (pane_idx > size(editor%tabs(tab_idx)%panes)) return

        ! The module bracket state still describes the previous caret
        old_bracket = bracket_line
        old_match = matching_bracket_line
        call update_bracket_match(buffer, editor)

        n_dirty = 0
        call mark(prev_line)
        call mark(editor%cursors(editor%active_cursor)%line)
        call mark(old_bracket)
        call mark(old_match)
        call mark(bracket_line)
        call mark(matching_bracket_line)

        ! Mirror render_editor_pane's own geometry, which is what
        ! render_all_panes uses for the single-pane case the guard admits.
        if (show_line_numbers) then
            line_num_width = LINE_NUMBER_WIDTH + 1
        else
            line_num_width = 0
        end if
        start_row = first_content_row(editor)

        line_count = buffer_get_line_count(buffer)

        ! Render in ascending order. The tokenizer's comment state is
        ! established per line by seed_comment_state, so isolated rows are
        ! coloured correctly without walking the viewport here.
        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            if (allocated(pane%filename)) call name_surface(pane%filename)
            adjusted_width = pane%screen_width - line_num_width
            do i = 1, n_dirty
                if (dirty(i) < 1) cycle
                if (dirty(i) > line_count) cycle
                screen_row = start_row + (dirty(i) - editor%viewport_line)
                if (screen_row < start_row) cycle
                if (screen_row > editor%screen_rows - 1) cycle
                call render_editor_row(pane%buffer, editor, dirty(i), screen_row, &
                                       1, adjusted_width, line_num_width, line_count)
            end do
        end associate

        call render_status_bar(editor, buffer, match_mode_active, match_case_sens)
        call render_cursor_for_panes(editor)

    contains

        subroutine mark(ln)
            integer, intent(in) :: ln

            if (ln < 1) return
            seen = .false.
            do j = 1, n_dirty
                if (dirty(j) == ln) seen = .true.
            end do
            if (seen) return
            if (n_dirty >= size(dirty)) return
            n_dirty = n_dirty + 1
            dirty(n_dirty) = ln
        end subroutine mark

    end subroutine render_caret_move

    subroutine update_bracket_match(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        type(cursor_t) :: cursor
        character(len=:), allocatable :: line_content, cursor_char
        logical :: found_match

        cursor = editor%cursors(editor%active_cursor)
        line_content = buffer_get_line(buffer, cursor%line)
        cursor_char = utf8_char_at(line_content, cursor%column)
        if (is_bracket_char(cursor_char)) then
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
    end subroutine update_bracket_match

    subroutine render_menu_overlay(editor)
        type(editor_state_t), intent(in) :: editor
        logical :: want_motion

        ! Any-motion reporting is switched here rather than in the menu
        ! module, for two reasons. The frame loop converges on the right state
        ! whatever route opened or closed the menu, so there is one place
        ! instead of one per dismissal path. And unit tests construct menus
        ! without ever rendering, so they no longer reconfigure the terminal
        ! they are run from -- an earlier version left mode 1003 on after
        ! `fpm test`, which makes a shell spew escape bytes on every mouse
        ! movement.
        !
        ! Two surfaces want motion now. They are combined into ONE demand here
        ! rather than each toggling the mode, so there is still exactly one
        ! owner and one flag; two owners would let whichever closed last turn
        ! the mode off underneath the other.
        !
        ! The group preview cannot ask for motion only while hovering -- you
        ! need the events to discover the pointer is over the bar. So the
        ! demand is standing: any group existing is enough. That is a real
        ! cost over a slow link, which is why it is behind a setting.
        want_motion = is_context_menu_visible() .or. tab_group_wants_motion(editor)

        if (want_motion .neqv. g_motion_tracking) then
            call terminal_set_motion_tracking(want_motion)
            g_motion_tracking = want_motion
        end if

        ! Drawn before the context menu so a menu still sits on top.
        call render_group_preview(editor)

        if (is_context_menu_visible()) then
            call render_context_menu()
            call terminal_hide_cursor()
        end if

        ! The dialog is topmost: it is what the user is looking at.
        ! The file browser window, on the same footing as the group dialog.
        if (is_fortress_visible()) &
            call render_fortress(first_content_row(editor), editor%screen_rows - 1, &
                                 1, editor%screen_cols)
        if (is_group_picker_visible()) call render_group_picker()

        ! Except while something is being dragged, which is above even that:
        ! it is attached to the pointer.
        call render_split_preview(editor)
        call render_drag_ghost(editor)

        ! The find bar replaces the status line while it is up, so it has to
        ! come after render_status_bar -- and it leaves the caret in its own
        ! field, so it has to come after the caret renderers too.
        if (is_search_panel_visible()) call render_search_panel(editor)

        ! This runs last in every frame, and render_screen's panes branch
        ! returns straight after it without flushing -- so anything drawn
        ! above would sit in the write buffer until some later frame happened
        ! to flush it. Which, for a preview that appears and disappears with
        ! the pointer, means never.
        call terminal_flush()
    end subroutine render_menu_overlay

    integer function group_find_public(editor, gid)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        group_find_public = group_find(editor, gid)
    end function group_find_public

    subroutine set_group_preview_enabled(on)
        logical, intent(in) :: on
        g_group_preview_enabled = on
    end subroutine set_group_preview_enabled

    !> Does anything want any-motion reporting for the tab bar?
    function tab_group_wants_motion(editor) result(want)
        type(editor_state_t), intent(in) :: editor
        logical :: want

        want = g_group_preview_enabled .and. size(editor%groups) > 0
    end function tab_group_wants_motion

    !> Note the group under the pointer. True only when it CHANGED, which is
    !> the caller's cue to repaint -- the same contract context_menu_hover
    !> uses, and the reason moving the mouse does not cost a frame per pixel.
    !> Takes no editor on purpose: the region table already knows where every
    !> entry was drawn, so re-deriving the layout here would be a second
    !> opinion that could disagree with the first.
    function tab_group_hover(row, col) result(moved)
        integer, intent(in) :: row, col
        logical :: moved
        type(clickable_region_t) :: hit
        integer(int32) :: found

        found = 0
        if (row == 1) then
            hit = region_at(row, col)
            ! Row 1 stores a group as a negative payload.
            if (hit%kind == REGION_TAB .and. hit%payload < 0) &
                found = int(-hit%payload, int32)
        else if (g_hover_group /= 0 .and. row == g_preview_row) then
            ! Inside the strip that is already being previewed, the hover
            ! stands. Without this, moving the pointer down towards the members
            ! dismissed them -- the preview could be seen but never reached,
            ! which is most of what a preview is for.
            !
            ! The WHOLE drawn strip counts, padding past the last member
            ! included. Resolving this through the region table would make the
            ! empty tail of the strip "outside", so the preview would vanish on
            ! crossing from the last member into blank space inside the same
            ! visual band. The band is one thing to the eye and is treated as
            ! one thing here.
            if (col >= g_preview_col0 .and. col <= g_preview_col1) &
                found = g_hover_group
        end if

        ! Resolving through the region table rather than re-deriving the
        ! layout is the point of having the table: multibyte labels come out
        ! right for free.
        !
        ! Still true only on a CHANGE. Persisting must cost nothing: motion is
        ! reported per cell of pointer travel, so repainting for "still on the
        ! same group" would be a frame per cell crossed. That is also why
        ! g_group_scroll is reset here and not on every event -- otherwise a
        ! scrolled member list would snap back to its head as the pointer moved
        ! within the strip.
        moved = (found /= g_hover_group)
        if (moved) g_group_scroll = 1     ! a preview always starts at its head
        g_hover_group = found
    end function tab_group_hover

    !> Preview `gid` outright, without asking what is under the pointer.
    !>
    !> The hover version resolves through the region table, which during a
    !> drag describes the PREVIEWED bar -- the held entry sits under the
    !> pointer, so it would answer "no group here" every time. A drag already
    !> knows which group it dwelt on; it just needs to say so.
    function tab_group_set_hover(gid) result(moved)
        integer(int32), intent(in) :: gid
        logical :: moved

        moved = (gid /= g_hover_group)
        if (moved) g_group_scroll = 1
        g_hover_group = gid
    end function tab_group_set_hover

    !> Forget the hovered group. True if that changed anything.
    function tab_group_clear_hover() result(moved)
        logical :: moved

        moved = (g_hover_group /= 0)
        g_hover_group = 0
    end function tab_group_clear_hover

    logical function tab_group_preview_visible()
        tab_group_preview_visible = (g_hover_group /= 0)
    end function tab_group_preview_visible

    !> Draw the hovered group's members over the first document row.
    !>
    !> Over, not above: reflowing on hover would shift the text every time the
    !> pointer crossed the bar. Once you are actually inside a group the row is
    !> pinned instead and the document does move down, which is stable because
    !> it only happens when you enter.
    subroutine render_group_preview(editor)
        type(editor_state_t), intent(in) :: editor

        ! Disarmed on every path that draws nothing, so the pointer can never
        ! persist a hover against a strip that is not on screen -- including
        ! row 2 while inside a group, where that row belongs to the pinned
        ! member list instead.
        g_preview_row = 0

        if (g_hover_group == 0) return
        if (group_find_public(editor, g_hover_group) == 0) return
        if (tab_bar_height(editor) >= 2) return

        call render_group_row(editor, g_hover_group, 2, &
                              g_tabbar_col0, g_tabbar_width)

        g_preview_row = 2
        g_preview_col0 = g_tabbar_col0
        g_preview_col1 = g_tabbar_col0 + g_tabbar_width - 1
    end subroutine render_group_preview


    !> Put the tokenizer's comment state where it belongs for `line_num`.
    !>
    !> Called from the one place every render path funnels through, and only
    !> does work when the line being drawn is not the one that would follow
    !> naturally -- so a normal top-to-bottom frame costs nothing extra.
    !> Is the anchor from the same document we are drawing now?
    logical function same_hl_file()
        if (.not. allocated(g_hl_key_file)) then
            same_hl_file = .not. allocated(g_hl_surface)
            return
        end if
        if (.not. allocated(g_hl_surface)) then
            same_hl_file = .false.
            return
        end if
        same_hl_file = (g_hl_key_file == g_hl_surface)
    end function same_hl_file

    !> Name the document about to be drawn, so its comment scan is not
    !> resumed from another pane's file. An unnamed pane -- an untitled
    !> buffer -- gets a stable placeholder rather than sharing "no name" with
    !> every other unnamed one.
    subroutine name_surface(name)
        character(len=*), intent(in) :: name
        g_hl_surface = name
    end subroutine name_surface

    subroutine seed_comment_state(buffer, editor, line_num)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_num
        type(token_t), allocatable :: throwaway(:)
        character(len=:), allocatable :: text
        integer :: ln, from_line
        integer(int64) :: rev
        integer :: tab_idx

        if (.not. syntax_highlighter%enabled) return

        tab_idx = editor%active_tab_index
        rev = -1
        if (tab_idx >= 1 .and. tab_idx <= size(editor%tabs)) &
            rev = editor%tabs(tab_idx)%doc_revision

        ! Continuing the scan we were already on: nothing to do.
        if (line_num == g_hl_next_line .and. rev == g_hl_key_rev .and. &
            tab_idx == g_hl_key_tab .and. same_hl_file()) return

        ! The anchor is only usable for the document it was built from, and
        ! only for lines at or after it -- an edit anywhere above invalidates
        ! everything below, which is what the revision check catches.
        from_line = 1
        if (rev == g_hl_key_rev .and. tab_idx == g_hl_key_tab .and. &
            same_hl_file() .and. g_hl_anchor_line <= line_num) then
            from_line = g_hl_anchor_line
            syntax_highlighter%in_multiline_comment = g_hl_anchor_mc
            syntax_highlighter%in_multiline_string = g_hl_anchor_ms
            syntax_highlighter%string_delimiter = g_hl_anchor_delim
            syntax_highlighter%in_interp = g_hl_anchor_interp
            syntax_highlighter%interp_depth = g_hl_anchor_interp_depth
        else
            syntax_highlighter%in_multiline_comment = .false.
            syntax_highlighter%in_multiline_string = .false.
            syntax_highlighter%string_delimiter = ''
            syntax_highlighter%in_interp = .false.
            syntax_highlighter%interp_depth = 0
            ! Remember where this scan starts, so a later line can resume.
            g_hl_anchor_line = 1
            g_hl_anchor_mc = .false.
            g_hl_anchor_ms = .false.
            g_hl_anchor_delim = ''
            g_hl_anchor_interp = .false.
            g_hl_anchor_interp_depth = 0
            g_hl_key_rev = rev
            g_hl_key_tab = tab_idx
            if (allocated(g_hl_surface)) then
                g_hl_key_file = g_hl_surface
            else if (allocated(g_hl_key_file)) then
                deallocate(g_hl_key_file)
            end if
        end if

        do ln = from_line, line_num - 1
            text = buffer_get_line(buffer, ln)
            call tokenize_line(syntax_highlighter, text, throwaway)
            if (allocated(throwaway)) deallocate(throwaway)
        end do

        ! Cache what we just computed so scrolling on does not rescan.
        g_hl_anchor_line = line_num
        g_hl_anchor_mc = syntax_highlighter%in_multiline_comment
        g_hl_anchor_ms = syntax_highlighter%in_multiline_string
        g_hl_anchor_delim = syntax_highlighter%string_delimiter
        g_hl_anchor_interp = syntax_highlighter%in_interp
        g_hl_anchor_interp_depth = syntax_highlighter%interp_depth
        g_hl_key_rev = rev
        g_hl_key_tab = tab_idx
    end subroutine seed_comment_state

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
        logical :: has_active_match, is_active_match
        integer :: active_sbyte, active_ebyte

        line = buffer_get_line(buffer, line_num)
        line_byte_len = len(line)
        char_count = utf8_char_count(line)

        ! Get all search matches on this line (these use byte indices)
        if (search_mode_active) then
            call get_matches_on_line(line, line_num, search_matches, num_search_matches)
        else
            num_search_matches = 0
        end if

        ! Which of them, if any, is the one the caret is on. Without this
        ! every match looks the same and walking a search tells you nothing
        ! about where you are in it.
        has_active_match = active_match_span(line_num, active_sbyte, active_ebyte)

        ! Get syntax tokens for this line (tokens use byte indices)
        if (syntax_highlighter%enabled) then
            call seed_comment_state(buffer, editor, line_num)
            call tokenize_line(syntax_highlighter, line, tokens)
            g_hl_next_line = line_num + 1
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
            is_active_match = .false.
            if (byte_pos > 0) then
                do match_idx = 1, num_search_matches
                    if (byte_pos >= search_matches(1, match_idx) .and. byte_pos <= search_matches(2, match_idx)) then
                        is_search_match = .true.
                        exit
                    end if
                end do
                if (has_active_match) then
                    if (byte_pos >= active_sbyte .and. byte_pos <= active_ebyte) is_active_match = .true.
                end if
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
            if (is_active_match) then
                ! A separate semantic role keeps the active match distinct
                ! from both ordinary matches and selected text.
                style = theme_sgr(THEME_SEARCH_MATCH_ACTIVE)
            else if (in_selection) then
                ! Selected text: reverse video
                style = theme_sgr(THEME_SELECTION)
            else if (is_bracket_match) then
                style = theme_sgr(THEME_STATUS_ACCENT)
            else if (is_search_match) then
                ! Search matches: yellow background (+ syntax color)
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_SEARCH_MATCH)
                else
                    style = theme_sgr(THEME_SEARCH_MATCH)
                end if
            else if (is_current_line) then
                ! Current line: subtle background (+ syntax color)
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_CURRENT_LINE)
                else
                    style = theme_sgr(THEME_CURRENT_LINE)
                end if
            else
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_EDITOR)
                else
                    style = theme_sgr(THEME_EDITOR)
                end if
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

        ! Fill the row explicitly. Terminal defaults are user-configurable and
        ! cannot stand in for the selected theme's editor background.
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
                style = theme_sgr(THEME_SELECTION)
            else if (is_current_line) then
                style = theme_sgr(THEME_CURRENT_LINE)
            else
                style = theme_sgr(THEME_EDITOR_BG)
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
        character(len=:), allocatable :: status_tail
        character(len=200) :: fname_disp
        integer :: padding_len, left_pad, right_pad, fname_len
        type(cursor_t) :: cursor
        logical :: show_match_hint
        character(len=:), allocatable :: chevron
        logical :: chevron_shown

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
            write(status_left, '(a,a,a)') trim(fname_disp), &
                   merge(' [modified]', '           ', buffer%modified), ' '
        else
            write(status_left, '(a,a,a)') '[No Name]', &
                   merge(' [modified]', '           ', buffer%modified), ' '
        end if

        ! The chevron is needed before the whole-bar messages below, which
        ! return early and must still carry it.
        if (editor%fuss_mode_active) then
            chevron = '«'
        else
            chevron = '»'
        end if
        chevron_shown = .false.

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
                call write_bar_with_chevron(editor, chevron, trim(editor%timed_message))
                return
            else if (len_trim(editor%timed_message) > 0) then
                editor%timed_message = ''  ! Expired, clear it
            end if

            ! Check for diagnostics at cursor position
            if (allocated(editor%filename)) then
                file_uri = 'file://' // trim(editor%filename)
                line_diagnostics = get_diagnostics_for_line(editor%diagnostics, file_uri, cursor%line)
            end if

            ! A one-shot message wins over a diagnostic: the diagnostic is
            ! still there next frame, but the message is gone. Losing "AI
            ! completion: ready" to a squiggle the user can already see makes
            ! commands look like they did nothing.
            if (has_status_message()) then
                call write_bar_with_chevron(editor, chevron, g_status_message)
                return
            end if

            if (allocated(line_diagnostics) .and. size(line_diagnostics) > 0) then
                ! Show first diagnostic message (highest severity)
                call write_bar_with_chevron(editor, chevron, &
                    trim(line_diagnostics(1)%message))
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

            ! Whenever code can leave this machine, say so and keep saying so.
            ! The user must never have to remember what they configured.
            block
                use ai_state_module, only: ai_remote_badge, ai_indicator
                character(len=:), allocatable :: badge, ind
                badge = ai_remote_badge(editor%ai)
                if (len(badge) > 0) then
                    status_center = badge // ' ' // trim(status_center)
                else
                    ind = ai_indicator(editor%ai)
                    if (len(ind) > 0) status_center = ind // ' ' // trim(status_center)
                end if
            end block
        end block

        if (size(editor%cursors) > 1) then
            write(status_right, '(a,i0,a,a,i0,a,i0,a)') '[', size(editor%cursors), ' cursors] ', &
                   'Ln ', cursor%line, ', Col ', cursor%column, ' '
        else
            write(status_right, '(a,i0,a,i0,a)') 'Ln ', cursor%line, ', Col ', cursor%column, ' '
        end if

        ! Clickable fuss-mode handle, pinned to the far right of the bar. It
        ! points the way the tree will move: right to open it, left to close.
        ! A reserved slot rather than borrowed padding, because the padding
        ! collapses to nothing whenever a message or a diagnostic takes the
        ! whole bar.

        ! The chevron leads the bar, in the corner, next to what it toggles.
        ! Prepended after the message override so it survives one -- it is a
        ! control, not a status, and a control that disappears when a message
        ! arrives is a control you cannot rely on. It stands in for the old
        ! 'ctrl-b:fuss' text: same meaning, and it also shows which way the
        ! tree will move and can be clicked.
        status_tail = trim(status_left)
        status_left = chevron // ' | ' // status_tail

        ! Create full status bar with center text.
        !
        ! Measured in display cells, not bytes. The leading chevron and a path
        ! containing wide characters must occupy the same columns the padding
        ! calculation reserves for them.
        padding_len = editor%screen_cols - utf8_display_width(trim(status_left)) &
                      - utf8_display_width(trim(status_center)) &
                      - utf8_display_width(trim(status_right))
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
                padding_len = editor%screen_cols - utf8_display_width(trim(status_left)) - &
                    utf8_display_width(trim(status_center)) - &
                    utf8_display_width(trim(status_right))
                if (padding_len > 0) then
                    left_pad = padding_len / 2
                    right_pad = padding_len - left_pad
                    status_bar = trim(status_left) // repeat(' ', left_pad) // &
                                trim(status_center) // repeat(' ', right_pad) // trim(status_right)
                else
                    ! Still not enough space, show hint + right only
                    padding_len = editor%screen_cols - &
                        utf8_display_width(trim(status_center)) - &
                        utf8_display_width(trim(status_right))
                    if (padding_len > 0) then
                        status_bar = repeat(' ', padding_len / 2) // trim(status_center) // &
                                    repeat(' ', padding_len - padding_len / 2) // trim(status_right)
                    else
                        ! Absolute minimum: just show the hint centered
                        padding_len = editor%screen_cols - &
                            utf8_display_width(trim(status_center))
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
                padding_len = editor%screen_cols - utf8_display_width(trim(status_left)) - &
                    utf8_display_width(trim(status_right))
                if (padding_len > 0) then
                    status_bar = trim(status_left) // repeat(' ', padding_len) // trim(status_right)
                else
                    ! Absolute paths on macOS can be longer than the entire
                    ! terminal. Coordinates remain actionable information, so
                    ! reserve their cells and retain the filename end of the
                    ! path. The Fuss control remains at column 1.
                    block
                        character(len=:), allocatable :: shown_left, shown_tail, ch
                        integer :: left_cells, left_budget, right_cells
                        integer :: prefix_cells, tail_budget, tail_cells
                        integer :: ci, ch_cells
                        right_cells = utf8_display_width(trim(status_right))
                        left_budget = max(0, editor%screen_cols - right_cells)
                        prefix_cells = utf8_display_width(chevron // ' | ')
                        if (utf8_display_width(trim(status_left)) <= left_budget) then
                            shown_left = trim(status_left)
                            left_cells = utf8_display_width(shown_left)
                        else if (left_budget > prefix_cells) then
                            tail_budget = max(0, left_budget - prefix_cells - 1)
                            shown_tail = ''
                            tail_cells = 0
                            do ci = utf8_char_count(status_tail), 1, -1
                                ch = utf8_char_at(status_tail, ci)
                                ch_cells = utf8_display_width(ch)
                                if (tail_cells + ch_cells > tail_budget) exit
                                shown_tail = ch // shown_tail
                                tail_cells = tail_cells + ch_cells
                            end do
                            shown_left = chevron // ' | ' // '…' // shown_tail
                            left_cells = prefix_cells + 1 + tail_cells
                        else
                            call clip_to_cells(chevron // ' | ', left_budget, &
                                               shown_left, left_cells)
                        end if
                        status_bar = shown_left // &
                            repeat(' ', max(0, left_budget - left_cells)) // &
                            trim(status_right)
                    end block
                end if
            end if
        end if

        ! Claim the chevron only if it really is the first thing on the bar.
        ! Several narrow fallbacks above drop the left section entirely, and
        ! asking the finished string is more reliable than tracking a flag
        ! down each of those branches -- it cannot fall out of step when
        ! someone adds another one.
        chevron_shown = .false.
        if (len(status_bar) >= len(chevron)) then
            if (status_bar(1:len(chevron)) == chevron) chevron_shown = .true.
        end if
        if (chevron_shown) then
            ! Column 1 is the chevron; include the space after it so the
            ! target is two cells rather than one.
            call region_add(REGION_FUSS_TOGGLE, editor%screen_rows, editor%screen_rows, &
                            1, 2)
        end if

        ! Render with inverse video, clamped to the terminal width
        call write_status_message(editor%screen_cols, trim(status_bar))
    end subroutine render_status_bar

    ! A whole-bar message that still carries the fuss chevron, and claims it.
    ! The messages take over the entire status bar, so without this the one
    ! control living there would blink out of existence every time something
    ! had anything to say.
    subroutine write_bar_with_chevron(editor, chevron, text)
        type(editor_state_t), intent(in) :: editor
        character(len=*), intent(in) :: chevron, text

        call region_add(REGION_FUSS_TOGGLE, editor%screen_rows, editor%screen_rows, &
                        1, 2)
        call write_status_message(editor%screen_cols, chevron // ' | ' // text)
    end subroutine write_bar_with_chevron

    ! Produce exactly `width` display cells for the status row. Control
    ! characters are blanked because LSP messages can contain newlines and
    ! tabs. Clipping is by cells and by character boundary. The old byte
    ! budget counted UTF-8 bytes beyond the visible prefix, which let distant
    ! multibyte punctuation inflate the prefix until it wrapped.
    subroutine format_status_line(width, text, line)
        integer, intent(in) :: width
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: line
        character(len=:), allocatable :: clipped
        integer :: i, used, text_width

        if (width < 1) then
            line = ''
            return
        end if

        line = text
        do i = 1, len(line)
            if (iachar(line(i:i)) < 32 .or. iachar(line(i:i)) == 127) line(i:i) = ' '
        end do

        text_width = utf8_display_width(line)
        if (text_width > width) then
            if (width > 3) then
                call clip_to_cells(line, width - 3, clipped, used)
                line = clipped // '...'
                used = used + 3
            else
                call clip_to_cells(line, width, clipped, used)
                line = clipped
            end if
        else
            used = text_width
        end if

        if (used < width) line = line // repeat(' ', width - used)
    end subroutine format_status_line

    ! Write one status-bar line in inverse video. The formatted text is always
    ! one physical terminal row, so later caret-only paints cannot inherit a
    ! wrapped cursor and draw document lines in the wrong place.
    subroutine write_status_message(width, text)
        integer, intent(in) :: width
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: line

        if (width < 1) return
        call format_status_line(width, text, line)

        call terminal_write(theme_sgr(THEME_STATUS))
        call terminal_write(line)
        call terminal_write(theme_reset())
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

        row_offset = first_content_row(editor)
        min_row = row_offset

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
                        call terminal_write(theme_sgr(THEME_SELECTION) // &
                                            cursor_char // theme_reset())
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
                        screen_height = text_area_height(editor)
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
        screen_height = text_area_height(editor)
        v_margin = min(margin, max(0, (screen_height - 1) / 2))

        ! Vertical scrolling
        if (cursor%line < editor%viewport_line + v_margin) then
            editor%viewport_line = max(1, cursor%line - v_margin)
        else if (cursor%line > editor%viewport_line + screen_height - v_margin) then
            editor%viewport_line = cursor%line - screen_height + v_margin
        end if

        ! Never scroll past the last buffer line (see pane path above)
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            editor%active_tab_index <= size(editor%tabs)) then
            editor%viewport_line = min(editor%viewport_line, &
                max(1, buffer_get_line_count(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                    editor%active_tab_index))%buffer)))
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
        call regions_begin_frame()

        ! Calculate split: 30% for tree, 70% for editor
        tree_width = editor%screen_cols * 30 / 100
        separator_col = tree_width + 1
        editor_start_col = tree_width + 2
        editor_width = editor%screen_cols - editor_start_col + 1

        ! Clear row 1 left side (tree area on tab bar row, not covered by other components)
        call terminal_move_cursor(1, 1)
        call terminal_write(theme_sgr(THEME_PANEL) // repeat(' ', tree_width) // theme_reset())

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
        call render_file_tree(tree_state, first_content_row(editor), content_bottom, 2, &
            tree_width - 2, editor%fuss_hints_expanded, &
            fuss_git_prefix_active)

        ! The split owns its column across the tab strip too. Starting at the
        ! content row left a terminal-background cell above the divider, so
        ! the editor tab bar appeared to bleed into the Fuss surface.
        call render_vertical_separator(separator_col, 1, content_bottom)

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
        call render_references_panel(editor%references_panel, first_content_row(editor))

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
        call render_menu_overlay(editor)
    end subroutine render_screen_with_tree

    subroutine render_vertical_separator(col, start_row, end_row)
        integer, intent(in) :: col, start_row, end_row
        integer :: row

        do row = start_row, end_row
            call terminal_move_cursor(row, col)
            call terminal_write(theme_paint(THEME_BORDER, '│'))
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

        screen_height = text_area_height(editor)

        ! If only one pane, use simple rendering
        if (n_panes == 1) then
            ! Record the rect before drawing. With the tree open the text
            ! starts at start_col, but the pane kept the screen_col = 1 it was
            ! given at tab creation, so anything resolving a click through
            ! position_cursor_at_screen was wrong by the whole tree width.
            ! Harmless until now only because fuss mode swallowed every mouse
            ! event before it got that far. Mirrors render_editor_pane's own
            ! geometry: rows start below the tab bar and stop above the status
            ! bar, columns span the editor area it is handed.
            block
                integer :: content_row

                content_row = first_content_row(editor)
                call store_pane_content_rect(editor, tab_idx, 1, start_col, &
                                             content_row, width, &
                                             editor%screen_rows - content_row)
            end block

            ! Use the pane's buffer, not the passed buffer parameter
            if (allocated(editor%tabs(tab_idx)%panes(1)%filename)) &
                call name_surface(editor%tabs(tab_idx)%panes(1)%filename)
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, start_col, width)
            return
        end if

        ! Multiple panes: render each with adjusted coordinates for tree view
        ! Clear the editor area first
        do i = first_content_row(editor), editor%screen_rows - 1
            call terminal_move_cursor(i, start_col)
            call terminal_write(theme_sgr(THEME_EDITOR_BG) // &
                repeat(' ', width) // theme_reset())
        end do

        ! Render each pane with coordinates adjusted for tree offset
        do i = 1, n_panes
            pane = editor%tabs(tab_idx)%panes(i)
            ! Each pane is its own document as far as the tokenizer's
            ! multi-line comment state is concerned.
            if (allocated(pane%filename)) call name_surface(pane%filename)

            ! Calculate pane position relative to editor area (not full screen)
            pane_col = start_col + int(pane%x_start * real(width))
            if (i < n_panes) then
                pane_width = int((pane%x_end - pane%x_start) * real(width)) - 1
            else
                pane_width = int((pane%x_end - pane%x_start) * real(width))
            end if
            pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            call store_pane_content_rect(editor, tab_idx, i, pane_col, pane_row, &
                                         pane_width, pane_height)

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

        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers if enabled)
        if (show_line_numbers) then
            line_num_width = LINE_NUMBER_WIDTH + 1
            adjusted_width = width - line_num_width
        else
            line_num_width = 0
            adjusted_width = width
        end if

        start_row = first_content_row(editor)

        ! Render each visible line in the editor pane
        do screen_row = start_row, editor%screen_rows - 1
            buffer_line = editor%viewport_line + screen_row - start_row
            call render_editor_row(buffer, editor, buffer_line, screen_row, &
                                   start_col, adjusted_width, line_num_width, &
                                   line_count)
        end do
    end subroutine render_editor_pane

    ! One screen row of the editor: gutter, content, clear-to-end.
    !
    ! Extracted so the caret-move fast path repaints a row exactly the way a
    ! full frame does. Anything that lives only in one of the two shows up as
    ! a line that looks subtly wrong until the next full redraw.
    subroutine render_editor_row(buffer, editor, buffer_line, screen_row, start_col, &
                                 adjusted_width, line_num_width, line_count)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: buffer_line, screen_row, start_col
        integer, intent(in) :: adjusted_width, line_num_width, line_count
        character(len=16) :: line_num_str

        call terminal_move_cursor(screen_row, start_col)

        if (show_line_numbers) then
            if (buffer_line <= line_count) then
                write(line_num_str, '(i5)') buffer_line
                if (buffer_line == editor%cursors(editor%active_cursor)%line) then
                    call terminal_write(theme_paint(THEME_LINE_NUMBER_ACTIVE, &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                else
                    call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                end if
            else
                call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                    repeat(' ', line_num_width)))
            end if
        end if

        ! render_line_with_selections writes at most adjusted_width cells;
        ! the ESC[K below clears the rest
        if (buffer_line <= line_count) then
            call render_line_with_selections(buffer, editor, buffer_line, &
                                            editor%viewport_column, adjusted_width)
        else
            call terminal_write(theme_sgr(THEME_EDITOR) // '~')
        end if
        call terminal_write(theme_sgr(THEME_EDITOR_BG) // &
            char(27) // '[K' // theme_reset())
    end subroutine render_editor_row

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
                if (is_bracket_char(cursor_char)) then
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
        screen_height = text_area_height(editor)

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
            editor%tabs(tab_idx)%panes(1)%screen_row = first_content_row(editor)
            editor%tabs(tab_idx)%panes(1)%screen_width = screen_width
            editor%tabs(tab_idx)%panes(1)%screen_height = screen_height

            ! Use the pane's buffer, not the passed buffer parameter
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, 1, screen_width)
            return
        end if

        ! Clear the editor area first with background
        do i = first_content_row(editor), editor%screen_rows - 1
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
            pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            call store_pane_content_rect(editor, tab_idx, i, pane_col, pane_row, &
                                         pane_width, pane_height)

            ! Render the pane content
            call render_single_pane(editor, i, pane_col, pane_row, pane_width, pane_height)

            ! Draw vertical separator between panes
            if (i < n_panes) then
                call render_pane_separator(pane_col + pane_width, pane_row, pane_height)
            end if
        end do
    end subroutine render_all_panes

    ! Record what a click can actually land on: the pane's CONTENT rectangle,
    ! excluding the header row that render_single_pane draws when more than
    ! one pane exists. (col, row, width, height) are the full pane rect as
    ! passed to render_single_pane.
    !
    ! Storing the header row here instead was an off-by-one for every click in
    ! a split: the caret renderers add the header offset themselves, but
    ! position_cursor_at_screen inverts the stored value directly, so clicking
    ! the row showing line 1 selected line 2. Excluding the header also means
    ! a click on it falls outside every pane and is ignored, rather than
    ! silently selecting the top visible line.
    subroutine store_pane_content_rect(editor, tab_idx, pane_idx, col, row, width, height)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx, col, row, width, height
        integer :: header

        header = 0
        if (size(editor%tabs(tab_idx)%panes) > 1) header = 1

        editor%tabs(tab_idx)%panes(pane_idx)%screen_col = col
        editor%tabs(tab_idx)%panes(pane_idx)%screen_row = row + header
        editor%tabs(tab_idx)%panes(pane_idx)%screen_width = width
        editor%tabs(tab_idx)%panes(pane_idx)%screen_height = height - header
    end subroutine store_pane_content_rect

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
            if (pane%is_active) then
                call terminal_write(theme_sgr(THEME_EDITOR_BG))
            else
                call terminal_write(theme_sgr(THEME_SELECTION_INACTIVE))
            end if
            call terminal_write(repeat(' ', width))
            call terminal_write(theme_reset())
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
                if (pane%is_active) then
                    call terminal_write(theme_sgr(THEME_EDITOR))
                else
                    call terminal_write(theme_sgr(THEME_SELECTION_INACTIVE))
                end if
                if (show_line_numbers) &
                    call terminal_write(repeat(' ', LINE_NUMBER_WIDTH + 1))
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
                call terminal_write(theme_reset())
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
            call terminal_write(theme_sgr(THEME_TAB_ACTIVE))
        else
            call terminal_write(theme_sgr(THEME_TAB_INACTIVE))
        end if

        ! Draw the header line
        call terminal_write(repeat('─', padding_left))
        call terminal_write(filename_display)
        call terminal_write(repeat('─', padding_right))

        ! Reset attributes
        call terminal_write(theme_reset())
    end subroutine render_pane_header

    !> The file a pane is showing, for keying the highlight scan.
    function pane_document(editor, pane_idx) result(name)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_idx
        character(len=:), allocatable :: name
        integer :: t

        name = '?'
        t = editor%active_tab_index
        if (t < 1 .or. t > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(t)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(t)%panes)) return
        if (allocated(editor%tabs(t)%panes(pane_idx)%filename)) &
            name = editor%tabs(t)%panes(pane_idx)%filename
    end function pane_document

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
                if (is_current_line) then
                    call terminal_write(theme_paint(THEME_LINE_NUMBER_ACTIVE, &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                else
                    call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                        adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' '))
                end if
            else
                ! Inactive pane: keep the dim background continuous across the
                ! whole gutter (number + separator) so no default-background
                ! stripe shows through, and use a fixed gray (not [90m, whose
                ! shade varies by terminal and can vanish into the background).
                call terminal_write(theme_sgr(THEME_SELECTION_INACTIVE) // &
                    adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) // ' ' // theme_reset())
            end if

            ! Continue with pane background for content
            if (.not. pane%is_active) then
                call terminal_write(theme_sgr(THEME_SELECTION_INACTIVE))
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
            ! Seed the multi-line comment state first. Without this the
            ! tokenizer simply carried on from whatever line it happened to
            ! colour last -- which, with a split, is a line in the OTHER
            ! pane's file. An open block comment there left every following
            ! line of an unrelated document rendered as a comment, and
            ! scrolling made real block comments flicker in and out because
            ! nothing established whether the top of the viewport was inside
            ! one.
            !
            ! The single-pane path has done this all along; this one never
            ! did, so panes were the only place it was wrong.
            call name_surface(pane_document(editor, pane_idx))
            call seed_comment_state(buffer, editor, line_num)
            call tokenize_line(syntax_highlighter, line, tokens)
            g_hl_next_line = line_num + 1
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
                style = theme_sgr(THEME_SELECTION)
            else if (is_bracket_match) then
                style = theme_sgr(THEME_STATUS_ACCENT)
            else if (pane%is_active .and. is_current_line) then
                ! Current line background (+ syntax color)
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_CURRENT_LINE)
                else
                    style = theme_sgr(THEME_CURRENT_LINE)
                end if
            else if (.not. pane%is_active) then
                ! Inactive pane background (+ syntax color)
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_SELECTION_INACTIVE)
                else
                    style = theme_sgr(THEME_SELECTION_INACTIVE)
                end if
            else
                if (len(token_color) > 0) then
                    style = token_color // theme_background_sgr(THEME_EDITOR)
                else
                    style = theme_sgr(THEME_EDITOR)
                end if
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
                style = theme_sgr(THEME_SELECTION_INACTIVE)
            else if (is_current_line) then
                style = theme_sgr(THEME_CURRENT_LINE)
            else
                style = theme_sgr(THEME_EDITOR_BG)
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
            call terminal_write(theme_sgr(THEME_BORDER_FOCUS) // ' ' // theme_reset())
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
        screen_height = text_area_height(editor)

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
        pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
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
                        call terminal_write(theme_sgr(THEME_SELECTION) // &
                                            cursor_char // theme_reset())
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
        use symbols_panel_module, only: is_symbols_panel_visible
        use code_actions_panel_module, only: is_code_actions_panel_visible
        use hover_tooltip_module, only: is_hover_visible
        use signature_tooltip_module, only: is_signature_tooltip_visible
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
        integer :: block_col0, block_width
        logical :: use_panes

        if (.not. ghost_is_active(editor%ghost)) return

        ! Anything drawn over the editor takes precedence. The ghost is
        ! painted after these panels, so without this it draws on top of
        ! them -- and it can still be live here, because keys consumed by a
        ! panel return from handle_key_command before the ghost_clear that
        ! ordinarily dismisses it. Suppressing at the draw site makes the
        ! invariant hold however the ghost got there. The diagnostics and
        ! references panels are deliberately absent: they shrink the editor
        ! rather than covering it, and are already accounted for in the
        ! width calculation below.
        if (editor%completion_popup%visible) return
        if (is_context_menu_visible()) return
        if (is_symbols_panel_visible(editor%symbols_panel)) return
        if (is_code_actions_panel_visible(editor%code_actions_panel)) return
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) return
        if (is_hover_visible(editor%hover_tooltip)) return
        if (is_signature_tooltip_visible(editor%signature_tooltip)) return

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
            screen_height = text_area_height(editor)
            pane_col = 1 + int(pane%x_start * real(screen_width))
            pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
            if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
                pane_width = pane_width - 1  ! Reserve space for separator
            end if
            pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
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
            block_col0 = pane_col
            block_width = pane_width
        else
            if (size(editor%cursors) /= 1) return
            cursor = editor%cursors(editor%active_cursor)
            if (cursor%line /= editor%ghost%anchor_line .or. &
                cursor%column /= editor%ghost%anchor_col) return
            line = buffer_get_line(buffer, cursor%line)

            row_offset = first_content_row(editor)
            min_row = row_offset
            screen_row = cursor%line - editor%viewport_line + row_offset
            disp = display_offset_of(line, editor%viewport_column, cursor%column)
            screen_col = col_offset + 1 + disp
            if (screen_row < min_row .or. screen_row >= editor%screen_rows) return
            if (screen_col < 1 .or. screen_col > screen_width) return
            avail = screen_width - screen_col + 1
            block_col0 = 1
            block_width = screen_width
        end if

        if (avail < 1) return
        ! avail is a count of screen CELLS, so the suggestion has to be
        ! measured and cut the same way. Clipping by byte count would both
        ! overshoot the pane on multibyte text and slice a character in half,
        ! emitting a partial UTF-8 sequence to the terminal.
        call clip_to_cells(suffix, avail, shown, ghost_cells)
        if (len(shown) == 0) return

        call terminal_move_cursor(screen_row, screen_col)
        if (highlight_current_line) then
            call terminal_write(theme_sgr(THEME_CURRENT_LINE))
        else
            call terminal_write(theme_sgr(THEME_EDITOR_BG))
        end if
        call terminal_write(theme_foreground_sgr(THEME_GHOST) // shown // theme_reset())

        ! Mid-line: redraw the real text right of the cursor, shifted past
        ! the suggestion, so nothing is hidden while the ghost is up.
        ! Blocks are only ever offered at end of line, so this and the block
        ! row loop below can never both run.
        if (cursor%column <= utf8_char_count(line)) then
            call render_line_tail_shifted(line, cursor%column, &
                disp + ghost_cells, avail - ghost_cells)
        end if

        if (ghost_is_block(editor%ghost)) then
            call render_ghost_block(editor, buffer, screen_row, block_col0, block_width)
        end if
    end subroutine render_ghost_text

    ! Draw rows 2..N of a block suggestion below the caret, then redraw the
    ! real buffer lines those rows were showing, shifted down so nothing is
    ! hidden. Overlaying instead would make the file look like it had already
    ! changed, which is exactly the impression a suggestion must not give.
    !> Fill the rest of a pane's row with spaces.
    !>
    !> Stands in for ESC[K wherever the thing being drawn belongs to a pane
    !> rather than to the whole screen -- [K would take the neighbouring pane
    !> with it.
    subroutine pad_to_pane(written, cwidth)
        integer, intent(in) :: written, cwidth

        if (cwidth - written > 0) call terminal_write(repeat(' ', cwidth - written))
    end subroutine pad_to_pane

    subroutine render_ghost_block(editor, buffer, anchor_row, col0, cwidth)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: anchor_row, col0, cwidth
        character(len=:), allocatable :: text, shown
        integer :: n, i, row, bottom, gutter, content_w, used
        integer :: src_line, line_count

        n = editor%ghost%block_lines
        if (n < 2) return

        if (show_line_numbers) then
            gutter = LINE_NUMBER_WIDTH + 1
        else
            gutter = 0
        end if
        content_w = cwidth - gutter
        if (content_w < 1) return

        bottom = last_content_row(editor)
        line_count = buffer_get_line_count(buffer)

        ! The block must fit entirely, or it is not shown as a block at all --
        ! a half-drawn block reads as a suggestion that stops mid-thought.
        if (anchor_row + n - 1 > bottom) then
            call render_block_overflow_marker(anchor_row, col0, cwidth, n - 1)
            return
        end if

        do i = 2, n
            row = anchor_row + i - 2 + 1
            text = ghost_block_line(editor%ghost, i)
            if (.not. is_terminal_safe(text)) return
            call clip_to_cells(text, content_w, shown, used)
            call terminal_move_cursor(row, col0)
            call terminal_write(theme_sgr(THEME_EDITOR_BG))
            ! Blank the gutter: these rows are not lines in the file yet, and
            ! leaving the number the base render put there would duplicate it
            ! against the real line pushed down below.
            if (gutter > 0) call terminal_write(repeat(' ', gutter))
            call terminal_write(theme_foreground_sgr(THEME_GHOST) // shown)
            call terminal_write(theme_sgr(THEME_EDITOR_BG))
            ! Pad to the pane's width rather than ESC[K.
            !
            ! [K clears to the end of the TERMINAL LINE, which in a vertical
            ! split is everything to the right of this pane -- so a block
            ! suggestion erased the neighbouring pane from its first row
            ! downwards, and dismissing it brought the pane back. Correct
            ! while a pane spans the full width, which is why this only ever
            ! showed up in a split.
            call pad_to_pane(gutter + used, cwidth)
            call terminal_write(theme_reset())
        end do

        ! The ghost occupies rows anchor_row .. anchor_row + n - 1, so the
        ! real lines resume at anchor_row + n. Starting a row earlier would
        ! paint over the last row of the suggestion.
        do i = 0, bottom - (anchor_row + n)
            row = anchor_row + n + i
            if (row > bottom) exit
            src_line = editor%ghost%anchor_line + 1 + i
            ! Blank this row's slice of the pane first, for the same reason:
            ! the row has to be cleared, but only as far as the pane goes.
            call terminal_move_cursor(row, col0)
            call terminal_write(theme_sgr(THEME_EDITOR_BG))
            call terminal_write(repeat(' ', cwidth))
            call terminal_move_cursor(row, col0)
            call render_line_number(src_line, line_count, gutter)
            if (src_line <= line_count) then
                call render_line_with_selections(buffer, editor, src_line, &
                                                 editor%viewport_column, content_w)
            else
                call terminal_write(theme_sgr(THEME_EDITOR) // '~')
            end if
            call terminal_write(theme_reset())
        end do
    end subroutine render_ghost_block

    ! Line-number gutter for a pushed-down row. Matching what render_screen
    ! draws matters: a shifted row with a blank gutter reads as though the
    ! file has lost its numbering.
    subroutine render_line_number(buffer_line, line_count, gutter)
        integer, intent(in) :: buffer_line, line_count, gutter
        character(len=8) :: num_str

        if (gutter <= 0) return
        if (buffer_line <= line_count) then
            write(num_str, '(i5)') buffer_line
            call terminal_write(theme_paint(THEME_LINE_NUMBER, &
                                adjustl(num_str(1:LINE_NUMBER_WIDTH)) // ' '))
        else
            call terminal_write(theme_paint(THEME_LINE_NUMBER, repeat(' ', gutter)))
        end if
    end subroutine render_line_number

    ! When a block will not fit, show only its first line plus how much more
    ! Tab would bring, rather than truncating it mid-thought.
    subroutine render_block_overflow_marker(anchor_row, col0, cwidth, extra)
        integer, intent(in) :: anchor_row, col0, cwidth, extra
        character(len=32) :: marker
        integer :: surface_role

        if (extra < 1) return
        write(marker, '(a,i0,a)') ' +', extra, ' more (Tab)'
        if (col0 + cwidth - 1 - len_trim(marker) < col0) return
        surface_role = THEME_EDITOR_BG
        if (highlight_current_line) surface_role = THEME_CURRENT_LINE
        call terminal_move_cursor(anchor_row, col0 + cwidth - len_trim(marker))
        call terminal_write(theme_sgr(surface_role) // &
                            theme_foreground_sgr(THEME_HINT) // &
                            trim(marker) // theme_reset())
    end subroutine render_block_overflow_marker

    ! Last row the editor may draw content on: above the status bar, and above
    ! the terminal panel when it is up.
    function last_content_row(editor) result(row)
        type(editor_state_t), intent(in) :: editor
        integer :: row

        row = editor%screen_rows - 1
        if (is_terminal_panel_visible(editor%terminal_panel)) then
            row = row - get_terminal_panel_height(editor%terminal_panel)
        end if
    end function last_content_row

    ! Rows the document actually gets: the screen less the tab bar, the status
    ! bar, and the terminal panel while it is up. Every consumer must agree --
    ! viewport scrolling, caret placement and page size alike. When one of them
    ! counts the panel's rows as its own, the caret walks below the last drawn
    ! line and parks behind the panel, and paging overshoots by the panel
    ! height each press.
    function text_area_height(editor) result(h)
        type(editor_state_t), intent(in) :: editor
        integer :: h

        ! screen, less the status bar, less whatever the tab bar occupies.
        h = editor%screen_rows - 1 - tab_bar_height(editor)
        if (is_terminal_panel_visible(editor%terminal_panel)) then
            h = h - get_terminal_panel_height(editor%terminal_panel)
        end if
        h = max(1, h)
    end function text_area_height

    !> Rows the tab bar permanently occupies: 0 with no tabs, 1 normally.
    !>
    !> It was a literal 2 subtracted from the screen height in one place and a
    !> literal `start_row = 2` in twenty-odd others, with the no-tabs case
    !> handled in the coordinates but not in the height -- so with zero tabs
    !> the document drew one more row than the height math believed, leaving
    !> the last line permanently unreachable by paging.
    !>
    !> A second row for tab groups will report 2 here, which is why every
    !> consumer has to ask rather than assume.
    function tab_bar_height(editor) result(h)
        type(editor_state_t), intent(in) :: editor
        integer :: h

        h = 1
        if (size(editor%tabs) == 0) then
            h = 0
        else if (active_group_id(editor) /= 0) then
            ! Inside a group the member row is pinned, and the document
            ! reflows down to make room for it. The hover preview does NOT
            ! count here: it is drawn over the document precisely so that
            ! moving the pointer across the bar does not shift the text.
            h = 2
        end if
    end function tab_bar_height

    !> First screen row the document may draw on. The inverse of
    !> tab_bar_height, named separately because the ~25 sites that need it want
    !> a coordinate, not a count, and doing the arithmetic at each one is how
    !> the two drifted apart in the first place.
    function first_content_row(editor) result(r)
        type(editor_state_t), intent(in) :: editor
        integer :: r

        r = 1 + tab_bar_height(editor)
    end function first_content_row

    ! Cut text to at most `cells` display columns, only ever on a character
    ! boundary, and report the width actually used. Wide characters that would
    ! straddle the limit are dropped rather than half-drawn.
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
    ! the highlighter's lexical scan state is saved and restored so the
    ! frame-sequential state machine is untouched.
    subroutine render_line_tail_shifted(buffer_line, start_char, start_off, budget)
        character(len=*), intent(in) :: buffer_line
        integer, intent(in) :: start_char, start_off, budget
        type(token_t), allocatable :: tokens(:)
        logical :: saved_mc, saved_ms, saved_interp
        character(len=4) :: saved_delim
        character(len=:), allocatable :: ch, style, prev_style
        integer :: ci, off, w, byte_pos, ti, surface_role, saved_interp_depth

        if (budget < 1) return

        surface_role = THEME_EDITOR_BG
        if (highlight_current_line) surface_role = THEME_CURRENT_LINE

        saved_mc = syntax_highlighter%in_multiline_comment
        saved_ms = syntax_highlighter%in_multiline_string
        saved_delim = syntax_highlighter%string_delimiter
        saved_interp = syntax_highlighter%in_interp
        saved_interp_depth = syntax_highlighter%interp_depth
        syntax_highlighter%in_multiline_comment = .false.
        syntax_highlighter%in_multiline_string = .false.
        syntax_highlighter%in_interp = .false.
        syntax_highlighter%interp_depth = 0
        call tokenize_line(syntax_highlighter, buffer_line, tokens)
        syntax_highlighter%in_multiline_comment = saved_mc
        syntax_highlighter%in_multiline_string = saved_ms
        syntax_highlighter%string_delimiter = saved_delim
        syntax_highlighter%in_interp = saved_interp
        syntax_highlighter%interp_depth = saved_interp_depth

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
            if (len(style) > 0) then
                style = style // theme_background_sgr(surface_role)
            else
                style = theme_sgr(surface_role)
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
        screen_height = text_area_height(editor)
        pane_col = tree_offset + int(pane%x_start * real(editor_width))
        pane_width = int((pane%x_end - pane%x_start) * real(editor_width))
        if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
            pane_width = pane_width - 1
        end if
        pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
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

        row_offset = first_content_row(editor)
        min_row = row_offset

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

    !> Decide where each entry goes, without drawing anything.
    !>
    !> Separate from the drawing so it can be tested without a terminal, and
    !> so the click regions come from the same arithmetic as the pixels. The
    !> old bar computed the layout inline, in bytes, and simply stopped at the
    !> first label that did not fit -- so with a dozen tabs the active one
    !> could be entirely absent from the screen and unclickable, and one CJK
    !> filename shifted every click to its right.
    !>
    !> `scroll` is in/out: it is nudged until the active entry is visible, and
    !> the caller keeps it so a click on a chevron persists.
    subroutine strip_layout(entries, n_entries, width, active_idx, scroll, &
                            spans, n_spans, more_left, more_right, follow_active)
        type(strip_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: n_entries, width, active_idx
        integer, intent(inout) :: scroll
        type(strip_span_t), intent(out) :: spans(:)
        integer, intent(out) :: n_spans
        logical, intent(out) :: more_left, more_right
        !> Bring the active entry into view. Default .true., which is what a
        !> tab SWITCH wants. A caller that is merely redrawing must pass
        !> .false., or the bar cannot be scrolled by hand at all -- see below.
        logical, intent(in), optional :: follow_active
        integer :: i, col, avail, used, first, guard
        logical :: follow
        character(len=:), allocatable :: shown

        n_spans = 0
        more_left = .false.
        more_right = .false.
        if (n_entries < 1 .or. width < 1) return

        if (scroll < 1) scroll = 1
        if (scroll > n_entries) scroll = n_entries

        ! Bring the active entry into view. Scrolling left is immediate;
        ! scrolling right advances one entry at a time until it fits, with a
        ! guard so a width too small for any single entry cannot spin.
        !
        ! Only when asked. Doing it on EVERY layout means the bar cannot hold
        ! a position the user chose: clicking the left chevron scrolls one
        ! entry, and if that would push the active entry off the right the
        ! next redraw immediately puts it back -- so the chevron appears to do
        ! nothing unless the active tab happens to be the adjacent one. It is
        ! also what pins the bar at the far right after a tab opens there,
        ! with everything else behind a chevron that will not move.
        follow = .true.
        if (present(follow_active)) follow = follow_active

        if (follow .and. active_idx >= 1 .and. active_idx <= n_entries) then
            if (active_idx < scroll) scroll = active_idx
            guard = 0
            do while (.not. fits(scroll, active_idx) .and. scroll < active_idx &
                      .and. guard < n_entries)
                scroll = scroll + 1
                guard = guard + 1
            end do
        end if

        first = scroll
        more_left = (first > 1)

        ! The chevrons occupy cells, so reserve them before placing anything.
        avail = width
        if (more_left) avail = avail - 2          ! "< "
        if (avail < 1) return

        col = 1
        if (more_left) col = col + 2

        do i = first, n_entries
            call clip_to_cells(trim(entries(i)%label), MAX_ENTRY_CELLS, shown, used)
            ! Leave room for the right chevron unless this is the last entry.
            if (i < n_entries) then
                if (col - 1 + used > width - 3) then
                    more_right = .true.
                    exit
                end if
            else
                if (col - 1 + used > width) then
                    more_right = .true.
                    exit
                end if
            end if
            n_spans = n_spans + 1
            if (n_spans > size(spans)) then
                n_spans = n_spans - 1
                more_right = .true.
                exit
            end if
            spans(n_spans)%idx = i
            spans(n_spans)%col0 = col
            spans(n_spans)%col1 = col + used - 1
            col = col + used + 1                  ! one space between entries
        end do

        ! Nothing fitted at all: draw the first entry clipped rather than an
        ! empty bar, so the user is never looking at a blank strip.
        if (n_spans == 0 .and. first <= n_entries) then
            call clip_to_cells(trim(entries(first)%label), max(1, width - 3), shown, used)
            if (used > 0) then
                n_spans = 1
                spans(1)%idx = first
                spans(1)%col0 = 1
                spans(1)%col1 = used
                more_right = (first < n_entries)
            end if
        end if

    contains

        !> Would entries first..target fit in the width, allowing for chevrons?
        logical function fits(first_i, target)
            integer, intent(in) :: first_i, target
            integer :: k, c, u
            character(len=:), allocatable :: sh

            c = 1
            if (first_i > 1) c = c + 2
            fits = .false.
            do k = first_i, target
                call clip_to_cells(trim(entries(k)%label), MAX_ENTRY_CELLS, sh, u)
                if (c - 1 + u > width - 3) return
                c = c + u + 1
            end do
            fits = .true.
        end function fits

    end subroutine strip_layout

    subroutine render_tab_bar(editor, start_col, width)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in), optional :: start_col, width
        type(strip_entry_t) :: entries(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: spans(STRIP_MAX_ENTRIES)
        integer :: i, n_entries, n_spans, tab_count, active_entry
        integer :: start_column, max_width, col, used
        integer(int32) :: gid, show_gid
        integer(int32) :: seen_gids(STRIP_MAX_ENTRIES)
        integer :: n_seen
        logical :: more_left, more_right, cand
        logical :: compact_labels
        character(len=:), allocatable :: shown, base, marker
        character(len=16) :: more_lbl

        tab_count = size(editor%tabs)
        if (tab_count == 0) return
        n_seen = 0

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
        if (max_width < 1) return
        ! Groups collapse many tabs into one row-one entry. Base density on
        ! what the strip will actually draw, otherwise a bar with two groups
        ! and two loose tabs is needlessly reduced to bare numbers merely
        ! because the groups contain several members.
        n_entries = 0
        n_seen = 0
        do i = 1, tab_count
            gid = editor%tabs(i)%group_id
            if (gid == 0) then
                n_entries = n_entries + 1
            else if (.not. group_seen(gid, seen_gids, n_seen)) then
                n_seen = n_seen + 1
                seen_gids(n_seen) = gid
                n_entries = n_entries + 1
            end if
        end do
        compact_labels = max_width / max(1, n_entries) < 12

        ! Build row 1. A group occupies ONE entry, placed where its first
        ! member sits, and its members do not appear individually -- they get
        ! row 2. Ungrouped tabs appear as themselves.
        !
        ! Payload convention: a positive payload is a tab index, a negative one
        ! is -(group id), so the click router can tell them apart without a
        ! second region kind for the ungrouped case.
        g_slot_n(1) = 0
        n_entries = 0
        n_seen = 0
        active_entry = 0
        do i = 1, tab_count
            if (n_entries >= STRIP_MAX_ENTRIES) exit
            gid = editor%tabs(i)%group_id
            if (gid /= 0) then
                if (.not. group_seen(gid, seen_gids, n_seen)) then
                    n_seen = n_seen + 1
                    seen_gids(n_seen) = gid
                    n_entries = n_entries + 1
                    entries(n_entries)%label = ' ' // group_label(editor, gid) // ' '
                    entries(n_entries)%payload = -gid
                    entries(n_entries)%dim = .false.
                    if (gid == active_group_id(editor)) active_entry = n_entries
                end if
                cycle
            end if
            base = basename_of(editor%tabs(i)%filename)
            n_entries = n_entries + 1
            marker = ''
            if (editor%tabs(i)%modified) marker = theme_glyph('modified')
            ! Dense bars may reduce inactive tabs to their index, but the
            ! active document must remain identifiable at every width.
            if (compact_labels .and. i /= editor%active_tab_index) then
                write(entries(n_entries)%label, '(a,i0,a,a)') ' ', i, trim(marker), ' '
            else
                write(entries(n_entries)%label, '(a,i0,a,a,a,a)') ' ', i, ' ', &
                    trim(base), trim(marker), ' '
            end if
            entries(n_entries)%payload = i
            entries(n_entries)%dim = editor%tabs(i)%is_orphan
            entries(n_entries)%modified = editor%tabs(i)%modified
            if (i == editor%active_tab_index) active_entry = n_entries
        end do

        call note_hit_map(1, 1, entries, n_entries, max_width, active_entry, &
                          g_tab_scroll, start_column)

        ! Show the drag where it would land. The array is untouched -- only
        ! the entries about to be laid out are reordered -- so a drag that is
        ! abandoned costs exactly nothing to undo.
        call apply_drag_preview(entries, n_entries, 1, active_entry)

        call strip_layout(entries, n_entries, max_width, active_entry, &
                          g_tab_scroll, spans, n_spans, more_left, more_right, &
                          follow_active=(active_entry /= g_last_active(1) .or. &
                                         max_width /= g_last_width(1)))
        g_last_active(1) = active_entry
        g_last_width(1) = max_width

        ! Remember where the bar was drawn: with the tree open it does not
        ! start at column 1, and the hover preview must inherit that window
        ! rather than paint over the tree.
        g_tabbar_col0 = start_column
        g_tabbar_width = max_width

        call terminal_move_cursor(1, start_column)
        call terminal_write(theme_sgr(THEME_TAB_BAR) // repeat(' ', max_width) // theme_reset())

        if (more_left) then
            call terminal_move_cursor(1, start_column)
            call terminal_write(theme_paint(THEME_MUTED, theme_glyph('chevron_left')))
            call region_add(REGION_TAB_SCROLL, 1, 1, start_column, start_column, -1)
        end if

        do i = 1, n_spans
            associate(sp => spans(i))
                call clip_to_cells(trim(entries(sp%idx)%label), MAX_ENTRY_CELLS, &
                                   shown, used)
                call terminal_move_cursor(1, start_column + sp%col0 - 1)
                ! A group the pointer is resting on is lit up, and lit up
                ! IMMEDIATELY -- before its member row opens. That is the
                ! whole affordance: it says "let go here, or come down into
                ! it" at the moment the pointer arrives, rather than leaving
                ! the user to discover the row by waiting.
                cand = .false.
                if (drag_candidate_gid() /= 0 .and. entries(sp%idx)%payload < 0) &
                    cand = (int(-entries(sp%idx)%payload, int32) == drag_candidate_gid())
                if (cand) then
                    call terminal_write(theme_sgr(THEME_TAB_DRAG))
                else if (sp%idx == active_entry) then
                    call terminal_write(theme_sgr(THEME_TAB_ACTIVE))
                else if (entries(sp%idx)%dim) then
                    call terminal_write(theme_sgr(THEME_TAB_ORPHAN))
                else if (entries(sp%idx)%modified) then
                    call terminal_write(theme_sgr(THEME_TAB_MODIFIED))
                else
                    call terminal_write(theme_sgr(THEME_TAB_INACTIVE))
                end if
                call terminal_write(shown)
                call terminal_write(theme_reset())

                ! The span is in CELLS, from the same layout that drew it, so a
                ! multibyte filename no longer shifts every click to its right.
                call region_add(REGION_TAB, 1, 1, &
                                start_column + sp%col0 - 1, &
                                start_column + sp%col1 - 1, &
                                entries(sp%idx)%payload)
                call note_slot(1, 1, sp%idx, start_column + sp%col0 - 1, &
                               start_column + sp%col1 - 1)
            end associate
        end do

        ! Say how many are off the right edge rather than letting them vanish.
        if (more_right) then
            write(more_lbl, '(a,i0)') '>', n_entries - (spans(max(1, n_spans))%idx)
            col = start_column + max_width - len_trim(more_lbl)
            call terminal_move_cursor(1, col)
            call terminal_write(theme_paint(THEME_MUTED, trim(more_lbl)))
            call region_add(REGION_TAB_SCROLL, 1, 1, col, &
                            start_column + max_width - 1, 1)
        else
            if (n_spans > 0) then
                col = start_column + spans(n_spans)%col1 + 1
            else
                col = start_column
            end if
            if (col + 2 <= start_column + max_width - 1) then
                call terminal_move_cursor(1, col)
                call terminal_write(theme_paint(THEME_ACCENT, ' ' // theme_glyph('add') // ' '))
                call region_add(REGION_NEW_TAB, 1, 1, col, col + 2)
            end if
        end if

        ! Row 2: the active group's members, pinned while we are inside it.
        ! tab_bar_height already reserved the row, so the document starts below.
        !
        ! Except while a tab is being carried. Then the row belongs to the
        ! drag and shows whichever group the pointer is resting on, so a
        ! member can be taken out of one group and dropped into another in ONE
        ! movement -- hover the second group, its members appear here, drop.
        ! Without this, being inside a group meant this row was permanently
        ! the group you were leaving, and no other group could be reached.
        !
        ! The row is swapped, never removed: collapsing it mid-drag would
        ! reflow the whole document under the pointer.
        if (active_group_id(editor) /= 0) then
            show_gid = active_group_id(editor)
            if (drag_is_showing() .and. g_hover_group /= 0) show_gid = g_hover_group
            call render_group_row(editor, show_gid, 2, &
                                  start_column, max_width)
        end if
    end subroutine render_tab_bar


    !> Draw a group's members on row 2.
    !>
    !> Same layout engine as row 1, so the two rows cannot disagree about
    !> where a click landed -- which is the whole reason placement was split
    !> out of drawing. Members carry their global tab index as the payload, so
    !> the existing REGION_TAB case in the click router handles them unchanged.
    subroutine render_group_row(editor, gid, row, start_col, width)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        integer, intent(in) :: row, start_col, width
        type(strip_entry_t) :: entries(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: spans(STRIP_MAX_ENTRIES)
        integer, allocatable :: members(:)
        integer :: i, n_entries, n_spans, active_entry, col, used
        logical :: more_left, more_right
        character(len=:), allocatable :: shown, base, marker
        character(len=16) :: more_lbl

        if (gid == 0 .or. width < 1) return
        g_slot_n(2) = 0
        g_slot2_gid = gid
        call group_members(editor, gid, members)
        if (size(members) == 0) return

        n_entries = min(size(members), STRIP_MAX_ENTRIES)
        active_entry = 0
        do i = 1, n_entries
            base = basename_of(editor%tabs(members(i))%filename)
            marker = ''
            if (editor%tabs(members(i))%modified) marker = theme_glyph('modified')
            ! Member jumps use this ordinal after Alt lands on the group. Keep
            ! it visible so the second digit is a direct read, not a count.
            write(entries(i)%label, '(a,i0,a,a,a,a)') ' ', i, ' ', &
                trim(base), trim(marker), ' '
            entries(i)%payload = members(i)
            entries(i)%dim = editor%tabs(members(i))%is_orphan
            entries(i)%modified = editor%tabs(members(i))%modified
            if (members(i) == editor%active_tab_index) active_entry = i
        end do

        call note_hit_map(2, row, entries, n_entries, width, active_entry, &
                          g_group_scroll, start_col)

        ! Same preview on the member row: a tab carried in from elsewhere is
        ! inserted where it would land, and a member being moved within the
        ! group slides to its new place.
        call apply_drag_preview(entries, n_entries, 2, active_entry)

        ! A preview is the prospective order, so its numbers must be prospective
        ! too. The shuffled labels still carry their old ordinals, and an entry
        ! arriving from row one has none until this pass rebuilds them.
        do i = 1, n_entries
            if (entries(i)%payload < 1 .or. entries(i)%payload > size(editor%tabs)) cycle
            base = basename_of(editor%tabs(entries(i)%payload)%filename)
            marker = ''
            if (editor%tabs(entries(i)%payload)%modified) marker = theme_glyph('modified')
            write(entries(i)%label, '(a,i0,a,a,a,a)') ' ', i, ' ', &
                trim(base), trim(marker), ' '
        end do

        call strip_layout(entries, n_entries, width, active_entry, &
                          g_group_scroll, spans, n_spans, more_left, more_right, &
                          follow_active=(active_entry /= g_last_active(2) .or. &
                                         width /= g_last_width(2)))
        g_last_active(2) = active_entry
        g_last_width(2) = width

        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_TAB_BAR) // repeat(' ', width) // theme_reset())

        if (more_left) then
            call terminal_move_cursor(row, start_col)
            call terminal_write(theme_paint(THEME_MUTED, theme_glyph('chevron_left')))
            call region_add(REGION_TAB_SCROLL, row, row, start_col, start_col, -2)
        end if

        do i = 1, n_spans
            associate(sp => spans(i))
                call clip_to_cells(trim(entries(sp%idx)%label), MAX_ENTRY_CELLS, &
                                   shown, used)
                call terminal_move_cursor(row, start_col + sp%col0 - 1)
                if (sp%idx == active_entry) then
                    call terminal_write(theme_sgr(THEME_TAB_ACTIVE))
                else if (entries(sp%idx)%dim) then
                    call terminal_write(theme_sgr(THEME_TAB_ORPHAN))
                else if (entries(sp%idx)%modified) then
                    call terminal_write(theme_sgr(THEME_TAB_MODIFIED))
                else
                    call terminal_write(theme_sgr(THEME_TAB_INACTIVE))
                end if
                call terminal_write(shown)
                call terminal_write(theme_reset())
                call region_add(REGION_TAB, row, row, &
                                start_col + sp%col0 - 1, &
                                start_col + sp%col1 - 1, &
                                entries(sp%idx)%payload)
                call note_slot(2, row, sp%idx, start_col + sp%col0 - 1, &
                               start_col + sp%col1 - 1)
            end associate
        end do

        if (more_right) then
            write(more_lbl, '(a,i0)') '>', n_entries - spans(max(1, n_spans))%idx
            col = start_col + width - len_trim(more_lbl)
            call terminal_move_cursor(row, col)
            call terminal_write(theme_paint(THEME_MUTED, trim(more_lbl)))
            call region_add(REGION_TAB_SCROLL, row, row, col, &
                            start_col + width - 1, 2)
        end if
    end subroutine render_group_row

    !> Move a strip's scroll by one entry. Payload sign gives the direction,
    !> magnitude gives the row: 1 for the tab row, 2 for the member row.
    !> Reorder the entries about to be drawn so the held one appears where it
    !> would land, and mark it so it can be drawn hollow.
    !>
    !> An entry-array shuffle, never a tabs-array one: this is a preview of a
    !> move that has not happened and may never happen.
    subroutine apply_drag_preview(entries, n_entries, row, active_entry)
        type(strip_entry_t), intent(inout) :: entries(:)
        integer, intent(inout) :: n_entries
        integer, intent(in) :: row
        integer, intent(inout) :: active_entry
        type(strip_entry_t) :: held
        integer :: i, from, to

        if (.not. drag_is_showing()) return
        if (.not. drag_has_target()) return
        if (drag_to_row() /= row) return

        from = 0
        do i = 1, n_entries
            if (entries(i)%payload == drag_payload()) then
                from = i
                exit
            end if
        end do

        if (from == 0) then
            ! The held tab does not belong to this strip: it is arriving from
            ! somewhere else. Insert it where it would land, so a tab carried
            ! into a group's member row can be SEEN taking its place rather
            ! than being dropped blind.
            if (n_entries >= STRIP_MAX_ENTRIES) return
            to = max(1, min(drag_to_slot(), n_entries + 1))
            do i = n_entries, to, -1
                entries(i + 1) = entries(i)
            end do
            n_entries = n_entries + 1
            entries(to)%label = drag_label()
            entries(to)%payload = drag_payload()
            entries(to)%dim = .true.        ! it is not really there yet
            if (active_entry >= to) active_entry = active_entry + 1
            return
        end if

        to = max(1, min(drag_to_slot(), n_entries))
        if (to /= from) then
            held = entries(from)
            if (to > from) then
                do i = from, to - 1
                    entries(i) = entries(i + 1)
                end do
            else
                do i = from, to + 1, -1
                    entries(i) = entries(i - 1)
                end do
            end if
            entries(to) = held
            ! The highlight is a POSITION in this array, so it has to follow
            ! the shuffle or the wrong entry is drawn reversed.
            if (active_entry == from) then
                active_entry = to
            else if (to > from .and. active_entry > from .and. active_entry <= to) then
                active_entry = active_entry - 1
            else if (to < from .and. active_entry >= to .and. active_entry < from) then
                active_entry = active_entry + 1
            end if
        end if
    end subroutine apply_drag_preview

    !> Shade where a split would open, while a tab is held over an edge.
    !>
    !> A filled band rather than a line: it shows the SHAPE the new pane will
    !> take, so the drop is predictable before it happens. Drawn under the
    !> ghost, which stays the topmost thing on screen.
    subroutine render_split_preview(editor)
        type(editor_state_t), intent(in) :: editor
        integer :: r0, c0, r1, c1, r, w

        if (.not. drag_is_showing()) return
        if (drag_split_side() == SPLIT_NONE) return

        call drag_split_rect(r0, c0, r1, c1)
        if (r1 < r0 .or. c1 < c0) return
        r0 = max(1, r0)
        c0 = max(1, c0)
        r1 = min(int(editor%screen_rows), r1)
        c1 = min(int(editor%screen_cols), c1)
        w = c1 - c0 + 1
        if (w < 1) return

        do r = r0, r1
            call terminal_move_cursor(r, c0)
            ! A dim reverse block: visible over text without hiding which
            ! text it is about to sit next to.
            call terminal_write(theme_sgr(THEME_SELECTION_INACTIVE) // &
                                repeat(' ', w) // theme_reset())
        end do
    end subroutine render_split_preview

    !> The label under the pointer while a tab is being dragged.
    !>
    !> Drawn from the overlay pass so it sits above the bar, the preview strip
    !> and the document -- it is the thing the pointer is carrying, so nothing
    !> should cover it. Clipped to the screen rather than wrapped, which would
    !> smear it onto the row below.
    subroutine render_drag_ghost(editor)
        type(editor_state_t), intent(in) :: editor
        character(len=:), allocatable :: text
        integer :: r, c, w

        if (.not. drag_is_showing()) return
        text = trim(drag_label())
        if (len_trim(text) == 0) return

        r = drag_pointer_row()
        c = drag_pointer_col()
        if (r < 1 .or. r > editor%screen_rows) return

        ! Never over a tab strip. The bar already shows where the held tab
        ! will land -- that is what the reorder preview IS -- so a label drawn
        ! on top of it is redundant, and it overwrites the entries underneath,
        ! which reads as tabs rendering in pieces. It can also be painted past
        ! the end of the bar's own window, which the next frame's clear does
        ! not reach, leaving it stranded there.
        !
        ! Over the document it is the only thing saying what is being carried,
        ! so that is where it is drawn.
        if (r == g_slot_row(1) .or. r == g_slot_row(2)) return

        w = len(text)
        if (c + w - 1 > editor%screen_cols) c = editor%screen_cols - w + 1
        if (c < 1) then
            c = 1
            if (w > editor%screen_cols) text = text(1:editor%screen_cols)
        end if

        call terminal_move_cursor(r, c)
        ! Reverse video on a dim background: it reads as lifted off the bar
        ! rather than as another entry sitting on it.
        call terminal_write(theme_sgr(THEME_TAB_DRAG) // text // theme_reset())
    end subroutine render_drag_ghost

    !> Remember that entry `slot` of `strip` was drawn at these columns.
    subroutine note_slot(strip, screen_row, slot, c0, c1)
        integer, intent(in) :: strip, screen_row, slot, c0, c1

        if (strip < 1 .or. strip > 2) return
        if (g_slot_n(strip) >= STRIP_MAX_ENTRIES) return
        g_slot_n(strip) = g_slot_n(strip) + 1
        g_slot_c0(strip, g_slot_n(strip)) = c0
        g_slot_c1(strip, g_slot_n(strip)) = c1
        g_slot_row(strip) = screen_row
        ! The slot number is the entry index, which is what a drop target is
        ! expressed in -- not the count of things drawn, since scrolling means
        ! the first drawn entry is rarely the first entry.
        g_slot_c0(strip, g_slot_n(strip)) = c0
        g_slot_idx(strip, g_slot_n(strip)) = slot
    end subroutine note_slot

    !> Which entry slot is drawn at (row, col), or 0. `strip` comes back as 1
    !> for the tab bar and 2 for a group's member row.
    function tabbar_slot_at(row, col, strip) result(slot)
        integer, intent(in) :: row, col
        integer, intent(out) :: strip
        integer :: slot, k, i

        slot = 0
        strip = 0
        do k = 1, 2
            if (g_slot_row(k) /= row) cycle
            do i = 1, g_slot_n(k)
                if (col >= g_slot_c0(k, i) .and. col <= g_slot_c1(k, i)) then
                    slot = g_slot_idx(k, i)
                    strip = k
                    return
                end if
            end do
        end do
    end function tabbar_slot_at

    !> The highest entry slot currently drawn on `strip`, or 0.
    !>
    !> Dropping on the blank tail of a strip means "at the end", and the end
    !> is the last thing DRAWN -- not the last that exists, because anything
    !> scrolled off is not a place the pointer can be aimed at.
    !> Lay a strip out as if nothing were being dragged, and remember where
    !> each entry would sit. Purely arithmetic -- nothing is drawn.
    subroutine note_hit_map(strip, screen_row, entries, n_entries, width, &
                            active_idx, scroll, start_col)
        integer, intent(in) :: strip, screen_row, n_entries, width, active_idx
        integer, intent(in) :: scroll, start_col
        type(strip_entry_t), intent(in) :: entries(:)
        type(strip_span_t) :: spans(STRIP_MAX_ENTRIES)
        integer :: n_spans, i, sc
        logical :: ml, mr

        g_hit_n(strip) = 0
        g_hit_row(strip) = screen_row
        if (n_entries < 1) return

        ! A copy: strip_layout adjusts the scroll to keep the active entry in
        ! view, and this pass must not have an opinion about that.
        sc = scroll
        call strip_layout(entries, n_entries, width, active_idx, sc, spans, &
                          n_spans, ml, mr, follow_active=.false.)
        do i = 1, n_spans
            if (g_hit_n(strip) >= STRIP_MAX_ENTRIES) exit
            g_hit_n(strip) = g_hit_n(strip) + 1
            g_hit_c0(strip, g_hit_n(strip)) = start_col + spans(i)%col0 - 1
            g_hit_c1(strip, g_hit_n(strip)) = start_col + spans(i)%col1 - 1
            g_hit_slot(strip, g_hit_n(strip)) = spans(i)%idx
            g_hit_payload(strip, g_hit_n(strip)) = entries(spans(i)%idx)%payload
        end do
    end subroutine note_hit_map

    !> What the pointer is over, as if nothing were being dragged.
    !>
    !> Returns the strip (1 or 2), the entry slot, and that entry's payload.
    !> slot 0 means the cell is on a strip but not on any entry; strip 0 means
    !> it is not on a strip at all.
    subroutine tabbar_hit_at(row, col, strip, slot, payload)
        integer, intent(in) :: row, col
        integer, intent(out) :: strip, slot, payload
        integer :: k, i

        strip = 0
        slot = 0
        payload = 0
        do k = 1, 2
            if (g_hit_row(k) /= row .or. g_hit_n(k) == 0) cycle
            strip = k
            do i = 1, g_hit_n(k)
                if (col >= g_hit_c0(k, i) .and. col <= g_hit_c1(k, i)) then
                    slot = g_hit_slot(k, i)
                    payload = g_hit_payload(k, i)
                    return
                end if
            end do
            return
        end do
    end subroutine tabbar_hit_at

    integer function tabbar_last_slot(strip)
        integer, intent(in) :: strip
        integer :: i

        tabbar_last_slot = 0
        if (strip < 1 .or. strip > 2) return
        do i = 1, g_slot_n(strip)
            if (g_slot_idx(strip, i) > tabbar_last_slot) &
                tabbar_last_slot = g_slot_idx(strip, i)
        end do
    end function tabbar_last_slot

    integer(int32) function tabbar_strip2_gid()
        tabbar_strip2_gid = g_slot2_gid
    end function tabbar_strip2_gid

    !> The screen rows the two strips were last drawn on, 0 if not drawn.
    subroutine tabbar_strip_rows(row1, row2)
        integer, intent(out) :: row1, row2
        row1 = g_slot_row(1)
        row2 = g_slot_row(2)
    end subroutine tabbar_strip_rows

    subroutine nudge_tab_scroll(payload)
        integer, intent(in) :: payload

        select case (payload)
        case (-1)
            g_tab_scroll = max(1, g_tab_scroll - 1)
        case (1)
            g_tab_scroll = g_tab_scroll + 1
        case (-2)
            g_group_scroll = max(1, g_group_scroll - 1)
        case (2)
            g_group_scroll = g_group_scroll + 1
        end select
    end subroutine nudge_tab_scroll

    !> Has this group already been placed on row 1?
    logical function group_seen(gid, seen, n)
        integer(int32), intent(in) :: gid, seen(:)
        integer, intent(in) :: n
        integer :: k

        group_seen = .false.
        do k = 1, n
            if (seen(k) == gid) then
                group_seen = .true.
                return
            end if
        end do
    end function group_seen

    !> Last path component, or the whole string when there is no separator.
    function basename_of(path) result(base)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: base
        integer :: p

        p = index(path, '/', back=.true.)
        if (p > 0 .and. p < len(path)) then
            base = path(p+1:)
        else
            base = path
        end if
    end function basename_of


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

        call terminal_begin_sync()
        call terminal_hide_cursor()
        call regions_begin_frame()

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

        ! Keep the split below every occupied tab-strip row.
        call render_vertical_separator(separator_col, first_content_row(editor), &
                                       editor%screen_rows - 1)

        ! Render appropriate LSP panel on the right
        select case (panel_type)
        case ("references")
            if (is_references_panel_visible(editor%references_panel)) then
                call render_references_panel_at(editor%references_panel, panel_start_col, panel_width, &
                                                first_content_row(editor), editor%screen_rows - 1)
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
        call render_menu_overlay(editor)
        call terminal_end_sync()
        call terminal_flush()
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

        screen_height = text_area_height(editor)

        ! If only one pane, use simple rendering
        if (n_panes == 1) then
            if (allocated(editor%tabs(tab_idx)%panes(1)%filename)) &
                call name_surface(editor%tabs(tab_idx)%panes(1)%filename)
            call render_editor_pane(editor%tabs(tab_idx)%panes(1)%buffer, editor, start_col, width)
            return
        end if

        ! Multiple panes: render each with adjusted coordinates
        do i = first_content_row(editor), editor%screen_rows - 1
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
            pane_row = first_content_row(editor) + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            call store_pane_content_rect(editor, tab_idx, i, pane_col, pane_row, &
                                         pane_width, pane_height)

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
