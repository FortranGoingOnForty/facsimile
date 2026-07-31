module command_handler_module
    use iso_fortran_env, only: int32, int64, error_unit
    use iso_c_binding, only: c_int
    use editor_state_module, only: editor_state_t, cursor_t, switch_to_tab_with_buffer, &
                                   close_tab, create_tab, can_create_tab, save_tab_pane, active_pane_of, close_pane, &
        group_create, group_add_member, group_member_count, active_group_id, &
                                       split_pane_vertical, split_pane_horizontal, &
                                   navigate_to_pane_left, navigate_to_pane_right, navigate_to_pane_up, navigate_to_pane_down, &
                                   sync_editor_to_pane, tab_t
    use text_buffer_module
    use platform_module, only: canonical_path
    use tab_drag_module, only: drag_is_armed, drag_cancel, drag_is_showing
    use clickable_region_module, only: clickable_region_t, region_at, REGION_TAB_SCROLL, &
                                       REGION_TAB, REGION_BLOCK, REGION_FUSS_TOGGLE, &
                                       REGION_GP_ROW, REGION_GP_NAME, &
                                       REGION_TREE_ROW, REGION_CTX_ROW, REGION_NONE
    use group_picker_module
    use context_menu_module, only: context_menu_begin, context_menu_add_item, &
                                   context_menu_add_separator, context_menu_show, &
                                   context_menu_hide, is_context_menu_visible, &
                                   context_menu_handle_key, context_menu_selected, &
                                   context_menu_kind, context_menu_row_action, &
                                   context_menu_row_enabled, context_menu_take, &
                                   context_menu_hover, context_menu_select
    use platform_module, only: platform_sleep_ms
    use renderer_module, only: update_viewport, render_screen, render_screen_with_tree, tree_state, &
                               first_content_row, nudge_tab_scroll, tab_group_hover, &
                               tab_group_clear_hover, &
                               text_area_height, &
                               fuss_search_buffer, fuss_search_len, fuss_search_last_time, &
                               fuss_fuzzy_jump, fuss_reset_search, get_time_ms, &
                               fuss_git_prefix_active, &
                               display_offset_of, char_col_at_offset, &
                               set_status_message, clear_status_message
    use yank_stack_module
    use clipboard_module
    use help_display_module, only: show_help
    use goto_prompt_module, only: show_goto_prompt
    use replace_prompt_module, only: show_replace_prompt
    use unified_search_module, only: show_unified_search_prompt, current_search_pattern, &
                                      search_forward, search_backward, search_mode_active, &
                                      exit_search_mode
    use undo_stack_module
    use terminal_io_module, only: terminal_move_cursor, terminal_write, terminal_clear_screen, terminal_flush
    use terminal_panel_module, only: toggle_terminal_panel, &
        is_terminal_panel_visible, terminal_panel_handle_key, &
        terminal_panel_handle_mouse, terminal_panel_in_region, &
        terminal_panel_paste, terminal_panel_scroll, &
        get_terminal_panel_height, &
        terminal_panel_is_alive, terminal_panel_restart, &
        terminal_panel_is_dragging, terminal_panel_has_selection, &
        terminal_panel_copy_selection, &
        terminal_panel_nudge_height, terminal_panel_toggle_maximize, &
        terminal_panel_is_maximized, terminal_panel_set_height, permille_for
    use input_handler_module, only: get_paste_text, parse_mouse_event
    use bracket_matching_module, only: find_matching_bracket
    use comment_command_module, only: toggle_comment_lines, comment_syntax_available
    use utf8_module, only: utf8_char_to_byte_index, &
        utf8_byte_to_char_index, utf8_char_count, &
        utf8_is_valid_start, &
        utf8_char_col_to_utf16, utf16_to_utf8_char_col
    use file_tree_module
    use git_ops_module
    use text_prompt_module, only: show_text_prompt, show_yes_no_prompt
    use fortress_navigator_module, only: open_fortress_navigator
    use binary_prompt_module, only: binary_file_prompt
    use lsp_server_manager_module, only: request_completion, request_hover, request_definition, &
                                         request_references, request_code_actions, request_document_symbols, &
                                         request_signature_help, request_formatting, request_rename, &
                                         process_server_messages, filename_to_uri, &
                                         get_server_with_capability, notify_file_opened, &
                                         CAP_COMPLETION, CAP_DEFINITION, CAP_REFERENCES, CAP_RENAME, &
                                         CAP_CODE_ACTIONS, CAP_FORMATTING, CAP_HOVER, CAP_DOCUMENT_SYMBOLS
    use rename_prompt_module, only: show_rename_prompt
    use completion_popup_module, only: show_completion_popup, hide_completion_popup, &
                                        handle_completion_response, navigate_completion_up, &
                                        navigate_completion_down, get_selected_completion, &
                                        is_completion_visible
    use ai_engine_module, only: ai_configure, ai_note_trigger, ai_tick, ai_cancel, &
                               ai_is_enabled, ai_set_enabled, ai_status_line, &
                               ai_request_deep, ai_probe_backend
    use completion_context_module, only: context_line_after_cursor
    use ghost_text_module, only: ghost_clear, ghost_clear_pending, &
                                 ghost_get_prefix_at_cursor, ghost_get_include_prefix, &
                                 ghost_update_from_buffer, &
                                 ghost_apply_lsp_result, ghost_suffix, ghost_is_active, &
                                 ghost_extend_prefix, GHOST_SRC_LLM, &
                                 ghost_insert_text, ghost_is_block, ghost_take_word, &
                                 ghost_take_line, ghost_set_anchor
    use hover_tooltip_module, only: show_hover_tooltip, hide_hover_tooltip, &
                                     handle_hover_response, is_hover_visible
    use diagnostics_panel_module, only: toggle_panel => toggle_diagnostics_panel, &
                                        is_diagnostics_panel_visible, &
                                        diagnostics_panel_handle_key
    use references_panel_module, only: toggle_references_panel, &
                                      is_references_panel_visible, &
                                      references_panel_handle_key, &
                                      get_selected_reference_location, &
                                      hide_references_panel, &
                                      show_references_panel, &
                                      set_references, reference_location_t
    use code_actions_panel_module, only: code_actions_panel_t, init_code_actions_panel, &
                                        cleanup_code_actions_panel, show_code_actions_panel, &
                                        hide_code_actions_panel, is_code_actions_panel_visible, &
                                        code_actions_panel_handle_key, set_code_actions, &
                                        clear_code_actions, get_selected_action
    use symbols_panel_module, only: symbols_panel_t, document_symbol_t, &
                                    toggle_symbols_panel, is_symbols_panel_visible, &
                                    symbols_panel_handle_key, get_selected_symbol_location, &
                                    hide_symbols_panel, show_symbols_panel, &
                                    set_symbols, clear_symbols
    use signature_tooltip_module, only: signature_tooltip_t, show_signature_tooltip, &
                                        hide_signature_tooltip, is_signature_tooltip_visible, &
                                        handle_signature_response
    use jump_stack_module, only: push_jump_location, pop_jump_location, &
                                 is_jump_stack_empty
    use diagnostics_module, only: get_diagnostics_for_line, get_diagnostics_for_line_by_server, &
                                  diagnostics_to_json, diagnostic_t
    use json_module, only: json_value_t
    use lsp_server_installer_panel_module, only: show_lsp_server_installer_panel, &
                                                  hide_lsp_server_installer_panel, &
                                                  is_lsp_server_installer_panel_visible, &
                                                  lsp_server_installer_panel_handle_key, &
                                                  render_lsp_server_installer_panel, &
                                                  refresh_server_status
    implicit none
    private

    ! Context-menu kinds and row actions. The menu module treats these as
    ! opaque integers; the meaning lives here, next to the dispatch.
    integer, parameter :: CTX_KIND_DOC = 1, CTX_KIND_TREE = 2
    integer, parameter :: CTX_KIND_GROUP = 3, CTX_KIND_TAB = 4
    integer, parameter :: ACT_CUT = 1, ACT_COPY = 2, ACT_PASTE = 3
    integer, parameter :: ACT_COMMENT = 4, ACT_SELECT_ALL = 5
    integer, parameter :: ACT_GOTO_DEF = 6, ACT_FIND_REFS = 7, ACT_PALETTE = 8
    integer, parameter :: ACT_TREE_ACTIVATE = 20, ACT_TREE_VSPLIT = 21
    integer, parameter :: ACT_TREE_HSPLIT = 22, ACT_TREE_STAGE = 23
    integer, parameter :: ACT_TREE_UNSTAGE = 24, ACT_TREE_DIFF = 25
    integer, parameter :: ACT_GROUP_EDIT = 40, ACT_GROUP_RENAME = 41
    integer, parameter :: ACT_GROUP_DISSOLVE = 42
    integer, parameter :: ACT_TAB_CLOSE = 50, ACT_TAB_CLOSE_OTHERS = 51
    integer, parameter :: ACT_TAB_COPY_PATH = 52, ACT_TAB_UNGROUP = 53
    ! Matches GP_MAX_PICKED in group_picker_module: an edit can never name
    ! more files than the dialog could tick.
    integer, parameter :: GP_EDIT_MAX = 256
    ! How long the clicked row stays highlighted before the menu closes.
    ! Long enough to register, short enough not to feel like a stall.
    integer, parameter :: MENU_FLASH_MS = 70

    public :: handle_key_command, init_command_handler, cleanup_command_handler
    public :: save_initial_state_for_undo
    public :: search_pattern, match_case_sensitive  ! Exposed for status bar hint
    public :: g_lsp_modified_buffer  ! Flag for immediate render after LSP edits
    public :: g_lsp_ui_changed       ! Flag for immediate render after LSP UI changes
    public :: g_cursor_only_move     ! Flag for cursor-only moves (skip full re-render)
    public :: tab_jump_tick
    public :: g_no_visible_change
    public :: session_requests_tick

    ! Flag to track if LSP modified the buffer (for immediate rendering)
    logical :: g_lsp_modified_buffer = .false.
    ! Flag to track if LSP changed UI panels (for immediate rendering)
    logical :: g_lsp_ui_changed = .false.
    ! Flag for cursor-only movements (can skip full re-render)
    logical :: g_cursor_only_move = .false.
    ! This key changed nothing on screen, so the frame need not be drawn again.
    !
    ! Opt-in, and reset to .false. at the top of every key: only the paths that
    ! can prove nothing changed set it. Getting that backwards -- defaulting to
    ! "no change" and clearing it on mutation -- would mean every unconsidered
    ! code path silently suppressed its own repaint.
    logical :: g_no_visible_change = .false.

    ! A tab jump waiting to see whether another digit follows.
    !
    ! alt-N jumps at once and THEN arms this, rather than waiting to find out
    ! whether the number has more digits. Waiting would put half a second of
    ! lag on the overwhelmingly common single-digit case; superseding a jump
    ! that already happened costs nothing, because switching tabs is cheap and
    ! reversible.
    !
    ! 0 means nothing pending, which is why tab numbering starting at 1 is
    ! convenient here.
    ! The identifier a rename in flight is replacing, so every edit the server
    ! sends back can be checked against what is actually in the buffer before
    ! anything is written.
    character(len=:), allocatable :: g_rename_expect

    ! Which group the open context menu is about. Recorded when the menu is
    ! opened rather than re-derived when a row is chosen, because the tab bar
    ! can scroll underneath an open menu and the entry at those coordinates
    ! may be a different group by the time the row is clicked.
    integer(int32) :: g_menu_gid = 0

    ! Throttle for chevron auto-scroll while dragging.
    integer(int64) :: g_drag_scroll_at = 0
    integer, parameter :: DRAG_SCROLL_MS = 120
    ! Hovering a held tab over a group opens that group's member strip, so it
    ! can be dropped in. On a dwell, not immediately: dragging PAST a group on
    ! the way somewhere else must not make its members flash open.
    integer(int32) :: g_dwell_gid = 0
    integer(int64) :: g_dwell_since = 0
    integer, parameter :: DRAG_DWELL_MS = 400

    ! Which tab the open menu is about, by PATH. Not the index the region
    ! carried: the bar scrolls, tabs close, and every index renumbers when one
    ! does -- so an index recorded when the menu opened can name a different
    ! file by the time a row is clicked.
    character(len=:), allocatable :: g_menu_tab_path

    ! Which group the open dialog is EDITING. Zero means the dialog is
    ! creating a new one, so this single value is what tells the confirm path
    ! which of the two it is finishing. Cleared whenever the dialog closes,
    ! by either route, or a later create would silently edit this group.
    integer(int32) :: g_editing_gid = 0

    integer :: g_jump_value = 0
    integer(int64) :: g_jump_deadline = 0
    ! Non-zero when the tab the first digit landed on belongs to a group, in
    ! which case the NEXT digit picks a member of that group rather than
    ! extending the number. A group entry carries no number on the tab bar, so
    ! there is nothing for a digit to extend towards anyway.
    integer(int32) :: g_jump_group = 0
    integer, parameter :: JUMP_WINDOW_MS = 500

    type(yank_stack_t) :: yank_stack
    type(undo_stack_t) :: undo_stack
    character(len=:), allocatable :: search_pattern  ! For ctrl-d functionality
    logical :: match_case_sensitive = .true.  ! Case sensitivity for ctrl-d match mode
    logical :: last_action_was_edit = .false.
    ! Columns one indent level occupies (also the width a tab is counted as)
    integer, parameter :: ENTER_INDENT_WIDTH = 4

    ! Closers that auto-close inserted and the caret has not yet passed,
    ! innermost last. Typing a closer that matches the innermost pending one
    ! steps over it instead of leaving a duplicate behind -- the muscle-memory
    ! case where you type the ')' auto-close already put there. Only
    ! auto-inserted closers are tracked, so typing ')' in front of a ')' you
    ! wrote yourself still inserts a character.
    integer, parameter :: MAX_PENDING_CLOSERS = 32
    character :: g_pending_closers(MAX_PENDING_CLOSERS) = ' '
    integer :: g_pending_closer_count = 0

    ! Module-level storage for LSP callbacks
    type(editor_state_t), pointer, save :: saved_editor_for_callback => null()
    type(buffer_t), pointer, save :: saved_buffer_for_callback => null()


contains

    ! Helper to get a server index for a specific capability
    ! Keys that should ask the model for a suggestion. Wider than the LSP
    ! set: Enter especially, because finishing a comment and pressing Enter is
    ! precisely when the user wants the code written.
    pure function is_cursor_move_key(key_str) result(res)
        character(len=*), intent(in) :: key_str
        logical :: res

        select case(trim(key_str))
        case('up', 'down', 'left', 'right', 'home', 'end', 'pageup', 'pagedown', &
             'ctrl-left', 'ctrl-right', 'alt-left', 'alt-right', 'ctrl-home', &
             'ctrl-end', 'esc')
            res = .true.
        case default
            res = index(key_str, 'mouse-') == 1
        end select
    end function is_cursor_move_key

    function ai_trigger_key(key_str) result(res)
        character(len=*), intent(in) :: key_str
        logical :: res

        res = .false.
        if (trim(key_str) == 'enter') then
            res = .true.
        else if (trim(key_str) == 'backspace') then
            res = .true.
        else if (len_trim(key_str) == 1) then
            select case(key_str(1:1))
            case('(', ',', '{', ':', '.', '=', ' ')
                res = .true.
            case default
                res = is_word_char(key_str(1:1))
            end select
        end if
    end function ai_trigger_key

    ! The typed word before the caret, or '' on a fresh line. A block
    ! suggestion after Enter legitimately has no prefix at all.
    function ai_prefix_at(buffer, line_num, col) result(prefix)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        character(len=:), allocatable :: prefix

        call ghost_get_prefix_at_cursor(buffer, line_num, col, prefix)
        if (.not. allocated(prefix)) prefix = ''
    end function ai_prefix_at

    ! The file being typed into, which since 0.21.0 is a property of the
    ! active PANE -- two panes in one tab can hold different files, and
    ! editor%filename tracks the tab. Indent policy is per-file, so asking the
    ! tab would use a Makefile's rules in the pane beside it, or miss them.
    function active_pane_filename(editor) result(name)
        type(editor_state_t), intent(in) :: editor
        character(len=:), allocatable :: name
        integer :: t, p

        name = ''
        t = editor%active_tab_index
        if (t >= 1 .and. t <= size(editor%tabs)) then
            p = editor%tabs(t)%active_pane_index
            if (allocated(editor%tabs(t)%panes)) then
                if (p >= 1 .and. p <= size(editor%tabs(t)%panes)) then
                    if (allocated(editor%tabs(t)%panes(p)%filename)) then
                        name = editor%tabs(t)%panes(p)%filename
                        if (len_trim(name) > 0) return
                    end if
                end if
            end if
        end if
        if (allocated(editor%filename)) name = editor%filename
    end function active_pane_filename

    function ai_active_filename(editor) result(name)
        type(editor_state_t), intent(in) :: editor
        character(len=:), allocatable :: name

        name = ''
        if (allocated(editor%filename)) name = editor%filename
    end function ai_active_filename

    ! Why rename cannot run right now. "Nothing happened" is the least
    ! useful thing F2 can do, so name the actual obstacle.
    function rename_unavailable_reason(editor) result(msg)
        type(editor_state_t), intent(in) :: editor
        character(len=:), allocatable :: msg
        integer :: tab_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) then
            msg = '[F2] No file open'
        else if (editor%tabs(tab_idx)%num_lsp_servers < 1 .or. &
                 .not. allocated(editor%tabs(tab_idx)%lsp_server_indices)) then
            msg = '[F2] No language server running for this file ' // &
                  '(check it is installed and the file type is supported)'
        else
            msg = '[F2] The language server for this file does not support rename'
        end if
    end function rename_unavailable_reason

    function get_lsp_server_for_cap(editor, capability) result(server_idx)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: capability
        integer :: server_idx
        integer :: tab_idx

        server_idx = 0
        tab_idx = editor%active_tab_index

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (editor%tabs(tab_idx)%num_lsp_servers < 1) return
        if (.not. allocated(editor%tabs(tab_idx)%lsp_server_indices)) return

        server_idx = get_server_with_capability(editor%lsp_manager, &
            editor%tabs(tab_idx)%lsp_server_indices, &
            editor%tabs(tab_idx)%num_lsp_servers, &
            capability)
    end function get_lsp_server_for_cap

    subroutine init_command_handler()
        call init_yank_stack(yank_stack)
        call init_undo_stack(undo_stack)
        last_action_was_edit = .false.
        ! Pending closers belong to the buffer that was being typed into
        call clear_pending_closers()
    end subroutine init_command_handler

    subroutine save_initial_state_for_undo(buffer, editor)
        use undo_stack_module, only: save_initial_undo_state
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        call save_initial_undo_state(undo_stack, buffer, editor%cursors(editor%active_cursor))
    end subroutine save_initial_state_for_undo

    subroutine cleanup_command_handler()
        call cleanup_yank_stack(yank_stack)
        call cleanup_undo_stack(undo_stack)
        if (allocated(search_pattern)) deallocate(search_pattern)
    end subroutine cleanup_command_handler

    subroutine save_undo_state(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor

        ! Save current state to undo stack
        call push_undo_state(undo_stack, buffer, editor%cursors(editor%active_cursor))
    end subroutine save_undo_state

    subroutine handle_key_command(key_str, editor, buffer, should_quit)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout), target :: editor
        type(buffer_t), intent(inout), target :: buffer
        logical, intent(out) :: should_quit
        integer :: line_count, i, pane_idx
        logical :: is_edit_action
        ! Typing a plain character is the only thing that keeps a pending
        ! auto-closed closer live (see clear_pending_closers below)
        logical :: is_text_insert
        ! True when the keystroke matched the live suggestion and advanced it
        logical :: ghost_extended
        type(cursor_t), allocatable :: new_cursors(:)
        character(len=:), allocatable :: line

        should_quit = .false.
        line_count = buffer_get_line_count(buffer)
        is_edit_action = .false.
        is_text_insert = .false.
        ghost_extended = .false.
        ! A message stays up until the caret moves, rather than vanishing on
        ! the next keystroke. "AI completion: ready" is worth reading, and it
        ! was disappearing before the user had looked at it.
        if (is_cursor_move_key(key_str)) call clear_status_message()
        g_cursor_only_move = .false.
        g_no_visible_change = .false.

        ! Ignore empty key strings (from terminal position reports, etc)
        if (len_trim(key_str) == 0 .and. key_str(1:1) /= ' ') then
            return
        end if

        ! Reset search_pattern for any key except ctrl-d and alt-c
        ! This ensures ctrl-d always starts fresh when not in an active match sequence
        ! alt-c is preserved to allow toggling case sensitivity during match mode
        if (trim(key_str) /= 'ctrl-d' .and. trim(key_str) /= 'alt-c' .and. allocated(search_pattern)) then
            deallocate(search_pattern)
            match_case_sensitive = .true.  ! Reset to default
        end if

        ! Route mouse events to the terminal panel when visible: a
        ! click/drag inside the terminal region focuses it and drives
        ! text selection; a click above it returns focus to the editor.
        if (is_terminal_panel_visible(editor%terminal_panel) .and. &
            index(key_str, 'mouse-') == 1) then
            block
                character(len=16) :: ev
                integer :: btn, mrow, mcol
                logical :: ok, copied
                call parse_mouse_event(key_str, ev, btn, &
                    mrow, mcol, ok)
                ! Wheel events now carry coordinates and so parse here too,
                ! but the panel's scrollback handler below owns them; taking
                ! them here would silently disable panel scrolling.
                if (index(key_str, 'mouse-scroll-') == 1) ok = .false.
                if (ok) then
                    ! Dragging the separator bar resizes the panel. Claimed
                    ! before the selection handler below, which would otherwise
                    ! take it: the separator row counts as inside the panel,
                    ! and a press there maps to grid row -1, gets clamped to 0,
                    ! and silently anchors a text selection instead.
                    if (terminal_resize_drag(editor, trim(ev), btn, mrow)) return
                    ! A drag that began in the panel keeps its events even
                    ! once the pointer leaves -- dragging up past the top is
                    ! how you select the last few lines, and without this the
                    ! release never arrived and the copy never fired.
                    if (terminal_panel_in_region( &
                        editor%terminal_panel, mrow) .or. &
                        terminal_panel_is_dragging(editor%terminal_panel)) then
                        call terminal_panel_handle_mouse( &
                            editor%terminal_panel, trim(ev), &
                            btn, mrow, mcol, copied)
                        if (copied) call set_status_message( &
                            'Copied terminal selection')
                        return
                    else if (trim(ev) == 'mouse-click') then
                        ! Click in the editor area: hand focus back
                        editor%terminal_panel%focused = .false.
                        editor%terminal_panel%sel_active = .false.
                    end if
                end if
            end block
        end if

        ! Hand a click to whatever the renderer marked clickable at that cell.
        ! Deliberately ahead of fuss mode and the fall-through panels below:
        ! fuss mode swallows every unrecognised key, and the panels return
        ! "unhandled" for mouse events, so a region registered by either
        ! would never be reached from further down.
        ! Anything that opens the context menu. A PRESS with a modifier held
        ! arrives as mouse-ctrl:/mouse-shift:/mouse-alt: rather than
        ! mouse-click:, and matching only the latter lost it. Movement is
        ! classified before the modifiers, so none of these are motion events
        ! -- the bit-32 guard below is belt and braces.
        ! Ctrl+left-click opens the menu too, the macOS convention for a
        ! one-button pointer.
        if (index(key_str, 'mouse-click:') == 1 .or. &
            index(key_str, 'mouse-ctrl:') == 1 .or. &
            index(key_str, 'mouse-shift:') == 1 .or. &
            index(key_str, 'mouse-alt:') == 1) then
            block
                character(len=16) :: ev
                integer :: btn, mrow, mcol, base_button
                logical :: ok, wants_menu
                type(clickable_region_t) :: hit

                call parse_mouse_event(key_str, ev, btn, mrow, mcol, ok)
                if (ok) then
                    ! Low two bits are the button; the rest are modifiers.
                    base_button = iand(btn, 3)
                    wants_menu = .false.
                    if (base_button == 2) then
                        wants_menu = .true.              ! right button, any modifier
                    else if (base_button == 0) then
                        if (iand(btn, 16) /= 0) wants_menu = .true.   ! ctrl + left
                    end if
                    ! Motion carries bit 5, so a ctrl- or shift-drag arrives
                    ! here too and must not spawn a menu per motion event.
                    if (iand(btn, 32) /= 0) wants_menu = .false.
                    ! Alt+left stays the multi-cursor toggle.
                    if (btn == 8) wants_menu = .false.

                    if (wants_menu) then
                        ! A second right-click re-anchors an open menu rather
                        ! than stacking, as every other editor does.
                        if (is_context_menu_visible()) call context_menu_hide()
                        hit = region_at(mrow, mcol)
                        if (hit%kind == REGION_TREE_ROW) then
                            call open_tree_context_menu(editor, hit%payload, mrow, mcol)
                        else if (hit%kind == REGION_TAB .and. hit%payload > 0) then
                            ! An ordinary tab. Row 2's member entries carry
                            ! their global tab index too, so right-clicking a
                            ! group member gets the same menu -- which is what
                            ! you want, since a member is a tab.
                            call open_tab_context_menu(editor, hit%payload, &
                                                       mrow, mcol)
                        else if (hit%kind == REGION_TAB .and. hit%payload < 0) then
                            ! A GROUP entry: the payload is -(group id), the
                            ! same encoding the left-click path decodes to
                            ! enter the group. Ordinary tabs -- a positive
                            ! payload -- still get no menu; that is its own
                            ! task, and falling through here keeps this change
                            ! from touching them at all.
                            call open_group_context_menu(editor, &
                                int(-hit%payload, int32), mrow, mcol)
                        else if (hit%kind == REGION_NONE) then
                            call open_document_context_menu(editor, buffer, mrow, mcol)
                        end if
                        ! The chevron and panel blocks get no menu, and the
                        ! click is swallowed rather than acted on.
                        return
                    end if
                    if (btn == 0) then
                        ! A left click anywhere but on a row dismisses an open
                        ! menu and goes no further -- it must not also move the
                        ! caret to wherever the user aimed to close the box.
                        if (is_context_menu_visible()) then
                            hit = region_at(mrow, mcol)
                            if (hit%kind /= REGION_CTX_ROW) then
                                call context_menu_hide()
                                return
                            end if
                        end if
                        hit = region_at(mrow, mcol)
                        select case (hit%kind)
                        case (REGION_NONE)
                            ! With the tree open, a click in the document takes
                            ! focus. Fuss mode is modal for the keyboard, so
                            ! leaving it open with the caret in the document
                            ! would send the next keystroke to the tree. The
                            ! caret is placed before the toggle, while the
                            ! pane rect still describes the tree-mode layout
                            ! the click was aimed at.
                            if (editor%fuss_mode_active) then
                                if (cell_in_a_pane(editor, mrow, mcol)) then
                                    if (allocated(editor%cursors)) then
                                        if (size(editor%cursors) > 1) then
                                            deallocate(editor%cursors)
                                            allocate(editor%cursors(1))
                                            call init_cursor(editor%cursors(1))
                                            editor%active_cursor = 1
                                        end if
                                    end if
                                    call position_cursor_at_screen(editor%active_cursor, &
                                        editor, buffer, mrow, mcol)
                                    editor%cursors(editor%active_cursor)%has_selection = .false.
                                    call toggle_fuss_mode(editor)
                                    return
                                end if
                            end if
                        case (REGION_CTX_ROW)
                            call activate_context_menu_row(hit%payload, editor, &
                                                           buffer, should_quit)
                            return
                        case (REGION_TAB)
                            ! Arm only. Pressing a tab does NOT open it: a tab
                            ! can be picked up and carried somewhere without
                            ! ever being looked at, which is what makes moving
                            ! tabs around cheap and is what dropping one into
                            ! the document needs -- the split has to be beside
                            ! the document that is already there, not beside
                            ! the file being dropped.
                            !
                            ! The switch happens on RELEASE instead, and only
                            ! if the pointer never moved. A click is still a
                            ! click; see activate_tab_entry.
                            call arm_tab_drag(editor, hit%payload, mrow, mcol)
                            return
                        case (REGION_TAB_SCROLL)
                            call nudge_tab_scroll(hit%payload)
                            g_lsp_ui_changed = .true.
                            return
                        case (REGION_FUSS_TOGGLE)
                            call toggle_fuss_mode(editor)
                            return
                        case (REGION_TREE_ROW)
                            ! Select the clicked entry, then let the existing
                            ! keyboard handler act on it: space expands a
                            ! directory, enter opens a file. Delegating keeps
                            ! one definition of what activating a row means.
                            if (hit%payload >= 1 .and. &
                                hit%payload <= tree_state%n_selectable) then
                                tree_state%selected_index = hit%payload
                                if (tree_state%selectable_files(hit%payload)%is_directory) then
                                    call handle_fuss_input('space', editor, buffer)
                                else
                                    call handle_fuss_input('enter', editor, buffer)
                                end if
                            end if
                            return
                        case (REGION_GP_ROW, REGION_GP_NAME)
                            ! The group dialog. It registered these rows and
                            ! has resolved clicks on them since it was written,
                            ! but nothing ever routed one to it -- so the
                            ! dialog was keyboard-only. That was survivable
                            ! while the only way in was pressing Enter in the
                            ! tree; it is not now that a right-click on the tab
                            ! bar opens it, which would land a mouse user in a
                            ! box they cannot click.
                            if (group_picker_click(mrow, mcol)) then
                                if (group_picker_result() == GP_CONFIRMED) then
                                    call finish_group_creation(editor, buffer)
                                end if
                                g_lsp_ui_changed = .true.
                            end if
                            return
                        case (REGION_BLOCK)
                            ! A panel owns this cell. Swallow the click rather
                            ! than letting it move the caret underneath.
                            return
                        end select
                    end if
                end if
            end block
        end if

        ! A tab being carried owns the pointer. Ahead of the document drag
        ! handler, which would otherwise read the same motion as the start of
        ! a text selection, and ahead of the menu, which swallows releases.
        ! Guarded on a press having landed on the bar, so nothing here costs
        ! anything the rest of the time.
        if (drag_is_armed()) then
            if (index(key_str, 'mouse-drag:') == 1) then
                block
                    character(len=16) :: dev
                    integer :: dbtn, drow, dcol
                    logical :: dok

                    call parse_mouse_event(key_str, dev, dbtn, drow, dcol, dok)
                    ! Left button held. Mode 1002 reports motion whichever
                    ! button is down, and only the left one is carrying a tab.
                    if (dok .and. iand(dbtn, 3) == 0) then
                        call tab_drag_motion(editor, drow, dcol)
                        g_lsp_ui_changed = .true.
                        return
                    end if
                end block
            end if
            if (index(key_str, 'mouse-release:') == 1) then
                call tab_drag_release(editor, buffer)
                return
            end if
            ! Anything else -- a key, another button -- abandons the drag
            ! rather than leaving it half-held while the user does something
            ! unrelated.
            if (index(key_str, 'mouse-') /= 1) then
                call drag_cancel()
            end if
        end if

        ! The context menu is topmost, so its keys are taken before the
        ! terminal panel's and long before fuss mode's.
        if (is_context_menu_visible()) then
            ! ctrl-q must pass through. A surface that can swallow it forever
            ! is exactly the trap integration_ctrlq.py exists to catch; the
            ! cascade closes the menu instead.
            if (trim(key_str) /= 'ctrl-q') then
                if (context_menu_handle_key(trim(key_str))) return
                if (trim(key_str) == 'enter') then
                    call activate_context_menu_row(context_menu_selected(), &
                                                   editor, buffer, should_quit)
                    return
                end if
                ! A release is swallowed without dismissing. The right-press
                ! that opens the menu is followed by its own release in the
                ! same coalesced burst, with no render in between, so
                ! treating a release as "some other key" closed the menu on
                ! the very frame it opened.
                if (index(key_str, 'mouse-release:') == 1) return

                ! Bare pointer motion moves the highlight, so the mouse and
                ! the arrow keys drive the same selection. Mode 1003 reports
                ! motion with no button as button 3, which arrives here as a
                ! drag; a real drag (a button held) still dismisses.
                if (index(key_str, 'mouse-drag:') == 1) then
                    block
                        character(len=16) :: mev
                        integer :: mbtn, mr, mc
                        logical :: mok

                        call parse_mouse_event(key_str, mev, mbtn, mr, mc, mok)
                        if (mok) then
                            if (iand(mbtn, 3) == 3) then
                                ! Redraw only when the highlight actually
                                ! moved, not on every motion event.
                                if (context_menu_hover(mr, mc)) then
                                    g_lsp_ui_changed = .true.
                                else
                                    ! The pointer moved within the same row.
                                    ! Nothing on screen differs, so there is
                                    ! nothing to draw.
                                    g_no_visible_change = .true.
                                end if
                                return
                            end if
                        end if
                    end block
                    return
                end if
                ! Anything else dismisses, including a wheel tick, which would
                ! otherwise slide the document out from under the box.
                call context_menu_hide()
                return
            end if
        end if

        ! Bare pointer motion over the tab bar: preview a group without
        ! entering it. Placed after the context menu's own motion branch, so an
        ! open menu keeps owning motion, and before the drag handler, which
        ! drops non-left buttons.
        if (index(key_str, 'mouse-drag:') == 1) then
            block
                character(len=16) :: mev
                integer :: mb, mr, mc
                logical :: mok
                call parse_mouse_event(trim(key_str), mev, mb, mr, mc, mok)
                if (mok) then
                    if (iand(mb, 3) == 3) then      ! motion with no button held
                        if (tab_group_hover(mr, mc)) then
                            g_lsp_ui_changed = .true.
                        else
                            g_no_visible_change = .true.
                        end if
                        return
                    end if
                end if
            end block
        end if

        ! Any other key clears the preview. The pointer can leave the terminal
        ! entirely without a final motion event, which would otherwise strand
        ! the overlay on screen.
        if (index(key_str, 'mouse-') /= 1) then
            if (tab_group_clear_hover()) g_lsp_ui_changed = .true.
        end if

        ! Route keys to integrated terminal when focused (highest priority)
        if (is_terminal_panel_visible(editor%terminal_panel) .and. &
            editor%terminal_panel%focused) then
            if (trim(key_str) == 'f5' .or. trim(key_str) == 'alt-t') then
                ! F5 / Alt-T hides terminal panel
                editor%terminal_panel%visible = .false.
                editor%terminal_panel%focused = .false.
                ! Buffered clear so next render has no stale content
                call terminal_write(achar(27) // '[2J')
                return
            end if
            if (trim(key_str) == 'paste') then
                ! Send pasted text to the shell as one chunk
                call terminal_panel_paste(editor%terminal_panel, &
                    get_paste_text())
                return
            end if
            ! Ctrl-C is SIGINT to the shell and must stay that way, so the
            ! terminal convention of ctrl-shift-c/v is used for the clipboard.
            ! Without these there was no keyboard route to the terminal's text
            ! at all -- copy happened only as a side effect of a mouse release.
            if (trim(key_str) == 'ctrl-shift-c') then
                block
                    integer :: n_copied
                    character(len=16) :: count_buf
                    call terminal_panel_copy_selection(editor%terminal_panel, &
                                                       n_copied)
                    if (n_copied > 0) then
                        write(count_buf, '(i0)') n_copied
                        call set_status_message('Copied ' // trim(count_buf) // &
                                                ' characters from the terminal')
                    else
                        call set_status_message('Nothing selected in the terminal')
                    end if
                end block
                return
            end if
            if (trim(key_str) == 'ctrl-shift-v') then
                call terminal_panel_paste(editor%terminal_panel, &
                                          paste_from_clipboard())
                return
            end if
            ! Resizing the panel. These are only bound while the panel has
            ! focus, where their editor meaning -- navigate to the pane above
            ! or below -- cannot apply anyway: the document is not what the
            ! keyboard is pointed at. Doing it this way costs no new chord,
            ! and every one- and two-modifier arrow combination is already
            ! spoken for.
            !
            ! They have to be caught HERE. Anything that reaches
            ! terminal_panel_handle_key below is forwarded to the shell, and
            ! the tail of this block deliberately swallows what it does not
            ! recognise -- so a chord handled in the main dispatch instead
            ! would fail silently, or worse, arrive as text at a prompt.
            if (trim(key_str) == 'ctrl-shift-up' .or. &
                trim(key_str) == 'ctrl-shift-down' .or. &
                trim(key_str) == 'ctrl-shift-m') then
                call resize_terminal_panel_key(editor, trim(key_str))
                return
            end if
            if (index(key_str, 'mouse-scroll-up') == 1) then
                call terminal_panel_scroll(editor%terminal_panel, 3)
                return
            end if
            if (index(key_str, 'mouse-scroll-down') == 1) then
                call terminal_panel_scroll(editor%terminal_panel, -3)
                return
            end if
            ! A dead shell must not turn the panel into a passthrough.
            ! `exit`, Ctrl-D or an idle timeout (bash TMOUT) kills the child
            ! while the panel stays visible and focused, and every keystroke
            ! after that was landing in the document -- silently editing the
            ! file the user thought they were typing a shell command next to.
            if (.not. terminal_panel_is_alive(editor%terminal_panel)) then
                if (trim(key_str) == 'enter') then
                    call terminal_panel_restart(editor%terminal_panel, &
                        editor%screen_rows, editor%screen_cols)
                else if (trim(key_str) == 'esc') then
                    editor%terminal_panel%visible = .false.
                    editor%terminal_panel%focused = .false.
                    call terminal_write(achar(27) // '[2J')
                end if
                return
            end if

            if (terminal_panel_handle_key(editor%terminal_panel, &
                                          trim(key_str))) then
                return
            end if

            ! The panel did not recognise the key. It must still not reach
            ! the editor: an unmapped chord like ctrl-shift-k would run its
            ! editor command against the document while the user is looking
            ! at a shell prompt. Ctrl-Q stays live as the way out.
            if (trim(key_str) /= 'ctrl-q') return
        end if

        ! The group dialog owns the keyboard while it is up. ctrl-q passes
        ! through, so no modal can trap the user.
        if (is_group_picker_visible()) then
            if (trim(key_str) /= 'ctrl-q') then
                ! NOT trim(): trim(' ') is empty, and space is the tick key.
                ! Trimming here is what stopped the command palette being able
                ! to type a space.
                if (group_picker_handle_key(key_str)) then
                    if (group_picker_result() == GP_CONFIRMED) then
                        call finish_group_creation(editor, buffer)
                    else if (group_picker_result() == GP_CANCELLED) then
                        call group_picker_hide()
                        ! Or the next NEW group would silently edit this one.
                        g_editing_gid = 0
                    end if
                    g_lsp_ui_changed = .true.
                    return
                end if
            end if
        end if

        ! Route input when in fuss mode (except keys that work in both modes)
        if (editor%fuss_mode_active .and. trim(key_str) /= 'ctrl-b' .and. &
            trim(key_str) /= 'ctrl-shift-b' .and. trim(key_str) /= 'f2' .and. &
            trim(key_str) /= 'f3' .and. trim(key_str) /= 'f4' .and. &
            trim(key_str) /= 'f5' .and. &
            trim(key_str) /= 'f6' .and. trim(key_str) /= 'f8' .and. &
            trim(key_str) /= 'f12' .and. trim(key_str) /= 'shift-f12' .and. &
            trim(key_str) /= 'alt-g' .and. trim(key_str) /= 'alt-o' .and. &
            trim(key_str) /= 'alt-p' .and. trim(key_str) /= 'alt-e' .and. &
            trim(key_str) /= 'alt-r' .and. trim(key_str) /= 'alt-t' .and. &
            trim(key_str) /= 'ctrl-\\' .and. trim(key_str) /= 'ctrl-q') then
            call handle_fuss_input(key_str, editor, buffer)
            return
        end if

        ! Route keys to diagnostics panel when visible (j/k/arrows for navigation)
        if (is_diagnostics_panel_visible(editor%diagnostics_panel)) then
            if (diagnostics_panel_handle_key(editor%diagnostics_panel, trim(key_str))) then
                return
            end if
        end if

        ! Route keys to code actions panel when visible
        if (is_code_actions_panel_visible(editor%code_actions_panel)) then
            if (code_actions_panel_handle_key(editor%code_actions_panel, trim(key_str))) then
                ! For Enter, we need to apply the code action here since panel just returns handled=true
                if (trim(key_str) == 'enter') then
                    call apply_selected_code_action(editor, buffer)
                end if
                return
            end if
        end if

        ! Route keys to references panel when visible
        if (is_references_panel_visible(editor%references_panel)) then
            ! Handle Enter specially - jump to reference location
            if (trim(key_str) == 'enter') then
                block
                    use iso_fortran_env, only: int32
                    use editor_state_module, only: switch_to_tab_with_buffer
                    character(len=:), allocatable :: uri, ref_path
                    integer(int32) :: ref_line, ref_col, ti

                    if (get_selected_reference_location(editor%references_panel, uri, ref_line, ref_col)) then
                        if (len(uri) >= 7 .and. uri(1:7) == "file://") then
                            ref_path = uri(8:)

                            if (allocated(editor%filename) .and. &
                                trim(ref_path) == trim(editor%filename)) then
                                ! Same file
                                editor%cursors(editor%active_cursor)%line = ref_line
                            else
                                ! Find open tab or open new
                                do ti = 1, size(editor%tabs)
                                    if (trim(editor%tabs(ti)%filename) == trim(ref_path)) then
                                        call switch_to_tab_with_buffer(editor, ti, buffer)
                                        exit
                                    end if
                                end do
                                if (.not. allocated(editor%filename) .or. &
                                    trim(editor%filename) /= trim(ref_path)) then
                                    call open_file_in_editor(ref_path, editor, buffer)
                                end if
                                editor%cursors(editor%active_cursor)%line = ref_line
                            end if
                            ! Stored column is LSP UTF-16 units + 1
                            editor%cursors(editor%active_cursor)%column = &
                                char_col_from_lsp(buffer, ref_line, ref_col - 1)
                            editor%cursors(editor%active_cursor)%desired_column = &
                                editor%cursors(editor%active_cursor)%column
                            editor%cursors(editor%active_cursor)%has_selection = .false.
                            editor%viewport_line = max(1, ref_line - editor%screen_rows / 2)
                            call hide_references_panel(editor%references_panel)
                        end if
                    end if
                end block
                call sync_editor_to_pane(editor)
                return
            end if
            if (references_panel_handle_key(editor%references_panel, trim(key_str))) then
                return
            end if
        end if

        ! Route keys to symbols panel when visible
        if (is_symbols_panel_visible(editor%symbols_panel)) then
            ! Handle Enter specially - jump to symbol location
            if (trim(key_str) == 'enter') then
                block
                    use iso_fortran_env, only: int32
                    integer(int32) :: sym_line, sym_col
                    if (get_selected_symbol_location(editor%symbols_panel, sym_line, sym_col)) then
                        ! Jump to the symbol location (stored column is
                        ! LSP UTF-16 units + 1)
                        editor%cursors(editor%active_cursor)%line = sym_line
                        editor%cursors(editor%active_cursor)%column = &
                            char_col_from_lsp(buffer, sym_line, sym_col - 1)
                        ! Center the view on the target line
                        editor%viewport_line = max(1, sym_line - editor%screen_rows / 2)
                        ! Hide the panel after jumping
                        call hide_symbols_panel(editor%symbols_panel)
                    end if
                end block
                return
            end if
            if (symbols_panel_handle_key(editor%symbols_panel, trim(key_str))) then
                return
            end if
        end if

        ! Route keys to LSP server installer panel when visible
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
            if (lsp_server_installer_panel_handle_key(editor%lsp_installer_panel, trim(key_str))) then
                call render_lsp_server_installer_panel(editor%lsp_installer_panel, &
                    editor%screen_cols)
                return
            end if
            ! The panel did not recognise the key, and it must still not reach
            ! the document. This panel claims j, k, enter, c, r and q and let
            ! everything else through, so typing at it edited the file behind
            ! it -- invisibly, since the panel covers the text. On first run it
            ! is the first thing a user ever sees, so anything typed then was
            ! certainly aimed at the panel.
            !
            ! Found by a CI failure that looked like a dropped keystroke:
            ! 'Hello World' arrived as 'Hello Wold' because the panel took the
            ! r as its refresh command. The same guard is on the terminal
            ! panel, for the same reason. Ctrl-Q stays live as the way out.
            if (trim(key_str) /= 'ctrl-q') return
        end if

        ! Ghost text: tab accepts the shadow suggestion; right accepts it
        ! only at end of line (mid-line, right must keep meaning "move over
        ! the next real character"). Any other key clears it before normal
        ! dispatch (recomputed in the edit tail below). Placed after all
        ! panel routing so panels keep key priority.
        if (ghost_is_active(editor%ghost) .and. &
            .not. is_completion_visible(editor%completion_popup) .and. &
            size(editor%cursors) == 1 .and. &
            .not. editor%cursors(editor%active_cursor)%has_selection) then
            if (trim(key_str) == 'tab') then
                call accept_ghost_suggestion(editor, buffer)
                return
            end if
            if (trim(key_str) == 'right') then
                if (editor%cursors(editor%active_cursor)%column > &
                    buffer_get_line_char_count(buffer, &
                        editor%cursors(editor%active_cursor)%line)) then
                    call accept_ghost_suggestion(editor, buffer)
                    return
                end if
            end if
            ! Partial accept. Both turn a suggestion that is mostly right into
            ! a partial win instead of an all-or-nothing choice.
            !
            ! These shadow word-move-right while a suggestion is showing, the
            ! way Copilot does. The cost is small and different for each:
            ! ctrl-right shadows it anywhere a suggestion is up, alt-right only
            ! where a BLOCK is up -- and blocks are offered only at end of
            ! line, where moving a word right does nothing anyway. alt-f is
            ! untouched and still moves by word in both cases.
            if (trim(key_str) == 'ctrl-right') then
                call accept_ghost_word(editor, buffer)
                return
            end if
            if (trim(key_str) == 'alt-right' .and. ghost_is_block(editor%ghost)) then
                call accept_ghost_line(editor, buffer)
                return
            end if

            ! Typing the character the suggestion already predicted advances
            ! it in place instead of discarding it. A round trip is ~300ms;
            ! not making one at all while the user types into a correct
            ! suggestion is what makes this feel immediate.
            if (len_trim(key_str) == 1 .and. iachar(key_str(1:1)) >= 32) then
                ghost_extended = ghost_extend_prefix(editor%ghost, key_str(1:1))
            end if
        end if
        if (.not. ghost_extended) call ghost_clear(editor%ghost)

        ! A bare digit right after alt-N continues that jump rather than being
        ! typed. Checked here, after every panel and prompt has had its chance
        ! to return, so a digit meant for the palette or the search box still
        ! reaches it.
        if (g_jump_value > 0) then
            if (continue_tab_jump(editor, buffer, key_str)) return
        end if

        ! A move without Shift ends the selection, and lands on the end it was
        ! moving towards.
        block
            logical :: jumped
            call collapse_selection_for_move(editor, trim(key_str), jumped)
            if (jumped) then
                call sync_editor_to_pane(editor)
                call update_viewport(editor)
                ! NOT a cursor-only move. The fast path repaints the caret's
                ! line and nothing else, which left the rows that had been
                ! selected still drawn as selected -- the caret went home and
                ! the highlight stayed behind it.
                return
            end if
        end block

        select case(trim(key_str))
        ! File operations
        case('ctrl-q')
            ! Close whatever is open on top of the document first, and only
            ! quit once nothing is. Getting out of the editor is then always
            ! the same key, pressed until there is nothing left to close.
            block
                logical :: closed

                call close_topmost_surface(editor, closed)
                if (closed) then
                    g_lsp_ui_changed = .true.
                    return
                end if
            end block
            should_quit = .true.

        case('ctrl-b', 'ctrl-shift-b', 'f3')
            ! Toggle fuss mode (file tree)
            ! ctrl-b: Original binding (conflicts with tmux prefix)
            ! ctrl-shift-b: Alternative (tmux may still catch this)
            ! f3: Tmux/terminal-safe alternative (recommended)
            call toggle_fuss_mode(editor)

        case('ctrl-o')
            ! Open fortress navigator (file/directory picker)
            call handle_fortress_navigator(editor, buffer)

        case('esc')
            ! If completion popup is visible, hide it
            if (is_completion_visible(editor%completion_popup)) then
                call hide_completion_popup(editor%completion_popup)
                return
            end if

            ! If hover tooltip is visible, hide it
            if (is_hover_visible(editor%hover_tooltip)) then
                call hide_hover_tooltip(editor%hover_tooltip)
                return
            end if

            ! Other panels (diagnostics, code_actions, references, symbols) are handled in early routing

            ! Dismiss search mode: stops match highlighting and releases
            ! n/N back to normal typing (the pattern is kept for reuse)
            call exit_search_mode()

            ! ESC - Clear selections and return to single cursor mode
            if (size(editor%cursors) > 1) then
                ! Keep only the active cursor
                allocate(new_cursors(1))
                new_cursors(1) = editor%cursors(editor%active_cursor)
                new_cursors(1)%has_selection = .false.
                deallocate(editor%cursors)
                editor%cursors = new_cursors
                editor%active_cursor = 1
            else
                ! Single cursor - just clear selection
                editor%cursors(editor%active_cursor)%has_selection = .false.
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('ctrl-shift-/', 'ctrl-?', 'f1')
            ! Show help menu. ctrl-/ now toggles comments (VSCode parity), so
            ! help moved up to ctrl-shift-/ (advertised as ctrl-?). Telling
            ! those two apart needs the kitty keyboard protocol, which
            ! terminal_init negotiates; f1 is the fallback for terminals that
            ! decline it and collapse both chords onto the same byte.
            call show_help(editor)
            ! Screen will be redrawn automatically by main loop

        case('ctrl-/')
            ! Toggle line comment over every cursor's lines (VSCode ctrl-/)
            block
                logical :: comment_changed
                character(len=:), allocatable :: comment_file

                if (allocated(editor%filename)) then
                    comment_file = editor%filename
                else
                    comment_file = ''
                end if
                ! Check first so an unknown language does not push an
                ! identical state onto the undo stack for nothing
                if (comment_syntax_available(comment_file)) then
                    if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                    call toggle_comment_lines(buffer, editor%cursors, comment_file, &
                                              comment_changed)
                    if (comment_changed) then
                        call sync_editor_to_pane(editor)
                        call update_viewport(editor)
                        is_edit_action = .true.
                    end if
                end if
            end block

        case('ctrl-g')
            ! Go to line:column
            call show_goto_prompt(editor, buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('ctrl-l')
            ! Clear and redraw screen
            call terminal_clear_screen()
            ! Screen will be redrawn automatically by main loop

        ! Undo/Redo
        case('ctrl-z')
            ! Undo
            if (can_undo(undo_stack)) then
                call perform_undo(undo_stack, buffer, editor%cursors(editor%active_cursor))
                ! Sync even for single cursor case since undo changes cursor position
                call sync_editor_to_pane(editor)
                ! If we have multiple cursors, reset to single cursor mode
                ! (Undo only tracks one cursor's state)
                if (size(editor%cursors) > 1) then
                    allocate(new_cursors(1))
                    new_cursors(1) = editor%cursors(editor%active_cursor)
                    ! Clamp cursor to actual line length (chars) after undo
                    line = buffer_get_line(buffer, new_cursors(1)%line)
                    if (new_cursors(1)%column > utf8_char_count(line) + 1) then
                        new_cursors(1)%column = utf8_char_count(line) + 1
                    end if
                    new_cursors(1)%desired_column = new_cursors(1)%column
                    if (allocated(line)) deallocate(line)
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = 1
                end if
                call sync_editor_to_pane(editor)
                call update_viewport(editor)
            end if

        case('ctrl-shift-z', 'ctrl-]')
            ! Redo
            ! ctrl-shift-z: Standard redo (WezTerm may intercept - disable in config)
            ! ctrl-]: Alternative redo binding
            if (can_redo(undo_stack)) then
                call perform_redo(undo_stack, buffer, editor%cursors(editor%active_cursor))
                ! Sync even for single cursor case since redo changes cursor position
                call sync_editor_to_pane(editor)
                ! If we have multiple cursors, reset to single cursor mode
                ! (Undo only tracks one cursor's state)
                if (size(editor%cursors) > 1) then
                    allocate(new_cursors(1))
                    new_cursors(1) = editor%cursors(editor%active_cursor)
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = 1
                end if
                call sync_editor_to_pane(editor)
                call update_viewport(editor)
            end if

        case('ctrl-y')
            ! Yank (paste from yank stack)
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply yank to all cursors
                do i = 1, size(editor%cursors)
                    call yank_text(editor%cursors(i), buffer)
                end do
            else
                call yank_text(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            is_edit_action = .true.

        ! Navigation
        case('up')
            ! If completion popup is visible, navigate it instead
            if (is_completion_visible(editor%completion_popup)) then
                call navigate_completion_up(editor%completion_popup)
                return
            end if

            ! Other panels (diagnostics, code_actions, references, symbols) are handled in early routing

            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_up(editor%cursors(i), buffer)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_up(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            g_cursor_only_move = .true.

        case('down')
            ! If completion popup is visible, navigate it instead
            if (is_completion_visible(editor%completion_popup)) then
                call navigate_completion_down(editor%completion_popup)
                return
            end if

            ! Other panels (diagnostics, code_actions, references, symbols) are handled in early routing

            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_down(editor%cursors(i), buffer, line_count)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_down(editor%cursors(editor%active_cursor), buffer, line_count)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            g_cursor_only_move = .true.

        case('left')
            ! Hide hover tooltip on movement
            if (is_hover_visible(editor%hover_tooltip)) then
                call hide_hover_tooltip(editor%hover_tooltip)
            end if

            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_left(editor%cursors(i), buffer)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_left(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            g_cursor_only_move = .true.

        case('right')
            ! Hide hover tooltip on movement
            if (is_hover_visible(editor%hover_tooltip)) then
                call hide_hover_tooltip(editor%hover_tooltip)
            end if

            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_right(editor%cursors(i), buffer)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_right(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            g_cursor_only_move = .true.

        ! Selection with shift+motion
        case('shift-up')
            call extend_selection_up(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-down')
            call extend_selection_down(editor%cursors(editor%active_cursor), buffer, line_count)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-left')
            call extend_selection_left(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-right')
            call extend_selection_right(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('home', 'ctrl-a')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_smart_home(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_smart_home(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('end', 'ctrl-e')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_end(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-a')
            ! Select all: selection start at (1,1), cursor at end of file
            editor%cursors(editor%active_cursor)%has_selection = .true.
            editor%cursors(editor%active_cursor)%selection_start_line = 1
            editor%cursors(editor%active_cursor)%selection_start_col = 1
            editor%cursors(editor%active_cursor)%line = line_count
            block
                character(len=:), allocatable :: last_line
                last_line = buffer_get_line(buffer, line_count)
                editor%cursors(editor%active_cursor)%column = &
                    utf8_char_count(last_line) + 1
                editor%cursors(editor%active_cursor)%desired_column =&
                    editor%cursors(editor%active_cursor)%column
                if (allocated(last_line)) deallocate(last_line)
            end block
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-home', 'ctrl-shift-a')
            call extend_selection_home(editor%cursors(editor%active_cursor))
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-end', 'ctrl-shift-e')
            call extend_selection_end(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('pageup')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_page_up(editor%cursors(i), editor)
                end do
            else
                call move_cursor_page_up(editor%cursors(editor%active_cursor), editor)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('pagedown')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_page_down(editor%cursors(i), editor, line_count)
                end do
            else
                call move_cursor_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        ! Wheel events are no longer bare strings: they carry row and column,
        ! so they fall through to handle_mouse_event_action like every other
        ! mouse event and scroll whatever is under the pointer.

        case('ctrl-home')
            ! Jump to beginning of file
            editor%cursors(editor%active_cursor)%line = 1
            editor%cursors(editor%active_cursor)%column = 1
            editor%cursors(editor%active_cursor)%desired_column = 1
            editor%cursors(editor%active_cursor)%has_selection = .false.
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('ctrl-end')
            ! Jump to end of file
            line_count = buffer_get_line_count(buffer)
            editor%cursors(editor%active_cursor)%line = line_count
            call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-pageup')
            call extend_selection_page_up(editor%cursors(editor%active_cursor), editor)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('shift-pagedown')
            call extend_selection_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-left', 'ctrl-left', 'alt-b')
            if (size(editor%cursors) > 1) then
                do i = 1, size(editor%cursors)
                    call move_cursor_word_left(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_word_left(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-right', 'ctrl-right', 'alt-f')
            if (size(editor%cursors) > 1) then
                do i = 1, size(editor%cursors)
                    call move_cursor_word_right(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_word_right(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-shift-left', 'ctrl-shift-left')
            call extend_selection_word_left(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-shift-right', 'ctrl-shift-right')
            call extend_selection_word_right(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('alt-up')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call move_line_up(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-down')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call move_line_down(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-shift-up')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call duplicate_line_up(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-shift-down')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call duplicate_line_down(editor%cursors(editor%active_cursor), buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        ! Tab navigation
        case('alt-1', 'ctrl-1')
            call begin_tab_jump(editor, buffer, 1)
        case('alt-2', 'ctrl-2')
            call begin_tab_jump(editor, buffer, 2)
        case('alt-3', 'ctrl-3')
            call begin_tab_jump(editor, buffer, 3)
        case('alt-4', 'ctrl-4')
            call begin_tab_jump(editor, buffer, 4)
        case('alt-5', 'ctrl-5')
            call begin_tab_jump(editor, buffer, 5)
        case('alt-6', 'ctrl-6')
            call begin_tab_jump(editor, buffer, 6)
        case('alt-7', 'ctrl-7')
            call begin_tab_jump(editor, buffer, 7)
        case('alt-8', 'ctrl-8')
            call begin_tab_jump(editor, buffer, 8)
        case('alt-9', 'ctrl-9')
            call begin_tab_jump(editor, buffer, 9)
        case('alt-0', 'ctrl-0')
            ! Tab 10, the way a keyboard's digit row runs.
            call begin_tab_jump(editor, buffer, 10)
        case('ctrl-alt-left', 'alt-ctrl-left', 'super-ctrl-left', 'ctrl-pageup')
            ! Previous entry on row 1. With no groups that is the previous
            ! tab, exactly as before; with groups a whole group is one entry,
            ! so this steps across groups.
            !
            ! 'ctrl-alt-left' never fired: the input layer emits 'alt-ctrl-'
            ! for modifier 7 and this case listed only the other spelling. Both
            ! are here now, plus super-ctrl for terminals that report super.
            call step_row1_entry(editor, buffer, -1)

        case('ctrl-alt-right', 'alt-ctrl-right', 'super-ctrl-right', 'ctrl-pagedown')
            ! Next entry on row 1. See above.
            call step_row1_entry(editor, buffer, 1)

        ! Text modification
        case('backspace')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call backspace_multiple_cursors(editor, buffer)
            else
                call handle_backspace(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('delete')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call delete_multiple_cursors(editor, buffer)
            else
                call handle_delete(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('enter')
            ! Code actions panel is handled in early routing above

            ! If symbols panel is visible, jump to selected symbol
            if (is_symbols_panel_visible(editor%symbols_panel)) then
                block
                    integer(int32) :: line, col

                    if (get_selected_symbol_location(editor%symbols_panel, line, col)) then
                        ! Jump to the symbol location
                        editor%cursors(editor%active_cursor)%line = line
                        editor%cursors(editor%active_cursor)%column = col
                        call update_viewport(editor)

                        ! Hide panel after jump
                        call hide_symbols_panel(editor%symbols_panel)
                    end if
                end block
                return
            end if

            ! If references panel is visible, jump to selected reference
            if (is_references_panel_visible(editor%references_panel)) then
                block
                    character(len=:), allocatable :: uri
                    integer(int32) :: line, col

                    if (get_selected_reference_location(editor%references_panel, uri, line, col)) then
                        ! Convert URI to file path
                        if (uri(1:7) == "file://") then
                            ! Jump to the reference location
                            ! TODO: Handle cross-file navigation
                            editor%cursors(editor%active_cursor)%line = line
                            editor%cursors(editor%active_cursor)%column = col
                            call update_viewport(editor)
                            call hide_references_panel(editor%references_panel)
                        end if
                    end if
                end block
                return
            end if

            ! If completion popup is visible, insert selected completion
            if (is_completion_visible(editor%completion_popup)) then
                block
                    character(len=:), allocatable :: completion_text
                    completion_text = get_selected_completion(editor%completion_popup)
                    if (len(completion_text) > 0) then
                        ! Insert the completion text at cursor (UTF-8 aware)
                        if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                        call insert_line_text(buffer, &
                            editor%cursors(editor%active_cursor), completion_text)
                        call absorb_closer_the_completion_supplied(editor, buffer, &
                                                                  completion_text)
                    end if
                    call hide_completion_popup(editor%completion_popup)
                end block
                is_edit_action = .true.
                return
            end if

            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call enter_multiple_cursors(editor, buffer)
            else
                call handle_enter(editor%cursors(editor%active_cursor), buffer, &
                                  active_pane_filename(editor))
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('tab')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call tab_multiple_cursors(editor, buffer)
            else
                if (editor%cursors(editor%active_cursor)%has_selection) then
                    call indent_selection(editor%cursors(editor%active_cursor), buffer, &
                                          active_pane_filename(editor))
                else
                    call handle_tab(editor%cursors(editor%active_cursor), buffer, &
                                    active_pane_filename(editor))
                end if
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('shift-tab')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (editor%cursors(editor%active_cursor)%has_selection) then
                call dedent_selection(editor%cursors(editor%active_cursor), buffer, &
                                      active_pane_filename(editor))
            else
                call dedent_current_line(editor%cursors(editor%active_cursor), buffer, &
                                         active_pane_filename(editor))
            end if
            ! Sync so the caret moves on screen this frame; the renderer draws
            ! the pane's cursors, not the editor-level ones (on a whitespace-only
            ! line the caret is the only visible sign the dedent fired)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('ctrl-shift-k')
            ! Delete the caret's lines outright -- nothing goes to the
            ! clipboard or the yank stack (VSCode's Delete Line)
            call save_undo_state(buffer, editor)
            call delete_lines(editor, buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-i', 'alt-shift-i')
            ! Turning OFF is instant and silent -- this is the key you reach
            ! for when a suggestion is in the way, so it must not stall.
            ! Turning ON probes the backend, because otherwise the first
            ! failure you meet is silence, and silence does not say "that
            ! model is not pulled" or "that model cannot fill in the middle".
            if (ai_is_enabled(editor%ai)) then
                call ai_set_enabled(editor%ai, .false.)
                call ghost_clear(editor%ghost)
                call set_status_message('AI completion off (alt-i)')
            else
                call ai_set_enabled(editor%ai, .true.)
                block
                    character(len=:), allocatable :: msg
                    call ai_probe_backend(editor%ai, msg)
                    call set_status_message('AI completion: ' // msg)
                end block
            end if

        case('alt-\')
            ! Explicitly asked for: a bigger model and a longer budget than
            ! anything the keystroke path would ever spend.
            block
                character(len=:), allocatable :: msg
                call ai_request_deep(editor%ai, editor, buffer, msg)
                call set_status_message('AI: ' // msg)
                call sync_editor_to_pane(editor)
            end block

        ! Editing keybinds
        case('ctrl-k')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call kill_line_forward(editor%cursors(i), buffer)
                end do
            else
                call kill_line_forward(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            is_edit_action = .true.

        case('ctrl-u')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call kill_line_backward(editor%cursors(i), buffer)
                end do
            else
                call kill_line_backward(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            is_edit_action = .true.

        case('alt-v')
            ! Split pane vertically
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                ! Sync current buffer to active pane before splitting
                call sync_editor_to_pane(editor)
                if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                    pane_idx = editor%tabs(editor%active_tab_index)%active_pane_index
                    if (pane_idx > 0 .and. pane_idx <= size(editor%tabs(editor%active_tab_index)%panes)) then
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(pane_idx)%buffer, buffer)
                    end if
                end if
                call split_pane_vertical(editor)
            end if

        case('alt-s')
            ! Split pane horizontally
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                ! Sync current buffer to active pane before splitting
                call sync_editor_to_pane(editor)
                if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                    pane_idx = editor%tabs(editor%active_tab_index)%active_pane_index
                    if (pane_idx > 0 .and. pane_idx <= size(editor%tabs(editor%active_tab_index)%panes)) then
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(pane_idx)%buffer, buffer)
                    end if
                end if
                call split_pane_horizontal(editor)
            end if

        case('alt-q')
            ! Close current pane (creates UNTITLED.txt if last pane of last tab)
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call close_pane(editor)

                ! Always copy the buffer (either new tab or UNTITLED.txt)
                if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                    call load_active_pane_into_buffer(editor, buffer)
                ! Should not happen with new logic
                else
                    editor%fuss_mode_active = .true.
                    if (allocated(editor%workspace_path)) then
                        call init_tree_state(tree_state, editor%workspace_path)
                    end if
                end if
            end if

        case('ctrl-w')
            ! Close current tab (prompts to save if modified)
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                ! Check if tab is modified - prompt before closing
                if (editor%tabs(editor%active_tab_index)%modified) then
                    call prompt_save_before_close_tab(editor, buffer)
                else
                    call close_tab_without_prompt(editor, buffer)
                end if
            end if

        case('alt-h')
            ! Navigate to pane on the left (Vim-style hjkl with alt)
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call navigate_to_pane_left(editor)
            end if

        case('alt-l')
            ! Navigate to pane on the right
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call navigate_to_pane_right(editor)
            end if

        case('ctrl-shift-up', 'alt-k')
            ! Navigate to pane above
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call navigate_to_pane_up(editor)
            end if

        case('ctrl-shift-down', 'alt-j')
            ! Navigate to pane below
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call navigate_to_pane_down(editor)
            end if

        case('alt-d', 'alt-delete')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call delete_word_forward(editor%cursors(i), buffer)
                end do
            else
                call delete_word_forward(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-backspace')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call delete_word_backward_multiple_cursors(editor, buffer)
            else
                call delete_word_backward(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            call update_viewport(editor)
            is_edit_action = .true.

        case('ctrl-t')
            ! Create new empty tab with unique name
            block
                integer :: untitled_counter, i, name_len, max_untitled, dash_pos, num_start
                character(len=32) :: untitled_name, num_str
                logical :: name_exists
                integer :: ios

                ! Scan existing tabs to find highest untitled number
                max_untitled = 0
                if (allocated(editor%tabs)) then
                    do i = 1, size(editor%tabs)
                        if (allocated(editor%tabs(i)%filename)) then
                            ! Check if it's an untitled tab
                            if (index(editor%tabs(i)%filename, '[Untitled') == 1) then
                                ! Check for plain [Untitled]
                                if (trim(editor%tabs(i)%filename) == '[Untitled]') then
                                    max_untitled = max(max_untitled, 1)
                                else
                                    ! Check for [Untitled-N]
                                    dash_pos = index(editor%tabs(i)%filename, '-')
                                    if (dash_pos > 0) then
                                        num_start = dash_pos + 1
                                        num_str = editor%tabs(i)%filename(num_start:len_trim(editor%tabs(i)%filename)-1)
                                        read(num_str, *, iostat=ios) untitled_counter
                                        if (ios == 0) then
                                            max_untitled = max(max_untitled, untitled_counter)
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end do
                end if

                ! Start checking from max_untitled + 1, but check backwards too in case of gaps
                untitled_counter = max(1, max_untitled)
                do
                    if (untitled_counter == 1) then
                        write(untitled_name, '(A)') '[Untitled]'
                    else
                        write(untitled_name, '(A,I0,A)') '[Untitled-', untitled_counter, ']'
                    end if

                    ! Check if this name already exists
                    name_exists = .false.
                    if (allocated(editor%tabs)) then
                        do i = 1, size(editor%tabs)
                            if (allocated(editor%tabs(i)%filename)) then
                                if (trim(editor%tabs(i)%filename) == trim(untitled_name)) then
                                    name_exists = .true.
                                    exit
                                end if
                            end if
                        end do
                    end if

                    if (.not. name_exists) exit
                    untitled_counter = untitled_counter + 1
                end do

                call create_tab(editor, trim(untitled_name))

                ! Switch to the new tab (it's already active after create_tab)
                if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                    ! Clear the display buffer and copy from the new tab's pane (which is empty)
                    call cleanup_buffer(buffer)
                    call init_buffer(buffer)

                    ! Copy empty buffer to the new tab's pane
                    if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                        size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(1)%buffer, buffer)
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                            editor%active_tab_index))%buffer, buffer)
                    end if

                    ! Update editor state with the new tab
                    name_len = len_trim(untitled_name)
                    if (allocated(editor%filename)) deallocate(editor%filename)
                    allocate(character(len=name_len) :: editor%filename)
                    editor%filename = trim(untitled_name)

                    ! Reset cursor to top
                    editor%cursors(editor%active_cursor)%line = 1
                    editor%cursors(editor%active_cursor)%column = 1
                    editor%cursors(editor%active_cursor)%desired_column = 1
                    editor%viewport_line = 1
                    editor%viewport_column = 1
                    editor%modified = .false.
                end if
            end block

        case('f5', 'alt-t')
            ! Toggle integrated terminal (F5 or Alt-T)
            if (is_terminal_panel_visible(editor%terminal_panel)) then
                ! Visible but unfocused — re-focus
                editor%terminal_panel%focused = .true.
            else
                ! Hidden — open and focus
                call toggle_terminal_panel(editor%terminal_panel, &
                    editor%screen_rows, editor%screen_cols)
            end if

        case('alt-shift-j')
            ! Join lines
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                do i = 1, size(editor%cursors)
                    call join_lines(editor%cursors(i), buffer)
                end do
            else
                call join_lines(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-x')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors in reverse order (bottom to top)
                do i = size(editor%cursors), 1, -1
                    call cut_selection_or_line(editor%cursors(i), buffer)
                end do
            else
                call cut_selection_or_line(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            is_edit_action = .true.

        case('ctrl-c')
            ! Copy only needs active cursor (copies to shared clipboard)
            call copy_selection_or_line(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-v')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                call paste_multiple_cursors(editor, buffer)
            else
                call paste_clipboard(editor%cursors(editor%active_cursor), buffer)
            end if
            call sync_editor_to_pane(editor)
            is_edit_action = .true.

        case('ctrl-s')
            ! Save the active pane/tab's buffer
            ! (All panes in a tab share the same buffer, so saving saves the entire tab)
            call save_file(editor, buffer)

        case('ctrl-shift-s')
            ! Save every modified tab.
            !
            ! The palette has advertised this as Ctrl+Shift+S since it was
            ! added and delegated to this key, but no case ever existed to
            ! catch it -- so both the menu entry and the chord did nothing at
            ! all. It is also what a hand reaching for Ctrl+S with Shift still
            ! down produces, which is how it came to be reported as "ctrl+S
            ! does not save".
            call save_all_tabs(editor, buffer)

        ! LSP features
        case('ctrl-space')
            ! Trigger code completion (popup shows when the response arrives)
            block
                integer :: completion_server
                completion_server = get_lsp_server_for_cap(editor, CAP_COMPLETION)
                if (completion_server > 0) then
                    ! Request completion at current cursor position
                    ! LSP uses 0-based positions
                    block
                        integer :: request_id, lsp_line, lsp_char
                        lsp_line = editor%cursors(editor%active_cursor)%line - 1
                        lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                        ! Popup and ghost text never coexist
                        call ghost_clear(editor%ghost)
                        call ghost_clear_pending(editor%ghost)

                        ! Flush debounced document sync so completions are
                        ! computed against the current text
                        block
                            use document_sync_module, only: flush_pending_changes
                            call flush_pending_changes( &
                                editor%tabs(editor%active_tab_index)%document_sync, &
                                editor%lsp_manager, .true.)
                        end block

                        saved_editor_for_callback => editor

                        request_id = request_completion(editor%lsp_manager, &
                            completion_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            lsp_line, lsp_char, &
                            handle_popup_completion_response_wrapper)
                    end block
                end if
            end block

        case('ctrl-h')
            ! Trigger hover information
            block
                integer :: hover_server
                hover_server = get_lsp_server_for_cap(editor, CAP_HOVER)
                if (hover_server > 0) then
                    ! Request hover at current cursor position
                    block
                        integer :: request_id, lsp_line, lsp_char
                        lsp_line = editor%cursors(editor%active_cursor)%line - 1
                        lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                        request_id = request_hover(editor%lsp_manager, &
                            hover_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            lsp_line, lsp_char)

                        if (request_id > 0) then
                            ! Show tooltip at cursor position (will populate when response arrives)
                            call show_hover_tooltip(editor%hover_tooltip, &
                                editor%cursors(editor%active_cursor)%line - editor%viewport_line + 2, &
                                editor%cursors(editor%active_cursor)%column - editor%viewport_column + 1, &
                                editor%screen_rows, editor%screen_cols)
                        end if
                    end block
                end if
            end block

        case('f10', 'alt-.')
            ! Trigger code actions (F10 or Alt+.) - toggle behavior
            ! If panel is already visible, close it
            if (is_code_actions_panel_visible(editor%code_actions_panel)) then
                call hide_code_actions_panel(editor%code_actions_panel)
            else
                block
                    integer :: code_actions_server
                    code_actions_server = get_lsp_server_for_cap(editor, CAP_CODE_ACTIONS)
                    if (code_actions_server > 0) then
                        ! Request code actions for the current line
                        block
                            integer :: request_id, lsp_line
                            character(len=:), allocatable :: file_uri
                            type(diagnostic_t), allocatable :: line_diags(:)
                            type(json_value_t) :: diags_json

                            lsp_line = editor%cursors(editor%active_cursor)%line - 1

                            ! Save editor state for callback
                            saved_editor_for_callback => editor

                            ! Get diagnostics for this line FROM THIS SERVER to include in request
                            ! This is critical for multi-LSP: Ruff should only see Ruff's diagnostics
                            file_uri = filename_to_uri(editor%tabs(editor%active_tab_index)%filename)

                            line_diags = get_diagnostics_for_line_by_server(editor%diagnostics, file_uri, &
                                editor%cursors(editor%active_cursor)%line, code_actions_server)

                            ! Convert diagnostics to JSON for request
                            diags_json = diagnostics_to_json(line_diags)

                            ! Request code actions for entire line with diagnostics context
                            request_id = request_code_actions(editor%lsp_manager, &
                                code_actions_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            lsp_line, 0, &
                            lsp_line, 999, &
                            handle_code_actions_response_wrapper, &
                            diags_json)
                            ! Panel will be shown when response arrives in handle_code_actions_response_impl
                        end block
                    end if
                end block
            end if

        case('f12', 'ctrl-\\', 'alt-g')
            ! Go to definition (F12, Ctrl+\, or Alt+G)
            block
                integer :: def_server
                def_server = get_lsp_server_for_cap(editor, CAP_DEFINITION)
                if (def_server > 0) then
                    ! Save current location to jump stack
                    if (allocated(editor%filename)) then
                        call push_jump_location(editor%jump_stack, &
                            trim(editor%filename), &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)
                    end if

                    ! Request definition at current cursor position
                    block
                        integer :: request_id, lsp_line, lsp_char
                        lsp_line = editor%cursors(editor%active_cursor)%line - 1
                        lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                        ! Save editor state and buffer pointers for callback
                        saved_editor_for_callback => editor
                        saved_buffer_for_callback => buffer

                        request_id = request_definition(editor%lsp_manager, &
                            def_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            lsp_line, lsp_char, handle_definition_response_wrapper)

                        if (request_id > 0) then
                            editor%timed_message = 'Searching for definition...'
                            editor%timed_message_ms = get_time_ms()
                        else
                            editor%timed_message = '[F12] LSP request failed'
                            editor%timed_message_ms = get_time_ms()
                        end if
                    end block
                else
                    editor%timed_message = '[F12] No LSP server with definition support'
                    editor%timed_message_ms = get_time_ms()
                end if
            end block

        case('shift-f12', 'alt-r')
            ! Find all references (Shift+F12 or Alt+R)
            block
                integer :: refs_server
                refs_server = get_lsp_server_for_cap(editor, CAP_REFERENCES)
                if (refs_server > 0) then
                    ! Request references at current cursor position
                    block
                        use references_panel_module, only: clear_references
                        integer :: request_id, lsp_line, lsp_char
                        lsp_line = editor%cursors(editor%active_cursor)%line - 1
                        lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                        ! Clear any existing references so we can detect when new ones arrive
                        call clear_references(editor%references_panel)

                        ! Save editor state for callback
                        saved_editor_for_callback => editor

                        request_id = request_references(editor%lsp_manager, &
                            refs_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            lsp_line, lsp_char, handle_references_response_wrapper)

                        if (request_id > 0) then
                            ! Response will be handled by callback
                            call terminal_move_cursor(editor%screen_rows, 1)
                            call terminal_write('Searching for references...                ')
                            ! Show panel (will be populated when response arrives)
                            call show_references_panel(editor%references_panel, editor%screen_cols, editor%screen_rows)

                            ! Wait for LSP response to populate panel
                            block
                                use lsp_server_manager_module, only: process_server_messages
                                integer :: poll_count, max_polls
                                logical :: response_received
                                integer :: count_rate, count_start, count_end
                                real :: elapsed_ms

                                max_polls = 50  ! Poll up to 50 times (500ms total)
                                poll_count = 0
                                response_received = .false.

                                call system_clock(count_rate=count_rate)

                                do while (poll_count < max_polls .and. .not. response_received)
                                    ! Process any pending LSP messages
                                    call process_server_messages(editor%lsp_manager)

                                    ! Check if we got a response (panel populated or explicitly set to 0)
                                    if (allocated(editor%references_panel%references)) then
                                        response_received = .true.
                                    end if

                                    if (.not. response_received) then
                                        ! Sleep for ~10ms between polls
                                        call system_clock(count=count_start)
                                        do
                                            call system_clock(count=count_end)
                                            elapsed_ms = real(count_end - count_start) / real(count_rate) * 1000.0
                                            if (elapsed_ms >= 10.0) exit
                                        end do
                                        poll_count = poll_count + 1
                                    end if
                                end do
                            end block

                            ! Interactive loop for references panel (offcanvas mode)
                            block
                                use input_handler_module, only: get_key_input
                                use references_panel_module, only: hide_references_panel, references_panel_handle_key
                                use renderer_module, only: render_screen_with_lsp_panel
                                character(len=32) :: key_input
                                integer :: status
                                logical :: handled

                                ! Initial render with offcanvas panel
                                call render_screen_with_lsp_panel(buffer, editor, "references")

                                do
                                    call get_key_input(key_input, status)
                                    if (status /= 0) cycle

                                    if (key_input == 'esc') then
                                        call hide_references_panel(editor%references_panel)
                                        call render_screen(buffer, editor)
                                        exit
                                    else if (key_input == 'enter') then
                                        ! Navigate to selected reference
                                        block
                                            use references_panel_module, only: get_selected_reference_location
                                            use editor_state_module, only: switch_to_tab_with_buffer
                                            character(len=:), allocatable :: ref_uri, ref_path
                                            integer :: ref_line, ref_col, ti

                                            if (get_selected_reference_location( &
                                                editor%references_panel, &
                                                ref_uri, ref_line, ref_col)) then
                                                if (len(ref_uri) >= 7 .and. ref_uri(1:7) == "file://") then
                                                    ref_path = ref_uri(8:)

                                                    ! Check if same file as current
                                                    if (allocated(editor%filename) .and. &
                                                        trim(ref_path) == trim(editor%filename)) then
                                                        ! Same file — just move cursor
                                                        editor%cursors(editor%active_cursor)%line = ref_line
                                                    else
                                                        ! Different file — find open tab or open new
                                                        do ti = 1, size(editor%tabs)
                                                            if (trim(editor%tabs(ti)%filename) == trim(ref_path)) then
                                                                call switch_to_tab_with_buffer(editor, ti, buffer)
                                                                exit
                                                            end if
                                                        end do
                                                        ! If no tab found, open the file
                                                        if (.not. allocated(editor%filename) .or. &
                                                            trim(editor%filename) /= trim(ref_path)) then
                                                            call open_file_in_editor(ref_path, editor, buffer)
                                                        end if
                                                        editor%cursors(editor%active_cursor)%line = ref_line
                                                    end if
                                                    ! Stored column is LSP UTF-16 units + 1
                                                    editor%cursors(editor%active_cursor)%column = &
                                                        char_col_from_lsp(buffer, ref_line, ref_col - 1)
                                                    editor%cursors(editor%active_cursor)%desired_column = &
                                                        editor%cursors(editor%active_cursor)%column
                                                    editor%cursors(editor%active_cursor)%has_selection = .false.
                                                    editor%viewport_line = max(1, ref_line - editor%screen_rows / 2)
                                                end if
                                            end if
                                        end block
                                        call sync_editor_to_pane(editor)
                                        call hide_references_panel(editor%references_panel)
                                        call render_screen(buffer, editor)
                                        exit
                                    else
                                        ! Try panel-specific key handling
                                        handled = references_panel_handle_key(editor%references_panel, key_input)
                                        ! Re-render with offcanvas panel
                                        call render_screen_with_lsp_panel(buffer, editor, "references")
                                    end if
                                end do
                            end block
                        end if
                    end block
                end if
            end block

        case('f2', 'alt-n')
            ! Rename symbol (F2 or Alt+N).
            ! F2 is the VSCode binding but is easily swallowed before any
            ! program sees it -- window managers and desktop shells bind it
            ! for rename too. alt-n follows the pattern the other LSP
            ! commands already use (alt-g definition, alt-r references,
            ! alt-e diagnostics).
            block
                integer :: rename_server
                rename_server = get_lsp_server_for_cap(editor, CAP_RENAME)
                if (rename_server > 0) then
                    ! Get word under cursor as old name
                    block
                        character(len=:), allocatable :: line, old_name, new_name
                        integer :: word_start, word_end, lsp_line, lsp_char, request_id
                        logical :: cancelled

                        line = buffer_get_line(buffer, editor%cursors(editor%active_cursor)%line)
                        block
                            integer :: byte_pos
                            byte_pos = utf8_char_to_byte_index(line, &
                                editor%cursors(editor%active_cursor)%column)
                            if (byte_pos == 0) byte_pos = len(line) + 1
                            call find_word_boundaries(line, byte_pos, word_start, word_end)
                        end block

                        if (word_start > 0 .and. word_end >= word_start) then
                            old_name = line(word_start:word_end)

                            ! Show rename prompt
                            call show_rename_prompt(editor%screen_rows, old_name, new_name, cancelled)

                            if (.not. cancelled .and. allocated(new_name)) then
                                ! The server must be looking at the SAME text
                                ! the ranges will be applied to. Document
                                ! changes are debounced half a second, so
                                ! renaming shortly after typing asked clangd
                                ! about a document that no longer existed: it
                                ! answered with ranges for the old text, and
                                ! those were applied to the new text. That
                                ! destroyed code -- 'char li' replaced by the
                                ! new name, and the name inserted on an
                                ! unrelated line -- rather than merely failing.
                                !
                                ! LSP orders notifications and requests on one
                                ! connection, so a forced didChange here is
                                ! guaranteed to be processed before the rename.
                                call flush_document_now(editor)

                                ! Send rename request
                                lsp_line = editor%cursors(editor%active_cursor)%line - 1
                                lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                                ! Save editor state for callback
                                saved_editor_for_callback => editor
                                ! What every edit must be replacing. The flush
                                ! above closes the window that caused the
                                ! corruption; this catches it happening for any
                                ! other reason, because the cost of being wrong
                                ! here is destroyed source.
                                g_rename_expect = old_name

                                request_id = request_rename(editor%lsp_manager, &
                                    rename_server, &
                                    editor%tabs(editor%active_tab_index)%filename, &
                                    lsp_line, lsp_char, new_name, handle_rename_response_wrapper)

                                if (request_id > 0) then
                                    call terminal_move_cursor(editor%screen_rows, 1)
                                    call terminal_write('Renaming symbol...                         ')

                                    ! Poll for LSP response and render immediately when received
                                    block
                                        integer :: poll_count, max_polls, pane_idx
                                        integer(8) :: start_time, end_time, count_rate, target_time
                                        max_polls = 100  ! Poll up to 100 times (1 second total)

                                        do poll_count = 1, max_polls
                                            ! Process any LSP messages
                                            call process_server_messages(editor%lsp_manager)

                                            ! Check if rename response modified the buffer
                                            if (g_lsp_modified_buffer) then
                                                ! LSP now modifies pane buffer directly, sync FROM pane TO local buffer and tab
                                                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                                                    size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                                                    pane_idx = editor%tabs(editor%active_tab_index)%active_pane_index
                                                    if (pane_idx > 0 .and. pane_idx <= &
                                                        size(editor%tabs(editor%active_tab_index)%panes)) then
                                                        ! Copy FROM pane buffer TO local buffer (for rendering)
                                                        call copy_buffer(buffer, &
                                                            editor%tabs(editor%active_tab_index)%panes(pane_idx)%buffer)
                                                        ! Also sync to tab buffer (to keep them consistent)
                                                        block
                                                            integer :: ap
                                                            ap = active_pane_of(editor, editor%active_tab_index)
                                                            call copy_buffer( &
                                                                editor%tabs(editor%active_tab_index)%panes(ap)%buffer, &
                                                                buffer)
                                                        end block
                                                    end if
                                                end if

                                                ! Render the updated buffer immediately
                                                if (editor%fuss_mode_active) then
                                                    call render_screen_with_tree(buffer, editor, &
                                                        allocated(search_pattern), match_case_sensitive)
                                                else
                                                    call render_screen(buffer, editor, &
                                                        allocated(search_pattern), match_case_sensitive)
                                                end if

                                                ! Reset flag and exit loop
                                                g_lsp_modified_buffer = .false.
                                                exit
                                            end if

                                            ! Delay 10ms between polls using system_clock
                                            call system_clock(start_time, count_rate)
                                            target_time = start_time + (count_rate / 100)  ! 10ms
                                            do
                                                call system_clock(end_time)
                                                if (end_time >= target_time) exit
                                            end do
                                        end do
                                    end block
                                end if

                                deallocate(new_name)
                            end if

                            if (allocated(old_name)) deallocate(old_name)
                        else
                            call set_status_message( &
                                '[F2] No symbol under the cursor to rename')
                        end if

                        if (allocated(line)) deallocate(line)
                    end block
                else
                    call set_status_message(rename_unavailable_reason(editor))
                end if
            end block

        case('shift-alt-f')
            ! Format document
            block
                integer :: format_server
                format_server = get_lsp_server_for_cap(editor, CAP_FORMATTING)
                if (format_server > 0) then
                    block
                        integer :: request_id

                        ! Save editor state for callback
                        saved_editor_for_callback => editor

                        ! Request formatting with 4 spaces (configurable later)
                        request_id = request_formatting(editor%lsp_manager, &
                            format_server, &
                            editor%tabs(editor%active_tab_index)%filename, &
                            4, .true., handle_formatting_response_wrapper)

                        if (request_id > 0) then
                            call terminal_move_cursor(editor%screen_rows, 1)
                            call terminal_write('Formatting document...                     ')
                        end if
                    end block
                end if
            end block

        case('f4', 'alt-o')
            ! Document symbols outline (F4 or Alt+O) - toggle behavior
            if (is_symbols_panel_visible(editor%symbols_panel)) then
                ! Panel is visible, hide it
                call hide_symbols_panel(editor%symbols_panel)
            else
                ! Panel is hidden, request symbols and show it
                block
                    integer :: symbols_server
                    symbols_server = get_lsp_server_for_cap(editor, CAP_DOCUMENT_SYMBOLS)
                    if (symbols_server > 0) then
                        ! Request document symbols
                        block
                            integer :: request_id

                            ! Save editor state for callback
                            saved_editor_for_callback => editor

                            request_id = request_document_symbols(editor%lsp_manager, &
                                symbols_server, &
                                editor%tabs(editor%active_tab_index)%filename, &
                                handle_symbols_response_wrapper)

                            if (request_id > 0) then
                                ! Response will be handled by callback
                                call terminal_move_cursor(editor%screen_rows, 1)
                                call terminal_write('Loading document symbols...                ')
                                ! Show panel (will be populated when response arrives)
                                call show_symbols_panel(editor%symbols_panel, editor%screen_cols, editor%screen_rows)
                            end if
                        end block
                    end if
                end block
            end if

        case('shift-f10', 'alt-z')
            ! Context menu at the caret. shift-f10 is the universal chord for
            ! this; alt-z is the fallback because GNOME Terminal grabs F10 for
            ! its own menubar, the same class of problem the README documents
            ! for F2. A letter rather than punctuation on purpose: the legacy
            ! escape decoder only accepts letters, digits and a couple of
            ! special-cased marks, so alt-; would never arrive at all.
            ! python3 tools/keycap.py shows which your terminal delivers.
            call open_document_menu_at_caret(editor, buffer)

        case('ctrl-p')
            ! Command palette (Ctrl+P - VSCode standard)
            ! Note: ctrl-shift-p doesn't work - terminals can't distinguish ctrl-p from ctrl-shift-p
            call open_command_palette(editor, buffer, should_quit)

        case('f6', 'alt-p')
            ! Workspace symbols (F6 or Alt+P for project) - offcanvas panel with fzf-like filtering
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                block
                    use workspace_symbols_panel_module, only: show_workspace_symbols_panel, &
                                                               render_workspace_symbols_panel, &
                                                               workspace_symbols_panel_handle_key, &
                                                               hide_workspace_symbols_panel, &
                                                               is_workspace_symbols_panel_visible, &
                                                               get_selected_symbol, &
                                                               get_search_query, &
                                                               workspace_symbol_t
                    use input_handler_module, only: get_key_input
                    use lsp_server_manager_module, only: request_workspace_symbols, &
                        CAP_WORKSPACE_SYMBOLS_USE => CAP_WORKSPACE_SYMBOLS, &
                        process_server_messages
                    integer :: request_id, status, ws_server
                    character(len=32) :: key_input
                    character(len=:), allocatable :: prev_query, curr_query
                    logical :: handled
                    type(workspace_symbol_t) :: selected_symbol

                    ! Toggle behavior - if already visible, hide and return
                    if (is_workspace_symbols_panel_visible(editor%workspace_symbols_panel)) then
                        call hide_workspace_symbols_panel(editor%workspace_symbols_panel)
                        call render_screen(buffer, editor)
                        return
                    end if

                    ! Show panel with screen dimensions
                    call show_workspace_symbols_panel(editor%workspace_symbols_panel, &
                        editor%screen_cols, editor%screen_rows)

                    ! Get server with workspace symbols capability
                    ws_server = get_lsp_server_for_cap(editor, CAP_WORKSPACE_SYMBOLS_USE)

                    ! Save editor state for LSP callback
                    saved_editor_for_callback => editor

                    ! Don't send initial empty query - pyright requires at least 1 char
                    ! Query will be sent when user starts typing

                    prev_query = ''

                    ! Interactive loop
                    do while (is_workspace_symbols_panel_visible(editor%workspace_symbols_panel))
                        ! Process any pending LSP responses
                        call process_server_messages(editor%lsp_manager)

                        ! Render the panel
                        call render_workspace_symbols_panel(editor%workspace_symbols_panel, editor%screen_rows)
                        call terminal_flush()

                        call get_key_input(key_input, status)
                        if (status /= 0) cycle

                        ! Handle enter specially - navigate to symbol
                        if (trim(key_input) == 'enter') then
                            selected_symbol = get_selected_symbol(editor%workspace_symbols_panel)
                            if (allocated(selected_symbol%file_uri) .and. len_trim(selected_symbol%file_uri) > 0) then
                                call navigate_to_workspace_symbol(editor, buffer, selected_symbol, should_quit)
                            else if (allocated(selected_symbol%file_path) .and. len_trim(selected_symbol%file_path) > 0) then
                                ! Use file_path if file_uri not set
                                selected_symbol%file_uri = 'file://' // trim(selected_symbol%file_path)
                                call navigate_to_workspace_symbol(editor, buffer, selected_symbol, should_quit)
                            end if
                            call hide_workspace_symbols_panel(editor%workspace_symbols_panel)
                            call render_screen(buffer, editor)
                            exit
                        end if

                        ! Let the panel handle all other keys
                        handled = workspace_symbols_panel_handle_key(editor%workspace_symbols_panel, trim(key_input))

                        ! Check if query changed - send new LSP request (only if query has content)
                        if (ws_server > 0) then
                            curr_query = get_search_query(editor%workspace_symbols_panel)
                            if (curr_query /= prev_query .and. len_trim(curr_query) > 0) then
                                request_id = request_workspace_symbols(editor%lsp_manager, &
                                    ws_server, curr_query, handle_workspace_symbols_response_wrapper)
                                prev_query = curr_query
                            end if
                        end if
                    end do

                    call render_screen(buffer, editor)
                end block
            end if

        case('alt-comma')
            ! Jump back in navigation history (Alt+,)
            if (.not. is_jump_stack_empty(editor%jump_stack)) then
                block
                    character(len=:), allocatable :: jump_filename
                    integer(int32) :: jump_line, jump_column
                    logical :: success

                    success = pop_jump_location(editor%jump_stack, jump_filename, jump_line, jump_column)
                    if (success) then
                        ! Check if we need to open a different file
                        if (allocated(editor%filename)) then
                            if (trim(jump_filename) /= trim(editor%filename)) then
                                ! TODO: Open the file
                                call terminal_move_cursor(editor%screen_rows, 1)
                                call terminal_write('Opening: ' // trim(jump_filename))
                                ! For now, just jump if same file
                            end if
                        end if

                        ! Jump to the location
                        editor%cursors(editor%active_cursor)%line = jump_line
                        editor%cursors(editor%active_cursor)%column = jump_column
                        editor%cursors(editor%active_cursor)%desired_column = jump_column
                        call sync_editor_to_pane(editor)
                        call update_viewport(editor)
                    end if
                end block
            end if

        case("ctrl-'", "ctrl-apostrophe", "alt-'")
            ! Cycle quotes: " -> ' -> `
            ! ctrl-': Doesn't work (terminals send plain apostrophe)
            ! alt-': Alternative binding (Option+' on Mac)
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call cycle_quotes(editor%cursors(editor%active_cursor), buffer)
            is_edit_action = .true.

        case('ctrl-opt-backspace', 'ctrl-alt-backspace', 'alt-shift-backspace', 'alt-shift-apostrophe')
            ! Remove surrounding brackets/quotes
            ! ctrl-alt-backspace: Doesn't work (terminals send alt-backspace)
            ! alt-shift-backspace: Doesn't work (terminals send alt-backspace)
            ! alt-shift-': Alternative binding (Alt+Shift+' = Alt+")
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call remove_brackets(editor%cursors(editor%active_cursor), buffer)
            is_edit_action = .true.

        case('ctrl-d')
            call select_next_match(editor, buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('f8', 'alt-e')
            ! Toggle diagnostics panel (F8 or Alt+E for errors)
            call toggle_panel(editor%diagnostics_panel)
            ! Re-render screen to show/hide the panel
            call render_screen(buffer, editor)

        case('alt-m')
            ! Toggle LSP server installer panel (Alt+M for Manager)
            if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
                call hide_lsp_server_installer_panel(editor%lsp_installer_panel)
            else
                call show_lsp_server_installer_panel(editor%lsp_installer_panel)
            end if
            call render_screen(buffer, editor)

        case('alt-c')
            ! Toggle case sensitivity for match mode (ctrl-d)
            ! Only has effect when in active match mode (search_pattern allocated)
            if (allocated(search_pattern)) then
                match_case_sensitive = .not. match_case_sensitive
            end if

        case('alt-[', 'alt-]')
            ! Jump to matching bracket
            call jump_to_matching_bracket(editor, buffer)

        case('super-ctrl-down', 'alt-shift-pagedown')
            ! Enter the group under the cursor on row 1, or the active one.
            call enter_group_from_row1(editor, buffer)

        case('super-ctrl-up', 'alt-shift-pageup')
            ! Leave the current group for the nearest tab outside it.
            call leave_current_group(editor, buffer)

        case('super-up', 'ctrl-alt-up', 'alt-ctrl-up', &
             'alt-ctrl-shift-up', 'ctrl-shift-alt-up', 'ctrl-alt-shift-up')
            ! Add cursor on line above. The input layer emits alt-ctrl- for
            ! modifier 7; ctrl-alt- is kept as a defensive alias. 'opt-meta-'
            ! used to be here for modifier 9, which is super, so it is spelled
            ! that way now.
            !
            ! The shift variants (modifier 8) exist because BOTH of the other
            ! two are chords a Linux desktop routinely takes before the
            ! terminal ever sees them: ctrl+alt+arrows switches workspace on
            ! GNOME and KDE, and super+arrows tiles or maximises the window.
            ! Adding shift is the cheapest chord neither of them wants. The
            ! prefix builder emits alt-ctrl-shift-; the other two spellings
            ! are defensive, matching the existing aliases above.
            call add_cursor_above(editor)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        case('super-down', 'ctrl-alt-down', 'alt-ctrl-down', &
             'alt-ctrl-shift-down', 'ctrl-shift-alt-down', &
             'ctrl-alt-shift-down')
            ! Add cursor on line below. See above.
            call add_cursor_below(editor, buffer)
            call sync_editor_to_pane(editor)
            call update_viewport(editor)

        ! Search commands
        case('ctrl-f')
            ! Unified search and replace (Ctrl+F)
            call show_unified_search_prompt(editor, buffer)
            call update_viewport(editor)

        case('ctrl-r')
            ! Find and replace
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call show_replace_prompt(editor, buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        case('n')
            ! Only use 'n' for search navigation if we have an active search
            ! (search mode ends on ESC; the pattern itself persists for reuse)
            if (search_mode_active .and. allocated(current_search_pattern)) then
                call search_forward(editor, buffer)
                call update_viewport(editor)
            else
                ! No active search, treat as regular character with multi-cursor support
                if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, 'n')
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, 'n')
                end if
                call sync_editor_to_pane(editor)
                is_edit_action = .true.
            end if

        case('N')
            ! Only use 'N' for search navigation if we have an active search
            if (search_mode_active .and. allocated(current_search_pattern)) then
                call search_backward(editor, buffer)
                call update_viewport(editor)
            else
                ! No active search, treat as regular character with multi-cursor support
                if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, 'N')
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, 'N')
                end if
                call sync_editor_to_pane(editor)
                is_edit_action = .true.
            end if

        case('paste')
            ! Bracketed paste into the editor: insert the whole
            ! block as a single undo unit.
            block
                character(len=:), allocatable :: ptext
                ptext = get_paste_text()
                if (len(ptext) > 0) then
                    call save_undo_state(buffer, editor)
                    if (size(editor%cursors) > 1) then
                        call paste_text_multiple_cursors(editor, buffer, ptext)
                    else
                        call insert_text_block( &
                            editor%cursors(editor%active_cursor), &
                            buffer, ptext)
                    end if
                    call sync_editor_to_pane(editor)
                    call update_viewport(editor)
                    is_edit_action = .true.
                end if
            end block

        case default
            ! Check for mouse events
            if (index(key_str, 'mouse-') == 1) then
                call handle_mouse_event_action(key_str, editor, buffer)
                call sync_editor_to_pane(editor)
            ! Regular character input (including space). A printable key is
            ! one ASCII char, a space (trim removes it), or one whole UTF-8
            ! multibyte sequence assembled by get_key_input (2-4 bytes whose
            ! first byte is a lead byte) — inserted as a single column.
            else if (len_trim(key_str) == 1 .or. &
                     (len_trim(key_str) == 0 .and. key_str(1:1) == ' ') .or. &
                     (len_trim(key_str) >= 2 .and. len_trim(key_str) <= 4 .and. &
                      iachar(key_str(1:1)) >= 192)) then
                is_text_insert = .true.
                block
                    integer :: klen
                    klen = max(1, len_trim(key_str))
                    if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                    ! Handle character input for all cursors
                    if (size(editor%cursors) > 1) then
                        call insert_char_multiple_cursors(editor, buffer, key_str(1:klen))
                    else
                        call insert_char(editor%cursors(editor%active_cursor), buffer, key_str(1:klen))
                    end if
                end block
                call sync_editor_to_pane(editor)
                call update_viewport(editor)
                is_edit_action = .true.

                ! Auto-trigger signature help on '(' or ','
                if (key_str(1:1) == '(' .or. key_str(1:1) == ',') then
                    block
                        integer :: sig_server
                        sig_server = get_lsp_server_for_cap(editor, &
                            CAP_COMPLETION)  ! Signature help typically comes from completion provider
                        if (sig_server > 0) then
                            block
                                integer :: request_id, lsp_line, lsp_char
                                lsp_line = editor%cursors(editor%active_cursor)%line - 1
                                lsp_char = lsp_char_of(buffer, &
                            editor%cursors(editor%active_cursor)%line, &
                            editor%cursors(editor%active_cursor)%column)

                                ! Save editor state for callback
                                saved_editor_for_callback => editor

                                request_id = request_signature_help(editor%lsp_manager, &
                                    sig_server, &
                                    editor%tabs(editor%active_tab_index)%filename, &
                                    lsp_line, lsp_char, handle_signature_response_wrapper)

                                if (request_id > 0) then
                                    ! Show tooltip placeholder
                                    call show_signature_tooltip(editor%signature_tooltip, &
                                        editor%cursors(editor%active_cursor)%line - editor%viewport_line + 1, &
                                        editor%cursors(editor%active_cursor)%column - editor%viewport_column + 1)
                                end if
                            end block
                        end if
                    end block
                end if

                ! Hide signature help on ')'
                if (key_str(1:1) == ')') then
                    call hide_signature_tooltip(editor%signature_tooltip)
                end if
            end if
        end select

        ! Anything other than typing text -- a cursor move, Enter, backspace,
        ! a paste, an undo -- breaks the link between the caret and the closer
        ! auto-close parked in front of it, so stop offering to step over it.
        if (.not. is_text_insert) call clear_pending_closers()

        ! Update edit action state
        last_action_was_edit = is_edit_action

        ! Notify LSP of document changes if buffer was modified
        if (is_edit_action) then
            ! Bumped here rather than in notify_buffer_change, which returns
            ! early for files with no language server -- the revision has to
            ! track every edit, not just the ones LSP hears about.
            call bump_doc_revision(editor)
            call notify_buffer_change(editor, buffer)
            ! Recompute the ghost suggestion after the LSP sync so a
            ! completion request sees the up-to-date document
            call update_ghost_suggestion(editor, buffer, key_str, ghost_extended)
        end if
    end subroutine handle_key_command

    subroutine move_cursor_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: target_line

        cursor%has_selection = .false.  ! Clear selection
        if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! Always use goal column, clamped to line bounds (standard editor behavior)
            cursor%column = cursor%desired_column
            if (cursor%column > utf8_char_count(target_line) + 1) then
                cursor%column = utf8_char_count(target_line) + 1
            end if

            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine move_cursor_up

    subroutine move_cursor_down(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: target_line

        cursor%has_selection = .false.  ! Clear selection
        if (cursor%line < line_count) then
            cursor%line = cursor%line + 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! Always use goal column, clamped to line bounds (standard editor behavior)
            cursor%column = cursor%desired_column
            if (cursor%column > utf8_char_count(target_line) + 1) then
                cursor%column = utf8_char_count(target_line) + 1
            end if

            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine move_cursor_down

    subroutine move_cursor_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer :: char_count

        ! If we have a selection, move to START of selection (leftmost/earliest position)
        if (cursor%has_selection) then
            ! Find which end is further left (start of selection)
            if (cursor%selection_start_line < cursor%line .or. &
                (cursor%selection_start_line == cursor%line .and. cursor%selection_start_col < cursor%column)) then
                ! selection_start is the start - move there
                cursor%line = cursor%selection_start_line
                cursor%column = cursor%selection_start_col
            end if
            ! Otherwise cursor is already at the start
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            char_count = buffer_get_line_char_count(buffer, cursor%line)
            cursor%column = char_count + 1
            cursor%desired_column = cursor%column
        end if
    end subroutine move_cursor_left

    subroutine move_cursor_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer :: line_count, char_count

        ! If we have a selection, move to END of selection (rightmost/latest position)
        if (cursor%has_selection) then
            ! Find which end is further right (end of selection)
            if (cursor%selection_start_line > cursor%line .or. &
                (cursor%selection_start_line == cursor%line .and. cursor%selection_start_col > cursor%column)) then
                ! selection_start is the end - move there
                cursor%line = cursor%selection_start_line
                cursor%column = cursor%selection_start_col
            end if
            ! Otherwise cursor is already at the end
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

        char_count = buffer_get_line_char_count(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= char_count) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = cursor%column
        end if
    end subroutine move_cursor_right

    subroutine move_cursor_smart_home(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: first_non_whitespace, i

        cursor%has_selection = .false.  ! Clear selection

        ! Get the current line
        line = buffer_get_line(buffer, cursor%line)

        ! Find the first non-whitespace character
        first_non_whitespace = 1
        do i = 1, len(line)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then  ! Not space or tab
                first_non_whitespace = i
                exit
            end if
        end do

        ! Smart home behavior:
        ! If we're already at the first non-whitespace, go to column 1
        ! If we're at column 1, go to first non-whitespace
        ! Otherwise, go to first non-whitespace
        if (cursor%column == first_non_whitespace .and. first_non_whitespace > 1) then
            cursor%column = 1
        else
            cursor%column = first_non_whitespace
        end if

        cursor%desired_column = cursor%column

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_smart_home

    subroutine move_cursor_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        cursor%has_selection = .false.  ! Clear selection
        line = buffer_get_line(buffer, cursor%line)
        ! Columns are character indices, not bytes
        cursor%column = utf8_char_count(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_end

    subroutine move_cursor_page_up(cursor, editor)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer :: page_size

        cursor%has_selection = .false.  ! Clear selection
        ! A page is what you can see: the terminal panel's rows are not ours.
        page_size = text_area_height(editor)
        cursor%line = max(1, cursor%line - page_size)
        cursor%column = cursor%desired_column
    end subroutine move_cursor_page_up

    subroutine move_cursor_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        cursor%has_selection = .false.  ! Clear selection
        ! A page is what you can see: the terminal panel's rows are not ours.
        page_size = text_area_height(editor)
        cursor%line = min(line_count, cursor%line + page_size)
        cursor%column = cursor%desired_column
    end subroutine move_cursor_page_down

    subroutine move_cursor_word_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_len

        cursor%has_selection = .false.
        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)

        if (line_len == 0) then
            if (cursor%line > 1) then
                cursor%line = cursor%line - 1
                if (allocated(line)) deallocate(line)
                line = buffer_get_line(buffer, cursor%line)
                cursor%column = &
                    utf8_char_count(line) + 1
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
            return
        end if

        pos = utf8_char_to_byte_index(line, &
            cursor%column)
        if (pos == 0) pos = line_len + 1

        if (pos > 1 .and. line_len > 0) then
            pos = pos - 1

            do while (pos > 1 .and. pos <= line_len)
                if (line(pos:pos) /= ' ') exit
                pos = pos - 1
            end do

            if (pos >= 1 .and. pos <= line_len) then
                if (is_word_char(line(pos:pos))) then
                    do while (pos > 1)
                        if (pos-1 < 1) exit
                        if (.not. is_word_char( &
                            line(pos-1:pos-1))) exit
                        pos = pos - 1
                    end do
                end if
            end if

            if (pos < 1) pos = 1
            if (pos > line_len + 1) pos = line_len + 1

            cursor%column = &
                utf8_byte_to_char_index(line, pos)
        else if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            if (allocated(line)) deallocate(line)
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = &
                utf8_char_count(line) + 1
        else
            cursor%column = 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_left

    subroutine move_cursor_word_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_count, line_len, char_len

        cursor%has_selection = .false.
        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)
        line_len = len(line)
        char_len = utf8_char_count(line)

        pos = utf8_char_to_byte_index(line, cursor%column)
        if (pos == 0) pos = line_len + 1

        if (pos <= line_len) then
            if (line(pos:pos) == ' ') then
                do while (pos < line_len)
                    if (pos+1 <= line_len .and. &
                        line(pos+1:pos+1) == ' ') then
                        pos = pos + 1
                    else
                        exit
                    end if
                end do
                pos = pos + 1
            else if (is_word_char(line(pos:pos))) then
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (.not. is_word_char( &
                            line(pos+1:pos+1))) exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1
                do while (pos <= line_len)
                    if (line(pos:pos) /= ' ') exit
                    pos = pos + 1
                end do
            else
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (is_word_char( &
                            line(pos+1:pos+1)) .or. &
                            line(pos+1:pos+1) == ' ') exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1
                do while (pos <= line_len)
                    if (line(pos:pos) /= ' ') exit
                    pos = pos + 1
                end do
            end if

            cursor%column = &
                utf8_byte_to_char_index(line, pos)
        else if (cursor%line < line_count) then
            cursor%line = cursor%line + 1
            cursor%column = 1
        else
            cursor%column = char_len + 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_right

    function is_word_char(ch) result(is_word)
        character, intent(in) :: ch
        logical :: is_word

        is_word = (ch >= 'a' .and. ch <= 'z') .or. &
                  (ch >= 'A' .and. ch <= 'Z') .or. &
                  (ch >= '0' .and. ch <= '9') .or. &
                  ch == '_'
    end function is_word_char

    subroutine handle_backspace(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: n, i

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
            return
        end if

        if (cursor%column > 1) then
            ! Inside a run of leading spaces, unwind a whole indent level at a
            ! time. Twelve presses to undo three levels is the friction; every
            ! modern editor treats the indent as the unit here (VSCode calls it
            ! useTabStops). Only spaces: a hard tab is already one press, and
            ! one press is already the answer.
            n = spaces_back_to_tab_stop(buffer, cursor)
            if (n > 1) then
                do i = 1, n
                    cursor%column = cursor%column - 1
                    call buffer_delete_at_cursor(buffer, cursor)
                end do
                cursor%desired_column = cursor%column
                return
            end if

            ! Delete character before cursor
            cursor%column = cursor%column - 1
            call buffer_delete_at_cursor(buffer, cursor)
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Join with previous line
            call join_line_with_previous(cursor, buffer)
        end if
    end subroutine handle_backspace

    !> How many spaces one Backspace should take, when the caret sits in a run
    !> of leading spaces: enough to land on the previous tab stop.
    !>
    !> Returns 1 in every other case, which is the ordinary behaviour, so the
    !> caller can treat "1" as "nothing special here".
    function spaces_back_to_tab_stop(buffer, cursor) result(n)
        use renderer_module, only: display_offset_of
        use indent_policy_module, only: INDENT_WIDTH
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: n
        character(len=:), allocatable :: line
        integer :: display_col, target, before_bytes, i

        n = 1
        line = buffer_get_line(buffer, cursor%line)
        before_bytes = utf8_char_to_byte_index(line, cursor%column) - 1
        if (before_bytes < 1) return
        if (before_bytes > len(line)) return

        ! Everything to the caret's left must be spaces -- otherwise the caret
        ! is in the text, where deleting four characters would be destructive
        ! rather than helpful.
        do i = 1, before_bytes
            if (line(i:i) /= ' ') return
        end do

        display_col = display_offset_of(line, 1, cursor%column)
        if (display_col <= 0) return

        target = ((display_col - 1) / INDENT_WIDTH) * INDENT_WIDTH
        n = display_col - target
        if (n < 1) n = 1
        ! Only spaces are being removed, so cells and characters agree; still,
        ! never ask for more than there are.
        if (n > before_bytes) n = before_bytes
    end function spaces_back_to_tab_stop

    subroutine handle_delete(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
            return
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= utf8_char_count(line)) then
            ! Delete character at cursor
            call buffer_delete_at_cursor(buffer, cursor)
        else if (cursor%line < line_count) then
            ! Join with next line
            call join_line_with_next(cursor, buffer)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine handle_delete

    ! Auto-indent on Enter.
    !
    ! The new line's leading whitespace is REPLACED by the computed indent,
    ! never prepended to it. Prepending is what made indentation run away:
    ! whenever the caret sat left of a line's own indentation, that whitespace
    ! was part of the text pushed down onto the new line, so re-inserting the
    ! indent doubled it -- and the next Enter doubled the doubled value. On a
    ! line with text that also silently shifted the text right (`    int x;`
    ! with the caret at column 1 became `        int x;`).
    !
    ! expand_pair pushes a closing brace that sits directly after the caret
    ! onto its own line. It adds a second line, which the multi-cursor
    ! transform cannot model, so enter_multiple_cursors turns it off.
    subroutine handle_enter(cursor, buffer, filename, expand_pair)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: expand_pair
        character(len=:), allocatable :: current_line, before, after
        integer :: indent_level, new_indent, split_byte
        logical :: opens_block, closes_immediately, do_expand

        do_expand = .true.
        if (present(expand_pair)) do_expand = expand_pair

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
        end if

        current_line = buffer_get_line(buffer, cursor%line)
        indent_level = indent_width(current_line)

        ! Text either side of the caret decides whether a block is opening
        split_byte = utf8_char_to_byte_index(current_line, cursor%column)
        if (split_byte <= 0) split_byte = len(current_line) + 1
        before = current_line(1:split_byte-1)
        after = current_line(split_byte:)

        ! A block opens only when the caret is directly after '{' -- which is
        ! the one moment an extra indent level is wanted. Every later Enter on
        ! the resulting blank line just inherits the indent it already has.
        opens_block = last_nonblank_is(before, '{')
        closes_immediately = opens_block .and. first_nonblank_is(after, '}')

        new_indent = indent_level
        if (opens_block) new_indent = indent_level + ENTER_INDENT_WIDTH

        call buffer_insert_newline(buffer, cursor)
        cursor%line = cursor%line + 1
        cursor%column = 1
        call set_line_indent(buffer, cursor%line, new_indent, filename)
        cursor%column = new_indent + 1

        ! '{|}' becomes an open brace, an indented blank line for the caret,
        ! and the closer back at the outer indent
        if (closes_immediately .and. do_expand) then
            call buffer_insert_text_at(buffer, cursor%line, cursor%column, char(10))
            call set_line_indent(buffer, cursor%line + 1, indent_level, filename)
        end if

        cursor%desired_column = cursor%column
    end subroutine handle_enter

    ! Display width of a line's leading whitespace, tabs counted as 4
    pure function indent_width(line) result(width)
        character(len=*), intent(in) :: line
        integer :: width
        integer :: i

        width = 0
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                width = width + 1
            else if (line(i:i) == char(9)) then
                width = width + ENTER_INDENT_WIDTH
            else
                exit
            end if
        end do
    end function indent_width

    ! Byte count of a line's leading whitespace run
    pure function leading_ws_len(line) result(n)
        character(len=*), intent(in) :: line
        integer :: n

        n = 0
        do while (n < len(line))
            if (line(n+1:n+1) /= ' ' .and. line(n+1:n+1) /= char(9)) exit
            n = n + 1
        end do
    end function leading_ws_len

    pure function last_nonblank_is(text, ch) result(res)
        character(len=*), intent(in) :: text
        character, intent(in) :: ch
        logical :: res
        integer :: i

        res = .false.
        do i = len(text), 1, -1
            if (text(i:i) == ' ' .or. text(i:i) == char(9)) cycle
            res = text(i:i) == ch
            return
        end do
    end function last_nonblank_is

    pure function first_nonblank_is(text, ch) result(res)
        character(len=*), intent(in) :: text
        character, intent(in) :: ch
        logical :: res
        integer :: i

        res = .false.
        do i = 1, len(text)
            if (text(i:i) == ' ' .or. text(i:i) == char(9)) cycle
            res = text(i:i) == ch
            return
        end do
    end function first_nonblank_is

    ! Give a line exactly `width` columns of indentation. Leaves the line
    ! untouched when it already measures that wide, so tab-indented files keep
    ! their tabs and a repeated Enter on a blank line is a no-op rather than a
    ! doubling. Leading whitespace is ASCII, so bytes and columns agree.
    !> Replace a line's leading whitespace with `width` display columns of it.
    !>
    !> Takes the filename because the CHARACTER is not universal: this wrote
    !> spaces unconditionally, so auto-indenting inside a makefile recipe
    !> produced a "missing separator" build failure. Tab learned that in 0.22;
    !> this path did not, because the decision lived at the Tab call site.
    subroutine set_line_indent(buffer, line_num, width, filename)
        use indent_policy_module, only: indent_string_for, indent_uses_hard_tabs
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_num, width
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: line, pad
        integer :: ws

        line = buffer_get_line(buffer, line_num)
        ws = leading_ws_len(line)

        ! Already the right WIDTH: leave the characters alone. A C file
        ! indented with tabs keeps its tabs -- rewriting them as spaces because
        ! they measure the same would reformat code nobody asked to reformat.
        !
        ! The exception is a file where the character is a build error rather
        ! than a preference, and then only when it is actually wrong: a
        ! makefile recipe measured four columns wide is still broken if those
        ! four columns are spaces.
        if (indent_width(line) == width) then
            if (.not. indent_uses_hard_tabs(filename)) return
            if (ws == 0) return
            if (verify(line(1:ws), achar(9)) == 0) return
        end if

        pad = indent_string_for(filename, width)
        if (ws > 0) call buffer_delete_range(buffer, line_num, 1, line_num, ws + 1)
        if (len(pad) > 0) call buffer_insert_text_at(buffer, line_num, 1, pad)
    end subroutine set_line_indent

    !> Where a blank line's indentation ought to be, judged from the code
    !> above it.
    !>
    !> The previous NON-BLANK line, plus a level if it opens a block. Blank
    !> lines are skipped because they carry no information -- the whole problem
    !> being solved is that the line you are on has none either, so inheriting
    !> from another empty line just propagates the nothing.
    !>
    !> Brace-based, and deliberately the same rule handle_enter already uses:
    !> if Tab and Enter disagreed about where a line belongs, one of them would
    !> always be wrong.
    function expected_indent_at(buffer, line_no) result(width)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_no
        integer :: width
        character(len=:), allocatable :: prev
        integer :: i

        width = 0
        do i = line_no - 1, 1, -1
            prev = buffer_get_line(buffer, i)
            if (len_trim(prev) == 0) cycle
            width = indent_width(prev)
            if (last_nonblank_is(prev, '{')) width = width + ENTER_INDENT_WIDTH
            return
        end do
    end function expected_indent_at

    subroutine handle_tab(cursor, buffer, filename)
        use renderer_module, only: display_offset_of
        use indent_policy_module, only: indent_text_for, indent_string_for
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: line, pad
        integer :: i, display_col, target

        ! The tab stop is a DISPLAY column, and cursor%column is a character
        ! index -- equal only on a line of plain ASCII with no tabs. Convert,
        ! or Tab lands somewhere other than where the text appears.
        line = buffer_get_line(buffer, cursor%line)
        display_col = display_offset_of(line, 1, cursor%column)

        ! On a line with nothing but whitespace, one Tab goes straight to where
        ! the line belongs rather than one stop nearer it. Pressing Tab four
        ! times to get back into a nested block is the friction this removes,
        ! and it is what every modern editor does.
        !
        ! Only when the target is FURTHER RIGHT than the indent already there,
        ! so Tab never pulls a line leftwards -- that would make it a dedent
        ! key, which Shift-Tab already is. Once at the expected indent, Tab
        ! goes on stepping one level at a time, so going deeper still works.
        if (len_trim(line) == 0) then
            target = expected_indent_at(buffer, cursor%line)
            if (target > indent_width(line)) then
                call set_line_indent(buffer, cursor%line, target, filename)
                pad = indent_string_for(filename, target)
                cursor%column = len(pad) + 1
                cursor%desired_column = cursor%column
                return
            end if
        end if

        pad = indent_text_for(filename, display_col)
        do i = 1, len(pad)
            call buffer_insert_char(buffer, cursor, pad(i:i))
            cursor%column = cursor%column + 1
        end do
        cursor%desired_column = cursor%column
    end subroutine handle_tab

    subroutine indent_selection(cursor, buffer, filename)
        use indent_policy_module, only: indent_text_for
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer :: start_line, end_line, i
        character(len=:), allocatable :: unit

        if (.not. cursor%has_selection) return

        ! Get the range of lines to indent
        start_line = min(cursor%selection_start_line, cursor%line)
        end_line = max(cursor%selection_start_line, cursor%line)

        ! Every line gets one whole indent level at column 1, so the caret's
        ! own column plays no part -- ask for the unit as though at column 0.
        unit = indent_text_for(filename, 0)

        do i = start_line, end_line
            call buffer_insert_text_at(buffer, i, 1, unit)
        end do

        ! Adjust cursor position if needed
        if (cursor%column > 1) then
            cursor%column = cursor%column + len(unit)
        end if
        if (cursor%selection_start_col > 1) then
            cursor%selection_start_col = cursor%selection_start_col + len(unit)
        end if
    end subroutine indent_selection

    subroutine dedent_selection(cursor, buffer, filename)
        use indent_policy_module, only: dedent_width_at
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer :: start_line, end_line, i, spaces_to_remove
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) return

        ! Get the range of lines to dedent
        start_line = min(cursor%selection_start_line, cursor%line)
        end_line = max(cursor%selection_start_line, cursor%line)

        ! Dedent each line in the selection
        do i = start_line, end_line
            line = buffer_get_line(buffer, i)
            ! Mirror of indent_selection, hard tabs included
            spaces_to_remove = dedent_width_at(filename, line)

            ! Remove the spaces
            if (spaces_to_remove > 0) then
                call buffer_delete_range(buffer, i, 1, i, spaces_to_remove + 1)

                ! Adjust cursor position for current line
                if (i == cursor%line .and. cursor%column > spaces_to_remove) then
                    cursor%column = cursor%column - spaces_to_remove
                else if (i == cursor%line .and. cursor%column <= spaces_to_remove) then
                    cursor%column = 1
                end if

                if (i == cursor%selection_start_line .and. cursor%selection_start_col > spaces_to_remove) then
                    cursor%selection_start_col = cursor%selection_start_col - spaces_to_remove
                else if (i == cursor%selection_start_line .and. cursor%selection_start_col <= spaces_to_remove) then
                    cursor%selection_start_col = 1
                end if
            end if

            if (allocated(line)) deallocate(line)
        end do
    end subroutine dedent_selection

    subroutine dedent_current_line(cursor, buffer, filename)
        use indent_policy_module, only: dedent_width_at
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: line
        integer :: spaces_to_remove

        line = buffer_get_line(buffer, cursor%line)

        ! Remove exactly what one Tab would have added here, so indent and
        ! dedent are inverses -- including a hard tab in a makefile, which the
        ! old space-only scan stepped straight over and left in place.
        spaces_to_remove = dedent_width_at(filename, line)

        ! Remove the spaces
        if (spaces_to_remove > 0) then
            call buffer_delete_range(buffer, cursor%line, 1, cursor%line, spaces_to_remove + 1)

            ! Adjust cursor position
            if (cursor%column > spaces_to_remove) then
                cursor%column = cursor%column - spaces_to_remove
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine dedent_current_line

    ! ch may be a full multibyte UTF-8 character; it advances the cursor by
    ! one character column regardless of byte length
    subroutine insert_char(cursor, buffer, ch)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: ch
        character :: closing_char
        logical :: should_auto_close, should_wrap
        integer :: start_line, start_col, end_line, end_col

        ! Step over a closer auto-close already inserted rather than typing a
        ! duplicate. Checked before classification because a quote is both an
        ! opener and a closer -- inside an auto-closed '""' the second '"'
        ! must close, not open a new pair.
        if (len(ch) == 1) then
            if (overtypes_pending_closer(cursor, buffer, ch(1:1))) then
                g_pending_closer_count = g_pending_closer_count - 1
                cursor%column = cursor%column + 1
                cursor%desired_column = cursor%column
                return
            end if
        end if

        ! Check if we should auto-close or wrap brackets/quotes
        call classify_auto_close(ch, closing_char, should_auto_close)
        should_wrap = should_auto_close .and. cursor%has_selection

        ! If we should wrap, don't delete - wrap the selection instead
        if (should_wrap) then
            ! Find selection bounds
            if (cursor%line < cursor%selection_start_line .or. &
                (cursor%line == cursor%selection_start_line .and. cursor%column < cursor%selection_start_col)) then
                start_line = cursor%line
                start_col = cursor%column
                end_line = cursor%selection_start_line
                end_col = cursor%selection_start_col
            else
                start_line = cursor%selection_start_line
                start_col = cursor%selection_start_col
                end_line = cursor%line
                end_col = cursor%column
            end if

            ! Insert closing character at end
            cursor%line = end_line
            cursor%column = end_col
            call buffer_insert_char(buffer, cursor, closing_char)

            ! Insert opening character at start
            cursor%line = start_line
            cursor%column = start_col
            call buffer_insert_char(buffer, cursor, ch)

            ! Position cursor after the opening bracket (inside the wrapped text)
            cursor%column = start_col + 1
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

        ! Delete selection if one exists (normal behavior)
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
        end if

        ! Insert the character (whole UTF-8 sequence, one column)
        call buffer_insert_string(buffer, cursor, ch)
        cursor%column = cursor%column + 1

        ! If auto-close is enabled, insert the closing character
        if (should_auto_close) then
            call buffer_insert_char(buffer, cursor, closing_char)
            ! Don't move cursor forward - stay between the brackets/quotes
            call push_pending_closer(closing_char)
        end if

        cursor%desired_column = cursor%column
    end subroutine insert_char

    ! ---- Multi-cursor coordinate transforms ------------------------------
    ! Any buffer edit made for one cursor moves the text every other cursor
    ! (and selection anchor) points into. After each per-cursor edit the
    ! helpers below shift all other cursors the way the text moved; with
    ! that, cursors sharing a line stay glued to their characters and
    ! processing order cannot corrupt positions. All columns are UTF-8
    ! character columns, matching cursor_t.

    ! Transform one point for a block insertion that began at (l, c) and
    ! left the inserting cursor at (l2, c2). Single char: (l,c)->(l,c+1);
    ! newline+indent: (l,c)->(l+1,indent+1); paste: end of pasted block.
    subroutine mc_point_after_insert(pl, pc, l, c, l2, c2)
        integer(int32), intent(inout) :: pl, pc
        integer, intent(in) :: l, c, l2, c2

        if (pl == l .and. pc >= c) then
            pl = int(l2, int32)
            pc = int(c2 + (pc - c), int32)
        else if (pl > l) then
            pl = pl + int(l2 - l, int32)
        end if
    end subroutine mc_point_after_insert

    ! Transform one point for deletion of the normalized, end-exclusive
    ! range (sl,sc)..(el,ec). Points inside the range collapse to its start.
    subroutine mc_point_after_delete(pl, pc, sl, sc, el, ec)
        integer(int32), intent(inout) :: pl, pc
        integer, intent(in) :: sl, sc, el, ec

        if (pl == el .and. pc >= ec) then
            pl = int(sl, int32)
            pc = int(sc + (pc - ec), int32)
        else if ((pl == sl .and. pc >= sc .and. (sl /= el .or. pc < ec)) .or. &
                 (pl > sl .and. pl < el) .or. &
                 (pl == el .and. sl /= el .and. pc < ec)) then
            pl = int(sl, int32)
            pc = int(sc, int32)
        else if (pl > el) then
            pl = pl - int(el - sl, int32)
        end if
    end subroutine mc_point_after_delete

    subroutine mc_others_inserted(editor, skip, l, c, l2, c2)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: skip, l, c, l2, c2
        integer :: k

        do k = 1, size(editor%cursors)
            if (k == skip) cycle
            call mc_point_after_insert(editor%cursors(k)%line, &
                editor%cursors(k)%column, l, c, l2, c2)
            editor%cursors(k)%desired_column = editor%cursors(k)%column
            if (editor%cursors(k)%has_selection) then
                call mc_point_after_insert(editor%cursors(k)%selection_start_line, &
                    editor%cursors(k)%selection_start_col, l, c, l2, c2)
            end if
        end do
    end subroutine mc_others_inserted

    subroutine mc_others_deleted(editor, skip, sl, sc, el, ec)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: skip, sl, sc, el, ec
        integer :: k

        do k = 1, size(editor%cursors)
            if (k == skip) cycle
            call mc_point_after_delete(editor%cursors(k)%line, &
                editor%cursors(k)%column, sl, sc, el, ec)
            editor%cursors(k)%desired_column = editor%cursors(k)%column
            if (editor%cursors(k)%has_selection) then
                call mc_point_after_delete(editor%cursors(k)%selection_start_line, &
                    editor%cursors(k)%selection_start_col, sl, sc, el, ec)
            end if
        end do
    end subroutine mc_others_deleted

    ! Normalized selection bounds (start <= end; end-exclusive column)
    subroutine normalize_selection(cursor, sl, sc, el, ec)
        type(cursor_t), intent(in) :: cursor
        integer, intent(out) :: sl, sc, el, ec

        if (cursor%line < cursor%selection_start_line .or. &
            (cursor%line == cursor%selection_start_line .and. &
             cursor%column < cursor%selection_start_col)) then
            sl = cursor%line
            sc = cursor%column
            el = cursor%selection_start_line
            ec = cursor%selection_start_col
        else
            sl = cursor%selection_start_line
            sc = cursor%selection_start_col
            el = cursor%line
            ec = cursor%column
        end if
    end subroutine normalize_selection

    subroutine push_pending_closer(ch)
        character, intent(in) :: ch

        if (g_pending_closer_count >= MAX_PENDING_CLOSERS) return
        g_pending_closer_count = g_pending_closer_count + 1
        g_pending_closers(g_pending_closer_count) = ch
    end subroutine push_pending_closer

    ! Anything other than typing text breaks the association between the
    ! caret and the closer auto-close put in front of it
    subroutine clear_pending_closers()
        g_pending_closer_count = 0
    end subroutine clear_pending_closers

    ! True when ch is the innermost pending closer AND it is the character
    ! sitting at the caret, i.e. typing it would duplicate it
    !> Drop the auto-closed character when a suggestion brought its own.
    !>
    !> Typing the opening quote of an include auto-closes it, so the line holds
    !> "|" with the caret inside. clangd's completion for a header is
    !> showme.h" -- it carries the closing quote, because from the server's
    !> point of view it is completing the whole reference. Inserting that on
    !> top of the auto-closed one gave "showme.h"" and left the user deleting
    !> a character by hand.
    !>
    !> Only when all three line up: the completion ends with that character,
    !> the innermost pending closer IS that character, and it is sitting
    !> immediately after the caret. A closer the user typed themselves is not
    !> tracked as pending, so this cannot eat one of those.
    subroutine absorb_closer_the_completion_supplied(editor, buffer, text)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: text
        character :: last_ch
        integer :: c

        if (len(text) == 0) return
        last_ch = text(len(text):len(text))

        c = editor%active_cursor
        if (c < 1 .or. c > size(editor%cursors)) return
        if (.not. overtypes_pending_closer(editor%cursors(c), buffer, last_ch)) return

        call buffer_delete_at_cursor(buffer, editor%cursors(c))
        g_pending_closer_count = g_pending_closer_count - 1
    end subroutine absorb_closer_the_completion_supplied

    function overtypes_pending_closer(cursor, buffer, ch) result(res)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character, intent(in) :: ch
        logical :: res

        res = .false.
        if (cursor%has_selection) return
        if (g_pending_closer_count <= 0) return
        if (g_pending_closers(g_pending_closer_count) /= ch) return
        res = buffer_char_at(buffer, cursor%line, cursor%column) == ch
    end function overtypes_pending_closer

    ! Auto-close/wrap classification shared by the single- and multi-cursor
    ! insert paths, so their behavior can never diverge
    subroutine classify_auto_close(ch, closing_char, should_auto_close)
        character(len=*), intent(in) :: ch
        character, intent(out) :: closing_char
        logical, intent(out) :: should_auto_close

        should_auto_close = .true.
        select case(ch)
        case('(')
            closing_char = ')'
        case('[')
            closing_char = ']'
        case('{')
            closing_char = '}'
        case('"')
            closing_char = '"'
        case("'")
            closing_char = "'"
        case('`')
            closing_char = '`'
        case default
            closing_char = ' '
            should_auto_close = .false.
        end select
    end subroutine classify_auto_close

    ! Insert a character at every cursor with full parity with the single-
    ! cursor path: selection wrap for brackets/quotes, selection replace,
    ! and bracket auto-close.
    subroutine insert_char_multiple_cursors(editor, buffer, ch)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: ch
        character :: closing_char
        logical :: should_auto_close, all_overtype
        integer :: i, sl, sc, el, ec, l0, c0

        ! Overtype only when EVERY cursor is sitting in front of the same
        ! pending closer. Cursors run in lockstep here, and stepping some over
        ! while inserting at others would desynchronise them.
        if (len(ch) == 1 .and. g_pending_closer_count > 0) then
            all_overtype = .true.
            do i = 1, size(editor%cursors)
                if (.not. overtypes_pending_closer(editor%cursors(i), buffer, ch(1:1))) then
                    all_overtype = .false.
                    exit
                end if
            end do
            if (all_overtype) then
                g_pending_closer_count = g_pending_closer_count - 1
                do i = 1, size(editor%cursors)
                    editor%cursors(i)%column = editor%cursors(i)%column + 1
                    editor%cursors(i)%desired_column = editor%cursors(i)%column
                end do
                return
            end if
        end if

        call classify_auto_close(ch, closing_char, should_auto_close)
        call sort_cursors_by_position(editor)

        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection .and. should_auto_close) then
                ! Wrap the selection in the pair. Closing char first, so the
                ! opening insert cannot shift the end position.
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                editor%cursors(i)%line = el
                editor%cursors(i)%column = ec
                call buffer_insert_char(buffer, editor%cursors(i), closing_char)
                call mc_others_inserted(editor, i, el, ec, el, ec + 1)
                editor%cursors(i)%line = sl
                editor%cursors(i)%column = sc
                call buffer_insert_char(buffer, editor%cursors(i), ch)
                call mc_others_inserted(editor, i, sl, sc, sl, sc + 1)
                editor%cursors(i)%column = sc + 1
                editor%cursors(i)%has_selection = .false.
                editor%cursors(i)%desired_column = editor%cursors(i)%column
                cycle
            end if

            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
                call mc_others_deleted(editor, i, sl, sc, el, ec)
            end if

            l0 = editor%cursors(i)%line
            c0 = editor%cursors(i)%column
            ! Insert character (whole UTF-8 sequence, one column)
            call buffer_insert_string(buffer, editor%cursors(i), ch)
            editor%cursors(i)%column = editor%cursors(i)%column + 1
            editor%cursors(i)%desired_column = editor%cursors(i)%column
            if (should_auto_close) then
                ! Cursor stays between the pair
                call buffer_insert_char(buffer, editor%cursors(i), closing_char)
                if (i == 1) call push_pending_closer(closing_char)
                call mc_others_inserted(editor, i, l0, c0, l0, c0 + 2)
            else
                call mc_others_inserted(editor, i, l0, c0, l0, c0 + 1)
            end if
        end do

        call deduplicate_cursors(editor)
    end subroutine insert_char_multiple_cursors

    ! Backspace at every cursor: same-line peers shift left with the text;
    ! a line join re-homes every cursor on and below the joined line
    subroutine backspace_multiple_cursors(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, sl, sc, el, ec

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
                call mc_others_deleted(editor, i, sl, sc, el, ec)
            else if (editor%cursors(i)%column > 1) then
                editor%cursors(i)%column = editor%cursors(i)%column - 1
                call buffer_delete_at_cursor(buffer, editor%cursors(i))
                editor%cursors(i)%desired_column = editor%cursors(i)%column
                call mc_others_deleted(editor, i, &
                    editor%cursors(i)%line, editor%cursors(i)%column, &
                    editor%cursors(i)%line, editor%cursors(i)%column + 1)
            else if (editor%cursors(i)%line > 1) then
                ! Join with previous line; the deleted range is the newline
                call join_line_with_previous(editor%cursors(i), buffer)
                call mc_others_deleted(editor, i, &
                    editor%cursors(i)%line, editor%cursors(i)%column, &
                    editor%cursors(i)%line + 1, 1)
            end if
        end do
        call deduplicate_cursors(editor)
    end subroutine backspace_multiple_cursors

    ! alt-backspace at every cursor. Each deletion shifts the cursors that
    ! sat after it -- including across a line join, which re-homes every
    ! cursor below onto the joined line.
    subroutine delete_word_backward_multiple_cursors(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, sl, sc, el, ec

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
                call mc_others_deleted(editor, i, sl, sc, el, ec)
                cycle
            end if
            call delete_word_backward(editor%cursors(i), buffer, sl, sc, el, ec)
            if (sl /= el .or. sc /= ec) call mc_others_deleted(editor, i, sl, sc, el, ec)
        end do
        call deduplicate_cursors(editor)
    end subroutine delete_word_backward_multiple_cursors

    ! Remove whole lines outright -- no clipboard, no yank stack. Everything
    ! else that removes a line (ctrl-x, ctrl-k) captures the text first; this
    ! is the one that simply discards it.
    subroutine delete_lines(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, k, first, last, line_count, start_pos, end_pos
        integer :: a, b

        line_count = buffer_get_line_count(buffer)

        ! Highest line range first, so lower line numbers stay valid
        call sort_cursors_by_position(editor)
        do i = size(editor%cursors), 1, -1
            call cursor_line_span(editor%cursors(i), first, last)
            first = max(1, min(first, line_count))
            last = max(1, min(last, line_count))
            if (last < first) cycle

            start_pos = get_line_start_pos(buffer, first)
            if (last < line_count) then
                ! Take the trailing newline with the block
                end_pos = get_line_start_pos(buffer, last + 1)
            else
                end_pos = get_buffer_content_size(buffer) + 1
                ! Last line of the file: take the newline that precedes it
                ! instead, so deleting it does not leave a blank line behind
                if (first > 1) start_pos = get_line_start_pos(buffer, first) - 1
            end if

            if (end_pos > start_pos) then
                call buffer_delete(buffer, start_pos, end_pos - start_pos)
                buffer%modified = .true.
            end if
            line_count = buffer_get_line_count(buffer)

            ! Re-home every cursor that sat on or below the deleted block
            do k = 1, size(editor%cursors)
                a = editor%cursors(k)%line
                if (a > last) then
                    editor%cursors(k)%line = a - (last - first + 1)
                else if (a >= first) then
                    editor%cursors(k)%line = first
                    editor%cursors(k)%column = 1
                end if
                editor%cursors(k)%line = max(1, min(editor%cursors(k)%line, line_count))
                b = buffer_get_line_char_count(buffer, editor%cursors(k)%line) + 1
                editor%cursors(k)%column = max(1, min(editor%cursors(k)%column, b))
                editor%cursors(k)%has_selection = .false.
                editor%cursors(k)%desired_column = editor%cursors(k)%column
            end do
        end do

        call deduplicate_cursors(editor)
    end subroutine delete_lines

    ! Lines one cursor covers. A selection ending in column 1 does not drag
    ! that line in, matching the comment-toggle rule.
    subroutine cursor_line_span(cursor, first, last)
        type(cursor_t), intent(in) :: cursor
        integer, intent(out) :: first, last
        integer :: end_col

        if (.not. cursor%has_selection) then
            first = cursor%line
            last = cursor%line
            return
        end if

        if (cursor%selection_start_line < cursor%line .or. &
            (cursor%selection_start_line == cursor%line .and. &
             cursor%selection_start_col <= cursor%column)) then
            first = cursor%selection_start_line
            last = cursor%line
            end_col = cursor%column
        else
            first = cursor%line
            last = cursor%selection_start_line
            end_col = cursor%selection_start_col
        end if

        if (last > first .and. end_col <= 1) last = last - 1
    end subroutine cursor_line_span

    subroutine delete_multiple_cursors(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: i, sl, sc, el, ec

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
                call mc_others_deleted(editor, i, sl, sc, el, ec)
                cycle
            end if
            line = buffer_get_line(buffer, editor%cursors(i)%line)
            if (editor%cursors(i)%column <= utf8_char_count(line)) then
                call buffer_delete_at_cursor(buffer, editor%cursors(i))
                call mc_others_deleted(editor, i, &
                    editor%cursors(i)%line, editor%cursors(i)%column, &
                    editor%cursors(i)%line, editor%cursors(i)%column + 1)
            else if (editor%cursors(i)%line < buffer_get_line_count(buffer)) then
                ! Join with next line; the deleted range is the newline
                call join_line_with_next(editor%cursors(i), buffer)
                call mc_others_deleted(editor, i, &
                    editor%cursors(i)%line, editor%cursors(i)%column, &
                    editor%cursors(i)%line + 1, 1)
            end if
        end do
        call deduplicate_cursors(editor)
    end subroutine delete_multiple_cursors

    ! Enter at every cursor. handle_enter's newline+auto-indent is one block
    ! insertion from the pre-split point to the cursor's landing position,
    ! so the generic insert transform covers same-line peers (they move to
    ! the new line, keeping their offset past the split) and lines below.
    subroutine enter_multiple_cursors(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, sl, sc, el, ec, l0, c0

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
                call mc_others_deleted(editor, i, sl, sc, el, ec)
            end if
            l0 = editor%cursors(i)%line
            c0 = editor%cursors(i)%column
            ! expand_pair off: pushing a closer onto its own line adds a
            ! second line, which mc_others_inserted cannot represent
            call handle_enter(editor%cursors(i), buffer, &
                              active_pane_filename(editor), expand_pair=.false.)
            call mc_others_inserted(editor, i, l0, c0, &
                editor%cursors(i)%line, editor%cursors(i)%column)
        end do
        call deduplicate_cursors(editor)
    end subroutine enter_multiple_cursors

    ! Paste the clipboard at every cursor. paste_clipboard leaves the
    ! cursor at the end of the inserted block, which is exactly the block
    ! insert transform's end point.
    subroutine paste_multiple_cursors(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, l0, c0

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            l0 = editor%cursors(i)%line
            c0 = editor%cursors(i)%column
            call paste_clipboard(editor%cursors(i), buffer)
            call mc_others_inserted(editor, i, l0, c0, &
                editor%cursors(i)%line, editor%cursors(i)%column)
        end do
        call deduplicate_cursors(editor)
    end subroutine paste_multiple_cursors

    ! Same for a bracketed-paste block of text
    subroutine paste_text_multiple_cursors(editor, buffer, text)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: text
        integer :: i, l0, c0

        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            l0 = editor%cursors(i)%line
            c0 = editor%cursors(i)%column
            call insert_text_block(editor%cursors(i), buffer, text)
            call mc_others_inserted(editor, i, l0, c0, &
                editor%cursors(i)%line, editor%cursors(i)%column)
        end do
        call deduplicate_cursors(editor)
    end subroutine paste_text_multiple_cursors

    subroutine tab_multiple_cursors(editor, buffer)
        use indent_policy_module, only: indent_text_for
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, ln, sl, sc, el, ec, l0, c0, w
        character(len=:), allocatable :: fname

        fname = active_pane_filename(editor)
        call sort_cursors_by_position(editor)
        do i = 1, size(editor%cursors)
            if (editor%cursors(i)%has_selection) then
                call normalize_selection(editor%cursors(i), sl, sc, el, ec)
                call indent_selection(editor%cursors(i), buffer, fname)
                ! One indent unit went in at the start of each selected line.
                ! Its width is no longer always four -- a makefile inserts a
                ! single tab -- so ask rather than assume, or every other
                ! cursor on those lines is shifted by the wrong amount.
                w = len(indent_text_for(fname, 0))
                ! Insert point col 2 matches indent_selection's own
                ! convention: a cursor parked at column 1 stays put.
                do ln = sl, el
                    call mc_others_inserted(editor, i, ln, 2, ln, 2 + w)
                end do
            else
                l0 = editor%cursors(i)%line
                c0 = editor%cursors(i)%column
                call handle_tab(editor%cursors(i), buffer, fname)
                ! Tab stops mean the width depends on where this cursor was,
                ! so each cursor can insert a different amount. Measure it.
                w = editor%cursors(i)%column - c0
                call mc_others_inserted(editor, i, l0, c0, l0, c0 + w)
            end if
        end do
        call deduplicate_cursors(editor)
    end subroutine tab_multiple_cursors

    subroutine delete_selection(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: start_line, start_col, end_line, end_col
        integer :: i
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) return

        ! Determine start and end of selection
        if (cursor%line < cursor%selection_start_line .or. &
            (cursor%line == cursor%selection_start_line .and. &
             cursor%column < cursor%selection_start_col)) then
            start_line = cursor%line
            start_col = cursor%column
            end_line = cursor%selection_start_line
            end_col = cursor%selection_start_col
        else
            start_line = cursor%selection_start_line
            start_col = cursor%selection_start_col
            end_line = cursor%line
            end_col = cursor%column
        end if

        ! Delete the selection
        if (start_line == end_line) then
            ! Single-line selection
            line = buffer_get_line(buffer, start_line)
            cursor%line = start_line
            cursor%column = start_col
            do i = start_col, end_col - 1
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            if (allocated(line)) deallocate(line)
        else
            ! Multi-line selection
            ! Delete from start_col to end of first line
            cursor%line = start_line
            cursor%column = start_col
            line = buffer_get_line(buffer, start_line)
            do i = start_col, utf8_char_count(line)
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            if (allocated(line)) deallocate(line)

            ! Delete entire lines in between
            do i = start_line + 1, end_line - 1
                ! After deleting from first line, the next line moves up
                ! So we keep deleting line at position start_line + 1
                if (buffer_get_line_count(buffer) > start_line) then
                    ! Delete the newline to join with next line
                    line = buffer_get_line(buffer, start_line)
                    cursor%column = utf8_char_count(line) + 1
                    call buffer_delete_at_cursor(buffer, cursor)  ! Delete newline
                    if (allocated(line)) deallocate(line)

                    ! Delete all content of the joined line
                    line = buffer_get_line(buffer, start_line)
                    cursor%column = utf8_char_count(line)
                    do while (cursor%column > start_col .and. cursor%column > 0)
                        call buffer_delete_at_cursor(buffer, cursor)
                        cursor%column = cursor%column - 1
                    end do
                    if (allocated(line)) deallocate(line)
                end if
            end do

            ! Delete from beginning of last line to end_col
            if (buffer_get_line_count(buffer) > start_line) then
                line = buffer_get_line(buffer, start_line)
                cursor%column = utf8_char_count(line) + 1
                call buffer_delete_at_cursor(buffer, cursor)  ! Delete newline
                if (allocated(line)) deallocate(line)

                ! Delete from start to end_col
                cursor%column = start_col
                do i = 1, end_col - 1
                    if (cursor%column <= buffer_get_line_count(buffer)) then
                        call buffer_delete_at_cursor(buffer, cursor)
                    end if
                end do
            end if

            cursor%line = start_line
            cursor%column = start_col
        end if

        cursor%has_selection = .false.
    end subroutine delete_selection

    function get_selection_text(cursor, buffer) result(text)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text
        integer :: start_line, start_col, end_line, end_col
        integer :: i
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) then
            allocate(character(len=0) :: text)
            return
        end if

        ! Determine start and end of selection
        if (cursor%line < cursor%selection_start_line .or. &
            (cursor%line == cursor%selection_start_line .and. &
             cursor%column < cursor%selection_start_col)) then
            start_line = cursor%line
            start_col = cursor%column
            end_line = cursor%selection_start_line
            end_col = cursor%selection_start_col
        else
            start_line = cursor%selection_start_line
            start_col = cursor%selection_start_col
            end_line = cursor%line
            end_col = cursor%column
        end if

        ! Extract text based on selection. Selection columns are character
        ! indices; slicing the line needs the matching byte positions.
        if (start_line == end_line) then
            ! Single-line selection
            line = buffer_get_line(buffer, start_line)
            block
                integer :: bs, be
                if (allocated(line) .and. end_col > start_col .and. &
                    start_col <= utf8_char_count(line) + 1 .and. &
                    end_col <= utf8_char_count(line) + 1) then
                    bs = utf8_char_to_byte_index(line, start_col)
                    be = utf8_char_to_byte_index(line, end_col)
                    if (bs > 0 .and. be > bs) then
                        text = line(bs:be - 1)
                    else
                        allocate(character(len=0) :: text)
                    end if
                else
                    allocate(character(len=0) :: text)
                end if
            end block
            if (allocated(line)) deallocate(line)
        else
            ! Multi-line selection
            text = ""

            ! First line (from start_col to end)
            line = buffer_get_line(buffer, start_line)
            if (allocated(line)) then
                block
                    integer :: bs
                    if (start_col <= utf8_char_count(line)) then
                        bs = utf8_char_to_byte_index(line, start_col)
                        if (bs > 0) text = text // line(bs:)
                    end if
                end block
                text = text // char(10)  ! newline
                deallocate(line)
            end if

            ! Middle lines (complete lines)
            do i = start_line + 1, end_line - 1
                line = buffer_get_line(buffer, i)
                if (allocated(line)) then
                    text = text // line // char(10)
                    deallocate(line)
                end if
            end do

            ! Last line (from beginning to end_col)
            line = buffer_get_line(buffer, end_line)
            if (allocated(line)) then
                block
                    integer :: be
                    if (end_col > 1 .and. end_col <= utf8_char_count(line) + 1) then
                        be = utf8_char_to_byte_index(line, end_col)
                        if (be > 1) text = text // line(1:be - 1)
                    end if
                end block
                deallocate(line)
            end if
        end if
    end function get_selection_text

    subroutine sort_cursors_by_position(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t) :: temp
        integer :: i, j
        logical :: swapped

        ! Simple bubble sort for small number of cursors
        do i = 1, size(editor%cursors) - 1
            swapped = .false.
            do j = 1, size(editor%cursors) - i
                if (editor%cursors(j)%line > editor%cursors(j+1)%line .or. &
                    (editor%cursors(j)%line == editor%cursors(j+1)%line .and. &
                     editor%cursors(j)%column > editor%cursors(j+1)%column)) then
                    temp = editor%cursors(j)
                    editor%cursors(j) = editor%cursors(j+1)
                    editor%cursors(j+1) = temp
                    if (editor%active_cursor == j) then
                        editor%active_cursor = j + 1
                    else if (editor%active_cursor == j + 1) then
                        editor%active_cursor = j
                    end if
                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit
        end do
    end subroutine sort_cursors_by_position

    subroutine deduplicate_cursors(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t), allocatable :: unique_cursors(:)
        integer :: i, j, unique_count, duplicate_of
        logical :: is_duplicate
        integer, allocatable :: old_to_new_map(:)

        if (size(editor%cursors) <= 1) return

        ! Allocate mapping from old cursor indices to new indices
        allocate(old_to_new_map(size(editor%cursors)))
        old_to_new_map = 0

        ! Count unique cursors
        unique_count = 0
        do i = 1, size(editor%cursors)
            is_duplicate = .false.
            do j = 1, i-1
                if (editor%cursors(i)%line == editor%cursors(j)%line .and. &
                    editor%cursors(i)%column == editor%cursors(j)%column) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) then
                unique_count = unique_count + 1
            end if
        end do

        ! If we have duplicates, create new array with only unique cursors
        if (unique_count < size(editor%cursors)) then
            allocate(unique_cursors(unique_count))
            unique_count = 0
            do i = 1, size(editor%cursors)
                is_duplicate = .false.
                duplicate_of = 0
                do j = 1, i-1
                    if (editor%cursors(i)%line == editor%cursors(j)%line .and. &
                        editor%cursors(i)%column == editor%cursors(j)%column) then
                        is_duplicate = .true.
                        duplicate_of = j
                        exit
                    end if
                end do
                if (.not. is_duplicate) then
                    unique_count = unique_count + 1
                    unique_cursors(unique_count) = editor%cursors(i)
                    old_to_new_map(i) = unique_count
                else
                    ! This cursor is a duplicate of an earlier one
                    ! Map it to the same new index as the earlier cursor
                    old_to_new_map(i) = old_to_new_map(duplicate_of)
                end if
            end do

            ! Update active cursor using the mapping
            if (editor%active_cursor > 0 .and. editor%active_cursor <= size(old_to_new_map)) then
                editor%active_cursor = old_to_new_map(editor%active_cursor)
            end if
            ! Ensure active_cursor is valid
            if (editor%active_cursor < 1 .or. editor%active_cursor > unique_count) then
                editor%active_cursor = 1
            end if

            deallocate(editor%cursors)
            editor%cursors = unique_cursors
            deallocate(old_to_new_map)
        end if
    end subroutine deduplicate_cursors

    subroutine join_line_with_previous(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: prev_line
        integer :: new_column

        prev_line = buffer_get_line(buffer, cursor%line - 1)
        new_column = utf8_char_count(prev_line) + 1

        ! Move to end of previous line
        cursor%line = cursor%line - 1
        cursor%column = new_column

        ! Delete the newline
        call buffer_delete_at_cursor(buffer, cursor)

        cursor%desired_column = cursor%column
        if (allocated(prev_line)) deallocate(prev_line)
    end subroutine join_line_with_previous

    subroutine join_line_with_next(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer

        ! Delete the newline at end of current line
        call buffer_delete_at_cursor(buffer, cursor)
    end subroutine join_line_with_next

    subroutine kill_line_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        character(len=:), allocatable :: killed_text
        integer :: i

        cursor%has_selection = .false.  ! Clear selection

        line = buffer_get_line(buffer, cursor%line)

        if (cursor%column <= utf8_char_count(line)) then
            ! Kill from cursor to end of line (column is a character
            ! index; slice the line at the matching byte position)
            killed_text = line(utf8_char_to_byte_index(line, cursor%column):)
            do i = cursor%column, utf8_char_count(line)
                call buffer_delete_at_cursor(buffer, cursor)
            end do
        else
            ! At end of line - kill the newline
            killed_text = char(10)  ! newline
            call buffer_delete_at_cursor(buffer, cursor)
        end if

        ! Add to yank stack
        if (len(killed_text) > 0) then
            call push_yank(yank_stack, killed_text)
        end if

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
        if (allocated(killed_text)) deallocate(killed_text)
    end subroutine kill_line_forward

    subroutine kill_line_backward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        character(len=:), allocatable :: killed_text
        integer :: i, start_col

        cursor%has_selection = .false.  ! Clear selection

        line = buffer_get_line(buffer, cursor%line)
        start_col = cursor%column

        if (cursor%column > 1) then
            ! Kill from start of line to cursor (byte slice for the yank
            ! text; the deletes are per character)
            killed_text = line(1:utf8_char_to_byte_index(line, cursor%column) - 1)
            cursor%column = 1
            do i = 1, start_col - 1
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            cursor%desired_column = 1
        end if

        ! Add to yank stack
        if (allocated(killed_text)) then
            if (len(killed_text) > 0) then
                call push_yank(yank_stack, killed_text)
            end if
        end if

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
        if (allocated(killed_text)) deallocate(killed_text)
    end subroutine kill_line_backward

    subroutine yank_text(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text

        text = pop_yank(yank_stack)
        if (allocated(text)) then
            call insert_text_block(cursor, buffer, text)
            deallocate(text)
        end if
    end subroutine yank_text

    subroutine move_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, prev_line
        integer :: saved_column, original_line, total_lines

        if (cursor%line <= 1) return

        ! Save state
        saved_column = cursor%column
        original_line = cursor%line
        total_lines = buffer_get_line_count(buffer)

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        prev_line = buffer_get_line(buffer, cursor%line - 1)

        ! Delete current line entirely (including newline)
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Move to previous line (now current_line position after delete)
        cursor%line = cursor%line - 1
        cursor%column = 1

        ! Delete previous line entirely (including newline)
        call delete_entire_line(buffer, cursor)

        ! Now insert current_line first, then prev_line
        cursor%column = 1
        call insert_line_text(buffer, cursor, current_line)
        call buffer_insert_newline(buffer, cursor)

        cursor%line = cursor%line + 1
        cursor%column = 1
        call insert_line_text(buffer, cursor, prev_line)
        ! Add newline if we're not at the last line
        if (original_line < total_lines) then
            call buffer_insert_newline(buffer, cursor)
        end if

        ! Restore cursor to moved line
        cursor%line = cursor%line - 1
        cursor%column = min(saved_column, utf8_char_count(current_line) + 1)
        cursor%desired_column = cursor%column

        buffer%modified = .true.
        if (allocated(current_line)) deallocate(current_line)
        if (allocated(prev_line)) deallocate(prev_line)
    end subroutine move_line_up

    subroutine move_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, next_line
        integer :: line_count, saved_column, original_line, total_lines

        line_count = buffer_get_line_count(buffer)
        if (cursor%line >= line_count) return

        ! Save state
        saved_column = cursor%column
        original_line = cursor%line
        total_lines = line_count

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        next_line = buffer_get_line(buffer, cursor%line + 1)

        ! Delete current line entirely (including newline)
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Delete next line entirely (including newline)
        ! After deleting current line, next line is now at cursor%line
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Now insert next_line first, then current_line
        cursor%column = 1
        call insert_line_text(buffer, cursor, next_line)
        call buffer_insert_newline(buffer, cursor)

        cursor%line = cursor%line + 1
        cursor%column = 1
        call insert_line_text(buffer, cursor, current_line)
        ! Add newline if we're not at the last line
        if (original_line + 1 < total_lines) then
            call buffer_insert_newline(buffer, cursor)
        end if

        ! Restore cursor position on moved line
        ! Current line is now at cursor%line (which is original_line + 1)
        ! So cursor is already on the moved line, just need to fix column
        cursor%column = min(saved_column, utf8_char_count(current_line) + 1)
        cursor%desired_column = cursor%column

        buffer%modified = .true.
        if (allocated(current_line)) deallocate(current_line)
        if (allocated(next_line)) deallocate(next_line)
    end subroutine move_line_down

    subroutine duplicate_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, cursor%line)

        ! Move to start of line
        cursor%column = 1
        ! Insert newline before
        call buffer_insert_newline(buffer, cursor)
        ! Insert the duplicated text
        call insert_line_text(buffer, cursor, line)
        ! Stay on original line
        cursor%line = cursor%line + 1

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_up

    subroutine duplicate_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: saved_column

        line = buffer_get_line(buffer, cursor%line)
        saved_column = cursor%column

        ! Move to end of line
        cursor%column = utf8_char_count(line) + 1
        ! Insert newline
        call buffer_insert_newline(buffer, cursor)
        cursor%line = cursor%line + 1
        cursor%column = 1
        ! Insert the duplicated text
        call insert_line_text(buffer, cursor, line)

        ! Return to original position
        cursor%line = cursor%line - 1
        cursor%column = saved_column
        cursor%desired_column = saved_column

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_down

    subroutine delete_entire_line(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=:), allocatable :: line
        integer :: i

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = 1

        ! Delete all characters in line (character count, not bytes)
        do i = 1, utf8_char_count(line)
            call buffer_delete_at_cursor(buffer, cursor)
        end do

        ! Delete the newline if not the last line
        if (cursor%line < buffer_get_line_count(buffer)) then
            call buffer_delete_at_cursor(buffer, cursor)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_entire_line

    subroutine insert_line_text(buffer, cursor, text)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: text
        integer :: i, nb

        ! Insert one whole UTF-8 character at a time: the column is a
        ! character index, so inserting byte-by-byte would scatter the
        ! bytes of a multibyte char across character positions
        i = 1
        do while (i <= len(text))
            nb = min(utf8_lead_len(text(i:i)), len(text) - i + 1)
            call buffer_insert_string(buffer, cursor, text(i:i+nb-1))
            cursor%column = cursor%column + 1
            i = i + nb
        end do
    end subroutine insert_line_text

    function buffer_get_char_at(buffer, pos) result(ch)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: pos
        character :: ch

        if (pos < buffer%gap_start) then
            ch = buffer%data(pos:pos)
        else
            ch = buffer%data(pos + (buffer%gap_end - buffer%gap_start):&
                           pos + (buffer%gap_end - buffer%gap_start))
        end if
    end function buffer_get_char_at

    subroutine cut_selection_or_line(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text

        if (cursor%has_selection) then
            ! Get selected text
            text = get_selection_text(cursor, buffer)

            ! Copy to clipboard
            if (allocated(text)) then
                call copy_to_clipboard(text)
            end if

            ! Delete the selection
            call delete_selection(cursor, buffer)
        else
            ! Get current line
            text = buffer_get_line(buffer, cursor%line)

            ! Copy to clipboard
            call copy_to_clipboard(text)

            ! Delete the line
            cursor%column = 1
            call delete_entire_line(buffer, cursor)
        end if

        buffer%modified = .true.
        if (allocated(text)) deallocate(text)
    end subroutine cut_selection_or_line

    subroutine copy_selection_or_line(cursor, buffer)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text

        if (cursor%has_selection) then
            ! Get selected text (only what's selected, no automatic newlines)
            text = get_selection_text(cursor, buffer)
        else
            ! Get current line - don't add newline, user can select it if they want it
            text = buffer_get_line(buffer, cursor%line)
        end if

        ! Copy to clipboard
        if (allocated(text)) then
            call copy_to_clipboard(text)
            deallocate(text)
        end if
    end subroutine copy_selection_or_line

    ! Insert a block of text at the cursor, treating LF, CR, and
    ! CRLF as line breaks. Multibyte UTF-8 sequences are inserted whole
    ! (one character column each). Caller is responsible for undo state.
    subroutine insert_text_block(cursor, buffer, text)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: text
        integer :: i, nb

        i = 1
        do while (i <= len(text))
            if (text(i:i) == char(13)) then
                call buffer_insert_newline(buffer, cursor)
                cursor%line = cursor%line + 1
                cursor%column = 1
                ! Swallow the LF of a CRLF pair
                if (i < len(text)) then
                    if (text(i+1:i+1) == char(10)) i = i + 1
                end if
                i = i + 1
            else if (text(i:i) == char(10)) then
                call buffer_insert_newline(buffer, cursor)
                cursor%line = cursor%line + 1
                cursor%column = 1
                i = i + 1
            else
                nb = min(utf8_lead_len(text(i:i)), len(text) - i + 1)
                call buffer_insert_string(buffer, cursor, text(i:i+nb-1))
                cursor%column = cursor%column + 1
                i = i + nb
            end if
        end do
        cursor%desired_column = cursor%column
        buffer%modified = .true.
    end subroutine insert_text_block

    subroutine paste_clipboard(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text

        ! Get text from clipboard
        text = paste_from_clipboard()

        if (allocated(text)) then
            ! Insert at cursor, UTF-8 and line-break aware
            call insert_text_block(cursor, buffer, text)
            deallocate(text)
        end if
    end subroutine paste_clipboard

    !> Save every modified tab.
    !>
    !> The active one goes through save_file, which knows how to prompt for a
    !> name when the tab is untitled and how to tell the language servers. The
    !> rest are written from their own panes: their edits live there, not in
    !> the working buffer, which only ever holds the tab being looked at.
    subroutine save_all_tabs(editor, buffer)
        use editor_state_module, only: save_tab_pane, tab_is_resident
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: i, status, saved, failed
        character(len=64) :: msg

        saved = 0
        failed = 0

        ! The working buffer is the active tab's only copy until this runs.
        call sync_editor_to_pane(editor)

        if (allocated(editor%tabs)) then
            do i = 1, size(editor%tabs)
                if (i == editor%active_tab_index) cycle
                if (.not. editor%tabs(i)%modified) cycle
                ! A deferred tab holds no text, so there is nothing of its own
                ! to write; save_tab_pane refuses it rather than truncating the
                ! file to the empty buffer standing in for it.
                if (.not. tab_is_resident(editor, i)) cycle
                call save_tab_pane(editor, i, &
                                   max(1, int(editor%tabs(i)%active_pane_index)), status)
                if (status == 0) then
                    editor%tabs(i)%modified = .false.
                    saved = saved + 1
                else
                    failed = failed + 1
                end if
            end do
        end if

        ! Last, because it may open a prompt for an untitled file and that
        ! should not interrupt a run of silent writes.
        call save_file(editor, buffer)

        if (failed > 0) then
            write(msg, '(a,i0,a,i0,a)') 'Saved ', saved + 1, ', failed ', failed, ''
        else
            write(msg, '(a,i0,a)') 'Saved ', saved + 1, ' file(s)'
        end if
        call set_status_message(trim(msg))
    end subroutine save_all_tabs

    subroutine save_file(editor, buffer)
        use text_prompt_module, only: show_text_prompt
        use lsp_server_manager_module, only: notify_file_saved
        use text_buffer_module, only: buffer_to_string
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: ios, tab_idx
        character(len=256) :: temp_filename, command
        character(len=512) :: new_filename
        logical :: file_exists, cancelled

        if (.not. allocated(editor%filename)) return

        ! Check if this is an untitled file - prompt for filename
        if (index(editor%filename, '[Untitled') == 1) then
            call show_text_prompt('Save as: ', new_filename, cancelled, editor%screen_rows)

            if (cancelled .or. len_trim(new_filename) == 0) then
                ! User cancelled or entered empty filename
                call terminal_move_cursor(editor%screen_rows, 1)
                call terminal_write('Save cancelled')
                return
            end if

            ! Update editor filename
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len_trim(new_filename)) :: editor%filename)
            editor%filename = trim(new_filename)

            ! Update tab filename if in workspace mode
            if (allocated(editor%tabs) .and. editor%active_tab_index > 0) then
                tab_idx = editor%active_tab_index
                if (tab_idx <= size(editor%tabs)) then
                    if (allocated(editor%tabs(tab_idx)%filename)) then
                        deallocate(editor%tabs(tab_idx)%filename)
                    end if
                    allocate(character(len=len_trim(new_filename)) :: editor%tabs(tab_idx)%filename)
                    editor%tabs(tab_idx)%filename = canonical_path(new_filename)

                    ! Update pane filename
                    if (allocated(editor%tabs(tab_idx)%panes)) then
                        if (size(editor%tabs(tab_idx)%panes) > 0) then
                            if (allocated(editor%tabs(tab_idx)%panes(1)%filename)) then
                                deallocate(editor%tabs(tab_idx)%panes(1)%filename)
                            end if
                            allocate(character(len=len_trim(new_filename)) :: &
                                    editor%tabs(tab_idx)%panes(1)%filename)
                            editor%tabs(tab_idx)%panes(1)%filename = canonical_path(new_filename)
                        end if
                    end if
                end if
            end if
        end if

        ! First try normal save
        call buffer_save_file(buffer, editor%filename, ios)

        if (ios == 0) then
            buffer%modified = .false.

            ! Send LSP didSave notification to ALL active servers
            if (allocated(editor%tabs) .and. editor%active_tab_index > 0) then
                tab_idx = editor%active_tab_index
                if (tab_idx <= size(editor%tabs)) then
                    if (editor%tabs(tab_idx)%num_lsp_servers > 0) then
                        block
                            integer :: srv_i
                            do srv_i = 1, editor%tabs(tab_idx)%num_lsp_servers
                                call notify_file_saved(editor%lsp_manager, &
                                    editor%tabs(tab_idx)%lsp_server_indices(srv_i), &
                                    trim(editor%filename), buffer_to_string(buffer))
                            end do
                        end block
                    end if
                end if
            end if

            return
        end if

        ! Check if file exists and we have write permission
        inquire(file=editor%filename, exist=file_exists)

        ! If save failed, try sudo save
        write(temp_filename, '(a,i0)') '/tmp/facsimile_sudo_', get_process_id()

        ! Save to temporary file
        call buffer_save_file(buffer, temp_filename, ios)
        if (ios /= 0) then
            ! Can't even save to /tmp, serious problem
            write(error_unit, *) 'Error: Cannot save file even to /tmp'
            return
        end if

        ! Use sudo to move the file
        write(command, '(a,a,a,a,a)') 'sudo mv ', trim(temp_filename), ' ', &
                                       trim(editor%filename), ' 2>/dev/null'

        ! Show message to user
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('[sudo] password required to save file')

        ! Execute sudo command
        call execute_command_line(command, exitstat=ios)

        if (ios == 0) then
            buffer%modified = .false.
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('File saved with sudo                  ')

            ! Send LSP didSave notification to ALL active servers
            if (allocated(editor%tabs) .and. editor%active_tab_index > 0) then
                tab_idx = editor%active_tab_index
                if (tab_idx <= size(editor%tabs)) then
                    if (editor%tabs(tab_idx)%num_lsp_servers > 0) then
                        block
                            integer :: srv_i
                            do srv_i = 1, editor%tabs(tab_idx)%num_lsp_servers
                                call notify_file_saved(editor%lsp_manager, &
                                    editor%tabs(tab_idx)%lsp_server_indices(srv_i), &
                                    trim(editor%filename), buffer_to_string(buffer))
                            end do
                        end block
                    end if
                end if
            end if
        else
            ! Clean up temp file
            write(command, '(a,a)') 'rm -f ', trim(temp_filename)
            call execute_command_line(command)
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('Save failed - permission denied        ')
        end if
    end subroutine save_file

    function get_process_id() result(pid)
        integer :: pid
        interface
            function c_getpid() bind(C, name="getpid")
                use iso_c_binding, only: c_int
                integer(c_int) :: c_getpid
            end function
        end interface
        pid = c_getpid()
    end function get_process_id

    subroutine cycle_quotes(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: quote_start, quote_end, byte_pos
        character :: current_quote, new_quote

        line = buffer_get_line(buffer, cursor%line)

        ! Find surrounding quotes (byte positions; cursor column is chars)
        byte_pos = utf8_char_to_byte_index(line, cursor%column)
        if (byte_pos == 0) byte_pos = len(line) + 1
        call find_surrounding_quotes(line, byte_pos, quote_start, quote_end, current_quote)

        if (quote_start > 0 .and. quote_end > 0) then
            ! Buffer edits below take char columns; quotes are single-byte
            ! ASCII so replacing them does not shift the byte/char mapping
            quote_start = utf8_byte_to_char_index(line, quote_start)
            quote_end = utf8_byte_to_char_index(line, quote_end)
            ! Determine next quote type
            select case(current_quote)
            case('"')
                new_quote = "'"
            case("'")
                new_quote = '`'
            case('`')
                new_quote = '"'
            case default
                return
            end select

            ! Replace quotes
            cursor%column = quote_start
            call buffer_delete_at_cursor(buffer, cursor)
            call buffer_insert_char(buffer, cursor, new_quote)

            cursor%column = quote_end
            call buffer_delete_at_cursor(buffer, cursor)
            call buffer_insert_char(buffer, cursor, new_quote)

            ! Restore cursor position
            cursor%column = quote_end
            buffer%modified = .true.
        end if

        if (allocated(line)) deallocate(line)
    end subroutine cycle_quotes

    ! pos, start_pos, end_pos are BYTE positions in line
    subroutine find_surrounding_quotes(line, pos, start_pos, end_pos, quote_char)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: start_pos, end_pos
        character, intent(out) :: quote_char
        integer :: i

        start_pos = 0
        end_pos = 0
        quote_char = ' '

        ! Search backward for opening quote
        do i = pos - 1, 1, -1
            if (line(i:i) == '"' .or. line(i:i) == "'" .or. line(i:i) == '`') then
                start_pos = i
                quote_char = line(i:i)
                exit
            end if
        end do

        if (start_pos > 0) then
            ! Search forward for closing quote
            do i = pos, len(line)
                if (line(i:i) == quote_char) then
                    end_pos = i
                    exit
                end if
            end do
        end if
    end subroutine find_surrounding_quotes

    subroutine remove_brackets(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: bracket_start, bracket_end, byte_pos
        character :: open_bracket, close_bracket

        line = buffer_get_line(buffer, cursor%line)

        ! Find surrounding brackets (byte positions; cursor column is chars)
        byte_pos = utf8_char_to_byte_index(line, cursor%column)
        if (byte_pos == 0) byte_pos = len(line) + 1
        call find_surrounding_brackets(line, byte_pos, bracket_start, bracket_end, &
                                       open_bracket, close_bracket)

        if (bracket_start > 0 .and. bracket_end > 0) then
            ! Buffer edits below take char columns
            bracket_start = utf8_byte_to_char_index(line, bracket_start)
            bracket_end = utf8_byte_to_char_index(line, bracket_end)
            ! Delete closing bracket first (to maintain positions)
            cursor%column = bracket_end
            call buffer_delete_at_cursor(buffer, cursor)

            ! Delete opening bracket
            cursor%column = bracket_start
            call buffer_delete_at_cursor(buffer, cursor)

            buffer%modified = .true.
        end if

        if (allocated(line)) deallocate(line)
    end subroutine remove_brackets

    ! pos, start_pos, end_pos are BYTE positions in line
    subroutine find_surrounding_brackets(line, pos, start_pos, end_pos, open_br, close_br)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: start_pos, end_pos
        character, intent(out) :: open_br, close_br
        integer :: i

        start_pos = 0
        end_pos = 0
        open_br = ' '
        close_br = ' '

        ! Search backward for opening bracket
        do i = pos - 1, 1, -1
            select case(line(i:i))
            case('(')
                start_pos = i
                open_br = '('
                close_br = ')'
                exit
            case('[')
                start_pos = i
                open_br = '['
                close_br = ']'
                exit
            case('{')
                start_pos = i
                open_br = '{'
                close_br = '}'
                exit
            end select
        end do

        if (start_pos > 0) then
            ! Search forward for matching closing bracket
            do i = pos, len(line)
                if (line(i:i) == close_br) then
                    end_pos = i
                    exit
                end if
            end do
        end if
    end subroutine find_surrounding_brackets

    subroutine init_cursor(cursor)
        type(cursor_t), intent(out) :: cursor
        cursor%line = 1
        cursor%column = 1
        cursor%desired_column = 1
        cursor%has_selection = .false.
        cursor%selection_start_line = 1
        cursor%selection_start_col = 1
    end subroutine init_cursor

    ! Parse "mouse-type:button:row:col" into parts. ok=.false. for
    ! events without the colon form (e.g. mouse-scroll-up).

    subroutine handle_mouse_event_action(key_str, editor, buffer)
        use editor_state_module, only: get_active_pane_indices
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer :: button, row, col
        integer :: colon1, colon2, colon3
        character(len=100) :: event_type
        integer :: ios, line_count
        type(cursor_t), allocatable :: new_cursors(:)
        integer :: i, cursor_exists
        ! Variables for mouse drag handling
        logical :: had_selection, in_active_pane
        integer :: selection_start_line, selection_start_col
        integer :: tab_idx, pane_idx

        line_count = buffer_get_line_count(buffer)

        ! Parse the mouse event string format: "mouse-type:button:row:col"
        colon1 = index(key_str, ':')
        if (colon1 == 0) return

        event_type = key_str(1:colon1-1)
        colon2 = index(key_str(colon1+1:), ':') + colon1
        if (colon2 == colon1) return

        colon3 = index(key_str(colon2+1:), ':') + colon2
        if (colon3 == colon2) return

        ! Parse button, row, and col
        read(key_str(colon1+1:colon2-1), '(i10)', iostat=ios) button
        if (ios /= 0) return

        read(key_str(colon2+1:colon3-1), '(i10)', iostat=ios) row
        if (ios /= 0) return

        read(key_str(colon3+1:), '(i10)', iostat=ios) col
        if (ios /= 0) return

        ! Handle different mouse event types
        select case(trim(event_type))
        case('mouse-click')
            ! Regular click - move cursor to position
            if (button == 0) then  ! Left click
                ! Clear other cursors first (single cursor mode)
                if (allocated(editor%cursors)) then
                    if (size(editor%cursors) > 1) then
                        deallocate(editor%cursors)
                        allocate(editor%cursors(1))
                        call init_cursor(editor%cursors(1))
                        editor%active_cursor = 1
                    end if
                end if

                ! Now position cursor (this might switch panes and update cursors)
                call position_cursor_at_screen(editor%active_cursor, &
                                              editor, buffer, row, col)

                ! Clear selection after positioning (cursor array is now stable)
                if (allocated(editor%cursors) .and. editor%active_cursor > 0 .and. &
                    editor%active_cursor <= size(editor%cursors)) then
                    editor%cursors(editor%active_cursor)%has_selection = .false.
                end if
            end if

        case('mouse-drag')
            ! Mouse drag - extend selection within current pane only
            ! We shouldn't switch panes while dragging, only move cursor within current pane

            ! Left button only. Mouse mode 1002 reports motion whichever button
            ! is held, so without this a right- or middle-drag arrives here as
            ! button 34 or 33 and silently starts a selection. The terminal
            ! panel already guards its own drag the same way.
            if (iand(button, 3) /= 0) return

            ! Check if we're in the current active pane - don't switch panes during drag
            call get_active_pane_indices(editor, tab_idx, pane_idx)
            in_active_pane = .false.

            if (tab_idx > 0 .and. pane_idx > 0 .and. allocated(editor%tabs(tab_idx)%panes)) then
                associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
                    ! Check if mouse is still in the active pane
                    if (row >= pane%screen_row .and. &
                        row < pane%screen_row + pane%screen_height .and. &
                        col >= pane%screen_col .and. &
                        col < pane%screen_col + pane%screen_width) then
                        in_active_pane = .true.
                    end if
                end associate
            end if

            ! Only process drag if within active pane
            if (in_active_pane) then
                had_selection = editor%cursors(editor%active_cursor)%has_selection
                if (.not. had_selection) then
                    ! Start selection from current position
                    selection_start_line = editor%cursors(editor%active_cursor)%line
                    selection_start_col = editor%cursors(editor%active_cursor)%column
                else
                    selection_start_line = editor%cursors(editor%active_cursor)%selection_start_line
                    selection_start_col = editor%cursors(editor%active_cursor)%selection_start_col
                end if

                ! Move cursor to drag position (won't switch panes since we're in active pane)
                call position_cursor_at_screen(editor%active_cursor, &
                                              editor, buffer, row, col)

                ! Restore/set selection state after positioning
                if (allocated(editor%cursors) .and. editor%active_cursor > 0 .and. &
                    editor%active_cursor <= size(editor%cursors)) then
                    editor%cursors(editor%active_cursor)%has_selection = .true.
                    editor%cursors(editor%active_cursor)%selection_start_line = selection_start_line
                    editor%cursors(editor%active_cursor)%selection_start_col = selection_start_col
                end if

                call update_viewport(editor)
            end if

        case('mouse-release')
            ! Mouse button released - nothing special to do
            continue

        case('mouse-scroll-up')
            call scroll_pane_at(editor, buffer, row, col, -3)

        case('mouse-scroll-down')
            call scroll_pane_at(editor, buffer, row, col, 3)

        case('mouse-alt')
            ! Alt+click - add or remove cursor
            if (button == 8) then  ! Alt + left click (button code includes alt modifier)
                ! Check if cursor already exists at this position
                cursor_exists = 0
                do i = 1, size(editor%cursors)
                    if (is_cursor_at_screen_pos(editor%cursors(i), editor, buffer, row, col)) then
                        cursor_exists = i
                        exit
                    end if
                end do

                if (cursor_exists > 0) then
                    ! Remove the cursor
                    if (size(editor%cursors) > 1) then
                        allocate(new_cursors(size(editor%cursors) - 1))
                        do i = 1, cursor_exists - 1
                            new_cursors(i) = editor%cursors(i)
                        end do
                        do i = cursor_exists + 1, size(editor%cursors)
                            new_cursors(i-1) = editor%cursors(i)
                        end do
                        deallocate(editor%cursors)
                        editor%cursors = new_cursors
                        if (editor%active_cursor >= cursor_exists) then
                            editor%active_cursor = max(1, editor%active_cursor - 1)
                        end if
                    end if
                else
                    ! Add a new cursor
                    allocate(new_cursors(size(editor%cursors) + 1))
                    do i = 1, size(editor%cursors)
                        new_cursors(i) = editor%cursors(i)
                    end do
                    call init_cursor(new_cursors(size(new_cursors)))
                    ! First move the new cursors to editor
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = size(editor%cursors)
                    ! Then position the new cursor using its index
                    call position_cursor_at_screen(editor%active_cursor, &
                                                  editor, buffer, row, col)
                end if
            end if

        end select
    end subroutine handle_mouse_event_action

    subroutine position_cursor_at_screen(cursor_idx, editor, buffer, screen_row, screen_col)
        use renderer_module, only: show_line_numbers, LINE_NUMBER_WIDTH
        use editor_state_module, only: get_active_pane_indices, switch_to_pane, sync_editor_to_pane
        integer, intent(inout) :: cursor_idx  ! Use index instead of reference
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: screen_row, screen_col
        integer :: target_line, target_col, col_offset, row_offset
        character(len=:), allocatable :: line
        integer :: line_count
        integer :: tab_idx, pane_idx, i
        integer :: pane_row, click_cells, vp_col
        logical :: in_pane

        line_count = buffer_get_line_count(buffer)
        in_pane = .false.

        row_offset = first_content_row(editor)

        ! Ignore clicks on the tab bar
        if (size(editor%tabs) > 0 .and. screen_row < row_offset) then
            return  ! Don't move cursor if clicking on tab bar
        end if

        ! Account for line number display offset
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Check if we're in a pane system
        call get_active_pane_indices(editor, tab_idx, pane_idx)
        if (tab_idx > 0 .and. pane_idx > 0 .and. allocated(editor%tabs(tab_idx)%panes)) then
            ! Find which pane was clicked
            do i = 1, size(editor%tabs(tab_idx)%panes)
                ! Check if click is within this pane's boundaries
                if (screen_row >= editor%tabs(tab_idx)%panes(i)%screen_row .and. &
                    screen_row < editor%tabs(tab_idx)%panes(i)%screen_row + &
                                  editor%tabs(tab_idx)%panes(i)%screen_height .and. &
                    screen_col >= editor%tabs(tab_idx)%panes(i)%screen_col .and. &
                    screen_col < editor%tabs(tab_idx)%panes(i)%screen_col + &
                                 editor%tabs(tab_idx)%panes(i)%screen_width) then

                    ! If clicking on a different pane, switch to it first
                    if (i /= pane_idx) then
                        call switch_to_pane(editor, tab_idx, i)
                        pane_idx = i
                        ! Update cursor index after switching (might have changed)
                        cursor_idx = editor%active_cursor
                    end if

                    ! Now use the active pane's data
                    associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
                        ! Invert the renderer's forward mapping. The stored
                        ! rect is the pane's CONTENT area (see
                        ! store_pane_content_rect), so no header adjustment
                        ! belongs here:
                        !   screen_row = pane%screen_row
                        !              + (line - viewport_line)
                        !   screen_col = pane%screen_col + col_offset
                        !              + display cells from viewport_column
                        pane_row = screen_row - pane%screen_row
                        target_line = pane%viewport_line + pane_row
                        click_cells = screen_col - pane%screen_col - col_offset
                        vp_col = pane%viewport_column
                        in_pane = .true.
                    end associate
                    exit
                end if
            end do

            if (.not. in_pane) then
                return  ! Click outside of any pane
            end if
        else
            ! No panes, use editor viewport. Text begins one cell
            ! after the gutter (screen column col_offset + 1).
            target_line = editor%viewport_line + screen_row - row_offset
            click_cells = screen_col - col_offset - 1
            vp_col = editor%viewport_column
        end if

        ! Clamp to valid range
        if (target_line < 1) target_line = 1
        if (target_line > line_count) target_line = line_count

        ! Map the clicked display cell back to a character column: on lines
        ! with tabs or wide characters cells and character indices diverge,
        ! so raw cell arithmetic would land the cursor on the wrong char
        line = buffer_get_line(buffer, target_line)
        if (click_cells < 0) click_cells = 0
        target_col = char_col_at_offset(line, vp_col, click_cells)
        if (target_col < 1) target_col = 1
        ! Clamp to character count + 1 (position after last char); len(line)
        ! is bytes, which overshoots on multibyte text
        if (target_col > utf8_char_count(line) + 1) then
            target_col = utf8_char_count(line) + 1
        end if

        ! Set cursor position (ensure cursor_idx is valid)
        if (allocated(editor%cursors) .and. cursor_idx > 0 .and. cursor_idx <= size(editor%cursors)) then
            editor%cursors(cursor_idx)%line = target_line
            editor%cursors(cursor_idx)%column = target_col
            editor%cursors(cursor_idx)%desired_column = target_col
        end if

        if (allocated(line)) deallocate(line)

        ! Sync the updated cursor position back to the active pane
        call sync_editor_to_pane(editor)
    end subroutine position_cursor_at_screen

    !> Scroll whichever pane the pointer is over by `delta` lines.
    !>
    !> A wheel tick used to move the active pane wherever the pointer was, so
    !> scrolling over the tab bar, the status bar, the file tree or an
    !> inactive pane moved a document the pointer was not on. Landing outside
    !> every pane now does nothing, which is the honest answer for the bars.
    subroutine scroll_pane_at(editor, buffer, screen_row, screen_col, delta)
        use editor_state_module, only: get_active_pane_indices
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: screen_row, screen_col, delta
        integer :: tab_idx, pane_idx, i, line_count, top, last_top

        call get_active_pane_indices(editor, tab_idx, pane_idx)
        if (tab_idx < 1) return
        if (tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        line_count = buffer_get_line_count(buffer)
        ! Same clamp the keyboard scroll uses. This was an open-coded
        ! screen_rows - 2 that missed the terminal panel, so wheel scrolling
        ! could run past the last drawn line while the panel was up.
        last_top = max(1, line_count - text_area_height(editor) + 1)

        do i = 1, size(editor%tabs(tab_idx)%panes)
            associate(pane => editor%tabs(tab_idx)%panes(i))
                if (screen_row >= pane%screen_row .and. &
                    screen_row < pane%screen_row + pane%screen_height .and. &
                    screen_col >= pane%screen_col .and. &
                    screen_col < pane%screen_col + pane%screen_width) then

                    top = max(1, pane%viewport_line + delta)
                    if (top > last_top) top = last_top
                    pane%viewport_line = top

                    ! The renderer reads the editor-level viewport for the
                    ! active pane, so keep the two in step; scrolling an
                    ! inactive pane must leave it alone.
                    if (i == pane_idx) editor%viewport_line = top
                    return
                end if
            end associate
        end do
    end subroutine scroll_pane_at

    !> The command palette, as its own routine so the context menu can open it
    !> without going through handle_key_command('ctrl-p'). That would nest
    !> dispatcher → palette → dispatcher three deep; this keeps it at two, the
    !> depth already reached in production.
    subroutine open_command_palette(editor, buffer, should_quit)
        use command_palette_module, only: show_command_palette_interactive
        type(editor_state_t), intent(inout), target :: editor
        type(buffer_t), intent(inout), target :: buffer
        logical, intent(inout) :: should_quit
        character(len=:), allocatable :: cmd_id

        cmd_id = show_command_palette_interactive(editor%command_palette, &
                                                  editor%screen_cols)
        if (allocated(cmd_id)) then
            if (len_trim(cmd_id) > 0) then
                call execute_palette_command(editor, buffer, cmd_id, should_quit)
            end if
        end if

        ! Redraw screen after palette
        call render_screen(buffer, editor)
    end subroutine open_command_palette

    !> Would a synthetic key sent from a menu row actually reach the main
    !> dispatcher? Every one of these routes input away before the select
    !> case, so a menu offered here would have inert rows. Refusing to open
    !> is honest; opening a menu whose rows do nothing is not.
    !>
    !> Fuss mode is included deliberately. With the tree open the keyboard
    !> belongs to the tree: ctrl-x, ctrl-c, ctrl-v, ctrl-/ and ctrl-p are all
    !> outside its exemption list and would reach handle_fuss_input instead.
    !> A left click in the document is already inert there, so this is
    !> consistent rather than a new limitation.
    function document_menu_available(editor) result(ok)
        type(editor_state_t), intent(in) :: editor
        logical :: ok

        ok = .false.
        if (editor%fuss_mode_active) return
        if (is_terminal_panel_visible(editor%terminal_panel)) then
            if (editor%terminal_panel%focused) return
        end if
        if (is_completion_visible(editor%completion_popup)) return
        if (is_diagnostics_panel_visible(editor%diagnostics_panel)) return
        if (is_code_actions_panel_visible(editor%code_actions_panel)) return
        if (is_references_panel_visible(editor%references_panel)) return
        if (is_symbols_panel_visible(editor%symbols_panel)) return
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) return
        ok = .true.
    end function document_menu_available

    !> The rectangle a menu may occupy: below the tab bar, above the status
    !> bar, and clear of the terminal panel when it is up.
    subroutine menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        type(editor_state_t), intent(in) :: editor
        integer, intent(out) :: top_row, bottom_row, left_col, right_col

        top_row = first_content_row(editor)
        bottom_row = editor%screen_rows - 1
        if (is_terminal_panel_visible(editor%terminal_panel)) then
            bottom_row = bottom_row - get_terminal_panel_height(editor%terminal_panel)
        end if
        left_col = 1
        right_col = editor%screen_cols
    end subroutine menu_bounds

    !> True when the cell lies inside some pane of the active tab. Without it
    !> a right-click on blank space would open a menu whose caret rule had
    !> nothing to act on.
    function cell_in_a_pane(editor, screen_row, screen_col) result(inside)
        use editor_state_module, only: get_active_pane_indices
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: screen_row, screen_col
        logical :: inside
        integer :: tab_idx, pane_idx, i

        inside = .false.
        call get_active_pane_indices(editor, tab_idx, pane_idx)
        if (tab_idx < 1) return
        if (tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        do i = 1, size(editor%tabs(tab_idx)%panes)
            associate(pane => editor%tabs(tab_idx)%panes(i))
                if (screen_row >= pane%screen_row .and. &
                    screen_row < pane%screen_row + pane%screen_height .and. &
                    screen_col >= pane%screen_col .and. &
                    screen_col < pane%screen_col + pane%screen_width) then
                    inside = .true.
                end if
            end associate
            if (inside) return
        end do
    end function cell_in_a_pane

    !> Is (pl, pc) inside the normalised range? Half-open at the end, matching
    !> what render_line_with_selections actually highlights: `ec` is the
    !> character after the last selected one, so clicking the cell just past a
    !> selection must read as outside. Nested ifs because .and. does not
    !> short-circuit.
    pure function point_in_range(pl, pc, sl, sc, el, ec) result(inside)
        integer, intent(in) :: pl, pc, sl, sc, el, ec
        logical :: inside

        inside = .false.
        if (pl < sl) return
        if (pl > el) return
        if (pl == sl) then
            if (pc < sc) return
        end if
        if (pl == el) then
            if (pc >= ec) return
        end if
        inside = .true.
    end function point_in_range

    !> Right-click in the document.
    !>
    !> The caret moves to the click, except when the click lands inside an
    !> existing selection, which is preserved so Cut and Copy act on it.
    !> Implemented by doing exactly what a left click does and undoing it if
    !> the point turns out to be inside: reusing position_cursor_at_screen
    !> rather than re-deriving the screen-to-document mapping means the two
    !> can never disagree about where a click lands.
    subroutine open_document_context_menu(editor, buffer, mrow, mcol)
        use editor_state_module, only: get_active_pane_indices
        type(editor_state_t), intent(inout), target :: editor
        type(buffer_t), intent(inout), target :: buffer
        integer, intent(in) :: mrow, mcol
        integer :: top_row, bottom_row, left_col, right_col
        integer :: t0, p0, t1, p1, sl, sc, el, ec, i
        integer :: hit_line, hit_col
        logical :: shown, inside
        type(cursor_t), allocatable :: saved(:)
        integer :: saved_active

        if (.not. document_menu_available(editor)) return
        if (.not. cell_in_a_pane(editor, mrow, mcol)) return

        saved = editor%cursors
        saved_active = editor%active_cursor
        call get_active_pane_indices(editor, t0, p0)

        ! What a left click does, verbatim
        if (size(editor%cursors) > 1) then
            deallocate(editor%cursors)
            allocate(editor%cursors(1))
            call init_cursor(editor%cursors(1))
            editor%active_cursor = 1
        end if
        call position_cursor_at_screen(editor%active_cursor, editor, buffer, mrow, mcol)

        hit_line = editor%cursors(editor%active_cursor)%line
        hit_col = editor%cursors(editor%active_cursor)%column

        ! A click in another pane always counts as outside: restoring the old
        ! pane's cursors into the newly focused one would be nonsense.
        inside = .false.
        call get_active_pane_indices(editor, t1, p1)
        if (t1 == t0) then
            if (p1 == p0) then
                do i = 1, size(saved)
                    if (saved(i)%has_selection) then
                        call normalize_selection(saved(i), sl, sc, el, ec)
                        if (point_in_range(hit_line, hit_col, sl, sc, el, ec)) then
                            inside = .true.
                            exit
                        end if
                    end if
                end do
            end if
        end if

        if (inside) then
            ! Every cursor and every selection survives, so Cut on a
            ! multi-cursor selection still cuts all of it.
            deallocate(editor%cursors)
            editor%cursors = saved
            editor%active_cursor = saved_active
            call sync_editor_to_pane(editor)
        else
            editor%cursors(editor%active_cursor)%has_selection = .false.
        end if

        call build_document_menu(editor)

        call menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        shown = context_menu_show(mrow, mcol, top_row, bottom_row, left_col, right_col)
        if (shown) g_lsp_ui_changed = .true.
    end subroutine open_document_context_menu

    !> Open the document menu at the caret, for the keyboard shortcut. No
    !> caret rule applies: the caret is already where the user put it, and
    !> whatever is selected stays selected.
    subroutine open_document_menu_at_caret(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer :: top_row, bottom_row, left_col, right_col
        integer :: crow, ccol
        logical :: shown

        if (.not. document_menu_available(editor)) return

        call caret_screen_pos(editor%cursors(editor%active_cursor), editor, &
                              buffer, crow, ccol)
        call build_document_menu(editor)

        call menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        ! One row down so the box does not sit on top of the caret it
        ! belongs to; clamping pulls it back up at the bottom of the screen.
        shown = context_menu_show(crow + 1, ccol, top_row, bottom_row, &
                                  left_col, right_col)
        if (shown) g_lsp_ui_changed = .true.
    end subroutine open_document_menu_at_caret

    subroutine build_document_menu(editor)
        type(editor_state_t), intent(in) :: editor

        call context_menu_begin(CTX_KIND_DOC)
        ! Cut and Copy fall back to the whole line when nothing is selected,
        ! so they are never disabled -- the label says which it will be.
        if (editor%cursors(editor%active_cursor)%has_selection) then
            call context_menu_add_item('Cut', 'Ctrl+X', ACT_CUT)
            call context_menu_add_item('Copy', 'Ctrl+C', ACT_COPY)
        else
            call context_menu_add_item('Cut Line', 'Ctrl+X', ACT_CUT)
            call context_menu_add_item('Copy Line', 'Ctrl+C', ACT_COPY)
        end if
        ! Paste is never probed: reading the system clipboard shells out to
        ! xsel/xclip/pbpaste and would stall the menu, and the honest answer
        ! without probing is "there may well be something there".
        call context_menu_add_item('Paste', 'Ctrl+V', ACT_PASTE)
        call context_menu_add_separator()
        call context_menu_add_item('Toggle Comment', 'Ctrl+/', ACT_COMMENT, &
                                   enabled=comment_available_here(editor))
        call context_menu_add_item('Select All', 'Alt+A', ACT_SELECT_ALL)
        call context_menu_add_separator()
        call context_menu_add_item('Go to Definition', 'F12', ACT_GOTO_DEF, &
                                   enabled=(get_lsp_server_for_cap(editor, CAP_DEFINITION) > 0))
        call context_menu_add_item('Find References', 'Shift+F12', ACT_FIND_REFS, &
                                   enabled=(get_lsp_server_for_cap(editor, CAP_REFERENCES) > 0))
        call context_menu_add_separator()
        call context_menu_add_item('Command Palette', 'Ctrl+P', ACT_PALETTE)
    end subroutine build_document_menu

    function comment_available_here(editor) result(ok)
        type(editor_state_t), intent(in) :: editor
        logical :: ok

        if (allocated(editor%filename)) then
            ok = comment_syntax_available(editor%filename)
        else
            ok = .false.
        end if
    end function comment_available_here

    !> Run one of the tree's git actions, which are reachable from the
    !> keyboard only behind a ctrl-g prefix.
    !>
    !> Setting the flag and sending the letter rather than calling the git
    !> internals keeps the workspace_path guard and the flag hygiene in a
    !> single place, and means the menu and the keyboard cannot diverge. The
    !> flag is set and consumed within one call, so the renderer never sees it
    !> true and the tree's prefix hint does not flash.
    subroutine invoke_fuss_git(letter, editor, buffer)
        character(len=*), intent(in) :: letter
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer

        fuss_git_prefix_active = .true.
        call handle_fuss_input(letter, editor, buffer)
        fuss_git_prefix_active = .false.
    end subroutine invoke_fuss_git

    !> Repaint before handing control to anything that owns the terminal.
    subroutine repaint_now(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer

        if (editor%fuss_mode_active) then
            call render_screen_with_tree(buffer, editor)
        else
            call render_screen(buffer, editor)
        end if
    end subroutine repaint_now

    !> Run a menu row.
    !>
    !> The menu is closed and the screen repainted before anything is
    !> dispatched. Three reasons: the keyboard gate above would swallow the
    !> synthetic key while the menu is still visible; Find References and the
    !> command palette take over the terminal with their own input loops and
    !> must own a clean screen; and a palette "Quit" would otherwise be
    !> absorbed by the Ctrl-Q cascade closing the menu instead of quitting.
    subroutine activate_context_menu_row(idx, editor, buffer, should_quit)
        integer, intent(in) :: idx
        type(editor_state_t), intent(inout), target :: editor
        type(buffer_t), intent(inout), target :: buffer
        logical, intent(inout) :: should_quit
        integer :: act, kind
        logical :: enabled, inner_quit

        ! Show which row was hit before acting on it. Clicking a row the
        ! highlight was not already on would otherwise tear the menu down
        ! with no acknowledgement that the right thing was chosen.
        if (context_menu_select(idx)) then
            call repaint_now(editor, buffer)
            call platform_sleep_ms(MENU_FLASH_MS)
        end if

        call context_menu_take(idx, act, kind, enabled)
        if (.not. enabled) return        ! a disabled row leaves the menu up

        call repaint_now(editor, buffer)

        inner_quit = .false.
        if (kind == CTX_KIND_DOC) then
            select case (act)
            case (ACT_CUT)
                call handle_key_command('ctrl-x', editor, buffer, inner_quit)
            case (ACT_COPY)
                call handle_key_command('ctrl-c', editor, buffer, inner_quit)
            case (ACT_PASTE)
                call handle_key_command('ctrl-v', editor, buffer, inner_quit)
            case (ACT_COMMENT)
                call handle_key_command('ctrl-/', editor, buffer, inner_quit)
            case (ACT_SELECT_ALL)
                call handle_key_command('alt-a', editor, buffer, inner_quit)
            case (ACT_GOTO_DEF)
                call handle_key_command('f12', editor, buffer, inner_quit)
            case (ACT_FIND_REFS)
                call handle_key_command('shift-f12', editor, buffer, inner_quit)
            case (ACT_PALETTE)
                call open_command_palette(editor, buffer, inner_quit)
            end select
        else if (kind == CTX_KIND_TREE) then
            select case (act)
            case (ACT_TREE_ACTIVATE)
                if (tree_state%selected_index >= 1 .and. &
                    tree_state%selected_index <= tree_state%n_selectable) then
                    if (tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                        call handle_fuss_input('space', editor, buffer)
                    else
                        call handle_fuss_input('enter', editor, buffer)
                    end if
                end if
            case (ACT_TREE_VSPLIT)
                call handle_fuss_input('alt-v', editor, buffer)
            case (ACT_TREE_HSPLIT)
                call handle_fuss_input('alt-s', editor, buffer)
            case (ACT_TREE_STAGE)
                call invoke_fuss_git('a', editor, buffer)
            case (ACT_TREE_UNSTAGE)
                call invoke_fuss_git('u', editor, buffer)
            case (ACT_TREE_DIFF)
                call invoke_fuss_git('d', editor, buffer)
            end select
        else if (kind == CTX_KIND_GROUP) then
            ! g_menu_gid was recorded when the menu was opened. Reading the
            ! group from the tab bar again here would be wrong: the menu can
            ! outlive a scroll of the bar, and the entry under those
            ! coordinates may be a different group by now.
            select case (act)
            case (ACT_GROUP_EDIT)
                call start_group_edit(editor, g_menu_gid)
            case (ACT_GROUP_RENAME)
                call rename_group_prompt(editor, g_menu_gid)
            case (ACT_GROUP_DISSOLVE)
                call dissolve_group_now(editor, g_menu_gid)
            end select
        else if (kind == CTX_KIND_TAB) then
            call tab_menu_action(editor, buffer, act)
        end if

        if (inner_quit) should_quit = .true.
    end subroutine activate_context_menu_row

    !> Right-click on a file-tree row. Rows are wired up in a later stage.
    subroutine open_tree_context_menu(editor, item_idx, mrow, mcol)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: item_idx, mrow, mcol
        integer :: top_row, bottom_row, left_col, right_col
        logical :: shown, is_dir, git_ok

        if (item_idx < 1) return
        if (item_idx > tree_state%n_selectable) return
        tree_state%selected_index = item_idx
        is_dir = tree_state%selectable_files(item_idx)%is_directory

        call context_menu_begin(CTX_KIND_TREE)
        if (is_dir) then
            ! One adaptive row rather than two, one of them always greyed:
            ! activating a directory is expanding or collapsing it, and which
            ! is already known here.
            call context_menu_add_item('Expand or Collapse', 'Space', ACT_TREE_ACTIVATE)
        else
            call context_menu_add_item('Open', 'Enter', ACT_TREE_ACTIVATE)
            call context_menu_add_item('Open to the Side', 'Alt+V', ACT_TREE_VSPLIT)
            call context_menu_add_item('Open Below', 'Alt+S', ACT_TREE_HSPLIT)

            ! Git actions, which otherwise live behind an undiscoverable
            ! ctrl-g prefix. Greyed from the per-file status the tree already
            ! tracks, so the menu says what is possible for this file.
            git_ok = tree_state%is_git_repo
            if (git_ok) then
                if (.not. allocated(editor%workspace_path)) git_ok = .false.
            end if
            call context_menu_add_separator()
            call context_menu_add_item('Stage', 'Ctrl+G A', ACT_TREE_STAGE, &
                enabled=(git_ok .and. &
                    (tree_state%selectable_files(item_idx)%is_unstaged .or. &
                     tree_state%selectable_files(item_idx)%is_untracked)))
            call context_menu_add_item('Unstage', 'Ctrl+G U', ACT_TREE_UNSTAGE, &
                enabled=(git_ok .and. &
                    tree_state%selectable_files(item_idx)%is_staged))
            call context_menu_add_item('Diff', 'Ctrl+G D', ACT_TREE_DIFF, &
                enabled=git_ok)
        end if

        call menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        shown = context_menu_show(mrow, mcol, top_row, bottom_row, left_col, right_col)
        if (shown) g_lsp_ui_changed = .true.
    end subroutine open_tree_context_menu

    !> Right-click on a group entry in the tab bar.
    !>
    !> A menu rather than opening the dialog outright: the tree is the only
    !> other right-clickable surface and it gives a menu, and Rename and
    !> Dissolve have to live somewhere reachable too.
    subroutine open_group_context_menu(editor, gid, mrow, mcol)
        use editor_state_module, only: group_find
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        integer, intent(in) :: mrow, mcol
        integer :: top_row, bottom_row, left_col, right_col
        logical :: shown

        if (group_find(editor, gid) == 0) return
        g_menu_gid = gid

        call context_menu_begin(CTX_KIND_GROUP)
        call context_menu_add_item('Edit Group...', '', ACT_GROUP_EDIT)
        call context_menu_add_item('Rename Group...', '', ACT_GROUP_RENAME)
        call context_menu_add_separator()
        ! Dissolving leaves every member open as an ordinary tab. It is the
        ! non-destructive way out of a group, which is why unticking in the
        ! dialog can afford to be the destructive one.
        call context_menu_add_item('Dissolve Group', '', ACT_GROUP_DISSOLVE)

        call menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        ! A menu opened FROM the tab bar hangs directly under the row that was
        ! clicked, rather than being pushed below the whole bar. Inside a group
        ! the bar is two rows, so the general rule left two rows of dead space
        ! between the pointer and the thing it is travelling to.
        if (mrow < top_row) top_row = min(mrow + 1, bottom_row)
        shown = context_menu_show(mrow, mcol, top_row, bottom_row, left_col, right_col)
        if (shown) g_lsp_ui_changed = .true.
    end subroutine open_group_context_menu

    ! ---- dragging a tab -------------------------------------------------

    !> What a plain click on a tab-bar entry does: switch to it, or enter the
    !> group. Deferred from the press so that a drag never opens what it
    !> carries.
    subroutine activate_tab_entry(editor, buffer, payload)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: payload

        if (payload < 0) then
            call enter_tab_group(editor, buffer, int(-payload, int32))
        else if (payload >= 1 .and. payload <= size(editor%tabs)) then
            call switch_to_tab_with_buffer(editor, payload, buffer)
        end if
    end subroutine activate_tab_entry

    !> Would dropping here split the view, and along which edge?
    !>
    !> Measured against the ACTIVE pane's rectangle, in quarters: the outer
    !> quarter of the left, right or bottom edge means a split along it, and
    !> the middle means nothing. Quarters rather than a thin border because
    !> the pointer is carrying something and does not deserve to have to be
    !> precise.
    subroutine aim_at_split(editor, mrow, mcol)
        use tab_drag_module, only: drag_set_split, drag_clear_target, &
                                   drag_kind, DRAG_TAB, &
                                   SPLIT_LEFT, SPLIT_RIGHT, SPLIT_BELOW
        use editor_state_module, only: get_active_pane_indices
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: mrow, mcol
        integer :: tab_idx, pane_idx, r0, c0, r1, c1, w, h

        call drag_clear_target()
        ! Only a file can become a split. A group is several files and has no
        ! single content to put in a pane.
        if (drag_kind() /= DRAG_TAB) return
        ! The last tab cannot leave the bar: something has to remain open.
        if (size(editor%tabs) < 2) return

        call get_active_pane_indices(editor, tab_idx, pane_idx)
        if (tab_idx < 1 .or. pane_idx < 1) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx > size(editor%tabs(tab_idx)%panes)) return

        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            r0 = pane%screen_row
            c0 = pane%screen_col
            h = pane%screen_height
            w = pane%screen_width
        end associate
        r1 = r0 + h - 1
        c1 = c0 + w - 1
        if (w < 8 .or. h < 4) return
        if (mrow < r0 .or. mrow > r1) return
        if (mcol < c0 .or. mcol > c1) return

        if (mrow >= r1 - h / 4 + 1) then
            call drag_set_split(SPLIT_BELOW, r1 - h / 4 + 1, c0, r1, c1)
        else if (mcol <= c0 + w / 4 - 1) then
            call drag_set_split(SPLIT_LEFT, r0, c0, r1, c0 + w / 4 - 1)
        else if (mcol >= c1 - w / 4 + 1) then
            call drag_set_split(SPLIT_RIGHT, r0, c1 - w / 4 + 1, r1, c1)
        end if
    end subroutine aim_at_split

    !> Record a press on a tab-bar entry so a following move can drag it.
    !>
    !> Arming, not dragging. A click that never moves must stay a click, so
    !> nothing is drawn and nothing is targeted until the pointer leaves the
    !> cell it was pressed on.
    subroutine arm_tab_drag(editor, payload, mrow, mcol)
        use tab_drag_module, only: drag_arm_only, DRAG_TAB, DRAG_GROUP
        use renderer_module, only: tabbar_slot_at
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: payload, mrow, mcol
        character(len=:), allocatable :: path, label
        integer :: strip, slot

        slot = tabbar_slot_at(mrow, mcol, strip)
        if (slot == 0) return

        if (payload < 0) then
            label = ' ' // group_label_public(editor, int(-payload, int32)) // ' '
            call drag_arm_only(DRAG_GROUP, payload, '', label, &
                               int(-payload, int32), strip, mrow, mcol, &
                               size(editor%tabs))
        else if (payload >= 1 .and. payload <= size(editor%tabs)) then
            if (.not. allocated(editor%tabs(payload)%filename)) return
            path = editor%tabs(payload)%filename
            label = ' ' // basename_public(path) // ' '
            call drag_arm_only(DRAG_TAB, payload, path, label, 0_int32, &
                               strip, mrow, mcol, size(editor%tabs))
        end if
    end subroutine arm_tab_drag

    function basename_public(path) result(b)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: b
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash > 0) then
            b = path(slash + 1:len_trim(path))
        else
            b = trim(path)
        end if
    end function basename_public

    !> The pointer moved with the button down. Work out where a drop would
    !> land; draw nothing here, the renderer reads the target.
    subroutine tab_drag_motion(editor, mrow, mcol)
        use tab_drag_module, only: drag_is_armed, drag_is_showing, drag_begin, &
                                   drag_set_pointer, drag_press_row, &
                                   drag_press_col, drag_set_target, &
                                   drag_clear_target, drag_payload, &
                                   drag_tab_count, drag_cancel
        use renderer_module, only: tabbar_slot_at, nudge_tab_scroll, &
                                   tabbar_strip2_gid, tabbar_strip_rows, &
                                   tabbar_last_slot
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: mrow, mcol
        type(clickable_region_t) :: hit
        integer :: slot, strip, row1, row2

        if (.not. drag_is_armed()) return
        ! A tab opened or closed underneath us. The drag was aimed at an
        ! arrangement that no longer exists, so abandon it rather than move
        ! something the user is no longer pointing at.
        if (drag_tab_count() /= size(editor%tabs)) then
            call drag_cancel()
            return
        end if

        if (.not. drag_is_showing()) then
            if (mrow == drag_press_row() .and. mcol == drag_press_col()) return
            call drag_begin()
        end if
        call drag_set_pointer(mrow, mcol)

        ! Held over a chevron: scroll that way so a tab can be carried past
        ! the edge of a full bar. On a timer, not per event -- a fast pointer
        ! emits far more motion reports than a slow one and the bar would fly.
        hit = region_at(mrow, mcol)
        if (hit%kind == REGION_TAB_SCROLL) then
            if (drag_scroll_due()) call nudge_tab_scroll(hit%payload)
            call drag_clear_target()
            return
        end if

        slot = tabbar_slot_at(mrow, mcol, strip)
        if (slot == 0) then
            ! The blank tail of a strip is still that strip. Dropping there
            ! means "at the end", which is the only way to carry a tab OUT of
            ! a group when the group is the only entry on row 1 -- there is
            ! nothing else to aim at.
            call tabbar_strip_rows(row1, row2)
            if (mrow == row1 .and. row1 /= 0) then
                call drag_set_target(1, tabbar_last_slot(1), 0_int32)
                call dwell_over_group(0)
            else if (mrow == row2 .and. row2 /= 0) then
                call drag_set_target(2, tabbar_last_slot(2), tabbar_strip2_gid())
            else
                ! Off the bar entirely: the document. Near an edge this is a
                ! split, which is the only meaning "outside the bar" has.
                call aim_at_split(editor, mrow, mcol)
                g_dwell_gid = 0
            end if
            return
        end if

        if (strip == 2) then
            ! A member row, either a group's pinned one or a strip that a
            ! dwell opened. Either way the drop joins THAT group.
            call drag_set_target(2, slot, tabbar_strip2_gid())
            return
        end if

        ! Row 1. If the pointer is resting on a DIFFERENT group's entry, open
        ! it after a moment so its members can be dropped into.
        call dwell_over_group(slot)
        call drag_set_target(1, slot, 0_int32)
    end subroutine tab_drag_motion

    !> Open a group's member strip when a dragged tab rests on its entry.
    !>
    !> Reuses the hover preview: it already draws a group's members on row 2
    !> and already treats its own strip as still-hovered, which is exactly the
    !> "move down into it to drop" behaviour needed here. Nothing new is drawn.
    subroutine dwell_over_group(slot)
        use tab_drag_module, only: drag_kind, drag_gid, DRAG_TAB
        use renderer_module, only: tab_group_set_hover, tabbar_pre_payload
        integer, intent(in) :: slot
        integer(int32) :: over
        integer(int64) :: now, rate
        integer :: payload
        logical :: ignored

        ! Deliberately NOT region_at: the region table describes the previewed
        ! bar, where the held entry has been moved under the pointer, so it
        ! would always answer "you are hovering the thing you are holding".
        over = 0
        payload = tabbar_pre_payload(slot)
        if (payload < 0) over = int(-payload, int32)

        ! Only a tab can join a group, and never the group it is already the
        ! entry for.
        if (drag_kind() /= DRAG_TAB) over = 0
        if (over /= 0 .and. over == drag_gid()) over = 0

        if (over == 0) then
            g_dwell_gid = 0
            return
        end if

        call system_clock(count=now, count_rate=rate)
        if (over /= g_dwell_gid) then
            g_dwell_gid = over
            g_dwell_since = now
            return
        end if
        if (rate <= 0) return
        if ((now - g_dwell_since) * 1000 / rate < DRAG_DWELL_MS) return

        ignored = tab_group_set_hover(over)
    end subroutine dwell_over_group

    !> True at most every DRAG_SCROLL_MS, so holding over a chevron scrolls at
    !> a readable rate rather than once per motion report.
    logical function drag_scroll_due()
        integer(int64) :: now, rate

        call system_clock(count=now, count_rate=rate)
        if (rate <= 0) then
            drag_scroll_due = .true.
            return
        end if
        drag_scroll_due = (now - g_drag_scroll_at) * 1000 / rate >= DRAG_SCROLL_MS
        if (drag_scroll_due) g_drag_scroll_at = now
    end function drag_scroll_due

    !> Button released: apply the target, or let go of it.
    !> Turn the held tab into a split of whatever was showing before it was
    !> grabbed, and take it off the bar.
    !>
    !> The host is the PREVIOUS tab, not the active one: pressing a tab makes
    !> it active, so by now the document on screen is the dragged file itself
    !> and splitting that against itself would put the same file in both
    !> panes. If the previous tab is gone, or is the dragged one, any other
    !> tab will do -- what matters is that the split has something to be
    !> beside.
    subroutine drop_as_split(editor, buffer, side)
        use tab_drag_module, only: drag_path, &
                                   SPLIT_LEFT, SPLIT_RIGHT, SPLIT_BELOW
        use editor_state_module, only: find_tab_by_path_public
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: side
        character(len=:), allocatable :: carried, host
        integer :: host_idx, i, gone

        carried = trim(drag_path())
        if (len_trim(carried) == 0) return
        if (size(editor%tabs) < 2) return

        ! The split RELOADS the file from disk into its new pane, so unsaved
        ! edits in the tab being consumed would be silently thrown away.
        ! Refusing is the honest answer; prompting in the middle of a drag,
        ! with a button still held, is not.
        gone = find_tab_by_path_public(editor, carried)
        if (gone /= 0) then
            if (editor%tabs(gone)%modified) then
                call set_status_message('Save ' // basename_public(carried) // &
                                        ' before splitting it off')
                return
            end if
        end if

        ! Whatever is showing. A press no longer switches tabs, so the
        ! document under the pointer is still the one that was there when the
        ! drag started -- there is nothing to remember.
        host = ''
        if (editor%active_tab_index >= 1 .and. &
            editor%active_tab_index <= size(editor%tabs)) then
            if (allocated(editor%tabs(editor%active_tab_index)%filename)) &
                host = trim(editor%tabs(editor%active_tab_index)%filename)
        end if
        if (len_trim(host) > 0) then
            if (trim(host) == carried) host = ''
        end if
        if (len_trim(host) == 0) then
            do i = 1, size(editor%tabs)
                if (.not. allocated(editor%tabs(i)%filename)) cycle
                if (trim(editor%tabs(i)%filename) == carried) cycle
                host = trim(editor%tabs(i)%filename)
                exit
            end do
        end if
        if (len_trim(host) == 0) return

        ! Close the carried tab BEFORE splitting, not after.
        !
        ! The other order looks more natural and corrupts the host: switching
        ! away from it to reach the tab being closed SAVES the working buffer
        ! into the host's pane, and by then the working buffer holds the
        ! carried file's text. The host ends up modified, holding the wrong
        ! document.
        if (gone /= 0) then
            call switch_to_tab_with_buffer(editor, gone, buffer)
            call close_tab_without_prompt(editor, buffer)
        end if

        host_idx = find_tab_by_path_public(editor, host)
        if (host_idx == 0) return
        call switch_to_tab_with_buffer(editor, host_idx, buffer)

        if (side == SPLIT_BELOW) then
            call open_file_in_horizontal_split(carried, editor, buffer)
        else
            call open_file_in_vertical_split(carried, editor, buffer)
        end if

        call set_status_message('Split off ' // basename_public(carried))
    end subroutine drop_as_split

    subroutine tab_drag_release(editor, buffer)
        use tab_drag_module, only: drag_is_showing, drag_has_target, drag_kind, &
                                   drag_to_row, drag_to_slot, drag_path, &
                                   drag_gid, drag_cancel, drag_to_gid, &
                                   drag_payload, &
                                   drag_split_side, SPLIT_NONE, &
                                   DRAG_TAB, DRAG_GROUP
        use editor_state_module, only: find_tab_by_path_public, reorder_tab, &
                                       reorder_group_block, set_group_ordinal, &
                                       group_remove_member, prune_empty_groups
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: from_idx, dest
        integer(int32) :: was_gid, to_gid

        if (.not. drag_is_showing()) then
            ! Pressed and released without moving: an ordinary click, acted on
            ! here rather than on the press so that carrying a tab somewhere
            ! never opens it on the way.
            call activate_tab_entry(editor, buffer, drag_payload())
            call end_drag()
            return
        end if
        if (drag_split_side() /= SPLIT_NONE) then
            call drop_as_split(editor, buffer, drag_split_side())
            call end_drag()
            return
        end if
        if (.not. drag_has_target()) then
            ! Released somewhere that means nothing. Nothing was ever applied,
            ! so letting go of the target IS the snap back.
            call end_drag()
            return
        end if

        dest = drag_to_slot()

        if (drag_kind() == DRAG_GROUP) then
            if (drag_to_row() == 1) call reorder_group_block(editor, drag_gid(), dest)
        else
            from_idx = find_tab_by_path_public(editor, trim(drag_path()))
            if (from_idx /= 0) then
                was_gid = editor%tabs(from_idx)%group_id
                if (drag_to_row() == 2) then
                    to_gid = drag_to_gid()
                    if (to_gid /= 0) then
                        if (to_gid == was_gid) then
                            ! Within one group: ordinals only. Moving a tab in
                            ! the array here would drag the group's neighbours
                            ! around row 1 for a change confined to row 2.
                            call set_group_ordinal(editor, from_idx, dest)
                        else
                            call join_group_at(editor, from_idx, to_gid, dest)
                        end if
                    end if
                else if (drag_to_row() == 1) then
                    if (was_gid /= 0) then
                        ! Carried out of its group and dropped on the bar. It
                        ! leaves the group and becomes a tab in its own right,
                        ! which is the whole point of the gesture.
                        call group_remove_member(editor, from_idx)
                        call prune_empty_groups(editor)
                    end if
                    from_idx = find_tab_by_path_public(editor, trim(drag_path()))
                    if (from_idx /= 0) call reorder_tab(editor, from_idx, dest)
                end if
            end if
        end if

        call end_drag()
    end subroutine tab_drag_release

    !> Let go of everything the drag was holding, including the group strip a
    !> dwell may have opened -- that strip exists only as a drop target and
    !> would otherwise stay open over a drag that has finished.
    subroutine end_drag()
        use tab_drag_module, only: drag_cancel
        use renderer_module, only: tab_group_clear_hover
        logical :: ignored

        call drag_cancel()
        g_dwell_gid = 0
        ignored = tab_group_clear_hover()
        g_lsp_ui_changed = .true.
    end subroutine end_drag

    !> Move a tab into `gid`, landing at position `pos` on its member row.
    !>
    !> The array move afterwards is not cosmetic. Row 1 draws a group at its
    !> FIRST member's position, so a tab that joins from the far side of the
    !> array would yank the whole group entry across the bar to wherever the
    !> newcomer happened to sit. Pulling the block back to where it already was
    !> keeps the group still while its membership changes.
    subroutine join_group_at(editor, tab_idx, gid, pos)
        use editor_state_module, only: group_add_member, group_remove_member, &
                                       set_group_ordinal, reorder_group_block, &
                                       prune_empty_groups, group_members
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pos
        integer(int32), intent(in) :: gid
        integer, allocatable :: members(:)
        integer :: block_start, i

        ! Where the group sits now, before the newcomer can influence it.
        block_start = 0
        call group_members(editor, gid, members)
        do i = 1, size(members)
            if (block_start == 0 .or. members(i) < block_start) block_start = members(i)
        end do
        if (block_start == 0) return

        if (editor%tabs(tab_idx)%group_id /= 0) then
            call group_remove_member(editor, tab_idx)
            call prune_empty_groups(editor)
        end if
        call group_add_member(editor, gid, tab_idx)
        call set_group_ordinal(editor, tab_idx, pos)
        call reorder_group_block(editor, gid, block_start)
    end subroutine join_group_at

    !> Right-click on an ordinary tab.
    subroutine open_tab_context_menu(editor, tab_idx, mrow, mcol)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, mrow, mcol
        integer :: top_row, bottom_row, left_col, right_col, others
        logical :: shown, in_group

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%filename)) return

        ! By path, once, here. Everything the menu then does re-finds the tab
        ! rather than trusting this index to still mean the same file.
        if (allocated(g_menu_tab_path)) deallocate(g_menu_tab_path)
        g_menu_tab_path = editor%tabs(tab_idx)%filename

        others = size(editor%tabs) - 1
        in_group = editor%tabs(tab_idx)%group_id /= 0

        call context_menu_begin(CTX_KIND_TAB)
        call context_menu_add_item('Close Tab', 'Ctrl+W', ACT_TAB_CLOSE)
        ! Greyed rather than hidden when there is nothing else open, so the
        ! menu keeps the same shape and the row does not move under the
        ! pointer between one right-click and the next.
        call context_menu_add_item('Close Other Tabs', '', &
            ACT_TAB_CLOSE_OTHERS, enabled=(others > 0))
        call context_menu_add_separator()
        call context_menu_add_item('Copy Path', '', ACT_TAB_COPY_PATH)
        call context_menu_add_item('Remove from Group', '', &
            ACT_TAB_UNGROUP, enabled=in_group)

        call menu_bounds(editor, top_row, bottom_row, left_col, right_col)
        ! A menu opened FROM the tab bar hangs directly under the row that was
        ! clicked, rather than being pushed below the whole bar. Inside a group
        ! the bar is two rows, so the general rule left two rows of dead space
        ! between the pointer and the thing it is travelling to.
        if (mrow < top_row) top_row = min(mrow + 1, bottom_row)
        shown = context_menu_show(mrow, mcol, top_row, bottom_row, left_col, right_col)
        if (shown) g_lsp_ui_changed = .true.
    end subroutine open_tab_context_menu

    !> Close the tab holding `path`, prompting if it has unsaved changes.
    !>
    !> `was_modified` is passed in rather than read here because a CALLER
    !> closing several tabs has to sample every flag before the first close:
    !> switching tabs writes editor%modified back over the outgoing tab's
    !> flag, so reading them as you go reads values the switching destroyed.
    subroutine close_tab_by_path(editor, buffer, path, was_modified, closed)
        use editor_state_module, only: find_tab_by_path_public
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: path
        logical, intent(in) :: was_modified
        logical, intent(out) :: closed
        integer :: tab_idx
        logical :: dirty

        ! Copied out FIRST. Fortran passes by reference, so if a caller hands
        ! us editor%tabs(i)%modified directly then `was_modified` is not a
        ! snapshot -- it is a live view of a field that switch_to_tab_with_buffer
        ! is about to overwrite, and close_tab is about to reallocate out from
        ! under. Reading it after either one reads the wrong answer, or freed
        ! memory. Callers should pass a local; this makes it safe either way.
        dirty = was_modified
        closed = .false.
        tab_idx = find_tab_by_path_public(editor, trim(path))
        if (tab_idx == 0) return

        ! The prompt names a file and acts on the ACTIVE tab, so the user has
        ! to be looking at the one being asked about.
        call switch_to_tab_with_buffer(editor, tab_idx, buffer)
        tab_idx = find_tab_by_path_public(editor, trim(path))
        if (tab_idx == 0) return

        if (dirty) then
            call prompt_save_before_close_tab(editor, buffer)
        else
            call close_tab_without_prompt(editor, buffer)
        end if
        closed = find_tab_by_path_public(editor, trim(path)) == 0
    end subroutine close_tab_by_path

    !> Act on the tab the open menu named.
    subroutine tab_menu_action(editor, buffer, act)
        use editor_state_module, only: find_tab_by_path_public, &
                                       group_remove_member, prune_empty_groups
        use clipboard_module, only: copy_to_clipboard
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: act
        ! Allocatable, not fixed. 256 x 512 characters is far past
        ! -fmax-stack-var-size, so gfortran quietly moves a fixed array of
        ! that size to STATIC storage -- shared between calls, and the
        ! procedure stops being reentrant. Sizing it to the tabs actually
        ! open costs one allocation and keeps it on the stack where a local
        ! belongs.
        character(len=512), allocatable :: others(:)
        logical, allocatable :: others_modified(:)
        integer :: n_others, i, tab_idx, gone
        logical :: closed, dirty

        if (.not. allocated(g_menu_tab_path)) return
        tab_idx = find_tab_by_path_public(editor, trim(g_menu_tab_path))
        if (tab_idx == 0) then
            call set_status_message('That tab is no longer open')
            return
        end if

        select case (act)
        case (ACT_TAB_CLOSE)
            ! Into a local first -- see close_tab_by_path on why passing the
            ! component straight through reads a field that has since moved.
            dirty = editor%tabs(tab_idx)%modified
            call close_tab_by_path(editor, buffer, trim(g_menu_tab_path), &
                                   dirty, closed)

        case (ACT_TAB_CLOSE_OTHERS)
            ! Every path and flag sampled BEFORE the first close, for both
            ! reasons at once: closing renumbers the indices this loop would
            ! otherwise walk, and switching clobbers the flags it would read.
            allocate(others(max(1, size(editor%tabs))))
            allocate(others_modified(size(others)))
            n_others = 0
            do i = 1, size(editor%tabs)
                if (i == tab_idx) cycle
                if (.not. allocated(editor%tabs(i)%filename)) cycle
                if (n_others >= size(others)) exit
                n_others = n_others + 1
                others(n_others) = editor%tabs(i)%filename
                others_modified(n_others) = editor%tabs(i)%modified
            end do
            gone = 0
            do i = 1, n_others
                call close_tab_by_path(editor, buffer, trim(others(i)), &
                                       others_modified(i), closed)
                if (closed) gone = gone + 1
            end do
            ! Land back on the tab the menu was about, which is the one the
            ! user kept -- closing the others walked away from it.
            tab_idx = find_tab_by_path_public(editor, trim(g_menu_tab_path))
            if (tab_idx >= 1) &
                call switch_to_tab_with_buffer(editor, tab_idx, buffer)
            call set_status_message('Closed ' // trim(int_to_text(gone)) // &
                ' other tabs')

        case (ACT_TAB_COPY_PATH)
            call copy_to_clipboard(trim(g_menu_tab_path))
            call set_status_message('Copied ' // trim(g_menu_tab_path))

        case (ACT_TAB_UNGROUP)
            if (editor%tabs(tab_idx)%group_id == 0) return
            call group_remove_member(editor, tab_idx)
            ! A group with nothing left in it is not a group.
            call prune_empty_groups(editor)
            call set_status_message('Removed from the group')
        end select

        g_lsp_ui_changed = .true.
    end subroutine tab_menu_action

    !> Open the group dialog on an existing group, members already ticked.
    subroutine start_group_edit(editor, gid)
        use editor_state_module, only: group_find, group_members, &
                                       group_member_count
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        integer, allocatable :: idx(:)
        integer :: gidx, i, n
        character(len=:), allocatable :: dir, label

        gidx = group_find(editor, gid)
        if (gidx == 0) return

        n = group_member_count(editor, gid)
        dir = group_edit_dir(editor, gid)

        label = ''
        if (allocated(editor%groups(gidx)%label)) label = editor%groups(gidx)%label

        if (.not. group_picker_show_edit(dir, label, editor%screen_rows, &
                                         editor%screen_cols)) then
            call set_status_message('Cannot read ' // dir)
            return
        end if

        ! Tick what is already in the group. By PATH, so a member in a
        ! different directory than the one we opened at is still ticked and
        ! still counted -- it simply is not on screen until you walk to it.
        if (n > 0) then
            call group_members(editor, gid, idx)
            do i = 1, size(idx)
                if (idx(i) < 1 .or. idx(i) > size(editor%tabs)) cycle
                if (.not. allocated(editor%tabs(idx(i))%filename)) cycle
                ! An [Untitled] member has no path to tick with. Left alone
                ! rather than listed, so confirming cannot silently drop an
                ! unsaved scratch buffer.
                if (index(editor%tabs(idx(i))%filename, '[Untitled') == 1) cycle
                call group_picker_preselect(editor%tabs(idx(i))%filename)
                if (editor%tabs(idx(i))%modified) &
                    call group_picker_mark_dirty(editor%tabs(idx(i))%filename)
            end do
        end if

        g_editing_gid = gid
        editor%fuss_mode_active = .false.
        editor%terminal_panel%focused = .false.
        g_lsp_ui_changed = .true.
    end subroutine start_group_edit

    !> Rename a group in place. The members are untouched.
    subroutine rename_group_prompt(editor, gid)
        use editor_state_module, only: group_find
        use text_prompt_module, only: show_text_prompt
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        character(len=512) :: new_name
        logical :: cancelled
        integer :: gidx

        gidx = group_find(editor, gid)
        if (gidx == 0) return

        call show_text_prompt('Rename group to: ', new_name, cancelled, &
                              int(editor%screen_rows, int32))
        if (cancelled) return
        if (len_trim(new_name) == 0) return

        if (allocated(editor%groups(gidx)%label)) &
            deallocate(editor%groups(gidx)%label)
        editor%groups(gidx)%label = trim(new_name)
        call set_status_message('Renamed group to ' // trim(new_name))
        g_lsp_ui_changed = .true.
    end subroutine rename_group_prompt

    !> Break the group up, leaving every member open as an ordinary tab.
    subroutine dissolve_group_now(editor, gid)
        use editor_state_module, only: group_find, group_dissolve, &
                                       group_member_count
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        integer :: n

        if (group_find(editor, gid) == 0) return
        n = group_member_count(editor, gid)
        call group_dissolve(editor, gid)
        call set_status_message('Dissolved the group; ' // &
            trim(int_to_text(n)) // ' tabs are still open')
        g_lsp_ui_changed = .true.
    end subroutine dissolve_group_now

    !> Where the edit dialog should open.
    !>
    !> The group's origin directory if it still reads; else wherever most of
    !> its members live, which is the only sensible answer for a group
    !> assembled from scattered files; else the workspace root.
    function group_edit_dir(editor, gid) result(dir)
        use editor_state_module, only: group_find
        use dir_scan_module, only: dir_entry_t, list_directory
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        character(len=:), allocatable :: dir, cand
        type(dir_entry_t), allocatable :: probe(:)
        integer :: gidx, i, j, n, best, count
        logical :: ok

        dir = ''
        gidx = group_find(editor, gid)

        if (gidx >= 1) then
            if (allocated(editor%groups(gidx)%dir_path)) then
                cand = editor%groups(gidx)%dir_path
                ! Belt and braces. The restore path resolves this against the
                ! workspace now, but a relative directory here would silently
                ! be read relative to the process's working directory, and the
                ! only symptom is a dialog with nothing ticked.
                if (len_trim(cand) > 0) then
                    if (cand(1:1) /= '/' .and. allocated(editor%workspace_path)) &
                        cand = trim(editor%workspace_path) // '/' // trim(cand)
                end if
                if (len_trim(cand) > 0) then
                    call list_directory(trim(cand), probe, n, ok)
                    if (ok) then
                        dir = trim(cand)
                        return
                    end if
                end if
            end if
        end if

        ! The commonest parent among the members. O(n^2) over a member list
        ! that is a handful of files, and the alternative -- sorting paths --
        ! buys nothing at this size.
        best = 0
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id /= gid) cycle
            if (.not. allocated(editor%tabs(i)%filename)) cycle
            cand = parent_dir_of(editor%tabs(i)%filename)
            if (len_trim(cand) == 0) cycle
            count = 0
            do j = 1, size(editor%tabs)
                if (editor%tabs(j)%group_id /= gid) cycle
                if (.not. allocated(editor%tabs(j)%filename)) cycle
                if (parent_dir_of(editor%tabs(j)%filename) == cand) count = count + 1
            end do
            if (count > best) then
                best = count
                dir = cand
            end if
        end do
        if (len_trim(dir) > 0) then
            call list_directory(trim(dir), probe, n, ok)
            if (ok) return
            dir = ''
        end if

        if (allocated(editor%workspace_path)) dir = editor%workspace_path
        if (len_trim(dir) == 0) dir = '.'
    end function group_edit_dir

    function parent_dir_of(path) result(d)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: d
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash > 1) then
            d = path(1:slash - 1)
        else if (slash == 1) then
            d = '/'
        else
            d = ''
        end if
    end function parent_dir_of

    !> Close the one surface sitting closest to the user, if any.
    !>
    !> The order is the same "what is on top" the mouse router uses: the
    !> terminal panel and the floating popups sit above the modal side
    !> panels, which sit above the file tree, which sits above the document.
    !> Dismissal used to be spread across the terminal-panel router, the
    !> fuss-mode router, five per-panel handlers and the esc case, so no
    !> single place knew that order; this states it once.
    subroutine close_topmost_surface(editor, closed)
        type(editor_state_t), intent(inout) :: editor
        logical, intent(out) :: closed

        closed = .true.

        ! The menu floats above everything and is the most transient thing on
        ! screen, so it goes first.
        if (is_context_menu_visible()) then
            call context_menu_hide()
            return
        end if

        if (is_terminal_panel_visible(editor%terminal_panel)) then
            editor%terminal_panel%visible = .false.
            editor%terminal_panel%focused = .false.
            call terminal_write(achar(27) // '[2J')
            return
        end if

        if (is_completion_visible(editor%completion_popup)) then
            call hide_completion_popup(editor%completion_popup)
            return
        end if

        if (is_hover_visible(editor%hover_tooltip)) then
            call hide_hover_tooltip(editor%hover_tooltip)
            return
        end if

        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) then
            call hide_lsp_server_installer_panel(editor%lsp_installer_panel)
            return
        end if

        if (is_code_actions_panel_visible(editor%code_actions_panel)) then
            call hide_code_actions_panel(editor%code_actions_panel)
            return
        end if

        if (is_references_panel_visible(editor%references_panel)) then
            call hide_references_panel(editor%references_panel)
            return
        end if

        if (is_symbols_panel_visible(editor%symbols_panel)) then
            call hide_symbols_panel(editor%symbols_panel)
            return
        end if

        if (is_diagnostics_panel_visible(editor%diagnostics_panel)) then
            ! No hide entry point on this one; the toggle is the way to close
            call toggle_panel(editor%diagnostics_panel)
            return
        end if

        ! The tree is last: it is a layout rather than something floating
        ! over the document, so it is the least surprising thing to lose on
        ! the press before the one that quits.
        if (editor%fuss_mode_active) then
            call toggle_fuss_mode(editor)
            return
        end if

        closed = .false.
    end subroutine close_topmost_surface

    !> Where on screen a cursor is drawn: the renderer's forward mapping,
    !> shared by the alt-click hit test and the keyboard menu opener so the
    !> two cannot drift apart.
    subroutine caret_screen_pos(cursor, editor, buffer, screen_row, screen_col)
        use renderer_module, only: show_line_numbers, LINE_NUMBER_WIDTH
        use editor_state_module, only: get_active_pane_indices
        type(cursor_t), intent(in) :: cursor
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(out) :: screen_row, screen_col
        integer :: row_offset, col_offset, tab_idx, pane_idx
        logical :: used_pane
        character(len=:), allocatable :: line

        ! Gutter offset plus the display-cell distance from the viewport
        ! start, so tabs and wide characters line up with the text
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1
        else
            col_offset = 0
        end if

        line = buffer_get_line(buffer, cursor%line)
        used_pane = .false.

        ! Use the active pane's rect and viewport, the same way
        ! position_cursor_at_screen inverts them. Assuming row 2 and the
        ! editor-level viewport made this disagree with where the caret is
        ! actually drawn in a split or with the tree open.
        call get_active_pane_indices(editor, tab_idx, pane_idx)
        if (tab_idx > 0 .and. pane_idx > 0) then
            if (tab_idx <= size(editor%tabs)) then
                if (allocated(editor%tabs(tab_idx)%panes)) then
                    if (pane_idx <= size(editor%tabs(tab_idx)%panes)) then
                        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
                            screen_row = pane%screen_row + &
                                (cursor%line - pane%viewport_line)
                            screen_col = pane%screen_col + col_offset + &
                                display_offset_of(line, pane%viewport_column, &
                                                  cursor%column)
                        end associate
                        used_pane = .true.
                    end if
                end if
            end if
        end if

        if (.not. used_pane) then
            row_offset = first_content_row(editor)
            screen_row = cursor%line - editor%viewport_line + row_offset
            screen_col = col_offset + 1 + &
                display_offset_of(line, editor%viewport_column, cursor%column)
        end if
    end subroutine caret_screen_pos

    function is_cursor_at_screen_pos(cursor, editor, buffer, screen_row, screen_col) result(at_pos)
        type(cursor_t), intent(in) :: cursor
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: screen_row, screen_col
        logical :: at_pos
        integer :: cursor_screen_row, cursor_screen_col

        call caret_screen_pos(cursor, editor, buffer, cursor_screen_row, &
                              cursor_screen_col)
        at_pos = (cursor_screen_row == screen_row .and. cursor_screen_col == screen_col)
    end function is_cursor_at_screen_pos


    subroutine select_next_match(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), allocatable :: new_cursors(:)
        character(len=:), allocatable :: word, line
        integer :: i
        integer :: found_line, found_col, start_byte, start_char, end_char
        logical :: found

        ! If no pattern selected yet, select word at cursor
        if (.not. allocated(search_pattern)) then
            call select_word_at_cursor(editor%cursors(editor%active_cursor), buffer)
            word = get_selected_text(editor%cursors(editor%active_cursor), buffer)
            if (allocated(word)) then
                search_pattern = word
            end if
        else
            ! Search for next occurrence (byte positions; cursor cols are chars)
            line = buffer_get_line(buffer, editor%cursors(size(editor%cursors))%line)
            start_byte = utf8_char_to_byte_index(line, &
                editor%cursors(size(editor%cursors))%column)
            if (start_byte == 0) start_byte = len(line) + 1
            call find_next_occurrence(buffer, search_pattern, &
                                     editor%cursors(size(editor%cursors))%line, &
                                     start_byte, &
                                     found, found_line, found_col)

            if (found) then
                line = buffer_get_line(buffer, found_line)
                start_char = utf8_byte_to_char_index(line, found_col)
                end_char = utf8_byte_to_char_index(line, found_col + len(search_pattern))
                if (start_char == 0) start_char = utf8_char_count(line) + 1
                if (end_char == 0) end_char = utf8_char_count(line) + 1

                ! Add a new cursor at the found position
                allocate(new_cursors(size(editor%cursors) + 1))
                do i = 1, size(editor%cursors)
                    new_cursors(i) = editor%cursors(i)
                end do

                ! Initialize new cursor
                call init_cursor(new_cursors(size(new_cursors)))
                new_cursors(size(new_cursors))%line = found_line
                new_cursors(size(new_cursors))%has_selection = .true.
                new_cursors(size(new_cursors))%selection_start_line = found_line
                new_cursors(size(new_cursors))%selection_start_col = start_char
                new_cursors(size(new_cursors))%column = end_char

                deallocate(editor%cursors)
                editor%cursors = new_cursors
                editor%active_cursor = size(editor%cursors)
            end if
        end if
    end subroutine select_next_match

    subroutine select_word_at_cursor(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: word_start, word_end, byte_pos

        line = buffer_get_line(buffer, cursor%line)

        ! Find word boundaries (byte positions; the cursor column is chars)
        byte_pos = utf8_char_to_byte_index(line, cursor%column)
        if (byte_pos == 0) byte_pos = len(line) + 1
        call find_word_boundaries(line, byte_pos, word_start, word_end)

        if (word_start > 0 .and. word_end >= word_start) then
            ! Select the word (selection columns are char indices)
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = utf8_byte_to_char_index(line, word_start)
            cursor%column = utf8_byte_to_char_index(line, word_end + 1)
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine select_word_at_cursor

    function get_selected_text(cursor, buffer) result(text)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text
        character(len=:), allocatable :: line
        integer :: start_col, end_col

        if (.not. cursor%has_selection) then
            allocate(character(len=0) :: text)
            return
        end if

        ! For single-line selection only (for now); selection columns are
        ! char indices, slicing needs bytes
        if (cursor%selection_start_line == cursor%line) then
            line = buffer_get_line(buffer, cursor%line)
            start_col = utf8_char_to_byte_index(line, &
                min(cursor%selection_start_col, cursor%column))
            end_col = utf8_char_to_byte_index(line, &
                max(cursor%selection_start_col, cursor%column)) - 1
            if (end_col == -1) end_col = len(line)

            if (start_col >= 1 .and. start_col <= len(line) .and. end_col <= len(line)) then
                text = line(start_col:end_col)
            else
                allocate(character(len=0) :: text)
            end if
            if (allocated(line)) deallocate(line)
        else
            allocate(character(len=0) :: text)
        end if
    end function get_selected_text

    ! LSP positions are 0-based UTF-16 code-unit offsets; cursor columns
    ! are 1-based char indices. These two convert via the line text.
    function lsp_char_of(buffer, line_num, char_col) result(units)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, char_col
        integer :: units
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        units = utf8_char_col_to_utf16(line, char_col)
    end function lsp_char_of

    function char_col_from_lsp(buffer, line_num, lsp_units) result(char_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, lsp_units
        integer :: char_col
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        char_col = utf16_to_utf8_char_col(line, lsp_units)
    end function char_col_from_lsp

    ! pos, word_start and word_end are BYTE positions in line (word chars
    ! are ASCII, but preceding multibyte text shifts bytes vs chars -
    ! callers must convert from/to cursor char columns)
    subroutine find_word_boundaries(line, pos, word_start, word_end)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: word_start, word_end
        integer :: i

        word_start = 0
        word_end = 0

        ! Check if we're on a word character
        if (pos <= len(line)) then
            if (.not. is_word_char(line(pos:pos))) then
                return
            end if

            ! Find start of word
            word_start = pos
            do i = pos - 1, 1, -1
                if (is_word_char(line(i:i))) then
                    word_start = i
                else
                    exit
                end if
            end do

            ! Find end of word
            word_end = pos
            do i = pos + 1, len(line)
                if (is_word_char(line(i:i))) then
                    word_end = i
                else
                    exit
                end if
            end do
        end if
    end subroutine find_word_boundaries

    subroutine find_next_occurrence(buffer, pattern, start_line, start_col, &
                                    found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        character(len=:), allocatable :: search_line, search_pattern
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

            ! Perform case-sensitive or case-insensitive search
            if (match_case_sensitive) then
                pos = index(line(search_col:), pattern)
            else
                search_line = to_lower(line(search_col:))
                search_pattern = to_lower(pattern)
                pos = index(search_line, search_pattern)
                if (allocated(search_line)) deallocate(search_line)
            end if

            if (pos > 0) then
                found = .true.
                found_line = current_line
                found_col = search_col + pos - 1
                if (allocated(line)) deallocate(line)
                if (allocated(search_pattern)) deallocate(search_pattern)
                return
            end if
            if (allocated(line)) deallocate(line)
        end do

        ! Wrap around to beginning
        do current_line = 1, start_line
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                ! Search only up to start position
                if (start_col > 1) then
                    if (match_case_sensitive) then
                        pos = index(line(1:start_col-1), pattern)
                    else
                        search_line = to_lower(line(1:start_col-1))
                        search_pattern = to_lower(pattern)
                        pos = index(search_line, search_pattern)
                        if (allocated(search_line)) deallocate(search_line)
                    end if
                else
                    pos = 0
                end if
            else
                if (match_case_sensitive) then
                    pos = index(line, pattern)
                else
                    search_line = to_lower(line)
                    search_pattern = to_lower(pattern)
                    pos = index(search_line, search_pattern)
                    if (allocated(search_line)) deallocate(search_line)
                end if
            end if

            if (pos > 0) then
                found = .true.
                found_line = current_line
                found_col = pos
                if (allocated(line)) deallocate(line)
                if (allocated(search_pattern)) deallocate(search_pattern)
                return
            end if

            if (allocated(line)) deallocate(line)
        end do

        if (allocated(search_pattern)) deallocate(search_pattern)
    end subroutine find_next_occurrence

    ! Helper function to convert a string to lowercase for case-insensitive comparison
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i

        allocate(character(len=len(str)) :: lower_str)

        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. &
                iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = char(iachar(str(i:i)) + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
    end function to_lower

    ! ========================================================================
    ! Buffer Helper Functions - Wrappers for cursor-based operations
    ! ========================================================================

    subroutine buffer_delete_at_cursor(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: pos, nbytes

        ! Convert cursor position to buffer position and delete the whole
        ! character there — one byte of a multibyte char would corrupt it
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        if (pos > 0 .and. pos <= get_buffer_content_size(buffer)) then
            nbytes = utf8_lead_len(buffer_get_char(buffer, pos))
            call buffer_delete(buffer, pos, nbytes)
        end if
    end subroutine buffer_delete_at_cursor

    subroutine buffer_insert_char(buffer, cursor, ch)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        character, intent(in) :: ch
        integer :: pos

        ! Convert cursor position to buffer position
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, ch)
    end subroutine buffer_insert_char

    subroutine buffer_insert_newline(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: pos

        ! Convert cursor position to buffer position
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, char(10))
    end subroutine buffer_insert_newline

    subroutine buffer_insert_text_at(buffer, line, column, text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: text
        integer :: pos

        ! Convert line/column to buffer position
        pos = get_buffer_position(buffer, line, column)
        call buffer_insert(buffer, pos, text)
    end subroutine buffer_insert_text_at

    subroutine buffer_delete_range(buffer, start_line, start_col, end_line, end_col)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: start_line, start_col, end_line, end_col
        integer :: start_pos, end_pos, count

        ! Convert positions to buffer positions
        start_pos = get_buffer_position(buffer, start_line, start_col)
        end_pos = get_buffer_position(buffer, end_line, end_col)
        count = end_pos - start_pos

        if (count > 0) then
            call buffer_delete(buffer, start_pos, count)
        end if
    end subroutine buffer_delete_range

    function get_buffer_position(buffer, line, column) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line, column
        integer :: pos
        integer :: current_line, i, col_in_line
        character :: ch, next_ch

        pos = 1
        current_line = 1
        col_in_line = 1

        ! Find the byte position for the given line and CHARACTER column.
        ! Columns are UTF-8 character indices (the cursor_t contract), so a
        ! multibyte character advances the column once, not per byte.
        do i = 1, get_buffer_content_size(buffer)
            if (current_line == line .and. col_in_line == column) then
                pos = i
                return
            end if

            ch = buffer_get_char(buffer, i)
            if (ch == char(10)) then
                if (current_line == line) then
                    ! We're at the end of the target line
                    pos = i
                    return
                end if
                current_line = current_line + 1
                col_in_line = 1
            else
                ! Advance the column only when the next byte starts a new
                ! character (continuation bytes belong to the current one)
                next_ch = buffer_get_char(buffer, i + 1)
                if (utf8_is_valid_start(iachar(next_ch))) then
                    col_in_line = col_in_line + 1
                end if
            end if
        end do

        ! If we reach here, we're at the end of the buffer
        pos = get_buffer_content_size(buffer) + 1
    end function get_buffer_position

    function get_buffer_content_size(buffer) result(size)
        type(buffer_t), intent(in) :: buffer
        integer :: size

        size = buffer%size - (buffer%gap_end - buffer%gap_start)
    end function get_buffer_content_size

    ! Byte length of a UTF-8 character from its lead byte (1 for ASCII
    ! and for invalid/continuation bytes)
    pure function utf8_lead_len(ch) result(nbytes)
        character, intent(in) :: ch
        integer :: nbytes
        integer :: b

        b = iachar(ch)
        if (b >= 192 .and. b <= 223) then
            nbytes = 2
        else if (b >= 224 .and. b <= 239) then
            nbytes = 3
        else if (b >= 240 .and. b <= 247) then
            nbytes = 4
        else
            nbytes = 1
        end if
    end function utf8_lead_len

    ! Insert a whole (possibly multibyte) string at the cursor's character
    ! position without advancing the cursor
    subroutine buffer_insert_string(buffer, cursor, s)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        character(len=*), intent(in) :: s
        integer :: pos

        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, s)
    end subroutine buffer_insert_string

    ! ========================================================================
    ! Multiple Cursor Addition Above/Below
    ! ========================================================================

    subroutine add_cursor_above(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t), allocatable :: new_cursors(:)
        type(cursor_t) :: active_cursor
        integer :: i, new_line

        active_cursor = editor%cursors(editor%active_cursor)
        new_line = active_cursor%line - 1

        ! Check if we can add a cursor above
        if (new_line < 1) return

        ! Allocate space for additional cursor
        allocate(new_cursors(size(editor%cursors) + 1))

        ! Copy existing cursors
        do i = 1, size(editor%cursors)
            new_cursors(i) = editor%cursors(i)
        end do

        ! Add new cursor above
        new_cursors(size(new_cursors))%line = new_line
        new_cursors(size(new_cursors))%column = active_cursor%column
        new_cursors(size(new_cursors))%desired_column = active_cursor%desired_column
        new_cursors(size(new_cursors))%has_selection = .false.

        ! Replace cursors array
        call move_alloc(new_cursors, editor%cursors)

        ! Set the new cursor as active
        editor%active_cursor = size(editor%cursors)
    end subroutine add_cursor_above

    subroutine add_cursor_below(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), allocatable :: new_cursors(:)
        type(cursor_t) :: active_cursor
        integer :: i, new_line, line_count

        active_cursor = editor%cursors(editor%active_cursor)
        line_count = buffer_get_line_count(buffer)
        new_line = active_cursor%line + 1

        ! Check if we can add a cursor below
        if (new_line > line_count) return

        ! Allocate space for additional cursor
        allocate(new_cursors(size(editor%cursors) + 1))

        ! Copy existing cursors
        do i = 1, size(editor%cursors)
            new_cursors(i) = editor%cursors(i)
        end do

        ! Add new cursor below
        new_cursors(size(new_cursors))%line = new_line
        new_cursors(size(new_cursors))%column = active_cursor%column
        new_cursors(size(new_cursors))%desired_column = active_cursor%desired_column
        new_cursors(size(new_cursors))%has_selection = .false.

        ! Replace cursors array
        call move_alloc(new_cursors, editor%cursors)

        ! Set the new cursor as active
        editor%active_cursor = size(editor%cursors)
    end subroutine add_cursor_below

    subroutine jump_to_matching_bracket(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        logical :: found
        integer :: match_line, match_col

        ! Find matching bracket from current cursor position
        call find_matching_bracket(buffer, &
                                  editor%cursors(editor%active_cursor)%line, &
                                  editor%cursors(editor%active_cursor)%column, &
                                  found, match_line, match_col)

        if (found) then
            ! Jump to the matching bracket
            editor%cursors(editor%active_cursor)%line = match_line
            editor%cursors(editor%active_cursor)%column = match_col
            editor%cursors(editor%active_cursor)%desired_column = match_col

            ! Update viewport to ensure cursor is visible
            call update_viewport(editor)
        end if
    end subroutine jump_to_matching_bracket

    ! ========================================================================
    ! Selection Extension Subroutines
    ! ========================================================================

    subroutine extend_selection_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: current_line, target_line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor up
        if (cursor%line > 1) then
            current_line = buffer_get_line(buffer, cursor%line)
            cursor%line = cursor%line - 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from empty line, go to end of target line
            if (len(current_line) == 0) then
                cursor%column = len(target_line) + 1
                cursor%desired_column = cursor%column
            else
                cursor%column = cursor%desired_column
                if (cursor%column > utf8_char_count(target_line) + 1) then
                    cursor%column = utf8_char_count(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine extend_selection_up

    subroutine extend_selection_down(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: current_line, target_line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor down
        if (cursor%line < line_count) then
            current_line = buffer_get_line(buffer, cursor%line)
            cursor%line = cursor%line + 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from empty line, go to column 1 of target line
            if (len(current_line) == 0) then
                cursor%column = 1
                cursor%desired_column = 1
            else
                cursor%column = cursor%desired_column
                if (cursor%column > utf8_char_count(target_line) + 1) then
                    cursor%column = utf8_char_count(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine extend_selection_down

    subroutine extend_selection_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor left
        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = utf8_char_count(line) + 1
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
        end if
    end subroutine extend_selection_left

    subroutine extend_selection_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        ! Move cursor right
        if (cursor%column <= utf8_char_count(line)) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_right

    subroutine extend_selection_home(cursor)
        type(cursor_t), intent(inout) :: cursor

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        cursor%column = 1
        cursor%desired_column = 1
    end subroutine extend_selection_home

    subroutine extend_selection_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = utf8_char_count(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_end

    subroutine extend_selection_page_up(cursor, editor)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer :: page_size

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! A page is what you can see: the terminal panel's rows are not ours.
        page_size = text_area_height(editor)
        cursor%line = max(1, cursor%line - page_size)
        cursor%column = cursor%desired_column
    end subroutine extend_selection_page_up

    subroutine extend_selection_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! A page is what you can see: the terminal panel's rows are not ours.
        page_size = text_area_height(editor)
        cursor%line = min(line_count, cursor%line + page_size)
        cursor%column = cursor%desired_column
    end subroutine extend_selection_page_down

    subroutine extend_selection_word_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_len

        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)

        if (line_len == 0) then
            if (cursor%line > 1) then
                cursor%line = cursor%line - 1
                if (allocated(line)) deallocate(line)
                line = buffer_get_line(buffer, cursor%line)
                cursor%column = &
                    utf8_char_count(line) + 1
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
            return
        end if

        pos = utf8_char_to_byte_index(line, &
            cursor%column)
        if (pos == 0) pos = line_len + 1

        if (pos > 1 .and. line_len > 0) then
            pos = pos - 1

            do while (pos > 1 .and. pos <= line_len)
                if (line(pos:pos) /= ' ') exit
                pos = pos - 1
            end do

            if (pos >= 1 .and. pos <= line_len) then
                if (is_word_char(line(pos:pos))) then
                    do while (pos > 1)
                        if (pos-1 < 1) exit
                        if (.not. is_word_char( &
                            line(pos-1:pos-1))) exit
                        pos = pos - 1
                    end do
                end if
            end if

            if (pos < 1) pos = 1
            if (pos > line_len + 1) pos = line_len + 1

            cursor%column = &
                utf8_byte_to_char_index(line, pos)
        else if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            if (allocated(line)) deallocate(line)
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = &
                utf8_char_count(line) + 1
        else
            cursor%column = 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_word_left

    subroutine extend_selection_word_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_count, line_len, char_len

        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)
        line_len = len(line)
        char_len = utf8_char_count(line)

        pos = utf8_char_to_byte_index(line, &
            cursor%column)
        if (pos == 0) pos = line_len + 1
        if (pos < 1) pos = 1

        if (line_len == 0 .or. pos > line_len) then
            if (cursor%line < line_count) then
                cursor%line = cursor%line + 1
                cursor%column = 1
            else
                cursor%column = char_len + 1
            end if
        else if (pos >= 1 .and. pos <= line_len) then
            if (line(pos:pos) == ' ') then
                do while (pos < line_len)
                    if (pos+1 <= line_len .and. &
                        line(pos+1:pos+1) == ' ') then
                        pos = pos + 1
                    else
                        exit
                    end if
                end do
                pos = pos + 1
            else if (is_word_char(line(pos:pos))) then
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (.not. is_word_char( &
                            line(pos+1:pos+1))) exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1
                do while (pos <= line_len)
                    if (line(pos:pos) /= ' ') exit
                    pos = pos + 1
                end do
            else
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (is_word_char( &
                            line(pos+1:pos+1)) .or. &
                            line(pos+1:pos+1) == ' ') &
                            exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1
                do while (pos <= line_len)
                    if (line(pos:pos) /= ' ') exit
                    pos = pos + 1
                end do
            end if

            cursor%column = &
                utf8_byte_to_char_index(line, pos)
        else if (cursor%line < line_count) then
            cursor%line = cursor%line + 1
            cursor%column = 1
        else
            cursor%column = char_len + 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_word_right

    ! ========================================================================
    ! Word Deletion Subroutines
    ! ========================================================================

    subroutine delete_word_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: start_col, end_col, line_len, char_len
        logical :: in_word

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        char_len = utf8_char_count(line)

        if (cursor%column > char_len + 1) then
            if (allocated(line)) deallocate(line)
            return
        end if

        start_col = utf8_char_to_byte_index(line, &
            cursor%column)
        if (start_col == 0) start_col = line_len + 1
        end_col = start_col

        if (end_col <= line_len) then
            in_word = is_word_char( &
                line(end_col:end_col))
            do while (end_col < line_len)
                if (is_word_char( &
                    line(end_col:end_col)) &
                    .eqv. in_word) then
                    end_col = end_col + 1
                else
                    exit
                end if
            end do

            if (end_col <= line_len) then
                if (is_word_char( &
                    line(end_col:end_col)) &
                    .eqv. in_word) then
                    end_col = end_col + 1
                end if
            end if

            do while (end_col <= line_len)
                if (line(end_col:end_col) == ' ') then
                    end_col = end_col + 1
                else
                    exit
                end if
            end do

            if (end_col > start_col) then
                call delete_range(buffer, &
                    cursor%line, start_col, &
                    cursor%line, end_col - 1)
                buffer%modified = .true.
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_word_forward

    ! Delete the word before the caret. At column 1 there is no word left on
    ! this line, so the line break itself is what gets deleted -- otherwise
    ! alt-backspace stalls at the start of a line and can never clear a blank
    ! one. Reports the deleted range (character columns, end-exclusive) so the
    ! multi-cursor path can shift its other cursors by it.
    subroutine delete_word_backward(cursor, buffer, del_sl, del_sc, del_el, del_ec)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(out), optional :: del_sl, del_sc, del_el, del_ec
        character(len=:), allocatable :: line
        integer :: start_col, end_col, line_len
        integer :: byte_cursor, from_line, from_col
        logical :: in_word

        ! An empty range means "nothing was deleted"
        if (present(del_sl)) del_sl = cursor%line
        if (present(del_sc)) del_sc = cursor%column
        if (present(del_el)) del_el = cursor%line
        if (present(del_ec)) del_ec = cursor%column

        if (cursor%column <= 1) then
            if (cursor%line <= 1) return
            from_line = cursor%line
            call join_line_with_previous(cursor, buffer)
            buffer%modified = .true.
            ! The newline between (cursor%line, cursor%column) and the start
            ! of the line that followed it
            if (present(del_sl)) del_sl = cursor%line
            if (present(del_sc)) del_sc = cursor%column
            if (present(del_el)) del_el = from_line
            if (present(del_ec)) del_ec = 1
            return
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)

        byte_cursor = utf8_char_to_byte_index(line, &
            cursor%column)
        if (byte_cursor == 0) byte_cursor = line_len + 1
        end_col = byte_cursor - 1
        start_col = end_col

        do while (start_col > 0 .and. &
                  start_col <= line_len)
            if (line(start_col:start_col) == ' ') then
                start_col = start_col - 1
            else
                exit
            end if
        end do

        if (start_col > 0 .and. &
            start_col <= line_len) then
            in_word = is_word_char( &
                line(start_col:start_col))
            do while (start_col > 1)
                if (is_word_char( &
                    line(start_col-1:start_col-1)) &
                    .eqv. in_word) then
                    start_col = start_col - 1
                else
                    exit
                end if
            end do
        end if

        if (start_col < 1) start_col = 1

        if (start_col <= end_col) then
            from_col = cursor%column
            call delete_range(buffer, &
                cursor%line, start_col, &
                cursor%line, end_col)
            cursor%column = &
                utf8_byte_to_char_index(line, &
                start_col)
            cursor%desired_column = cursor%column
            buffer%modified = .true.
            if (present(del_sl)) del_sl = cursor%line
            if (present(del_sc)) del_sc = cursor%column
            if (present(del_el)) del_el = cursor%line
            if (present(del_ec)) del_ec = from_col
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_word_backward

    ! ========================================================================
    ! Character Transpose Subroutine
    ! ========================================================================

    subroutine join_lines(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, next_line
        integer :: line_count, current_len, leading_spaces

        line_count = buffer_get_line_count(buffer)

        ! Can't join if we're on the last line
        if (cursor%line >= line_count) return

        ! Get the current line and next line
        current_line = buffer_get_line(buffer, cursor%line)
        next_line = buffer_get_line(buffer, cursor%line + 1)
        current_len = len(current_line)

        ! Count leading whitespace in next line
        leading_spaces = 0
        do while (leading_spaces < len(next_line) .and. &
                 (next_line(leading_spaces + 1:leading_spaces + 1) == ' ' .or. &
                  next_line(leading_spaces + 1:leading_spaces + 1) == char(9)))
            leading_spaces = leading_spaces + 1
        end do

        ! Delete the newline and leading whitespace from next line
        if (leading_spaces > 0) then
            call buffer_delete_range(buffer, cursor%line, current_len + 1, cursor%line + 1, leading_spaces + 1)
        else
            call buffer_delete_range(buffer, cursor%line, current_len + 1, cursor%line + 1, 1)
        end if

        ! If the next line had non-whitespace content, insert a space between the lines
        if (leading_spaces < len(next_line)) then
            ! Insert a space if current line doesn't end with space
            if (current_len > 0) then
                if (current_line(current_len:current_len) /= ' ') then
                    call buffer_insert_text_at(buffer, cursor%line, current_len + 1, ' ')
                end if
            end if
        end if

        if (allocated(current_line)) deallocate(current_line)
        if (allocated(next_line)) deallocate(next_line)
    end subroutine join_lines

    function get_line_start_pos(buffer, line_num) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer :: pos
        integer :: i, current_line

        pos = 1
        current_line = 1

        ! Find the start position of the given line
        do i = 1, buffer%size
            if (current_line == line_num) then
                return
            end if

            if (buffer_get_char_at(buffer, i) == char(10)) then  ! Newline
                current_line = current_line + 1
                pos = i + 1
            end if
        end do

        ! If line_num is beyond the last line
        if (current_line < line_num) then
            pos = buffer%size + 1
        end if
    end function get_line_start_pos

    subroutine delete_range(buffer, start_line, start_col, end_line, end_col)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: start_line, start_col, end_line, end_col
        integer :: pos

        ! For now, handle single-line deletions
        if (start_line == end_line) then
            ! Calculate buffer position
            pos = get_line_start_pos(buffer, start_line) + start_col - 1

            ! Move gap to deletion point
            call buffer_move_gap(buffer, pos)

            ! Extend gap to delete characters
            buffer%gap_end = buffer%gap_end + (end_col - start_col + 1)
        end if
    end subroutine delete_range

    ! UNUSED:     subroutine insert_char_at(buffer, line_num, col, ch)
    ! UNUSED:         type(buffer_t), intent(inout) :: buffer
    ! UNUSED:         integer, intent(in) :: line_num, col
    ! UNUSED:         character, intent(in) :: ch
    ! UNUSED:         integer :: pos
    ! UNUSED: 
    ! UNUSED:         ! Calculate buffer position
    ! UNUSED:         pos = get_line_start_pos(buffer, line_num) + col - 1
    ! UNUSED: 
    ! UNUSED:         ! Move gap to insertion point
    ! UNUSED:         call buffer_move_gap(buffer, pos)
    ! UNUSED: 
    ! UNUSED:         ! Insert character
    ! UNUSED:         buffer%data(buffer%gap_start:buffer%gap_start) = ch
    ! UNUSED:         buffer%gap_start = buffer%gap_start + 1
    ! UNUSED:     end subroutine insert_char_at

    ! Handle input when in fuss mode
    subroutine handle_fuss_input(key_str, editor, buffer)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: selected_path
        type(tree_node_t), pointer :: sel_node
        integer :: i

        select case(trim(key_str))
        case('down')
            ! Move down in tree (arrows only; j/k are fuzzy-search)
            call tree_move_down(tree_state)
            ! Update viewport to keep selection visible (estimate ~18 visible lines)
            call update_tree_viewport(tree_state, 18)

        case('up')
            ! Move up in tree (arrows only; j/k are fuzzy-search)
            call tree_move_up(tree_state)
            ! Update viewport to keep selection visible (estimate ~18 visible lines)
            call update_tree_viewport(tree_state, 18)

        case('left')
            ! Move up to parent directory
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (associated(tree_state%selectable_files(tree_state%selected_index)%node)) then
                    if (associated(tree_state%selectable_files(tree_state%selected_index)%node%parent)) then
                        ! Find the parent in the selectable list
                        do i = 1, tree_state%n_selectable
                            if (associated(tree_state%selectable_files(i)%node, &
                                         tree_state%selectable_files(tree_state%selected_index)%node%parent)) then
                                tree_state%selected_index = i
                                exit
                            end if
                        end do
                    end if
                end if
            end if

        case('right')
            ! Move into first child of directory (and expand if needed)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (tree_state%selectable_files(tree_state%selected_index)%is_directory .and. &
                    associated(tree_state%selectable_files(tree_state%selected_index)%node)) then
                    sel_node => tree_state%selectable_files(tree_state%selected_index)%node
                    ! Expand if collapsed (scans lazily on first expand,
                    ! rebuilds selectable list, keeps selection on sel_node)
                    if (.not. sel_node%expanded) then
                        call tree_expand_node(tree_state, sel_node)
                    end if
                    ! Find first child in selectable list (look for item whose parent is current node)
                    do i = tree_state%selected_index + 1, tree_state%n_selectable
                        if (associated(tree_state%selectable_files(i)%node)) then
                            if (associated(tree_state%selectable_files(i)%node%parent, sel_node)) then
                                tree_state%selected_index = i
                                exit
                            end if
                        end if
                    end do
                end if
            end if

        case(' ', 'space')
            ! Toggle directory expand/collapse (scans lazily on first
            ! expand, rebuilds selectable list, restores selection)
            call tree_toggle_expand(tree_state)

        case('ctrl-g')
            ! Activate git prefix mode (Ctrl+g then a/u/m/p/f/l/t/d)
            fuss_git_prefix_active = .true.

        case('a')
            ! Stage file (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call tree_stage_file(tree_state, editor%workspace_path)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('u')
            ! Unstage file (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call tree_unstage_file(tree_state, editor%workspace_path)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('m')
            ! Git commit with message (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_commit(editor)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('p')
            ! Git push (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_push(editor)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('f')
            ! Git fetch (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_fetch(editor)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('l')
            ! Git pull (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_pull(editor)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('t')
            ! Git tag (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_tag(editor)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('d')
            ! Git diff (only with Ctrl+g prefix)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
                if (allocated(editor%workspace_path)) then
                    call handle_git_diff(editor, buffer)
                end if
            else
                call handle_fuss_fuzzy_search(key_str)
            end if

        case('enter')
            ! Open file in editor (only for files, not directories)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                selected_path = get_selected_item_path(tree_state)
                if (.not. tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                    if (len_trim(selected_path) > 0) then
                        call open_file_in_editor(selected_path, editor, buffer)
                    end if
                else if (len_trim(selected_path) > 0) then
                    ! Enter on a directory used to do nothing at all -- the
                    ! guard above had no else. It opens the group dialog now,
                    ! pre-filled with that directory.
                    !
                    ! A single CLICK on a directory still just expands it (the
                    ! region router sends 'space'). A click opening a modal
                    ! would be hostile; Enter is a deliberate act.
                    ! Absolute, not workspace-relative: the picker reads the
                    ! directory itself, and resolving a relative path would
                    ! depend on the working directory happening to be the
                    ! workspace root.
                    block
                        character(len=:), allocatable :: abs_dir

                        abs_dir = selected_path
                        if (allocated(editor%workspace_path)) then
                            if (len_trim(selected_path) > 0) then
                                if (selected_path(1:1) /= '/') &
                                    abs_dir = trim(editor%workspace_path) // &
                                              '/' // trim(selected_path)
                            end if
                        end if
                        if (group_picker_show(abs_dir, editor%screen_rows, &
                                              editor%screen_cols)) then
                            g_editing_gid = 0
                            editor%fuss_mode_active = .false.
                            g_lsp_ui_changed = .true.
                        else
                            ! Say so rather than appearing to ignore the key.
                            call set_status_message( &
                                'Cannot read ' // trim(selected_path))
                        end if
                    end block
                end if
            end if

        case('v')
            ! Fuzzy search (vsplit moved to alt-v)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
            end if
            call handle_fuss_fuzzy_search(key_str)

        case('s')
            ! Fuzzy search (hsplit moved to alt-s)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
            end if
            call handle_fuss_fuzzy_search(key_str)

        case('alt-v')
            ! Open file in vertical split (direct shortcut)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (.not. tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                    selected_path = get_selected_item_path(tree_state)
                    if (len_trim(selected_path) > 0) then
                        call open_file_in_vertical_split(selected_path, editor, buffer)
                    end if
                end if
            end if

        case('alt-s')
            ! Open file in horizontal split (direct shortcut)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (.not. tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                    selected_path = get_selected_item_path(tree_state)
                    if (len_trim(selected_path) > 0) then
                        call open_file_in_horizontal_split(selected_path, editor, buffer)
                    end if
                end if
            end if

        case('.')
            ! Toggle hiding dotfiles/gitignored files
            tree_state%hide_dotfiles = .not. tree_state%hide_dotfiles
            ! Rebuild selectable list to match visible items
            if (allocated(tree_state%selectable_files)) deallocate(tree_state%selectable_files)
            call build_selectable_list(tree_state%root, tree_state%selectable_files, &
                tree_state%n_selectable, tree_state%hide_dotfiles)
            if (tree_state%selected_index > tree_state%n_selectable .and. tree_state%n_selectable > 0) then
                tree_state%selected_index = tree_state%n_selectable
            end if

        case('ctrl-/')
            ! Toggle fuss mode hints expansion
            editor%fuss_hints_expanded = .not. editor%fuss_hints_expanded

        case('esc')
            ! Exit fuss mode (or cancel git prefix mode)
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
            else
                editor%fuss_mode_active = .false.
                editor%fuss_hints_expanded = .false.  ! Reset to collapsed
                fuss_git_prefix_active = .false.
                call fuss_reset_search()
                ! Tree kept: see open_fuss_tree.
            end if

        case default
            ! Reset git prefix mode if invalid key in prefix mode
            if (fuss_git_prefix_active) then
                fuss_git_prefix_active = .false.
            end if
            ! Fuzzy search: accumulate typed characters and jump to match
            ! Only handle single printable characters (letters, digits)
            if (len_trim(key_str) == 1) then
                call handle_fuss_fuzzy_search(key_str)
            end if

        end select
    end subroutine handle_fuss_input

    ! Handle fuzzy search character input in fuss mode
    subroutine handle_fuss_fuzzy_search(key_str)
        use iso_fortran_env, only: int64
        character(len=*), intent(in) :: key_str
        integer(int64) :: current_time, elapsed
        character(len=1) :: ch
        logical :: found

        ch = key_str(1:1)

        ! Only accept printable characters (letters, digits, some punctuation)
        if (ichar(ch) < 32 .or. ichar(ch) > 126) return

        ! Get current time
        current_time = get_time_ms()

        ! Check for timeout (500ms) - reset if too long since last keystroke
        if (fuss_search_last_time > 0) then
            elapsed = current_time - fuss_search_last_time
            if (elapsed > 500) then
                ! Timeout - reset search buffer
                fuss_search_buffer = ''
                fuss_search_len = 0
            end if
        end if

        ! Add character to search buffer (if there's room)
        if (fuss_search_len < 64) then
            fuss_search_len = fuss_search_len + 1
            fuss_search_buffer(fuss_search_len:fuss_search_len) = ch
        end if

        ! Update timestamp
        fuss_search_last_time = current_time

        ! Try to jump to a match
        found = fuss_fuzzy_jump(fuss_search_buffer(1:fuss_search_len))

        ! Update viewport to keep selection visible
        if (found) then
            call update_tree_viewport(tree_state, 18)
        end if
    end subroutine handle_fuss_fuzzy_search

    ! Open a file in the editor
    subroutine open_file_in_editor(file_path, editor, buffer)
        use editor_state_module, only: create_tab
        use text_buffer_module, only: copy_buffer
        character(len=*), intent(in) :: file_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: full_path
        integer :: status
        logical :: tab_created

        ! Build full path (skip workspace prefix if already absolute)
        if (len_trim(file_path) > 0 .and. file_path(1:1) == '/') then
            full_path = trim(file_path)
        else if (allocated(editor%workspace_path)) then
            full_path = trim(editor%workspace_path) // '/' // trim(file_path)
        else
            full_path = trim(file_path)
        end if

        ! Create a new tab for this file.
        !
        ! The guard is the whole point: create_tab refuses at the tab cap, and
        ! the load below targets tabs(active_tab_index) -- which, if no tab was
        ! created, is the tab that was already active, still named after a
        ! different file. Loading into it puts this file's text under that
        ! file's name, and the next Ctrl-S writes it to that file's path.
        call create_tab(editor, full_path, tab_created)
        if (.not. tab_created) then
            call set_status_message('Too many tabs open: close one first')
            return
        end if

        ! Load file into the new tab's buffer
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            call buffer_load_file(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                editor%active_tab_index))%buffer, full_path, status)

            ! Handle binary files
            if (status == -2) then
                ! Binary file detected - prompt user
                if (binary_file_prompt(full_path)) then
                    ! User wants to view in hex mode
                    call buffer_load_file_as_hex(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                        editor%active_tab_index))%buffer, full_path, status)
                    if (status /= 0) then
                        ! Failed to load hex view - close the tab and return
                        call close_tab(editor, editor%active_tab_index)
                        return
                    end if
                    ! Mark as hex view in filename
                    full_path = trim(full_path) // ' [HEX]'
                else
                    ! User cancelled - close the tab and return
                    call close_tab(editor, editor%active_tab_index)
                    return
                end if
            end if

            if (status == 0) then
                ! Copy tab's buffer to pane's buffer
                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                    editor%tabs(editor%active_tab_index)%active_pane_index > 0) then
                    call copy_buffer(editor%tabs(editor%active_tab_index)%panes( &
                        editor%tabs(editor%active_tab_index)%active_pane_index)%buffer, &
                        editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, editor%active_tab_index))%buffer)
                end if

                ! Copy tab's buffer to main buffer so it's displayed
                call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                    editor%active_tab_index))%buffer)

                ! Update editor state with the new tab's info
                if (allocated(editor%filename)) deallocate(editor%filename)
                allocate(character(len=len_trim(full_path)) :: editor%filename)
                editor%filename = full_path

                ! Send LSP didOpen notification to ALL active servers
                if (editor%tabs(editor%active_tab_index)%num_lsp_servers > 0) then
                    block
                        integer :: srv_i
                        do srv_i = 1, editor%tabs(editor%active_tab_index)%num_lsp_servers
                            call notify_file_opened(editor%lsp_manager, &
                                editor%tabs(editor%active_tab_index)%lsp_server_indices(srv_i), &
                                full_path, buffer_to_string(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                                    editor%active_tab_index))%buffer))
                        end do
                    end block
                end if

                ! Reset cursor to top of file
                editor%cursors(editor%active_cursor)%line = 1
                editor%cursors(editor%active_cursor)%column = 1
                editor%cursors(editor%active_cursor)%desired_column = 1
                editor%viewport_line = 1
                editor%viewport_column = 1

                ! Also update tab's active pane state
                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                    editor%tabs(editor%active_tab_index)%active_pane_index > 0) then
                    associate (pane => &
                        editor%tabs(editor%active_tab_index)%panes( &
                        editor%tabs(editor%active_tab_index)%active_pane_index))
                        if (allocated(pane%cursors) .and. size(pane%cursors) > 0) then
                            pane%cursors(1)%line = 1
                            pane%cursors(1)%column = 1
                            pane%cursors(1)%desired_column = 1
                        end if
                        pane%viewport_line = 1
                        pane%viewport_column = 1
                    end associate
                end if
            end if
        end if
        ! Note: fuss mode stays active - user must press ctrl-b to exit
    end subroutine open_file_in_editor

    ! Open a file in a vertical split
    subroutine open_file_in_vertical_split(file_path, editor, buffer)
        use editor_state_module, only: split_pane_vertical, sync_editor_to_pane
        use text_buffer_module, only: copy_buffer
        character(len=*), intent(in) :: file_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: full_path
        integer :: status, tab_idx, pane_idx

        ! Build full path (skip workspace prefix if already absolute).
        !
        ! The absolute case is not hypothetical: an orphan file outside the
        ! workspace carries an absolute path, and so does a tab dropped into
        ! the document to make a split. Prepending the workspace to one gave
        ! /workspace//home/... and quietly opened nothing.
        if (len_trim(file_path) > 0 .and. file_path(1:1) == '/') then
            full_path = trim(file_path)
        else if (allocated(editor%workspace_path)) then
            full_path = trim(editor%workspace_path) // '/' // trim(file_path)
        else
            full_path = trim(file_path)
        end if

        ! Exit fuss mode. The tree is kept -- opening a file from it is the
        ! strongest possible signal that its directory is worth showing again.
        editor%fuss_mode_active = .false.
        editor%fuss_hints_expanded = .false.

        ! If no tabs exist, create one first
        if (size(editor%tabs) == 0 .or. editor%active_tab_index == 0) then
            call open_file_in_editor(file_path, editor, buffer)
            return
        end if

        ! Split the current pane vertically
        call split_pane_vertical(editor)

        ! Get the new pane index (it becomes the active pane)
        tab_idx = editor%active_tab_index
        if (tab_idx > 0 .and. tab_idx <= size(editor%tabs)) then
            pane_idx = editor%tabs(tab_idx)%active_pane_index

            ! Load the file into the new pane's buffer
            if (allocated(editor%tabs(tab_idx)%panes) .and. pane_idx > 0) then
                call buffer_load_file(editor%tabs(tab_idx)%panes(pane_idx)%buffer, full_path, status)

                ! Handle binary files
                if (status == -2) then
                    ! Binary file detected - prompt user
                    if (binary_file_prompt(full_path)) then
                        ! User wants to view in hex mode
                        call buffer_load_file_as_hex(editor%tabs(tab_idx)%panes(pane_idx)%buffer, full_path, status)
                        if (status /= 0) then
                            ! Failed to load hex view - close the pane and return
                            call close_pane(editor)
                            return
                        end if
                        ! Mark as hex view in filename
                        full_path = trim(full_path) // ' [HEX]'
                    else
                        ! User cancelled - close the pane and return
                        call close_pane(editor)
                        return
                    end if
                end if

                if (status == 0) then
                    ! Update filename for the pane
                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)) &
                        deallocate(editor%tabs(tab_idx)%panes(pane_idx)%filename)
                    allocate(character(len=len_trim(full_path)) :: editor%tabs(tab_idx)%panes(pane_idx)%filename)
                    editor%tabs(tab_idx)%panes(pane_idx)%filename = canonical_path(full_path)

                    ! Copy to main buffer
                    call copy_buffer(buffer, editor%tabs(tab_idx)%panes(pane_idx)%buffer)

                    ! Update editor filename
                    if (allocated(editor%filename)) deallocate(editor%filename)
                    allocate(character(len=len_trim(full_path)) :: editor%filename)
                    editor%filename = full_path

                    ! Reset cursor in the new pane
                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors) .and. &
                        size(editor%tabs(tab_idx)%panes(pane_idx)%cursors) > 0) then
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%line = 1
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%column = 1
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%desired_column = 1
                    end if
                    editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = 1
                    editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = 1

                    ! Sync editor state with the new pane
                    call sync_editor_to_pane(editor)
                end if
            end if
        end if
    end subroutine open_file_in_vertical_split

    ! Open a file in a horizontal split
    subroutine open_file_in_horizontal_split(file_path, editor, buffer)
        use editor_state_module, only: split_pane_horizontal, sync_editor_to_pane
        use text_buffer_module, only: copy_buffer
        character(len=*), intent(in) :: file_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: full_path
        integer :: status, tab_idx, pane_idx

        ! Build full path (skip workspace prefix if already absolute).
        !
        ! The absolute case is not hypothetical: an orphan file outside the
        ! workspace carries an absolute path, and so does a tab dropped into
        ! the document to make a split. Prepending the workspace to one gave
        ! /workspace//home/... and quietly opened nothing.
        if (len_trim(file_path) > 0 .and. file_path(1:1) == '/') then
            full_path = trim(file_path)
        else if (allocated(editor%workspace_path)) then
            full_path = trim(editor%workspace_path) // '/' // trim(file_path)
        else
            full_path = trim(file_path)
        end if

        ! Exit fuss mode. The tree is kept -- opening a file from it is the
        ! strongest possible signal that its directory is worth showing again.
        editor%fuss_mode_active = .false.
        editor%fuss_hints_expanded = .false.

        ! If no tabs exist, create one first
        if (size(editor%tabs) == 0 .or. editor%active_tab_index == 0) then
            call open_file_in_editor(file_path, editor, buffer)
            return
        end if

        ! Split the current pane horizontally
        call split_pane_horizontal(editor)

        ! Get the new pane index (it becomes the active pane)
        tab_idx = editor%active_tab_index
        if (tab_idx > 0 .and. tab_idx <= size(editor%tabs)) then
            pane_idx = editor%tabs(tab_idx)%active_pane_index

            ! Load the file into the new pane's buffer
            if (allocated(editor%tabs(tab_idx)%panes) .and. pane_idx > 0) then
                call buffer_load_file(editor%tabs(tab_idx)%panes(pane_idx)%buffer, full_path, status)

                ! Handle binary files
                if (status == -2) then
                    ! Binary file detected - prompt user
                    if (binary_file_prompt(full_path)) then
                        ! User wants to view in hex mode
                        call buffer_load_file_as_hex(editor%tabs(tab_idx)%panes(pane_idx)%buffer, full_path, status)
                        if (status /= 0) then
                            ! Failed to load hex view - close the pane and return
                            call close_pane(editor)
                            return
                        end if
                        ! Mark as hex view in filename
                        full_path = trim(full_path) // ' [HEX]'
                    else
                        ! User cancelled - close the pane and return
                        call close_pane(editor)
                        return
                    end if
                end if

                if (status == 0) then
                    ! Update filename for the pane
                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)) &
                        deallocate(editor%tabs(tab_idx)%panes(pane_idx)%filename)
                    allocate(character(len=len_trim(full_path)) :: editor%tabs(tab_idx)%panes(pane_idx)%filename)
                    editor%tabs(tab_idx)%panes(pane_idx)%filename = canonical_path(full_path)

                    ! Copy to main buffer
                    call copy_buffer(buffer, editor%tabs(tab_idx)%panes(pane_idx)%buffer)

                    ! Update editor filename
                    if (allocated(editor%filename)) deallocate(editor%filename)
                    allocate(character(len=len_trim(full_path)) :: editor%filename)
                    editor%filename = full_path

                    ! Reset cursor in the new pane
                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors) .and. &
                        size(editor%tabs(tab_idx)%panes(pane_idx)%cursors) > 0) then
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%line = 1
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%column = 1
                        editor%tabs(tab_idx)%panes(pane_idx)%cursors(1)%desired_column = 1
                    end if
                    editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = 1
                    editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = 1

                    ! Sync editor state with the new pane
                    call sync_editor_to_pane(editor)
                end if
            end if
        end if
    end subroutine open_file_in_horizontal_split

    ! Toggle fuss mode (file tree)
    !> Show the tree, building it only if there is not one already.
    !>
    !> A refresh re-reads the directories, so files that appeared or vanished
    !> while the panel was closed still show up -- it just no longer forgets
    !> which directories were open, or whether hidden ones were being shown.
    subroutine open_fuss_tree(editor)
        type(editor_state_t), intent(inout) :: editor

        if (associated(tree_state%root) .and. &
            trim(tree_state%workspace_path) == trim(editor%workspace_path)) then
            call refresh_tree_state(tree_state, editor%workspace_path)
        else
            ! First time here, or the workspace changed underneath us.
            call cleanup_tree_state(tree_state)
            call init_tree_state(tree_state, editor%workspace_path)
        end if

        call reveal_open_files(editor)
    end subroutine open_fuss_tree

    !> Open the folders holding the files that are open.
    !>
    !> The same rule the tree already applies to git's changes: what you are
    !> working on is worth showing, whatever its directory is called. It is
    !> also why no list of open directories needs storing anywhere -- the tabs
    !> are already persisted, so after a restart the folders holding them come
    !> back on their own rather than from a second record that could disagree.
    subroutine reveal_open_files(editor)
        type(editor_state_t), intent(inout) :: editor
        character(len=:), allocatable :: root, rel
        integer :: i, n

        if (.not. allocated(editor%workspace_path)) return
        if (.not. allocated(editor%tabs)) return

        root = trim(editor%workspace_path)
        n = len(root)

        do i = 1, size(editor%tabs)
            if (.not. allocated(editor%tabs(i)%filename)) cycle
            rel = trim(editor%tabs(i)%filename)
            if (len(rel) == 0) cycle
            ! Tabs may hold an absolute path or one already relative to the
            ! workspace; the tree only speaks the latter.
            if (len(rel) > n + 1) then
                if (rel(1:n) == root .and. rel(n+1:n+1) == '/') rel = rel(n+2:)
            end if
            if (index(rel, '/') <= 0) cycle     ! at the root; nothing to open
            call tree_reveal_path(tree_state, rel, force=.true.)
        end do
    end subroutine reveal_open_files

    subroutine toggle_fuss_mode(editor)
        type(editor_state_t), intent(inout) :: editor

        editor%fuss_mode_active = .not. editor%fuss_mode_active

        if (editor%fuss_mode_active) then
            if (allocated(editor%workspace_path)) call open_fuss_tree(editor)
        end if
        ! Leaving fuss mode deliberately keeps the tree. It used to be freed
        ! here and rebuilt from nothing on the way back in -- init_tree_state is
        ! intent(out), so that reset the dotfile toggle as well as every open
        ! directory. Closing the panel is not a reason to forget where the user
        ! had got to; the tree is released when the workspace changes or the
        ! editor exits.
    end subroutine toggle_fuss_mode

    ! UNUSED: Toggle diagnostics panel
    ! Kept for potential future use
    ! subroutine toggle_diagnostics_panel(editor)
    !     type(editor_state_t), intent(inout) :: editor
    !     call toggle_panel(editor%diagnostics_panel)
    ! end subroutine toggle_diagnostics_panel

    ! Handle git commit with message prompt
    subroutine handle_git_commit(editor)
        type(editor_state_t), intent(inout) :: editor
        character(len=512) :: commit_message
        logical :: cancelled, success

        ! Show prompt for commit message
        call show_text_prompt('Commit message (ESC to cancel): ', commit_message, cancelled, editor%screen_rows)

        if (.not. cancelled .and. len_trim(commit_message) > 0) then
            call git_commit(editor%workspace_path, commit_message, success)

            ! Show feedback message
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(repeat(' ', 200))
            call terminal_move_cursor(editor%screen_rows, 1)
            if (success) then
                call terminal_write(char(27) // '[32m✓ Committed successfully!' // char(27) // '[0m')
            else
                call terminal_write(char(27) // '[31m✗ Commit failed (nothing staged?)' // char(27) // '[0m')
            end if

            ! Brief pause
            call execute_command_line('sleep 1')

            ! Refresh tree
            call refresh_tree_state(tree_state, editor%workspace_path)
        end if
    end subroutine handle_git_commit

    ! Handle git push
    subroutine handle_git_push(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Pushing to remote...')

        call git_push(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Pushed successfully!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Push failed (check remote/branch)' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_push

    ! Handle git fetch
    subroutine handle_git_fetch(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Fetching from remote...')

        call git_fetch(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Fetch completed!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Fetch failed!' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_fetch

    ! Handle git pull
    subroutine handle_git_pull(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Pulling from remote...')

        call git_pull(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Pull completed!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Pull failed!' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_pull

    ! Handle git tag
    subroutine handle_git_tag(editor)
        use help_display_module, only: display_tags_header
        type(editor_state_t), intent(inout) :: editor
        character(len=256) :: tag_name, tag_message
        character(len=256), allocatable :: existing_tags(:)
        integer :: n_tags
        logical :: cancelled, success, push_tag

        ! Fetch and display existing tags (keeps them visible during prompts)
        call git_list_tags(editor%workspace_path, existing_tags, n_tags)
        call display_tags_header(editor, existing_tags, n_tags)

        ! Show prompt for tag name (tags remain visible above)
        call show_text_prompt('Tag name (ESC to cancel): ', tag_name, cancelled, editor%screen_rows)

        if (allocated(existing_tags)) deallocate(existing_tags)

        if (.not. cancelled .and. len_trim(tag_name) > 0) then
            ! Show prompt for tag message (optional)
            call show_text_prompt('Tag message (optional, ESC to skip): ', tag_message, cancelled, editor%screen_rows)

            if (.not. cancelled) then
                call git_tag(editor%workspace_path, tag_name, tag_message, success)

                ! Show result
                call terminal_move_cursor(editor%screen_rows, 1)
                call terminal_write(repeat(' ', 200))
                call terminal_move_cursor(editor%screen_rows, 1)
                if (success) then
                    call terminal_write(char(27) // '[32m✓ Tag created: ' // trim(tag_name) // char(27) // '[0m')

                    ! Brief pause
                    call execute_command_line('sleep 1')

                    ! Ask if user wants to push the tag to origin (auto-submit on y/n)
                    call show_yes_no_prompt('Push tag to origin? (y/n, ESC to skip): ', push_tag, cancelled, editor%screen_rows)

                    if (.not. cancelled .and. push_tag) then
                        call git_push_tag(editor%workspace_path, tag_name, success)

                        ! Show push result
                        call terminal_move_cursor(editor%screen_rows, 1)
                        call terminal_write(repeat(' ', 200))
                        call terminal_move_cursor(editor%screen_rows, 1)
                        if (success) then
                            call terminal_write(char(27) // '[32m✓ Tag pushed to origin' // char(27) // '[0m')
                        else
                            call terminal_write(char(27) // '[31m✗ Failed to push tag (check remote)' // char(27) // '[0m')
                        end if

                        call execute_command_line('sleep 1')
                    end if
                else
                    call terminal_write(char(27) // '[31m✗ Failed to create tag' // char(27) // '[0m')
                    call execute_command_line('sleep 1')
                end if

                ! Refresh tree
                call refresh_tree_state(tree_state, editor%workspace_path)
            end if
        end if
    end subroutine handle_git_tag

    subroutine handle_git_diff(editor, buffer)
        use editor_state_module, only: create_tab
        use text_buffer_module, only: buffer_insert, copy_buffer
        use file_tree_module, only: get_selected_item_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: selected_path, diff_content, tab_name
        character(len=256) :: branch_name
        logical :: success

        ! Get selected file from tree
        if (tree_state%selected_index < 1 .or. tree_state%selected_index > tree_state%n_selectable) return
        if (tree_state%selectable_files(tree_state%selected_index)%is_directory) return

        selected_path = get_selected_item_path(tree_state)
        if (len_trim(selected_path) == 0) return

        ! Get diff content
        call git_diff_file(editor%workspace_path, selected_path, diff_content, branch_name, success)

        if (.not. success) then
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(repeat(' ', 200))
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(char(27) // '[31m✗ Failed to get diff' // char(27) // '[0m')
            call execute_command_line('sleep 1')
            return
        end if

        ! Create tab name: diff:<filename>:<branch>
        if (len_trim(branch_name) > 0) then
            tab_name = 'diff:' // trim(selected_path) // ':' // trim(branch_name)
        else
            tab_name = 'diff:' // trim(selected_path)
        end if

        ! Refuse before creating, not after: the load below targets
        ! tabs(active_tab_index), which without a new tab is the tab already
        ! active under a different name.
        if (.not. can_create_tab(editor)) then
            call set_status_message('Too many tabs open: close one first')
            return
        end if
        call create_tab(editor, tab_name)

        ! Load diff content into the tab's buffer
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            ! Insert diff content at the beginning of the buffer
            call buffer_insert(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                editor%active_tab_index))%buffer, 1, diff_content)

            ! Copy tab's buffer to main buffer so it's displayed
            call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                editor%active_tab_index))%buffer)

            ! Update editor state with the new tab's info
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len_trim(tab_name)) :: editor%filename)
            editor%filename = tab_name

            ! Reset cursor to top of file
            editor%cursors(editor%active_cursor)%line = 1
            editor%cursors(editor%active_cursor)%column = 1
            editor%cursors(editor%active_cursor)%desired_column = 1

            ! Exit fuss mode and show diff
            editor%fuss_mode_active = .false.
        end if
    end subroutine handle_git_diff

    subroutine handle_fortress_navigator(editor, buffer)
        use workspace_module, only: workspace_is_file_in_workspace, workspace_switch
        use save_prompt_module, only: save_prompt, save_prompt_result_t
        use input_handler_module, only: get_key_input
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: selected_path
        character(len=32) :: key_input
        logical :: is_directory, cancelled, is_in_workspace, switch_success
        logical :: should_switch
        integer :: load_status, tab_idx, status

        ! Call fortress navigator (start in workspace if available)
        if (allocated(editor%workspace_path)) then
            call open_fortress_navigator(selected_path, is_directory, cancelled, editor%workspace_path)
        else
            call open_fortress_navigator(selected_path, is_directory, cancelled)
        end if

        ! If user selected something, open it
        if (.not. cancelled .and. allocated(selected_path)) then
            if (len_trim(selected_path) > 0) then
                if (.not. is_directory) then
                    ! Selected a file - create a new tab for it
                    ! Check if file is within workspace
                    if (allocated(editor%workspace_path)) then
                        is_in_workspace = workspace_is_file_in_workspace(selected_path, editor%workspace_path)
                    else
                        is_in_workspace = .false.
                    end if

                    ! Create new tab
                    if (.not. can_create_tab(editor)) then
                        call set_status_message('Too many tabs open: close one first')
                        return
                    end if
                    call create_tab(editor, trim(selected_path))
                    tab_idx = editor%active_tab_index

                    ! Mark as orphan if outside workspace
                    if (allocated(editor%tabs) .and. tab_idx > 0 .and. tab_idx <= size(editor%tabs)) then
                        editor%tabs(tab_idx)%is_orphan = .not. is_in_workspace

                        ! Read once, into the pane that owns the file.
                        block
                            integer :: p0
                            p0 = active_pane_of(editor, tab_idx)
                            call buffer_load_file( &
                                editor%tabs(tab_idx)%panes(p0)%buffer, &
                                selected_path, load_status)
                            call copy_buffer(buffer, &
                                editor%tabs(tab_idx)%panes(p0)%buffer)
                        end block

                        ! Update editor filename
                        if (allocated(editor%filename)) deallocate(editor%filename)
                        allocate(character(len=len_trim(selected_path)) :: editor%filename)
                        editor%filename = selected_path

                        ! Reset cursor to top
                        editor%cursors(editor%active_cursor)%line = 1
                        editor%cursors(editor%active_cursor)%column = 1
                        editor%cursors(editor%active_cursor)%desired_column = 1
                    end if
                else
                    ! Selected a directory - switch workspace (Phase 6)
                    should_switch = .true.

                    ! Check for dirty buffers and prompt to save
                    if (allocated(editor%tabs)) then
                        call handle_dirty_buffers_before_switch(editor, should_switch)
                    end if

                    ! If user didn't cancel, perform the switch
                    if (should_switch) then
                        call workspace_switch(editor, selected_path, switch_success)

                        if (.not. switch_success) then
                            ! Show error message
                            call terminal_move_cursor(1, 1)
                            call terminal_write("Error: Could not switch to workspace: " // trim(selected_path))
                            call terminal_write("Press any key to continue...")
                            call terminal_flush()
                            call get_key_input(key_input, status)
                        else
                            ! Sync the main buffer to the restored
                            ! workspace tabs, or create a blank
                            ! Untitled tab for a new/empty workspace.
                            if (allocated(editor%tabs) .and. &
                                editor%active_tab_index > 0 .and. &
                                editor%active_tab_index <= size(editor%tabs)) then
                                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                                    size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                                    call copy_buffer(buffer, &
                                        editor%tabs(editor%active_tab_index)%panes(1)%buffer)
                                else
                                    call copy_buffer(buffer, &
                                        editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                                            editor%active_tab_index))%buffer)
                                end if
                                if (allocated(editor%tabs(editor%active_tab_index)%filename)) then
                                    if (allocated(editor%filename)) deallocate(editor%filename)
                                    editor%filename = editor%tabs(editor%active_tab_index)%filename
                                end if
                            else
                                ! No restored tabs — create blank Untitled
                                call init_buffer(buffer)
                                call create_tab(editor, '[Untitled]')
                                buffer%modified = .false.
                                if (allocated(editor%filename)) deallocate(editor%filename)
                                editor%filename = '[Untitled]'
                            end if

                            ! Update file tree if active
                            if (editor%fuss_mode_active .and. allocated(editor%workspace_path)) then
                                call refresh_tree_state(tree_state, editor%workspace_path)
                            end if
                        end if
                    end if
                end if
            end if
        end if

        ! Re-render after returning from fortress
        call terminal_clear_screen()
    end subroutine handle_fortress_navigator

    !> Handle dirty buffers before workspace switch
    !> Write the working buffer to the ACTIVE PANE's file.
    !>
    !> The working buffer holds the active pane's text and may be newer than
    !> the pane's own copy, so it -- not the pane buffer -- is what must be
    !> written. The only question is which name to write it under, and the
    !> answer is the pane's, never the tab's.

    function int_to_text(v) result(t)
        integer, intent(in) :: v
        character(len=:), allocatable :: t
        character(len=16) :: b

        write(b, '(i0)') v
        t = trim(b)
    end function int_to_text



    !> Act on anything `fac` typed in the integrated terminal handed us.
    !>
    !> A file opens as a tab, exactly as opening it any other way would. A
    !> directory opens the group dialog, which is what a directory already
    !> means inside a session -- Enter on one in the tree does the same thing.
    !> It stays a whole new workspace only from a normal terminal, where there
    !> is no session to add it to.
    !>
    !> `changed` is set when the screen needs redrawing, so an idle session
    !> with a terminal open does not repaint on every tick.
    subroutine session_requests_tick(editor, buffer, changed)
        use session_ipc_module, only: session_ipc_take
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(inout) :: changed
        character(len=:), allocatable :: kind, path
        logical :: found

        call session_ipc_take(kind, path, found)
        if (.not. found) return

        ! Focus has to leave the panel, and this is the whole reason it is
        ! done here rather than left alone: the request arrives while the user
        ! is typing in the terminal, so the panel still owns the keyboard. A
        ! dialog opened underneath that gets no keys at all -- Esc reaches the
        ! shell instead, and the dialog sits there unreachable. Opening a file
        ! moves focus for the same reason it does in any other editor: you
        ! asked for the file because you want to be in it.
        !
        ! The panel stays VISIBLE. Only the keyboard moves.
        editor%terminal_panel%focused = .false.

        if (kind == 'dir') then
            if (group_picker_show(path, editor%screen_rows, &
                                  editor%screen_cols)) then
                g_editing_gid = 0
                ! Leave the terminal up. The dialog draws over it and the
                ! command that opened it is still worth seeing underneath.
                editor%fuss_mode_active = .false.
                changed = .true.
            else
                call set_status_message('Cannot read ' // path)
                changed = .true.
            end if
        else
            call open_file_in_editor(path, editor, buffer)
            changed = .true.
        end if
    end subroutine session_requests_tick

    !> Turn a confirmed dialog into a real group.
    !>
    !> Opens each ticked file that is not already open, reuses the tab when it
    !> is -- a tab belongs to exactly one group, so a file open elsewhere moves
    !> rather than being duplicated.
    subroutine finish_group_creation(editor, buffer)
        use editor_state_module, only: group_create, group_add_member, &
                                       find_tab_by_path_public, defer_tab
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32) :: gid
        integer :: i, n, tab_idx, first_tab
        character(len=:), allocatable :: path

        ! The same dialog answers for both modes, so which one is being
        ! finished is decided here and nowhere else.
        if (g_editing_gid /= 0) then
            call apply_group_edit(editor, buffer, g_editing_gid)
            g_editing_gid = 0
            return
        end if

        n = group_picker_count()
        if (n == 0) then
            call group_picker_hide()
            return
        end if

        call group_create(editor, group_picker_dir(), group_picker_name(), gid)
        first_tab = 0

        do i = 1, n
            path = group_picker_path(i)
            if (len_trim(path) == 0) cycle

            tab_idx = find_tab_by_path_public(editor, path)
            if (tab_idx == 0) then
                call open_file_in_editor(path, editor, buffer)
                tab_idx = editor%active_tab_index
            end if
            if (tab_idx >= 1 .and. tab_idx <= size(editor%tabs)) then
                call group_add_member(editor, gid, tab_idx)
                if (first_tab == 0) first_tab = tab_idx
            end if
        end do

        ! Defer every member except the one we are about to land on. A group
        ! of forty files then costs one file read rather than forty; the rest
        ! are read the first time they are looked at.
        !
        ! Safe because residency is structural: save_tab_pane refuses a tab
        ! whose buffer was never allocated, sync_buffer_to_all_instances skips
        ! it so it can never be marked modified, and switch_to_tab_with_buffer
        ! hydrates before anything reads the text.
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id /= gid) cycle
            if (i == first_tab) cycle
            if (editor%tabs(i)%modified) cycle       ! never discard real edits
            call defer_tab(editor, i)
        end do

        call group_picker_hide()
        if (first_tab > 0) call switch_to_tab_with_buffer(editor, first_tab, buffer)
        call set_status_message('Created ' // group_label_public(editor, gid))
    end subroutine finish_group_creation

    !> Make an existing group match what the dialog says it should be.
    !>
    !> Additions run BEFORE removals. Removing first can empty the group, and
    !> an empty group is pruned, which would take the id out from under the
    !> additions that were about to be made against it.
    !>
    !> Everything here works in PATHS, never in tab indices. Closing a tab
    !> compacts the tabs array and renumbers every index above it, so an index
    !> read before a close means something different after one -- which is
    !> exactly the shape of bug that would delete the wrong file.
    subroutine apply_group_edit(editor, buffer, gid)
        use editor_state_module, only: group_find, group_add_member, &
                                       group_members, group_member_count, &
                                       find_tab_by_path_public, defer_tab
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32), intent(in) :: gid
        ! Allocatable for the reason given in tab_menu_action: a fixed array
        ! this large is moved to static storage and silently shared between
        ! calls.
        character(len=512), allocatable :: want(:), drop(:), fresh(:)
        logical, allocatable :: drop_modified(:)
        integer :: n_want, n_drop, n_fresh
        integer :: i, gidx, tab_idx, added, removed, kept
        integer, allocatable :: idx(:)
        character(len=:), allocatable :: path, label

        call group_picker_hide()
        gidx = group_find(editor, gid)
        if (gidx == 0) return

        ! ---- what the dialog asked for.
        ! Bounds: the dialog cannot hand back more than it could tick, and no
        ! more tabs can be dropped than are open. Both +1 so a zero case
        ! still allocates something indexable.
        allocate(want(max(1, min(GP_EDIT_MAX, group_picker_count()))))
        allocate(fresh(size(want)))
        allocate(drop(max(1, size(editor%tabs))))
        allocate(drop_modified(size(drop)))

        n_want = 0
        do i = 1, group_picker_count()
            if (n_want >= size(want)) exit
            path = group_picker_path(i)
            if (len_trim(path) == 0) cycle
            n_want = n_want + 1
            want(n_want) = path
        end do

        ! ---- members it no longer names.
        ! [Untitled] members are skipped, not dropped: they have no path, so
        ! the dialog could not list them and their absence is not a decision.
        n_drop = 0
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id /= gid) cycle
            if (.not. allocated(editor%tabs(i)%filename)) cycle
            if (index(editor%tabs(i)%filename, '[Untitled') == 1) cycle
            if (in_list(want, n_want, editor%tabs(i)%filename)) cycle
            if (n_drop >= size(drop)) exit
            n_drop = n_drop + 1
            drop(n_drop) = editor%tabs(i)%filename
            ! Recorded NOW, before anything switches tabs.
            ! switch_to_tab_with_buffer writes editor%modified back over the
            ! outgoing tab's flag as it saves it, and editor%modified is not
            ! kept current while a modal owns the keyboard -- so switching to
            ! a tab to close it is what destroys the very fact we then want
            ! to branch on. Ctrl-W never noticed because it reads the flag
            ! and closes without switching anywhere first.
            drop_modified(n_drop) = editor%tabs(i)%modified
        end do

        ! ---- additions
        added = 0
        n_fresh = 0
        do i = 1, n_want
            tab_idx = find_tab_by_path_public(editor, trim(want(i)))
            if (tab_idx == 0) then
                call open_file_in_editor(trim(want(i)), editor, buffer)
                tab_idx = editor%active_tab_index
                if (n_fresh < size(fresh)) then
                    n_fresh = n_fresh + 1
                    fresh(n_fresh) = want(i)
                end if
            end if
            if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) cycle
            if (editor%tabs(tab_idx)%group_id == gid) cycle
            call group_add_member(editor, gid, tab_idx)
            added = added + 1
        end do

        ! ---- removals, one at a time, each re-found by path
        removed = 0
        kept = 0
        do i = 1, n_drop
            tab_idx = find_tab_by_path_public(editor, trim(drop(i)))
            if (tab_idx == 0) cycle
            call switch_to_tab_with_buffer(editor, tab_idx, buffer)
            ! Re-found: switching can hydrate and reorder nothing today, but
            ! the path is the identity here and the index is not.
            tab_idx = find_tab_by_path_public(editor, trim(drop(i)))
            if (tab_idx == 0) cycle
            if (drop_modified(i)) then
                ! Same rule ctrl-w applies. Switching first is not cosmetic:
                ! the prompt names a file and the user should be looking at
                ! the one it is asking about.
                call prompt_save_before_close_tab(editor, buffer)
            else
                call close_tab_without_prompt(editor, buffer)
            end if
            ! Cancelling the prompt leaves the tab alone, and the only honest
            ! way to know is to look. A tab that survived keeps its membership
            ! too, so the untick is abandoned rather than half-applied.
            if (find_tab_by_path_public(editor, trim(drop(i))) == 0) then
                removed = removed + 1
            else
                kept = kept + 1
            end if
        end do

        ! ---- the name
        label = group_picker_name()
        if (len_trim(label) > 0) then
            gidx = group_find(editor, gid)
            if (gidx >= 1) then
                if (allocated(editor%groups(gidx)%label)) &
                    deallocate(editor%groups(gidx)%label)
                editor%groups(gidx)%label = trim(label)
            end if
        end if

        ! ---- newly opened members cost one file read, not all of them.
        ! Only the ones opened just now: an existing member may be resident
        ! and modified, and deferring that would discard real edits.
        if (group_find(editor, gid) /= 0) then
            call group_members(editor, gid, idx)
            do i = 1, size(idx)
                if (idx(i) < 1 .or. idx(i) > size(editor%tabs)) cycle
                if (i == 1) cycle                     ! the one we land on
                if (editor%tabs(idx(i))%modified) cycle
                if (.not. allocated(editor%tabs(idx(i))%filename)) cycle
                if (.not. in_list(fresh, n_fresh, editor%tabs(idx(i))%filename)) cycle
                call defer_tab(editor, idx(i))
            end do
            if (size(idx) >= 1) then
                if (idx(1) >= 1 .and. idx(1) <= size(editor%tabs)) &
                    call switch_to_tab_with_buffer(editor, idx(1), buffer)
            end if
        end if

        path = 'Group updated: +' // trim(int_to_text(added)) // &
               ' -' // trim(int_to_text(removed))
        ! Only mentioned when it happened, and it only happens when someone
        ! cancelled a save prompt -- silence there would look like the untick
        ! was applied.
        if (kept > 0) path = path // '  (' // trim(int_to_text(kept)) // &
                                     ' kept, still unsaved)'
        call set_status_message(path)
        g_lsp_ui_changed = .true.
    end subroutine apply_group_edit

    logical function in_list(list, n, path)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: path
        integer :: k

        in_list = .false.
        do k = 1, n
            if (trim(list(k)) == trim(path)) then
                in_list = .true.
                return
            end if
        end do
    end function in_list

    function group_label_public(editor, gid) result(t)
        use editor_state_module, only: group_label
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        character(len=:), allocatable :: t
        t = group_label(editor, gid)
    end function group_label_public

    !> Move to the previous or next entry on row 1.
    !>
    !> An entry is a group or an ungrouped tab, so with no groups this is
    !> exactly the previous/next tab it always was. Wraps at both ends.
    !> The text a range currently covers, for checking an edit before writing.
    !>
    !> Returns '' for a multi-line or empty range. A zero-width range is worth
    !> noticing in its own right: a rename edit that deletes nothing is not
    !> replacing an identifier, it is inserting one somewhere, which is exactly
    !> the shape of the bug this guards.
    function text_in_range(buffer, sl, sc, el, ec) result(text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: sl, sc, el, ec
        character(len=:), allocatable :: text
        character(len=:), allocatable :: line
        integer :: b1, b2

        text = ''
        if (sl /= el) return
        if (ec <= sc) return

        line = buffer_get_line(buffer, sl)
        b1 = utf8_char_to_byte_index(line, sc)
        b2 = utf8_char_to_byte_index(line, ec)
        if (b1 < 1) return
        if (b2 < 1) b2 = len(line) + 1
        if (b2 - 1 > len(line)) b2 = len(line) + 1
        if (b2 <= b1) return
        text = line(b1:b2-1)
    end function text_in_range

    !> Push the active document to its language servers right now.
    !>
    !> The per-tab sync is debounced, which is right for keystrokes and wrong
    !> before any request whose answer is a set of positions: the server would
    !> be describing text the editor has already moved on from.
    subroutine flush_document_now(editor)
        use document_sync_module, only: flush_pending_changes
        type(editor_state_t), intent(inout) :: editor
        integer :: i

        if (.not. allocated(editor%tabs)) return
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%num_lsp_servers > 0) then
                call flush_pending_changes(editor%tabs(i)%document_sync, &
                                           editor%lsp_manager, .true.)
            end if
        end do
    end subroutine flush_document_now

    !> A cursor move without Shift ends the selection.
    !>
    !> Two things were wrong and they are the same thing. The selection stayed
    !> painted while the caret walked away from it, so it had to be dismissed
    !> with Esc; and there was no notion of collapsing to an end, so returning
    !> from a downward selection went one line up from the BOTTOM rather than
    !> back to where the selection started.
    !>
    !> Which end it collapses to is the direction of the key: left and up to
    !> the start, right and down to the end.
    !>
    !> `handled` is true whenever a selection was collapsed, so the ordinary
    !> move does not also run.
    subroutine collapse_selection_for_move(editor, key_str, handled)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: key_str
        logical, intent(out) :: handled
        integer(int32) :: s_line, s_col, e_line, e_col
        logical :: backward
        integer :: c

        handled = .false.
        if (.not. allocated(editor%cursors)) return
        ! Multiple cursors are left alone: their selections come from ctrl-d,
        ! where an arrow moving every head at once is the point, and Esc is
        ! already the way back to one cursor.
        if (size(editor%cursors) /= 1) return
        c = editor%active_cursor
        if (c < 1 .or. c > size(editor%cursors)) return
        if (.not. editor%cursors(c)%has_selection) return

        select case (trim(key_str))
        case ('left', 'up', 'home', 'pageup', 'ctrl-left', 'alt-left', 'ctrl-home')
            backward = .true.
        case ('right', 'down', 'end', 'pagedown', 'ctrl-right', 'alt-right', 'ctrl-end')
            backward = .false.
        case default
            return                      ! not a plain move; leave it alone
        end select

        ! Anchor and head in document order.
        s_line = editor%cursors(c)%selection_start_line
        s_col = editor%cursors(c)%selection_start_col
        e_line = editor%cursors(c)%line
        e_col = editor%cursors(c)%column
        if (s_line > e_line .or. (s_line == e_line .and. s_col > e_col)) then
            call swap_i32(s_line, e_line)
            call swap_i32(s_col, e_col)
        end if

        editor%cursors(c)%has_selection = .false.

        ! Collapse only -- never also move. The first press after selecting
        ! puts the caret on an end and stops there; the next one moves from it.
        ! Moving as well would mean Right after selecting rightwards skipped a
        ! character, and there would be no press that simply lands on the end.
        if (backward) then
            editor%cursors(c)%line = s_line
            editor%cursors(c)%column = s_col
        else
            editor%cursors(c)%line = e_line
            editor%cursors(c)%column = e_col
        end if
        editor%cursors(c)%desired_column = editor%cursors(c)%column
        handled = .true.
    end subroutine collapse_selection_for_move

    pure subroutine swap_i32(a, b)
        integer(int32), intent(inout) :: a, b
        integer(int32) :: t
        t = a; a = b; b = t
    end subroutine swap_i32

    !> Jump to tab `n`, then wait briefly to see whether more digits follow.
    subroutine begin_tab_jump(editor, buffer, n)
        use platform_module, only: platform_now_ms
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: n

        call clear_tab_jump()
        if (n < 1 .or. n > size(editor%tabs)) return

        call switch_to_tab_with_buffer(editor, n, buffer)
        g_jump_value = n
        g_jump_deadline = platform_now_ms() + int(JUMP_WINDOW_MS, int64)
        g_jump_group = editor%tabs(n)%group_id
        call announce_tab_jump(editor)
    end subroutine begin_tab_jump

    !> A key arrived while a jump was pending. True when it was consumed.
    function continue_tab_jump(editor, buffer, key_str) result(handled)
        use platform_module, only: platform_now_ms
        use editor_state_module, only: group_members
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: key_str
        logical :: handled
        integer, allocatable :: members(:)
        integer :: digit, target
        character(len=48) :: msg

        handled = .false.

        ! Expired, or not a digit at all: the jump that already happened stands
        ! and this key is whatever it is. Clearing before returning matters --
        ! otherwise a digit typed much later would be read as a continuation.
        if (platform_now_ms() > g_jump_deadline) then
            call clear_tab_jump()
            return
        end if
        digit = digit_of_key(key_str)
        if (digit < 0) then
            call clear_tab_jump()
            return
        end if

        handled = .true.

        if (g_jump_group /= 0) then
            ! Into the group the first digit landed in. Its members are
            ! numbered by their order in the group, not by their tab index --
            ! that order is what row 2 shows, so it is what the user is
            ! counting.
            call group_members(editor, g_jump_group, members)
            if (digit >= 1 .and. digit <= size(members)) then
                call switch_to_tab_with_buffer(editor, members(digit), buffer)
                call clear_tab_jump()
                call set_status_message('Group member ' // digit_str(digit))
            else
                write(msg, '(a,i0,a)') 'This group has ', size(members), ' members'
                call clear_tab_jump()
                call set_status_message(trim(msg))
            end if
            return
        end if

        target = g_jump_value * 10 + digit
        if (target < 1 .or. target > size(editor%tabs)) then
            ! Out of range. The earlier jump stands rather than being undone,
            ! and the digit is not typed into the document either -- it was
            ! meant as part of a chord, and inserting it would be a surprise
            ! edit to a file the user was only navigating.
            write(msg, '(a,i0,a)') 'No tab ', target, ''
            call clear_tab_jump()
            call set_status_message(trim(msg))
            return
        end if

        call switch_to_tab_with_buffer(editor, target, buffer)
        ! Re-arm, so three-digit tab numbers work on the way up too.
        g_jump_value = target
        g_jump_deadline = platform_now_ms() + int(JUMP_WINDOW_MS, int64)
        g_jump_group = editor%tabs(target)%group_id
        call announce_tab_jump(editor)
    end function continue_tab_jump

    !> The digit a key carries, or -1.
    !>
    !> A bare '5', and also alt-5 or ctrl-5. Holding the modifier down is the
    !> natural way to type a two-digit tab number -- alt-1 then 5 without
    !> letting go -- and only the bare form was accepted, so the second digit
    !> fell through to the main dispatch and was taken as its own jump. The
    !> sequence went to tab 1 and then to tab 5 rather than to tab 15.
    pure function digit_of_key(key_str) result(d)
        character(len=*), intent(in) :: key_str
        integer :: d
        character(len=:), allocatable :: k
        character :: c

        d = -1
        k = trim(key_str)
        if (len(k) == 1) then
            c = k(1:1)
        else if (len(k) == 5 .and. k(1:4) == 'alt-') then
            c = k(5:5)
        else if (len(k) == 6 .and. k(1:5) == 'ctrl-') then
            c = k(6:6)
        else
            return
        end if
        if (c < '0' .or. c > '9') return
        d = iachar(c) - iachar('0')
    end function digit_of_key

    subroutine clear_tab_jump()
        g_jump_value = 0
        g_jump_deadline = 0
        g_jump_group = 0
    end subroutine clear_tab_jump

    !> Expire a pending jump once its window has passed.
    !>
    !> Called from the frame loop, whose key read times out every 50ms, so this
    !> runs even while nothing is typed. Without it the status hint would sit
    !> there claiming a window that has long closed.
    subroutine tab_jump_tick(changed)
        use platform_module, only: platform_now_ms
        logical, intent(inout) :: changed

        if (g_jump_value == 0) return
        if (platform_now_ms() <= g_jump_deadline) return
        call clear_tab_jump()
        call set_status_message('')
        changed = .true.
    end subroutine tab_jump_tick

    !> Say what a further digit would do, so the window never feels like a
    !> keystroke that went missing.
    subroutine announce_tab_jump(editor)
        use editor_state_module, only: group_members
        type(editor_state_t), intent(inout) :: editor
        integer, allocatable :: members(:)
        character(len=64) :: msg

        if (g_jump_group /= 0) then
            call group_members(editor, g_jump_group, members)
            write(msg, '(a,i0,a)') 'Tab ' // digit_str(g_jump_value) // &
                ' - digit picks a member (1-', size(members), ')'
        else
            write(msg, '(a)') 'Tab ' // digit_str(g_jump_value) // &
                ' - another digit extends it'
        end if
        call set_status_message(trim(msg))
    end subroutine announce_tab_jump

    function digit_str(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: b

        write(b, '(i0)') n
        s = trim(b)
    end function digit_str

    !> Dragging the panel's top edge to resize it.
    !>
    !> Returns .true. when the event belonged to a resize, so the caller stops
    !> before the selection handler.
    !>
    !> Lives here rather than in terminal_panel_module because a height change
    !> needs the screen size and the viewport, neither of which the panel knows
    !> about -- and because the panel's own mouse handler exists to map screen
    !> coordinates into the grid, which is precisely what the separator row
    !> must NOT do.
    function terminal_resize_drag(editor, ev, button, mrow) result(handled)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: ev
        integer, intent(in) :: button, mrow
        logical :: handled
        logical :: changed
        integer :: new_height

        handled = .false.
        if (.not. is_terminal_panel_visible(editor%terminal_panel)) return

        select case (ev)
        case ('mouse-click')
            ! Left button only, and only on the separator bar itself.
            if (iand(button, 3) /= 0) return
            if (mrow /= editor%terminal_panel%screen_start_row) return
            editor%terminal_panel%resize_dragging = .true.
            editor%terminal_panel%focused = .true.
            handled = .true.

        case ('mouse-drag')
            if (.not. editor%terminal_panel%resize_dragging) return
            handled = .true.
            ! The pointer row IS where the separator should go, so the bar
            ! stays under the cursor instead of drifting away from it.
            new_height = editor%screen_rows - mrow
            call terminal_panel_set_height(editor%terminal_panel, &
                permille_for(new_height, editor%screen_rows), &
                editor%screen_rows, editor%screen_cols, changed)
            ! Only when the row count actually moved. A drag delivers an event
            ! per cell of pointer travel and most land on the row already
            ! showing; resizing for those is a calloc and a SIGWINCH to the
            ! shell for no visible change.
            if (changed) then
                call update_viewport(editor)
                call terminal_write(achar(27) // '[2J')
                g_lsp_ui_changed = .true.
            end if

        case ('mouse-release')
            if (.not. editor%terminal_panel%resize_dragging) return
            editor%terminal_panel%resize_dragging = .false.
            handled = .true.
        end select
    end function terminal_resize_drag

    !> Resize the panel from the command palette, where the panel is usually
    !> not the thing with focus.
    subroutine palette_resize_terminal(editor, chord)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: chord

        if (.not. is_terminal_panel_visible(editor%terminal_panel)) then
            ! Say what is wrong rather than reporting a limit that is not the
            ! reason nothing happened.
            call set_status_message('The terminal panel is not open (F5 opens it)')
            return
        end if
        call resize_terminal_panel_key(editor, chord)
    end subroutine palette_resize_terminal

    !> Grow, shrink or maximise the terminal panel.
    !>
    !> Every height change has to do three things together, which is why they
    !> live here rather than at each call site: resize the panel, re-clamp the
    !> viewport -- the document just gained or lost rows and the caret may now
    !> be off-screen -- and force a full repaint, because the panel draws only
    !> its own rows and shrinking would otherwise leave the old ones behind.
    subroutine resize_terminal_panel_key(editor, key_str)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: key_str
        logical :: changed
        character(len=48) :: msg

        changed = .false.
        select case (key_str)
        case ('ctrl-shift-up')
            call terminal_panel_nudge_height(editor%terminal_panel, 1, &
                editor%screen_rows, editor%screen_cols, changed)
        case ('ctrl-shift-down')
            call terminal_panel_nudge_height(editor%terminal_panel, -1, &
                editor%screen_rows, editor%screen_cols, changed)
        case ('ctrl-shift-m')
            call terminal_panel_toggle_maximize(editor%terminal_panel, &
                editor%screen_rows, editor%screen_cols, changed)
        end select

        if (.not. changed) then
            ! Say so rather than doing nothing visible: at the clamp the key
            ! is not broken, there is simply no room.
            call set_status_message('Terminal panel is at its limit')
            return
        end if

        call update_viewport(editor)
        call terminal_write(achar(27) // '[2J')
        g_lsp_ui_changed = .true.

        if (terminal_panel_is_maximized(editor%terminal_panel)) then
            call set_status_message('Terminal maximized (ctrl-shift-m restores)')
        else
            write(msg, '(a,i0,a)') 'Terminal panel ', &
                get_terminal_panel_height(editor%terminal_panel), ' rows'
            call set_status_message(trim(msg))
        end if
    end subroutine resize_terminal_panel_key

    subroutine step_row1_entry(editor, buffer, delta)
        use editor_state_module, only: active_group_id, group_members
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: delta
        integer(int32) :: ids(512), gid
        integer :: n, here, target, i, pos
        integer, allocatable :: members(:)

        if (size(editor%tabs) == 0) return

        ! Inside a group the walk is over its members, so left/right reads as
        ! one continuous line through every open file rather than treating a
        ! group as a single stop. Only stepping off the first or last member
        ! leaves -- and it does leave, which is what makes this a way out that
        ! survives a compositor eating super+ctrl+up.
        gid = active_group_id(editor)
        if (gid /= 0) then
            call group_members(editor, gid, members)
            pos = 0
            do i = 1, size(members)
                if (members(i) == editor%active_tab_index) pos = i
            end do
            if (pos > 0) then
                target = pos + delta
                if (target >= 1 .and. target <= size(members)) then
                    call switch_to_tab_with_buffer(editor, members(target), buffer)
                    return
                end if
            end if
            ! Off the edge. Fall through: row1_entries reports `here` as this
            ! group, so `here + delta` is the entry beside it.
        end if

        call row1_entries(editor, ids, n, here)
        if (n == 0) return

        target = here + delta
        if (target < 1) target = n
        if (target > n) target = 1

        call note_group_position(editor)
        if (ids(target) < 0) then
            call enter_group_at_edge(editor, buffer, int(-ids(target), int32), delta)
        else
            call switch_to_tab_with_buffer(editor, int(ids(target)), buffer)
        end if
    end subroutine step_row1_entry

    !> Enter `gid` from the side the walk arrived from: moving right lands on
    !> the first member, moving left on the last.
    !>
    !> That is what makes the walk reversible -- retrace your steps and you
    !> visit the same files in reverse order. Deliberately NOT enter_tab_group,
    !> which resumes at the remembered member: right for an explicit
    !> super+ctrl+down, wrong for a walk, which would then skip every member
    !> between the edge and wherever you last happened to be.
    subroutine enter_group_at_edge(editor, buffer, gid, delta)
        use editor_state_module, only: group_members
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32), intent(in) :: gid
        integer, intent(in) :: delta
        integer, allocatable :: members(:)

        call group_members(editor, gid, members)
        if (size(members) == 0) return
        if (delta > 0) then
            call switch_to_tab_with_buffer(editor, members(1), buffer)
        else
            call switch_to_tab_with_buffer(editor, members(size(members)), buffer)
        end if
    end subroutine enter_group_at_edge

    !> The row-1 entries, in order, and which one is current.
    !>
    !> Mirrors what render_tab_bar builds: a group appears once, where its
    !> first member sits, encoded as -(group id); an ungrouped tab appears as
    !> its own index.
    subroutine row1_entries(editor, ids, n, here)
        use editor_state_module, only: active_group_id
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(out) :: ids(:)
        integer, intent(out) :: n, here
        integer :: i, k
        integer(int32) :: gid
        logical :: seen

        n = 0
        here = 1
        do i = 1, size(editor%tabs)
            if (n >= size(ids)) exit
            gid = editor%tabs(i)%group_id
            if (gid /= 0) then
                seen = .false.
                do k = 1, n
                    if (ids(k) == -gid) seen = .true.
                end do
                if (seen) cycle
                n = n + 1
                ids(n) = -gid
                if (gid == active_group_id(editor)) here = n
            else
                n = n + 1
                ids(n) = i
                if (i == editor%active_tab_index) here = n
            end if
        end do
    end subroutine row1_entries

    !> Descend into a group: the one being previewed if the pointer is on it,
    !> otherwise the first group on row 1.
    subroutine enter_group_from_row1(editor, buffer)
        use editor_state_module, only: active_group_id
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32) :: ids(512)
        integer :: n, here, i

        if (active_group_id(editor) /= 0) return    ! already inside one
        call row1_entries(editor, ids, n, here)
        do i = 1, n
            if (ids(i) < 0) then
                call enter_tab_group(editor, buffer, int(-ids(i), int32))
                return
            end if
        end do
    end subroutine enter_group_from_row1

    !> Step out of the current group to the nearest tab outside it.
    subroutine leave_current_group(editor, buffer)
        use editor_state_module, only: active_group_id
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32) :: gid
        integer :: i

        gid = active_group_id(editor)
        if (gid == 0) return
        call note_group_position(editor)

        ! Prefer a tab after the group, then one before it.
        do i = editor%active_tab_index + 1, size(editor%tabs)
            if (editor%tabs(i)%group_id /= gid) then
                call switch_to_tab_with_buffer(editor, i, buffer)
                return
            end if
        end do
        do i = editor%active_tab_index - 1, 1, -1
            if (editor%tabs(i)%group_id /= gid) then
                call switch_to_tab_with_buffer(editor, i, buffer)
                return
            end if
        end do
        call set_status_message('Every open tab is in this group')
    end subroutine leave_current_group

    !> Make `gid` the active group, landing on the member it was last on.
    !>
    !> last_active_member is a filename rather than an index because indices
    !> renumber; if it no longer resolves, fall back to the first member.
    subroutine enter_tab_group(editor, buffer, gid)
        use editor_state_module, only: group_members, group_find
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer(int32), intent(in) :: gid
        integer, allocatable :: members(:)
        integer :: i, target, gidx

        call group_members(editor, gid, members)
        if (size(members) == 0) return

        target = members(1)
        gidx = group_find(editor, gid)
        if (gidx > 0) then
            if (allocated(editor%groups(gidx)%last_active_member)) then
                do i = 1, size(members)
                    if (allocated(editor%tabs(members(i))%filename)) then
                        if (editor%tabs(members(i))%filename == &
                            editor%groups(gidx)%last_active_member) then
                            target = members(i)
                            exit
                        end if
                    end if
                end do
            end if
        end if

        call switch_to_tab_with_buffer(editor, target, buffer)
    end subroutine enter_tab_group

    !> Remember where we were in a group before leaving it.
    subroutine note_group_position(editor)
        use editor_state_module, only: active_group_id, group_find
        type(editor_state_t), intent(inout) :: editor
        integer(int32) :: gid
        integer :: gidx

        gid = active_group_id(editor)
        if (gid == 0) return
        gidx = group_find(editor, gid)
        if (gidx < 1) return
        if (.not. allocated(editor%tabs(editor%active_tab_index)%filename)) return
        editor%groups(gidx)%last_active_member = &
            editor%tabs(editor%active_tab_index)%filename
    end subroutine note_group_position

    subroutine save_active_pane_buffer(editor, buffer, tab_idx, status)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: tab_idx
        integer, intent(out) :: status
        integer :: pane_idx

        status = 1
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return

        if (allocated(editor%tabs(tab_idx)%panes)) then
            pane_idx = editor%tabs(tab_idx)%active_pane_index
            if (pane_idx >= 1 .and. pane_idx <= size(editor%tabs(tab_idx)%panes)) then
                if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)) then
                    call buffer_save_file(buffer, &
                        editor%tabs(tab_idx)%panes(pane_idx)%filename, status)
                    return
                end if
            end if
        end if

        ! No pane name to go on: fall back to the tab's, which is the old
        ! behaviour and still correct when a tab holds a single file.
        if (allocated(editor%tabs(tab_idx)%filename)) &
            call buffer_save_file(buffer, editor%tabs(tab_idx)%filename, status)
    end subroutine save_active_pane_buffer

    subroutine handle_dirty_buffers_before_switch(editor, should_continue)
        use save_prompt_module, only: save_prompt, save_prompt_result_t
        use text_buffer_module, only: buffer_save_file
        type(editor_state_t), intent(inout) :: editor
        logical, intent(inout) :: should_continue
        type(save_prompt_result_t) :: prompt_result
        integer :: i, save_status, pane_i, pane_status

        should_continue = .true.

        ! Check each tab for modified buffers
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%modified .and. allocated(editor%tabs(i)%filename)) then
                ! Prompt user for this file
                call save_prompt(editor%tabs(i)%filename, prompt_result)

                select case (prompt_result%action)
                    case ('y')
                        ! Save the file
                        ! Every pane, each to its own file. Saving pane 1's
                        ! text under the tab's name wrote the wrong bytes
                        ! whenever a tab held two different files, and left a
                        ! dirty second pane unsaved either way.
                        save_status = 0
                        if (allocated(editor%tabs(i)%panes)) then
                            do pane_i = 1, size(editor%tabs(i)%panes)
                                call save_tab_pane(editor, i, pane_i, pane_status)
                                if (pane_status /= 0 .and. pane_status /= 1) &
                                    save_status = pane_status
                            end do
                        end if

                        if (save_status == 0) then
                            editor%tabs(i)%modified = .false.
                        end if

                    case ('n')
                        ! Skip saving - continue

                    case ('c')
                        ! Cancel the workspace switch
                        should_continue = .false.
                        return
                end select
            end if
        end do
    end subroutine handle_dirty_buffers_before_switch

    !> Prompt to save before closing tab
    subroutine prompt_save_before_close_tab(editor, buffer)
        use save_prompt_module, only: save_prompt, save_prompt_result_t
        use text_prompt_module, only: show_text_prompt
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        type(save_prompt_result_t) :: prompt_result
        integer :: save_status, tab_idx
        character(len=512) :: new_filename
        logical :: cancelled

        tab_idx = editor%active_tab_index

        ! Prompt user to save
        call save_prompt(editor%tabs(tab_idx)%filename, prompt_result)

        if (prompt_result%action == 's') then
            ! User wants to save
            ! Check if untitled - need filename
            if (index(editor%tabs(tab_idx)%filename, '[Untitled') == 1) then
                call show_text_prompt('Save as: ', new_filename, cancelled, editor%screen_rows)
                if (.not. cancelled .and. len_trim(new_filename) > 0) then
                    ! Update filename and save
                    if (allocated(editor%tabs(tab_idx)%filename)) deallocate(editor%tabs(tab_idx)%filename)
                    allocate(character(len=len_trim(new_filename)) :: editor%tabs(tab_idx)%filename)
                    editor%tabs(tab_idx)%filename = trim(new_filename)

                    call buffer_save_file(buffer, new_filename, save_status)
                    if (save_status == 0) then
                        buffer%modified = .false.
                        editor%tabs(tab_idx)%modified = .false.
                    end if
                else
                    ! User cancelled filename prompt - don't close tab
                    return
                end if
            else
                ! Not untitled - just save
                ! The working buffer is the ACTIVE PANE's text, so it must go
                ! to that pane's file -- not to the tab's, which may name a
                ! different one.
                call save_active_pane_buffer(editor, buffer, tab_idx, save_status)
                if (save_status == 0) then
                    buffer%modified = .false.
                    editor%tabs(tab_idx)%modified = .false.
                end if
            end if

            ! After saving, close the tab
            call close_tab_without_prompt(editor, buffer)

        else if (prompt_result%action == 'd') then
            ! User wants to discard - just close
            call close_tab_without_prompt(editor, buffer)

        ! else if 'c' (cancel) - do nothing, don't close tab
        end if
    end subroutine prompt_save_before_close_tab

    !> Close tab without prompting
    ! Load the active pane's own text and identity into the working buffer.
    !
    ! A tab's panes need not hold the same file -- opening from the tree with
    ! alt-v/alt-s splits a *different* file into the current tab -- so the
    ! tab-level buffer and filename describe only whichever pane was last
    ! active. Reloading from those after closing a pane handed the survivor
    ! the closed pane's text while it kept its own name, and the next Ctrl-S
    ! wrote that text over the survivor's file on disk.
    subroutine load_active_pane_into_buffer(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: tab_idx, pane_idx
        logical :: from_pane

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return

        from_pane = .false.
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (allocated(editor%tabs(tab_idx)%panes)) then
            if (pane_idx >= 1 .and. pane_idx <= size(editor%tabs(tab_idx)%panes)) then
                from_pane = allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)
            end if
        end if

        if (from_pane) then
            call copy_buffer(buffer, editor%tabs(tab_idx)%panes(pane_idx)%buffer)
            editor%modified = buffer%modified

            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len(editor%tabs(tab_idx)%panes(pane_idx)%filename)) :: &
                     editor%filename)
            editor%filename = editor%tabs(tab_idx)%panes(pane_idx)%filename

            ! Keep the tab's own copies in step, or the tab bar keeps naming
            ! the file that is gone and the next tab-level write saves this
            ! text under that name.
            if (size(editor%tabs(tab_idx)%panes) == 1) then
                if (allocated(editor%tabs(tab_idx)%filename)) &
                    deallocate(editor%tabs(tab_idx)%filename)
                allocate(character(len=len(editor%filename)) :: editor%tabs(tab_idx)%filename)
                editor%tabs(tab_idx)%filename = editor%filename
            end if
            call copy_buffer(editor%tabs(tab_idx)%panes(active_pane_of(editor, tab_idx))%buffer, buffer)
            editor%tabs(tab_idx)%modified = buffer%modified
        else
            call copy_buffer(buffer, editor%tabs(tab_idx)%panes(active_pane_of(editor, tab_idx))%buffer)
            editor%modified = editor%tabs(tab_idx)%modified
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len(editor%tabs(tab_idx)%filename)) :: editor%filename)
            editor%filename = editor%tabs(tab_idx)%filename
        end if
    end subroutine load_active_pane_into_buffer

    subroutine close_tab_without_prompt(editor, buffer)
        use editor_state_module, only: tab_is_resident
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: tab_idx

        tab_idx = editor%active_tab_index

        ! If this is the last tab, close it and clear editor
        if (size(editor%tabs) == 1) then
            call close_tab(editor, tab_idx)

            ! Clear the buffer and open fuss mode
            call cleanup_buffer(buffer)
            call init_buffer(buffer)
            editor%fuss_mode_active = .true.
            if (allocated(editor%filename)) deallocate(editor%filename)
            editor%modified = .false.
            if (allocated(editor%workspace_path)) then
                call init_tree_state(tree_state, editor%workspace_path)
            end if
        else
            ! Multiple tabs - close current tab normally
            call close_tab(editor, tab_idx)

            ! Copy new active tab's buffer
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                ! The tab we land on may never have been READ. Group members
                ! are deferred, so the neighbour of a closed member usually
                ! has no panes at all, and reading its buffer below is a null
                ! dereference -- a segfault on closing a tab next to a lazily
                ! loaded one. This code predates deferred tabs.
                !
                ! switch_to_tab_with_buffer already knows how to read one in,
                ! and clearing the index first is what tells it there is
                ! nothing to save: the working buffer still holds the text of
                ! the tab that was just closed, and saving that into the
                ! survivor would write the wrong file's contents into it.
                if (.not. tab_is_resident(editor, int(editor%active_tab_index))) then
                    block
                        integer :: landed
                        landed = editor%active_tab_index
                        editor%active_tab_index = 0
                        call switch_to_tab_with_buffer(editor, &
                            int(landed, int32), buffer)
                    end block
                else
                    call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                        editor%active_tab_index))%buffer)
                    editor%modified = editor%tabs(editor%active_tab_index)%modified
                    if (allocated(editor%filename)) deallocate(editor%filename)
                    allocate(character(len=len(editor%tabs(editor%active_tab_index)%filename)) :: editor%filename)
                    editor%filename = editor%tabs(editor%active_tab_index)%filename
                end if
            end if
        end if
    end subroutine close_tab_without_prompt

    ! Notify LSP server of buffer changes
    ! Mark the active tab's document as changed. Async replies captured the
    ! old value and drop themselves when it no longer matches.
    subroutine bump_doc_revision(editor)
        type(editor_state_t), intent(inout) :: editor

        if (editor%active_tab_index < 1 .or. &
            editor%active_tab_index > size(editor%tabs)) return
        editor%tabs(editor%active_tab_index)%doc_revision = &
            editor%tabs(editor%active_tab_index)%doc_revision + 1
    end subroutine bump_doc_revision

    function current_doc_revision(editor) result(rev)
        type(editor_state_t), intent(in) :: editor
        integer(int64) :: rev

        rev = 0
        if (editor%active_tab_index < 1 .or. &
            editor%active_tab_index > size(editor%tabs)) return
        rev = editor%tabs(editor%active_tab_index)%doc_revision
    end function current_doc_revision

    subroutine notify_buffer_change(editor, buffer)
        use document_sync_module, only: notify_document_change
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: full_content

        ! Only notify if we have an active tab with LSP support
        if (editor%active_tab_index < 1 .or. &
            editor%active_tab_index > size(editor%tabs)) return
        if (editor%tabs(editor%active_tab_index) &
            %num_lsp_servers < 1) return

        ! Get full content in O(n) via gap buffer extraction
        full_content = buffer_to_string(buffer)

        call notify_document_change( &
            editor%tabs(editor%active_tab_index)%document_sync, &
            full_content)
        if (allocated(full_content)) deallocate(full_content)
    end subroutine notify_buffer_change

    ! Insert the not-yet-typed remainder of the ghost suggestion at the cursor
    subroutine accept_ghost_suggestion(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text
        logical :: is_block

        ! Revalidate: the suggestion must still be anchored at the live
        ! cursor (mid-line is fine; insertion pushes the tail right)
        if (editor%cursors(editor%active_cursor)%line /= editor%ghost%anchor_line .or. &
            editor%cursors(editor%active_cursor)%column /= editor%ghost%anchor_col) then
            call ghost_clear(editor%ghost)
            return
        end if

        text = ghost_insert_text(editor%ghost)
        if (len(text) == 0) then
            call ghost_clear(editor%ghost)
            return
        end if

        is_block = ghost_is_block(editor%ghost)

        ! A block ALWAYS gets its own checkpoint. Without this, accepting one
        ! straight after typing coalesces with that typing run and Ctrl-Z
        ! cannot remove the block on its own.
        if (is_block .or. .not. last_action_was_edit) call save_undo_state(buffer, editor)

        if (is_block) then
            ! One insert for the whole block. insert_line_text moves the gap
            ! once per character and mis-tracks the column across a newline;
            ! buffer_insert is byte-oriented and line count is derived by
            ! scanning for LF, so embedded newlines just work.
            call insert_block_at_cursor(buffer, editor%cursors(editor%active_cursor), text)
        else
            call insert_line_text(buffer, editor%cursors(editor%active_cursor), text)
            ! A header suggestion carries its own closing quote, so accepting
            ! one on top of the quote auto-close already parked there left
            ! #include "showme.h"" and a character to delete by hand.
            call absorb_closer_the_completion_supplied(editor, buffer, text)
        end if

        editor%cursors(editor%active_cursor)%desired_column = &
            editor%cursors(editor%active_cursor)%column
        call sync_editor_to_pane(editor)
        call update_viewport(editor)
        call ghost_clear(editor%ghost)

        ! We return before handle_key_command's common tail, so update the
        ! edit-coalescing state and notify LSP here
        last_action_was_edit = .true.
        call notify_buffer_change(editor, buffer)
    end subroutine accept_ghost_suggestion

    ! Insert LF-separated text at the cursor in one buffer operation, then
    ! place the caret at the end of what was inserted.
    subroutine insert_block_at_cursor(buffer, cursor, text)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: text
        integer :: pos, i, newlines, last_nl

        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, text)

        newlines = 0
        last_nl = 0
        do i = 1, len(text)
            if (text(i:i) == achar(10)) then
                newlines = newlines + 1
                last_nl = i
            end if
        end do

        if (newlines == 0) then
            cursor%column = cursor%column + utf8_char_count(text)
        else
            cursor%line = cursor%line + newlines
            cursor%column = utf8_char_count(text(last_nl+1:)) + 1
        end if
        cursor%desired_column = cursor%column
    end subroutine insert_block_at_cursor

    ! Accept one word of the suggestion, keeping the rest ghosted.
    subroutine accept_ghost_word(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: word

        if (editor%cursors(editor%active_cursor)%line /= editor%ghost%anchor_line .or. &
            editor%cursors(editor%active_cursor)%column /= editor%ghost%anchor_col) then
            call ghost_clear(editor%ghost)
            return
        end if

        word = ghost_take_word(editor%ghost)
        if (len(word) == 0) return

        if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
        call insert_line_text(buffer, editor%cursors(editor%active_cursor), word)
        editor%cursors(editor%active_cursor)%desired_column = &
            editor%cursors(editor%active_cursor)%column
        call sync_editor_to_pane(editor)
        call update_viewport(editor)

        last_action_was_edit = .true.
        call notify_buffer_change(editor, buffer)
    end subroutine accept_ghost_word

    ! Accept one line of a block, keeping the rest offered below.
    subroutine accept_ghost_line(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text
        logical :: ok

        if (editor%cursors(editor%active_cursor)%line /= editor%ghost%anchor_line .or. &
            editor%cursors(editor%active_cursor)%column /= editor%ghost%anchor_col) then
            call ghost_clear(editor%ghost)
            return
        end if

        call ghost_take_line(editor%ghost, text, ok)
        if (.not. ok .or. len(text) == 0) return

        ! Each accepted line gets its own checkpoint, so undo walks back
        ! through them one at a time rather than collapsing the lot.
        call save_undo_state(buffer, editor)
        call insert_block_at_cursor(buffer, editor%cursors(editor%active_cursor), text)

        ! Re-anchor to wherever the caret actually ended up -- only the buffer
        ! knows that once a newline has been inserted.
        call ghost_set_anchor(editor%ghost, &
                              editor%cursors(editor%active_cursor)%line, &
                              editor%cursors(editor%active_cursor)%column)

        call sync_editor_to_pane(editor)
        call update_viewport(editor)

        last_action_was_edit = .true.
        call notify_buffer_change(editor, buffer)
    end subroutine accept_ghost_line

    ! Recompute the shadow suggestion after an edit keystroke. The word-scan
    ! result shows immediately; an LSP completion request may upgrade it when
    ! the async response arrives.
    subroutine update_ghost_suggestion(editor, buffer, key_str, ghost_was_extended)
        type(editor_state_t), intent(inout), target :: editor
        type(buffer_t), intent(inout), target :: buffer
        character(len=*), intent(in) :: key_str
        logical, intent(in) :: ghost_was_extended
        character(len=:), allocatable :: prefix
        integer :: completion_server, request_id, cur_line, cur_col
        logical :: trigger_key, in_include

        if (.not. editor%ghost%enabled) return

        ! The suggestion already advanced with the keystroke; re-querying
        ! would throw away a correct answer and pay for another round trip.
        if (ghost_was_extended) return

        if (size(editor%cursors) /= 1) return
        if (editor%cursors(editor%active_cursor)%has_selection) return
        if (is_completion_visible(editor%completion_popup)) return

        cur_line = editor%cursors(editor%active_cursor)%line
        cur_col = editor%cursors(editor%active_cursor)%column

        ! '#include <par' completes header names; prefix is the path
        ! segment being typed (may be empty right after '<' or '/')
        call ghost_get_include_prefix(buffer, cur_line, cur_col, prefix, in_include)

        ! Word-char typing and backspace refresh a suggestion; in include
        ! context any printable char does ('<', '/', '.', ...)
        trigger_key = .false.
        if (len_trim(key_str) == 1) then
            if (in_include) then
                trigger_key = iachar(key_str(1:1)) >= 33
            else
                trigger_key = is_word_char(key_str(1:1))
            end if
        else if (trim(key_str) == 'backspace') then
            trigger_key = .true.
        end if

        ! The model backend gets a WIDER trigger set than LSP, and is asked
        ! before the LSP early-return below.
        !
        ! Enter is the single most valuable moment: you finish a comment
        ! describing what you want, press Enter, and that is exactly when a
        ! completion should appear. Sharing the LSP trigger set meant the AI
        ! only ever fired on word characters, so "write the comment, get the
        ! code" -- the headline capability -- never actually happened.
        if (ai_is_enabled(editor%ai) .and. .not. in_include) then
            if (ai_trigger_key(key_str)) then
                call ai_note_trigger(editor%ai, cur_line, cur_col, &
                                     ai_prefix_at(buffer, cur_line, cur_col), &
                                     context_line_after_cursor(buffer, cur_line, cur_col), &
                                     current_doc_revision(editor), &
                                     ai_active_filename(editor))
            end if
        end if

        if (.not. trigger_key) return

        if (.not. in_include) then
            ! Mid-line is fine, but only at a word boundary: with the
            ! cursor inside a word the ghost would duplicate its tail
            if (.not. ghost_at_word_boundary(buffer, cur_line, cur_col)) return
            call ghost_get_prefix_at_cursor(buffer, cur_line, cur_col, prefix)
            if (len(prefix) == 0) return

            ! Instant suggestion from words in this file
            call ghost_update_from_buffer(editor%ghost, buffer, prefix, cur_line, cur_col)
        else
            ! Header names come from the LSP only; drop any stale ghost
            call ghost_clear(editor%ghost)
        end if

        ! Ask LSP for a (better) completion; the response is validated and
        ! applied asynchronously by the wrapper below
        completion_server = get_lsp_server_for_cap(editor, CAP_COMPLETION)
        if (completion_server > 0) then
            ! Document sync is debounced (~500ms); force-flush so the server
            ! completes against the just-edited document, not a stale one
            block
                use document_sync_module, only: flush_pending_changes
                call flush_pending_changes( &
                    editor%tabs(editor%active_tab_index)%document_sync, &
                    editor%lsp_manager, .true.)
            end block
            saved_editor_for_callback => editor
            saved_buffer_for_callback => buffer
            request_id = request_completion(editor%lsp_manager, completion_server, &
                editor%tabs(editor%active_tab_index)%filename, &
                cur_line - 1, cur_col - 1, &
                handle_ghost_completion_response_wrapper)
            if (request_id > 0) then
                editor%ghost%pending_request_id = request_id
                editor%ghost%pending_prefix = prefix
                editor%ghost%pending_include = in_include
                editor%ghost%pending_doc_revision = current_doc_revision(editor)
            end if
        end if
    end subroutine update_ghost_suggestion

    ! True when the cursor is not sitting inside a word: at end of line or
    ! on a non-word character (closing bracket, quote, space, ...)
    function ghost_at_word_boundary(buffer, line_num, col) result(ok)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        logical :: ok
        character(len=:), allocatable :: line
        integer :: b

        ok = .true.
        line = buffer_get_line(buffer, line_num)
        b = utf8_char_to_byte_index(line, col)
        if (b >= 1 .and. b <= len(line)) ok = .not. is_word_char(line(b:b))
    end function ghost_at_word_boundary

    ! Wrapper callback matching the LSP callback signature (ghost text)
    subroutine handle_ghost_completion_response_wrapper(request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: request_id
        type(lsp_message_t), intent(in) :: response

        if (associated(saved_editor_for_callback) .and. &
            associated(saved_buffer_for_callback)) then
            call handle_ghost_completion_response_impl(saved_editor_for_callback, &
                saved_buffer_for_callback, request_id, response)
        end if
    end subroutine handle_ghost_completion_response_wrapper

    subroutine handle_ghost_completion_response_impl(editor, buffer, request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: request_id
        type(lsp_message_t), intent(in) :: response
        character(len=:), allocatable :: pend, prefix, before
        integer :: cur_line, cur_col
        integer(int64) :: pend_rev
        logical :: was_include, in_include

        ! Only the most recent request may update the ghost
        if (request_id /= editor%ghost%pending_request_id) return
        pend_rev = editor%ghost%pending_doc_revision
        if (allocated(editor%ghost%pending_prefix)) then
            pend = editor%ghost%pending_prefix
        else
            pend = ''
        end if
        was_include = editor%ghost%pending_include
        call ghost_clear_pending(editor%ghost)

        ! Revalidate against current editor state: the user may have moved,
        ! edited, or opened the popup while the request was in flight.
        ! The document revision is checked first because an undo can restore
        ! the cursor and the typed prefix while changing everything else,
        ! which every other check here would wave through.
        if (pend_rev /= current_doc_revision(editor)) return
        if (is_completion_visible(editor%completion_popup)) return
        if (size(editor%cursors) /= 1) return
        if (editor%cursors(editor%active_cursor)%has_selection) return
        cur_line = editor%cursors(editor%active_cursor)%line
        cur_col = editor%cursors(editor%active_cursor)%column
        if (was_include) then
            call ghost_get_include_prefix(buffer, cur_line, cur_col, prefix, in_include)
            if (.not. in_include) return
        else
            if (.not. ghost_at_word_boundary(buffer, cur_line, cur_col)) return
            call ghost_get_prefix_at_cursor(buffer, cur_line, cur_col, prefix)
            if (len(prefix) == 0) return
        end if
        if (prefix /= pend) return

        if (ghost_is_active(editor%ghost)) before = editor%ghost%suggestion
        call ghost_apply_lsp_result(editor%ghost, response%result, prefix, cur_line, cur_col, &
                                    was_include)

        ! Redraw (without a keypress) only if the suggestion actually changed
        if (ghost_is_active(editor%ghost)) then
            if (.not. allocated(before)) then
                g_lsp_ui_changed = .true.
            else if (editor%ghost%suggestion /= before) then
                g_lsp_ui_changed = .true.
            end if
        end if
    end subroutine handle_ghost_completion_response_impl

    ! Wrapper callback matching the LSP callback signature (ctrl-space popup)
    subroutine handle_popup_completion_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        if (associated(saved_editor_for_callback)) then
            call handle_popup_completion_response_impl(saved_editor_for_callback, response)
        end if
    end subroutine handle_popup_completion_response_wrapper

    subroutine handle_popup_completion_response_impl(editor, response)
        use lsp_protocol_module, only: lsp_message_t
        type(editor_state_t), intent(inout) :: editor
        type(lsp_message_t), intent(in) :: response

        call handle_completion_response(editor%completion_popup, response%result)
        if (editor%completion_popup%item_count > 0) then
            call show_completion_popup(editor%completion_popup, &
                editor%cursors(editor%active_cursor)%line - editor%viewport_line + 2, &
                editor%cursors(editor%active_cursor)%column - editor%viewport_column + 1, &
                editor%screen_rows, editor%screen_cols)
            g_lsp_ui_changed = .true.
        end if
    end subroutine handle_popup_completion_response_impl

    ! Wrapper callback that matches the LSP callback signature
    subroutine handle_references_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        ! Call the actual handler with saved editor state
        if (associated(saved_editor_for_callback)) then
            call handle_references_response_impl(saved_editor_for_callback, response)
        end if
    end subroutine handle_references_response_wrapper

    ! Handle LSP textDocument/references response implementation
    subroutine handle_references_response_impl(editor, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_get_array, json_get_object, &
                               json_get_string, json_get_number, json_array_size, &
                               json_get_array_element, json_has_key
        type(editor_state_t), intent(inout) :: editor
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: result_array, location_obj, range_obj
        type(json_value_t) :: start_obj, end_obj
        type(reference_location_t), allocatable :: references(:)
        integer :: num_refs, i
        character(len=:), allocatable :: uri
        real(8) :: line_real, col_real

        ! The result is directly in response%result for LSP responses
        result_array = response%result
        num_refs = json_array_size(result_array)

        if (num_refs == 0) then
            ! No references found
            allocate(references(0))
            call set_references(editor%references_panel, references, 0)
            return
        end if

        ! Allocate references array
        allocate(references(num_refs))

        ! Initialize all fields
        do i = 1, num_refs
            references(i)%line = 1
            references(i)%column = 1
            references(i)%end_line = 1
            references(i)%end_column = 1
        end do

        ! Parse each reference location
        do i = 1, num_refs
            location_obj = json_get_array_element(result_array, i - 1)

            ! Get URI
            uri = json_get_string(location_obj, 'uri', '')
            if (len(uri) > 0) then
                allocate(character(len=len(uri)) :: references(i)%uri)
                references(i)%uri = uri

                ! Extract filename from URI
                if (len(uri) > 7) then
                    if (uri(1:7) == "file://") then
                        allocate(character(len=len(uri)-7) :: references(i)%filename)
                        references(i)%filename = uri(8:)
                    end if
                end if
            end if

            ! Get range
            if (json_has_key(location_obj, 'range')) then
                range_obj = json_get_object(location_obj, 'range')

                ! Get start position
                if (json_has_key(range_obj, 'start')) then
                    start_obj = json_get_object(range_obj, 'start')
                    line_real = json_get_number(start_obj, 'line', 0.0d0)
                    references(i)%line = int(line_real) + 1  ! Convert from 0-based to 1-based
                    col_real = json_get_number(start_obj, 'character', 0.0d0)
                    references(i)%column = int(col_real) + 1  ! Convert from 0-based to 1-based
                end if

                ! Get end position
                if (json_has_key(range_obj, 'end')) then
                    end_obj = json_get_object(range_obj, 'end')
                    line_real = json_get_number(end_obj, 'line', 0.0d0)
                    references(i)%end_line = int(line_real) + 1
                    col_real = json_get_number(end_obj, 'character', 0.0d0)
                    references(i)%end_column = int(col_real) + 1
                end if
            end if

            ! TODO: Load preview text from the file if available
            allocate(character(len=50) :: references(i)%preview_text)
            references(i)%preview_text = "..."  ! Placeholder
        end do

        ! Update the references panel
        call set_references(editor%references_panel, references, num_refs)

        ! Clean up
        do i = 1, num_refs
            if (allocated(references(i)%uri)) deallocate(references(i)%uri)
            if (allocated(references(i)%filename)) deallocate(references(i)%filename)
            if (allocated(references(i)%preview_text)) deallocate(references(i)%preview_text)
        end do
        deallocate(references)

    end subroutine handle_references_response_impl

    ! Wrapper callback that matches the LSP callback signature for code actions
    subroutine handle_code_actions_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        ! Call the actual handler with saved editor state
        if (associated(saved_editor_for_callback)) then
            call handle_code_actions_response_impl(saved_editor_for_callback, response)
        end if
    end subroutine handle_code_actions_response_wrapper

    ! Handle LSP textDocument/codeAction response implementation
    subroutine handle_code_actions_response_impl(editor, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_get_array, json_get_object, &
                               json_get_string, json_get_bool, json_array_size, &
                               json_get_array_element, json_has_key, json_stringify
        use code_actions_panel_module, only: code_action_t
        type(editor_state_t), intent(inout) :: editor
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: result_array, action_obj
        type(code_action_t), allocatable :: actions(:)
        integer :: num_actions, i
        character(len=:), allocatable :: title, kind, action_json

        ! The result is directly in response%result for LSP responses
        result_array = response%result
        num_actions = json_array_size(result_array)

        if (num_actions == 0) then
            ! No actions available - don't show panel
            return
        end if

        ! Allocate and fill actions array
        allocate(actions(num_actions))

        do i = 1, num_actions
            ! json_get_array_element expects 0-based index
            action_obj = json_get_array_element(result_array, i - 1)

            ! Get title (required)
            if (json_has_key(action_obj, 'title')) then
                title = json_get_string(action_obj, 'title', '')
                if (allocated(actions(i)%title)) deallocate(actions(i)%title)
                allocate(character(len=len(title)) :: actions(i)%title)
                actions(i)%title = title
            end if

            ! Get kind (optional)
            if (json_has_key(action_obj, 'kind')) then
                kind = json_get_string(action_obj, 'kind', '')
                if (allocated(actions(i)%kind)) deallocate(actions(i)%kind)
                allocate(character(len=len(kind)) :: actions(i)%kind)
                actions(i)%kind = kind
            end if

            ! Get isPreferred (optional)
            if (json_has_key(action_obj, 'isPreferred')) then
                actions(i)%is_preferred = json_get_bool(action_obj, 'isPreferred', .false.)
            else
                actions(i)%is_preferred = .false.
            end if

            ! Store the entire action as JSON for later application
            action_json = json_stringify(action_obj)
            if (allocated(actions(i)%action_json)) deallocate(actions(i)%action_json)
            allocate(character(len=len(action_json)) :: actions(i)%action_json)
            actions(i)%action_json = action_json
        end do

        ! Update the code actions panel and show it
        call set_code_actions(editor%code_actions_panel, actions, num_actions)
        call show_code_actions_panel(editor%code_actions_panel)
        g_lsp_ui_changed = .true.  ! Trigger re-render

        ! Clean up
        do i = 1, num_actions
            if (allocated(actions(i)%title)) deallocate(actions(i)%title)
            if (allocated(actions(i)%kind)) deallocate(actions(i)%kind)
            if (allocated(actions(i)%action_json)) deallocate(actions(i)%action_json)
        end do
        deallocate(actions)

    end subroutine handle_code_actions_response_impl

    ! Wrapper callback that matches the LSP callback signature for symbols
    subroutine handle_symbols_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        ! Call the actual handler with saved editor state
        if (associated(saved_editor_for_callback)) then
            call handle_symbols_response_impl(saved_editor_for_callback, response)
        end if
    end subroutine handle_symbols_response_wrapper

    ! Handle LSP textDocument/documentSymbol response implementation
    subroutine handle_symbols_response_impl(editor, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_get_array, json_get_object, &
                               json_get_string, json_get_number, json_array_size, &
                               json_get_array_element, json_has_key, json_stringify
        type(editor_state_t), intent(inout) :: editor
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: result_array, symbol_obj, location_obj, range_obj
        type(json_value_t) :: start_obj, end_obj, children_array
        type(document_symbol_t), allocatable :: symbols(:)
        integer :: num_symbols, i
        character(len=:), allocatable :: name, detail
        real(8) :: kind_real, line_real, col_real

        ! The result is directly in response%result for LSP responses
        result_array = response%result
        num_symbols = json_array_size(result_array)

        if (num_symbols == 0) then
            call clear_symbols(editor%symbols_panel)
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('No symbols found in document                ')
            return
        end if

        ! Allocate symbols array
        allocate(symbols(num_symbols))

        ! Parse each symbol
        do i = 1, num_symbols
            ! json_get_array_element expects 0-based index
            symbol_obj = json_get_array_element(result_array, i - 1)

            ! Get symbol name (required)
            if (json_has_key(symbol_obj, 'name')) then
                name = json_get_string(symbol_obj, 'name', '')
                if (len(name) > 0) then
                    if (allocated(symbols(i)%name)) deallocate(symbols(i)%name)
                    allocate(character(len=len(name)) :: symbols(i)%name)
                    symbols(i)%name = name
                end if
            end if

            ! Get detail (optional)
            if (json_has_key(symbol_obj, 'detail')) then
                detail = json_get_string(symbol_obj, 'detail', '')
                if (len(detail) > 0) then
                    if (allocated(symbols(i)%detail)) deallocate(symbols(i)%detail)
                    allocate(character(len=len(detail)) :: symbols(i)%detail)
                    symbols(i)%detail = detail
                end if
            end if

            ! Get kind (required)
            if (json_has_key(symbol_obj, 'kind')) then
                kind_real = json_get_number(symbol_obj, 'kind', 13.0d0)  ! Default to Variable
                symbols(i)%kind = int(kind_real)
            else
                symbols(i)%kind = 13  ! Variable
            end if

            ! Get range or location
            if (json_has_key(symbol_obj, 'range')) then
                ! DocumentSymbol format (hierarchical)
                range_obj = json_get_object(symbol_obj, 'range')

                ! Get start position
                if (json_has_key(range_obj, 'start')) then
                    start_obj = json_get_object(range_obj, 'start')
                    line_real = json_get_number(start_obj, 'line', 0.0d0)
                    symbols(i)%line = int(line_real) + 1
                    col_real = json_get_number(start_obj, 'character', 0.0d0)
                    symbols(i)%column = int(col_real) + 1
                end if

                ! Get end position
                if (json_has_key(range_obj, 'end')) then
                    end_obj = json_get_object(range_obj, 'end')
                    line_real = json_get_number(end_obj, 'line', 0.0d0)
                    symbols(i)%end_line = int(line_real) + 1
                    col_real = json_get_number(end_obj, 'character', 0.0d0)
                    symbols(i)%end_column = int(col_real) + 1
                end if

                ! Check for children (hierarchical symbols)
                if (json_has_key(symbol_obj, 'children')) then
                    children_array = json_get_array(symbol_obj, 'children')
                    symbols(i)%num_children = json_array_size(children_array)
                    ! TODO: Parse children recursively
                end if

            else if (json_has_key(symbol_obj, 'location')) then
                ! SymbolInformation format (flat)
                location_obj = json_get_object(symbol_obj, 'location')

                if (json_has_key(location_obj, 'range')) then
                    range_obj = json_get_object(location_obj, 'range')

                    ! Get start position
                    if (json_has_key(range_obj, 'start')) then
                        start_obj = json_get_object(range_obj, 'start')
                        line_real = json_get_number(start_obj, 'line', 0.0d0)
                        symbols(i)%line = int(line_real) + 1
                        col_real = json_get_number(start_obj, 'character', 0.0d0)
                        symbols(i)%column = int(col_real) + 1
                    end if

                    ! Get end position
                    if (json_has_key(range_obj, 'end')) then
                        end_obj = json_get_object(range_obj, 'end')
                        line_real = json_get_number(end_obj, 'line', 0.0d0)
                        symbols(i)%end_line = int(line_real) + 1
                        col_real = json_get_number(end_obj, 'character', 0.0d0)
                        symbols(i)%end_column = int(col_real) + 1
                    end if
                end if
            end if

            symbols(i)%depth = 0  ! Top level
            symbols(i)%is_expanded = .true.
        end do

        ! Update the symbols panel
        call set_symbols(editor%symbols_panel, symbols, num_symbols)
        g_lsp_ui_changed = .true.  ! Trigger re-render to show symbols

        ! Show success message
        call terminal_move_cursor(editor%screen_rows, 1)
        if (num_symbols == 1) then
            call terminal_write('1 symbol found                ')
        else
            block
                character(len=50) :: msg
                write(msg, '(I0,A)') num_symbols, ' symbols found                '
                call terminal_write(trim(msg))
            end block
        end if

        ! Clean up
        do i = 1, num_symbols
            if (allocated(symbols(i)%name)) deallocate(symbols(i)%name)
            if (allocated(symbols(i)%detail)) deallocate(symbols(i)%detail)
            if (allocated(symbols(i)%children)) deallocate(symbols(i)%children)
        end do
        deallocate(symbols)

    end subroutine handle_symbols_response_impl

    ! Wrapper callback that matches the LSP callback signature for signature help
    subroutine handle_signature_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        ! Call the actual handler with saved editor state
        if (associated(saved_editor_for_callback)) then
            call handle_signature_response(saved_editor_for_callback%signature_tooltip, response)
        end if
    end subroutine handle_signature_response_wrapper

    ! Wrapper callback that matches the LSP callback signature for rename
    subroutine handle_rename_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_stringify
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        character(len=:), allocatable :: result_str
        integer :: changes_applied

        if (.false.) print *, unused_request_id  ! Silence unused warning

        if (.not. associated(saved_editor_for_callback)) return

        ! Convert result to string for apply_workspace_edit
        result_str = json_stringify(response%result)

        if (.not. allocated(result_str) .or. result_str == 'null' .or. len_trim(result_str) == 0) then
            saved_editor_for_callback%timed_message = 'Rename failed or not supported'
            saved_editor_for_callback%timed_message_ms = get_time_ms()
            if (allocated(result_str)) deallocate(result_str)
            return
        end if

        ! Apply workspace edit
        call apply_workspace_edit(saved_editor_for_callback, result_str, changes_applied)

        ! Cleared however this turned out. A stale expectation would make the
        ! NEXT workspace edit -- a code action, say -- check itself against an
        ! identifier that has nothing to do with it and refuse to apply.
        if (allocated(g_rename_expect)) deallocate(g_rename_expect)

        if (changes_applied > 0) then
            block
                character(len=64) :: msg
                write(msg, '(A,I0,A)') 'Renamed symbol (', changes_applied, ' changes applied)'
                saved_editor_for_callback%timed_message = trim(msg)
            end block
        else if (index(saved_editor_for_callback%timed_message, 'Rename aborted') /= 1) then
            ! Leave a refusal's own explanation in place. Overwriting it with
            ! 'No changes applied' would report the symptom and discard the
            ! reason, which is the part worth knowing.
            saved_editor_for_callback%timed_message = 'No changes applied'
        end if
        saved_editor_for_callback%timed_message_ms = get_time_ms()

        if (allocated(result_str)) deallocate(result_str)
    end subroutine handle_rename_response_wrapper

    ! Wrapper callback for formatting response
    subroutine handle_formatting_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_array_size, json_get_array_element, &
                               json_get_object, json_get_string, json_get_number, json_has_key
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        type(json_value_t) :: edits_array, edit_obj, range_obj, start_obj, end_obj
        character(len=:), allocatable :: new_text
        integer :: num_edits, i, tab_idx
        integer :: start_line, start_char, end_line, end_char
        integer :: changes_applied, cb_pane

        if (.false.) print *, unused_request_id  ! Silence unused warning

        if (.not. associated(saved_editor_for_callback)) return

        tab_idx = saved_editor_for_callback%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(saved_editor_for_callback%tabs)) return

        ! The result is an array of TextEdit objects
        edits_array = response%result
        num_edits = json_array_size(edits_array)

        if (num_edits == 0) then
            call terminal_move_cursor(saved_editor_for_callback%screen_rows, 1)
            call terminal_write('No formatting changes needed                ')
            return
        end if

        changes_applied = 0
        cb_pane = active_pane_of(saved_editor_for_callback, tab_idx)

        ! Apply edits in reverse order (to preserve positions)
        do i = num_edits - 1, 0, -1
            edit_obj = json_get_array_element(edits_array, i)

            if (.not. json_has_key(edit_obj, 'range')) cycle
            range_obj = json_get_object(edit_obj, 'range')

            if (json_has_key(range_obj, 'start') .and. json_has_key(range_obj, 'end')) then
                start_obj = json_get_object(range_obj, 'start')
                end_obj = json_get_object(range_obj, 'end')

                ! LSP range characters are UTF-16 units; apply_single_edit
                ! takes char columns
                start_line = int(json_get_number(start_obj, 'line', 0.0d0)) + 1
                start_char = char_col_from_lsp(saved_editor_for_callback%tabs(tab_idx)%panes(cb_pane)%buffer, &
                    start_line, int(json_get_number(start_obj, 'character', 0.0d0)))
                end_line = int(json_get_number(end_obj, 'line', 0.0d0)) + 1
                end_char = char_col_from_lsp(saved_editor_for_callback%tabs(tab_idx)%panes(cb_pane)%buffer, &
                    end_line, int(json_get_number(end_obj, 'character', 0.0d0)))

                new_text = json_get_string(edit_obj, 'newText')

                if (allocated(new_text)) then
                    call apply_single_edit(saved_editor_for_callback%tabs(tab_idx)%panes(cb_pane)%buffer, &
                        start_line, start_char, end_line, end_char, new_text)
                    changes_applied = changes_applied + 1
                    deallocate(new_text)
                end if
            end if
        end do

        call terminal_move_cursor(saved_editor_for_callback%screen_rows, 1)
        if (changes_applied > 0) then
            block
                character(len=64) :: msg
                write(msg, '(A,I0,A)') 'Formatted (', changes_applied, ' edits applied)'
                call terminal_write(trim(msg) // '                    ')
            end block
        else
            call terminal_write('No formatting changes applied               ')
        end if
    end subroutine handle_formatting_response_wrapper

    ! Apply the selected code action from the panel
    subroutine apply_selected_code_action(editor, buffer)
        use json_module, only: json_parse, json_value_t, json_get_object, &
                               json_has_key, json_stringify
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: action_json, edit_json
        type(json_value_t) :: action_obj, edit_obj
        integer :: changes_applied

        if (get_selected_action(editor%code_actions_panel, action_json)) then
            ! Parse the action JSON to extract the edit
            action_obj = json_parse(action_json)

            if (json_has_key(action_obj, 'edit')) then
                ! Get the edit object and convert to string for apply_workspace_edit
                edit_obj = json_get_object(action_obj, 'edit')
                edit_json = json_stringify(edit_obj)

                ! Apply the workspace edit
                call apply_workspace_edit(editor, edit_json, changes_applied)

                if (changes_applied > 0) then
                    ! Sync modified tab buffer back to the buffer parameter
                    if (editor%active_tab_index > 0 .and. &
                        editor%active_tab_index <= size(editor%tabs)) then
                        call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                            editor%active_tab_index))%buffer)
                    end if
                    ! Re-render screen to show the applied changes
                    call render_screen(buffer, editor)
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write('Code action applied                        ')
                else
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write('No changes from code action                ')
                end if
            else
                call terminal_move_cursor(editor%screen_rows, 1)
                call terminal_write('Code action has no edit                    ')
            end if

            ! Hide menu after selection
            call hide_code_actions_panel(editor%code_actions_panel)
        end if
    end subroutine apply_selected_code_action

    ! Apply a workspace edit from LSP
    subroutine apply_workspace_edit(editor, edit_json, changes_applied)
        use json_module, only: json_parse, json_value_t, json_get_array, json_array_size, &
                               json_get_array_element, json_get_object, json_get_string, &
                               json_get_number, json_has_key
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: edit_json
        integer, intent(out) :: changes_applied

        type(json_value_t) :: edit_obj, doc_changes_arr, file_change_obj
        type(json_value_t) :: text_doc_obj, edits_arr
        character(len=:), allocatable :: uri
        integer :: num_files, i

        changes_applied = 0

        ! Parse the edit JSON
        edit_obj = json_parse(edit_json)

        ! Try to get documentChanges first (newer format)
        if (json_has_key(edit_obj, 'documentChanges')) then
            doc_changes_arr = json_get_array(edit_obj, 'documentChanges')
            num_files = json_array_size(doc_changes_arr)

            do i = 0, num_files - 1  ! 0-based index
                file_change_obj = json_get_array_element(doc_changes_arr, i)

                ! Get text document URI
                if (json_has_key(file_change_obj, 'textDocument')) then
                    text_doc_obj = json_get_object(file_change_obj, 'textDocument')
                    uri = json_get_string(text_doc_obj, 'uri')
                end if

                ! Get edits array
                if (json_has_key(file_change_obj, 'edits') .and. allocated(uri)) then
                    edits_arr = json_get_array(file_change_obj, 'edits')
                    call apply_file_edits_obj(editor, uri, edits_arr, changes_applied)
                    deallocate(uri)
                end if
            end do

            ! Set flag if any edits were applied (documentChanges format)
            if (changes_applied > 0) then
                g_lsp_modified_buffer = .true.
            end if
            return
        end if

        ! Fall back to changes format (older format - map of URI to edits)
        ! Format: {"changes": {"file:///path": [TextEdit, ...], ...}}
        if (json_has_key(edit_obj, 'changes')) then
            block
                type(json_value_t) :: changes_obj
                integer :: ci

                changes_obj = json_get_object(edit_obj, 'changes')
                if (associated(changes_obj%object_value)) then
                    do ci = 1, changes_obj%object_value%count
                        uri = changes_obj%object_value%pairs(ci)%key
                        call apply_file_edits_obj(editor, uri, &
                            changes_obj%object_value%pairs(ci)%value, &
                            changes_applied)
                    end do
                end if
            end block

            if (changes_applied > 0) then
                g_lsp_modified_buffer = .true.
            end if
            return
        end if

        ! Set flag if any edits were applied
        if (changes_applied > 0) then
            g_lsp_modified_buffer = .true.
        end if

    end subroutine apply_workspace_edit

    ! Apply edits to a specific file (using json_value_t)
    subroutine apply_file_edits_obj(editor, uri, edits_arr, changes_applied)
        use json_module, only: json_value_t, json_array_size, json_get_array_element, &
                               json_get_object, json_get_string, json_get_number, json_has_key
        use text_buffer_module, only: buffer_to_string
        use lsp_server_manager_module, only: notify_file_changed
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: uri
        type(json_value_t), intent(in) :: edits_arr
        integer, intent(inout) :: changes_applied

        type(json_value_t) :: edit_obj, range_obj, start_obj, end_obj
        character(len=:), allocatable :: filename, new_text, buffer_content
        integer :: num_edits, i, j, tab_idx, server_idx, pane_idx
        integer :: start_line, start_char, end_line, end_char

        ! Convert URI to filename
        if (len(uri) >= 8 .and. uri(1:8) == 'file:///') then
            filename = uri(8:)  ! Skip "file://" leaving one /
        else if (len(uri) >= 7 .and. uri(1:7) == 'file://') then
            filename = uri(8:)
        else
            filename = uri
        end if

        ! Find the tab with this file
        tab_idx = 0
        do j = 1, size(editor%tabs)
            if (allocated(editor%tabs(j)%filename)) then
                ! Try exact match first, then check if the absolute path ends with the relative path
                if (trim(editor%tabs(j)%filename) == trim(filename)) then
                    tab_idx = j
                    exit
                else if (len(filename) >= len(editor%tabs(j)%filename)) then
                    ! Check if filename ends with tab filename (handles absolute vs relative paths)
                    if (filename(len(filename)-len(editor%tabs(j)%filename)+1:) == editor%tabs(j)%filename) then
                        tab_idx = j
                        exit
                    end if
                end if
            end if
        end do

        if (tab_idx == 0) then
            ! File not open - skip for now
            if (allocated(filename)) deallocate(filename)
            return
        end if

        ! Get the active pane for this tab (panes contain the actual buffers)
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. .not. allocated(editor%tabs(tab_idx)%panes)) then
            pane_idx = 1  ! Default to first pane
        end if
        if (pane_idx > size(editor%tabs(tab_idx)%panes)) then
            if (allocated(filename)) deallocate(filename)
            return
        end if

        num_edits = json_array_size(edits_arr)

        ! Read every edit out FIRST, check them all, and only then write.
        !
        ! An edit set is one operation. Applying them as they are read means a
        ! set that turns out to be inapplicable has already half-changed the
        ! file, which is worse than either applying it or refusing it -- there
        ! is no way back to a state the user recognises.
        block
            integer :: sl(num_edits), sc(num_edits), el(num_edits), ec(num_edits)
            character(len=:), allocatable :: texts(:)
            integer :: n, maxlen, k
            logical :: ok
            character(len=:), allocatable :: found
            character(len=200) :: why

            n = 0
            maxlen = 1
            do i = 0, num_edits - 1
                edit_obj = json_get_array_element(edits_arr, i)
                if (.not. json_has_key(edit_obj, 'range')) cycle
                new_text = json_get_string(edit_obj, 'newText')
                if (.not. allocated(new_text)) cycle
                maxlen = max(maxlen, len(new_text))
                deallocate(new_text)
            end do
            allocate(character(len=maxlen) :: texts(max(1, num_edits)))

            do i = 0, num_edits - 1
                edit_obj = json_get_array_element(edits_arr, i)
                if (.not. json_has_key(edit_obj, 'range')) cycle
                range_obj = json_get_object(edit_obj, 'range')
                if (.not. (json_has_key(range_obj, 'start') .and. &
                           json_has_key(range_obj, 'end'))) cycle
                new_text = json_get_string(edit_obj, 'newText')
                if (.not. allocated(new_text)) cycle

                start_obj = json_get_object(range_obj, 'start')
                end_obj = json_get_object(range_obj, 'end')

                ! LSP range characters are UTF-16 units; apply_single_edit
                ! takes char columns
                start_line = int(json_get_number(start_obj, 'line', 0.0d0)) + 1
                start_char = char_col_from_lsp(editor%tabs(tab_idx)%panes(pane_idx)%buffer, &
                    start_line, int(json_get_number(start_obj, 'character', 0.0d0)))
                end_line = int(json_get_number(end_obj, 'line', 0.0d0)) + 1
                end_char = char_col_from_lsp(editor%tabs(tab_idx)%panes(pane_idx)%buffer, &
                    end_line, int(json_get_number(end_obj, 'character', 0.0d0)))

                n = n + 1
                sl(n) = start_line; sc(n) = start_char
                el(n) = end_line;   ec(n) = end_char
                texts(n) = new_text
                deallocate(new_text)
            end do

            ! A rename knows what it is replacing, so it can tell whether the
            ! server is describing the same text the editor holds. When it is
            ! not, the ranges point at whatever now occupies those coordinates
            ! -- which is how 'char li' came to be replaced by a variable name.
            ok = .true.
            why = ''
            if (allocated(g_rename_expect)) then
                do k = 1, n
                    found = text_in_range(editor%tabs(tab_idx)%panes(pane_idx)%buffer, &
                                          sl(k), sc(k), el(k), ec(k))
                    if (found /= g_rename_expect) then
                        ok = .false.
                        write(why, '(a,i0,a)') 'Rename aborted: line ', sl(k), &
                            ' holds "' // trim(found) // '", not "' // &
                            trim(g_rename_expect) // '"'
                        exit
                    end if
                end do
            end if

            if (.not. ok) then
                ! Nothing written. Saying which line disagreed matters: the
                ! honest report is that the editor and the server were looking
                ! at different text, not that rename is broken.
                editor%timed_message = trim(why)
                editor%timed_message_ms = get_time_ms()
                if (allocated(filename)) deallocate(filename)
                return
            end if

            ! Back to front, so an earlier edit never shifts a later one's range.
            do k = n, 1, -1
                call apply_single_edit(editor%tabs(tab_idx)%panes(pane_idx)%buffer, &
                    sl(k), sc(k), el(k), ec(k), trim(texts(k)))
                changes_applied = changes_applied + 1
            end do
        end block

        ! Sync the changed document back to all LSP servers
        if (changes_applied > 0) then
            buffer_content = buffer_to_string(editor%tabs(tab_idx)%panes(pane_idx)%buffer)
            if (allocated(buffer_content)) then
                ! Notify all active LSP servers about the document change
                ! Use the absolute path from the URI (filename variable) not the tab's relative path
                do server_idx = 1, editor%lsp_manager%num_servers
                    if (editor%lsp_manager%servers(server_idx)%initialized) then
                        call notify_file_changed(editor%lsp_manager, server_idx, &
                            'file://' // filename, buffer_content)
                    end if
                end do
                deallocate(buffer_content)
            end if
        end if

        if (allocated(filename)) deallocate(filename)
    end subroutine apply_file_edits_obj

    ! Apply a single text edit to a buffer
    subroutine apply_single_edit(buffer, start_line, start_char, end_line, end_char, new_text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: start_line, start_char, end_line, end_char
        character(len=*), intent(in) :: new_text

        integer :: start_pos, end_pos, delete_count

        ! Calculate buffer positions
        start_pos = get_buffer_position(buffer, start_line, start_char)
        end_pos = get_buffer_position(buffer, end_line, end_char)

        if (start_pos <= 0 .or. end_pos <= 0) return

        ! Delete the old text
        delete_count = end_pos - start_pos

        if (delete_count > 0) then
            call buffer_delete(buffer, start_pos, delete_count)
        end if

        ! Insert the new text
        if (len(new_text) > 0) then
            call buffer_insert(buffer, start_pos, new_text)
        end if
    end subroutine apply_single_edit

    ! Execute a command from the command palette
    subroutine execute_palette_command(editor, buffer, cmd_id, should_quit)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: cmd_id
        logical, intent(out) :: should_quit

        should_quit = .false.

        ! Map command IDs to their corresponding key commands
        select case(trim(cmd_id))
        ! File operations
        case('save')
            call handle_key_command('ctrl-s', editor, buffer, should_quit)
        case('save-all')
            call handle_key_command('ctrl-shift-s', editor, buffer, should_quit)
        case('quit')
            call handle_key_command('ctrl-q', editor, buffer, should_quit)
        case('open')
            ! Open fortress mode for file browsing
            editor%fuss_mode_active = .true.
            call render_screen(buffer, editor)
        case('toggle-tree')
            call handle_key_command('f3', editor, buffer, should_quit)

        ! Edit operations
        case('copy')
            call handle_key_command('ctrl-c', editor, buffer, should_quit)
        case('paste')
            call handle_key_command('ctrl-v', editor, buffer, should_quit)
        case('cut')
            call handle_key_command('ctrl-x', editor, buffer, should_quit)
        case('undo')
            call handle_key_command('ctrl-z', editor, buffer, should_quit)
        case('redo')
            ! ctrl-y is yank from the yank stack, not redo
            call handle_key_command('ctrl-shift-z', editor, buffer, should_quit)
        case('toggle-comment')
            call handle_key_command('ctrl-/', editor, buffer, should_quit)
        case('context-menu')
            call handle_key_command('shift-f10', editor, buffer, should_quit)

        ! AI completion
        case('ai-toggle')
            call handle_key_command('alt-i', editor, buffer, should_quit)
        case('ai-deep')
            call handle_key_command('alt-\', editor, buffer, should_quit)
        case('ai-status')
            call set_status_message(ai_status_line(editor%ai))

        ! Tab groups. Until the picker modal lands, this is how a group gets
        ! made: it takes every open tab under the workspace and groups them.
        case('group-all')
            block
                integer(int32) :: new_gid
                integer :: t
                character(len=:), allocatable :: root
                root = 'group'
                if (allocated(editor%workspace_path)) root = editor%workspace_path
                call group_create(editor, root, '', new_gid)
                do t = 1, size(editor%tabs)
                    if (editor%tabs(t)%group_id == 0) &
                        call group_add_member(editor, new_gid, t)
                end do
                call set_status_message('Grouped ' // &
                    trim(int_to_text(group_member_count(editor, new_gid))) // ' tabs')
            end block
        ! The keyboard route to what right-clicking a group entry offers.
        ! All three act on the group the active tab is in, which is the only
        ! group the keyboard can be said to be pointing at.
        case('group-edit', 'group-rename', 'group-dissolve')
            block
                integer(int32) :: cur_gid
                cur_gid = active_group_id(editor)
                if (cur_gid == 0) then
                    call set_status_message('Not inside a tab group')
                else if (trim(cmd_id) == 'group-edit') then
                    call start_group_edit(editor, cur_gid)
                else if (trim(cmd_id) == 'group-rename') then
                    call rename_group_prompt(editor, cur_gid)
                else
                    call dissolve_group_now(editor, cur_gid)
                end if
            end block

        case('group-leave')
            block
                integer :: t
                if (active_group_id(editor) /= 0) then
                    do t = 1, size(editor%tabs)
                        if (editor%tabs(t)%group_id == 0) then
                            call switch_to_tab_with_buffer(editor, t, buffer)
                            exit
                        end if
                    end do
                end if
            end block

        ! Panes and tabs. ctrl-w closes the TAB and alt-q closes the PANE;
        ! the single "Close Pane / Ctrl+W" entry that used to be here named
        ! one and did the other, and did neither because it had no case.
        case('terminal')
            call handle_key_command('f5', editor, buffer, should_quit)
        ! Deliberately NOT delegating to the ctrl-shift chords. Those only
        ! resize while the panel has focus, and reaching them from the palette
        ! means it usually does not -- they would navigate panes instead.
        case('terminal-taller')
            call palette_resize_terminal(editor, 'ctrl-shift-up')
        case('terminal-shorter')
            call palette_resize_terminal(editor, 'ctrl-shift-down')
        case('terminal-max')
            call palette_resize_terminal(editor, 'ctrl-shift-m')

        case('close-tab')
            call handle_key_command('ctrl-w', editor, buffer, should_quit)
        case('close-pane')
            call handle_key_command('alt-q', editor, buffer, should_quit)
        case('split-v')
            call handle_key_command('alt-v', editor, buffer, should_quit)
        case('split-h')
            call handle_key_command('alt-s', editor, buffer, should_quit)
        case('pane-left')
            call handle_key_command('alt-h', editor, buffer, should_quit)
        case('pane-right')
            call handle_key_command('alt-l', editor, buffer, should_quit)
        case('pane-up')
            call handle_key_command('alt-k', editor, buffer, should_quit)
        case('pane-down')
            call handle_key_command('alt-j', editor, buffer, should_quit)

        ! Search and navigation
        case('find')
            call handle_key_command('ctrl-f', editor, buffer, should_quit)
        case('replace')
            call handle_key_command('ctrl-r', editor, buffer, should_quit)
        case('goto-line')
            call handle_key_command('ctrl-g', editor, buffer, should_quit)
        case('jump-back')
            call handle_key_command('alt-comma', editor, buffer, should_quit)
        ! 'n' and 'N' navigate matches only while a search is live; without
        ! one they are ordinary letters and would be typed into the file.
        case('find-next')
            if (allocated(search_pattern)) then
                call handle_key_command('n', editor, buffer, should_quit)
            else
                call set_status_message('Find Next: no active search (Ctrl+F first)')
            end if
        case('find-prev')
            if (allocated(search_pattern)) then
                call handle_key_command('N', editor, buffer, should_quit)
            else
                call set_status_message('Find Previous: no active search (Ctrl+F first)')
            end if

        ! Language server
        case('goto-def')
            call handle_key_command('f12', editor, buffer, should_quit)
        case('find-refs')
            call handle_key_command('shift-f12', editor, buffer, should_quit)
        case('rename')
            call handle_key_command('f2', editor, buffer, should_quit)
        case('hover')
            call handle_key_command('ctrl-h', editor, buffer, should_quit)
        case('code-actions')
            call handle_key_command('f10', editor, buffer, should_quit)
        case('diagnostics')
            call handle_key_command('f8', editor, buffer, should_quit)
        case('goto-symbol')
            call handle_key_command('f4', editor, buffer, should_quit)

        ! Misc
        case('delete-line')
            call handle_key_command('ctrl-shift-k', editor, buffer, should_quit)
        case('help')
            call handle_key_command('ctrl-?', editor, buffer, should_quit)
        case('palette')
            ! Already in the palette; re-opening from inside it would nest.
            call set_status_message('Command Palette: already open (Ctrl+P)')

        case default
            ! Unknown command - show message
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('Unknown command: ' // trim(cmd_id) // repeat(' ', 20))
        end select
    end subroutine execute_palette_command

    ! Handle workspace symbols LSP response
    subroutine handle_workspace_symbols_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module
        use workspace_symbols_panel_module, only: workspace_symbol_t, set_workspace_symbols
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: symbol_obj, location_obj, range_obj, start_obj
        integer :: num_symbols, i
        type(workspace_symbol_t), allocatable :: symbols(:)
        character(len=:), allocatable :: name, container, uri
        real(8) :: line_num, char_num, kind_num

        if (.false.) print *, unused_request_id  ! Silence unused warning

        num_symbols = json_array_size(response%result)
        if (num_symbols == 0) return
        allocate(symbols(num_symbols))

        do i = 0, num_symbols - 1
            symbol_obj = json_get_array_element(response%result, i)

            ! Get name
            name = json_get_string(symbol_obj, 'name', '')
            if (allocated(name)) then
                symbols(i+1)%name = name
            end if

            ! Get kind (as number) and convert to string
            kind_num = json_get_number(symbol_obj, 'kind', 0.0d0)
            symbols(i+1)%kind_name = symbol_kind_to_string(int(kind_num))

            ! Get container name (optional)
            container = json_get_string(symbol_obj, 'containerName', '')
            if (allocated(container)) then
                symbols(i+1)%container_name = container
            end if

            ! Get location
            location_obj = json_get_object(symbol_obj, 'location')
            uri = json_get_string(location_obj, 'uri', '')
            if (allocated(uri) .and. len_trim(uri) > 0) then
                symbols(i+1)%file_uri = uri

                ! Get range -> start -> line/character
                range_obj = json_get_object(location_obj, 'range')
                start_obj = json_get_object(range_obj, 'start')
                line_num = json_get_number(start_obj, 'line', 0.0d0)
                char_num = json_get_number(start_obj, 'character', 0.0d0)
                symbols(i+1)%line = int(line_num)
                symbols(i+1)%column = int(char_num)
            end if
        end do

        ! Update the panel
        if (associated(saved_editor_for_callback)) then
            call set_workspace_symbols(saved_editor_for_callback%workspace_symbols_panel, symbols, num_symbols)
        end if

        if (allocated(symbols)) deallocate(symbols)
    end subroutine handle_workspace_symbols_response_wrapper

    ! Helper to convert LSP symbol kind number to string
    function symbol_kind_to_string(kind) result(kind_str)
        integer, intent(in) :: kind
        character(len=:), allocatable :: kind_str

        select case(kind)
        case(1); kind_str = "File"
        case(2); kind_str = "Module"
        case(3); kind_str = "Namespace"
        case(4); kind_str = "Package"
        case(5); kind_str = "Class"
        case(6); kind_str = "Method"
        case(7); kind_str = "Property"
        case(8); kind_str = "Field"
        case(9); kind_str = "Constructor"
        case(10); kind_str = "Enum"
        case(11); kind_str = "Interface"
        case(12); kind_str = "Function"
        case(13); kind_str = "Variable"
        case(14); kind_str = "Constant"
        case(15); kind_str = "String"
        case(16); kind_str = "Number"
        case(17); kind_str = "Boolean"
        case(18); kind_str = "Array"
        case default; kind_str = "Unknown"
        end select
    end function symbol_kind_to_string

    ! Navigate to a workspace symbol
    subroutine navigate_to_workspace_symbol(editor, buffer, symbol, should_quit)
        use workspace_symbols_panel_module, only: workspace_symbol_t
        use jump_stack_module, only: push_jump_location
        use editor_state_module, only: switch_to_tab_with_buffer, create_tab, sync_pane_to_editor, sync_editor_to_pane
        use text_buffer_module, only: buffer_load_file, copy_buffer
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        type(workspace_symbol_t), intent(in) :: symbol
        logical, intent(out) :: should_quit
        character(len=:), allocatable :: filepath
        integer :: i

        should_quit = .false.

        ! Convert file:// URI to filepath
        if (index(symbol%file_uri, "file://") == 1) then
            filepath = symbol%file_uri(8:)  ! Remove "file://"
        else
            filepath = symbol%file_uri
        end if

        ! Push current location to jump stack
        if (allocated(editor%filename)) then
            call push_jump_location(editor%jump_stack, editor%filename, &
                editor%cursors(editor%active_cursor)%line, &
                editor%cursors(editor%active_cursor)%column)
        end if

        ! FIRST: Check if symbol is in the currently active tab (just jump, no tab switch)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            if (allocated(editor%tabs(editor%active_tab_index)%filename)) then
                if (paths_match(editor%tabs(editor%active_tab_index)%filename, filepath)) then
                    ! Same file - just jump to the position (LSP is
                    ! 0-based; columns are UTF-16 units)
                    editor%cursors(editor%active_cursor)%line = symbol%line + 1
                    editor%cursors(editor%active_cursor)%column = &
                        char_col_from_lsp(buffer, symbol%line + 1, symbol%column)
                    editor%cursors(editor%active_cursor)%desired_column = &
                        editor%cursors(editor%active_cursor)%column
                    editor%viewport_line = max(1, symbol%line + 1 - editor%screen_rows / 2)
                    call sync_editor_to_pane(editor)
                    return
                end if
            end if
        end if

        ! SECOND: Check if file is open in another (inactive) tab
        do i = 1, size(editor%tabs)
            if (i == editor%active_tab_index) cycle  ! Skip active tab, already checked
            if (allocated(editor%tabs(i)%filename)) then
                if (paths_match(editor%tabs(i)%filename, filepath)) then
                    ! Save current buffer and switch to existing tab
                    call switch_to_tab_with_buffer(editor, i, buffer)
                    ! Jump to the symbol's position (LSP 0-based, UTF-16)
                    editor%cursors(editor%active_cursor)%line = symbol%line + 1
                    editor%cursors(editor%active_cursor)%column = &
                        char_col_from_lsp(buffer, symbol%line + 1, symbol%column)
                    editor%cursors(editor%active_cursor)%desired_column = &
                        editor%cursors(editor%active_cursor)%column
                    editor%viewport_line = max(1, symbol%line + 1 - editor%screen_rows / 2)
                    call sync_editor_to_pane(editor)
                    return
                end if
            end if
        end do

        ! File not open - create a new tab and load the file
        block
            integer :: status, new_tab_idx, old_tab_idx, old_pane_idx

            ! CRITICAL: Save current buffer to old tab BEFORE create_tab changes active_tab_index
            old_tab_idx = editor%active_tab_index
            if (old_tab_idx > 0 .and. old_tab_idx <= size(editor%tabs)) then
                old_pane_idx = editor%tabs(old_tab_idx)%active_pane_index
                if (allocated(editor%tabs(old_tab_idx)%panes) .and. &
                    old_pane_idx > 0 .and. old_pane_idx <= size(editor%tabs(old_tab_idx)%panes)) then
                    call copy_buffer(editor%tabs(old_tab_idx)%panes(old_pane_idx)%buffer, buffer)
                end if
                call copy_buffer(editor%tabs(old_tab_idx)%panes(active_pane_of(editor, old_tab_idx))%buffer, buffer)
            end if

            ! Without this, a refusal leaves new_tab_idx pointing at the last
            ! EXISTING tab and the load below overwrites its buffer.
            if (.not. can_create_tab(editor)) then
                call set_status_message('Too many tabs open: close one first')
                return
            end if
            call create_tab(editor, filepath)

            new_tab_idx = size(editor%tabs)  ! The tab we just created

            call buffer_load_file(editor%tabs(new_tab_idx)%panes(active_pane_of(editor, new_tab_idx))%buffer, filepath, status)

            if (status == 0) then
                ! File loaded successfully
                ! Copy buffer to the pane's buffer
                if (allocated(editor%tabs(new_tab_idx)%panes)) then
                end if

                ! Load the new tab's buffer into working buffer (create_tab already switched active_tab_index)
                call copy_buffer(buffer, editor%tabs(new_tab_idx)%panes(active_pane_of(editor, new_tab_idx))%buffer)

                ! Update editor%filename to the new tab's filename
                if (allocated(editor%filename)) deallocate(editor%filename)
                allocate(character(len=len(editor%tabs(new_tab_idx)%filename)) :: editor%filename)
                editor%filename = editor%tabs(new_tab_idx)%filename
                editor%modified = editor%tabs(new_tab_idx)%modified

                ! Sync the pane to editor state (this updates editor%cursors, etc.)
                call sync_pane_to_editor(editor, new_tab_idx, 1)

                ! Navigate to the symbol's position (LSP 0-based, UTF-16)
                editor%cursors(editor%active_cursor)%line = symbol%line + 1
                editor%cursors(editor%active_cursor)%column = &
                    char_col_from_lsp(buffer, symbol%line + 1, symbol%column)
                editor%cursors(editor%active_cursor)%desired_column = &
                    editor%cursors(editor%active_cursor)%column
                editor%viewport_line = max(1, symbol%line + 1 - editor%screen_rows / 2)

                ! Sync editor state back to pane
                call sync_editor_to_pane(editor)
            else
                ! File load failed - could show error message
                ! For now, just don't navigate
                continue
            end if
        end block
    end subroutine navigate_to_workspace_symbol

    ! Helper function to compare file paths (handles relative vs absolute)
    function paths_match(path1, path2) result(match)
        character(len=*), intent(in) :: path1, path2
        logical :: match
        character(len=:), allocatable :: p1, p2

        match = .false.

        ! Direct comparison first
        if (trim(path1) == trim(path2)) then
            match = .true.
            return
        end if

        ! Try comparing just the filenames (basename) if one is relative
        p1 = get_path_basename(path1)
        p2 = get_path_basename(path2)

        ! If basenames match and one path ends with the other, consider it a match
        if (trim(p1) == trim(p2)) then
            ! Check if one path is a suffix of the other
            if (index(path1, trim(path2)) > 0 .or. index(path2, trim(path1)) > 0) then
                match = .true.
                return
            end if
            ! Also match if the absolute path ends with the relative path
            if (len_trim(path1) > len_trim(path2)) then
                if (path1(len_trim(path1)-len_trim(path2)+1:) == trim(path2)) then
                    match = .true.
                    return
                end if
            else if (len_trim(path2) > len_trim(path1)) then
                if (path2(len_trim(path2)-len_trim(path1)+1:) == trim(path1)) then
                    match = .true.
                    return
                end if
            end if
        end if
    end function paths_match

    ! Get basename from a path
    function get_path_basename(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: basename
        integer :: i, last_slash

        last_slash = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/') then
                last_slash = i
                exit
            end if
        end do

        if (last_slash > 0 .and. last_slash < len_trim(path)) then
            basename = path(last_slash+1:len_trim(path))
        else
            basename = trim(path)
        end if
    end function get_path_basename

    ! ==================================================
    ! LSP Definition Response Handler
    ! ==================================================

    ! Wrapper callback for go to definition
    subroutine handle_definition_response_wrapper(unused_request_id, response)
        use lsp_protocol_module, only: lsp_message_t
        integer, intent(in) :: unused_request_id
        type(lsp_message_t), intent(in) :: response

        if (.false.) print *, unused_request_id  ! Silence unused warning

        ! Call actual handler with saved editor state
        if (associated(saved_editor_for_callback)) then
            call handle_definition_response_impl(saved_editor_for_callback, response)
        end if
    end subroutine handle_definition_response_wrapper

    ! Handle LSP textDocument/definition response
    subroutine handle_definition_response_impl(editor, response)
        use lsp_protocol_module, only: lsp_message_t
        use json_module, only: json_value_t, json_get_object, json_get_string, &
                               json_get_number, json_array_size, json_get_array_element, &
                               json_has_key, json_stringify
        use editor_state_module, only: switch_to_tab, sync_pane_to_editor, sync_editor_to_pane
        use text_buffer_module, only: buffer_load_file, copy_buffer
        use renderer_module, only: render_screen
        type(editor_state_t), intent(inout) :: editor
        type(lsp_message_t), intent(in) :: response
        type(json_value_t) :: location_obj, range_obj, start_obj
        character(len=:), allocatable :: uri, filepath
        real(8) :: line_real, col_real
        integer :: target_line, target_col, i, num_locations
        logical :: found_file

        ! Try to treat result as array first
        num_locations = json_array_size(response%result)

        if (num_locations > 0) then
            ! Array of locations - take first one
            location_obj = json_get_array_element(response%result, 0)
        else if (json_has_key(response%result, "uri")) then
            ! Single location object
            location_obj = response%result
        else
            ! No definition found
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('No definition found                           ')
            if (associated(saved_buffer_for_callback)) then
                call render_screen(saved_buffer_for_callback, editor)
            end if
            return
        end if

        ! Extract URI
        uri = json_get_string(location_obj, 'uri', '')
        if (len(uri) == 0) then
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('Invalid definition response                   ')
            if (associated(saved_buffer_for_callback)) then
                call render_screen(saved_buffer_for_callback, editor)
            end if
            return
        end if

        ! Convert URI to filepath (remove file:// prefix)
        if (len(uri) > 7 .and. uri(1:7) == 'file://') then
            filepath = uri(8:)
        else
            filepath = uri
        end if

        ! Get range
        range_obj = json_get_object(location_obj, 'range')
        start_obj = json_get_object(range_obj, 'start')

        line_real = json_get_number(start_obj, 'line', 0.0d0)
        col_real = json_get_number(start_obj, 'character', 0.0d0)

        ! Convert from 0-based LSP to 1-based editor coordinates. The
        ! column is refined to a char index per target buffer below (LSP
        ! sends UTF-16 code units).
        target_line = int(line_real) + 1
        target_col = int(col_real) + 1

        ! Check if the file is already open in a tab
        found_file = .false.
        do i = 1, size(editor%tabs)
            if (allocated(editor%tabs(i)%filename)) then
                ! Check for exact match or suffix match (handles relative vs absolute paths)
                if (trim(editor%tabs(i)%filename) == trim(filepath)) then
                    found_file = .true.
                else if (len_trim(filepath) > len_trim(editor%tabs(i)%filename)) then
                    ! Check if filepath ends with tab filename
                    if (filepath(len_trim(filepath)-len_trim(editor%tabs(i)%filename)+1:) == &
                        trim(editor%tabs(i)%filename)) then
                        found_file = .true.
                    end if
                else if (len_trim(editor%tabs(i)%filename) > len_trim(filepath)) then
                    ! Check if tab filename ends with filepath
                    if (editor%tabs(i)%filename(len_trim(editor%tabs(i)%filename)-len_trim(filepath)+1:) == &
                        trim(filepath)) then
                        found_file = .true.
                    end if
                end if

                if (found_file) then
                    ! Properly switch to this tab
                    call switch_to_tab(editor, i)
                    call sync_pane_to_editor(editor, i, editor%tabs(i)%active_pane_index)
                    exit
                end if
            end if
        end do

        ! If file not found in tabs, create a new tab and load it
        if (.not. found_file) then
            if (.not. can_create_tab(editor)) then
                call set_status_message('Too many tabs open: close one first')
                return
            end if
            call create_tab(editor, filepath)

            ! Load file content into the new tab's buffer
            block
                integer :: status, new_tab_idx

                new_tab_idx = size(editor%tabs)  ! The tab we just created

                call buffer_load_file(editor%tabs(new_tab_idx)%panes(active_pane_of(editor, new_tab_idx))%buffer, filepath, status)

                if (status == 0) then
                    ! File loaded successfully
                    ! Copy buffer to the pane's buffer
                    if (allocated(editor%tabs(new_tab_idx)%panes)) then
                    end if

                    ! Send LSP didOpen notification to all active servers for this tab
                    if (editor%tabs(new_tab_idx)%num_lsp_servers > 0) then
                        block
                            use text_buffer_module, only: buffer_to_string
                            integer :: srv_i
                            do srv_i = 1, editor%tabs(new_tab_idx)%num_lsp_servers
                                call notify_file_opened(editor%lsp_manager, &
                                    editor%tabs(new_tab_idx)%lsp_server_indices(srv_i), &
                                    filepath, buffer_to_string(editor%tabs(new_tab_idx)%panes(active_pane_of(editor, &
                                        new_tab_idx))%buffer))
                            end do
                        end block
                    end if

                    ! Switch to the new tab
                    call switch_to_tab(editor, new_tab_idx)

                    ! Sync the pane to editor state (this updates editor%cursors, etc.)
                    call sync_pane_to_editor(editor, new_tab_idx, 1)

                    ! Navigate to the definition position
                    target_col = char_col_from_lsp(editor%tabs(new_tab_idx)%panes(active_pane_of(editor, new_tab_idx))%buffer, &
                        target_line, int(col_real))
                    editor%cursors(editor%active_cursor)%line = target_line
                    editor%cursors(editor%active_cursor)%column = target_col
                    editor%cursors(editor%active_cursor)%desired_column = target_col
                    editor%viewport_line = max(1, target_line - editor%screen_rows / 2)

                    ! Sync editor state back to pane
                    call sync_editor_to_pane(editor)

                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write('Jumped to definition in ' // trim(filepath) // '                          ')
                    if (associated(saved_buffer_for_callback)) then
                        call render_screen(saved_buffer_for_callback, editor)
                    end if
                else
                    ! File load failed
                    call terminal_move_cursor(editor%screen_rows, 1)
                    call terminal_write('Failed to load: ' // trim(filepath) // '                ')
                    if (associated(saved_buffer_for_callback)) then
                        call render_screen(saved_buffer_for_callback, editor)
                    end if
                end if
            end block
            return
        end if

        ! File already open in tabs - jump to the line and column
        target_col = char_col_from_lsp(editor%tabs(i)%panes(active_pane_of(editor, i))%buffer, target_line, int(col_real))
        editor%cursors(editor%active_cursor)%line = target_line
        editor%cursors(editor%active_cursor)%column = target_col
        editor%cursors(editor%active_cursor)%desired_column = target_col

        ! Center viewport on target
        editor%viewport_line = max(1, target_line - editor%screen_rows / 2)

        ! Sync cursor changes back to pane
        call sync_editor_to_pane(editor)

        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Jumped to definition                          ')
        if (associated(saved_buffer_for_callback)) then
            call render_screen(saved_buffer_for_callback, editor)
        end if
    end subroutine handle_definition_response_impl

end module command_handler_module
