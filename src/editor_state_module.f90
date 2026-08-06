module editor_state_module
    use iso_fortran_env, only: int32, int64
    use ai_state_module, only: ai_state_t
    use text_buffer_module
    use platform_module, only: canonical_path
    use lsp_server_manager_module, only: lsp_manager_t, init_lsp_manager, cleanup_lsp_manager, &
                                         get_or_start_server, process_server_messages, &
                                         start_lsp_for_file, start_all_lsp_servers_for_file, &
                                         get_server_with_capability, notify_file_opened, &
                                         notify_file_changed, notify_file_closed, &
                                         request_completion, request_hover
    use completion_popup_module, only: completion_popup_t, init_completion_popup, &
                                       cleanup_completion_popup
    use hover_tooltip_module, only: hover_tooltip_t, init_hover_tooltip, &
                                    cleanup_hover_tooltip
    use diagnostics_module, only: diagnostics_store_t, init_diagnostics_store, &
                                  cleanup_diagnostics_store
    use diagnostics_panel_module, only: diagnostics_panel_t, init_diagnostics_panel, &
                                       cleanup_diagnostics_panel
    use references_panel_module, only: references_panel_t, init_references_panel, &
                                      cleanup_references_panel
    use code_actions_panel_module, only: code_actions_panel_t, init_code_actions_panel, &
                                        cleanup_code_actions_panel
    use symbols_panel_module, only: symbols_panel_t, init_symbols_panel, &
                                     cleanup_symbols_panel
    use signature_tooltip_module, only: signature_tooltip_t, init_signature_tooltip, &
                                        cleanup_signature_tooltip
    use command_palette_module, only: command_palette_t, init_command_palette, &
                                       cleanup_command_palette
    use workspace_symbols_panel_module, only: workspace_symbols_panel_t, init_workspace_symbols_panel, &
                                               cleanup_workspace_symbols_panel
    use document_sync_module, only: document_sync_t, init_document_sync, &
                                    cleanup_document_sync
    use jump_stack_module, only: jump_stack_t, init_jump_stack, &
                                 cleanup_jump_stack
    use lsp_server_installer_panel_module, only: lsp_server_installer_panel_t, &
                                                  init_lsp_server_installer_panel, &
                                                  cleanup_lsp_server_installer_panel
    use terminal_panel_module, only: terminal_panel_t, &
        init_terminal_panel, cleanup_terminal_panel
    use ghost_text_module, only: ghost_text_t
    implicit none
    private

    public :: editor_state_t, cursor_t, pane_t, tab_t
    public :: init_editor, cleanup_editor
    public :: create_tab, can_create_tab, find_tab_by_id, save_tab_pane, active_pane_of
    public :: tab_group_t, group_create, group_dissolve, group_find
    public :: group_member_count, group_members, group_label
    public :: group_add_member, group_remove_member, active_group_id
    public :: prune_empty_groups, find_tab_by_path_public
    public :: reorder_tab, reorder_group_block, set_group_ordinal
    public :: bar_slot_to_index
    public :: tab_is_resident, hydrate_tab, defer_tab
    public :: switch_to_tab, &
        switch_to_tab_with_buffer, get_active_tab_index, close_tab
    public :: split_pane_vertical, split_pane_horizontal, close_pane, get_active_pane_indices
    public :: navigate_to_pane_left, navigate_to_pane_right, navigate_to_pane_up, navigate_to_pane_down
    public :: sync_pane_to_editor, sync_editor_to_pane, switch_to_pane, switch_to_pane_with_buffer
    public :: sync_buffer_to_all_instances
    public :: note_tab_saved, refresh_tab_modified
    public :: find_open_file

    ! Cursor position and selection
    ! Cursor type - positions are UTF-8 CHARACTER indices (not byte indices)
    !
    ! IMPORTANT: All column values are 1-based CHARACTER positions, NOT byte positions
    ! For example, in the string "├──", column=2 refers to the second character (─),
    ! even though that character starts at byte 4.
    type :: cursor_t
        integer(int32) :: line = 1             ! Line number (1-based)
        integer(int32) :: column = 1           ! UTF-8 character position (1-based), NOT byte index
        integer(int32) :: desired_column = 1   ! For vertical movement (character position)
        ! The same goal, in DISPLAY CELLS, which is what actually has to be
        ! preserved when moving between lines: a tab is one character but
        ! several cells, so equal character columns are not the same place on
        ! screen. Held lazily -- goal_for_column records which
        ! desired_column produced goal_display, so a horizontal move (which
        ! only updates desired_column) is detected and the goal recomputed.
        integer(int32) :: goal_display = -1
        integer(int32) :: goal_for_column = -1
        logical :: has_selection = .false.
        integer(int32) :: selection_start_line = 1
        integer(int32) :: selection_start_col = 1  ! UTF-8 character position
    end type cursor_t

    ! Pane - represents a view within a tab
    type :: pane_t
        ! Position within tab (0.0 to 1.0 normalized coordinates)
        real :: x_start = 0.0
        real :: y_start = 0.0
        real :: x_end = 1.0
        real :: y_end = 1.0

        ! Actual screen coordinates (calculated during render)
        integer :: screen_col = 1
        integer :: screen_row = 1
        integer :: screen_width = 80
        integer :: screen_height = 24

        ! Each pane has its own buffer and file
        type(buffer_t) :: buffer
        character(len=:), allocatable :: filename

        ! Independent view state
        integer(int32) :: viewport_line = 1
        integer(int32) :: viewport_column = 1
        type(cursor_t), allocatable :: cursors(:)
        integer(int32) :: active_cursor = 1

        ! State
        logical :: is_active = .false.
    end type pane_t

    ! Tab - represents a single file buffer with one or more panes
    !> A tab group: a sub-workspace holding some of the open tabs.
    !>
    !> Deliberately holds NO member list. Closing a tab compacts the tabs
    !> array and renumbers every index above it, so any stored membership
    !> would be stale one close later. Membership lives on the tab, and the
    !> member count is computed on demand -- a cached count is a label that
    !> can lie about what is in the group.
    type :: tab_group_t
        integer(int32) :: id = 0
        !> Where the group came from. An origin label, not a constraint: a
        !> file opened from anywhere joins the active group, so a group named
        !> src/ may legitimately hold docs/readme.md.
        character(len=:), allocatable :: dir_path
        character(len=:), allocatable :: label
        !> Which member to return to when the group is re-entered. A filename
        !> rather than an index, because indices renumber; non-authoritative,
        !> and falls back to the lowest ordinal if it dangles.
        character(len=:), allocatable :: last_active_member
    end type tab_group_t

    type :: tab_t
        character(len=:), allocatable :: filename
        ! No buffer here. A tab used to carry a shadow copy of whichever pane
        ! was last active, which meant every file was read from disk twice and
        ! every save path had to guess which copy was current. The panes own
        ! the text; ask active_pane_of which one holds it.

        ! Panes within this tab
        type(pane_t), allocatable :: panes(:)
        integer(int32) :: active_pane_index = 1

        logical :: modified = .false.
        ! Signature of the text as it was last loaded or saved, so `modified`
        ! can be recomputed from the CONTENT instead of latching on the first
        ! edit and staying on. -1 means not established yet, in which case the
        ! flag is left alone rather than guessed at.
        integer(int64) :: saved_sig = -1_int64
        logical :: is_orphan = .false.  ! True if file is outside workspace (uses absolute path)

        ! Identity that survives the array being rebuilt. Closing a tab
        ! compacts `tabs`, so every index above the removed one shifts down and
        ! a saved index silently comes to mean a different tab -- the same
        ! failure that let a closed pane's text be written over its sibling's
        ! file. Anything that needs to refer to "this tab" across a close must
        ! hold the id, not the position.
        integer(int32) :: tab_id = 0

        !> 0 means ungrouped. Authoritative: the group does not keep a list.
        integer(int32) :: group_id = 0
        !> Position within the group, 1-based, compacted when a member leaves.
        integer(int32) :: group_ordinal = 0

        ! Bumped on every edit to this tab. An async request (LSP completion,
        ! and later a model completion) captures this when it is sent; if the
        ! value has moved by the time the reply lands, the reply was computed
        ! against a document that no longer exists and must be dropped.
        ! Cursor position and typed prefix are not sufficient on their own --
        ! undo/redo can restore both while changing everything else.
        integer(int64) :: doc_revision = 0

        ! LSP support - multiple servers per file
        integer, allocatable :: lsp_server_indices(:)  ! Indices of LSP servers handling this file
        integer :: num_lsp_servers = 0                 ! Number of active LSP servers
        type(document_sync_t) :: document_sync         ! Document synchronization for LSP
    end type tab_t

    ! Main editor state
    type :: editor_state_t
        ! Legacy fields (kept for backward compatibility during transition)
        type(cursor_t), allocatable :: cursors(:)
        integer(int32) :: active_cursor = 1
        integer(int32) :: viewport_line = 1
        integer(int32) :: viewport_column = 1
        integer(int32) :: screen_rows = 24
        integer(int32) :: screen_cols = 80
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: workspace_path  ! Current working directory
        logical :: modified = .false.
        logical :: fuss_mode_active = .false.  ! Toggle for file tree mode
        logical :: fuss_hints_expanded = .false.  ! Toggle for expanded fuss legend

        ! Tab management
        type(tab_t), allocatable :: tabs(:)
        integer(int32) :: active_tab_index = 1
        ! Monotonic, never reused, so a stale id refers to nothing rather than
        ! to whatever later took that slot.
        integer(int32) :: next_tab_id = 1

        ! Tab groups. init_editor must allocate this to size 0, exactly as it
        ! does tabs, because every consumer calls size() on it unguarded.
        type(tab_group_t), allocatable :: groups(:)
        integer(int32) :: next_group_id = 1
        ! Was 10. A tab group opened from a directory routinely exceeds that,
        ! and the cap is a real limit rather than a suggestion -- create_tab
        ! refuses past it and the caller must cope.
        integer(int32) :: max_tabs = 512

        ! LSP support
        type(lsp_manager_t) :: lsp_manager
        type(completion_popup_t) :: completion_popup
        type(ghost_text_t) :: ghost
        ! Model-backed completion; inert until ai.enabled is turned on
        type(ai_state_t) :: ai              ! Inline shadow-text suggestion
        type(hover_tooltip_t) :: hover_tooltip
        type(diagnostics_store_t) :: diagnostics
        type(diagnostics_panel_t) :: diagnostics_panel
        type(references_panel_t) :: references_panel
        type(code_actions_panel_t) :: code_actions_panel
        type(symbols_panel_t) :: symbols_panel
        type(signature_tooltip_t) :: signature_tooltip
        type(command_palette_t) :: command_palette
        type(workspace_symbols_panel_t) :: workspace_symbols_panel
        type(lsp_server_installer_panel_t) :: lsp_installer_panel

        ! Navigation
        type(jump_stack_t) :: jump_stack

        ! Integrated terminal
        type(terminal_panel_t) :: terminal_panel

        ! Timed status message (persists for ~2 seconds)
        character(len=256) :: timed_message = ''
        integer(int64) :: timed_message_ms = 0  ! timestamp when set
    end type editor_state_t

contains

    subroutine init_editor(editor)
        type(editor_state_t), intent(out) :: editor

        ! Initialize with single cursor
        allocate(editor%cursors(1))
        editor%cursors(1)%line = 1
        editor%cursors(1)%column = 1
        editor%cursors(1)%desired_column = 1
        editor%active_cursor = 1

        ! Default screen size (will be updated by terminal query)
        editor%screen_rows = 24
        editor%screen_cols = 80
        editor%viewport_line = 1
        editor%viewport_column = 1

        editor%modified = .false.

        ! Initialize tabs array (empty initially)
        allocate(editor%tabs(0))
        editor%active_tab_index = 0
        ! Same treatment for groups: everything calls size() on it unguarded.
        allocate(editor%groups(0))

        ! Initialize LSP manager
        call init_lsp_manager(editor%lsp_manager)

        ! Initialize completion popup
        call init_completion_popup(editor%completion_popup)

        ! Initialize hover tooltip
        call init_hover_tooltip(editor%hover_tooltip)

        ! Initialize diagnostics store
        call init_diagnostics_store(editor%diagnostics)

        ! Initialize diagnostics panel
        call init_diagnostics_panel(editor%diagnostics_panel)

        ! Initialize references panel
        call init_references_panel(editor%references_panel)

        ! Initialize code actions menu
        call init_code_actions_panel(editor%code_actions_panel)

        ! Initialize symbols panel
        call init_symbols_panel(editor%symbols_panel)

        ! Initialize signature tooltip
        call init_signature_tooltip(editor%signature_tooltip)

        ! Initialize command palette
        call init_command_palette(editor%command_palette)

        ! Initialize workspace symbols panel
        call init_workspace_symbols_panel(editor%workspace_symbols_panel)

        ! Initialize jump stack
        call init_jump_stack(editor%jump_stack)

        ! Initialize integrated terminal
        call init_terminal_panel(editor%terminal_panel)

        ! Initialize LSP server installer panel
        call init_lsp_server_installer_panel(editor%lsp_installer_panel)
    end subroutine init_editor

    subroutine cleanup_editor(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: i

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        if (allocated(editor%filename)) deallocate(editor%filename)
        if (allocated(editor%workspace_path)) deallocate(editor%workspace_path)

        ! Cleanup tabs
        if (allocated(editor%tabs)) then
            do i = 1, size(editor%tabs)
                call cleanup_tab(editor%tabs(i))
            end do
            deallocate(editor%tabs)
        end if

        ! Cleanup LSP manager
        call cleanup_lsp_manager(editor%lsp_manager)

        ! Cleanup completion popup
        call cleanup_completion_popup(editor%completion_popup)

        ! Cleanup hover tooltip
        call cleanup_hover_tooltip(editor%hover_tooltip)

        ! Cleanup diagnostics store
        call cleanup_diagnostics_store(editor%diagnostics)

        ! Cleanup diagnostics panel
        call cleanup_diagnostics_panel(editor%diagnostics_panel)

        ! Cleanup references panel
        call cleanup_references_panel(editor%references_panel)

        ! Cleanup code actions menu
        call cleanup_code_actions_panel(editor%code_actions_panel)

        ! Cleanup symbols panel
        call cleanup_symbols_panel(editor%symbols_panel)

        ! Cleanup signature tooltip
        call cleanup_signature_tooltip(editor%signature_tooltip)

        ! Cleanup command palette
        call cleanup_command_palette(editor%command_palette)

        ! Cleanup workspace symbols panel
        call cleanup_workspace_symbols_panel(editor%workspace_symbols_panel)

        ! Cleanup jump stack
        call cleanup_jump_stack(editor%jump_stack)

        ! Cleanup integrated terminal
        call cleanup_terminal_panel(editor%terminal_panel)

        ! Cleanup LSP server installer panel
        call cleanup_lsp_server_installer_panel(editor%lsp_installer_panel)
    end subroutine cleanup_editor

    ! Helper to cleanup a single tab
    subroutine cleanup_tab(tab)
        use text_buffer_module, only: cleanup_buffer
        type(tab_t), intent(inout) :: tab
        integer :: i

        if (allocated(tab%filename)) deallocate(tab%filename)

        ! Cleanup panes
        if (allocated(tab%panes)) then
            do i = 1, size(tab%panes)
                if (allocated(tab%panes(i)%cursors)) deallocate(tab%panes(i)%cursors)
            end do
            deallocate(tab%panes)
        end if


        ! Cleanup LSP server indices
        if (allocated(tab%lsp_server_indices)) deallocate(tab%lsp_server_indices)
        tab%num_lsp_servers = 0

        ! Cleanup document sync
        call cleanup_document_sync(tab%document_sync)
    end subroutine cleanup_tab

    ! Create a new tab with the given filename
    !> Index of the pane whose buffer is a tab's live text, or 0.
    !>
    !> A tab used to carry its own buffer as well, a shadow of whichever pane
    !> was last active. Two copies of one document meant every open path loaded
    !> the file twice, and every save path had to guess which copy was current
    !> -- which is how a pane's text came to be written under the tab's name.
    !> The panes own the text now; this says which one to ask.
    function active_pane_of(editor, tab_idx) result(p)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: tab_idx
        integer :: p

        p = 0
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        p = editor%tabs(tab_idx)%active_pane_index
        if (p < 1 .or. p > size(editor%tabs(tab_idx)%panes)) p = 1
    end function active_pane_of

    !> Write one pane's text to that pane's own file.
    !>
    !> The one place a non-active document gets written. Before this there were
    !> three, and each picked its buffer and its filename from different
    !> places: the workspace-switch prompt saved pane 1's text under the TAB's
    !> name, the close-tab prompt saved the WORKING buffer under the tab's
    !> name, and quit-time save-all did the same. Any of them wrote the wrong
    !> bytes to a real file whenever a tab held panes on two different files,
    !> which alt-v and alt-s on a tree row make routine.
    !>
    !> status: 0 written, -3 buffer never loaded (nothing written), -1 I/O
    !> failure, 1 nothing to write to.
    subroutine save_tab_pane(editor, tab_idx, pane_idx, status)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx
        integer, intent(out) :: status

        status = 1
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        ! Never write a tab whose file was never read: buffer_save_file
        ! refuses an unallocated buffer, but refusing here as well means the
        ! caller gets a distinguishable status rather than an I/O error.
        if (.not. tab_is_resident(editor, tab_idx)) then
            status = 1
            return
        end if

        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            ! A pane with no name of its own has never been given a file --
            ! writing it to the tab's name is exactly the confusion this
            ! routine exists to end.
            if (.not. allocated(pane%filename)) return
            if (len_trim(pane%filename) == 0) return
            call buffer_save_file(pane%buffer, pane%filename, status)
        end associate
    end subroutine save_tab_pane


    ! ---- tab groups ------------------------------------------------------

    !> Is this tab's text actually in memory?
    !>
    !> Derived from the allocation, never from a flag. A flag can drift; the
    !> allocation is the thing every save path ultimately asks about, and
    !> buffer_save_file refuses an unallocated buffer outright. Making the
    !> question structural means a stale answer cannot exist.
    function tab_is_resident(editor, tab_idx) result(res)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: tab_idx
        logical :: res
        integer :: p

        res = .false.
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        do p = 1, size(editor%tabs(tab_idx)%panes)
            if (.not. allocated(editor%tabs(tab_idx)%panes(p)%buffer%data)) return
        end do
        res = .true.
    end function tab_is_resident

    !> Read a deferred tab's file in, and tell the language server about it.
    subroutine hydrate_tab(editor, tab_idx, status)
        use text_buffer_module, only: buffer_load_file, buffer_to_string, init_buffer
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx
        integer, intent(out) :: status
        integer :: p, srv

        status = 0
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (tab_is_resident(editor, tab_idx)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        do p = 1, size(editor%tabs(tab_idx)%panes)
            if (allocated(editor%tabs(tab_idx)%panes(p)%buffer%data)) cycle
            call init_buffer(editor%tabs(tab_idx)%panes(p)%buffer)
            if (allocated(editor%tabs(tab_idx)%panes(p)%filename)) then
                call buffer_load_file(editor%tabs(tab_idx)%panes(p)%buffer, &
                                      editor%tabs(tab_idx)%panes(p)%filename, status)
            end if
        end do

        ! The server was never told about this file, because it was never
        ! opened. Do it now, with the text we just read.
        if (status == 0 .and. editor%tabs(tab_idx)%num_lsp_servers > 0) then
            do srv = 1, editor%tabs(tab_idx)%num_lsp_servers
                call notify_file_opened(editor%lsp_manager, &
                    editor%tabs(tab_idx)%lsp_server_indices(srv), &
                    editor%tabs(tab_idx)%filename, &
                    buffer_to_string(editor%tabs(tab_idx)%panes(1)%buffer))
            end do
        end if
    end subroutine hydrate_tab

    !> Create a tab whose file is not read until it is first looked at.
    subroutine defer_tab(editor, tab_idx)
        use text_buffer_module, only: cleanup_buffer
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx
        integer :: p

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        do p = 1, size(editor%tabs(tab_idx)%panes)
            call cleanup_buffer(editor%tabs(tab_idx)%panes(p)%buffer)
        end do
        ! No cached flag: tab_is_resident asks the allocation, so there is
        ! nothing here that could disagree with reality.
        ! A deferred tab has been read from nowhere, so it cannot be modified.
        editor%tabs(tab_idx)%modified = .false.
    end subroutine defer_tab

    !> Index of the tab holding `path`, or 0. Paths are canonicalised where
    !> they are stored, so this is a plain comparison.
    !> Where a file is already open, if it is: its tab, and the pane within
    !> that tab showing it.
    !>
    !> Searches PANES as well as tab names, because a file opened into a split
    !> lives in a pane of a tab named after something else -- and it is open
    !> either way, which is what the caller is asking about.
    !>
    !> Canonical paths throughout. Matching on basename would be enough to
    !> find the wrong file the moment two directories both hold a main.c,
    !> which is the ordinary case in a C project.
    subroutine find_open_file(editor, path, tab_idx, pane_idx)
        type(editor_state_t), intent(in) :: editor
        character(len=*), intent(in) :: path
        integer, intent(out) :: tab_idx, pane_idx
        character(len=:), allocatable :: canon
        integer :: i, p

        tab_idx = 0
        pane_idx = 0
        if (len_trim(path) == 0) return
        canon = canonical_path(path)

        ! Panes first: a tab whose NAME matches may still be showing something
        ! else in its active pane, and the pane is the thing to reveal.
        do i = 1, size(editor%tabs)
            if (.not. allocated(editor%tabs(i)%panes)) cycle
            do p = 1, size(editor%tabs(i)%panes)
                if (.not. allocated(editor%tabs(i)%panes(p)%filename)) cycle
                if (canonical_path(editor%tabs(i)%panes(p)%filename) == canon) then
                    tab_idx = i
                    pane_idx = p
                    return
                end if
            end do
        end do

        do i = 1, size(editor%tabs)
            if (.not. allocated(editor%tabs(i)%filename)) cycle
            if (canonical_path(editor%tabs(i)%filename) == canon) then
                tab_idx = i
                pane_idx = 0
                return
            end if
        end do
    end subroutine find_open_file

    function find_tab_by_path_public(editor, path) result(idx)
        type(editor_state_t), intent(in) :: editor
        character(len=*), intent(in) :: path
        integer :: idx, i
        character(len=:), allocatable :: canon

        idx = 0
        canon = canonical_path(path)
        do i = 1, size(editor%tabs)
            if (.not. allocated(editor%tabs(i)%filename)) cycle
            if (editor%tabs(i)%filename == canon) then
                idx = i
                return
            end if
        end do
    end function find_tab_by_path_public

    !> Index into groups(:) for `gid`, or 0 if there is no such group.
    function group_find(editor, gid) result(gidx)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        integer :: gidx, i

        gidx = 0
        if (gid <= 0) return
        do i = 1, size(editor%groups)
            if (editor%groups(i)%id == gid) then
                gidx = i
                return
            end if
        end do
    end function group_find

    !> How many tabs belong to `gid`. Computed, never stored: a cached count
    !> is a label that can disagree with the tabs actually open.
    function group_member_count(editor, gid) result(n)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        integer :: n, i

        n = 0
        if (gid <= 0) return
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id == gid) n = n + 1
        end do
    end function group_member_count

    !> Tab indices belonging to `gid`, in ordinal order.
    subroutine group_members(editor, gid, idx)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        integer, allocatable, intent(out) :: idx(:)
        integer :: i, j, n, best, best_ord

        n = group_member_count(editor, gid)
        allocate(idx(n))
        if (n == 0) return

        ! Selection sort by ordinal: n is small and this avoids assuming the
        ! ordinals are contiguous, which they are not mid-removal.
        do j = 1, n
            best = 0
            best_ord = huge(1)
            do i = 1, size(editor%tabs)
                if (editor%tabs(i)%group_id /= gid) cycle
                if (any(idx(1:j-1) == i)) cycle
                if (editor%tabs(i)%group_ordinal < best_ord) then
                    best_ord = editor%tabs(i)%group_ordinal
                    best = i
                end if
            end do
            if (best == 0) exit
            idx(j) = best
        end do
    end subroutine group_members

    !> The bar label: "src/ (4)". The count comes from the tabs, so it cannot
    !> drift from what is open.
    function group_label(editor, gid) result(text)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        character(len=:), allocatable :: text
        character(len=16) :: cnt
        integer :: gidx

        text = ''
        gidx = group_find(editor, gid)
        if (gidx == 0) return
        write(cnt, '(i0)') group_member_count(editor, gid)
        if (allocated(editor%groups(gidx)%label)) then
            text = editor%groups(gidx)%label // ' (' // trim(cnt) // ')'
        else
            text = '(' // trim(cnt) // ')'
        end if
    end function group_label

    !> Create an empty group named after `dir_path`. Returns its id.
    subroutine group_create(editor, dir_path, name, gid)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: dir_path
        character(len=*), intent(in) :: name
        integer(int32), intent(out) :: gid
        type(tab_group_t), allocatable :: tmp(:)
        integer :: n, i

        n = size(editor%groups)
        allocate(tmp(n + 1))
        do i = 1, n
            if (allocated(editor%groups(i)%dir_path)) &
                call move_alloc(editor%groups(i)%dir_path, tmp(i)%dir_path)
            if (allocated(editor%groups(i)%label)) &
                call move_alloc(editor%groups(i)%label, tmp(i)%label)
            if (allocated(editor%groups(i)%last_active_member)) &
                call move_alloc(editor%groups(i)%last_active_member, &
                                tmp(i)%last_active_member)
            tmp(i)%id = editor%groups(i)%id
        end do

        gid = editor%next_group_id
        editor%next_group_id = editor%next_group_id + 1
        tmp(n + 1)%id = gid
        tmp(n + 1)%dir_path = canonical_path(dir_path)
        if (len_trim(name) > 0) then
            tmp(n + 1)%label = trim(name)
        else
            tmp(n + 1)%label = basename_of_path(canonical_path(dir_path)) // '/'
        end if
        call move_alloc(tmp, editor%groups)
    end subroutine group_create

    !> Remove a group from the array. Members are expected to be gone already.
    subroutine group_dissolve(editor, gid)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        type(tab_group_t), allocatable :: tmp(:)
        integer :: n, i, j, gidx

        gidx = group_find(editor, gid)
        if (gidx == 0) return

        ! Any tab still pointing here becomes ungrouped rather than orphaned
        ! against an id that no longer resolves.
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id == gid) then
                editor%tabs(i)%group_id = 0
                editor%tabs(i)%group_ordinal = 0
            end if
        end do

        n = size(editor%groups)
        allocate(tmp(n - 1))
        j = 0
        do i = 1, n
            if (i == gidx) cycle
            j = j + 1
            if (allocated(editor%groups(i)%dir_path)) &
                call move_alloc(editor%groups(i)%dir_path, tmp(j)%dir_path)
            if (allocated(editor%groups(i)%label)) &
                call move_alloc(editor%groups(i)%label, tmp(j)%label)
            if (allocated(editor%groups(i)%last_active_member)) &
                call move_alloc(editor%groups(i)%last_active_member, &
                                tmp(j)%last_active_member)
            tmp(j)%id = editor%groups(i)%id
        end do
        call move_alloc(tmp, editor%groups)
    end subroutine group_dissolve

    !> Put a tab in a group, at the end of its order.
    subroutine group_add_member(editor, gid, tab_idx)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        integer, intent(in) :: tab_idx
        integer :: i, max_ord

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (group_find(editor, gid) == 0) return

        ! A tab belongs to exactly one group; leaving the old one first keeps
        ! the ordinals of both consistent.
        if (editor%tabs(tab_idx)%group_id /= 0) call group_remove_member(editor, tab_idx)

        max_ord = 0
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id == gid) &
                max_ord = max(max_ord, editor%tabs(i)%group_ordinal)
        end do
        editor%tabs(tab_idx)%group_id = gid
        editor%tabs(tab_idx)%group_ordinal = max_ord + 1
    end subroutine group_add_member

    !> Take a tab out of its group, compacting the remaining ordinals and
    !> dissolving the group if that was the last member.
    subroutine group_remove_member(editor, tab_idx)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx
        integer(int32) :: gid
        integer :: i, gone_ord

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        gid = editor%tabs(tab_idx)%group_id
        if (gid == 0) return

        gone_ord = editor%tabs(tab_idx)%group_ordinal
        editor%tabs(tab_idx)%group_id = 0
        editor%tabs(tab_idx)%group_ordinal = 0

        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%group_id == gid .and. &
                editor%tabs(i)%group_ordinal > gone_ord) then
                editor%tabs(i)%group_ordinal = editor%tabs(i)%group_ordinal - 1
            end if
        end do

        if (group_member_count(editor, gid) == 0) call group_dissolve(editor, gid)
    end subroutine group_remove_member

    !> The group the active tab belongs to, or 0.
    !>
    !> Derived, never stored: active_tab_index is assigned raw in the main loop
    !> and the workspace restore, so a cached value would drift out of step.
    function active_group_id(editor) result(gid)
        type(editor_state_t), intent(in) :: editor
        integer(int32) :: gid

        gid = 0
        if (editor%active_tab_index < 1) return
        if (editor%active_tab_index > size(editor%tabs)) return
        gid = editor%tabs(editor%active_tab_index)%group_id
    end function active_group_id

    !> Drop groups that have no members. Restore skips tabs whose file is
    !> gone, so a group can come back empty.
    subroutine prune_empty_groups(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: i
        integer(int32) :: gid

        i = 1
        do while (i <= size(editor%groups))
            gid = editor%groups(i)%id
            if (group_member_count(editor, gid) == 0) then
                call group_dissolve(editor, gid)
            else
                i = i + 1
            end if
        end do
    end subroutine prune_empty_groups

    function basename_of_path(path) result(base)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: base
        integer :: p

        p = index(path, '/', back=.true.)
        if (p > 0 .and. p < len(path)) then
            base = path(p+1:)
        else
            base = path
        end if
    end function basename_of_path

    !> Move a tab from `src` to `dst`, transferring ownership of everything
    !> allocatable rather than copying it.
    !>
    !> The array grows and shrinks by building a new array and assigning each
    !> element across, and intrinsic derived-type assignment deep-copies every
    !> allocatable component -- for a tab that means its whole text, every
    !> pane's text, and the pending LSP payload. Inserting into an N-tab array
    !> therefore copied N documents, making open-a-directory quadratic in the
    !> number of files. move_alloc hands over the pointers instead.
    !>
    !> `src` is left deallocated, which is what makes it a move: the two must
    !> never both own the same buffer.
    subroutine move_tab(dst, src)
        type(tab_t), intent(inout) :: dst, src
        integer :: i

        if (allocated(src%filename)) call move_alloc(src%filename, dst%filename)
        if (allocated(src%panes)) then
            ! Moving the array moves each pane's buffer, filename and cursors
            ! with it: they are components of the elements being transferred.
            call move_alloc(src%panes, dst%panes)
        end if
        dst%active_pane_index = src%active_pane_index

        if (allocated(src%lsp_server_indices)) &
            call move_alloc(src%lsp_server_indices, dst%lsp_server_indices)
        dst%num_lsp_servers = src%num_lsp_servers

        if (allocated(src%document_sync%uri)) &
            call move_alloc(src%document_sync%uri, dst%document_sync%uri)
        if (allocated(src%document_sync%pending_content)) &
            call move_alloc(src%document_sync%pending_content, &
                            dst%document_sync%pending_content)
        dst%document_sync%version             = src%document_sync%version
        dst%document_sync%last_change_time    = src%document_sync%last_change_time
        dst%document_sync%sync_delay          = src%document_sync%sync_delay
        dst%document_sync%has_pending_changes = src%document_sync%has_pending_changes
        dst%document_sync%server_index        = src%document_sync%server_index

        dst%modified      = src%modified
        dst%is_orphan     = src%is_orphan
        dst%doc_revision  = src%doc_revision
        dst%tab_id        = src%tab_id
        ! Group membership travels with the tab. Omitting these silently
        ! emptied every group the first time any tab was closed.
        dst%group_id      = src%group_id
        dst%group_ordinal = src%group_ordinal

        i = 0   ! silence unused-variable warnings on compilers that want it
    end subroutine move_tab

    !> Move the tab at `from_idx` to `to_idx`, shifting everything between.
    !>
    !> An insertion, not a swap: dragging a tab three places to the right
    !> should leave the two it passed in their original relative order, which
    !> is what every editor does and what a swap would not give.
    !>
    !> move_tab leaves its source deallocated, so the shuffle goes through a
    !> temporary and never has two tabs owning the same buffer. Assigning
    !> tab_t directly instead would deep-copy every pane's text.
    subroutine reorder_tab(editor, from_idx, to_idx)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: from_idx, to_idx
        type(tab_t) :: held
        integer :: i, n, was_active

        n = size(editor%tabs)
        if (from_idx < 1 .or. from_idx > n) return
        if (to_idx < 1 .or. to_idx > n) return
        if (from_idx == to_idx) return

        was_active = editor%active_tab_index

        call move_tab(held, editor%tabs(from_idx))
        if (to_idx > from_idx) then
            do i = from_idx, to_idx - 1
                call move_tab(editor%tabs(i), editor%tabs(i + 1))
            end do
        else
            do i = from_idx, to_idx + 1, -1
                call move_tab(editor%tabs(i), editor%tabs(i - 1))
            end do
        end if
        call move_tab(editor%tabs(to_idx), held)

        ! The active tab is a POSITION, so it has to follow the shuffle or the
        ! editor ends up showing a different file than the one it names.
        editor%active_tab_index = shifted_index(was_active, from_idx, to_idx)
    end subroutine reorder_tab

    !> Where an index lands after a tab moves from `from_idx` to `to_idx`.
    pure function shifted_index(idx, from_idx, to_idx) result(moved)
        integer, intent(in) :: idx, from_idx, to_idx
        integer :: moved

        moved = idx
        if (idx == from_idx) then
            moved = to_idx
        else if (to_idx > from_idx) then
            if (idx > from_idx .and. idx <= to_idx) moved = idx - 1
        else
            if (idx >= to_idx .and. idx < from_idx) moved = idx + 1
        end if
    end function shifted_index

    !> Turn a tab-bar SLOT into an index into the tabs array.
    !>
    !> The two are different units and conflating them was a bug. A slot counts
    !> a group as ONE entry however many members it has; the array counts every
    !> member. They agree only while nothing multi-member sits before the
    !> destination, which is why dragging a group one slot at a time worked and
    !> a long drag landed short by exactly the members hidden behind the groups
    !> it passed -- the drop went somewhere the preview had never shown.
    !>
    !> Pass the dragged entry so it can be left out of the count: it is being
    !> moved, so the tabs it occupies do not stand between it and its
    !> destination. Give `gid` for a group, or `tab_idx` for a lone tab.
    !>
    !> Dropping on a slot to the RIGHT lands after the entry sitting there,
    !> which is what makes a rightward drag advance at all; dropping to the
    !> LEFT lands before it. That asymmetry is the ordinary drag-and-drop
    !> convention and it is what reorder_tab already does for indices.
    integer function bar_slot_to_index(editor, gid, tab_idx, to_slot)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: gid
        integer, intent(in) :: tab_idx, to_slot
        integer(int32), allocatable :: slot_gid(:)
        integer, allocatable :: slot_tabs(:)
        integer(int32) :: g
        integer :: i, n, slot, n_slots, from_slot, last, tally
        logical :: seen

        n = size(editor%tabs)
        bar_slot_to_index = max(1, to_slot)
        if (n == 0) return

        allocate(slot_gid(n), slot_tabs(n))
        slot_gid = 0
        slot_tabs = 0
        n_slots = 0
        from_slot = 0
        do i = 1, n
            g = editor%tabs(i)%group_id
            if (g /= 0) then
                seen = .false.
                do slot = 1, n_slots
                    if (slot_gid(slot) == g) then
                        seen = .true.
                        slot_tabs(slot) = slot_tabs(slot) + 1
                        exit
                    end if
                end do
                if (seen) cycle
                n_slots = n_slots + 1
                slot_gid(n_slots) = g
                slot_tabs(n_slots) = 1
                if (g == gid) from_slot = n_slots
            else
                n_slots = n_slots + 1
                slot_gid(n_slots) = 0
                slot_tabs(n_slots) = 1
                if (gid == 0 .and. i == tab_idx) from_slot = n_slots
            end if
        end do

        last = max(1, min(to_slot, n_slots))
        if (from_slot > 0 .and. last < from_slot) last = last - 1

        tally = 0
        do slot = 1, last
            if (slot == from_slot) cycle
            tally = tally + slot_tabs(slot)
        end do
        bar_slot_to_index = tally + 1
        deallocate(slot_gid, slot_tabs)
    end function bar_slot_to_index

    !> Move a whole run of tabs -- a group's members -- to start at `to_idx`.
    !>
    !> A group is ONE entry on the tab bar but several entries in the array,
    !> and its position on the bar is wherever its first member sits. Moving
    !> the entry therefore means moving every member, keeping their order.
    !> The members need not be contiguous to begin with; they are afterwards,
    !> which is the only arrangement the bar can draw.
    !>
    !> Works out the whole destination order FIRST and then sorts the array
    !> into it, rather than moving members one at a time to their slots. The
    !> obvious version is wrong: a member moved up from below the block drags
    !> the already-placed ones back down with it, so a two-member group landed
    !> interleaved with the tabs it was supposed to have passed.
    subroutine reorder_group_block(editor, gid, to_idx)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: gid
        integer, intent(in) :: to_idx
        integer, allocatable :: members(:), want(:)
        integer :: i, n, total, dest, cursor, pos, cur

        total = size(editor%tabs)
        n = group_member_count(editor, gid)
        if (n == 0 .or. total == 0) return
        dest = max(1, min(to_idx, total - n + 1))

        ! The order we want, as tab_ids. Ids, not indices: the sort below
        ! moves tabs, and an index recorded now would mean something else one
        ! move later.
        call group_members(editor, gid, members)
        allocate(want(total))
        cursor = 0
        pos = 0
        do i = 1, total
            if (editor%tabs(i)%group_id == gid) cycle
            pos = pos + 1
            if (pos == dest) then
                do cursor = 1, n
                    want(pos + cursor - 1) = editor%tabs(members(cursor))%tab_id
                end do
                pos = pos + n
            end if
            want(pos) = editor%tabs(i)%tab_id
        end do
        ! The block goes last when dest is past every non-member.
        if (pos < total) then
            do cursor = 1, n
                want(pos + cursor) = editor%tabs(members(cursor))%tab_id
            end do
        end if

        ! Place position by position. Everything below `i` is already correct,
        ! so the tab being fetched is always at or after i and the move can
        ! only shift tabs that are not yet placed.
        do i = 1, total
            cur = index_of_tab_id(editor, want(i))
            if (cur > i) call reorder_tab(editor, cur, i)
        end do
    end subroutine reorder_group_block

    !> Where the tab carrying `id` currently sits, or 0.
    function index_of_tab_id(editor, id) result(idx)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: id
        integer :: idx, i

        idx = 0
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%tab_id == id) then
                idx = i
                return
            end if
        end do
    end function index_of_tab_id

    !> Put `tab_idx` at position `pos` within its group, 1-based.
    !>
    !> Row 2's order is the ordinals, not the tabs array, so reordering inside
    !> a group touches no arrays at all -- and must not, or moving a file
    !> within a group would drag the group's neighbours around row 1.
    !>
    !> `pos` counts in the FINAL list, the one the user is looking at. Writing
    !> it as "skip the moved member, insert at pos" is off by one whenever the
    !> member is moving right, because the slot it vacated is still being
    !> counted; building the final order and numbering it is not.
    subroutine set_group_ordinal(editor, tab_idx, pos)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pos
        integer, allocatable :: members(:), final(:)
        integer(int32) :: gid
        integer :: i, n, slot, k

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        gid = editor%tabs(tab_idx)%group_id
        if (gid == 0) return

        call group_members(editor, gid, members)
        n = size(members)
        if (n == 0) return
        slot = max(1, min(pos, n))

        allocate(final(n))
        k = 0
        do i = 1, n
            if (k + 1 == slot) then
                k = k + 1
                final(k) = tab_idx
            end if
            if (members(i) == tab_idx) cycle
            k = k + 1
            if (k <= n) final(k) = members(i)
        end do
        if (k < n) final(n) = tab_idx

        do i = 1, n
            editor%tabs(final(i))%group_ordinal = int(i, int32)
        end do
    end subroutine set_group_ordinal

    !> Index of the tab carrying `id`, or 0 if it is gone.
    function find_tab_by_id(editor, id) result(idx)
        type(editor_state_t), intent(in) :: editor
        integer(int32), intent(in) :: id
        integer :: idx, i

        idx = 0
        if (id <= 0) return
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%tab_id == id) then
                idx = i
                return
            end if
        end do
    end function find_tab_by_id

    !> Whether another tab can be opened. The one source of the cap policy --
    !> create_tab consults it, and callers that cannot usefully recover from a
    !> refusal check it first so they never reach the load that would target
    !> the wrong tab.
    function can_create_tab(editor) result(res)
        type(editor_state_t), intent(in) :: editor
        logical :: res
        res = size(editor%tabs) < editor%max_tabs
    end function can_create_tab

    !> Open `filename` in a new tab.
    !>
    !> `ok` reports whether a tab was actually created. It is not decoration:
    !> this used to fail silently at the tab cap, and callers went on to load
    !> the new file's text into `tabs(active_tab_index)%buffer` -- the tab that
    !> was already active, still carrying the previous file's name. Ctrl-S then
    !> wrote the new file's content over the old file's path. Every caller must
    !> check.
    subroutine create_tab(editor, filename, ok)
        use text_buffer_module, only: init_buffer
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: ok
        type(tab_t), allocatable :: temp_tabs(:)
        integer :: n_tabs, new_index, i
        character(len=:), allocatable :: canon

        if (present(ok)) ok = .false.
        if (.not. can_create_tab(editor)) return
        n_tabs = size(editor%tabs)

        ! Resize tabs array
        allocate(temp_tabs(n_tabs + 1))
        if (n_tabs > 0) then
            ! move, not copy: intrinsic assignment here would duplicate every
            ! open document on every new tab
            do i = 1, n_tabs
                call move_tab(temp_tabs(i), editor%tabs(i))
            end do
        end if

        ! Initialize new tab
        new_index = n_tabs + 1
        ! Normalise here, at the one place a tab's name is established. Doing
        ! it in the comparator instead would put it on the every-keystroke
        ! path in sync_buffer_to_all_instances.
        canon = canonical_path(filename)
        allocate(character(len=len(canon)) :: temp_tabs(new_index)%filename)
        temp_tabs(new_index)%filename = canon

        ! Create default pane (full screen)
        allocate(temp_tabs(new_index)%panes(1))
        temp_tabs(new_index)%panes(1)%x_start = 0.0
        temp_tabs(new_index)%panes(1)%y_start = 0.0
        temp_tabs(new_index)%panes(1)%x_end = 1.0
        temp_tabs(new_index)%panes(1)%y_end = 1.0
        temp_tabs(new_index)%panes(1)%viewport_line = 1
        temp_tabs(new_index)%panes(1)%viewport_column = 1
        temp_tabs(new_index)%panes(1)%is_active = .true.

        ! Initialize screen coordinates for the default pane
        ! These will be updated during rendering, but set reasonable defaults
        temp_tabs(new_index)%panes(1)%screen_col = 1
        temp_tabs(new_index)%panes(1)%screen_row = 2  ! After tab bar
        temp_tabs(new_index)%panes(1)%screen_width = 80  ! Default width
        temp_tabs(new_index)%panes(1)%screen_height = 22  ! Default height (24 - 2)

        ! Initialize cursor in the default pane
        allocate(temp_tabs(new_index)%panes(1)%cursors(1))
        temp_tabs(new_index)%panes(1)%cursors(1)%line = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%column = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%desired_column = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%has_selection = .false.
        temp_tabs(new_index)%panes(1)%active_cursor = 1

        ! The pane owns the text. There is no tab-level copy to seed it from.
        call init_buffer(temp_tabs(new_index)%panes(1)%buffer)
        allocate(character(len=len(canon)) :: temp_tabs(new_index)%panes(1)%filename)
        temp_tabs(new_index)%panes(1)%filename = canon

        temp_tabs(new_index)%active_pane_index = 1
        temp_tabs(new_index)%modified = .false.

        ! Start ALL LSP servers for this file (multi-server support)
        call start_all_lsp_servers_for_file(editor%lsp_manager, filename, &
                                           temp_tabs(new_index)%lsp_server_indices, &
                                           temp_tabs(new_index)%num_lsp_servers)

        ! Initialize document sync for LSP if we have servers
        if (temp_tabs(new_index)%num_lsp_servers > 0) then
            block
                character(len=:), allocatable :: file_uri
                file_uri = 'file://' // trim(filename)
                ! Use first server for document sync (primary server)
                call init_document_sync(temp_tabs(new_index)%document_sync, &
                                      file_uri, temp_tabs(new_index)%lsp_server_indices(1))
            end block
        end if

        ! Replace tabs array
        call move_alloc(temp_tabs, editor%tabs)
        editor%active_tab_index = new_index

        ! Load the new pane's cursor/viewport/filename into the editor
        ! globals. Without this the previously active tab's state leaks
        ! into the new tab: the next sync_editor_to_pane stamps the old
        ! cursor into the new pane and the viewport scrolls a short
        ! buffer completely off screen.
        editor%tabs(new_index)%tab_id = editor%next_tab_id
        editor%next_tab_id = editor%next_tab_id + 1

        call sync_pane_to_editor(editor, new_index, 1)
        editor%modified = .false.
        if (present(ok)) ok = .true.
    end subroutine create_tab

    ! Switch to a specific tab index (1-based)
    subroutine switch_to_tab(editor, tab_index)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: tab_index
        integer :: pane_idx

        if (tab_index < 1 .or. tab_index > size(editor%tabs)) return

        ! Save current tab state (if any)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            ! Save to active pane of current tab
            pane_idx = editor%tabs(editor%active_tab_index)%active_pane_index
            if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                pane_idx > 0 .and. pane_idx <= size(editor%tabs(editor%active_tab_index)%panes)) then
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%cursors = editor%cursors
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%active_cursor = editor%active_cursor
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%viewport_line = editor%viewport_line
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%viewport_column = editor%viewport_column
            end if
            ! Deliberately does NOT touch the tab's modified flag. There is
            ! no buffer here to compare against, and the editor-level latch
            ! this used to copy is not maintained by editing -- copying it
            ! silently cleared the mark on a file with unsaved changes.
        end if

        ! Load new tab state
        editor%active_tab_index = tab_index

        ! Load from active pane of new tab (clamps cursors to the
        ! tab's buffer so persisted positions can't point past EOF)
        pane_idx = editor%tabs(tab_index)%active_pane_index
        call sync_pane_to_editor(editor, tab_index, pane_idx)

        ! sync_pane_to_editor has just set editor%filename from the ACTIVE
        ! PANE, which is the file whose text is in the working buffer. This
        ! used to overwrite it with the tab's name -- and since Ctrl-S writes
        ! the working buffer to editor%filename, switching into a tab whose
        ! active pane holds a different file (alt-v/alt-s split a second file
        ! in) then saved that pane's text over the tab's file. Fall back to the
        ! tab only when the pane has no name of its own.
        if (.not. allocated(editor%filename)) then
            if (allocated(editor%tabs(tab_index)%filename)) &
                editor%filename = editor%tabs(tab_index)%filename
        end if
        editor%modified = editor%tabs(tab_index)%modified
    end subroutine switch_to_tab

    ! Switch to a tab with buffer synchronization
    subroutine switch_to_tab_with_buffer(editor, tab_index, buffer)
        use text_buffer_module, only: copy_buffer
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: tab_index
        type(buffer_t), intent(inout) :: buffer
        integer :: pane_idx

        if (tab_index < 1 .or. tab_index > size(editor%tabs)) return

        ! Save current buffer to current tab's active pane (if any).
        !
        ! Never into a tab whose file was never read: the working buffer does
        ! not belong to it, and copying anything in would ALLOCATE its buffer
        ! and so make it look resident -- after which the real file is never
        ! read and the fabricated content is what gets saved.
        if (editor%active_tab_index > 0 .and. &
            editor%active_tab_index <= size(editor%tabs) .and. &
            tab_is_resident(editor, int(editor%active_tab_index))) then
            ! Save to active pane of current tab
            pane_idx = editor%tabs(editor%active_tab_index)%active_pane_index
            if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                pane_idx > 0 .and. pane_idx <= size(editor%tabs(editor%active_tab_index)%panes)) then
                ! Save buffer to pane's buffer
                call copy_buffer(editor%tabs(editor%active_tab_index)%panes(pane_idx)%buffer, buffer)

                ! Sync buffer to all other instances of this file
                if (allocated(editor%tabs(editor%active_tab_index)%panes(pane_idx)%filename)) then
                    call sync_buffer_to_all_instances(editor, &
                        editor%tabs(editor%active_tab_index)%panes(pane_idx)%filename, buffer)
                end if

                ! Save cursor and viewport state
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%cursors = editor%cursors
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%active_cursor = editor%active_cursor
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%viewport_line = editor%viewport_line
                editor%tabs(editor%active_tab_index)%panes(pane_idx)%viewport_column = editor%viewport_column
            end if
            ! Whether the tab being LEFT is dirty is a fact about its text.
            !
            ! This used to copy editor%modified, an editor-level latch that no
            ! ordinary edit ever sets -- so switching away from a file with
            ! unsaved changes cleared its asterisk and the change looked
            ! saved. Nothing was ever written and the text stayed in the tab's
            ! buffer, but a mark that cannot be trusted in that direction is
            ! worse than no mark.
            call refresh_tab_modified(editor, int(editor%active_tab_index), buffer)
        end if

        ! Switch to new tab
        editor%active_tab_index = tab_index

        ! Load new tab's active pane buffer
        ! Read the file now if it was deferred. Everything below this point
        ! assumes the pane holds text.
        block
            integer :: hydrate_status
            call hydrate_tab(editor, tab_index, hydrate_status)
        end block

        pane_idx = editor%tabs(tab_index)%active_pane_index
        if (allocated(editor%tabs(tab_index)%panes) .and. &
            pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_index)%panes)) then
            call copy_buffer(buffer, editor%tabs(tab_index)%panes(pane_idx)%buffer)
        end if

        ! Load from active pane of new tab (clamps cursors to the
        ! tab's buffer so persisted positions can't point past EOF)
        pane_idx = editor%tabs(tab_index)%active_pane_index
        call sync_pane_to_editor(editor, tab_index, pane_idx)

        ! sync_pane_to_editor has just set editor%filename from the ACTIVE
        ! PANE, which is the file whose text is in the working buffer. This
        ! used to overwrite it with the tab's name -- and since Ctrl-S writes
        ! the working buffer to editor%filename, switching into a tab whose
        ! active pane holds a different file (alt-v/alt-s split a second file
        ! in) then saved that pane's text over the tab's file. Fall back to the
        ! tab only when the pane has no name of its own.
        if (.not. allocated(editor%filename)) then
            if (allocated(editor%tabs(tab_index)%filename)) &
                editor%filename = editor%tabs(tab_index)%filename
        end if
        ! Arriving: the tab owns the answer, and the other two copies of it
        ! follow. buffer%modified in particular, because the main loop copies
        ! it straight back onto the tab every turn -- leave it holding the
        ! previous tab's claim and the new tab inherits it a moment later.
        editor%modified = editor%tabs(tab_index)%modified
        buffer%modified = editor%tabs(tab_index)%modified
    end subroutine switch_to_tab_with_buffer

    ! Get the active tab index
    function get_active_tab_index(editor) result(index)
        type(editor_state_t), intent(in) :: editor
        integer(int32) :: index
        index = editor%active_tab_index
    end function get_active_tab_index

    ! Close a tab
    subroutine close_tab(editor, tab_index)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: tab_index
        type(tab_t), allocatable :: temp_tabs(:)
        integer :: n_tabs, i, j
        integer(int32) :: survivor_id

        n_tabs = size(editor%tabs)
        if (tab_index < 1 .or. tab_index > n_tabs) return

        ! Leave the group before the array is rebuilt, so the remaining
        ! ordinals are compacted against the right membership. A group whose
        ! last member goes is dissolved here.
        call group_remove_member(editor, tab_index)

        ! Cleanup the tab being closed
        call cleanup_tab(editor%tabs(tab_index))

        if (n_tabs == 1) then
            ! Last tab - just deallocate the array
            deallocate(editor%tabs)
            allocate(editor%tabs(0))
            editor%active_tab_index = 0
            return
        end if

        ! Decide WHICH TAB should end up active before the array is rebuilt,
        ! and remember it by id. Compaction shifts every index above the
        ! removed one down, so an index chosen now would name a different tab
        ! afterwards -- the failure that let a closed pane's text be saved over
        ! its sibling's file.
        survivor_id = 0
        if (editor%active_tab_index /= tab_index) then
            ! Not closing the active tab: it simply stays active.
            if (editor%active_tab_index >= 1 .and. &
                editor%active_tab_index <= n_tabs) &
                survivor_id = editor%tabs(editor%active_tab_index)%tab_id
        else if (tab_index < n_tabs) then
            survivor_id = editor%tabs(tab_index + 1)%tab_id   ! the one to its right
        else if (tab_index > 1) then
            survivor_id = editor%tabs(tab_index - 1)%tab_id   ! closing the last: go left
        end if

        ! Create new array without this tab
        allocate(temp_tabs(n_tabs - 1))
        j = 1
        do i = 1, n_tabs
            if (i /= tab_index) then
                call move_tab(temp_tabs(j), editor%tabs(i))
                j = j + 1
            end if
        end do

        ! Replace tabs array
        call move_alloc(temp_tabs, editor%tabs)

        editor%active_tab_index = find_tab_by_id(editor, survivor_id)
        if (editor%active_tab_index < 1) &
            editor%active_tab_index = min(max(1, tab_index), size(editor%tabs))

        ! Switch to the new active tab
        if (editor%active_tab_index > 0) then
            call switch_to_tab(editor, editor%active_tab_index)
        end if
    end subroutine close_tab

    ! Split the active pane vertically
    subroutine split_pane_vertical(editor)
        type(editor_state_t), intent(inout) :: editor
        type(pane_t), allocatable :: temp_panes(:)
        integer :: tab_idx, pane_idx, n_panes, new_idx, i
        real :: mid_x

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        ! Get active pane
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        n_panes = size(editor%tabs(tab_idx)%panes)
        if (pane_idx < 1 .or. pane_idx > n_panes) return

        ! Check pane limit (maximum 6 panes per tab)
        if (n_panes >= 6) return

        ! A lone pane must fill the tab. A stale sub-full width (from a restored
        ! layout or a prior mis-normalization) would make the split fail the
        ! minimum-size check below, so normalize first.
        if (n_panes == 1) then
            editor%tabs(tab_idx)%panes(pane_idx)%x_start = 0.0
            editor%tabs(tab_idx)%panes(pane_idx)%x_end = 1.0
            editor%tabs(tab_idx)%panes(pane_idx)%y_start = 0.0
            editor%tabs(tab_idx)%panes(pane_idx)%y_end = 1.0
        end if

        ! Ensure active pane has cursors from editor state
        if (.not. allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors)) then
            allocate(editor%tabs(tab_idx)%panes(pane_idx)%cursors(size(editor%cursors)))
            editor%tabs(tab_idx)%panes(pane_idx)%cursors = editor%cursors
            editor%tabs(tab_idx)%panes(pane_idx)%active_cursor = editor%active_cursor
        end if

        ! Calculate split point
        associate(active_pane => editor%tabs(tab_idx)%panes(pane_idx))
            mid_x = (active_pane%x_start + active_pane%x_end) / 2.0

            ! Check minimum size (20 columns minimum)
            ! Assuming screen is ~80 cols, 20 cols = 0.25 of width
            if ((mid_x - active_pane%x_start) < 0.25 .or. &
                (active_pane%x_end - mid_x) < 0.25) then
                ! Too small to split
                return
            end if

            ! Resize array
            allocate(temp_panes(n_panes + 1))
            temp_panes(1:n_panes) = editor%tabs(tab_idx)%panes(1:n_panes)

            ! Setup new pane (right half)
            new_idx = n_panes + 1
            temp_panes(new_idx)%x_start = mid_x
            temp_panes(new_idx)%x_end = active_pane%x_end
            temp_panes(new_idx)%y_start = active_pane%y_start
            temp_panes(new_idx)%y_end = active_pane%y_end

            ! Copy viewport and cursor state
            temp_panes(new_idx)%viewport_line = active_pane%viewport_line
            temp_panes(new_idx)%viewport_column = active_pane%viewport_column
            if (allocated(active_pane%cursors)) then
                allocate(temp_panes(new_idx)%cursors(size(active_pane%cursors)))
                ! Deep copy each cursor to ensure all fields are copied
                do i = 1, size(active_pane%cursors)
                    temp_panes(new_idx)%cursors(i)%line = active_pane%cursors(i)%line
                    temp_panes(new_idx)%cursors(i)%column = active_pane%cursors(i)%column
                    temp_panes(new_idx)%cursors(i)%desired_column = active_pane%cursors(i)%desired_column
                    temp_panes(new_idx)%cursors(i)%has_selection = active_pane%cursors(i)%has_selection
                    temp_panes(new_idx)%cursors(i)%selection_start_line = active_pane%cursors(i)%selection_start_line
                    temp_panes(new_idx)%cursors(i)%selection_start_col = active_pane%cursors(i)%selection_start_col
                end do
                temp_panes(new_idx)%active_cursor = active_pane%active_cursor
            else
                ! Initialize cursor if not already allocated
                allocate(temp_panes(new_idx)%cursors(1))
                temp_panes(new_idx)%cursors(1)%line = 1
                temp_panes(new_idx)%cursors(1)%column = 1
                temp_panes(new_idx)%cursors(1)%desired_column = 1
                temp_panes(new_idx)%cursors(1)%has_selection = .false.
                temp_panes(new_idx)%active_cursor = 1
            end if

            ! Initialize and copy buffer from active pane
            call init_buffer(temp_panes(new_idx)%buffer)
            ! Copy from active pane's buffer - make sure it's initialized
            call copy_buffer(temp_panes(new_idx)%buffer, active_pane%buffer)
            if (allocated(active_pane%filename)) then
                temp_panes(new_idx)%filename = active_pane%filename
            end if

            temp_panes(new_idx)%is_active = .false.

            ! Update active pane (left half)
            temp_panes(pane_idx)%x_end = mid_x

            ! Replace panes array
            call move_alloc(temp_panes, editor%tabs(tab_idx)%panes)

            ! Clear all is_active flags first
            do i = 1, size(editor%tabs(tab_idx)%panes)
                editor%tabs(tab_idx)%panes(i)%is_active = .false.
            end do

            ! Set new pane as active
            editor%tabs(tab_idx)%panes(new_idx)%is_active = .true.
            editor%tabs(tab_idx)%active_pane_index = new_idx

            ! Sync new pane to editor state
            call sync_pane_to_editor(editor, tab_idx, new_idx)
        end associate
    end subroutine split_pane_vertical

    ! Split the active pane horizontally
    subroutine split_pane_horizontal(editor)
        type(editor_state_t), intent(inout) :: editor
        type(pane_t), allocatable :: temp_panes(:)
        integer :: tab_idx, pane_idx, n_panes, new_idx, i
        real :: mid_y

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        ! Get active pane
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        n_panes = size(editor%tabs(tab_idx)%panes)
        if (pane_idx < 1 .or. pane_idx > n_panes) return

        ! Check pane limit (maximum 6 panes per tab)
        if (n_panes >= 6) return

        ! A lone pane must fill the tab. A stale sub-full width (from a restored
        ! layout or a prior mis-normalization) would make the split fail the
        ! minimum-size check below, so normalize first.
        if (n_panes == 1) then
            editor%tabs(tab_idx)%panes(pane_idx)%x_start = 0.0
            editor%tabs(tab_idx)%panes(pane_idx)%x_end = 1.0
            editor%tabs(tab_idx)%panes(pane_idx)%y_start = 0.0
            editor%tabs(tab_idx)%panes(pane_idx)%y_end = 1.0
        end if

        ! Ensure active pane has cursors from editor state
        if (.not. allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors)) then
            allocate(editor%tabs(tab_idx)%panes(pane_idx)%cursors(size(editor%cursors)))
            editor%tabs(tab_idx)%panes(pane_idx)%cursors = editor%cursors
            editor%tabs(tab_idx)%panes(pane_idx)%active_cursor = editor%active_cursor
        end if

        ! Calculate split point
        associate(active_pane => editor%tabs(tab_idx)%panes(pane_idx))
            mid_y = (active_pane%y_start + active_pane%y_end) / 2.0

            ! Check minimum size (5 rows minimum)
            ! Assuming screen is ~24 rows, 5 rows = 0.21 of height
            if ((mid_y - active_pane%y_start) < 0.21 .or. &
                (active_pane%y_end - mid_y) < 0.21) then
                ! Too small to split
                return
            end if

            ! Resize array
            allocate(temp_panes(n_panes + 1))
            temp_panes(1:n_panes) = editor%tabs(tab_idx)%panes(1:n_panes)

            ! Setup new pane (bottom half)
            new_idx = n_panes + 1
            temp_panes(new_idx)%x_start = active_pane%x_start
            temp_panes(new_idx)%x_end = active_pane%x_end
            temp_panes(new_idx)%y_start = mid_y
            temp_panes(new_idx)%y_end = active_pane%y_end

            ! Copy viewport and cursor state
            temp_panes(new_idx)%viewport_line = active_pane%viewport_line
            temp_panes(new_idx)%viewport_column = active_pane%viewport_column
            if (allocated(active_pane%cursors)) then
                allocate(temp_panes(new_idx)%cursors(size(active_pane%cursors)))
                ! Deep copy each cursor to ensure all fields are copied
                do i = 1, size(active_pane%cursors)
                    temp_panes(new_idx)%cursors(i)%line = active_pane%cursors(i)%line
                    temp_panes(new_idx)%cursors(i)%column = active_pane%cursors(i)%column
                    temp_panes(new_idx)%cursors(i)%desired_column = active_pane%cursors(i)%desired_column
                    temp_panes(new_idx)%cursors(i)%has_selection = active_pane%cursors(i)%has_selection
                    temp_panes(new_idx)%cursors(i)%selection_start_line = active_pane%cursors(i)%selection_start_line
                    temp_panes(new_idx)%cursors(i)%selection_start_col = active_pane%cursors(i)%selection_start_col
                end do
                temp_panes(new_idx)%active_cursor = active_pane%active_cursor
            else
                ! Initialize cursor if not already allocated
                allocate(temp_panes(new_idx)%cursors(1))
                temp_panes(new_idx)%cursors(1)%line = 1
                temp_panes(new_idx)%cursors(1)%column = 1
                temp_panes(new_idx)%cursors(1)%desired_column = 1
                temp_panes(new_idx)%cursors(1)%has_selection = .false.
                temp_panes(new_idx)%active_cursor = 1
            end if

            ! Initialize and copy buffer from active pane
            call init_buffer(temp_panes(new_idx)%buffer)
            ! Copy from active pane's buffer - make sure it's initialized
            call copy_buffer(temp_panes(new_idx)%buffer, active_pane%buffer)
            if (allocated(active_pane%filename)) then
                temp_panes(new_idx)%filename = active_pane%filename
            end if

            temp_panes(new_idx)%is_active = .false.

            ! Update active pane (top half)
            temp_panes(pane_idx)%y_end = mid_y

            ! Replace panes array
            call move_alloc(temp_panes, editor%tabs(tab_idx)%panes)

            ! Clear all is_active flags first
            do i = 1, size(editor%tabs(tab_idx)%panes)
                editor%tabs(tab_idx)%panes(i)%is_active = .false.
            end do

            ! Set new pane as active
            editor%tabs(tab_idx)%panes(new_idx)%is_active = .true.
            editor%tabs(tab_idx)%active_pane_index = new_idx

            ! Sync new pane to editor state
            call sync_pane_to_editor(editor, tab_idx, new_idx)
        end associate
    end subroutine split_pane_horizontal

    ! Close the active pane
    subroutine close_pane(editor)
        type(editor_state_t), intent(inout) :: editor
        type(pane_t), allocatable :: temp_panes(:)
        integer :: tab_idx, pane_idx, n_panes, i, j

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        ! Get active pane
        pane_idx = editor%tabs(tab_idx)%active_pane_index
        n_panes = size(editor%tabs(tab_idx)%panes)
        if (pane_idx < 1 .or. pane_idx > n_panes) return

        ! If only one pane, check if it's the last tab
        if (n_panes == 1) then
            ! If this is the last tab, create an UNTITLED.txt tab instead of closing
            if (size(editor%tabs) == 1) then
                ! Create a new untitled tab
                call create_untitled_tab(editor)
                return
            else
                ! Multiple tabs exist, close this tab normally
                call close_tab(editor, tab_idx)
                return
            end if
        end if

        ! Remove the pane
        allocate(temp_panes(n_panes - 1))
        j = 1
        do i = 1, n_panes
            if (i /= pane_idx) then
                temp_panes(j) = editor%tabs(tab_idx)%panes(i)
                j = j + 1
            else
                ! Clean up the pane being removed
                if (allocated(editor%tabs(tab_idx)%panes(i)%cursors)) then
                    deallocate(editor%tabs(tab_idx)%panes(i)%cursors)
                end if
            end if
        end do

        ! Replace panes array
        call move_alloc(temp_panes, editor%tabs(tab_idx)%panes)

        ! Recalculate layout for remaining panes
        call recalculate_pane_layout(editor%tabs(tab_idx)%panes)

        ! Determine new active pane index
        if (pane_idx > size(editor%tabs(tab_idx)%panes)) then
            ! Was the last pane, activate the new last pane
            editor%tabs(tab_idx)%active_pane_index = size(editor%tabs(tab_idx)%panes)
        else if (pane_idx > 1) then
            ! Activate the previous pane
            editor%tabs(tab_idx)%active_pane_index = pane_idx - 1
        else
            ! Was the first pane, activate what is now the first pane
            editor%tabs(tab_idx)%active_pane_index = 1
        end if

        ! Clear all is_active flags first
        do i = 1, size(editor%tabs(tab_idx)%panes)
            editor%tabs(tab_idx)%panes(i)%is_active = .false.
        end do

        ! Set new active pane
        editor%tabs(tab_idx)%panes(editor%tabs(tab_idx)%active_pane_index)%is_active = .true.

        ! Sync to editor state
        call sync_pane_to_editor(editor, tab_idx, editor%tabs(tab_idx)%active_pane_index)
    end subroutine close_pane

    ! Get the active pane indices
    subroutine get_active_pane_indices(editor, tab_idx, pane_idx)
        type(editor_state_t), intent(in) :: editor
        integer, intent(out) :: tab_idx, pane_idx

        tab_idx = editor%active_tab_index
        pane_idx = -1

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) then
            tab_idx = -1
            return
        end if

        if (.not. allocated(editor%tabs(tab_idx)%panes)) then
            tab_idx = -1
            return
        end if

        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) then
            pane_idx = -1
        end if
    end subroutine get_active_pane_indices

    ! Helper to sync pane state to editor
    subroutine sync_pane_to_editor(editor, tab_idx, pane_idx)
        use text_buffer_module, only: buffer_get_line, buffer_get_line_count
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx
        integer :: i, line_count
        character(len=:), allocatable :: line

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            ! Copy pane state to editor
            if (allocated(editor%cursors)) deallocate(editor%cursors)
            if (allocated(pane%cursors) .and. size(pane%cursors) > 0) then
                allocate(editor%cursors(size(pane%cursors)))
                editor%cursors = pane%cursors
                editor%active_cursor = min(pane%active_cursor, size(pane%cursors))
                if (editor%active_cursor < 1) editor%active_cursor = 1
            else
                ! Initialize with single cursor if pane has no cursors
                allocate(editor%cursors(1))
                editor%cursors(1)%line = 1
                editor%cursors(1)%column = 1
                editor%cursors(1)%desired_column = 1
                editor%cursors(1)%has_selection = .false.
                editor%cursors(1)%selection_start_line = 1
                editor%cursors(1)%selection_start_col = 1
                editor%active_cursor = 1
            end if

            ! Validate cursor positions are within buffer bounds
            ! Clamp against the pane being synced. Clamping against a
            ! tab-level copy put every cursor of a pane holding a different
            ! file at the wrong limit.
            line_count = buffer_get_line_count( &
                editor%tabs(tab_idx)%panes(pane_idx)%buffer)
            if (line_count > 0 .and. allocated(editor%cursors)) then
                do i = 1, size(editor%cursors)
                    ! Clamp line to valid range
                    if (editor%cursors(i)%line > line_count) then
                        editor%cursors(i)%line = line_count
                    end if
                    if (editor%cursors(i)%line < 1) then
                        editor%cursors(i)%line = 1
                    end if

                    ! Clamp column to valid range for the line
                    line = buffer_get_line( &
                        editor%tabs(tab_idx)%panes(pane_idx)%buffer, editor%cursors(i)%line)
                    if (editor%cursors(i)%column > len(line) + 1) then
                        editor%cursors(i)%column = len(line) + 1
                    end if
                    if (editor%cursors(i)%column < 1) then
                        editor%cursors(i)%column = 1
                    end if
                    editor%cursors(i)%desired_column = editor%cursors(i)%column

                    if (allocated(line)) deallocate(line)
                end do
            end if
            editor%viewport_line = pane%viewport_line
            editor%viewport_column = pane%viewport_column

            ! Sync filename from pane to editor
            if (allocated(editor%filename)) deallocate(editor%filename)
            if (allocated(pane%filename)) then
                allocate(character(len=len(pane%filename)) :: editor%filename)
                editor%filename = pane%filename
            end if
        end associate
    end subroutine sync_pane_to_editor

    ! Helper to sync editor state back to the active pane
    subroutine sync_editor_to_pane(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, pane_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            ! Copy editor state back to pane
            if (allocated(pane%cursors)) deallocate(pane%cursors)
            if (allocated(editor%cursors) .and. size(editor%cursors) > 0) then
                allocate(pane%cursors(size(editor%cursors)))
                pane%cursors = editor%cursors
                pane%active_cursor = min(editor%active_cursor, size(editor%cursors))
                if (pane%active_cursor < 1) pane%active_cursor = 1
            else
                ! Should not happen, but ensure we have at least one cursor
                allocate(pane%cursors(1))
                pane%cursors(1)%line = 1
                pane%cursors(1)%column = 1
                pane%cursors(1)%desired_column = 1
                pane%cursors(1)%has_selection = .false.
                pane%cursors(1)%selection_start_line = 1
                pane%cursors(1)%selection_start_col = 1
                pane%active_cursor = 1
            end if
            pane%viewport_line = editor%viewport_line
            pane%viewport_column = editor%viewport_column
        end associate
    end subroutine sync_editor_to_pane

    ! Navigate to pane on the left
    subroutine navigate_to_pane_left(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, current_idx, i
        real :: current_x, best_x
        integer :: best_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        current_idx = editor%tabs(tab_idx)%active_pane_index
        if (current_idx < 1 .or. current_idx > size(editor%tabs(tab_idx)%panes)) return

        current_x = (editor%tabs(tab_idx)%panes(current_idx)%x_start + &
                     editor%tabs(tab_idx)%panes(current_idx)%x_end) / 2.0

        best_idx = -1
        best_x = -1.0

        ! Find the nearest pane to the left
        do i = 1, size(editor%tabs(tab_idx)%panes)
            if (i /= current_idx) then
                ! Check if pane is to the left
                if (editor%tabs(tab_idx)%panes(i)%x_end <= current_x) then
                    if (best_idx == -1 .or. editor%tabs(tab_idx)%panes(i)%x_end > best_x) then
                        best_idx = i
                        best_x = editor%tabs(tab_idx)%panes(i)%x_end
                    end if
                end if
            end if
        end do

        if (best_idx > 0) then
            call switch_to_pane(editor, tab_idx, best_idx)
        end if
    end subroutine navigate_to_pane_left

    ! Navigate to pane on the right
    subroutine navigate_to_pane_right(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, current_idx, i
        real :: current_x, best_x
        integer :: best_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        current_idx = editor%tabs(tab_idx)%active_pane_index
        if (current_idx < 1 .or. current_idx > size(editor%tabs(tab_idx)%panes)) return

        current_x = (editor%tabs(tab_idx)%panes(current_idx)%x_start + &
                     editor%tabs(tab_idx)%panes(current_idx)%x_end) / 2.0

        best_idx = -1
        best_x = 2.0  ! Start with value beyond max

        ! Find the nearest pane to the right
        do i = 1, size(editor%tabs(tab_idx)%panes)
            if (i /= current_idx) then
                ! Check if pane is to the right
                if (editor%tabs(tab_idx)%panes(i)%x_start >= current_x) then
                    if (best_idx == -1 .or. editor%tabs(tab_idx)%panes(i)%x_start < best_x) then
                        best_idx = i
                        best_x = editor%tabs(tab_idx)%panes(i)%x_start
                    end if
                end if
            end if
        end do

        if (best_idx > 0) then
            call switch_to_pane(editor, tab_idx, best_idx)
        end if
    end subroutine navigate_to_pane_right

    ! Navigate to pane above
    subroutine navigate_to_pane_up(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, current_idx, i
        real :: current_y, best_y
        integer :: best_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        current_idx = editor%tabs(tab_idx)%active_pane_index
        if (current_idx < 1 .or. current_idx > size(editor%tabs(tab_idx)%panes)) return

        current_y = (editor%tabs(tab_idx)%panes(current_idx)%y_start + &
                     editor%tabs(tab_idx)%panes(current_idx)%y_end) / 2.0

        best_idx = -1
        best_y = -1.0

        ! Find the nearest pane above
        do i = 1, size(editor%tabs(tab_idx)%panes)
            if (i /= current_idx) then
                ! Check if pane is above
                if (editor%tabs(tab_idx)%panes(i)%y_end <= current_y) then
                    if (best_idx == -1 .or. editor%tabs(tab_idx)%panes(i)%y_end > best_y) then
                        best_idx = i
                        best_y = editor%tabs(tab_idx)%panes(i)%y_end
                    end if
                end if
            end if
        end do

        if (best_idx > 0) then
            call switch_to_pane(editor, tab_idx, best_idx)
        end if
    end subroutine navigate_to_pane_up

    ! Navigate to pane below
    subroutine navigate_to_pane_down(editor)
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx, current_idx, i
        real :: current_y, best_y
        integer :: best_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        current_idx = editor%tabs(tab_idx)%active_pane_index
        if (current_idx < 1 .or. current_idx > size(editor%tabs(tab_idx)%panes)) return

        current_y = (editor%tabs(tab_idx)%panes(current_idx)%y_start + &
                     editor%tabs(tab_idx)%panes(current_idx)%y_end) / 2.0

        best_idx = -1
        best_y = 2.0  ! Start with value beyond max

        ! Find the nearest pane below
        do i = 1, size(editor%tabs(tab_idx)%panes)
            if (i /= current_idx) then
                ! Check if pane is below
                if (editor%tabs(tab_idx)%panes(i)%y_start >= current_y) then
                    if (best_idx == -1 .or. editor%tabs(tab_idx)%panes(i)%y_start < best_y) then
                        best_idx = i
                        best_y = editor%tabs(tab_idx)%panes(i)%y_start
                    end if
                end if
            end if
        end do

        if (best_idx > 0) then
            call switch_to_pane(editor, tab_idx, best_idx)
        end if
    end subroutine navigate_to_pane_down

    ! Helper to switch to a specific pane
    subroutine switch_to_pane(editor, tab_idx, pane_idx)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx
        integer :: i, old_pane_idx

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        ! Don't do anything if we're already in this pane
        if (pane_idx == editor%tabs(tab_idx)%active_pane_index) return

        ! Save current editor state to the old pane before switching
        old_pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (old_pane_idx > 0 .and. old_pane_idx <= size(editor%tabs(tab_idx)%panes)) then
            call sync_editor_to_pane(editor)
        end if

        ! Clear all is_active flags
        do i = 1, size(editor%tabs(tab_idx)%panes)
            editor%tabs(tab_idx)%panes(i)%is_active = .false.
        end do

        ! Set new active pane
        editor%tabs(tab_idx)%panes(pane_idx)%is_active = .true.
        editor%tabs(tab_idx)%active_pane_index = pane_idx

        ! Load the new pane's state to editor
        call sync_pane_to_editor(editor, tab_idx, pane_idx)
    end subroutine switch_to_pane

    ! Helper to switch to a specific pane with buffer synchronization
    subroutine switch_to_pane_with_buffer(editor, tab_idx, pane_idx, buffer)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx
        type(buffer_t), intent(inout) :: buffer
        integer :: i, old_pane_idx

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        ! Don't do anything if we're already in this pane
        if (pane_idx == editor%tabs(tab_idx)%active_pane_index) return

        ! Save current buffer and editor state to the old pane before switching
        old_pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (old_pane_idx > 0 .and. old_pane_idx <= size(editor%tabs(tab_idx)%panes)) then
            ! Save buffer to old pane
            call copy_buffer(editor%tabs(tab_idx)%panes(old_pane_idx)%buffer, buffer)

            ! Sync buffer to all other instances of this file
            if (allocated(editor%tabs(tab_idx)%panes(old_pane_idx)%filename)) then
                call sync_buffer_to_all_instances(editor, editor%tabs(tab_idx)%panes(old_pane_idx)%filename, buffer)
            end if

            ! Save cursor/viewport state
            call sync_editor_to_pane(editor)
        end if

        ! Clear all is_active flags
        do i = 1, size(editor%tabs(tab_idx)%panes)
            editor%tabs(tab_idx)%panes(i)%is_active = .false.
        end do

        ! Set new active pane
        editor%tabs(tab_idx)%panes(pane_idx)%is_active = .true.
        editor%tabs(tab_idx)%active_pane_index = pane_idx

        ! Load the new pane's buffer
        call copy_buffer(buffer, editor%tabs(tab_idx)%panes(pane_idx)%buffer)

        ! Load the new pane's cursor/viewport state to editor
        call sync_pane_to_editor(editor, tab_idx, pane_idx)

        ! Update editor filename if pane has different file
        if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)) then
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len(editor%tabs(tab_idx)%panes(pane_idx)%filename)) :: editor%filename)
            editor%filename = editor%tabs(tab_idx)%panes(pane_idx)%filename
        end if
    end subroutine switch_to_pane_with_buffer

    ! Sync buffer to all panes/tabs that have the same file open
    ! This enables live updates when the same file is open in multiple locations
    !> Record the buffer as the clean state for this tab.
    subroutine note_tab_saved(editor, tab_idx, buffer)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx
        type(buffer_t), intent(in) :: buffer

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        editor%tabs(tab_idx)%saved_sig = buffer_signature(buffer)
        editor%tabs(tab_idx)%modified = .false.
    end subroutine note_tab_saved

    !> Set a tab's modified flag from what its text actually IS.
    !>
    !> Two reported bugs came from the flag being asserted rather than
    !> derived. Undoing an edit back to the original left the asterisk on,
    !> because nothing ever cleared it; and switching away from a file set it
    !> unconditionally, so an untouched file came back dirty every time it was
    !> left. Both are answered by comparing against the last saved text.
    subroutine refresh_tab_modified(editor, tab_idx, buffer)
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx
        type(buffer_t), intent(in) :: buffer

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        ! Never established -- a tab restored from a session, say. Leave the
        ! flag as it stands rather than declare a file clean on no evidence.
        if (editor%tabs(tab_idx)%saved_sig == -1_int64) return
        editor%tabs(tab_idx)%modified = &
            buffer_signature(buffer) /= editor%tabs(tab_idx)%saved_sig
    end subroutine refresh_tab_modified

    subroutine sync_buffer_to_all_instances(editor, filename, buffer)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: filename
        type(buffer_t), intent(in) :: buffer
        integer :: tab_idx, pane_idx
        character(len=:), allocatable :: normalized_filename

        ! Normalize filename for comparison (trim whitespace)
        normalized_filename = trim(filename)
        if (len_trim(normalized_filename) == 0) return

        ! Loop through all tabs
        do tab_idx = 1, size(editor%tabs)
            ! A deferred tab holds no text to update, and marking it modified
            ! would make a save path believe it had unsaved changes -- which,
            ! for a buffer that was never read, means writing an empty file.
            if (.not. tab_is_resident(editor, tab_idx)) cycle
            ! Update tab's buffer if it matches
            if (allocated(editor%tabs(tab_idx)%filename)) then
                if (trim(editor%tabs(tab_idx)%filename) == normalized_filename) then
                    ! Was an unconditional .true.. This routine runs on every
                    ! tab and pane switch, so leaving a file marked it dirty
                    ! for the act of leaving it -- saving cleared the asterisk
                    ! and the next switch put it straight back.
                    call refresh_tab_modified(editor, tab_idx, buffer)
                end if
            end if

            ! Loop through all panes in this tab
            if (allocated(editor%tabs(tab_idx)%panes)) then
                do pane_idx = 1, size(editor%tabs(tab_idx)%panes)
                    ! Check if this pane has the same file open
                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%filename)) then
                        if (trim(editor%tabs(tab_idx)%panes(pane_idx)%filename) == normalized_filename) then
                            ! Skip the currently active pane (already has the latest buffer)
                            if (tab_idx == editor%active_tab_index .and. &
                                pane_idx == editor%tabs(tab_idx)%active_pane_index) then
                                cycle
                            end if

                            ! Copy buffer to this pane (preserves cursor/viewport)
                            call copy_buffer(editor%tabs(tab_idx)%panes(pane_idx)%buffer, buffer)
                        end if
                    end if
                end do
            end if
        end do
    end subroutine sync_buffer_to_all_instances

    ! Create an untitled tab (replaces current tab)
    subroutine create_untitled_tab(editor)
        use text_buffer_module, only: init_buffer, cleanup_buffer
        type(editor_state_t), intent(inout) :: editor
        integer :: tab_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return

        ! Reinitialize as untitled
        if (allocated(editor%tabs(tab_idx)%filename)) deallocate(editor%tabs(tab_idx)%filename)
        allocate(character(len=12) :: editor%tabs(tab_idx)%filename)
        editor%tabs(tab_idx)%filename = "UNTITLED.txt"
        editor%tabs(tab_idx)%modified = .false.

        ! Reset panes
        if (allocated(editor%tabs(tab_idx)%panes)) then
            deallocate(editor%tabs(tab_idx)%panes)
        end if
        allocate(editor%tabs(tab_idx)%panes(1))
        editor%tabs(tab_idx)%panes(1)%x_start = 0.0
        editor%tabs(tab_idx)%panes(1)%y_start = 0.0
        editor%tabs(tab_idx)%panes(1)%x_end = 1.0
        editor%tabs(tab_idx)%panes(1)%y_end = 1.0
        editor%tabs(tab_idx)%panes(1)%is_active = .true.
        editor%tabs(tab_idx)%panes(1)%viewport_line = 1
        editor%tabs(tab_idx)%panes(1)%viewport_column = 1

        ! Initialize cursor
        allocate(editor%tabs(tab_idx)%panes(1)%cursors(1))
        editor%tabs(tab_idx)%panes(1)%cursors(1)%line = 1
        editor%tabs(tab_idx)%panes(1)%cursors(1)%column = 1
        editor%tabs(tab_idx)%panes(1)%cursors(1)%desired_column = 1
        editor%tabs(tab_idx)%panes(1)%cursors(1)%has_selection = .false.
        editor%tabs(tab_idx)%panes(1)%active_cursor = 1
        editor%tabs(tab_idx)%active_pane_index = 1

        ! Initialize screen coordinates for the pane
        editor%tabs(tab_idx)%panes(1)%screen_col = 1
        editor%tabs(tab_idx)%panes(1)%screen_row = 2  ! After tab bar
        editor%tabs(tab_idx)%panes(1)%screen_width = 80  ! Default width
        editor%tabs(tab_idx)%panes(1)%screen_height = 22  ! Default height

        ! Sync to editor
        call sync_pane_to_editor(editor, tab_idx, 1)
    end subroutine create_untitled_tab

    ! Recalculate pane layout after closing a pane
    subroutine recalculate_pane_layout(panes)
        type(pane_t), intent(inout) :: panes(:)
        integer :: n_panes, i
        real :: x_min, x_max, y_min, y_max
        logical :: is_vertical_split, is_horizontal_split
        real :: pane_width, pane_height

        n_panes = size(panes)

        ! If only one pane, make it full screen
        if (n_panes == 1) then
            panes(1)%x_start = 0.0
            panes(1)%x_end = 1.0
            panes(1)%y_start = 0.0
            panes(1)%y_end = 1.0
            return
        end if

        ! Determine the layout type by checking if panes share x or y coordinates
        is_vertical_split = .false.
        is_horizontal_split = .false.

        ! Check if all panes share same y coordinates (vertical split - side by side)
        y_min = panes(1)%y_start
        y_max = panes(1)%y_end
        is_vertical_split = .true.
        do i = 2, n_panes
            if (abs(panes(i)%y_start - y_min) > 0.01 .or. abs(panes(i)%y_end - y_max) > 0.01) then
                is_vertical_split = .false.
                exit
            end if
        end do

        ! Check if all panes share same x coordinates (horizontal split - top/bottom)
        if (.not. is_vertical_split) then
            x_min = panes(1)%x_start
            x_max = panes(1)%x_end
            is_horizontal_split = .true.
            do i = 2, n_panes
                if (abs(panes(i)%x_start - x_min) > 0.01 .or. abs(panes(i)%x_end - x_max) > 0.01) then
                    is_horizontal_split = .false.
                    exit
                end if
            end do
        end if

        ! Recalculate based on layout type
        if (is_vertical_split) then
            ! Panes are side by side - redistribute horizontally
            pane_width = 1.0 / real(n_panes)
            do i = 1, n_panes
                panes(i)%x_start = real(i - 1) * pane_width
                panes(i)%x_end = real(i) * pane_width
                panes(i)%y_start = 0.0
                panes(i)%y_end = 1.0
            end do
        else if (is_horizontal_split) then
            ! Panes are top/bottom - redistribute vertically
            pane_height = 1.0 / real(n_panes)
            do i = 1, n_panes
                panes(i)%x_start = 0.0
                panes(i)%x_end = 1.0
                panes(i)%y_start = real(i - 1) * pane_height
                panes(i)%y_end = real(i) * pane_height
            end do
        else
            ! Mixed layout - try to expand panes to fill gaps
            ! For now, just ensure at least the first pane is properly sized
            ! This is a simplified approach - a more sophisticated algorithm
            ! would detect and fill gaps properly

            ! Find the overall bounds
            x_min = 1.0
            x_max = 0.0
            y_min = 1.0
            y_max = 0.0
            do i = 1, n_panes
                x_min = min(x_min, panes(i)%x_start)
                x_max = max(x_max, panes(i)%x_end)
                y_min = min(y_min, panes(i)%y_start)
                y_max = max(y_max, panes(i)%y_end)
            end do

            ! If we have exactly 2 panes, try to expand them smartly
            if (n_panes == 2) then
                ! Check if they're adjacent horizontally
                if (abs(panes(1)%x_end - panes(2)%x_start) < 0.01 .or. &
                    abs(panes(2)%x_end - panes(1)%x_start) < 0.01) then
                    ! Expand vertically
                    panes(1)%y_start = 0.0
                    panes(1)%y_end = 1.0
                    panes(2)%y_start = 0.0
                    panes(2)%y_end = 1.0
                ! Check if they're adjacent vertically
                else if (abs(panes(1)%y_end - panes(2)%y_start) < 0.01 .or. &
                         abs(panes(2)%y_end - panes(1)%y_start) < 0.01) then
                    ! Expand horizontally
                    panes(1)%x_start = 0.0
                    panes(1)%x_end = 1.0
                    panes(2)%x_start = 0.0
                    panes(2)%x_end = 1.0
                end if
            end if
        end if
    end subroutine recalculate_pane_layout

end module editor_state_module