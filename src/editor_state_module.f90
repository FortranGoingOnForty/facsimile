module editor_state_module
    use iso_fortran_env, only: int32, int64
    use text_buffer_module, only: buffer_t, copy_buffer, init_buffer
    use lsp_server_manager_module, only: lsp_manager_t, init_lsp_manager, cleanup_lsp_manager, &
                                         get_or_start_server, process_server_messages, &
                                         start_lsp_for_file, notify_file_opened, &
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
    use code_actions_menu_module, only: code_actions_menu_t, init_code_actions_menu, &
                                        cleanup_code_actions_menu
    use symbols_panel_module, only: symbols_panel_t, init_symbols_panel, &
                                     cleanup_symbols_panel
    use document_sync_module, only: document_sync_t, init_document_sync, &
                                    cleanup_document_sync
    use jump_stack_module, only: jump_stack_t, init_jump_stack, &
                                 cleanup_jump_stack
    implicit none
    private

    public :: editor_state_t, cursor_t, pane_t, tab_t
    public :: init_editor, cleanup_editor
    public :: create_tab, switch_to_tab, switch_to_tab_with_buffer, get_active_tab_index, close_tab
    public :: split_pane_vertical, split_pane_horizontal, close_pane, get_active_pane_indices
    public :: navigate_to_pane_left, navigate_to_pane_right, navigate_to_pane_up, navigate_to_pane_down
    public :: sync_pane_to_editor, sync_editor_to_pane, switch_to_pane, switch_to_pane_with_buffer
    public :: sync_buffer_to_all_instances

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
    type :: tab_t
        character(len=:), allocatable :: filename
        type(buffer_t) :: buffer

        ! Panes within this tab
        type(pane_t), allocatable :: panes(:)
        integer(int32) :: active_pane_index = 1

        logical :: modified = .false.
        logical :: is_orphan = .false.  ! True if file is outside workspace (uses absolute path)

        ! LSP support
        integer :: lsp_server_index = 0  ! Index of LSP server handling this file
        type(document_sync_t) :: document_sync   ! Document synchronization for LSP
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
        integer(int32) :: max_tabs = 10

        ! LSP support
        type(lsp_manager_t) :: lsp_manager
        type(completion_popup_t) :: completion_popup
        type(hover_tooltip_t) :: hover_tooltip
        type(diagnostics_store_t) :: diagnostics
        type(diagnostics_panel_t) :: diagnostics_panel
        type(references_panel_t) :: references_panel
        type(code_actions_menu_t) :: code_actions_menu
        type(symbols_panel_t) :: symbols_panel

        ! Navigation
        type(jump_stack_t) :: jump_stack
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
        call init_code_actions_menu(editor%code_actions_menu)

        ! Initialize symbols panel
        call init_symbols_panel(editor%symbols_panel)

        ! Initialize jump stack
        call init_jump_stack(editor%jump_stack)
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
        call cleanup_code_actions_menu(editor%code_actions_menu)

        ! Cleanup symbols panel
        call cleanup_symbols_panel(editor%symbols_panel)

        ! Cleanup jump stack
        call cleanup_jump_stack(editor%jump_stack)
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

        call cleanup_buffer(tab%buffer)

        ! Cleanup document sync
        call cleanup_document_sync(tab%document_sync)
    end subroutine cleanup_tab

    ! Create a new tab with the given filename
    subroutine create_tab(editor, filename)
        use text_buffer_module, only: init_buffer
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: filename
        type(tab_t), allocatable :: temp_tabs(:)
        integer :: n_tabs, new_index

        n_tabs = size(editor%tabs)

        ! Check max tabs limit
        if (n_tabs >= editor%max_tabs) then
            ! Could add error handling here
            return
        end if

        ! Resize tabs array
        allocate(temp_tabs(n_tabs + 1))
        if (n_tabs > 0) then
            temp_tabs(1:n_tabs) = editor%tabs(1:n_tabs)
        end if

        ! Initialize new tab
        new_index = n_tabs + 1
        allocate(character(len=len_trim(filename)) :: temp_tabs(new_index)%filename)
        temp_tabs(new_index)%filename = trim(filename)
        call init_buffer(temp_tabs(new_index)%buffer)

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

        ! Initialize pane's buffer and filename (copy from tab)
        call init_buffer(temp_tabs(new_index)%panes(1)%buffer)
        call copy_buffer(temp_tabs(new_index)%panes(1)%buffer, temp_tabs(new_index)%buffer)
        allocate(character(len=len_trim(filename)) :: temp_tabs(new_index)%panes(1)%filename)
        temp_tabs(new_index)%panes(1)%filename = trim(filename)

        temp_tabs(new_index)%active_pane_index = 1
        temp_tabs(new_index)%modified = .false.

        ! Start LSP server for this file if applicable
        temp_tabs(new_index)%lsp_server_index = start_lsp_for_file(editor%lsp_manager, filename)

        ! Initialize document sync for LSP if we have a server
        if (temp_tabs(new_index)%lsp_server_index > 0) then
            block
                character(len=:), allocatable :: file_uri
                file_uri = 'file://' // trim(filename)
                call init_document_sync(temp_tabs(new_index)%document_sync, &
                                      file_uri, temp_tabs(new_index)%lsp_server_index)
            end block
        end if

        ! Replace tabs array
        call move_alloc(temp_tabs, editor%tabs)
        editor%active_tab_index = new_index
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
            editor%tabs(editor%active_tab_index)%modified = editor%modified
        end if

        ! Load new tab state
        editor%active_tab_index = tab_index

        ! Load from active pane of new tab
        pane_idx = editor%tabs(tab_index)%active_pane_index
        if (allocated(editor%tabs(tab_index)%panes) .and. &
            pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_index)%panes)) then

            if (allocated(editor%cursors)) deallocate(editor%cursors)
            allocate(editor%cursors(size(editor%tabs(tab_index)%panes(pane_idx)%cursors)))
            editor%cursors = editor%tabs(tab_index)%panes(pane_idx)%cursors
            editor%active_cursor = editor%tabs(tab_index)%panes(pane_idx)%active_cursor
            editor%viewport_line = editor%tabs(tab_index)%panes(pane_idx)%viewport_line
            editor%viewport_column = editor%tabs(tab_index)%panes(pane_idx)%viewport_column
        end if

        if (allocated(editor%filename)) deallocate(editor%filename)
        allocate(character(len=len(editor%tabs(tab_index)%filename)) :: editor%filename)
        editor%filename = editor%tabs(tab_index)%filename
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

        ! Save current buffer to current tab's active pane (if any)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
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
            ! Also save to tab's buffer for backwards compatibility
            call copy_buffer(editor%tabs(editor%active_tab_index)%buffer, buffer)
            editor%tabs(editor%active_tab_index)%modified = editor%modified
        end if

        ! Switch to new tab
        editor%active_tab_index = tab_index

        ! Load new tab's active pane buffer
        pane_idx = editor%tabs(tab_index)%active_pane_index
        if (allocated(editor%tabs(tab_index)%panes) .and. &
            pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_index)%panes)) then
            call copy_buffer(buffer, editor%tabs(tab_index)%panes(pane_idx)%buffer)
        else
            ! Fallback to tab buffer if no pane
            call copy_buffer(buffer, editor%tabs(tab_index)%buffer)
        end if

        ! Load from active pane of new tab
        pane_idx = editor%tabs(tab_index)%active_pane_index
        if (allocated(editor%tabs(tab_index)%panes) .and. &
            pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_index)%panes)) then

            if (allocated(editor%cursors)) deallocate(editor%cursors)
            allocate(editor%cursors(size(editor%tabs(tab_index)%panes(pane_idx)%cursors)))
            editor%cursors = editor%tabs(tab_index)%panes(pane_idx)%cursors
            editor%active_cursor = editor%tabs(tab_index)%panes(pane_idx)%active_cursor
            editor%viewport_line = editor%tabs(tab_index)%panes(pane_idx)%viewport_line
            editor%viewport_column = editor%tabs(tab_index)%panes(pane_idx)%viewport_column
        end if

        if (allocated(editor%filename)) deallocate(editor%filename)
        allocate(character(len=len(editor%tabs(tab_index)%filename)) :: editor%filename)
        editor%filename = editor%tabs(tab_index)%filename
        editor%modified = editor%tabs(tab_index)%modified
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

        n_tabs = size(editor%tabs)
        if (tab_index < 1 .or. tab_index > n_tabs) return

        ! Cleanup the tab being closed
        call cleanup_tab(editor%tabs(tab_index))

        if (n_tabs == 1) then
            ! Last tab - just deallocate the array
            deallocate(editor%tabs)
            allocate(editor%tabs(0))
            editor%active_tab_index = 0
            return
        end if

        ! Create new array without this tab
        allocate(temp_tabs(n_tabs - 1))
        j = 1
        do i = 1, n_tabs
            if (i /= tab_index) then
                temp_tabs(j) = editor%tabs(i)
                j = j + 1
            end if
        end do

        ! Replace tabs array
        call move_alloc(temp_tabs, editor%tabs)

        ! Adjust active tab index
        if (editor%active_tab_index > n_tabs - 1) then
            editor%active_tab_index = n_tabs - 1
        else if (editor%active_tab_index >= tab_index) then
            editor%active_tab_index = max(1, editor%active_tab_index - 1)
        end if

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
            if (active_pane%buffer%size > 0) then
                call copy_buffer(temp_panes(new_idx)%buffer, active_pane%buffer)
            else
                ! Active pane buffer not initialized, copy from tab buffer
                call copy_buffer(temp_panes(new_idx)%buffer, editor%tabs(tab_idx)%buffer)
            end if
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
            if (active_pane%buffer%size > 0) then
                call copy_buffer(temp_panes(new_idx)%buffer, active_pane%buffer)
            else
                ! Active pane buffer not initialized, copy from tab buffer
                call copy_buffer(temp_panes(new_idx)%buffer, editor%tabs(tab_idx)%buffer)
            end if
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
            line_count = buffer_get_line_count(editor%tabs(tab_idx)%buffer)
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
                    line = buffer_get_line(editor%tabs(tab_idx)%buffer, editor%cursors(i)%line)
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
            ! Update tab's buffer if it matches
            if (allocated(editor%tabs(tab_idx)%filename)) then
                if (trim(editor%tabs(tab_idx)%filename) == normalized_filename) then
                    call copy_buffer(editor%tabs(tab_idx)%buffer, buffer)
                    editor%tabs(tab_idx)%modified = .true.
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

        ! Clean up the old tab's buffer
        call cleanup_buffer(editor%tabs(tab_idx)%buffer)

        ! Reinitialize as untitled
        if (allocated(editor%tabs(tab_idx)%filename)) deallocate(editor%tabs(tab_idx)%filename)
        allocate(character(len=12) :: editor%tabs(tab_idx)%filename)
        editor%tabs(tab_idx)%filename = "UNTITLED.txt"
        editor%tabs(tab_idx)%modified = .false.

        ! Initialize empty buffer
        call init_buffer(editor%tabs(tab_idx)%buffer)

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