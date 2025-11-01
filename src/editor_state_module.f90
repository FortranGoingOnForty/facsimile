module editor_state_module
    use iso_fortran_env, only: int32, int64
    use text_buffer_module, only: buffer_t
    implicit none
    private

    public :: editor_state_t, cursor_t, pane_t, tab_t
    public :: init_editor, cleanup_editor
    public :: create_tab, switch_to_tab, switch_to_tab_with_buffer, get_active_tab_index, close_tab
    public :: split_pane_vertical, split_pane_horizontal, close_pane, get_active_pane_indices
    public :: navigate_to_pane_left, navigate_to_pane_right, navigate_to_pane_up, navigate_to_pane_down
    public :: sync_editor_to_pane

    ! Cursor position and selection
    type :: cursor_t
        integer(int32) :: line = 1
        integer(int32) :: column = 1
        integer(int32) :: desired_column = 1  ! For vertical movement
        logical :: has_selection = .false.
        integer(int32) :: selection_start_line = 1
        integer(int32) :: selection_start_col = 1
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

        ! Initialize cursor in the default pane
        allocate(temp_tabs(new_index)%panes(1)%cursors(1))
        temp_tabs(new_index)%panes(1)%cursors(1)%line = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%column = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%desired_column = 1
        temp_tabs(new_index)%panes(1)%cursors(1)%has_selection = .false.
        temp_tabs(new_index)%panes(1)%active_cursor = 1

        temp_tabs(new_index)%active_pane_index = 1
        temp_tabs(new_index)%modified = .false.

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

        ! Save current buffer to current tab (if any)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            call copy_buffer(editor%tabs(editor%active_tab_index)%buffer, buffer)

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

        ! Switch to new tab
        editor%active_tab_index = tab_index

        ! Load new tab's buffer
        call copy_buffer(buffer, editor%tabs(tab_index)%buffer)

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

        ! If only one pane, close the whole tab
        if (n_panes == 1) then
            call close_tab(editor, tab_idx)
            return
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
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, pane_idx

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        associate(pane => editor%tabs(tab_idx)%panes(pane_idx))
            ! Copy pane state to editor
            if (allocated(editor%cursors)) deallocate(editor%cursors)
            if (allocated(pane%cursors)) then
                allocate(editor%cursors(size(pane%cursors)))
                editor%cursors = pane%cursors
                editor%active_cursor = pane%active_cursor
            end if
            editor%viewport_line = pane%viewport_line
            editor%viewport_column = pane%viewport_column
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
            allocate(pane%cursors(size(editor%cursors)))
            pane%cursors = editor%cursors
            pane%active_cursor = editor%active_cursor
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
        integer :: i

        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        ! Clear all is_active flags
        do i = 1, size(editor%tabs(tab_idx)%panes)
            editor%tabs(tab_idx)%panes(i)%is_active = .false.
        end do

        ! Set new active pane
        editor%tabs(tab_idx)%panes(pane_idx)%is_active = .true.
        editor%tabs(tab_idx)%active_pane_index = pane_idx

        ! Sync to editor state
        call sync_pane_to_editor(editor, tab_idx, pane_idx)
    end subroutine switch_to_pane

end module editor_state_module