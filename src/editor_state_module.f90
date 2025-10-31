module editor_state_module
    use iso_fortran_env, only: int32, int64
    use text_buffer_module, only: buffer_t
    implicit none
    private

    public :: editor_state_t, cursor_t, tab_t
    public :: init_editor, cleanup_editor
    public :: create_tab, switch_to_tab, switch_to_tab_with_buffer, get_active_tab_index, close_tab

    ! Cursor position and selection
    type :: cursor_t
        integer(int32) :: line = 1
        integer(int32) :: column = 1
        integer(int32) :: desired_column = 1  ! For vertical movement
        logical :: has_selection = .false.
        integer(int32) :: selection_start_line = 1
        integer(int32) :: selection_start_col = 1
    end type cursor_t

    ! Tab - represents a single file buffer
    type :: tab_t
        character(len=:), allocatable :: filename
        type(buffer_t) :: buffer
        type(cursor_t), allocatable :: cursors(:)
        integer(int32) :: active_cursor = 1
        integer(int32) :: viewport_line = 1
        integer(int32) :: viewport_column = 1
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

        if (allocated(tab%filename)) deallocate(tab%filename)
        if (allocated(tab%cursors)) deallocate(tab%cursors)
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

        ! Initialize cursor
        allocate(temp_tabs(new_index)%cursors(1))
        temp_tabs(new_index)%cursors(1)%line = 1
        temp_tabs(new_index)%cursors(1)%column = 1
        temp_tabs(new_index)%cursors(1)%desired_column = 1
        temp_tabs(new_index)%active_cursor = 1
        temp_tabs(new_index)%viewport_line = 1
        temp_tabs(new_index)%viewport_column = 1
        temp_tabs(new_index)%modified = .false.

        ! Replace tabs array
        call move_alloc(temp_tabs, editor%tabs)
        editor%active_tab_index = new_index
    end subroutine create_tab

    ! Switch to a specific tab index (1-based)
    subroutine switch_to_tab(editor, tab_index)
        type(editor_state_t), intent(inout) :: editor
        integer(int32), intent(in) :: tab_index

        if (tab_index < 1 .or. tab_index > size(editor%tabs)) return

        ! Save current tab state (if any)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            editor%tabs(editor%active_tab_index)%cursors = editor%cursors
            editor%tabs(editor%active_tab_index)%active_cursor = editor%active_cursor
            editor%tabs(editor%active_tab_index)%viewport_line = editor%viewport_line
            editor%tabs(editor%active_tab_index)%viewport_column = editor%viewport_column
            editor%tabs(editor%active_tab_index)%modified = editor%modified
        end if

        ! Load new tab state
        editor%active_tab_index = tab_index
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(size(editor%tabs(tab_index)%cursors)))
        editor%cursors = editor%tabs(tab_index)%cursors
        editor%active_cursor = editor%tabs(tab_index)%active_cursor
        editor%viewport_line = editor%tabs(tab_index)%viewport_line
        editor%viewport_column = editor%tabs(tab_index)%viewport_column
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

        if (tab_index < 1 .or. tab_index > size(editor%tabs)) return

        ! Save current buffer to current tab (if any)
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            call copy_buffer(editor%tabs(editor%active_tab_index)%buffer, buffer)
            editor%tabs(editor%active_tab_index)%cursors = editor%cursors
            editor%tabs(editor%active_tab_index)%active_cursor = editor%active_cursor
            editor%tabs(editor%active_tab_index)%viewport_line = editor%viewport_line
            editor%tabs(editor%active_tab_index)%viewport_column = editor%viewport_column
            editor%tabs(editor%active_tab_index)%modified = editor%modified
        end if

        ! Switch to new tab
        editor%active_tab_index = tab_index

        ! Load new tab's buffer
        call copy_buffer(buffer, editor%tabs(tab_index)%buffer)

        ! Load new tab's state
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(size(editor%tabs(tab_index)%cursors)))
        editor%cursors = editor%tabs(tab_index)%cursors
        editor%active_cursor = editor%tabs(tab_index)%active_cursor
        editor%viewport_line = editor%tabs(tab_index)%viewport_line
        editor%viewport_column = editor%tabs(tab_index)%viewport_column
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

end module editor_state_module