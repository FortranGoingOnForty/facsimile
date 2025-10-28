module editor_state_module
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: editor_state_t, cursor_t, init_editor, cleanup_editor

    ! Cursor position and selection
    type :: cursor_t
        integer(int32) :: line = 1
        integer(int32) :: column = 1
        integer(int32) :: desired_column = 1  ! For vertical movement
        logical :: has_selection = .false.
        integer(int32) :: selection_start_line = 1
        integer(int32) :: selection_start_col = 1
    end type cursor_t

    ! Main editor state
    type :: editor_state_t
        type(cursor_t), allocatable :: cursors(:)
        integer(int32) :: active_cursor = 1
        integer(int32) :: viewport_line = 1
        integer(int32) :: viewport_column = 1
        integer(int32) :: screen_rows = 24
        integer(int32) :: screen_cols = 80
        character(len=:), allocatable :: filename
        logical :: modified = .false.
        character(len=32) :: last_key = ''  ! Track last key for status display
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
    end subroutine init_editor

    subroutine cleanup_editor(editor)
        type(editor_state_t), intent(inout) :: editor

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        if (allocated(editor%filename)) deallocate(editor%filename)
    end subroutine cleanup_editor

end module editor_state_module