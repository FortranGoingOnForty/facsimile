module jump_stack_module
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: jump_stack_t
    public :: init_jump_stack, cleanup_jump_stack
    public :: push_jump_location, pop_jump_location
    public :: peek_jump_location, is_jump_stack_empty

    ! Maximum number of jump locations to track
    integer, parameter :: MAX_JUMP_STACK = 100

    ! Jump location entry
    type :: jump_location_t
        character(len=:), allocatable :: filename
        integer(int32) :: line
        integer(int32) :: column
    end type jump_location_t

    ! Jump stack for navigation history
    type :: jump_stack_t
        type(jump_location_t), allocatable :: locations(:)
        integer :: top = 0
        integer :: capacity = MAX_JUMP_STACK
    end type jump_stack_t

contains

    subroutine init_jump_stack(stack)
        type(jump_stack_t), intent(out) :: stack

        allocate(stack%locations(MAX_JUMP_STACK))
        stack%top = 0
        stack%capacity = MAX_JUMP_STACK
    end subroutine init_jump_stack

    subroutine cleanup_jump_stack(stack)
        type(jump_stack_t), intent(inout) :: stack
        integer :: i

        if (allocated(stack%locations)) then
            ! Deallocate all filename strings
            do i = 1, stack%top
                if (allocated(stack%locations(i)%filename)) then
                    deallocate(stack%locations(i)%filename)
                end if
            end do
            deallocate(stack%locations)
        end if
        stack%top = 0
    end subroutine cleanup_jump_stack

    subroutine push_jump_location(stack, filename, line, column)
        type(jump_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: line, column
        integer :: i

        ! Don't push if same as current top location
        if (stack%top > 0) then
            if (allocated(stack%locations(stack%top)%filename)) then
                if (stack%locations(stack%top)%filename == filename .and. &
                    stack%locations(stack%top)%line == line) then
                    return  ! Don't duplicate the same location
                end if
            end if
        end if

        ! If stack is full, shift everything down
        if (stack%top >= stack%capacity) then
            ! Deallocate the first location's filename
            if (allocated(stack%locations(1)%filename)) then
                deallocate(stack%locations(1)%filename)
            end if

            ! Shift all locations down by one
            do i = 1, stack%capacity - 1
                stack%locations(i) = stack%locations(i + 1)
            end do
            stack%top = stack%capacity - 1
        end if

        ! Add new location
        stack%top = stack%top + 1
        if (allocated(stack%locations(stack%top)%filename)) then
            deallocate(stack%locations(stack%top)%filename)
        end if
        allocate(character(len=len_trim(filename)) :: stack%locations(stack%top)%filename)
        stack%locations(stack%top)%filename = trim(filename)
        stack%locations(stack%top)%line = line
        stack%locations(stack%top)%column = column
    end subroutine push_jump_location

    function pop_jump_location(stack, filename, line, column) result(success)
        type(jump_stack_t), intent(inout) :: stack
        character(len=:), allocatable, intent(out) :: filename
        integer(int32), intent(out) :: line, column
        logical :: success

        success = .false.
        if (stack%top <= 0) return

        ! Get the top location
        if (allocated(stack%locations(stack%top)%filename)) then
            allocate(character(len=len(stack%locations(stack%top)%filename)) :: filename)
            filename = stack%locations(stack%top)%filename
            line = stack%locations(stack%top)%line
            column = stack%locations(stack%top)%column

            ! Remove from stack
            deallocate(stack%locations(stack%top)%filename)
            stack%top = stack%top - 1
            success = .true.
        end if
    end function pop_jump_location

    function peek_jump_location(stack, filename, line, column) result(success)
        type(jump_stack_t), intent(in) :: stack
        character(len=:), allocatable, intent(out) :: filename
        integer(int32), intent(out) :: line, column
        logical :: success

        success = .false.
        if (stack%top <= 0) return

        ! Get the top location without removing it
        if (allocated(stack%locations(stack%top)%filename)) then
            allocate(character(len=len(stack%locations(stack%top)%filename)) :: filename)
            filename = stack%locations(stack%top)%filename
            line = stack%locations(stack%top)%line
            column = stack%locations(stack%top)%column
            success = .true.
        end if
    end function peek_jump_location

    function is_jump_stack_empty(stack) result(is_empty)
        type(jump_stack_t), intent(in) :: stack
        logical :: is_empty

        is_empty = (stack%top <= 0)
    end function is_jump_stack_empty

end module jump_stack_module