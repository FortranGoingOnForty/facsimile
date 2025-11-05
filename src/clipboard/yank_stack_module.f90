module yank_stack_module
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: yank_stack_t, init_yank_stack, cleanup_yank_stack
    public :: push_yank, pop_yank, peek_yank, clear_yank_stack

    integer, parameter :: MAX_YANK_SIZE = 1024 * 1024  ! 1MB max per entry
    integer, parameter :: MAX_YANK_ENTRIES = 100

    type :: yank_entry_t
        character(len=:), allocatable :: text
    end type yank_entry_t

    type :: yank_stack_t
        type(yank_entry_t), allocatable :: entries(:)
        integer :: top = 0
        integer :: capacity = MAX_YANK_ENTRIES
    end type yank_stack_t

contains

    subroutine init_yank_stack(stack)
        type(yank_stack_t), intent(out) :: stack

        allocate(stack%entries(stack%capacity))
        stack%top = 0
    end subroutine init_yank_stack

    subroutine cleanup_yank_stack(stack)
        type(yank_stack_t), intent(inout) :: stack
        integer :: i

        if (allocated(stack%entries)) then
            do i = 1, stack%top
                if (allocated(stack%entries(i)%text)) then
                    deallocate(stack%entries(i)%text)
                end if
            end do
            deallocate(stack%entries)
        end if
        stack%top = 0
    end subroutine cleanup_yank_stack

    subroutine push_yank(stack, text)
        type(yank_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: text
        integer :: i

        ! Check if we need to grow the stack
        if (stack%top >= stack%capacity) then
            ! Shift entries down, losing the oldest
            do i = 1, stack%capacity - 1
                if (allocated(stack%entries(i)%text)) then
                    deallocate(stack%entries(i)%text)
                end if
                if (allocated(stack%entries(i+1)%text)) then
                    allocate(character(len=len(stack%entries(i+1)%text)) :: stack%entries(i)%text)
                    stack%entries(i)%text = stack%entries(i+1)%text
                end if
            end do
            stack%top = stack%capacity - 1
        end if

        ! Add new entry
        stack%top = stack%top + 1
        if (allocated(stack%entries(stack%top)%text)) then
            deallocate(stack%entries(stack%top)%text)
        end if

        ! Limit text size
        if (len(text) > MAX_YANK_SIZE) then
            allocate(character(len=MAX_YANK_SIZE) :: stack%entries(stack%top)%text)
            stack%entries(stack%top)%text = text(1:MAX_YANK_SIZE)
        else
            allocate(character(len=len(text)) :: stack%entries(stack%top)%text)
            stack%entries(stack%top)%text = text
        end if
    end subroutine push_yank

    function pop_yank(stack) result(text)
        type(yank_stack_t), intent(inout) :: stack
        character(len=:), allocatable :: text

        if (stack%top > 0) then
            if (allocated(stack%entries(stack%top)%text)) then
                allocate(character(len=len(stack%entries(stack%top)%text)) :: text)
                text = stack%entries(stack%top)%text
                deallocate(stack%entries(stack%top)%text)
            else
                text = ''
            end if
            stack%top = stack%top - 1
        else
            text = ''
        end if
    end function pop_yank

    function peek_yank(stack) result(text)
        type(yank_stack_t), intent(in) :: stack
        character(len=:), allocatable :: text

        if (stack%top > 0) then
            if (allocated(stack%entries(stack%top)%text)) then
                allocate(character(len=len(stack%entries(stack%top)%text)) :: text)
                text = stack%entries(stack%top)%text
            else
                text = ''
            end if
        else
            text = ''
        end if
    end function peek_yank

    subroutine clear_yank_stack(stack)
        type(yank_stack_t), intent(inout) :: stack
        integer :: i

        do i = 1, stack%top
            if (allocated(stack%entries(i)%text)) then
                deallocate(stack%entries(i)%text)
            end if
        end do
        stack%top = 0
    end subroutine clear_yank_stack

end module yank_stack_module