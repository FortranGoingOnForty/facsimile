module undo_stack_module
    use text_buffer_module
    use editor_state_module, only: cursor_t
    implicit none
    private

    public :: undo_stack_t, init_undo_stack, cleanup_undo_stack
    public :: push_undo_state, perform_undo, perform_redo, can_undo, can_redo
    public :: save_initial_undo_state

    integer, parameter :: MAX_UNDO_LEVELS = 100

    type :: undo_state_t
        character(len=:), allocatable :: buffer_data
        integer :: gap_start
        integer :: gap_end
        integer :: size
        logical :: modified
        type(cursor_t) :: cursor_state
    end type undo_state_t

    type :: undo_stack_t
        type(undo_state_t), allocatable :: states(:)
        integer :: current_pos = 0
        integer :: stack_size = 0
    end type undo_stack_t

contains

    subroutine init_undo_stack(stack)
        type(undo_stack_t), intent(out) :: stack

        ! Allocate from 0 to support initial state at position 0
        allocate(stack%states(0:MAX_UNDO_LEVELS))
        stack%current_pos = 0
        stack%stack_size = 0
    end subroutine init_undo_stack

    subroutine save_initial_undo_state(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor

        ! Don't save initial cursor position - just buffer state
        ! We'll save cursor from the first edit location
        if (stack%stack_size == 0) then
            call save_buffer_state(stack%states(0), buffer, cursor)
            ! Mark that we don't have a valid cursor state at position 0
            stack%states(0)%cursor_state%line = -1
            stack%stack_size = 0  ! Keep at 0, first edit will go to position 1
        end if
    end subroutine save_initial_undo_state

    subroutine cleanup_undo_stack(stack)
        type(undo_stack_t), intent(inout) :: stack
        integer :: i

        if (allocated(stack%states)) then
            do i = 0, stack%stack_size
                if (allocated(stack%states(i)%buffer_data)) then
                    deallocate(stack%states(i)%buffer_data)
                end if
            end do
            deallocate(stack%states)
        end if
    end subroutine cleanup_undo_stack

    subroutine push_undo_state(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: i

        ! If we're not at the end of the stack (due to undo), clear redo states
        if (stack%current_pos < stack%stack_size) then
            do i = stack%current_pos + 1, stack%stack_size
                if (allocated(stack%states(i)%buffer_data)) then
                    deallocate(stack%states(i)%buffer_data)
                end if
            end do
            stack%stack_size = stack%current_pos
        end if

        ! Move position forward
        if (stack%stack_size < MAX_UNDO_LEVELS) then
            stack%current_pos = stack%current_pos + 1
            stack%stack_size = stack%current_pos
        else
            ! Shift everything down and add at the end
            do i = 1, MAX_UNDO_LEVELS - 1
                if (allocated(stack%states(i)%buffer_data)) then
                    deallocate(stack%states(i)%buffer_data)
                end if
                stack%states(i) = stack%states(i + 1)
            end do
            stack%current_pos = MAX_UNDO_LEVELS
            stack%stack_size = MAX_UNDO_LEVELS
        end if

        ! Store the current state
        call save_buffer_state(stack%states(stack%current_pos), buffer, cursor)
    end subroutine push_undo_state

    subroutine save_buffer_state(state, buffer, cursor)
        type(undo_state_t), intent(out) :: state
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), intent(in) :: cursor

        ! Save buffer data
        if (allocated(state%buffer_data)) deallocate(state%buffer_data)
        allocate(character(len=len(buffer%data)) :: state%buffer_data)
        state%buffer_data = buffer%data

        ! Save buffer metadata
        state%gap_start = buffer%gap_start
        state%gap_end = buffer%gap_end
        state%size = buffer%size
        state%modified = buffer%modified

        ! Save cursor state
        state%cursor_state = cursor
    end subroutine save_buffer_state

    subroutine restore_buffer_state(buffer, cursor, state)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        type(undo_state_t), intent(in) :: state

        ! Restore buffer data
        if (allocated(buffer%data)) deallocate(buffer%data)
        allocate(character(len=len(state%buffer_data)) :: buffer%data)
        buffer%data = state%buffer_data

        ! Restore buffer metadata
        buffer%gap_start = state%gap_start
        buffer%gap_end = state%gap_end
        buffer%size = state%size
        buffer%modified = state%modified

        ! Validate buffer integrity - size must match actual data length
        if (buffer%size /= len(buffer%data)) then
            buffer%size = len(buffer%data)
        end if

        ! Validate gap buffer integrity
        if (buffer%gap_start < 1) buffer%gap_start = 1
        if (buffer%gap_end > buffer%size + 1) buffer%gap_end = buffer%size + 1
        if (buffer%gap_start > buffer%gap_end) then
            ! Gap is invalid - reset to empty gap at end
            buffer%gap_start = buffer%size + 1
            buffer%gap_end = buffer%size + 1
        end if

        ! Restore cursor state only if it was a valid saved position
        ! Position 0 (initial state) has cursor marked as -1 to preserve current position
        if (state%cursor_state%line >= 1) then
            cursor = state%cursor_state
        end if
        ! Otherwise keep cursor where it is
    end subroutine restore_buffer_state

    function can_undo(stack) result(can)
        type(undo_stack_t), intent(in) :: stack
        logical :: can

        can = (stack%current_pos >= 1)
    end function can_undo

    function can_redo(stack) result(can)
        type(undo_stack_t), intent(in) :: stack
        logical :: can

        can = (stack%current_pos < stack%stack_size)
    end function can_redo

    subroutine perform_undo(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        type(undo_state_t) :: temp_state

        if (can_undo(stack)) then
            ! If we're at the end of the stack (haven't undone yet),
            ! save current state for redo
            if (stack%current_pos == stack%stack_size .and. &
                stack%current_pos < MAX_UNDO_LEVELS) then
                ! Save current state at next position for redo
                call save_buffer_state(temp_state, buffer, cursor)
                stack%stack_size = stack%current_pos + 1
                stack%states(stack%stack_size) = temp_state
            end if

            ! Move position back first
            stack%current_pos = stack%current_pos - 1

            ! Restore the state at the previous position
            call restore_buffer_state(buffer, cursor, stack%states(stack%current_pos))
        end if
    end subroutine perform_undo

    subroutine perform_redo(stack, buffer, cursor)
        type(undo_stack_t), intent(inout) :: stack
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor

        if (can_redo(stack)) then
            ! Move forward to next state
            ! Special case: if at position 0 and stack_size > 1, we should jump to the
            ! last saved state (which was saved during undo), not position 1
            if (stack%current_pos == 0 .and. stack%stack_size > 1) then
                ! Jump to the state saved during undo (skip intermediate states)
                stack%current_pos = stack%stack_size
            else
                ! Normal forward movement
                stack%current_pos = stack%current_pos + 1
            end if

            ! Restore that state
            call restore_buffer_state(buffer, cursor, stack%states(stack%current_pos))
        end if
    end subroutine perform_redo

end module undo_stack_module