module command_handler_module
    use iso_fortran_env, only: int32
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    use renderer_module, only: update_viewport
    implicit none
    private

    public :: handle_key_command

contains

    subroutine handle_key_command(key_str, editor, buffer, should_quit)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(out) :: should_quit
        integer :: line_count

        should_quit = .false.
        line_count = buffer_get_line_count(buffer)

        select case(trim(key_str))
        ! File operations
        case('ctrl-q')
            should_quit = .true.

        ! Navigation
        case('up')
            call move_cursor_up(editor%cursors(editor%active_cursor), line_count)
            call update_viewport(editor)

        case('down')
            call move_cursor_down(editor%cursors(editor%active_cursor), line_count)
            call update_viewport(editor)

        case('left')
            call move_cursor_left(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('right')
            call move_cursor_right(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('home', 'ctrl-a')
            call move_cursor_home(editor%cursors(editor%active_cursor))
            call update_viewport(editor)

        case('end', 'ctrl-e')
            call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('pageup')
            call move_cursor_page_up(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        case('pagedown')
            call move_cursor_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        ! Text modification
        case('backspace')
            call handle_backspace(editor%cursors(editor%active_cursor), buffer)

        case('delete')
            call handle_delete(editor%cursors(editor%active_cursor), buffer)

        case('enter')
            call handle_enter(editor%cursors(editor%active_cursor), buffer)

        case('tab')
            call handle_tab(editor%cursors(editor%active_cursor), buffer)

        ! Editing keybinds
        case('ctrl-k')
            call kill_line_forward(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-u')
            call kill_line_backward(editor%cursors(editor%active_cursor), buffer)

        case default
            ! Regular character input
            if (len_trim(key_str) == 1) then
                call insert_char(editor%cursors(editor%active_cursor), buffer, key_str(1:1))
            end if
        end select
    end subroutine handle_key_command

    subroutine move_cursor_up(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            cursor%column = cursor%desired_column
            ! Will adjust column in boundary check
        end if
    end subroutine move_cursor_up

    subroutine move_cursor_down(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        if (cursor%line < line_count) then
            cursor%line = cursor%line + 1
            cursor%column = cursor%desired_column
            ! Will adjust column in boundary check
        end if
    end subroutine move_cursor_down

    subroutine move_cursor_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = len(line) + 1
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
        end if
    end subroutine move_cursor_left

    subroutine move_cursor_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= len(line)) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to beginning of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = 1
        end if

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_right

    subroutine move_cursor_home(cursor)
        type(cursor_t), intent(inout) :: cursor
        cursor%column = 1
        cursor%desired_column = 1
    end subroutine move_cursor_home

    subroutine move_cursor_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = len(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_end

    subroutine move_cursor_page_up(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        page_size = editor%screen_rows - 2  ! Minus status bar and margin
        cursor%line = max(1, cursor%line - page_size)
        editor%viewport_line = max(1, editor%viewport_line - page_size)
    end subroutine move_cursor_page_up

    subroutine move_cursor_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        page_size = editor%screen_rows - 2  ! Minus status bar and margin
        cursor%line = min(line_count, cursor%line + page_size)
        editor%viewport_line = min(max(1, line_count - page_size), editor%viewport_line + page_size)
    end subroutine move_cursor_page_down

    function get_buffer_position(cursor, buffer) result(pos)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer :: pos
        integer :: line_num
        character(len=:), allocatable :: line

        pos = 0
        ! Calculate byte position in buffer
        do line_num = 1, cursor%line - 1
            line = buffer_get_line(buffer, line_num)
            pos = pos + len(line) + 1  ! +1 for newline
            if (allocated(line)) deallocate(line)
        end do
        pos = pos + cursor%column
    end function get_buffer_position

    subroutine insert_char(cursor, buffer, ch)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=1), intent(in) :: ch
        integer :: pos

        pos = get_buffer_position(cursor, buffer)
        call buffer_insert(buffer, pos, ch)
        cursor%column = cursor%column + 1
        cursor%desired_column = cursor%column
    end subroutine insert_char

    subroutine handle_backspace(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos

        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            pos = get_buffer_position(cursor, buffer)
            call buffer_delete(buffer, pos, 1)
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Join with previous line
            call move_cursor_left(cursor, buffer)
            pos = get_buffer_position(cursor, buffer)
            call buffer_delete(buffer, pos, 1)  ! Delete the newline
        end if
    end subroutine handle_backspace

    subroutine handle_delete(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos
        character(len=:), allocatable :: line
        integer :: line_count

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        pos = get_buffer_position(cursor, buffer)
        if (cursor%column <= len(line)) then
            call buffer_delete(buffer, pos, 1)
        else if (cursor%line < line_count) then
            ! Delete newline to join with next line
            call buffer_delete(buffer, pos, 1)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine handle_delete

    subroutine handle_enter(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos

        pos = get_buffer_position(cursor, buffer)
        call buffer_insert(buffer, pos, char(10))  ! Insert newline
        cursor%line = cursor%line + 1
        cursor%column = 1
        cursor%desired_column = 1
    end subroutine handle_enter

    subroutine handle_tab(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, spaces_to_add, i

        pos = get_buffer_position(cursor, buffer)
        spaces_to_add = 4 - mod(cursor%column - 1, 4)

        do i = 1, spaces_to_add
            call buffer_insert(buffer, pos + i - 1, ' ')
        end do

        cursor%column = cursor%column + spaces_to_add
        cursor%desired_column = cursor%column
    end subroutine handle_tab

    subroutine kill_line_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, delete_count
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, cursor%line)
        pos = get_buffer_position(cursor, buffer)
        delete_count = len(line) - cursor%column + 1

        if (delete_count > 0) then
            call buffer_delete(buffer, pos, delete_count)
        else if (cursor%line < buffer_get_line_count(buffer)) then
            ! Delete the newline
            call buffer_delete(buffer, pos, 1)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine kill_line_forward

    subroutine kill_line_backward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, delete_count

        if (cursor%column > 1) then
            pos = get_buffer_position(cursor, buffer)
            delete_count = cursor%column - 1
            call buffer_delete(buffer, pos - delete_count, delete_count)
            cursor%column = 1
            cursor%desired_column = 1
        end if
    end subroutine kill_line_backward

end module command_handler_module