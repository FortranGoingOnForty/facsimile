module renderer_module
    use iso_fortran_env, only: int32, output_unit
    use terminal_io_module
    use text_buffer_module
    use editor_state_module, only: editor_state_t, cursor_t
    implicit none
    private

    public :: render_screen, update_viewport, init_renderer, cleanup_renderer
    public :: render_status_bar, render_cursor

    ! Screen buffer for double buffering
    type :: screen_buffer_t
        character(len=:), allocatable :: lines(:)
        integer :: rows
        integer :: cols
        logical :: needs_full_redraw
    end type screen_buffer_t

    type(screen_buffer_t) :: screen_buffer

contains

    subroutine init_renderer(rows, cols)
        integer, intent(in) :: rows, cols
        integer :: i

        screen_buffer%rows = rows
        screen_buffer%cols = cols
        screen_buffer%needs_full_redraw = .true.

        allocate(character(len=cols) :: screen_buffer%lines(rows))
        do i = 1, rows
            screen_buffer%lines(i) = repeat(' ', cols)
        end do
    end subroutine init_renderer

    subroutine cleanup_renderer()
        if (allocated(screen_buffer%lines)) deallocate(screen_buffer%lines)
    end subroutine cleanup_renderer

    subroutine render_screen(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer :: screen_row, buffer_line, line_count
        character(len=:), allocatable :: line_content
        character(len=1) :: ch
        integer :: col, buffer_pos, line_start_pos

        call terminal_hide_cursor()

        ! Get total lines in buffer
        line_count = buffer_get_line_count(buffer)

        ! Clear and render each visible line
        do screen_row = 1, editor%screen_rows - 1  ! Leave last row for status bar
            buffer_line = editor%viewport_line + screen_row - 1

            call terminal_move_cursor(screen_row, 1)

            if (buffer_line <= line_count) then
                ! Render actual line content
                call render_line(buffer, buffer_line, editor%viewport_column, editor%screen_cols)
            else
                ! Render empty line indicator
                if (buffer_line == line_count + 1 .and. line_count == 0) then
                    ! Empty file
                    call terminal_write('~' // repeat(' ', editor%screen_cols - 1))
                else
                    ! Beyond file content
                    call terminal_write('~' // repeat(' ', editor%screen_cols - 1))
                end if
            end if
        end do

        ! Render status bar
        call render_status_bar(editor, buffer)

        ! Position cursor
        call render_cursor(editor)

        call terminal_show_cursor()
    end subroutine render_screen

    subroutine render_line(buffer, line_num, start_col, width)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, start_col, width
        character(len=:), allocatable :: line
        character(len=:), allocatable :: visible_part
        integer :: line_len, end_col

        ! Get the line content
        line = buffer_get_line(buffer, line_num)
        line_len = len(line)

        ! Calculate visible portion
        if (start_col > line_len) then
            ! Line is scrolled past its end
            visible_part = repeat(' ', width)
        else
            end_col = min(start_col + width - 1, line_len)
            if (end_col >= start_col) then
                visible_part = line(start_col:end_col)
                ! Pad with spaces if needed
                if (len(visible_part) < width) then
                    visible_part = visible_part // repeat(' ', width - len(visible_part))
                end if
            else
                visible_part = repeat(' ', width)
            end if
        end if

        ! Write the visible part
        call terminal_write(visible_part)

        if (allocated(line)) deallocate(line)
        if (allocated(visible_part)) deallocate(visible_part)
    end subroutine render_line

    subroutine render_status_bar(editor, buffer)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        character(len=256) :: status_left, status_right, status_bar
        integer :: padding_len
        type(cursor_t) :: cursor

        cursor = editor%cursors(editor%active_cursor)

        ! Move to status bar position
        call terminal_move_cursor(editor%screen_rows, 1)

        ! Prepare status bar content
        if (allocated(editor%filename)) then
            write(status_left, '(a,a,a)') ' ', trim(editor%filename), &
                   merge(' [modified]', '           ', buffer%modified)
        else
            write(status_left, '(a,a)') ' [No Name]', &
                   merge(' [modified]', '           ', buffer%modified)
        end if

        write(status_right, '(a,i0,a,i0,a)') 'Ln ', cursor%line, ', Col ', cursor%column, ' '

        ! Create full status bar with padding
        padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_right)
        if (padding_len > 0) then
            status_bar = trim(status_left) // repeat(' ', padding_len) // trim(status_right)
        else
            status_bar = status_left(1:editor%screen_cols)
        end if

        ! Render with inverse video
        call terminal_write(char(27) // '[7m')  ! Inverse video
        call terminal_write(status_bar(1:editor%screen_cols))
        call terminal_write(char(27) // '[0m')  ! Reset attributes
    end subroutine render_status_bar

    subroutine render_cursor(editor)
        type(editor_state_t), intent(in) :: editor
        type(cursor_t) :: cursor
        integer :: screen_row, screen_col

        cursor = editor%cursors(editor%active_cursor)

        ! Calculate screen position from buffer position
        screen_row = cursor%line - editor%viewport_line + 1
        screen_col = cursor%column - editor%viewport_column + 1

        ! Ensure cursor is within screen bounds
        if (screen_row >= 1 .and. screen_row < editor%screen_rows .and. &
            screen_col >= 1 .and. screen_col <= editor%screen_cols) then
            call terminal_move_cursor(screen_row, screen_col)
        end if
    end subroutine render_cursor

    subroutine update_viewport(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t) :: cursor
        integer :: margin = 3  ! Lines to keep visible above/below cursor

        cursor = editor%cursors(editor%active_cursor)

        ! Vertical scrolling
        if (cursor%line < editor%viewport_line + margin) then
            editor%viewport_line = max(1, cursor%line - margin)
        else if (cursor%line > editor%viewport_line + editor%screen_rows - margin - 2) then
            ! -2 for status bar and margin
            editor%viewport_line = cursor%line - editor%screen_rows + margin + 2
        end if

        ! Horizontal scrolling
        if (cursor%column < editor%viewport_column + margin) then
            editor%viewport_column = max(1, cursor%column - margin)
        else if (cursor%column > editor%viewport_column + editor%screen_cols - margin) then
            editor%viewport_column = cursor%column - editor%screen_cols + margin
        end if
    end subroutine update_viewport

end module renderer_module