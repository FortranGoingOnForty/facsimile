module diagnostics_panel_module
    use iso_fortran_env, only: int32
    use diagnostics_module, only: diagnostic_t, diagnostics_store_t, &
                                  get_diagnostics_for_file, &
                                  SEVERITY_ERROR, SEVERITY_WARNING, &
                                  SEVERITY_INFO, SEVERITY_HINT
    use terminal_io_module, only: terminal_write, terminal_move_cursor
    implicit none
    private

    public :: diagnostics_panel_t
    public :: init_diagnostics_panel, cleanup_diagnostics_panel
    public :: render_diagnostics_panel, toggle_diagnostics_panel
    public :: diagnostics_panel_handle_key
    public :: is_diagnostics_panel_visible

    type :: diagnostics_panel_t
        logical :: visible = .false.
        integer :: width = 40  ! Panel width in columns
        integer :: selected_index = 1  ! Currently selected diagnostic
        integer :: scroll_offset = 0   ! For scrolling through long lists
        integer :: diagnostic_count = 0
        type(diagnostic_t), allocatable :: diagnostics(:)
    end type diagnostics_panel_t

contains

    subroutine init_diagnostics_panel(panel)
        type(diagnostics_panel_t), intent(out) :: panel
        panel%visible = .false.
        panel%width = 40
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%diagnostic_count = 0
        if (allocated(panel%diagnostics)) deallocate(panel%diagnostics)
    end subroutine init_diagnostics_panel

    subroutine cleanup_diagnostics_panel(panel)
        type(diagnostics_panel_t), intent(inout) :: panel
        if (allocated(panel%diagnostics)) deallocate(panel%diagnostics)
        panel%diagnostic_count = 0
    end subroutine cleanup_diagnostics_panel

    subroutine toggle_diagnostics_panel(panel)
        type(diagnostics_panel_t), intent(inout) :: panel
        panel%visible = .not. panel%visible
        if (panel%visible) then
            panel%selected_index = 1
            panel%scroll_offset = 0
        end if
    end subroutine toggle_diagnostics_panel

    function is_diagnostics_panel_visible(panel) result(visible)
        type(diagnostics_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_diagnostics_panel_visible

    subroutine update_diagnostics(panel, diagnostics_store, file_uri)
        type(diagnostics_panel_t), intent(inout) :: panel
        type(diagnostics_store_t), intent(in) :: diagnostics_store
        character(len=*), intent(in) :: file_uri

        ! Get all diagnostics for current file
        if (allocated(panel%diagnostics)) deallocate(panel%diagnostics)
        panel%diagnostics = get_diagnostics_for_file(diagnostics_store, file_uri)

        if (allocated(panel%diagnostics)) then
            panel%diagnostic_count = size(panel%diagnostics)
        else
            panel%diagnostic_count = 0
        end if

        ! Reset selection if out of bounds
        if (panel%selected_index > panel%diagnostic_count) then
            panel%selected_index = max(1, panel%diagnostic_count)
        end if
    end subroutine update_diagnostics

    subroutine render_diagnostics_panel(panel, diagnostics_store, file_uri, screen_rows, screen_cols)
        type(diagnostics_panel_t), intent(inout) :: panel
        type(diagnostics_store_t), intent(in) :: diagnostics_store
        character(len=*), intent(in) :: file_uri
        integer, intent(in) :: screen_rows, screen_cols
        integer :: start_col, row, i, visible_items, item_idx
        character(len=256) :: line_buffer
        character(len=3) :: severity_marker
        character(len=10) :: severity_color

        if (.not. panel%visible) return

        ! Update diagnostics
        call update_diagnostics(panel, diagnostics_store, file_uri)

        ! Calculate panel position (right side)
        start_col = screen_cols - panel%width + 1
        if (start_col < 1) start_col = 1

        ! Draw panel border and title
        row = 1
        call terminal_move_cursor(row, start_col)

        ! Top border with title
        line_buffer = '╭─ Diagnostics '
        do i = len_trim(line_buffer) + 1, panel%width - 1
            line_buffer(i:i) = '─'
        end do
        line_buffer(panel%width:panel%width) = '╮'
        call terminal_write(char(27) // '[48;5;236m')  ! Dark background
        call terminal_write(trim(line_buffer(1:panel%width)))
        call terminal_write(char(27) // '[0m')

        ! Content area
        visible_items = min(panel%diagnostic_count, screen_rows - 3)

        do i = 1, screen_rows - 2
            row = i + 1
            call terminal_move_cursor(row, start_col)

            if (i <= visible_items) then
                item_idx = i + panel%scroll_offset
                if (item_idx <= panel%diagnostic_count) then
                    ! Get severity marker and color
                    call get_severity_display(panel%diagnostics(item_idx)%severity, &
                                            severity_marker, severity_color)

                    ! Format diagnostic line
                    write(line_buffer, '(A1,A3,A,I0,A,I0,A)') &
                        '│', severity_marker, ' L', &
                        panel%diagnostics(item_idx)%range%start_line + 1, ':', &
                        panel%diagnostics(item_idx)%range%start_col + 1, ' '

                    ! Add truncated message
                    call append_truncated_message(line_buffer, &
                        panel%diagnostics(item_idx)%message, panel%width - 2)

                    ! Highlight if selected
                    if (item_idx == panel%selected_index) then
                        call terminal_write(char(27) // '[7m')  ! Inverse video
                    else
                        call terminal_write(char(27) // '[48;5;236m')  ! Dark background
                    end if

                    ! Write severity color
                    call terminal_write(severity_color)

                    ! Write the line
                    call terminal_write(trim(line_buffer(1:panel%width-1)))

                    ! Right border
                    call terminal_write(char(27) // '[0m')  ! Reset
                    call terminal_write(char(27) // '[48;5;236m')
                    call terminal_write('│')
                    call terminal_write(char(27) // '[0m')
                else
                    ! Empty line
                    call render_empty_line(start_col, panel%width)
                end if
            else if (i == screen_rows - 2) then
                ! Bottom border
                line_buffer = '╰'
                do item_idx = 2, panel%width - 1
                    line_buffer(item_idx:item_idx) = '─'
                end do
                line_buffer(panel%width:panel%width) = '╯'
                call terminal_write(char(27) // '[48;5;236m')
                call terminal_write(trim(line_buffer(1:panel%width)))
                call terminal_write(char(27) // '[0m')
            else
                ! Empty line
                call render_empty_line(start_col, panel%width)
            end if
        end do

        ! Show count in bottom right corner
        if (panel%diagnostic_count > 0) then
            call terminal_move_cursor(screen_rows - 1, start_col + 2)
            write(line_buffer, '(A,I0,A,I0,A)') '[', panel%selected_index, '/', &
                   panel%diagnostic_count, ']'
            call terminal_write(char(27) // '[48;5;236m')
            call terminal_write(trim(line_buffer))
            call terminal_write(char(27) // '[0m')
        end if

    end subroutine render_diagnostics_panel

    subroutine render_empty_line(start_col, width)
        integer, intent(in) :: start_col, width
        character(len=256) :: spaces
        integer :: i

        call terminal_write(char(27) // '[48;5;236m')
        spaces = '│'
        do i = 2, width - 1
            spaces(i:i) = ' '
        end do
        spaces(width:width) = '│'
        call terminal_write(trim(spaces(1:width)))
        call terminal_write(char(27) // '[0m')
    end subroutine render_empty_line

    subroutine get_severity_display(severity, marker, color)
        integer, intent(in) :: severity
        character(len=3), intent(out) :: marker
        character(len=10), intent(out) :: color

        select case(severity)
        case(SEVERITY_ERROR)
            marker = ' ● '
            color = char(27) // '[31m'  ! Red
        case(SEVERITY_WARNING)
            marker = ' ▲ '
            color = char(27) // '[33m'  ! Yellow
        case(SEVERITY_INFO)
            marker = ' ◆ '
            color = char(27) // '[36m'  ! Cyan
        case(SEVERITY_HINT)
            marker = ' ○ '
            color = char(27) // '[90m'  ! Gray
        case default
            marker = '   '
            color = ''
        end select
    end subroutine get_severity_display

    subroutine append_truncated_message(buffer, message, max_len)
        character(len=*), intent(inout) :: buffer
        character(len=*), intent(in) :: message
        integer, intent(in) :: max_len
        integer :: current_len, msg_start, available_space

        current_len = len_trim(buffer)
        msg_start = current_len + 1
        available_space = max_len - current_len - 1  ! -1 for border

        if (available_space > 3) then
            if (len_trim(message) <= available_space) then
                buffer(msg_start:) = message
            else
                buffer(msg_start:msg_start + available_space - 4) = &
                    message(1:available_space - 3)
                buffer(msg_start + available_space - 3:) = '...'
            end if
        end if

        ! Pad to width
        current_len = len_trim(buffer)
        do while (current_len < max_len - 1)
            current_len = current_len + 1
            buffer(current_len:current_len) = ' '
        end do
    end subroutine append_truncated_message

    function diagnostics_panel_handle_key(panel, key) result(handled)
        type(diagnostics_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled
        integer :: max_visible

        handled = .false.
        if (.not. panel%visible) return

        select case(key)
        case('j', char(27) // '[B')  ! j or down arrow
            if (panel%selected_index < panel%diagnostic_count) then
                panel%selected_index = panel%selected_index + 1
                handled = .true.
            end if

        case('k', char(27) // '[A')  ! k or up arrow
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
                handled = .true.
            end if

        case(char(13), char(10))  ! Enter - jump to diagnostic
            ! This will need to be handled by the main editor
            handled = .true.

        case(char(27), 'q')  ! ESC or q - close panel
            panel%visible = .false.
            handled = .true.

        end select
    end function diagnostics_panel_handle_key

    function get_selected_diagnostic_location(panel, line, col) result(has_location)
        type(diagnostics_panel_t), intent(in) :: panel
        integer, intent(out) :: line, col
        logical :: has_location

        has_location = .false.
        line = 1
        col = 1

        if (panel%visible .and. panel%diagnostic_count > 0 .and. &
            panel%selected_index > 0 .and. panel%selected_index <= panel%diagnostic_count) then

            line = panel%diagnostics(panel%selected_index)%range%start_line + 1  ! Convert to 1-based
            col = panel%diagnostics(panel%selected_index)%range%start_col + 1
            has_location = .true.
        end if
    end function get_selected_diagnostic_location

end module diagnostics_panel_module