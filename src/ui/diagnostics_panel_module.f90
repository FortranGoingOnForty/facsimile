module diagnostics_panel_module
    use iso_fortran_env, only: int32
    use diagnostics_module, only: diagnostic_t, diagnostics_store_t, &
                                  get_diagnostics_for_file, &
                                  SEVERITY_ERROR, SEVERITY_WARNING, &
                                  SEVERITY_INFO, SEVERITY_HINT
    use terminal_io_module, only: terminal_write, terminal_move_cursor
    use clickable_region_module, only: region_add, REGION_BLOCK
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
        integer :: start_col, row, i, visible_items
        character(len=256) :: line_buffer
        character(len=5) :: severity_marker
        character(len=10) :: severity_color

        ! Initialize line_buffer to prevent garbage characters
        line_buffer = repeat(' ', len(line_buffer))

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
        call terminal_write(char(27) // '[48;5;236m')  ! Dark background
        write(line_buffer, '(A,I0,A)') ' Diagnostics (', panel%diagnostic_count, ') '
        call terminal_write(char(27) // '[1m' // trim(line_buffer) // char(27) // '[0m')

        ! Separator
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(char(27) // '[48;5;236m' // repeat("─", panel%width) // char(27) // '[0m')

        ! Content area
        visible_items = min(panel%diagnostic_count, screen_rows - 3)
        row = row + 1

        ! Display diagnostics or "No diagnostics" message
        if (panel%diagnostic_count == 0) then
            call terminal_move_cursor(row, start_col)
            call terminal_write(char(27) // '[48;5;235m' // char(27) // '[90m')
            call terminal_write(' No diagnostics found')
            call terminal_write(char(27) // '[K')  ! Clear to end of line
            call terminal_write(char(27) // '[0m')
            return
        end if

        ! Render diagnostics with wrapping for selected item
        block
            integer :: screen_line, diag_idx, wrap_line
            integer :: max_content_lines
            logical :: is_selected

            screen_line = 0
            diag_idx = 1
            max_content_lines = screen_rows - 4  ! Leave room for header and footer

            do while (screen_line < max_content_lines .and. diag_idx <= panel%diagnostic_count)
                is_selected = (diag_idx == panel%selected_index)

                ! Get severity marker and color
                call get_severity_display(panel%diagnostics(diag_idx)%severity, &
                                        severity_marker, severity_color)

                ! Clear line_buffer to prevent leftover characters
                line_buffer = repeat(' ', len(line_buffer))

                ! Format diagnostic header line (severity + line number)
                write(line_buffer, '(A2,A5,A,I0,A,I0,A)') &
                    ' ', severity_marker, ' L', &
                    panel%diagnostics(diag_idx)%range%start_line + 1, ':', &
                    panel%diagnostics(diag_idx)%range%start_col + 1, ' '

                if (is_selected) then
                    ! Selected: show full message wrapped
                    block
                        integer :: header_len, first_line_chars, msg_len, remaining_len
                        integer :: chars_per_line, total_lines
                        character(len=512) :: full_message

                        full_message = panel%diagnostics(diag_idx)%message
                        msg_len = len_trim(full_message)
                        header_len = len_trim(line_buffer)
                        first_line_chars = panel%width - header_len - 1  ! Space available on first line
                        chars_per_line = panel%width - 4  ! Continuation lines have 4-char indent

                        ! Render first line with header + start of message
                        screen_line = screen_line + 1

                        call terminal_move_cursor(row + screen_line - 1, start_col)
                        call terminal_write(char(27) // '[48;5;240m')  ! Highlight
                        call terminal_write(trim(severity_color))

                        ! Append first portion of message (no truncation marker)
                        if (first_line_chars > 0 .and. msg_len > 0) then
                            if (msg_len <= first_line_chars) then
                                line_buffer(header_len+1:header_len+msg_len) = full_message(1:msg_len)
                            else
                                line_buffer(header_len+1:header_len+first_line_chars) = &
                                    full_message(1:first_line_chars)
                            end if
                        end if

                        ! Pad to width
                        do i = len_trim(line_buffer) + 1, panel%width
                            line_buffer(i:i) = ' '
                        end do

                        call terminal_write(line_buffer(1:panel%width))
                        call terminal_write(char(27) // '[0m')

                        ! Calculate and render continuation lines for remaining message
                        remaining_len = msg_len - first_line_chars
                        if (remaining_len > 0) then
                            total_lines = (remaining_len + chars_per_line - 1) / chars_per_line
                            do wrap_line = 1, total_lines
                                if (screen_line >= max_content_lines) exit
                                screen_line = screen_line + 1

                                call terminal_move_cursor(row + screen_line - 1, start_col)

                                ! Render continuation line
                                block
                                    character(len=256) :: cont_buffer
                                    integer :: cont_start, cont_end, k

                                    cont_buffer = repeat(' ', len(cont_buffer))
                                    cont_buffer(1:4) = '    '  ! Indent

                                    cont_start = first_line_chars + (wrap_line - 1) * chars_per_line + 1
                                    cont_end = min(cont_start + chars_per_line - 1, msg_len)

                                    if (cont_start <= msg_len) then
                                        cont_buffer(5:5 + cont_end - cont_start) = &
                                            full_message(cont_start:cont_end)
                                    end if

                                    ! Pad
                                    do k = len_trim(cont_buffer) + 1, panel%width
                                        cont_buffer(k:k) = ' '
                                    end do

                                    call terminal_write(char(27) // '[48;5;240m')  ! Highlight
                                    call terminal_write(cont_buffer(1:panel%width))
                                    call terminal_write(char(27) // '[0m')
                                end block
                            end do
                        end if
                    end block
                else
                    ! Not selected: single truncated line
                    screen_line = screen_line + 1
                    call terminal_move_cursor(row + screen_line - 1, start_col)
                    call terminal_write(char(27) // '[48;5;235m')  ! Normal
                    call terminal_write(trim(severity_color))
                    call append_truncated_message(line_buffer, &
                        panel%diagnostics(diag_idx)%message, panel%width)
                    call terminal_write(line_buffer(1:panel%width))
                    call terminal_write(char(27) // '[0m')
                end if

                diag_idx = diag_idx + 1
            end do

            ! Fill remaining lines with empty space
            do while (screen_line < max_content_lines)
                screen_line = screen_line + 1
                call terminal_move_cursor(row + screen_line - 1, start_col)
                call render_empty_line(panel%width)
            end do
        end block

        ! Show count in bottom right corner
        if (panel%diagnostic_count > 0) then
            call terminal_move_cursor(screen_rows - 1, start_col + 2)
            write(line_buffer, '(A,I0,A,I0,A)') '[', panel%selected_index, '/', &
                   panel%diagnostic_count, ']'
            call terminal_write(char(27) // '[48;5;236m')
            call terminal_write(trim(line_buffer))
            call terminal_write(char(27) // '[0m')
        end if

        ! Claim the strip so clicks cannot reach the document behind it
        call region_add(REGION_BLOCK, 1, screen_rows, start_col, &
                        start_col + panel%width - 1)

    end subroutine render_diagnostics_panel

    subroutine render_empty_line(width)
        integer, intent(in) :: width

        call terminal_write(char(27) // '[48;5;235m')  ! Dark background
        call terminal_write(repeat(' ', width))
        call terminal_write(char(27) // '[0m')
    end subroutine render_empty_line

    subroutine get_severity_display(severity, marker, color)
        integer, intent(in) :: severity
        character(len=5), intent(out) :: marker
        character(len=10), intent(out) :: color

        select case(severity)
        case(SEVERITY_ERROR)
            marker = '●'
            color = char(27) // '[31m'  ! Red
        case(SEVERITY_WARNING)
            marker = '▲'
            color = char(27) // '[33m'  ! Yellow
        case(SEVERITY_INFO)
            marker = '◆'
            color = char(27) // '[36m'  ! Cyan
        case(SEVERITY_HINT)
            marker = '○'
            color = char(27) // '[90m'  ! Gray
        case default
            marker = ' '
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

    ! UNUSED: Calculate how many lines a message needs when wrapped
    ! Kept for potential future use
    ! function calc_wrapped_lines(message, width) result(num_lines)
    !     character(len=*), intent(in) :: message
    !     integer, intent(in) :: width
    !     integer :: num_lines
    !     integer :: msg_len, wrap_width
    !
    !     msg_len = len_trim(message)
    !     wrap_width = width - 4  ! Leave space for indentation
    !
    !     if (msg_len <= wrap_width) then
    !         num_lines = 1
    !     else
    !         num_lines = (msg_len + wrap_width - 1) / wrap_width
    !     end if
    ! end function calc_wrapped_lines

    ! UNUSED: Render a wrapped line of a message (line_num is 1-based)
    ! Kept for potential future use
    ! subroutine render_wrapped_line(message, line_num, width, start_col, is_selected)
    !     character(len=*), intent(in) :: message
    !     integer, intent(in) :: line_num, width, start_col
    !     logical, intent(in) :: is_selected
    !     character(len=256) :: output_buffer
    !     integer :: msg_len, wrap_width, start_pos, end_pos, i
    !
    !     msg_len = len_trim(message)
    !     wrap_width = width - 4  ! Leave space for indentation
    !
    !     ! Calculate which portion of message to show
    !     start_pos = (line_num - 1) * wrap_width + 1
    !     end_pos = min(start_pos + wrap_width - 1, msg_len)
    !
    !     ! Initialize buffer with spaces
    !     output_buffer = repeat(' ', len(output_buffer))
    !
    !     ! Add indentation for continuation lines
    !     output_buffer(1:4) = '    '
    !
    !     ! Copy message portion
    !     if (start_pos <= msg_len) then
    !         output_buffer(5:5 + end_pos - start_pos) = message(start_pos:end_pos)
    !     end if
    !
    !     ! Pad to width
    !     do i = len_trim(output_buffer) + 1, width
    !         output_buffer(i:i) = ' '
    !     end do
    !
    !     ! Set background color
    !     if (is_selected) then
    !         call terminal_write(char(27) // '[48;5;240m')  ! Highlight
    !     else
    !         call terminal_write(char(27) // '[48;5;235m')  ! Normal
    !     end if
    !
    !     ! Write the line
    !     call terminal_write(output_buffer(1:width))
    !     call terminal_write(char(27) // '[0m')
    ! end subroutine render_wrapped_line

    function diagnostics_panel_handle_key(panel, key) result(handled)
        type(diagnostics_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled

        handled = .false.
        if (.not. panel%visible) return

        select case(key)
        case('j', 'down')  ! j or down arrow
            if (panel%selected_index < panel%diagnostic_count) then
                panel%selected_index = panel%selected_index + 1
            end if
            handled = .true.  ! Always consume navigation keys

        case('k', 'up')  ! k or up arrow
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
            end if
            handled = .true.  ! Always consume navigation keys

        case('enter')  ! Enter - jump to diagnostic
            ! This will need to be handled by the main editor
            handled = .true.

        case('esc', 'q')  ! ESC or q - close panel
            panel%visible = .false.
            handled = .true.

        end select
    end function diagnostics_panel_handle_key

    ! UNUSED: Get location of selected diagnostic
    ! Kept for potential future use
    ! function get_selected_diagnostic_location(panel, line, col) result(has_location)
    !     type(diagnostics_panel_t), intent(in) :: panel
    !     integer, intent(out) :: line, col
    !     logical :: has_location
    !
    !     has_location = .false.
    !     line = 1
    !     col = 1
    !
    !     if (panel%visible .and. panel%diagnostic_count > 0 .and. &
    !         panel%selected_index > 0 .and. panel%selected_index <= panel%diagnostic_count) then
    !
    !         line = panel%diagnostics(panel%selected_index)%range%start_line + 1  ! Convert to 1-based
    !         col = panel%diagnostics(panel%selected_index)%range%start_col + 1
    !         has_location = .true.
    !     end if
    ! end function get_selected_diagnostic_location

end module diagnostics_panel_module