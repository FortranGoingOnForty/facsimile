module hover_tooltip_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use json_module, only: json_value_t, json_get_string, &
                           json_get_object, json_has_key
    implicit none
    private

    public :: hover_tooltip_t
    public :: init_hover_tooltip, cleanup_hover_tooltip
    public :: show_hover_tooltip, hide_hover_tooltip
    public :: handle_hover_response
    public :: is_hover_visible

    integer, parameter :: MAX_WIDTH = 60
    integer, parameter :: MAX_HEIGHT = 10

    type :: hover_tooltip_t
        character(len=:), allocatable :: content
        logical :: visible = .false.

        ! Position on screen
        integer :: row = 0
        integer :: col = 0
        integer :: width = 0
        integer :: height = 0
    end type hover_tooltip_t

contains

    subroutine init_hover_tooltip(tooltip)
        type(hover_tooltip_t), intent(out) :: tooltip

        tooltip%visible = .false.
        tooltip%row = 0
        tooltip%col = 0
        tooltip%width = 0
        tooltip%height = 0

        if (allocated(tooltip%content)) deallocate(tooltip%content)
    end subroutine init_hover_tooltip

    subroutine cleanup_hover_tooltip(tooltip)
        type(hover_tooltip_t), intent(inout) :: tooltip

        if (allocated(tooltip%content)) deallocate(tooltip%content)
        tooltip%visible = .false.
    end subroutine cleanup_hover_tooltip

    ! Handle LSP hover response and populate tooltip
    subroutine handle_hover_response(tooltip, response)
        type(hover_tooltip_t), intent(inout) :: tooltip
        type(json_value_t), intent(in) :: response
        type(json_value_t) :: contents
        character(len=:), allocatable :: hover_text

        call cleanup_hover_tooltip(tooltip)

        ! Check if response has contents
        if (json_has_key(response, "contents")) then
            contents = json_get_object(response, "contents")

            ! Try to get value from MarkupContent or MarkedString
            if (json_has_key(contents, "value")) then
                hover_text = json_get_string(contents, "value")
            else
                ! Might be a plain string
                hover_text = json_get_string(response, "contents")
            end if

            if (len_trim(hover_text) > 0) then
                tooltip%content = hover_text
                call calculate_tooltip_dimensions(tooltip)
            end if
        end if
    end subroutine handle_hover_response

    subroutine calculate_tooltip_dimensions(tooltip)
        type(hover_tooltip_t), intent(inout) :: tooltip
        integer :: content_len, lines, i, line_start, line_len, max_line_len

        if (.not. allocated(tooltip%content)) return

        content_len = len(tooltip%content)
        lines = 1
        max_line_len = 0
        line_start = 1

        ! Count lines and find max line length
        do i = 1, content_len
            if (tooltip%content(i:i) == char(10)) then  ! newline
                line_len = i - line_start
                if (line_len > max_line_len) max_line_len = line_len
                lines = lines + 1
                line_start = i + 1
            end if
        end do

        ! Check last line
        line_len = content_len - line_start + 1
        if (line_len > max_line_len) max_line_len = line_len

        tooltip%width = min(max_line_len + 4, MAX_WIDTH)  ! +4 for borders and padding
        tooltip%height = min(lines + 2, MAX_HEIGHT)  ! +2 for borders
    end subroutine calculate_tooltip_dimensions

    subroutine show_hover_tooltip(tooltip, row, col)
        type(hover_tooltip_t), intent(inout) :: tooltip
        integer, intent(in) :: row, col
        integer :: display_row, i, line_start, line_end
        character(len=256) :: line

        if (.not. allocated(tooltip%content)) return
        if (len_trim(tooltip%content) == 0) return

        tooltip%row = row
        tooltip%col = col
        tooltip%visible = .true.

        ! Draw top border
        call terminal_move_cursor(tooltip%row, tooltip%col)
        call terminal_write("┌" // repeat("─", tooltip%width - 2) // "┐")

        ! Draw content lines
        display_row = tooltip%row + 1
        line_start = 1

        do i = 1, len(tooltip%content)
            if (tooltip%content(i:i) == char(10) .or. i == len(tooltip%content)) then
                if (i == len(tooltip%content) .and. tooltip%content(i:i) /= char(10)) then
                    line_end = i
                else
                    line_end = i - 1
                end if

                call terminal_move_cursor(display_row, tooltip%col)
                write(line, '(a,a,a)') "│ ", &
                    tooltip%content(line_start:line_end), " "
                call terminal_write(line(1:min(len_trim(line), tooltip%width - 1)))
                call terminal_move_cursor(display_row, tooltip%col + tooltip%width - 1)
                call terminal_write("│")

                display_row = display_row + 1
                line_start = i + 1

                if (display_row - tooltip%row >= tooltip%height - 1) exit
            end if
        end do

        ! Draw bottom border
        call terminal_move_cursor(display_row, tooltip%col)
        call terminal_write("└" // repeat("─", tooltip%width - 2) // "┘")

    end subroutine show_hover_tooltip

    subroutine hide_hover_tooltip(tooltip)
        type(hover_tooltip_t), intent(inout) :: tooltip
        tooltip%visible = .false.
    end subroutine hide_hover_tooltip

    function is_hover_visible(tooltip) result(visible)
        type(hover_tooltip_t), intent(in) :: tooltip
        logical :: visible

        visible = tooltip%visible
    end function is_hover_visible

end module hover_tooltip_module