module hover_tooltip_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use json_module, only: json_value_t, json_get_string, &
                           json_get_object, json_has_key
    use clickable_region_module, only: region_add, REGION_BLOCK
    use modal_box_module, only: box_shadow
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_BORDER, THEME_PANEL, theme_reset, theme_sgr
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
        character(len=:), allocatable :: hover_text, language, value

        call cleanup_hover_tooltip(tooltip)

        ! Check if response has contents
        if (json_has_key(response, "contents")) then
            contents = json_get_object(response, "contents")

            ! LSP hover can return different formats:
            ! 1. MarkupContent with kind and value
            ! 2. MarkedString (deprecated but still used)
            ! 3. Plain string
            ! 4. Array of MarkedString

            if (json_has_key(contents, "kind") .and. json_has_key(contents, "value")) then
                ! MarkupContent format
                hover_text = json_get_string(contents, "value")
            else if (json_has_key(contents, "language") .and. json_has_key(contents, "value")) then
                ! MarkedString with language
                language = json_get_string(contents, "language")
                value = json_get_string(contents, "value")
                hover_text = language // ": " // value
            else
                ! Try as plain string
                hover_text = json_get_string(response, "contents")
            end if

            ! Clean up markdown formatting for terminal display
            if (len_trim(hover_text) > 0) then
                ! Remove markdown code block markers if present
                if (hover_text(1:3) == "```") then
                    ! Find end of first line
                    block
                        integer :: start_pos, end_pos
                        start_pos = index(hover_text, char(10))
                        if (start_pos > 0) then
                            end_pos = index(hover_text, "```", back=.true.)
                            if (end_pos > start_pos) then
                                hover_text = hover_text(start_pos+1:end_pos-1)
                            end if
                        end if
                    end block
                end if

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

    subroutine show_hover_tooltip(tooltip, row, col, screen_rows, screen_cols)
        type(hover_tooltip_t), intent(inout) :: tooltip
        integer, intent(in) :: row, col
        integer, intent(in), optional :: screen_rows, screen_cols
        integer :: display_row, i, line_start, line_end, used, inner
        integer :: max_row, max_col
        character(len=:), allocatable :: line, shown

        if (.not. allocated(tooltip%content)) return
        if (len_trim(tooltip%content) == 0) return

        tooltip%row = row
        tooltip%col = col
        max_row = huge(1)
        max_col = huge(1)

        ! Clamp the box and, when the terminal has room, reserve the shared
        ! one-row/two-column shadow instead of clipping it at the edge.
        if (present(screen_rows) .and. tooltip%height > 0) then
            max_row = screen_rows
            if (screen_rows >= tooltip%height + 1 .and. &
                tooltip%row + tooltip%height > screen_rows) then
                tooltip%row = screen_rows - tooltip%height
            else if (tooltip%row + tooltip%height - 1 > screen_rows) then
                tooltip%row = screen_rows - tooltip%height + 1
            end if
            if (tooltip%row < 1) tooltip%row = 1
        end if
        if (present(screen_cols) .and. tooltip%width > 0) then
            max_col = screen_cols
            if (screen_cols >= tooltip%width + 2 .and. &
                tooltip%col + tooltip%width + 1 > screen_cols) then
                tooltip%col = screen_cols - tooltip%width - 1
            else if (tooltip%col + tooltip%width - 1 > screen_cols) then
                tooltip%col = screen_cols - tooltip%width + 1
            end if
            if (tooltip%col < 1) tooltip%col = 1
        end if

        tooltip%visible = .true.
        call box_shadow(tooltip%row, tooltip%col, tooltip%height, tooltip%width, &
                        max_row, max_col)

        ! Draw top border
        call terminal_move_cursor(tooltip%row, tooltip%col)
        call terminal_write(theme_sgr(THEME_BORDER) // "┌" // &
            repeat("─", tooltip%width - 2) // "┐" // theme_reset())

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

                line = ' ' // tooltip%content(line_start:line_end)
                inner = max(0, tooltip%width - 2)
                call clip_to_cells(line, inner, shown, used)
                call terminal_move_cursor(display_row, tooltip%col)
                call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                    theme_sgr(THEME_PANEL) // shown // &
                    repeat(' ', max(0, inner - used)) // &
                    theme_sgr(THEME_BORDER) // '│' // theme_reset())

                display_row = display_row + 1
                line_start = i + 1

                if (display_row - tooltip%row >= tooltip%height - 1) exit
            end if
        end do

        ! Draw bottom border
        call terminal_move_cursor(display_row, tooltip%col)
        call terminal_write(theme_sgr(THEME_BORDER) // "└" // &
            repeat("─", tooltip%width - 2) // "┘" // theme_reset())

        call region_add(REGION_BLOCK, tooltip%row, display_row, tooltip%col, &
                        tooltip%col + tooltip%width - 1)

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
