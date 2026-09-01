module completion_popup_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use json_module, only: json_value_t, json_get_string, &
                           json_get_object, json_has_key, &
                           json_array_size, json_get_array_element, &
                           json_get_array, json_get_number
    use clickable_region_module, only: region_add, REGION_BLOCK
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_BORDER, THEME_MUTED, THEME_PANEL, &
        THEME_PANEL_SELECTION, theme_glyph, theme_reset, theme_sgr
    implicit none
    private

    public :: completion_popup_t
    public :: init_completion_popup, cleanup_completion_popup
    public :: show_completion_popup, hide_completion_popup
    public :: render_completion_popup
    public :: handle_completion_response
    public :: navigate_completion_up, navigate_completion_down
    public :: get_selected_completion, is_completion_visible

    integer, parameter :: MAX_VISIBLE_ITEMS = 10
    integer, parameter :: MAX_LABEL_WIDTH = 40

    type :: completion_item_t
        character(len=:), allocatable :: label       ! Main text to display
        character(len=:), allocatable :: kind        ! Variable, Function, etc.
        character(len=:), allocatable :: detail      ! Additional info
        character(len=:), allocatable :: insert_text ! Text to insert when selected
    end type completion_item_t

    type :: completion_popup_t
        type(completion_item_t), allocatable :: items(:)
        integer :: item_count = 0
        integer :: selected_index = 1
        integer :: scroll_offset = 0
        logical :: visible = .false.

        ! Position on screen
        integer :: row = 0
        integer :: col = 0
        integer :: width = 0
        integer :: height = 0
    end type completion_popup_t

contains

    subroutine init_completion_popup(popup)
        type(completion_popup_t), intent(out) :: popup

        popup%item_count = 0
        popup%selected_index = 1
        popup%scroll_offset = 0
        popup%visible = .false.
        popup%row = 0
        popup%col = 0
        popup%width = MAX_LABEL_WIDTH + 10
        popup%height = 0

        if (allocated(popup%items)) deallocate(popup%items)
    end subroutine init_completion_popup

    subroutine cleanup_completion_popup(popup)
        type(completion_popup_t), intent(inout) :: popup
        integer :: i

        if (allocated(popup%items)) then
            do i = 1, popup%item_count
                if (allocated(popup%items(i)%label)) deallocate(popup%items(i)%label)
                if (allocated(popup%items(i)%kind)) deallocate(popup%items(i)%kind)
                if (allocated(popup%items(i)%detail)) deallocate(popup%items(i)%detail)
                if (allocated(popup%items(i)%insert_text)) deallocate(popup%items(i)%insert_text)
            end do
            deallocate(popup%items)
        end if

        popup%item_count = 0
        popup%visible = .false.
    end subroutine cleanup_completion_popup

    ! Handle LSP completion response and populate popup
    subroutine handle_completion_response(popup, response)
        type(completion_popup_t), intent(inout) :: popup
        type(json_value_t), intent(in) :: response
        type(json_value_t) :: items_array, item
        integer :: i, n_items, kind_num
        character(len=:), allocatable :: label, detail, insert_text

        call cleanup_completion_popup(popup)

        ! Check if response has items array
        if (json_has_key(response, "items")) then
            items_array = json_get_array(response, "items")
            n_items = json_array_size(items_array)

            ! Limit to max visible items for now
            n_items = min(n_items, MAX_VISIBLE_ITEMS * 2)

            if (n_items > 0) then
                allocate(popup%items(n_items))
                popup%item_count = n_items

                do i = 1, n_items
                    item = json_get_array_element(items_array, i-1)

                    ! Extract completion item fields
                    label = json_get_string(item, "label")
                    if (len_trim(label) > 0) then
                        popup%items(i)%label = label
                    else
                        popup%items(i)%label = "Item " // char(48 + mod(i-1, 10))
                    end if

                    ! Get completion kind (number mapping to type)
                    if (json_has_key(item, "kind")) then
                        kind_num = int(json_get_number(item, "kind"))
                        select case(kind_num)
                        case(1)
                            popup%items(i)%kind = "Text"
                        case(2)
                            popup%items(i)%kind = "Method"
                        case(3)
                            popup%items(i)%kind = "Function"
                        case(4)
                            popup%items(i)%kind = "Constructor"
                        case(5)
                            popup%items(i)%kind = "Field"
                        case(6)
                            popup%items(i)%kind = "Variable"
                        case(7)
                            popup%items(i)%kind = "Class"
                        case(8)
                            popup%items(i)%kind = "Interface"
                        case(9)
                            popup%items(i)%kind = "Module"
                        case(10)
                            popup%items(i)%kind = "Property"
                        case(14)
                            popup%items(i)%kind = "Keyword"
                        case default
                            popup%items(i)%kind = ""
                        end select
                    else
                        popup%items(i)%kind = ""
                    end if

                    ! Get detail text if available
                    if (json_has_key(item, "detail")) then
                        detail = json_get_string(item, "detail")
                        popup%items(i)%detail = detail
                    else
                        popup%items(i)%detail = ""
                    end if

                    ! Get insert text or fall back to label
                    if (json_has_key(item, "insertText")) then
                        insert_text = json_get_string(item, "insertText")
                        popup%items(i)%insert_text = insert_text
                    else
                        popup%items(i)%insert_text = popup%items(i)%label
                    end if
                end do

                popup%selected_index = 1
                popup%scroll_offset = 0
                popup%height = min(popup%item_count, MAX_VISIBLE_ITEMS) + 2  ! +2 for borders
            end if
        end if
    end subroutine handle_completion_response

    subroutine show_completion_popup(popup, row, col, screen_rows, screen_cols)
        type(completion_popup_t), intent(inout) :: popup
        integer, intent(in) :: row, col
        integer, intent(in), optional :: screen_rows, screen_cols

        if (popup%item_count == 0) return

        popup%row = row
        popup%col = col

        ! Clamp the box inside the screen when dimensions are known —
        ! near the bottom/right edge (or at small terminals) an unclamped
        ! popup draws off-screen or wraps onto the next line
        if (present(screen_rows)) then
            if (popup%row + popup%height - 1 > screen_rows) then
                popup%row = screen_rows - popup%height + 1
            end if
            if (popup%row < 1) popup%row = 1
        end if
        if (present(screen_cols)) then
            if (popup%col + popup%width - 1 > screen_cols) then
                popup%col = screen_cols - popup%width + 1
            end if
            if (popup%col < 1) popup%col = 1
        end if

        popup%visible = .true.

        call render_completion_popup(popup)
    end subroutine show_completion_popup

    ! Draw the popup at its stored position. Also called from render_screen
    ! each frame so the box survives full-screen redraws.
    subroutine render_completion_popup(popup)
        type(completion_popup_t), intent(in) :: popup
        integer :: i, display_row, start_idx, end_idx, used, inner
        character(len=:), allocatable :: line, shown

        if (.not. popup%visible .or. popup%item_count == 0) return

        ! Calculate visible range
        start_idx = popup%scroll_offset + 1
        end_idx = min(popup%scroll_offset + MAX_VISIBLE_ITEMS, popup%item_count)

        ! Draw top border
        call terminal_move_cursor(popup%row, popup%col)
        call terminal_write(theme_sgr(THEME_BORDER) // "┌" // &
            repeat("─", popup%width - 2) // "┐" // theme_reset())

        ! Draw items
        display_row = popup%row + 1
        do i = start_idx, end_idx
            call terminal_move_cursor(display_row, popup%col)

            line = ' ' // theme_glyph('symbol') // ' ' // trim(popup%items(i)%label)
            if (allocated(popup%items(i)%kind)) then
                if (len_trim(popup%items(i)%kind) > 0) line = line // &
                    '  ' // trim(popup%items(i)%kind)
            end if
            inner = max(0, popup%width - 2)
            call clip_to_cells(line, inner, shown, used)
            call terminal_write(theme_sgr(THEME_BORDER) // '│')
            if (i == popup%selected_index) then
                call terminal_write(theme_sgr(THEME_PANEL_SELECTION))
            else
                call terminal_write(theme_sgr(THEME_PANEL))
            end if
            call terminal_write(shown // repeat(' ', max(0, inner - used)))
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())

            display_row = display_row + 1
        end do

        ! Draw bottom border
        call terminal_move_cursor(display_row, popup%col)
        call terminal_write(theme_sgr(THEME_BORDER) // "└" // &
            repeat("─", popup%width - 2) // "┘" // theme_reset())

        ! Claim the box so a click on it is swallowed instead of moving the
        ! caret in the document underneath -- which used to leave the popup
        ! open over a caret that had moved out from under it, so the next
        ! Enter inserted the completion in the wrong place. display_row is
        ! the bottom border, the last row actually drawn.
        call region_add(REGION_BLOCK, popup%row, display_row, &
                        popup%col, popup%col + popup%width - 1)

    end subroutine render_completion_popup

    subroutine hide_completion_popup(popup)
        type(completion_popup_t), intent(inout) :: popup
        popup%visible = .false.
    end subroutine hide_completion_popup

    subroutine navigate_completion_up(popup)
        type(completion_popup_t), intent(inout) :: popup

        if (.not. popup%visible .or. popup%item_count == 0) return

        if (popup%selected_index > 1) then
            popup%selected_index = popup%selected_index - 1

            ! Adjust scroll if needed
            if (popup%selected_index <= popup%scroll_offset) then
                popup%scroll_offset = popup%selected_index - 1
            end if
        else
            ! Wrap to bottom
            popup%selected_index = popup%item_count
            popup%scroll_offset = max(0, popup%item_count - MAX_VISIBLE_ITEMS)
        end if
    end subroutine navigate_completion_up

    subroutine navigate_completion_down(popup)
        type(completion_popup_t), intent(inout) :: popup

        if (.not. popup%visible .or. popup%item_count == 0) return

        if (popup%selected_index < popup%item_count) then
            popup%selected_index = popup%selected_index + 1

            ! Adjust scroll if needed
            if (popup%selected_index > popup%scroll_offset + MAX_VISIBLE_ITEMS) then
                popup%scroll_offset = popup%selected_index - MAX_VISIBLE_ITEMS
            end if
        else
            ! Wrap to top
            popup%selected_index = 1
            popup%scroll_offset = 0
        end if
    end subroutine navigate_completion_down

    function get_selected_completion(popup) result(text)
        type(completion_popup_t), intent(in) :: popup
        character(len=:), allocatable :: text

        if (popup%visible .and. popup%item_count > 0 .and. &
            popup%selected_index > 0 .and. popup%selected_index <= popup%item_count) then

            if (allocated(popup%items(popup%selected_index)%insert_text)) then
                text = popup%items(popup%selected_index)%insert_text
            else
                text = popup%items(popup%selected_index)%label
            end if
        else
            text = ""
        end if
    end function get_selected_completion

    function is_completion_visible(popup) result(visible)
        type(completion_popup_t), intent(in) :: popup
        logical :: visible

        visible = popup%visible
    end function is_completion_visible

end module completion_popup_module
