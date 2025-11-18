module completion_popup_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use json_module, only: json_value_t, json_get_string, &
                           json_get_object, json_has_key
    implicit none
    private

    public :: completion_popup_t
    public :: init_completion_popup, cleanup_completion_popup
    public :: show_completion_popup, hide_completion_popup
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
        type(json_value_t) :: items_array, item, text_edit
        integer :: i, n_items
        character(len=:), allocatable :: label, kind_str, detail, insert_text

        call cleanup_completion_popup(popup)

        ! Check if response has items array
        if (json_has_key(response, "items")) then
            items_array = json_get_object(response, "items")
            ! TODO: Get array count properly
            n_items = 10  ! Placeholder - need to implement array size getter

            if (n_items > 0) then
                allocate(popup%items(n_items))
                popup%item_count = n_items

                do i = 1, n_items
                    ! TODO: Get array element properly
                    ! item = json_get_array_element(items_array, i-1)

                    ! Extract completion item fields
                    popup%items(i)%label = "Item " // char(48 + i)  ! Placeholder
                    popup%items(i)%kind = "Variable"
                    popup%items(i)%detail = ""
                    popup%items(i)%insert_text = popup%items(i)%label
                end do

                popup%selected_index = 1
                popup%scroll_offset = 0
                popup%height = min(popup%item_count, MAX_VISIBLE_ITEMS) + 2  ! +2 for borders
            end if
        end if
    end subroutine handle_completion_response

    subroutine show_completion_popup(popup, row, col)
        type(completion_popup_t), intent(inout) :: popup
        integer, intent(in) :: row, col
        integer :: i, display_row, start_idx, end_idx
        character(len=256) :: line

        if (popup%item_count == 0) return

        popup%row = row
        popup%col = col
        popup%visible = .true.

        ! Calculate visible range
        start_idx = popup%scroll_offset + 1
        end_idx = min(popup%scroll_offset + MAX_VISIBLE_ITEMS, popup%item_count)

        ! Draw top border
        call terminal_move_cursor(popup%row, popup%col)
        call terminal_write("┌" // repeat("─", popup%width - 2) // "┐")

        ! Draw items
        display_row = popup%row + 1
        do i = start_idx, end_idx
            call terminal_move_cursor(display_row, popup%col)

            ! Format item line
            if (i == popup%selected_index) then
                ! Highlight selected item with > marker
                write(line, '(a,a)') "│>", adjustl(popup%items(i)%label)
                call terminal_write(line(1:min(len_trim(line), popup%width - 1)))
                call terminal_move_cursor(display_row, popup%col + popup%width - 1)
                call terminal_write("│")
            else
                write(line, '(a,a)') "│ ", adjustl(popup%items(i)%label)
                call terminal_write(line(1:min(len_trim(line), popup%width - 1)))
                call terminal_move_cursor(display_row, popup%col + popup%width - 1)
                call terminal_write("│")
            end if

            display_row = display_row + 1
        end do

        ! Draw bottom border
        call terminal_move_cursor(display_row, popup%col)
        call terminal_write("└" // repeat("─", popup%width - 2) // "┘")

    end subroutine show_completion_popup

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