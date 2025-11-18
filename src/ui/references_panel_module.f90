module references_panel_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    implicit none
    private

    public :: references_panel_t, reference_location_t
    public :: init_references_panel, cleanup_references_panel
    public :: show_references_panel, hide_references_panel, toggle_references_panel
    public :: is_references_panel_visible, references_panel_handle_key
    public :: set_references, clear_references
    public :: get_selected_reference_location
    public :: render_references_panel

    ! Reference location
    type :: reference_location_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: filename  ! Extracted from URI
        integer(int32) :: line
        integer(int32) :: column
        integer(int32) :: end_line
        integer(int32) :: end_column
        character(len=:), allocatable :: preview_text  ! Line content for preview
    end type reference_location_t

    ! References panel
    type :: references_panel_t
        logical :: visible = .false.
        integer :: width = 50  ! Panel width in columns
        integer :: selected_index = 1
        integer :: scroll_offset = 0

        ! References data
        type(reference_location_t), allocatable :: references(:)
        integer :: num_references = 0
        character(len=:), allocatable :: symbol_name

        ! Screen position
        integer :: screen_width = 80
        integer :: screen_height = 24
    end type references_panel_t

contains

    subroutine init_references_panel(panel)
        type(references_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%width = 50
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%num_references = 0
        panel%screen_width = 80
        panel%screen_height = 24
    end subroutine init_references_panel

    subroutine cleanup_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        integer :: i

        if (allocated(panel%references)) then
            do i = 1, panel%num_references
                if (allocated(panel%references(i)%uri)) deallocate(panel%references(i)%uri)
                if (allocated(panel%references(i)%filename)) deallocate(panel%references(i)%filename)
                if (allocated(panel%references(i)%preview_text)) deallocate(panel%references(i)%preview_text)
            end do
            deallocate(panel%references)
        end if

        if (allocated(panel%symbol_name)) deallocate(panel%symbol_name)

        panel%num_references = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine cleanup_references_panel

    subroutine set_references(panel, references, num_refs, symbol_name)
        type(references_panel_t), intent(inout) :: panel
        type(reference_location_t), intent(in) :: references(:)
        integer, intent(in) :: num_refs
        character(len=*), intent(in), optional :: symbol_name
        integer :: i

        ! Clear existing references
        call cleanup_references_panel(panel)

        ! Allocate and copy new references
        if (num_refs > 0) then
            allocate(panel%references(num_refs))
            panel%num_references = num_refs

            do i = 1, num_refs
                if (allocated(references(i)%uri)) then
                    allocate(character(len=len(references(i)%uri)) :: panel%references(i)%uri)
                    panel%references(i)%uri = references(i)%uri
                end if

                if (allocated(references(i)%filename)) then
                    allocate(character(len=len(references(i)%filename)) :: panel%references(i)%filename)
                    panel%references(i)%filename = references(i)%filename
                end if

                if (allocated(references(i)%preview_text)) then
                    allocate(character(len=len(references(i)%preview_text)) :: panel%references(i)%preview_text)
                    panel%references(i)%preview_text = references(i)%preview_text
                end if

                panel%references(i)%line = references(i)%line
                panel%references(i)%column = references(i)%column
                panel%references(i)%end_line = references(i)%end_line
                panel%references(i)%end_column = references(i)%end_column
            end do
        end if

        ! Set symbol name
        if (present(symbol_name)) then
            if (allocated(panel%symbol_name)) deallocate(panel%symbol_name)
            allocate(character(len=len_trim(symbol_name)) :: panel%symbol_name)
            panel%symbol_name = trim(symbol_name)
        end if

        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine set_references

    subroutine clear_references(panel)
        type(references_panel_t), intent(inout) :: panel
        call cleanup_references_panel(panel)
    end subroutine clear_references

    subroutine show_references_panel(panel, screen_width, screen_height)
        type(references_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_width, screen_height

        panel%screen_width = screen_width
        panel%screen_height = screen_height
        panel%visible = .true.
    end subroutine show_references_panel

    subroutine hide_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        panel%visible = .false.
    end subroutine hide_references_panel

    subroutine toggle_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        panel%visible = .not. panel%visible
    end subroutine toggle_references_panel

    function is_references_panel_visible(panel) result(visible)
        type(references_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_references_panel_visible

    subroutine render_references_panel(panel, start_row)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: start_row
        integer :: row, col, start_col
        integer :: i, visible_index, max_visible
        character(len=256) :: line
        character(len=100) :: header, location_str
        character(len=:), allocatable :: display_text

        if (.not. panel%visible) return

        ! Calculate panel position (right side of screen)
        start_col = panel%screen_width - panel%width + 1
        max_visible = panel%screen_height - start_row - 2  ! Leave room for header/footer

        ! Draw panel border and header
        row = start_row

        ! Header with symbol name
        call terminal_move_cursor(row, start_col)
        call terminal_write(char(27) // '[48;5;237m')  ! Dark background

        if (allocated(panel%symbol_name)) then
            write(header, '(A,A,A,I0,A)') " References: ", trim(panel%symbol_name), &
                " (", panel%num_references, ") "
        else
            write(header, '(A,I0,A)') " References (", panel%num_references, ") "
        end if

        ! Truncate header if too long
        if (len_trim(header) > panel%width) then
            header = header(1:panel%width-3) // "..."
        end if

        ! Center the header
        col = start_col + (panel%width - len_trim(header)) / 2
        call terminal_move_cursor(row, col)
        call terminal_write(char(27) // '[1m' // trim(header) // char(27) // '[0m')

        ! Clear to end of header line
        call terminal_move_cursor(row, start_col + len_trim(header))
        call render_empty_line(start_col + len_trim(header), &
            panel%width - len_trim(header))

        row = row + 1

        ! Draw separator
        call terminal_move_cursor(row, start_col)
        call terminal_write(char(27) // '[48;5;237m' // repeat("─", panel%width) // char(27) // '[0m')
        row = row + 1

        ! Display references
        if (panel%num_references == 0) then
            call terminal_move_cursor(row, start_col)
            call terminal_write(char(27) // '[48;5;235m' // char(27) // '[90m')
            call terminal_write(" No references found")
            call terminal_write(char(27) // '[K')  ! Clear to end of line
            call terminal_write(char(27) // '[0m')
        else
            ! Display visible references
            do i = 1, min(max_visible, panel%num_references - panel%scroll_offset)
                visible_index = panel%scroll_offset + i

                if (visible_index > panel%num_references) exit

                call terminal_move_cursor(row, start_col)

                ! Highlight selected item
                if (visible_index == panel%selected_index) then
                    call terminal_write(char(27) // '[48;5;240m')  ! Highlight background
                else
                    call terminal_write(char(27) // '[48;5;235m')  ! Normal background
                end if

                ! Format location string
                if (allocated(panel%references(visible_index)%filename)) then
                    write(location_str, '(A,A,I0,A,I0)') &
                        trim(get_basename(panel%references(visible_index)%filename)), &
                        ":", panel%references(visible_index)%line, &
                        ":", panel%references(visible_index)%column
                else
                    write(location_str, '(I0,A,I0)') &
                        panel%references(visible_index)%line, &
                        ":", panel%references(visible_index)%column
                end if

                ! Truncate location if needed
                if (len_trim(location_str) > 20) then
                    location_str = location_str(1:17) // "..."
                end if

                ! Format display line
                write(line, '(A2,A20,A)') " ", adjustl(location_str), " "

                ! Add preview text if available
                if (allocated(panel%references(visible_index)%preview_text)) then
                    display_text = trim(panel%references(visible_index)%preview_text)
                    if (len(display_text) > panel%width - 25) then
                        display_text = display_text(1:panel%width-28) // "..."
                    end if
                    line = trim(line) // display_text
                end if

                ! Ensure line fits in panel width
                if (len_trim(line) > panel%width) then
                    line = line(1:panel%width)
                end if

                call terminal_write(line(1:panel%width))
                call terminal_write(char(27) // '[0m')

                row = row + 1
            end do

            ! Clear remaining lines
            do i = row, start_row + max_visible + 1
                if (i > panel%screen_height - 1) exit
                call terminal_move_cursor(i, start_col)
                call render_empty_line(start_col, panel%width)
            end do

            ! Show scroll indicator if needed
            if (panel%num_references > max_visible) then
                call terminal_move_cursor(start_row + max_visible + 2, start_col)
                call terminal_write(char(27) // '[48;5;237m' // char(27) // '[90m')
                write(line, '(A,I0,A,I0,A)') " [", panel%selected_index, "/", panel%num_references, "] "
                if (panel%scroll_offset > 0) then
                    line = trim(line) // "↑"
                end if
                if (panel%scroll_offset + max_visible < panel%num_references) then
                    line = trim(line) // "↓"
                end if
                call terminal_write(trim(line))
                call terminal_write(char(27) // '[0m')
            end if
        end if
    end subroutine render_references_panel

    subroutine render_empty_line(start_col, width)
        integer, intent(in) :: start_col, width
        call terminal_write(char(27) // '[48;5;235m' // repeat(" ", width) // char(27) // '[0m')
    end subroutine render_empty_line

    function references_panel_handle_key(panel, key) result(handled)
        type(references_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled
        integer :: max_visible

        handled = .false.
        if (.not. panel%visible) return

        max_visible = panel%screen_height - 4

        select case(trim(key))
        case('j', 'down')
            ! Move selection down
            if (panel%selected_index < panel%num_references) then
                panel%selected_index = panel%selected_index + 1

                ! Adjust scroll if needed
                if (panel%selected_index > panel%scroll_offset + max_visible) then
                    panel%scroll_offset = panel%selected_index - max_visible
                end if
                handled = .true.
            end if

        case('k', 'up')
            ! Move selection up
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1

                ! Adjust scroll if needed
                if (panel%selected_index <= panel%scroll_offset) then
                    panel%scroll_offset = max(0, panel%selected_index - 1)
                end if
                handled = .true.
            end if

        case('pagedown')
            ! Page down
            panel%selected_index = min(panel%num_references, &
                panel%selected_index + max_visible)
            panel%scroll_offset = min(max(0, panel%num_references - max_visible), &
                panel%scroll_offset + max_visible)
            handled = .true.

        case('pageup')
            ! Page up
            panel%selected_index = max(1, panel%selected_index - max_visible)
            panel%scroll_offset = max(0, panel%scroll_offset - max_visible)
            handled = .true.

        case('home')
            ! Jump to first
            panel%selected_index = 1
            panel%scroll_offset = 0
            handled = .true.

        case('end')
            ! Jump to last
            panel%selected_index = panel%num_references
            panel%scroll_offset = max(0, panel%num_references - max_visible)
            handled = .true.

        case('enter')
            ! User wants to jump to this reference
            handled = .true.

        case('escape', 'shift-f12')
            ! Hide panel
            panel%visible = .false.
            handled = .true.
        end select
    end function references_panel_handle_key

    function get_selected_reference_location(panel, uri, line, col) result(has_location)
        type(references_panel_t), intent(in) :: panel
        character(len=:), allocatable, intent(out) :: uri
        integer(int32), intent(out) :: line, col
        logical :: has_location

        has_location = .false.

        if (panel%selected_index > 0 .and. panel%selected_index <= panel%num_references) then
            if (allocated(panel%references(panel%selected_index)%uri)) then
                allocate(character(len=len(panel%references(panel%selected_index)%uri)) :: uri)
                uri = panel%references(panel%selected_index)%uri
                line = panel%references(panel%selected_index)%line
                col = panel%references(panel%selected_index)%column
                has_location = .true.
            end if
        end if
    end function get_selected_reference_location

    ! Helper function to extract basename from path
    function get_basename(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: basename
        integer :: last_slash

        last_slash = index(path, '/', back=.true.)
        if (last_slash > 0) then
            basename = path(last_slash+1:)
        else
            basename = path
        end if
    end function get_basename

end module references_panel_module