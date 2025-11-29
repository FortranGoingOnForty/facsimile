module symbols_panel_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    implicit none
    private

    public :: symbols_panel_t
    public :: init_symbols_panel, cleanup_symbols_panel
    public :: show_symbols_panel, hide_symbols_panel, toggle_symbols_panel
    public :: is_symbols_panel_visible, symbols_panel_handle_key
    public :: set_symbols, clear_symbols
    public :: get_selected_symbol_location
    public :: document_symbol_t
    public :: render_symbols_panel

    ! LSP Symbol kinds
    integer, parameter :: SYMBOL_FILE = 1
    integer, parameter :: SYMBOL_MODULE = 2
    integer, parameter :: SYMBOL_NAMESPACE = 3
    integer, parameter :: SYMBOL_PACKAGE = 4
    integer, parameter :: SYMBOL_CLASS = 5
    integer, parameter :: SYMBOL_METHOD = 6
    integer, parameter :: SYMBOL_PROPERTY = 7
    integer, parameter :: SYMBOL_FIELD = 8
    integer, parameter :: SYMBOL_CONSTRUCTOR = 9
    integer, parameter :: SYMBOL_ENUM = 10
    integer, parameter :: SYMBOL_INTERFACE = 11
    integer, parameter :: SYMBOL_FUNCTION = 12
    integer, parameter :: SYMBOL_VARIABLE = 13
    integer, parameter :: SYMBOL_CONSTANT = 14
    integer, parameter :: SYMBOL_STRING = 15
    integer, parameter :: SYMBOL_NUMBER = 16
    integer, parameter :: SYMBOL_BOOLEAN = 17
    integer, parameter :: SYMBOL_ARRAY = 18
    integer, parameter :: SYMBOL_OBJECT = 19
    integer, parameter :: SYMBOL_KEY = 20
    integer, parameter :: SYMBOL_NULL = 21
    integer, parameter :: SYMBOL_ENUMMEMBER = 22
    integer, parameter :: SYMBOL_STRUCT = 23
    integer, parameter :: SYMBOL_EVENT = 24
    integer, parameter :: SYMBOL_OPERATOR = 25
    integer, parameter :: SYMBOL_TYPEPARAMETER = 26

    ! Document symbol type
    type :: document_symbol_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: detail  ! Additional info
        integer :: kind = SYMBOL_VARIABLE
        integer :: line = 1
        integer :: column = 1
        integer :: end_line = 1
        integer :: end_column = 1
        ! For hierarchical symbols
        type(document_symbol_t), allocatable :: children(:)
        integer :: num_children = 0
        integer :: depth = 0  ! Nesting level for display
        logical :: is_expanded = .true.  ! For tree view
    end type document_symbol_t

    ! Symbols panel
    type :: symbols_panel_t
        logical :: visible = .false.
        integer :: selected_index = 1
        integer :: panel_width = 40
        integer :: panel_start_col = 1

        ! Symbol data
        type(document_symbol_t), allocatable :: symbols(:)
        type(document_symbol_t), allocatable :: flat_symbols(:)  ! Flattened for navigation
        integer :: num_symbols = 0
        integer :: num_flat_symbols = 0

        ! Display settings
        integer :: scroll_offset = 0
        integer :: max_visible = 20
        logical :: show_details = .false.
    end type symbols_panel_t

contains

    subroutine init_symbols_panel(panel)
        type(symbols_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%num_symbols = 0
        panel%num_flat_symbols = 0
        panel%show_details = .false.
    end subroutine init_symbols_panel

    subroutine cleanup_symbols_panel(panel)
        type(symbols_panel_t), intent(inout) :: panel
        call clear_symbols(panel)
        panel%visible = .false.
    end subroutine cleanup_symbols_panel

    recursive subroutine cleanup_symbol(symbol)
        type(document_symbol_t), intent(inout) :: symbol
        integer :: i

        if (allocated(symbol%name)) deallocate(symbol%name)
        if (allocated(symbol%detail)) deallocate(symbol%detail)

        if (allocated(symbol%children)) then
            do i = 1, symbol%num_children
                call cleanup_symbol(symbol%children(i))
            end do
            deallocate(symbol%children)
        end if
    end subroutine cleanup_symbol

    subroutine clear_symbols(panel)
        type(symbols_panel_t), intent(inout) :: panel
        integer :: i

        if (allocated(panel%symbols)) then
            do i = 1, panel%num_symbols
                call cleanup_symbol(panel%symbols(i))
            end do
            deallocate(panel%symbols)
        end if

        if (allocated(panel%flat_symbols)) then
            ! Flat symbols are references, don't cleanup twice
            deallocate(panel%flat_symbols)
        end if

        panel%num_symbols = 0
        panel%num_flat_symbols = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine clear_symbols

    subroutine set_symbols(panel, symbols, num_symbols)
        type(symbols_panel_t), intent(inout) :: panel
        type(document_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: num_symbols
        integer :: i, flat_count

        ! Clear existing symbols
        call clear_symbols(panel)

        if (num_symbols > 0) then
            allocate(panel%symbols(num_symbols))
            panel%num_symbols = num_symbols

            ! Deep copy symbols
            do i = 1, num_symbols
                call copy_symbol(panel%symbols(i), symbols(i))
            end do

            ! Count total flat symbols
            flat_count = count_flat_symbols(panel%symbols, num_symbols)
            allocate(panel%flat_symbols(flat_count))
            panel%num_flat_symbols = flat_count

            ! Flatten for navigation
            call flatten_symbols(panel%symbols, num_symbols, panel%flat_symbols, flat_count)
        end if

        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine set_symbols

    recursive subroutine copy_symbol(dest, src)
        type(document_symbol_t), intent(out) :: dest
        type(document_symbol_t), intent(in) :: src
        integer :: i

        if (allocated(src%name)) then
            allocate(character(len=len(src%name)) :: dest%name)
            dest%name = src%name
        end if

        if (allocated(src%detail)) then
            allocate(character(len=len(src%detail)) :: dest%detail)
            dest%detail = src%detail
        end if

        dest%kind = src%kind
        dest%line = src%line
        dest%column = src%column
        dest%end_line = src%end_line
        dest%end_column = src%end_column
        dest%depth = src%depth
        dest%is_expanded = src%is_expanded
        dest%num_children = src%num_children

        if (allocated(src%children) .and. src%num_children > 0) then
            allocate(dest%children(src%num_children))
            do i = 1, src%num_children
                call copy_symbol(dest%children(i), src%children(i))
            end do
        end if
    end subroutine copy_symbol

    recursive function count_flat_symbols(symbols, num_symbols) result(count)
        type(document_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: num_symbols
        integer :: count, i

        count = num_symbols
        do i = 1, num_symbols
            if (allocated(symbols(i)%children) .and. symbols(i)%is_expanded) then
                count = count + count_flat_symbols(symbols(i)%children, symbols(i)%num_children)
            end if
        end do
    end function count_flat_symbols

    recursive subroutine flatten_symbols(symbols, num_symbols, flat_array, idx)
        type(document_symbol_t), intent(in), target :: symbols(:)
        integer, intent(in) :: num_symbols
        type(document_symbol_t), intent(out) :: flat_array(:)
        integer, intent(inout) :: idx
        integer :: i

        do i = 1, num_symbols
            if (idx <= size(flat_array)) then
                flat_array(idx) = symbols(i)
                idx = idx + 1

                if (allocated(symbols(i)%children) .and. symbols(i)%is_expanded) then
                    call flatten_symbols(symbols(i)%children, symbols(i)%num_children, flat_array, idx)
                end if
            end if
        end do
    end subroutine flatten_symbols

    subroutine show_symbols_panel(panel, screen_width, screen_height)
        type(symbols_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_width, screen_height

        panel%panel_width = min(50, screen_width / 3)
        panel%panel_start_col = screen_width - panel%panel_width + 1
        panel%max_visible = screen_height - 4  ! Leave room for title and border
        panel%visible = .true.
    end subroutine show_symbols_panel

    subroutine hide_symbols_panel(panel)
        type(symbols_panel_t), intent(inout) :: panel
        panel%visible = .false.
    end subroutine hide_symbols_panel

    subroutine toggle_symbols_panel(panel, screen_width, screen_height)
        type(symbols_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_width, screen_height

        if (panel%visible) then
            call hide_symbols_panel(panel)
        else
            call show_symbols_panel(panel, screen_width, screen_height)
        end if
    end subroutine toggle_symbols_panel

    function is_symbols_panel_visible(panel) result(visible)
        type(symbols_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_symbols_panel_visible

    subroutine render_symbols_panel(panel, screen_height)
        type(symbols_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_height
        integer :: row, i, start_idx, end_idx
        character(len=256) :: line
        character(len=20) :: location
        character(len=5) :: icon
        character(len=1), parameter :: ESC = char(27)

        if (.not. panel%visible) return

        ! Draw title bar with background color
        row = 1
        call terminal_move_cursor(row, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // ESC // '[1m')  ! Dark bg, bold
        line = " Document Symbols"
        if (panel%num_flat_symbols > 0) then
            write(line, '(A,I0,A)') trim(line) // " (", panel%num_flat_symbols, ")"
        end if
        call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
        call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
        call terminal_write(ESC // '[0m')

        ! Draw separator
        row = 2
        call terminal_move_cursor(row, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // repeat("-", panel%panel_width) // ESC // '[0m')

        ! Calculate visible range
        start_idx = panel%scroll_offset + 1
        end_idx = min(panel%scroll_offset + panel%max_visible, panel%num_flat_symbols)

        ! Render symbols
        if (panel%num_flat_symbols > 0) then
            do i = start_idx, end_idx
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)

                ! Background color based on selection
                if (i == panel%selected_index) then
                    call terminal_write(ESC // '[48;5;240m')  ! Highlight background
                else
                    call terminal_write(ESC // '[48;5;235m')  ! Normal background
                end if

                ! Get symbol icon
                icon = get_symbol_icon(panel%flat_symbols(i)%kind)

                ! Build line with indentation
                line = " "
                if (panel%flat_symbols(i)%depth > 0) then
                    line = trim(line) // repeat("  ", panel%flat_symbols(i)%depth)
                end if

                ! Add expansion indicator for containers with children
                if (allocated(panel%flat_symbols(i)%children) .and. &
                    panel%flat_symbols(i)%num_children > 0) then
                    if (panel%flat_symbols(i)%is_expanded) then
                        line = trim(line) // "v "
                    else
                        line = trim(line) // "> "
                    end if
                else
                    line = trim(line) // "  "
                end if

                ! Add icon and name
                line = trim(line) // icon // " " // trim(panel%flat_symbols(i)%name)

                ! Add location if room
                if (panel%show_details .or. i == panel%selected_index) then
                    write(location, '(A,I0)') " :", panel%flat_symbols(i)%line
                    if (len_trim(line) + len_trim(location) < panel%panel_width - 1) then
                        line = trim(line) // location
                    end if
                end if

                ! Write line and pad to width
                call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
                call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
                call terminal_write(ESC // '[0m')
            end do

            ! Fill empty rows
            do while (row < screen_height - 1)
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)
                call render_empty_line(panel%panel_width)
            end do
        else
            ! No symbols message
            row = row + 1
            call terminal_move_cursor(row, panel%panel_start_col)
            call terminal_write(ESC // '[48;5;235m' // ESC // '[90m')
            line = " No symbols found"
            call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
            call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
            call terminal_write(ESC // '[0m')

            do while (row < screen_height - 1)
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)
                call render_empty_line(panel%panel_width)
            end do
        end if

        ! Draw hint bar at bottom
        call terminal_move_cursor(screen_height, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // ESC // '[90m')
        line = " j/k:nav  Enter:jump  Esc:close"
        call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
        call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
        call terminal_write(ESC // '[0m')
    end subroutine render_symbols_panel

    function get_symbol_icon(kind) result(icon)
        integer, intent(in) :: kind
        character(len=5) :: icon

        select case(kind)
        case(SYMBOL_FILE)
            icon = "[F] "
        case(SYMBOL_MODULE)
            icon = "[M] "
        case(SYMBOL_NAMESPACE)
            icon = "[N] "
        case(SYMBOL_PACKAGE)
            icon = "[P] "
        case(SYMBOL_CLASS)
            icon = "[C] "
        case(SYMBOL_METHOD)
            icon = "m() "
        case(SYMBOL_PROPERTY)
            icon = ".p  "
        case(SYMBOL_FIELD)
            icon = ".f  "
        case(SYMBOL_CONSTRUCTOR)
            icon = "new "
        case(SYMBOL_ENUM)
            icon = "[E] "
        case(SYMBOL_INTERFACE)
            icon = "[I] "
        case(SYMBOL_FUNCTION)
            icon = "fn  "
        case(SYMBOL_VARIABLE)
            icon = "var "
        case(SYMBOL_CONSTANT)
            icon = "const"
        case(SYMBOL_STRING)
            icon = "str "
        case(SYMBOL_NUMBER)
            icon = "num "
        case(SYMBOL_BOOLEAN)
            icon = "bool"
        case(SYMBOL_ARRAY)
            icon = "[]  "
        case(SYMBOL_OBJECT)
            icon = "{}  "
        case(SYMBOL_STRUCT)
            icon = "[S] "
        case(SYMBOL_EVENT)
            icon = "evt "
        case(SYMBOL_OPERATOR)
            icon = "op  "
        case(SYMBOL_TYPEPARAMETER)
            icon = "<T> "
        case default
            icon = "-   "
        end select
    end function get_symbol_icon

    subroutine render_empty_line(width)
        integer, intent(in) :: width
        character(len=1), parameter :: ESC = char(27)

        call terminal_write(ESC // '[48;5;235m' // repeat(" ", width) // ESC // '[0m')
    end subroutine render_empty_line

    function symbols_panel_handle_key(panel, key) result(handled)
        type(symbols_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled

        handled = .false.
        if (.not. panel%visible) return

        select case(trim(key))
        case('j', 'down')
            if (panel%selected_index < panel%num_flat_symbols) then
                panel%selected_index = panel%selected_index + 1
                ! Adjust scroll if needed
                if (panel%selected_index > panel%scroll_offset + panel%max_visible) then
                    panel%scroll_offset = panel%selected_index - panel%max_visible
                end if
                handled = .true.
            end if

        case('k', 'up')
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
                ! Adjust scroll if needed
                if (panel%selected_index <= panel%scroll_offset) then
                    panel%scroll_offset = max(0, panel%selected_index - 1)
                end if
                handled = .true.
            end if

        case('g')
            ! Go to top
            panel%selected_index = 1
            panel%scroll_offset = 0
            handled = .true.

        case('G')
            ! Go to bottom
            if (panel%num_flat_symbols > 0) then
                panel%selected_index = panel%num_flat_symbols
                panel%scroll_offset = max(0, panel%num_flat_symbols - panel%max_visible)
                handled = .true.
            end if

        case('space', 'l', 'right')
            ! Toggle expansion of current symbol
            if (panel%selected_index > 0 .and. panel%selected_index <= panel%num_flat_symbols) then
                if (allocated(panel%flat_symbols(panel%selected_index)%children) .and. &
                    panel%flat_symbols(panel%selected_index)%num_children > 0) then
                    panel%flat_symbols(panel%selected_index)%is_expanded = &
                        .not. panel%flat_symbols(panel%selected_index)%is_expanded
                    ! Rebuild flat list
                    call rebuild_flat_list(panel)
                    handled = .true.
                end if
            end if

        case('h', 'left')
            ! Collapse current or parent
            handled = .true.

        case('d')
            ! Toggle details
            panel%show_details = .not. panel%show_details
            handled = .true.

        case('esc', 'escape')
            panel%visible = .false.
            handled = .true.

        case('enter')
            ! Jump to symbol handled by caller
            handled = .true.
        end select
    end function symbols_panel_handle_key

    subroutine rebuild_flat_list(panel)
        type(symbols_panel_t), intent(inout) :: panel
        integer :: flat_count, idx

        if (.not. allocated(panel%symbols)) return

        ! Count new flat size
        flat_count = count_flat_symbols(panel%symbols, panel%num_symbols)

        ! Reallocate if size changed
        if (flat_count /= panel%num_flat_symbols) then
            if (allocated(panel%flat_symbols)) deallocate(panel%flat_symbols)
            allocate(panel%flat_symbols(flat_count))
            panel%num_flat_symbols = flat_count
        end if

        ! Rebuild flat list
        idx = 1
        call flatten_symbols(panel%symbols, panel%num_symbols, panel%flat_symbols, idx)

        ! Adjust selected index if out of bounds
        if (panel%selected_index > panel%num_flat_symbols) then
            panel%selected_index = max(1, panel%num_flat_symbols)
        end if
    end subroutine rebuild_flat_list

    function get_selected_symbol_location(panel, line, col) result(has_location)
        type(symbols_panel_t), intent(in) :: panel
        integer(int32), intent(out) :: line, col
        logical :: has_location

        has_location = .false.
        line = 1
        col = 1

        if (panel%selected_index > 0 .and. panel%selected_index <= panel%num_flat_symbols) then
            line = panel%flat_symbols(panel%selected_index)%line
            col = panel%flat_symbols(panel%selected_index)%column
            has_location = .true.
        end if
    end function get_selected_symbol_location

end module symbols_panel_module