module code_actions_menu_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    implicit none
    private

    public :: code_actions_menu_t, code_action_t
    public :: init_code_actions_menu, cleanup_code_actions_menu
    public :: show_code_actions_menu, hide_code_actions_menu
    public :: is_code_actions_menu_visible, code_actions_menu_handle_key
    public :: set_code_actions, clear_code_actions
    public :: get_selected_action

    ! Code action type
    type :: code_action_t
        character(len=:), allocatable :: title
        character(len=:), allocatable :: kind  ! quickfix, refactor, etc.
        character(len=:), allocatable :: command
        logical :: is_preferred = .false.
        ! Store the full action JSON for applying later
        character(len=:), allocatable :: action_json
    end type code_action_t

    ! Code actions menu
    type :: code_actions_menu_t
        logical :: visible = .false.
        integer :: selected_index = 1
        integer :: row = 1
        integer :: col = 1

        ! Actions data
        type(code_action_t), allocatable :: actions(:)
        integer :: num_actions = 0

        ! Display settings
        integer :: max_width = 60
        integer :: max_visible = 10
        integer :: scroll_offset = 0
    end type code_actions_menu_t

contains

    subroutine init_code_actions_menu(menu)
        type(code_actions_menu_t), intent(out) :: menu

        menu%visible = .false.
        menu%selected_index = 1
        menu%scroll_offset = 0
        menu%num_actions = 0
        menu%max_width = 60
        menu%max_visible = 10
    end subroutine init_code_actions_menu

    subroutine cleanup_code_actions_menu(menu)
        type(code_actions_menu_t), intent(inout) :: menu
        integer :: i

        if (allocated(menu%actions)) then
            do i = 1, menu%num_actions
                if (allocated(menu%actions(i)%title)) deallocate(menu%actions(i)%title)
                if (allocated(menu%actions(i)%kind)) deallocate(menu%actions(i)%kind)
                if (allocated(menu%actions(i)%command)) deallocate(menu%actions(i)%command)
                if (allocated(menu%actions(i)%action_json)) deallocate(menu%actions(i)%action_json)
            end do
            deallocate(menu%actions)
        end if

        menu%num_actions = 0
        menu%selected_index = 1
        menu%scroll_offset = 0
    end subroutine cleanup_code_actions_menu

    subroutine set_code_actions(menu, actions, num_actions)
        type(code_actions_menu_t), intent(inout) :: menu
        type(code_action_t), intent(in) :: actions(:)
        integer, intent(in) :: num_actions
        integer :: i

        ! Clear existing actions
        call cleanup_code_actions_menu(menu)

        if (num_actions > 0) then
            allocate(menu%actions(num_actions))
            menu%num_actions = num_actions

            do i = 1, num_actions
                if (allocated(actions(i)%title)) then
                    allocate(character(len=len(actions(i)%title)) :: menu%actions(i)%title)
                    menu%actions(i)%title = actions(i)%title
                end if

                if (allocated(actions(i)%kind)) then
                    allocate(character(len=len(actions(i)%kind)) :: menu%actions(i)%kind)
                    menu%actions(i)%kind = actions(i)%kind
                end if

                if (allocated(actions(i)%command)) then
                    allocate(character(len=len(actions(i)%command)) :: menu%actions(i)%command)
                    menu%actions(i)%command = actions(i)%command
                end if

                if (allocated(actions(i)%action_json)) then
                    allocate(character(len=len(actions(i)%action_json)) :: menu%actions(i)%action_json)
                    menu%actions(i)%action_json = actions(i)%action_json
                end if

                menu%actions(i)%is_preferred = actions(i)%is_preferred
            end do
        end if

        menu%selected_index = 1
        menu%scroll_offset = 0
    end subroutine set_code_actions

    subroutine clear_code_actions(menu)
        type(code_actions_menu_t), intent(inout) :: menu
        call cleanup_code_actions_menu(menu)
    end subroutine clear_code_actions

    subroutine show_code_actions_menu(menu, row, col)
        type(code_actions_menu_t), intent(inout) :: menu
        integer, intent(in) :: row, col

        menu%row = row
        menu%col = col
        menu%visible = .true.
    end subroutine show_code_actions_menu

    subroutine hide_code_actions_menu(menu)
        type(code_actions_menu_t), intent(inout) :: menu
        menu%visible = .false.
    end subroutine hide_code_actions_menu

    function is_code_actions_menu_visible(menu) result(visible)
        type(code_actions_menu_t), intent(in) :: menu
        logical :: visible
        visible = menu%visible
    end function is_code_actions_menu_visible

    subroutine render_code_actions_menu(menu)
        type(code_actions_menu_t), intent(in) :: menu
        integer :: i, display_row, start_idx, end_idx
        character(len=256) :: line
        character(len=10) :: num_str
        character(len=3) :: icon

        if (.not. menu%visible .or. menu%num_actions == 0) return

        ! Calculate visible range
        start_idx = menu%scroll_offset + 1
        end_idx = min(menu%scroll_offset + menu%max_visible, menu%num_actions)

        ! Draw menu background and title
        display_row = menu%row
        call terminal_move_cursor(display_row, menu%col)
        call terminal_write(char(27) // '[48;5;238m')  ! Dark background

        ! Title bar
        line = " 💡 Code Actions "
        if (menu%num_actions > 1) then
            write(num_str, '(I0)') menu%num_actions
            line = trim(line) // "(" // trim(num_str) // ") "
        end if
        call terminal_write(char(27) // '[1m')  ! Bold
        call center_text(line, menu%max_width)
        call terminal_write(char(27) // '[0m')
        display_row = display_row + 1

        ! Separator
        call terminal_move_cursor(display_row, menu%col)
        call terminal_write(char(27) // '[48;5;238m')
        call terminal_write(repeat("─", menu%max_width))
        display_row = display_row + 1

        ! Display actions
        do i = start_idx, end_idx
            call terminal_move_cursor(display_row, menu%col)

            ! Highlight selected item
            if (i == menu%selected_index) then
                call terminal_write(char(27) // '[48;5;240m')  ! Highlight
            else
                call terminal_write(char(27) // '[48;5;236m')  ! Normal
            end if

            ! Get icon based on kind
            icon = get_action_icon(menu%actions(i)%kind)

            ! Format line with number shortcut
            if (i <= 9) then
                write(line, '(A1,I1,A,A,A,A)') " ", i, ". ", icon, " ", trim(menu%actions(i)%title)
            else
                write(line, '(A,A,A,A)') "    ", icon, " ", trim(menu%actions(i)%title)
            end if

            ! Add preferred indicator
            if (menu%actions(i)%is_preferred) then
                line = trim(line) // " ⭐"
            end if

            ! Truncate if too long
            if (len_trim(line) > menu%max_width - 2) then
                line = line(1:menu%max_width-5) // "..."
            end if

            ! Pad to full width
            line = line(1:menu%max_width)
            call terminal_write(line)

            display_row = display_row + 1
        end do

        ! Show scroll indicators if needed
        if (menu%num_actions > menu%max_visible) then
            call terminal_move_cursor(display_row, menu%col)
            call terminal_write(char(27) // '[48;5;238m')
            if (menu%scroll_offset > 0 .and. &
                menu%scroll_offset + menu%max_visible < menu%num_actions) then
                line = " ↑↓ "
            else if (menu%scroll_offset > 0) then
                line = " ↑ "
            else
                line = " ↓ "
            end if
            call center_text(line, menu%max_width)
            display_row = display_row + 1
        end if

        ! Bottom border
        call terminal_move_cursor(display_row, menu%col)
        call terminal_write(char(27) // '[48;5;238m')
        call terminal_write(repeat("─", menu%max_width))

        ! Reset colors
        call terminal_write(char(27) // '[0m')
    end subroutine render_code_actions_menu

    function get_action_icon(kind) result(icon)
        character(len=*), intent(in), optional :: kind
        character(len=3) :: icon

        if (.not. present(kind)) then
            icon = "•"
            return
        end if

        select case(trim(kind))
        case("quickfix")
            icon = "🔧"
        case("refactor")
            icon = "♻"
        case("refactor.extract")
            icon = "📦"
        case("refactor.inline")
            icon = "📥"
        case("refactor.rewrite")
            icon = "✏"
        case("source")
            icon = "📝"
        case("source.organizeImports")
            icon = "📚"
        case default
            icon = "•"
        end select
    end function get_action_icon

    subroutine center_text(text, width)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        integer :: padding, text_len
        character(len=256) :: padded

        text_len = len_trim(text)
        if (text_len >= width) then
            call terminal_write(text(1:width))
        else
            padding = (width - text_len) / 2
            padded = repeat(" ", padding) // trim(text) // repeat(" ", width - padding - text_len)
            call terminal_write(padded(1:width))
        end if
    end subroutine center_text

    function code_actions_menu_handle_key(menu, key) result(handled)
        type(code_actions_menu_t), intent(inout) :: menu
        character(len=*), intent(in) :: key
        logical :: handled
        integer :: num

        handled = .false.
        if (.not. menu%visible) return

        select case(trim(key))
        case('j', 'down')
            if (menu%selected_index < menu%num_actions) then
                menu%selected_index = menu%selected_index + 1
                ! Adjust scroll if needed
                if (menu%selected_index > menu%scroll_offset + menu%max_visible) then
                    menu%scroll_offset = menu%selected_index - menu%max_visible
                end if
                handled = .true.
            end if

        case('k', 'up')
            if (menu%selected_index > 1) then
                menu%selected_index = menu%selected_index - 1
                ! Adjust scroll if needed
                if (menu%selected_index <= menu%scroll_offset) then
                    menu%scroll_offset = max(0, menu%selected_index - 1)
                end if
                handled = .true.
            end if

        case('1':'9')
            ! Quick select by number
            read(key, '(I1)') num
            if (num <= menu%num_actions) then
                menu%selected_index = num
                handled = .true.
            end if

        case('enter')
            ! Action will be applied by caller
            handled = .true.

        case('escape')
            menu%visible = .false.
            handled = .true.
        end select
    end function code_actions_menu_handle_key

    function get_selected_action(menu, action_json) result(has_action)
        type(code_actions_menu_t), intent(in) :: menu
        character(len=:), allocatable, intent(out) :: action_json
        logical :: has_action

        has_action = .false.

        if (menu%selected_index > 0 .and. menu%selected_index <= menu%num_actions) then
            if (allocated(menu%actions(menu%selected_index)%action_json)) then
                allocate(character(len=len(menu%actions(menu%selected_index)%action_json)) :: action_json)
                action_json = menu%actions(menu%selected_index)%action_json
                has_action = .true.
            end if
        end if
    end function get_selected_action

end module code_actions_menu_module