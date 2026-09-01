module code_actions_panel_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use clickable_region_module, only: region_add, REGION_BLOCK
    use theme_module, only: THEME_INFO, THEME_MUTED, THEME_PANEL, THEME_PANEL_FOOTER, &
        THEME_PANEL_HEADER, THEME_PANEL_SELECTION, THEME_SUCCESS, THEME_WARNING, &
        theme_reset, theme_sgr
    implicit none
    private

    public :: code_actions_panel_t, code_action_t
    public :: init_code_actions_panel, cleanup_code_actions_panel
    public :: show_code_actions_panel, hide_code_actions_panel
    public :: toggle_code_actions_panel
    public :: is_code_actions_panel_visible, code_actions_panel_handle_key
    public :: set_code_actions, clear_code_actions
    public :: get_selected_action, render_code_actions_panel

    ! Code action type
    type :: code_action_t
        character(len=:), allocatable :: title
        character(len=:), allocatable :: kind  ! quickfix, refactor, etc.
        character(len=:), allocatable :: command
        logical :: is_preferred = .false.
        ! Store the full action JSON for applying later
        character(len=:), allocatable :: action_json
    end type code_action_t

    ! Code actions panel (offcanvas on right side)
    type :: code_actions_panel_t
        logical :: visible = .false.
        integer :: width = 45  ! Panel width in columns
        integer :: selected_index = 1
        integer :: scroll_offset = 0

        ! Actions data
        type(code_action_t), allocatable :: actions(:)
        integer :: num_actions = 0
    end type code_actions_panel_t

contains

    subroutine init_code_actions_panel(panel)
        type(code_actions_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%width = 45
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%num_actions = 0
    end subroutine init_code_actions_panel

    subroutine cleanup_code_actions_panel(panel)
        type(code_actions_panel_t), intent(inout) :: panel
        integer :: i

        if (allocated(panel%actions)) then
            do i = 1, panel%num_actions
                if (allocated(panel%actions(i)%title)) deallocate(panel%actions(i)%title)
                if (allocated(panel%actions(i)%kind)) deallocate(panel%actions(i)%kind)
                if (allocated(panel%actions(i)%command)) deallocate(panel%actions(i)%command)
                if (allocated(panel%actions(i)%action_json)) deallocate(panel%actions(i)%action_json)
            end do
            deallocate(panel%actions)
        end if

        panel%num_actions = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine cleanup_code_actions_panel

    subroutine set_code_actions(panel, actions, num_actions)
        type(code_actions_panel_t), intent(inout) :: panel
        type(code_action_t), intent(in) :: actions(:)
        integer, intent(in) :: num_actions
        integer :: i

        ! Clear existing actions
        call cleanup_code_actions_panel(panel)

        if (num_actions > 0) then
            allocate(panel%actions(num_actions))
            panel%num_actions = num_actions

            do i = 1, num_actions
                if (allocated(actions(i)%title)) then
                    allocate(character(len=len(actions(i)%title)) :: panel%actions(i)%title)
                    panel%actions(i)%title = actions(i)%title
                end if

                if (allocated(actions(i)%kind)) then
                    allocate(character(len=len(actions(i)%kind)) :: panel%actions(i)%kind)
                    panel%actions(i)%kind = actions(i)%kind
                end if

                if (allocated(actions(i)%command)) then
                    allocate(character(len=len(actions(i)%command)) :: panel%actions(i)%command)
                    panel%actions(i)%command = actions(i)%command
                end if

                if (allocated(actions(i)%action_json)) then
                    allocate(character(len=len(actions(i)%action_json)) :: panel%actions(i)%action_json)
                    panel%actions(i)%action_json = actions(i)%action_json
                end if

                panel%actions(i)%is_preferred = actions(i)%is_preferred
            end do
        end if

        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine set_code_actions

    subroutine clear_code_actions(panel)
        type(code_actions_panel_t), intent(inout) :: panel
        call cleanup_code_actions_panel(panel)
    end subroutine clear_code_actions

    subroutine show_code_actions_panel(panel)
        type(code_actions_panel_t), intent(inout) :: panel
        panel%visible = .true.
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine show_code_actions_panel

    subroutine hide_code_actions_panel(panel)
        type(code_actions_panel_t), intent(inout) :: panel
        panel%visible = .false.
    end subroutine hide_code_actions_panel

    subroutine toggle_code_actions_panel(panel)
        type(code_actions_panel_t), intent(inout) :: panel
        panel%visible = .not. panel%visible
        if (panel%visible) then
            panel%selected_index = 1
            panel%scroll_offset = 0
        end if
    end subroutine toggle_code_actions_panel

    function is_code_actions_panel_visible(panel) result(visible)
        type(code_actions_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_code_actions_panel_visible

    subroutine render_code_actions_panel(panel, screen_rows, screen_cols)
        type(code_actions_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_rows, screen_cols
        integer :: start_col, row, i, max_content_lines
        character(len=256) :: line_buffer
        character(len=10) :: kind_icon
        character(len=:), allocatable :: kind_color

        if (.not. panel%visible) return

        ! Calculate panel position (right side)
        start_col = screen_cols - panel%width + 1
        if (start_col < 1) start_col = 1

        ! Initialize line buffer
        line_buffer = repeat(' ', len(line_buffer))

        ! Draw panel header
        row = 1
        call terminal_move_cursor(row, start_col)
        write(line_buffer, '(A,I0,A)') ' Code Actions (', panel%num_actions, ') '
        call terminal_write(theme_sgr(THEME_PANEL_HEADER) // trim(line_buffer) // theme_reset())

        ! Pad header to width
        call terminal_move_cursor(row, start_col + len_trim(line_buffer))
        call terminal_write(theme_sgr(THEME_PANEL_HEADER) // &
            repeat(' ', panel%width - len_trim(line_buffer)) // theme_reset())

        ! Separator
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_PANEL_HEADER) // repeat('─', panel%width) // theme_reset())

        ! Content area
        row = row + 1
        max_content_lines = screen_rows - 4

        ! Display "No code actions" if empty
        if (panel%num_actions == 0) then
            call terminal_move_cursor(row, start_col)
            call terminal_write(theme_sgr(THEME_MUTED))
            line_buffer = ' No code actions available'
            call pad_to_width(line_buffer, panel%width)
            call terminal_write(line_buffer(1:panel%width))
            call terminal_write(theme_reset())

            ! Fill remaining lines
            do i = row + 1, screen_rows - 1
                call terminal_move_cursor(i, start_col)
                call terminal_write(theme_sgr(THEME_PANEL) // repeat(' ', panel%width) // theme_reset())
            end do
            return
        end if

        ! Render code actions
        do i = 1, min(panel%num_actions, max_content_lines)
            call terminal_move_cursor(row, start_col)
            line_buffer = repeat(' ', len(line_buffer))

            ! Get icon and color based on kind
            call get_action_display(panel%actions(i)%kind, kind_icon, kind_color)

            ! Highlight selected item
            if (i == panel%selected_index) then
                call terminal_write(theme_sgr(THEME_PANEL_SELECTION))
            else
                call terminal_write(theme_sgr(THEME_PANEL))
            end if

            ! Format: " [icon] Title "
            if (i <= 9) then
                write(line_buffer, '(A1,I1,A,A,A,A)') ' ', i, '. ', trim(kind_icon), ' ', trim(panel%actions(i)%title)
            else
                write(line_buffer, '(A,A,A,A)') '    ', trim(kind_icon), ' ', trim(panel%actions(i)%title)
            end if

            ! Add preferred indicator
            if (panel%actions(i)%is_preferred) then
                line_buffer = trim(line_buffer) // ' *'
            end if

            ! Truncate if too long
            if (len_trim(line_buffer) > panel%width - 1) then
                line_buffer = line_buffer(1:panel%width - 4) // '...'
            end if

            ! Pad to width
            call pad_to_width(line_buffer, panel%width)

            ! Apply kind color to icon
            if (i /= panel%selected_index) call terminal_write(trim(kind_color))
            call terminal_write(line_buffer(1:panel%width))
            call terminal_write(theme_reset())

            row = row + 1
        end do

        ! Fill remaining lines with empty background
        do i = row, screen_rows - 1
            call terminal_move_cursor(i, start_col)
            call terminal_write(theme_sgr(THEME_PANEL) // repeat(' ', panel%width) // theme_reset())
        end do

        ! Footer with hints
        call terminal_move_cursor(screen_rows - 1, start_col)
        call terminal_write(theme_sgr(THEME_PANEL_FOOTER))
        line_buffer = ' j/k:Navigate  Enter:Apply  Esc:Close'
        call pad_to_width(line_buffer, panel%width)
        call terminal_write(line_buffer(1:panel%width))
        call terminal_write(theme_reset())

        ! Claim the strip so clicks cannot reach the document behind it
        call region_add(REGION_BLOCK, 1, screen_rows, start_col, &
                        start_col + panel%width - 1)

    end subroutine render_code_actions_panel

    subroutine get_action_display(kind, icon, color)
        character(len=*), intent(in), optional :: kind
        character(len=10), intent(out) :: icon
        character(len=:), allocatable, intent(out) :: color

        icon = ''
        color = ''

        if (.not. present(kind)) return
        if (len_trim(kind) == 0) return

        select case(trim(kind))
        case('quickfix')
            icon = '[fix]'
            color = theme_sgr(THEME_WARNING)
        case('refactor')
            icon = '[ref]'
            color = theme_sgr(THEME_INFO)
        case('refactor.extract')
            icon = '[ext]'
            color = theme_sgr(THEME_INFO)
        case('refactor.inline')
            icon = '[inl]'
            color = theme_sgr(THEME_INFO)
        case('refactor.rewrite')
            icon = '[rw]'
            color = theme_sgr(THEME_INFO)
        case('source')
            icon = '[src]'
            color = theme_sgr(THEME_SUCCESS)
        case('source.organizeImports', 'source.organizeImports.ruff')
            icon = '[org]'
            color = theme_sgr(THEME_SUCCESS)
        case('source.fixAll', 'source.fixAll.ruff')
            icon = '[all]'
            color = theme_sgr(THEME_SUCCESS)
        case default
            icon = ''
            color = theme_sgr(THEME_PANEL)
        end select
    end subroutine get_action_display

    subroutine pad_to_width(buffer, width)
        character(len=*), intent(inout) :: buffer
        integer, intent(in) :: width
        integer :: current_len, i

        current_len = len_trim(buffer)
        do i = current_len + 1, width
            if (i <= len(buffer)) buffer(i:i) = ' '
        end do
    end subroutine pad_to_width

    function code_actions_panel_handle_key(panel, key) result(handled)
        type(code_actions_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled
        integer :: num

        handled = .false.
        if (.not. panel%visible) return

        select case(trim(key))
        case('j', 'down')
            if (panel%selected_index < panel%num_actions) then
                panel%selected_index = panel%selected_index + 1
            end if
            handled = .true.

        case('k', 'up')
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
            end if
            handled = .true.

        case('1':'9')
            ! Quick select by number
            read(key, '(I1)') num
            if (num <= panel%num_actions) then
                panel%selected_index = num
            end if
            handled = .true.

        case('enter')
            ! Action will be applied by caller
            handled = .true.

        case('escape', 'esc', 'q')
            panel%visible = .false.
            handled = .true.
        end select
    end function code_actions_panel_handle_key

    function get_selected_action(panel, action_json) result(has_action)
        type(code_actions_panel_t), intent(in) :: panel
        character(len=:), allocatable, intent(out) :: action_json
        logical :: has_action

        has_action = .false.

        if (panel%selected_index > 0 .and. panel%selected_index <= panel%num_actions) then
            if (allocated(panel%actions(panel%selected_index)%action_json)) then
                allocate(character(len=len(panel%actions(panel%selected_index)%action_json)) :: action_json)
                action_json = panel%actions(panel%selected_index)%action_json
                has_action = .true.
            end if
        end if
    end function get_selected_action

end module code_actions_panel_module
