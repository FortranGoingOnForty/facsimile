module command_palette_module
    use iso_fortran_env, only: int32
    use terminal_io_module
    use theme_module, only: THEME_ACCENT, THEME_BORDER, THEME_MUTED, THEME_PANEL, &
        THEME_PANEL_HEADER, THEME_PANEL_SELECTION, theme_paint, theme_reset, theme_sgr
    implicit none
    private

    public :: command_palette_t, command_t
    public :: init_command_palette, cleanup_command_palette
    public :: show_command_palette, hide_command_palette, show_command_palette_interactive
    public :: is_command_palette_visible, command_palette_handle_key
    public :: register_command, get_selected_command
    public :: filter_commands, render_command_palette
    public :: command_palette_row_at

    integer, parameter :: MAX_COMMANDS = 100
    integer, parameter :: MAX_VISIBLE = 10
    integer, parameter :: PALETTE_WIDTH = 60  ! Fixed width for centered palette

    type :: command_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: shortcut
        character(len=:), allocatable :: category
        character(len=:), allocatable :: command_id
        integer :: score = 0  ! For fuzzy matching
    end type command_t

    type :: command_palette_t
        logical :: visible = .false.
        type(command_t), allocatable :: all_commands(:)
        integer :: num_commands = 0
        type(command_t), allocatable :: filtered_commands(:)
        integer :: num_filtered = 0
        integer :: selected_index = 1
        integer :: scroll_offset = 0
        character(len=256) :: search_query = ''
        integer :: search_pos = 0
    end type command_palette_t

    ! Module-level command registry
    type(command_t), allocatable :: command_registry(:)
    integer :: registry_size = 0

contains

    subroutine init_command_palette(palette)
        type(command_palette_t), intent(out) :: palette

        palette%visible = .false.
        palette%num_commands = 0
        palette%num_filtered = 0
        palette%selected_index = 1
        palette%scroll_offset = 0
        palette%search_query = ''
        palette%search_pos = 0

        allocate(palette%all_commands(MAX_COMMANDS))
        allocate(palette%filtered_commands(MAX_COMMANDS))
    end subroutine init_command_palette

    subroutine cleanup_command_palette(palette)
        type(command_palette_t), intent(inout) :: palette
        integer :: i

        if (allocated(palette%all_commands)) then
            do i = 1, palette%num_commands
                if (allocated(palette%all_commands(i)%name)) deallocate(palette%all_commands(i)%name)
                if (allocated(palette%all_commands(i)%shortcut)) deallocate(palette%all_commands(i)%shortcut)
                if (allocated(palette%all_commands(i)%category)) deallocate(palette%all_commands(i)%category)
                if (allocated(palette%all_commands(i)%command_id)) deallocate(palette%all_commands(i)%command_id)
            end do
            deallocate(palette%all_commands)
        end if

        if (allocated(palette%filtered_commands)) deallocate(palette%filtered_commands)
    end subroutine cleanup_command_palette

    subroutine register_command(name, command_id, shortcut, category)
        character(len=*), intent(in) :: name, command_id
        character(len=*), intent(in), optional :: shortcut, category
        type(command_t), allocatable :: temp(:)
        integer :: i

        ! Grow registry if needed
        if (.not. allocated(command_registry)) then
            allocate(command_registry(20))
            registry_size = 0
        else if (registry_size >= size(command_registry)) then
            allocate(temp(size(command_registry) * 2))
            do i = 1, registry_size
                temp(i) = command_registry(i)
            end do
            ! move_alloc, not assignment: assigning would deep-copy every
            ! entry a second time and leave temp allocated until the
            ! procedure ends. This hands the allocation over and leaves temp
            ! deallocated, which is also what stops older gfortran reading
            ! the branch as "temp may be used uninitialised".
            call move_alloc(temp, command_registry)
        end if

        ! Add command
        registry_size = registry_size + 1
        command_registry(registry_size)%name = name
        command_registry(registry_size)%command_id = command_id

        if (present(shortcut)) then
            command_registry(registry_size)%shortcut = shortcut
        else
            command_registry(registry_size)%shortcut = ''
        end if

        if (present(category)) then
            command_registry(registry_size)%category = category
        else
            command_registry(registry_size)%category = ''
        end if
    end subroutine register_command

    subroutine show_command_palette(palette)
        type(command_palette_t), intent(inout) :: palette
        integer :: i

        palette%visible = .true.
        palette%search_query = ''
        palette%search_pos = 0
        palette%selected_index = 1
        palette%scroll_offset = 0

        ! Load all registered commands
        palette%num_commands = registry_size
        do i = 1, registry_size
            palette%all_commands(i) = command_registry(i)
        end do

        ! Initially show all commands
        call filter_commands(palette, '')
    end subroutine show_command_palette

    subroutine hide_command_palette(palette)
        type(command_palette_t), intent(inout) :: palette
        palette%visible = .false.
    end subroutine hide_command_palette

    function is_command_palette_visible(palette) result(visible)
        type(command_palette_t), intent(in) :: palette
        logical :: visible
        visible = palette%visible
    end function is_command_palette_visible

    subroutine filter_commands(palette, query)
        type(command_palette_t), intent(inout) :: palette
        character(len=*), intent(in) :: query
        integer :: i, score

        palette%num_filtered = 0

        do i = 1, palette%num_commands
            score = fuzzy_match_score(palette%all_commands(i)%name, query)
            if (score > 0) then
                palette%num_filtered = palette%num_filtered + 1
                palette%filtered_commands(palette%num_filtered) = palette%all_commands(i)
                palette%filtered_commands(palette%num_filtered)%score = score
            end if
        end do

        ! Sort by score (simple bubble sort for now)
        call sort_commands_by_score(palette%filtered_commands, palette%num_filtered)

        ! Reset selection
        palette%selected_index = 1
        palette%scroll_offset = 0
    end subroutine filter_commands

    function fuzzy_match_score(text, pattern) result(score)
        character(len=*), intent(in) :: text, pattern
        integer :: score
        integer :: i, j, text_len, pattern_len
        integer :: consecutive_matches
        character :: text_lower, pattern_lower

        score = 0
        text_len = len_trim(text)
        pattern_len = len_trim(pattern)

        ! Empty pattern matches everything
        if (pattern_len == 0) then
            score = 100
            return
        end if

        j = 1
        consecutive_matches = 0

        do i = 1, text_len
            if (j > pattern_len) exit

            ! Case-insensitive comparison
            text_lower = to_lower(text(i:i))
            pattern_lower = to_lower(pattern(j:j))

            if (text_lower == pattern_lower) then
                score = score + 10
                consecutive_matches = consecutive_matches + 1

                ! Bonus for consecutive matches
                if (consecutive_matches > 1) then
                    score = score + 5
                end if

                ! Bonus for matching at word start.
                !
                ! Nested, not `i == 1 .or. text(i-1:i-1) == ' '`: Fortran does
                ! not promise to short-circuit .or., and gfortran evaluates
                ! both sides -- so at i == 1 that reads text(0:0), one
                ! character before the string. Harmless in a release build,
                ! which is why it sat here unnoticed, and an immediate
                ! "substring out of bounds" abort under -fcheck=all.
                if (i == 1) then
                    score = score + 15
                else if (text(i-1:i-1) == ' ') then
                    score = score + 15
                end if

                j = j + 1
            else
                consecutive_matches = 0
            end if
        end do

        ! Only match if all pattern characters were found
        if (j <= pattern_len) then
            score = 0
        end if
    end function fuzzy_match_score

    function to_lower(ch) result(lower)
        character, intent(in) :: ch
        character :: lower
        integer :: code

        code = iachar(ch)
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower = achar(code + 32)
        else
            lower = ch
        end if
    end function to_lower

    subroutine sort_commands_by_score(commands, count)
        type(command_t), intent(inout) :: commands(:)
        integer, intent(in) :: count
        type(command_t) :: temp
        integer :: i, j

        ! Bubble sort (good enough for small lists)
        do i = 1, count - 1
            do j = i + 1, count
                if (commands(j)%score > commands(i)%score) then
                    temp = commands(i)
                    commands(i) = commands(j)
                    commands(j) = temp
                end if
            end do
        end do
    end subroutine sort_commands_by_score

    function get_selected_command(palette) result(cmd)
        type(command_palette_t), intent(in) :: palette
        type(command_t) :: cmd

        if (palette%num_filtered > 0 .and. palette%selected_index <= palette%num_filtered) then
            cmd = palette%filtered_commands(palette%selected_index)
        else
            cmd%command_id = ''
        end if
    end function get_selected_command

    subroutine command_palette_handle_key(palette, key, handled)
        type(command_palette_t), intent(inout) :: palette
        character(len=*), intent(in) :: key
        logical, intent(out) :: handled

        handled = .true.

        select case(key)
        case('up', 'ctrl-k', 'k')
            if (palette%selected_index > 1) then
                palette%selected_index = palette%selected_index - 1
                if (palette%selected_index < palette%scroll_offset + 1) then
                    palette%scroll_offset = palette%selected_index - 1
                end if
            end if

        case('down', 'ctrl-j', 'j')
            if (palette%selected_index < palette%num_filtered) then
                palette%selected_index = palette%selected_index + 1
                if (palette%selected_index > palette%scroll_offset + MAX_VISIBLE) then
                    palette%scroll_offset = palette%selected_index - MAX_VISIBLE
                end if
            end if

        case('esc')
            call hide_command_palette(palette)

        case default
            handled = .false.
        end select
    end subroutine command_palette_handle_key

    subroutine render_command_palette(palette, screen_cols)
        type(command_palette_t), intent(in) :: palette
        integer, intent(in) :: screen_cols
        integer :: i, visible_start, visible_end, row, start_col, start_row
        integer :: content_width, display_width
        character(len=256) :: line, category_tag
        type(command_t) :: cmd
        character(len=:), allocatable :: border_top, border_bottom
        ! Calculate centering - top-center like VSCode
        content_width = min(PALETTE_WIDTH, screen_cols - 4)
        start_col = max(1, (screen_cols - content_width) / 2)
        start_row = 2  ! Start near top (below tab bar if present)

        ! Build border strings
        border_top = '┌' // repeat('─', content_width - 2) // '┐'
        border_bottom = '└' // repeat('─', content_width - 2) // '┘'

        ! Draw top border
        call terminal_move_cursor(start_row, start_col)
        call terminal_write(theme_paint(THEME_BORDER, border_top))

        ! Draw header line with cyan title
        row = start_row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_PANEL_HEADER) // &
            theme_sgr(THEME_BORDER) // '│' // theme_sgr(THEME_PANEL_HEADER))
        call terminal_write(' Command Palette')
        display_width = 16  ! " Command Palette" visible length
        call terminal_write(repeat(' ', max(0, content_width - 2 - display_width)) // &
            theme_sgr(THEME_BORDER) // '│' // theme_reset())

        ! Draw search query line with yellow prompt
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_PANEL) // theme_sgr(THEME_BORDER) // '│' // &
            theme_sgr(THEME_ACCENT) // ' > ' // theme_sgr(THEME_PANEL))
        ! Not trim(): a trailing space is part of what the user typed, and
        ! hiding it makes the space bar look broken.
        if (palette%search_pos > 0) then
            call terminal_write(palette%search_query(1:palette%search_pos))
        end if
        display_width = 3 + palette%search_pos  ! " > " + query length
        call terminal_write(repeat(' ', max(0, content_width - 2 - display_width)))
        call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())

        ! Draw separator
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_paint(THEME_BORDER, '├' // repeat('─', content_width - 2) // '┤'))

        ! Calculate visible range
        visible_start = palette%scroll_offset + 1
        visible_end = min(visible_start + MAX_VISIBLE - 1, palette%num_filtered)

        ! Draw commands
        do i = visible_start, visible_end
            row = row + 1
            cmd = palette%filtered_commands(i)

            call terminal_move_cursor(row, start_col)
            call terminal_write(theme_sgr(THEME_BORDER) // '│')

            ! Build line with category, name, and shortcut (for display_width calculation)
            if (len_trim(cmd%category) > 0) then
                write(category_tag, '(A,A,A)') '[', trim(cmd%category), '] '
            else
                category_tag = ''
            end if

            write(line, '(A,A,A)') ' ', trim(category_tag), trim(cmd%name)

            ! Add shortcut if available
            if (len_trim(cmd%shortcut) > 0) then
                write(line, '(A,A,A)') trim(line), '  ', trim(cmd%shortcut)
            end if

            ! Calculate visible width (actual characters, no ANSI codes)
            display_width = len_trim(line)

            ! Ensure it fits
            if (display_width > content_width - 2) then
                display_width = content_width - 2
            end if

            ! Calculate padding
            display_width = max(0, min(display_width, content_width - 2))

            if (i == palette%selected_index) then
                ! Highlight selected item with inverse colors
                call terminal_write(theme_sgr(THEME_PANEL_SELECTION))
                call terminal_write(line(1:display_width))
                call terminal_write(repeat(' ', max(0, content_width - 2 - display_width)))
                call terminal_write(theme_reset())
            else
                call terminal_write(theme_sgr(THEME_PANEL) // line(1:display_width))
                call terminal_write(repeat(' ', max(0, content_width - 2 - display_width)))
                call terminal_write(theme_reset())
            end if

            call terminal_write(theme_paint(THEME_BORDER, '│'))
        end do

        ! Fill remaining visible slots with empty rows
        do i = visible_end + 1, visible_start + MAX_VISIBLE - 1
            row = row + 1
            call terminal_move_cursor(row, start_col)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_sgr(THEME_PANEL) // &
                repeat(' ', content_width - 2) // theme_sgr(THEME_BORDER) // '│' // theme_reset())
        end do

        ! Draw bottom border
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_paint(THEME_BORDER, border_bottom))

        ! Position cursor at end of search query (inside the box)
        call terminal_move_cursor(start_row + 2, start_col + 4 + palette%search_pos)
    end subroutine render_command_palette


    !> Which filtered command is drawn at screen (row, col), or 0.
    !>
    !> The row arithmetic mirrors render_command_palette exactly: start_row is
    !> 2, then border, header, query and separator occupy the next three, so
    !> the first command sits on row 6. Kept beside the renderer so the two
    !> cannot drift apart.
    function command_palette_row_at(palette, screen_cols, row, col) result(idx)
        type(command_palette_t), intent(in) :: palette
        integer, intent(in) :: screen_cols, row, col
        integer :: idx
        integer :: content_width, start_col, first_row, visible_end, offset

        idx = 0
        if (.not. palette%visible) return

        content_width = min(PALETTE_WIDTH, screen_cols - 4)
        start_col = max(1, (screen_cols - content_width) / 2)
        if (col < start_col .or. col > start_col + content_width - 1) return

        first_row = 2 + 4          ! top border, header, query, separator
        if (row < first_row) return

        offset = row - first_row   ! 0-based position within the visible list
        if (offset > MAX_VISIBLE - 1) return

        visible_end = min(palette%scroll_offset + MAX_VISIBLE, palette%num_filtered)
        idx = palette%scroll_offset + 1 + offset
        if (idx > visible_end) idx = 0
    end function command_palette_row_at

    function show_command_palette_interactive(palette, screen_cols) result(selected_cmd_id)
        use input_handler_module, only: get_key_input, parse_mouse_event
        type(command_palette_t), intent(inout) :: palette
        integer, intent(in) :: screen_cols
        character(len=:), allocatable :: selected_cmd_id
        character(len=32) :: key_input
        integer :: ch, status
        logical :: handled, is_text
        type(command_t) :: cmd

        call show_command_palette(palette)
        call render_command_palette(palette, screen_cols)
        call terminal_flush()

        do
            call get_key_input(key_input, status)
            if (status /= 0) cycle

            ! The palette runs its own input loop, so the renderer's clickable
            ! region table -- consulted only by the main key router -- never
            ! sees these events. Resolve them here against the same geometry.
            if (index(key_input, 'mouse-') == 1) then
                block
                    character(len=16) :: ev
                    integer :: btn, mrow, mcol, hit
                    logical :: mok
                    call parse_mouse_event(trim(key_input), ev, btn, mrow, mcol, mok)
                    if (mok) then
                        hit = command_palette_row_at(palette, screen_cols, mrow, mcol)
                        if (trim(ev) == 'mouse-click' .and. hit > 0) then
                            ! Run the row under the pointer, not whatever the
                            ! keyboard had highlighted.
                            palette%selected_index = hit
                            cmd = get_selected_command(palette)
                            if (allocated(cmd%command_id)) then
                                if (len_trim(cmd%command_id) > 0) then
                                    selected_cmd_id = cmd%command_id
                                    call hide_command_palette(palette)
                                    return
                                end if
                            end if
                        else if (trim(ev) == 'mouse-click') then
                            ! A click outside the box dismisses, as clicking off
                            ! any other menu does.
                            selected_cmd_id = ''
                            call hide_command_palette(palette)
                            return
                        else if (trim(ev) == 'mouse-drag' .or. trim(ev) == 'mouse-release') then
                            if (hit > 0) palette%selected_index = hit
                        end if
                    end if
                end block
                call render_command_palette(palette, screen_cols)
                call terminal_flush()
                cycle
            end if

            ! Handle special keys
            if (key_input == 'enter') then
                cmd = get_selected_command(palette)
                if (allocated(cmd%command_id) .and. len_trim(cmd%command_id) > 0) then
                    selected_cmd_id = cmd%command_id
                    call hide_command_palette(palette)
                    return
                end if
            else if (key_input == 'esc') then
                selected_cmd_id = ''
                call hide_command_palette(palette)
                return
            else if (key_input == 'backspace') then
                if (palette%search_pos > 0) then
                    palette%search_query(palette%search_pos:palette%search_pos) = ' '
                    palette%search_pos = palette%search_pos - 1
                    call filter_commands(palette, palette%search_query(1:palette%search_pos))
                end if
            else
                ! Try navigation keys
                call command_palette_handle_key(palette, key_input, handled)
                ! A space arrives as a blank key_input, so len_trim() is 0 and
                ! the old `== 1` guard dropped it -- which made every
                ! multi-word query ("close pane") impossible to type.
                is_text = (len_trim(key_input) == 1) .or. &
                          (len_trim(key_input) == 0 .and. key_input(1:1) == ' ')
                if (.not. handled .and. is_text) then
                    ! Regular character - add to search
                    ch = iachar(key_input(1:1))
                    if (ch >= 32 .and. ch < 127 .and. palette%search_pos < 255) then
                        palette%search_pos = palette%search_pos + 1
                        palette%search_query(palette%search_pos:palette%search_pos) = key_input(1:1)
                        call filter_commands(palette, palette%search_query(1:palette%search_pos))
                    end if
                end if
            end if

            call render_command_palette(palette, screen_cols)
            call terminal_flush()
        end do
    end function show_command_palette_interactive

end module command_palette_module
