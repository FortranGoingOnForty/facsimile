module command_palette_module
    use iso_fortran_env, only: int32
    use terminal_io_module
    implicit none
    private

    public :: command_palette_t, command_t
    public :: init_command_palette, cleanup_command_palette
    public :: show_command_palette, hide_command_palette, show_command_palette_interactive
    public :: is_command_palette_visible, command_palette_handle_key
    public :: register_command, get_selected_command
    public :: filter_commands, render_command_palette

    integer, parameter :: MAX_COMMANDS = 100
    integer, parameter :: MAX_VISIBLE = 10

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
            deallocate(command_registry)
            command_registry = temp
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

                ! Bonus for matching at word start
                if (i == 1 .or. text(i-1:i-1) == ' ') then
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

    subroutine render_command_palette(palette, screen_rows)
        type(command_palette_t), intent(in) :: palette
        integer, intent(in) :: screen_rows
        integer :: i, visible_start, visible_end, row
        character(len=256) :: line, category_tag
        type(command_t) :: cmd

        ! Clear and draw header
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 2, 1)
        call terminal_write(repeat(' ', 80))
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 2, 1)
        call terminal_write('Command Palette (type to search, Esc to cancel)')

        ! Draw search query
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 1)
        call terminal_write(repeat(' ', 80))
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 1)
        call terminal_write('> ' // trim(palette%search_query))

        ! Calculate visible range
        visible_start = palette%scroll_offset + 1
        visible_end = min(visible_start + MAX_VISIBLE - 1, palette%num_filtered)

        ! Draw commands
        row = screen_rows - MAX_VISIBLE
        do i = visible_start, visible_end
            cmd = palette%filtered_commands(i)

            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', 80))
            call terminal_move_cursor(row, 1)

            ! Build line with category, name, and shortcut
            if (len_trim(cmd%category) > 0) then
                write(category_tag, '(A,A,A)') '[', trim(cmd%category), '] '
            else
                category_tag = ''
            end if

            if (i == palette%selected_index) then
                ! Highlight selected item
                write(line, '(A,A,A)') '> ', trim(category_tag), trim(cmd%name)
            else
                write(line, '(A,A,A)') '  ', trim(category_tag), trim(cmd%name)
            end if

            ! Add shortcut if available
            if (len_trim(cmd%shortcut) > 0) then
                write(line, '(A,A,A)') trim(line), ' (', trim(cmd%shortcut) // ')'
            end if

            call terminal_write(trim(line))
            row = row + 1
        end do

        ! Clear remaining lines
        do while (row <= screen_rows)
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', 80))
            row = row + 1
        end do

        ! Position cursor at end of search query
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 3 + palette%search_pos)
    end subroutine render_command_palette

    function show_command_palette_interactive(palette, screen_rows) result(selected_cmd_id)
        use input_handler_module, only: get_key_input
        type(command_palette_t), intent(inout) :: palette
        integer, intent(in) :: screen_rows
        character(len=:), allocatable :: selected_cmd_id
        character(len=32) :: key_input
        integer :: ch, status
        logical :: handled
        type(command_t) :: cmd

        call show_command_palette(palette)
        call render_command_palette(palette, screen_rows)

        do
            call get_key_input(key_input, status)
            if (status /= 0) cycle

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
                    call filter_commands(palette, trim(palette%search_query(1:palette%search_pos)))
                end if
            else
                ! Try navigation keys
                call command_palette_handle_key(palette, key_input, handled)
                if (.not. handled .and. len_trim(key_input) == 1) then
                    ! Regular character - add to search
                    ch = iachar(key_input(1:1))
                    if (ch >= 32 .and. ch < 127 .and. palette%search_pos < 255) then
                        palette%search_pos = palette%search_pos + 1
                        palette%search_query(palette%search_pos:palette%search_pos) = key_input(1:1)
                        call filter_commands(palette, trim(palette%search_query(1:palette%search_pos)))
                    end if
                end if
            end if

            call render_command_palette(palette, screen_rows)
        end do
    end function show_command_palette_interactive

end module command_palette_module
