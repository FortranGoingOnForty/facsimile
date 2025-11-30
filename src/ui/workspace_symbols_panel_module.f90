module workspace_symbols_panel_module
    use iso_fortran_env, only: int32
    use terminal_io_module
    implicit none
    private

    public :: workspace_symbols_panel_t, workspace_symbol_t
    public :: init_workspace_symbols_panel, cleanup_workspace_symbols_panel
    public :: show_workspace_symbols_panel, hide_workspace_symbols_panel
    public :: is_workspace_symbols_panel_visible, workspace_symbols_panel_handle_key
    public :: set_workspace_symbols, get_selected_symbol
    public :: render_workspace_symbols_panel, get_search_query
    public :: add_search_char, delete_search_char, clear_search

    integer, parameter :: MAX_SYMBOLS = 1000

    type :: workspace_symbol_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: kind_name  ! "Function", "Class", etc.
        character(len=:), allocatable :: container_name
        character(len=:), allocatable :: file_uri
        character(len=:), allocatable :: file_path  ! Extracted from URI
        integer :: line = 0
        integer :: column = 0
        integer :: kind = 0
        integer :: score = 0  ! For fuzzy matching
    end type workspace_symbol_t

    type :: workspace_symbols_panel_t
        logical :: visible = .false.
        integer :: panel_width = 60
        integer :: panel_start_col = 1
        integer :: max_visible = 20
        type(workspace_symbol_t), allocatable :: all_symbols(:)
        integer :: num_symbols = 0
        type(workspace_symbol_t), allocatable :: filtered_symbols(:)
        integer :: num_filtered = 0
        integer :: selected_index = 1
        integer :: scroll_offset = 0
        character(len=256) :: search_query = ''
        integer :: search_pos = 0
        logical :: needs_lsp_query = .false.  ! Flag to trigger LSP request
    end type workspace_symbols_panel_t

contains

    subroutine init_workspace_symbols_panel(panel)
        type(workspace_symbols_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%num_symbols = 0
        panel%num_filtered = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%search_query = ''
        panel%search_pos = 0
        panel%needs_lsp_query = .false.

        allocate(panel%all_symbols(MAX_SYMBOLS))
        allocate(panel%filtered_symbols(MAX_SYMBOLS))
    end subroutine init_workspace_symbols_panel

    subroutine cleanup_workspace_symbols_panel(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        integer :: i

        if (allocated(panel%all_symbols)) then
            do i = 1, panel%num_symbols
                if (allocated(panel%all_symbols(i)%name)) deallocate(panel%all_symbols(i)%name)
                if (allocated(panel%all_symbols(i)%kind_name)) deallocate(panel%all_symbols(i)%kind_name)
                if (allocated(panel%all_symbols(i)%container_name)) deallocate(panel%all_symbols(i)%container_name)
                if (allocated(panel%all_symbols(i)%file_uri)) deallocate(panel%all_symbols(i)%file_uri)
                if (allocated(panel%all_symbols(i)%file_path)) deallocate(panel%all_symbols(i)%file_path)
            end do
            deallocate(panel%all_symbols)
        end if

        if (allocated(panel%filtered_symbols)) deallocate(panel%filtered_symbols)
    end subroutine cleanup_workspace_symbols_panel

    subroutine show_workspace_symbols_panel(panel, screen_width, screen_height)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_width, screen_height

        panel%visible = .true.
        panel%panel_width = min(70, screen_width / 2)
        panel%panel_start_col = screen_width - panel%panel_width + 1
        panel%max_visible = screen_height - 5  ! Room for header, search, hints
        panel%search_query = ''
        panel%search_pos = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%needs_lsp_query = .true.  ! Request initial symbols

        ! Initially show all symbols
        call filter_symbols(panel)
    end subroutine show_workspace_symbols_panel

    subroutine hide_workspace_symbols_panel(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        panel%visible = .false.
    end subroutine hide_workspace_symbols_panel

    function is_workspace_symbols_panel_visible(panel) result(visible)
        type(workspace_symbols_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_workspace_symbols_panel_visible

    function get_search_query(panel) result(query)
        type(workspace_symbols_panel_t), intent(in) :: panel
        character(len=:), allocatable :: query
        if (panel%search_pos > 0) then
            query = trim(panel%search_query(1:panel%search_pos))
        else
            query = ''
        end if
    end function get_search_query

    subroutine add_search_char(panel, ch)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        character, intent(in) :: ch

        if (panel%search_pos < 255) then
            panel%search_pos = panel%search_pos + 1
            panel%search_query(panel%search_pos:panel%search_pos) = ch
            panel%needs_lsp_query = .true.
            call filter_symbols(panel)
        end if
    end subroutine add_search_char

    subroutine delete_search_char(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel

        if (panel%search_pos > 0) then
            panel%search_query(panel%search_pos:panel%search_pos) = ' '
            panel%search_pos = panel%search_pos - 1
            panel%needs_lsp_query = .true.
            call filter_symbols(panel)
        end if
    end subroutine delete_search_char

    subroutine clear_search(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        panel%search_query = ''
        panel%search_pos = 0
        panel%needs_lsp_query = .true.
        call filter_symbols(panel)
    end subroutine clear_search

    subroutine set_workspace_symbols(panel, symbols, count)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        type(workspace_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: count
        integer :: i, copy_count

        copy_count = min(count, MAX_SYMBOLS)
        panel%num_symbols = copy_count

        do i = 1, copy_count
            ! Deep copy each symbol
            if (allocated(panel%all_symbols(i)%name)) deallocate(panel%all_symbols(i)%name)
            if (allocated(symbols(i)%name)) then
                allocate(character(len=len(symbols(i)%name)) :: panel%all_symbols(i)%name)
                panel%all_symbols(i)%name = symbols(i)%name
            end if

            if (allocated(panel%all_symbols(i)%kind_name)) deallocate(panel%all_symbols(i)%kind_name)
            if (allocated(symbols(i)%kind_name)) then
                allocate(character(len=len(symbols(i)%kind_name)) :: panel%all_symbols(i)%kind_name)
                panel%all_symbols(i)%kind_name = symbols(i)%kind_name
            end if

            if (allocated(panel%all_symbols(i)%container_name)) deallocate(panel%all_symbols(i)%container_name)
            if (allocated(symbols(i)%container_name)) then
                allocate(character(len=len(symbols(i)%container_name)) :: panel%all_symbols(i)%container_name)
                panel%all_symbols(i)%container_name = symbols(i)%container_name
            end if

            if (allocated(panel%all_symbols(i)%file_uri)) deallocate(panel%all_symbols(i)%file_uri)
            if (allocated(symbols(i)%file_uri)) then
                allocate(character(len=len(symbols(i)%file_uri)) :: panel%all_symbols(i)%file_uri)
                panel%all_symbols(i)%file_uri = symbols(i)%file_uri
            end if

            if (allocated(panel%all_symbols(i)%file_path)) deallocate(panel%all_symbols(i)%file_path)
            if (allocated(symbols(i)%file_path)) then
                allocate(character(len=len(symbols(i)%file_path)) :: panel%all_symbols(i)%file_path)
                panel%all_symbols(i)%file_path = symbols(i)%file_path
            end if

            panel%all_symbols(i)%line = symbols(i)%line
            panel%all_symbols(i)%column = symbols(i)%column
            panel%all_symbols(i)%kind = symbols(i)%kind
            panel%all_symbols(i)%score = symbols(i)%score
        end do

        ! Refilter with current query
        call filter_symbols(panel)
    end subroutine set_workspace_symbols

    subroutine filter_symbols(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        character(len=:), allocatable :: query
        integer :: i, score

        if (panel%search_pos > 0) then
            query = trim(panel%search_query(1:panel%search_pos))
        else
            query = ''
        end if

        panel%num_filtered = 0

        do i = 1, panel%num_symbols
            if (allocated(panel%all_symbols(i)%name)) then
                score = fuzzy_match_score(panel%all_symbols(i)%name, query)
                if (score > 0) then
                    panel%num_filtered = panel%num_filtered + 1
                    panel%filtered_symbols(panel%num_filtered) = panel%all_symbols(i)
                    panel%filtered_symbols(panel%num_filtered)%score = score
                end if
            end if
        end do

        ! Sort by score (simple bubble sort for now)
        call sort_symbols_by_score(panel%filtered_symbols, panel%num_filtered)

        ! Reset selection
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine filter_symbols

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
                if (i == 1 .or. text(i-1:i-1) == ' ' .or. text(i-1:i-1) == '_') then
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

    subroutine sort_symbols_by_score(symbols, count)
        type(workspace_symbol_t), intent(inout) :: symbols(:)
        integer, intent(in) :: count
        type(workspace_symbol_t) :: temp
        integer :: i, j

        ! Bubble sort (good enough for small lists)
        do i = 1, count - 1
            do j = i + 1, count
                if (symbols(j)%score > symbols(i)%score) then
                    temp = symbols(i)
                    symbols(i) = symbols(j)
                    symbols(j) = temp
                end if
            end do
        end do
    end subroutine sort_symbols_by_score

    function get_selected_symbol(panel) result(sym)
        type(workspace_symbols_panel_t), intent(in) :: panel
        type(workspace_symbol_t) :: sym

        if (panel%num_filtered > 0 .and. panel%selected_index <= panel%num_filtered) then
            sym = panel%filtered_symbols(panel%selected_index)
        end if
    end function get_selected_symbol

    function workspace_symbols_panel_handle_key(panel, key) result(handled)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled

        handled = .false.
        if (.not. panel%visible) return

        select case(trim(key))
        case('j', 'down', 'ctrl-n')
            ! Always handle to clamp at boundary
            handled = .true.
            if (panel%selected_index < panel%num_filtered) then
                panel%selected_index = panel%selected_index + 1
                if (panel%selected_index > panel%scroll_offset + panel%max_visible) then
                    panel%scroll_offset = panel%selected_index - panel%max_visible
                end if
            end if

        case('k', 'up', 'ctrl-p')
            ! Always handle to clamp at boundary
            handled = .true.
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
                if (panel%selected_index <= panel%scroll_offset) then
                    panel%scroll_offset = max(0, panel%selected_index - 1)
                end if
            end if

        case('ctrl-u')
            ! Clear search
            call clear_search(panel)
            handled = .true.

        case('esc', 'escape')
            call hide_workspace_symbols_panel(panel)
            handled = .true.

        case('enter')
            ! Signal to jump - handled by command_handler
            handled = .true.

        case('backspace', 'ctrl-h')
            call delete_search_char(panel)
            handled = .true.

        case default
            ! Check if it's a printable character for search
            if (len_trim(key) == 1) then
                if (iachar(key(1:1)) >= 32 .and. iachar(key(1:1)) < 127) then
                    call add_search_char(panel, key(1:1))
                    handled = .true.
                end if
            end if
        end select
    end function workspace_symbols_panel_handle_key

    subroutine render_workspace_symbols_panel(panel, screen_height)
        type(workspace_symbols_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_height
        integer :: row, i, start_idx, end_idx
        character(len=256) :: line, file_info
        character(len=1), parameter :: ESC = char(27)

        if (.not. panel%visible) return

        ! Draw title bar with background color
        row = 1
        call terminal_move_cursor(row, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // ESC // '[1m')  ! Dark bg, bold
        line = " Workspace Symbols"
        if (panel%num_filtered > 0) then
            write(line, '(A,I0,A)') trim(line) // " (", panel%num_filtered, ")"
        end if
        call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
        call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
        call terminal_write(ESC // '[0m')

        ! Draw search input
        row = 2
        call terminal_move_cursor(row, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;236m')  ! Slightly lighter for input
        if (panel%search_pos > 0) then
            line = " > " // trim(panel%search_query(1:panel%search_pos))
        else
            line = " > " // ESC // '[90m' // "(type to filter)" // ESC // '[0m' // ESC // '[48;5;236m'
        end if
        call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
        call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
        call terminal_write(ESC // '[0m')

        ! Draw separator
        row = 3
        call terminal_move_cursor(row, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // repeat("-", panel%panel_width) // ESC // '[0m')

        ! Calculate visible range
        start_idx = panel%scroll_offset + 1
        end_idx = min(panel%scroll_offset + panel%max_visible, panel%num_filtered)

        ! Render symbols
        if (panel%num_filtered > 0) then
            do i = start_idx, end_idx
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)

                ! Background color based on selection
                if (i == panel%selected_index) then
                    call terminal_write(ESC // '[48;5;240m')  ! Highlight
                else
                    call terminal_write(ESC // '[48;5;235m')  ! Normal
                end if

                ! Build display line: icon + name + file:line
                line = " "

                ! Add kind indicator
                if (allocated(panel%filtered_symbols(i)%kind_name)) then
                    line = trim(line) // "[" // trim(panel%filtered_symbols(i)%kind_name(1:min(3, len_trim(panel%filtered_symbols(i)%kind_name)))) // "] "
                else
                    line = trim(line) // "    "
                end if

                ! Add symbol name
                if (allocated(panel%filtered_symbols(i)%name)) then
                    line = trim(line) // trim(panel%filtered_symbols(i)%name)
                end if

                ! Add file info on selected item
                if (i == panel%selected_index) then
                    if (allocated(panel%filtered_symbols(i)%file_path)) then
                        write(file_info, '(A,A,A,I0)') " (", &
                            trim(get_basename(panel%filtered_symbols(i)%file_path)), &
                            ":", panel%filtered_symbols(i)%line
                        file_info = trim(file_info) // ")"
                        if (len_trim(line) + len_trim(file_info) < panel%panel_width - 1) then
                            line = trim(line) // ESC // '[90m' // trim(file_info) // ESC // '[0m' // ESC // '[48;5;240m'
                        end if
                    end if
                end if

                ! Write line and pad
                call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
                call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
                call terminal_write(ESC // '[0m')
            end do

            ! Fill empty rows
            do while (row < screen_height - 1)
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)
                call terminal_write(ESC // '[48;5;235m' // repeat(" ", panel%panel_width) // ESC // '[0m')
            end do
        else
            ! No symbols message
            row = row + 1
            call terminal_move_cursor(row, panel%panel_start_col)
            call terminal_write(ESC // '[48;5;235m' // ESC // '[90m')
            if (panel%search_pos == 0) then
                line = " Type to search..."
            else if (panel%num_symbols == 0) then
                line = " Searching..."
            else
                line = " No matching symbols"
            end if
            call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
            call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
            call terminal_write(ESC // '[0m')

            do while (row < screen_height - 1)
                row = row + 1
                call terminal_move_cursor(row, panel%panel_start_col)
                call terminal_write(ESC // '[48;5;235m' // repeat(" ", panel%panel_width) // ESC // '[0m')
            end do
        end if

        ! Draw hint bar at bottom
        call terminal_move_cursor(screen_height, panel%panel_start_col)
        call terminal_write(ESC // '[48;5;237m' // ESC // '[90m')
        line = " Enter:open  Esc:close  Ctrl-U:clear"
        call terminal_write(line(1:min(len_trim(line), panel%panel_width)))
        call terminal_write(repeat(" ", max(0, panel%panel_width - len_trim(line))))
        call terminal_write(ESC // '[0m')
    end subroutine render_workspace_symbols_panel

    function get_basename(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=256) :: basename
        integer :: i, last_slash

        last_slash = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/') then
                last_slash = i
                exit
            end if
        end do

        if (last_slash > 0 .and. last_slash < len_trim(path)) then
            basename = path(last_slash+1:len_trim(path))
        else
            basename = path
        end if
    end function get_basename

end module workspace_symbols_panel_module
