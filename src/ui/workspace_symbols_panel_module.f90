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
    public :: render_workspace_symbols_panel

    integer, parameter :: MAX_SYMBOLS = 1000
    integer, parameter :: MAX_VISIBLE = 15

    type :: workspace_symbol_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: kind_name  ! "Function", "Class", etc.
        character(len=:), allocatable :: container_name
        character(len=:), allocatable :: file_uri
        integer :: line = 0
        integer :: character = 0
        integer :: score = 0  ! For fuzzy matching
    end type workspace_symbol_t

    type :: workspace_symbols_panel_t
        logical :: visible = .false.
        type(workspace_symbol_t), allocatable :: all_symbols(:)
        integer :: num_symbols = 0
        type(workspace_symbol_t), allocatable :: filtered_symbols(:)
        integer :: num_filtered = 0
        integer :: selected_index = 1
        integer :: scroll_offset = 0
        character(len=256) :: search_query = ''
        integer :: search_pos = 0
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
            end do
            deallocate(panel%all_symbols)
        end if

        if (allocated(panel%filtered_symbols)) deallocate(panel%filtered_symbols)
    end subroutine cleanup_workspace_symbols_panel

    subroutine show_workspace_symbols_panel(panel)
        type(workspace_symbols_panel_t), intent(inout) :: panel

        panel%visible = .true.
        panel%search_query = ''
        panel%search_pos = 0
        panel%selected_index = 1
        panel%scroll_offset = 0

        ! Initially show all symbols
        call filter_symbols(panel, '')
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

    subroutine set_workspace_symbols(panel, symbols, count)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        type(workspace_symbol_t), intent(in) :: symbols(:)
        integer, intent(in) :: count
        integer :: i, copy_count

        copy_count = min(count, MAX_SYMBOLS)
        panel%num_symbols = copy_count

        do i = 1, copy_count
            panel%all_symbols(i) = symbols(i)
        end do

        ! Refilter with current query
        call filter_symbols(panel, trim(panel%search_query(1:panel%search_pos)))
    end subroutine set_workspace_symbols

    subroutine filter_symbols(panel, query)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: query
        integer :: i, score

        panel%num_filtered = 0

        do i = 1, panel%num_symbols
            score = fuzzy_match_score(panel%all_symbols(i)%name, query)
            if (score > 0) then
                panel%num_filtered = panel%num_filtered + 1
                panel%filtered_symbols(panel%num_filtered) = panel%all_symbols(i)
                panel%filtered_symbols(panel%num_filtered)%score = score
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
        else
            sym%file_uri = ''
        end if
    end function get_selected_symbol

    subroutine workspace_symbols_panel_handle_key(panel, key, handled)
        type(workspace_symbols_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical, intent(out) :: handled

        handled = .true.

        select case(key)
        case('up', 'ctrl-k', 'k')
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
                if (panel%selected_index < panel%scroll_offset + 1) then
                    panel%scroll_offset = panel%selected_index - 1
                end if
            end if

        case('down', 'ctrl-j', 'j')
            if (panel%selected_index < panel%num_filtered) then
                panel%selected_index = panel%selected_index + 1
                if (panel%selected_index > panel%scroll_offset + MAX_VISIBLE) then
                    panel%scroll_offset = panel%selected_index - MAX_VISIBLE
                end if
            end if

        case('esc')
            call hide_workspace_symbols_panel(panel)

        case default
            handled = .false.
        end select
    end subroutine workspace_symbols_panel_handle_key

    subroutine render_workspace_symbols_panel(panel, screen_rows)
        type(workspace_symbols_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_rows
        integer :: i, visible_start, visible_end, row
        character(len=256) :: line, kind_tag, container_info
        type(workspace_symbol_t) :: sym

        if (.not. panel%visible) return

        ! Clear and draw header
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 2, 1)
        call terminal_write(repeat(' ', 120))
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 2, 1)
        call terminal_write('Workspace Symbols (type to search, Esc to cancel)')

        ! Draw search query
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 1)
        call terminal_write(repeat(' ', 120))
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 1)
        call terminal_write('> ' // trim(panel%search_query))

        ! Calculate visible range
        visible_start = panel%scroll_offset + 1
        visible_end = min(visible_start + MAX_VISIBLE - 1, panel%num_filtered)

        ! Draw symbols
        row = screen_rows - MAX_VISIBLE
        do i = visible_start, visible_end
            sym = panel%filtered_symbols(i)

            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', 120))
            call terminal_move_cursor(row, 1)

            ! Build line with kind, name, and container
            if (len_trim(sym%kind_name) > 0) then
                write(kind_tag, '(A,A,A)') '[', trim(sym%kind_name), '] '
            else
                kind_tag = ''
            end if

            if (len_trim(sym%container_name) > 0) then
                write(container_info, '(A,A)') ' in ', trim(sym%container_name)
            else
                container_info = ''
            end if

            if (i == panel%selected_index) then
                ! Highlight selected item
                write(line, '(A,A,A,A)') '> ', trim(kind_tag), trim(sym%name), trim(container_info)
            else
                write(line, '(A,A,A,A)') '  ', trim(kind_tag), trim(sym%name), trim(container_info)
            end if

            call terminal_write(trim(line))
            row = row + 1
        end do

        ! Clear remaining lines
        do while (row <= screen_rows)
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', 120))
            row = row + 1
        end do

        ! Position cursor at end of search query
        call terminal_move_cursor(screen_rows - MAX_VISIBLE - 1, 3 + panel%search_pos)
    end subroutine render_workspace_symbols_panel

end module workspace_symbols_panel_module
