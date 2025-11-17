! Fortress Navigator Integration for fac
! Main API for opening file/directory navigator (Ctrl-O)

module fortress_navigator_module
    use iso_fortran_env, only: output_unit, input_unit
    use fortress_fs_module
    use fortress_display_module
    use terminal_io_module, only: terminal_read_char, terminal_write, terminal_move_cursor
    use favorites_module, only: favorites_add
    implicit none
    private

    public :: open_fortress_navigator

    ! Navigation state
    character(len=MAX_PATH), dimension(MAX_FILES) :: current_files, parent_files
    logical, dimension(MAX_FILES) :: current_is_dir, parent_is_dir
    logical, dimension(MAX_FILES) :: current_is_exec, parent_is_exec
    integer :: current_count, parent_count
    integer :: selected, parent_selected
    integer :: scroll_offset, parent_scroll_offset

contains

    !> Main entry point: open fortress navigator and return selection
    !! @param selected_path - Output: selected file/directory path (empty if cancelled)
    !! @param is_directory - Output: true if selected item is directory
    !! @param cancelled - Output: true if user pressed ESC/q
    !! @param initial_path - Input (optional): starting directory
    subroutine open_fortress_navigator(selected_path, is_directory, cancelled, initial_path)
        character(len=:), allocatable, intent(out) :: selected_path
        logical, intent(out) :: is_directory, cancelled
        character(len=*), intent(in), optional :: initial_path
        character(len=MAX_PATH) :: current_dir, parent_dir, temp_dir, last_dir, last_parent
        character(len=1) :: key
        integer :: rows, cols, ios, last_selected, last_scroll
        logical :: running, dir_changed, first_draw, need_redraw

        ! Initialize state
        selected = 1
        parent_selected = -1
        scroll_offset = 0
        parent_scroll_offset = 0
        running = .true.
        cancelled = .false.
        last_dir = ""
        last_parent = ""
        first_draw = .true.
        need_redraw = .true.
        last_selected = -1
        last_scroll = -1

        ! Set initial directory
        if (present(initial_path)) then
            current_dir = initial_path
        else
            current_dir = get_pwd()
        end if

        ! Get terminal size once (assumes terminal doesn't resize during navigation)
        call get_term_size(rows, cols)

        ! Main navigation loop
        do while (running)
            ! Only refresh directory listings if directory changed
            dir_changed = (current_dir /= last_dir)
            if (dir_changed) then
                parent_dir = get_parent_path(current_dir)

                ! Only refresh parent if it changed too
                if (parent_dir /= last_parent) then
                    call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)
                    last_parent = parent_dir
                end if

                call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)
                last_dir = current_dir
            else
                ! Just update parent_dir for consistency
                parent_dir = get_parent_path(current_dir)
            end if

            ! Find current directory in parent listing
            parent_selected = find_in_parent(current_dir, parent_files, parent_count)

            ! Bounds check
            if (selected < 1) selected = 1
            if (selected > current_count) selected = current_count
            if (current_count == 0) selected = 1

            ! Adjust scroll offsets
            call adjust_scroll(selected, scroll_offset, rows - 4)
            call adjust_scroll(parent_selected, parent_scroll_offset, rows - 4)

            ! Check if we need to redraw (directory changed, selection changed, or scroll changed)
            need_redraw = dir_changed .or. first_draw .or. &
                         selected /= last_selected .or. scroll_offset /= last_scroll

            ! Render interface only if something changed
            if (need_redraw) then
                call draw_fortress_interface(rows, cols, current_dir, &
                                             current_files, current_is_dir, current_is_exec, current_count, &
                                             parent_files, parent_is_dir, parent_count, &
                                             selected, parent_selected, scroll_offset, parent_scroll_offset, first_draw)

                ! Update tracking variables
                last_selected = selected
                last_scroll = scroll_offset
                if (first_draw) first_draw = .false.
            end if

            ! Read key using raw terminal input
            ! Note: terminal_read_char is non-blocking, returns -1 if no input
            ios = terminal_read_char()
            if (ios < 0) then
                ! No input available - With conditional redraw optimization above,
                ! we won't redraw unnecessarily, so this tight loop is acceptable
                cycle
            end if
            key = achar(ios)

            ! Handle input
            select case (key)
                case (char(27))  ! ESC
                    ! Check if arrow key or standalone ESC
                    if (check_arrow_key(key)) then
                        call handle_arrow_key(key, selected, current_dir, temp_dir, current_files, &
                                             current_is_dir, current_count)
                    else
                        ! Standalone ESC - quit
                        cancelled = .true.
                        running = .false.
                    end if

                case ('q', 'Q')  ! Quit
                    cancelled = .true.
                    running = .false.

                case (char(10), char(13))  ! Enter - select current item (file or directory)
                    if (current_count > 0) then
                        if (current_is_dir(selected)) then
                            ! Select directory and exit
                            selected_path = join_path(current_dir, trim(current_files(selected)))
                            is_directory = .true.
                            running = .false.
                        else
                            ! Select file and exit
                            selected_path = join_path(current_dir, trim(current_files(selected)))
                            is_directory = .false.
                            running = .false.
                        end if
                    end if

                case ('~')  ! Jump to home
                    call get_environment_variable("HOME", current_dir)
                    selected = 1
                    scroll_offset = 0

                case ('/')  ! Jump to root
                    current_dir = "/"
                    selected = 1
                    scroll_offset = 0

                case ('f', 'F')  ! Add current directory to favorites
                    call add_to_favorites(current_dir, rows)

            end select
        end do

        ! Set outputs based on result
        if (.not. cancelled) then
            ! Check if selected_path was already set (file selection in loop)
            if (.not. allocated(selected_path)) then
                ! Directory selection or other exit - set to current directory
                selected_path = trim(current_dir)
            end if
            ! Determine if the selected path is a directory
            if (allocated(selected_path)) then
                if (selected_path == trim(current_dir)) then
                    is_directory = .true.
                else
                    is_directory = .false.  ! Already set in loop for files
                end if
            end if
        else
            ! Cancelled - set empty path
            selected_path = ""
            is_directory = .false.
        end if

    end subroutine open_fortress_navigator

    !> Adjust scroll offset to keep selection visible with margin
    subroutine adjust_scroll(sel, offset, visible_height)
        integer, intent(in) :: sel, visible_height
        integer, intent(inout) :: offset
        integer :: margin

        ! Add a margin to avoid selection being at the very edge
        margin = 3
        if (margin > visible_height / 4) margin = visible_height / 4

        ! If selection is above the visible window (with margin)
        if (sel < offset + 1 + margin) then
            offset = sel - margin - 1
            if (offset < 0) offset = 0
        ! If selection is below the visible window (with margin)
        else if (sel > offset + visible_height - margin) then
            offset = sel - visible_height + margin
        end if

        ! Ensure offset is not negative
        if (offset < 0) offset = 0
    end subroutine adjust_scroll

    !> Check if ESC is start of arrow key sequence
    function check_arrow_key(key) result(is_arrow)
        character(len=1), intent(inout) :: key
        logical :: is_arrow
        integer :: char_code

        is_arrow = .false.

        if (key == char(27)) then
            ! Try to read next character
            char_code = terminal_read_char()
            if (char_code >= 0) then
                if (achar(char_code) == '[') then
                    ! It's an arrow key sequence - read the direction
                    char_code = terminal_read_char()
                    if (char_code >= 0) then
                        key = achar(char_code)
                        is_arrow = .true.
                    end if
                end if
            end if
        end if
    end function check_arrow_key

    !> Handle arrow key navigation
    subroutine handle_arrow_key(key, sel, curr_dir, temp_dir, files, is_dir, file_count)
        character(len=1), intent(in) :: key
        integer, intent(inout) :: sel
        character(len=MAX_PATH), intent(inout) :: curr_dir, temp_dir
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: file_count

        select case (key)
            case ('A')  ! Up arrow
                if (sel > 1) sel = sel - 1

            case ('B')  ! Down arrow
                if (sel < file_count) sel = sel + 1

            case ('C')  ! Right arrow - enter directory
                if (file_count > 0 .and. is_dir(sel)) then
                    temp_dir = curr_dir
                    curr_dir = join_path(curr_dir, trim(files(sel)))
                    sel = 1
                end if

            case ('D')  ! Left arrow - go to parent
                temp_dir = curr_dir
                curr_dir = get_parent_path(curr_dir)
                sel = find_in_parent(temp_dir, files, file_count)
        end select
    end subroutine handle_arrow_key

    !> Get terminal size using fac's terminal module
    subroutine get_term_size(rows, cols)
        use terminal_io_module, only: terminal_get_size
        integer, intent(out) :: rows, cols

        call terminal_get_size(rows, cols)

        ! Sanity check
        if (rows <= 0) rows = 24
        if (cols <= 0) cols = 80
    end subroutine get_term_size

    !> Add current directory to favorites
    subroutine add_to_favorites(dir_path, rows)
        character(len=*), intent(in) :: dir_path
        integer, intent(in) :: rows
        character(len=256) :: label
        logical :: success
        integer :: i

        ! Extract basename for label
        label = dir_path
        do i = len_trim(dir_path), 1, -1
            if (dir_path(i:i) == '/') then
                label = dir_path(i+1:)
                exit
            end if
        end do

        ! Add to favorites
        call favorites_add(dir_path, trim(label), success)

        ! Show feedback message
        call terminal_move_cursor(rows, 1)
        if (success) then
            call terminal_write('Added to favorites: ' // trim(label))
        else
            call terminal_write('Already in favorites or error')
        end if

        ! Pause briefly so user can see message
        call sleep_ms(800)
    end subroutine add_to_favorites

    !> Sleep for specified milliseconds
    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        integer :: i, j, dummy

        ! Simple busy-wait (not ideal but portable)
        dummy = 0
        do i = 1, milliseconds * 1000
            do j = 1, 100
                dummy = dummy + 1
            end do
        end do
    end subroutine sleep_ms

end module fortress_navigator_module
