! Fortress Navigator Integration for fac
! Main API for opening file/directory navigator (Ctrl-O)

module fortress_navigator_module
    use iso_fortran_env, only: output_unit, input_unit
    use fortress_fs_module
    use fortress_display_module
    use terminal_io_module, only: terminal_read_char
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
        character(len=MAX_PATH) :: current_dir, parent_dir, temp_dir
        character(len=1) :: key
        integer :: rows, cols, ios
        logical :: running

        ! Initialize state
        selected = 1
        parent_selected = -1
        scroll_offset = 0
        parent_scroll_offset = 0
        running = .true.
        cancelled = .false.

        ! Set initial directory
        if (present(initial_path)) then
            current_dir = initial_path
        else
            current_dir = get_pwd()
        end if

        ! Main navigation loop
        do while (running)
            ! Get directory listings
            parent_dir = get_parent_path(current_dir)
            call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)
            call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)

            ! Find current directory in parent listing
            parent_selected = find_in_parent(current_dir, parent_files, parent_count)

            ! Get terminal size
            call get_term_size(rows, cols)

            ! Bounds check
            if (selected < 1) selected = 1
            if (selected > current_count) selected = current_count
            if (current_count == 0) selected = 1

            ! Adjust scroll offsets
            call adjust_scroll(selected, scroll_offset, rows - 4)
            call adjust_scroll(parent_selected, parent_scroll_offset, rows - 4)

            ! Render interface
            call draw_fortress_interface(rows, cols, current_dir, &
                                         current_files, current_is_dir, current_is_exec, current_count, &
                                         parent_files, parent_is_dir, parent_is_exec, parent_count, &
                                         selected, parent_selected, scroll_offset, parent_scroll_offset)

            ! Read key using raw terminal input (blocking mode)
            ! Keep trying until we get input to avoid tight redraw loop
            do
                ios = terminal_read_char()
                if (ios >= 0) exit
                ! Small delay to avoid busy-waiting
                call sleep(0)
            end do
            key = achar(ios)

            ! Handle input
            select case (key)
                case (char(27))  ! ESC
                    ! Check if arrow key or standalone ESC
                    if (check_arrow_key(key)) then
                        call handle_arrow_key(key, selected, current_dir, temp_dir, current_files, &
                                             current_is_dir, current_count, parent_selected, rows - 4, running)
                    else
                        ! Standalone ESC - quit
                        cancelled = .true.
                        running = .false.
                    end if

                case ('q', 'Q')  ! Quit
                    cancelled = .true.
                    running = .false.

                case (char(10), char(13))  ! Enter - select current item
                    if (current_count > 0) then
                        if (current_is_dir(selected)) then
                            ! Enter directory
                            temp_dir = current_dir
                            current_dir = join_path(current_dir, trim(current_files(selected)))
                            selected = 1
                            scroll_offset = 0
                        else
                            ! Select file
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

            end select
        end do

        ! If not cancelled, allocate and set selected_path
        if (.not. cancelled) then
            is_directory = current_is_dir(selected)
        else
            selected_path = ""
        end if

    end subroutine open_fortress_navigator

    !> Adjust scroll offset to keep selection visible
    subroutine adjust_scroll(sel, offset, visible_height)
        integer, intent(in) :: sel, visible_height
        integer, intent(inout) :: offset

        if (sel < offset + 1) then
            offset = sel - 1
        else if (sel > offset + visible_height) then
            offset = sel - visible_height
        end if

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
    subroutine handle_arrow_key(key, sel, curr_dir, temp_dir, files, is_dir, file_count, par_sel, vis_h, running)
        character(len=1), intent(in) :: key
        integer, intent(inout) :: sel, par_sel
        character(len=MAX_PATH), intent(inout) :: curr_dir, temp_dir
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: file_count, vis_h
        logical, intent(inout) :: running

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

end module fortress_navigator_module
