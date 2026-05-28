! Welcome menu module
! Displays favorites and recents lists for quick workspace access

module welcome_menu_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_write, terminal_move_cursor, terminal_clear_screen, terminal_flush
    use input_handler_module, only: get_key_input
    use favorites_module, only: favorite_t, favorites_load, favorites_remove
    use recents_module, only: recent_t, recents_load, recents_remove
    implicit none
    private

    public :: show_welcome_menu

    integer, parameter :: MAX_DISPLAY = 20

contains

    !> Show welcome menu and return selected path
    subroutine show_welcome_menu(selected_path, cancelled)
        character(len=:), allocatable, intent(out) :: selected_path
        logical, intent(out) :: cancelled
        type(favorite_t), allocatable :: favorites(:)
        type(recent_t), allocatable :: recents(:)
        integer :: fav_count, rec_count, max_recents
        logical :: success, showing_favorites
        integer :: selected_index, scroll_offset
        character(len=32) :: key_input
        integer :: status, rows, cols
        character(len=512) :: cwd

        cancelled = .false.
        showing_favorites = .true.  ! Start with favorites view
        selected_index = 0  ! Start at CURRENT DIRECTORY option
        scroll_offset = 0

        ! Get current working directory for display
        call get_cwd(cwd)

        ! Load favorites and recents
        call favorites_load(favorites, fav_count, success)
        if (.not. success) fav_count = 0

        call recents_load(recents, rec_count, max_recents, success)
        if (.not. success) rec_count = 0

        ! Get terminal size
        call terminal_get_size(rows, cols)

        ! Main loop
        do
            ! Render menu
            call render_welcome_menu(favorites, fav_count, recents, rec_count, &
                showing_favorites, selected_index, scroll_offset, rows, cwd)
            call terminal_flush()

            ! Get input
            call get_key_input(key_input, status)
            if (status /= 0) cycle

            ! Handle input
            select case (key_input)
                case ('up', 'k')
                    if (selected_index > 0) then
                        selected_index = selected_index - 1
                        call adjust_scroll(scroll_offset)
                    end if

                case ('down', 'j')
                    if (showing_favorites) then
                        ! Allow selecting up to fav_count (0 = CWD, 1..fav_count = favorites)
                        if (selected_index < fav_count) then
                            selected_index = selected_index + 1
                            call adjust_scroll(scroll_offset)
                        end if
                    else
                        ! Allow selecting up to rec_count (0 = CWD, 1..rec_count = recents)
                        if (selected_index < rec_count) then
                            selected_index = selected_index + 1
                            call adjust_scroll(scroll_offset)
                        end if
                    end if

                case ('enter', 'right')  ! Enter or right arrow
                    ! Handle selection
                    if (selected_index == 0) then
                        ! CURRENT DIRECTORY selected
                        selected_path = "CWD"
                        cancelled = .false.
                        exit
                    else if (showing_favorites .and. fav_count > 0) then
                        ! Favorites list - adjust index to account for CWD at index 0
                        if (selected_index >= 1 .and. selected_index <= fav_count) then
                            ! Check if directory exists
                            if (directory_exists(trim(favorites(selected_index)%path))) then
                                selected_path = trim(favorites(selected_index)%path)
                                cancelled = .false.
                                exit
                            else
                                ! Directory doesn't exist - show warning and remove
                                call handle_deleted_workspace(trim(favorites(selected_index)%path), &
                                    showing_favorites, selected_index)
                                ! Reload favorites and recents
                                call favorites_load(favorites, fav_count, success)
                                if (.not. success) fav_count = 0
                                ! Adjust selection if needed
                                if (selected_index > fav_count) selected_index = fav_count
                                if (selected_index < 0) selected_index = 0
                            end if
                        end if
                    else if (.not. showing_favorites .and. rec_count > 0) then
                        ! Recents list - adjust index to account for CWD at index 0
                        if (selected_index >= 1 .and. selected_index <= rec_count) then
                            ! Check if directory exists
                            if (directory_exists(trim(recents(selected_index)%path))) then
                                selected_path = trim(recents(selected_index)%path)
                                cancelled = .false.
                                exit
                            else
                                ! Directory doesn't exist - show warning and remove
                                call handle_deleted_workspace(trim(recents(selected_index)%path), &
                                    showing_favorites, selected_index)
                                ! Reload recents
                                call recents_load(recents, rec_count, max_recents, success)
                                if (.not. success) rec_count = 0
                                ! Adjust selection if needed
                                if (selected_index > rec_count) selected_index = rec_count
                                if (selected_index < 0) selected_index = 0
                            end if
                        end if
                    end if

                case ('8')
                    ! Toggle between favorites and recents
                    showing_favorites = .not. showing_favorites
                    selected_index = 0  ! Reset to CURRENT DIRECTORY
                    scroll_offset = 0

                case ('esc', 'q')
                    cancelled = .true.
                    exit

                case ('b')
                    ! Browse filesystem - launch fortress navigator
                    cancelled = .true.  ! Signal to launch navigator instead
                    selected_path = "BROWSE"
                    exit
            end select
        end do

        call terminal_clear_screen()
    end subroutine show_welcome_menu

    !> Render the welcome menu
    subroutine render_welcome_menu(favorites, fav_count, recents, rec_count, &
                                   showing_favorites, selected_index, scroll_offset, rows, cwd)
        type(favorite_t), intent(in) :: favorites(:)
        type(recent_t), intent(in) :: recents(:)
        integer, intent(in) :: fav_count, rec_count
        logical, intent(in) :: showing_favorites
        integer, intent(in) :: selected_index, scroll_offset, rows
        character(len=*), intent(in) :: cwd
        character(len=512) :: line
        integer :: i, display_row, visible_height, item_count, actual_index
        character(len=64) :: title

        call terminal_clear_screen()
        call terminal_move_cursor(1, 1)

        ! Header
        line = '╔═══════════════════════' // &
               '═══════════════════════' // &
               '══════════════════════╗'
        call terminal_write(trim(line))
        call terminal_move_cursor(2, 1)
        write(line, '(A)') '║                     FAC - Welcome Menu                               ║'
        call terminal_write(trim(line))
        call terminal_move_cursor(3, 1)
        line = '╚═══════════════════════' // &
               '═══════════════════════' // &
               '══════════════════════╝'
        call terminal_write(trim(line))

        ! View title
        call terminal_move_cursor(5, 1)
        if (showing_favorites) then
            write(title, '(A,I0,A)') 'FAVORITES (', fav_count, ' total)'
            item_count = fav_count
        else
            write(title, '(A,I0,A)') 'RECENT WORKSPACES (', rec_count, ' total)'
            item_count = rec_count
        end if
        call terminal_write(trim(title))

        ! Separator
        call terminal_move_cursor(6, 1)
        line = '────────────────────────' // &
               '────────────────────────' // &
               '──────────────────────'
        call terminal_write(trim(line))

        ! List items
        visible_height = rows - 8  ! Reserve space for header and footer
        display_row = 7

        ! Always show at least the CURRENT DIRECTORY option
        ! Render items from scroll_offset to scroll_offset + visible_height
        ! Index 0 = CURRENT DIRECTORY, indices 1..item_count = actual items
        do i = 0, min(visible_height - 1, item_count)
            actual_index = i + scroll_offset
            if (actual_index > item_count) exit

            call terminal_move_cursor(display_row, 1)

            ! Check if this item is selected
            if (actual_index == selected_index) then
                ! Highlight selected
                write(line, '(A)') char(27) // '[7m'  ! Reverse video
            else
                write(line, '(A)') ''
            end if

            if (actual_index == 0) then
                ! CURRENT DIRECTORY option
                write(line, '(A,A,I0,A,A)') trim(line), '  [', actual_index, '] ', &
                    'CURRENT DIRECTORY → ' // trim(cwd)
            else
                ! Regular items (favorites or recents)
                if (showing_favorites) then
                    write(line, '(A,A,I0,A,A,A,A)') trim(line), '  [', actual_index, '] ', &
                        trim(favorites(actual_index)%label), ' → ', &
                        trim(favorites(actual_index)%path)
                else
                    write(line, '(A,A,I0,A,A,A,A)') trim(line), '  [', actual_index, '] ', &
                        trim(recents(actual_index)%label), ' → ', &
                        trim(recents(actual_index)%path)
                end if
            end if

            if (actual_index == selected_index) then
                write(line, '(A,A)') trim(line), char(27) // '[0m'  ! Reset
            end if

            call terminal_write(trim(line))
            display_row = display_row + 1
        end do

        ! Show message if no items (but CWD is always there)
        if (item_count == 0 .and. selected_index /= 0) then
            call terminal_move_cursor(display_row, 1)
            if (showing_favorites) then
                call terminal_write('  (No favorites yet - press ''f'' in Fortress to add)')
            else
                call terminal_write('  (No recent workspaces)')
            end if
        end if

        ! Footer with keybindings
        call terminal_move_cursor(rows - 2, 1)
        line = '────────────────────────' // &
               '────────────────────────' // &
               '──────────────────────'
        call terminal_write(trim(line))

        call terminal_move_cursor(rows - 1, 1)
        write(line, '(A)') '↑/↓:navigate  Enter:select  8:toggle fav/recent  b:browse  ESC/q:quit'
        call terminal_write(trim(line))
    end subroutine render_welcome_menu

    !> Adjust scroll offset to keep CURRENT DIRECTORY visible
    subroutine adjust_scroll(offset)
        integer, intent(inout) :: offset

        ! IMPORTANT: offset must always be 0 to keep CWD (index 0) visible
        ! Since CWD is always at the top, we never scroll past it
        offset = 0

        ! Note: This means with many items, CWD is always visible but
        ! you scroll through the rest of the list below it
    end subroutine adjust_scroll

    !> Get terminal size (wrapper)
    subroutine terminal_get_size(rows, cols)
        integer, intent(out) :: rows, cols
        ! Default size
        rows = 24
        cols = 80
        ! TODO: Query actual terminal size if available
    end subroutine terminal_get_size

    !> Check if a directory exists (Phase 7: deleted workspace detection)
    function directory_exists(path) result(exists)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: ios

        ! Try to open directory (will fail if doesn't exist)
        call execute_command_line('test -d "' // trim(path) // '"', wait=.true., exitstat=ios)
        exists = (ios == 0)
    end function directory_exists

    !> Handle deleted workspace (Phase 7: show warning and remove from list)
    subroutine handle_deleted_workspace(path, is_favorite, index)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_favorite
        integer, intent(in) :: index
        character(len=512) :: warning_msg
        logical :: remove_success

        ! Show warning message
        call terminal_move_cursor(1, 1)
        warning_msg = "Warning: Workspace no longer exists: " // trim(path)
        call terminal_write(trim(warning_msg))
        call terminal_move_cursor(2, 1)
        call terminal_write("Removing from list...")

        ! Remove from appropriate list
        if (is_favorite) then
            call favorites_remove(index, remove_success)
        else
            call recents_remove(index, remove_success)
        end if

        ! Brief pause so user can see the message
        call execute_command_line("sleep 1.0", wait=.true.)
    end subroutine handle_deleted_workspace

    !> Get current working directory
    subroutine get_cwd(cwd)
        character(len=*), intent(out) :: cwd
        integer :: unit, ios

        ! Use pwd command to get current directory
        call execute_command_line('pwd > /tmp/.fac_cwd 2>/dev/null', wait=.true.)

        open(newunit=unit, file='/tmp/.fac_cwd', status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(A)', iostat=ios) cwd
            close(unit)
            call execute_command_line('rm -f /tmp/.fac_cwd', wait=.true.)
        else
            cwd = '.'
        end if
    end subroutine get_cwd

end module welcome_menu_module
