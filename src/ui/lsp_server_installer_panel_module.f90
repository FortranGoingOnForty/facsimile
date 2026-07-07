module lsp_server_installer_panel_module
    use terminal_io_module
    use server_detection_module, only: detected_server_t, detect_all_servers, check_server_installed
    use server_installer_module, only: run_install_command, install_result_t
    use clipboard_module, only: copy_to_clipboard
    implicit none
    private

    public :: lsp_server_installer_panel_t
    public :: init_lsp_server_installer_panel, cleanup_lsp_server_installer_panel
    public :: show_lsp_server_installer_panel, hide_lsp_server_installer_panel
    public :: is_lsp_server_installer_panel_visible
    public :: lsp_server_installer_panel_handle_key
    public :: render_lsp_server_installer_panel
    public :: refresh_server_status

    integer, parameter :: PANEL_WIDTH = 70
    integer, parameter :: MAX_VISIBLE = 8

    type :: lsp_server_installer_panel_t
        logical :: visible = .false.
        integer :: selected_index = 1
        integer :: scroll_offset = 0
        type(detected_server_t), allocatable :: servers(:)
        integer :: num_servers = 0
        logical :: confirm_mode = .false.
        integer :: confirm_server_index = 0
        logical :: installing = .false.
        character(len=256) :: status_message = ''
    end type lsp_server_installer_panel_t

contains

    subroutine init_lsp_server_installer_panel(panel)
        type(lsp_server_installer_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%num_servers = 0
        panel%confirm_mode = .false.
        panel%confirm_server_index = 0
        panel%installing = .false.
        panel%status_message = ''
    end subroutine init_lsp_server_installer_panel

    subroutine cleanup_lsp_server_installer_panel(panel)
        type(lsp_server_installer_panel_t), intent(inout) :: panel

        if (allocated(panel%servers)) deallocate(panel%servers)
        panel%num_servers = 0
    end subroutine cleanup_lsp_server_installer_panel

    subroutine show_lsp_server_installer_panel(panel)
        type(lsp_server_installer_panel_t), intent(inout) :: panel

        panel%visible = .true.
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%confirm_mode = .false.
        panel%status_message = ''

        ! Detect servers if not already done
        if (panel%num_servers == 0) then
            call refresh_server_status(panel)
        end if
    end subroutine show_lsp_server_installer_panel

    subroutine hide_lsp_server_installer_panel(panel)
        type(lsp_server_installer_panel_t), intent(inout) :: panel
        panel%visible = .false.
        panel%confirm_mode = .false.
    end subroutine hide_lsp_server_installer_panel

    function is_lsp_server_installer_panel_visible(panel) result(visible)
        type(lsp_server_installer_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_lsp_server_installer_panel_visible

    subroutine refresh_server_status(panel)
        type(lsp_server_installer_panel_t), intent(inout) :: panel

        if (allocated(panel%servers)) deallocate(panel%servers)
        call detect_all_servers(panel%servers, panel%num_servers)
        panel%status_message = 'Server status refreshed'
    end subroutine refresh_server_status

    function lsp_server_installer_panel_handle_key(panel, key) result(handled)
        type(lsp_server_installer_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled
        type(install_result_t) :: result

        handled = .true.

        ! Handle confirm mode separately
        if (panel%confirm_mode) then
            select case(trim(key))
            case('y', 'Y')
                ! Execute installation
                panel%installing = .true.
                panel%status_message = 'Installing ' // trim(panel%servers(panel%confirm_server_index)%name) // '...'

                result = run_install_command(trim(panel%servers(panel%confirm_server_index)%install_cmd))

                panel%installing = .false.
                if (result%success) then
                    panel%status_message = 'Successfully installed ' // trim(panel%servers(panel%confirm_server_index)%name)
                    ! Refresh to update status
                    call refresh_server_status(panel)
                else
                    panel%status_message = 'Installation failed. Try manually: ' // &
                        trim(panel%servers(panel%confirm_server_index)%install_cmd)
                end if
                panel%confirm_mode = .false.

            case('c', 'C')
                ! Copy the command instead of running it
                call copy_to_clipboard(trim( &
                    panel%servers(panel%confirm_server_index)%install_cmd))
                panel%status_message = 'Copied: ' // &
                    trim(panel%servers(panel%confirm_server_index)%install_cmd)
                panel%confirm_mode = .false.

            case('n', 'N', 'esc', 'escape')
                panel%confirm_mode = .false.
                panel%status_message = ''

            case default
                ! Ignore other keys in confirm mode
            end select
            return
        end if

        ! Normal mode key handling
        select case(trim(key))
        case('j', 'down')
            if (panel%selected_index < panel%num_servers) then
                panel%selected_index = panel%selected_index + 1
                ! Scroll if needed
                if (panel%selected_index > panel%scroll_offset + MAX_VISIBLE) then
                    panel%scroll_offset = panel%selected_index - MAX_VISIBLE
                end if
            end if

        case('k', 'up')
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1
                ! Scroll if needed
                if (panel%selected_index <= panel%scroll_offset) then
                    panel%scroll_offset = panel%selected_index - 1
                end if
            end if

        case('enter')
            ! Only allow install for non-installed servers
            if (panel%num_servers > 0 .and. panel%selected_index <= panel%num_servers) then
                if (panel%servers(panel%selected_index)%is_installed) then
                    panel%status_message = trim(panel%servers(panel%selected_index)%name) // ' is already installed'
                else if (panel%servers(panel%selected_index)%install_cmd(1:1) == '#') then
                    ! No runnable command (manual install) — offer copy instead
                    panel%status_message = 'Manual install — press c to copy'
                else
                    panel%confirm_mode = .true.
                    panel%confirm_server_index = panel%selected_index
                end if
            end if

        case('c', 'C')
            ! Copy the selected server's install command to the clipboard
            if (panel%num_servers > 0 .and. panel%selected_index <= panel%num_servers) then
                call copy_to_clipboard(trim( &
                    panel%servers(panel%selected_index)%install_cmd))
                panel%status_message = 'Copied: ' // &
                    trim(panel%servers(panel%selected_index)%install_cmd)
            end if

        case('r', 'R')
            ! Refresh server status
            call refresh_server_status(panel)

        case('esc', 'escape', 'q')
            call hide_lsp_server_installer_panel(panel)

        case default
            handled = .false.
        end select
    end function lsp_server_installer_panel_handle_key

    subroutine render_lsp_server_installer_panel(panel, screen_cols)
        type(lsp_server_installer_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_cols
        integer :: start_col, start_row, row, i, visible_end
        integer :: content_width, visible_len, status_len, padding
        character(len=:), allocatable :: border_top, border_mid, border_bottom
        character(len=128) :: visible_text, status_text
        character(len=*), parameter :: ESC = char(27)
        character(len=*), parameter :: GREEN = ESC // '[32m'
        character(len=*), parameter :: RED = ESC // '[31m'
        character(len=*), parameter :: CYAN = ESC // '[36m'
        character(len=*), parameter :: YELLOW = ESC // '[33m'
        character(len=*), parameter :: DIM = ESC // '[90m'
        character(len=*), parameter :: INVERSE = ESC // '[7m'
        character(len=*), parameter :: RESET = ESC // '[0m'

        if (.not. panel%visible) return

        ! Calculate centering
        content_width = min(PANEL_WIDTH, screen_cols - 4)
        start_col = max(1, (screen_cols - content_width) / 2)
        start_row = 2

        ! Build borders
        border_top = '┌' // repeat('─', content_width - 2) // '┐'
        border_mid = '├' // repeat('─', content_width - 2) // '┤'
        border_bottom = '└' // repeat('─', content_width - 2) // '┘'

        ! Render confirm dialog if in confirm mode
        if (panel%confirm_mode) then
            call render_confirm_dialog(panel, screen_cols)
            return
        end if

        ! Draw top border
        call terminal_move_cursor(start_row, start_col)
        call terminal_write(border_top)

        ! Draw header
        row = start_row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│' // CYAN // ' Language Server Manager' // RESET)
        call terminal_write(repeat(' ', content_width - 32) // DIM // 'Alt+M' // RESET // ' │')

        ! Draw separator
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(border_mid)

        ! Draw server list
        visible_end = min(panel%scroll_offset + MAX_VISIBLE, panel%num_servers)
        do i = panel%scroll_offset + 1, visible_end
            row = row + 1
            call terminal_move_cursor(row, start_col)
            call terminal_write('│')

            ! Build line content (visible text only for width calculation)
            ! Format: " X servername (Language) <spaces> status"
            ! Use ' X ' (3 ASCII chars) instead of ' ✓ ' (5 bytes) for correct display width
            visible_text = ' X ' // trim(panel%servers(i)%name) // ' (' // &
                          trim(panel%servers(i)%language) // ')'

            ! Calculate visible length (icon + space + name + space + language + parens)
            visible_len = len_trim(visible_text)

            ! Add status text length
            if (panel%servers(i)%is_installed) then
                status_text = 'installed'
                status_len = 9
            else
                status_text = 'Enter to install'
                status_len = 16
            end if

            ! Calculate padding needed (content_width - 2 for borders, minus visible text, minus status)
            padding = max(1, content_width - 2 - visible_len - status_len)

            ! Highlight selected row
            if (i == panel%selected_index) then
                call terminal_write(INVERSE)
            end if

            ! Write status icon with color
            if (panel%servers(i)%is_installed) then
                call terminal_write(' ' // GREEN // '✓' // RESET // ' ')
            else
                call terminal_write(' ' // RED // '✗' // RESET // ' ')
            end if

            ! Write server name and language
            call terminal_write(trim(panel%servers(i)%name) // ' (' // &
                               trim(panel%servers(i)%language) // ')')

            ! Write padding
            call terminal_write(repeat(' ', padding))

            ! Write status text with color
            if (panel%servers(i)%is_installed) then
                call terminal_write(DIM // trim(status_text) // RESET)
            else
                call terminal_write(YELLOW // trim(status_text) // RESET)
            end if

            ! Reset highlighting
            if (i == panel%selected_index) then
                call terminal_write(RESET)
            end if

            call terminal_write('│')
        end do

        ! Fill remaining rows if needed
        do i = visible_end + 1, panel%scroll_offset + MAX_VISIBLE
            row = row + 1
            call terminal_move_cursor(row, start_col)
            call terminal_write('│' // repeat(' ', content_width - 2) // '│')
        end do

        ! Draw separator before footer
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(border_mid)

        ! Draw status message or help
        row = row + 1
        call terminal_move_cursor(row, start_col)
        if (len_trim(panel%status_message) > 0) then
            call terminal_write('│ ' // YELLOW // trim(panel%status_message) // RESET)
            call terminal_write(repeat(' ', max(0, content_width - len_trim(panel%status_message) - 4)) // ' │')
        else
            call terminal_write('│' // DIM // ' ↑↓ Navigate  Enter Install  c Copy  r Refresh  Esc Close' // RESET)
            call terminal_write(repeat(' ', max(0, content_width - 59)) // '│')
        end if

        ! Draw bottom border
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(border_bottom)

        ! Hide cursor while panel is shown
        call terminal_hide_cursor()
    end subroutine render_lsp_server_installer_panel

    subroutine render_confirm_dialog(panel, screen_cols)
        type(lsp_server_installer_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_cols
        integer :: start_col, start_row, row, content_width
        character(len=:), allocatable :: border_top, border_bottom
        character(len=256) :: server_name, install_cmd
        character(len=*), parameter :: ESC = char(27)
        character(len=*), parameter :: CYAN = ESC // '[36m'
        character(len=*), parameter :: YELLOW = ESC // '[33m'
        character(len=*), parameter :: GREEN = ESC // '[32m'
        character(len=*), parameter :: RED = ESC // '[31m'
        character(len=*), parameter :: RESET = ESC // '[0m'

        content_width = min(PANEL_WIDTH, screen_cols - 4)
        start_col = max(1, (screen_cols - content_width) / 2)
        start_row = 5

        border_top = '┌' // repeat('─', content_width - 2) // '┐'
        border_bottom = '└' // repeat('─', content_width - 2) // '┘'

        server_name = panel%servers(panel%confirm_server_index)%name
        install_cmd = panel%servers(panel%confirm_server_index)%install_cmd

        ! Top border
        call terminal_move_cursor(start_row, start_col)
        call terminal_write(border_top)

        ! Title
        row = start_row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│' // CYAN // ' Install ' // trim(server_name) // '?' // RESET)
        call terminal_write(repeat(' ', max(0, content_width - 12 - len_trim(server_name))) // '│')

        ! Blank line
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│' // repeat(' ', content_width - 2) // '│')

        ! Command
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│ Command: ' // YELLOW // trim(install_cmd) // RESET)
        call terminal_write(repeat(' ', max(0, content_width - 12 - len_trim(install_cmd))) // '│')

        ! Blank line
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│' // repeat(' ', content_width - 2) // '│')

        ! Yes/Copy/No buttons (21 visible chars: [Y]es   [C]opy   [N]o)
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write('│' // repeat(' ', max(0, (content_width - 23) / 2)))
        call terminal_write('[' // GREEN // 'Y' // RESET // ']es   ')
        call terminal_write('[' // CYAN // 'C' // RESET // ']opy   ')
        call terminal_write('[' // RED // 'N' // RESET // ']o')
        call terminal_write(repeat(' ', max(0, content_width - 23 - (content_width - 23) / 2)) // '│')

        ! Bottom border
        row = row + 1
        call terminal_move_cursor(row, start_col)
        call terminal_write(border_bottom)

        call terminal_hide_cursor()
    end subroutine render_confirm_dialog

end module lsp_server_installer_panel_module
