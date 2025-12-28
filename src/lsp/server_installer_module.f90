module server_installer_module
    use raw_mode_module, only: disable_raw_mode, enable_raw_mode
    use terminal_io_module, only: terminal_clear_screen, terminal_show_cursor
    implicit none
    private

    public :: run_install_command
    public :: install_result_t

    type :: install_result_t
        logical :: success = .false.
        integer :: exit_code = -1
        character(len=1024) :: message = ''
    end type install_result_t

contains

    function run_install_command(command) result(result)
        character(len=*), intent(in) :: command
        type(install_result_t) :: result
        integer :: exit_status, cmd_status
        logical :: raw_disabled

        result%success = .false.
        result%exit_code = -1
        result%message = ''

        if (len_trim(command) == 0) then
            result%message = 'No command specified'
            return
        end if

        ! Must exit raw mode before running external commands
        ! Otherwise child process inherits broken terminal state
        raw_disabled = disable_raw_mode()

        ! Show cursor and clear screen for command output
        call terminal_show_cursor()
        call terminal_clear_screen()

        ! Execute the command with cmdstat to catch errors gracefully
        ! Without cmdstat, invalid commands cause Fortran runtime errors
        call execute_command_line(trim(command), wait=.true., exitstat=exit_status, cmdstat=cmd_status)

        ! Re-enable raw mode for editor
        raw_disabled = enable_raw_mode()

        ! Check for command execution errors (cmdstat /= 0 means execute failed)
        if (cmd_status /= 0) then
            result%exit_code = cmd_status
            result%message = 'Failed to execute command (cmdstat=' // trim(itoa(cmd_status)) // ')'
            return
        end if

        result%exit_code = exit_status
        result%success = (exit_status == 0)

        if (result%success) then
            result%message = 'Installation completed successfully'
        else
            write(result%message, '(A,I0)') 'Installation failed with exit code: ', exit_status
        end if
    end function run_install_command

    ! Simple integer to string helper
    function itoa(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
    end function itoa

end module server_installer_module
