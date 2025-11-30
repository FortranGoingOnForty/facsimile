module server_installer_module
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
        integer :: exit_status

        result%success = .false.
        result%exit_code = -1
        result%message = ''

        if (len_trim(command) == 0) then
            result%message = 'No command specified'
            return
        end if

        ! Execute the command
        ! Note: This runs synchronously and blocks until complete
        call execute_command_line(trim(command), wait=.true., exitstat=exit_status)

        result%exit_code = exit_status
        result%success = (exit_status == 0)

        if (result%success) then
            result%message = 'Installation completed successfully'
        else
            write(result%message, '(A,I0)') 'Installation failed with exit code: ', exit_status
        end if
    end function run_install_command

end module server_installer_module
