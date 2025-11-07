! Configuration directory management module
! Handles ~/.config/fac/ directory creation and path resolution

module config_module
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: get_config_dir, ensure_config_dir

    integer, parameter :: MAX_PATH_LEN = 512

contains

    !> Get configuration directory path
    !> Returns ~/.config/fac/ or ~/.fac/ as fallback
    subroutine get_config_dir(config_path)
        character(len=:), allocatable, intent(out) :: config_path
        character(len=MAX_PATH_LEN) :: home_dir, xdg_config_home
        integer :: unit, ios

        ! Try XDG_CONFIG_HOME environment variable
        call get_environment_variable('XDG_CONFIG_HOME', xdg_config_home)

        if (len_trim(xdg_config_home) > 0) then
            ! XDG_CONFIG_HOME is set
            config_path = trim(xdg_config_home) // '/fac'
        else
            ! Fall back to ~/.config/fac
            call get_home_directory(home_dir)

            ! Check if ~/.config exists
            call execute_command_line('test -d "' // trim(home_dir) // &
                '/.config" && echo "1" > /tmp/.fac_xdg_check || echo "0" > /tmp/.fac_xdg_check', &
                wait=.true.)

            open(newunit=unit, file='/tmp/.fac_xdg_check', status='old', iostat=ios)
            if (ios == 0) then
                read(unit, '(A)', iostat=ios) xdg_config_home
                close(unit)
                call execute_command_line('rm -f /tmp/.fac_xdg_check', wait=.true.)

                if (trim(xdg_config_home) == '1') then
                    config_path = trim(home_dir) // '/.config/fac'
                else
                    ! Fallback to ~/.fac
                    config_path = trim(home_dir) // '/.fac'
                end if
            else
                ! Error checking, use fallback
                config_path = trim(home_dir) // '/.fac'
            end if
        end if
    end subroutine get_config_dir

    !> Ensure configuration directory exists (create if needed)
    subroutine ensure_config_dir(success)
        logical, intent(out) :: success
        character(len=:), allocatable :: config_path
        integer :: exit_status

        success = .false.
        call get_config_dir(config_path)

        ! Create directory if it doesn't exist
        call execute_command_line('mkdir -p "' // trim(config_path) // '"', &
            wait=.true., exitstat=exit_status)

        success = (exit_status == 0)
    end subroutine ensure_config_dir

    !> Get user's home directory
    subroutine get_home_directory(home_dir)
        character(len=*), intent(out) :: home_dir
        integer :: unit, ios

        ! Try HOME environment variable first
        call get_environment_variable('HOME', home_dir)

        if (len_trim(home_dir) == 0) then
            ! Fallback: use shell expansion
            call execute_command_line('echo $HOME > /tmp/.fac_home', wait=.true.)
            open(newunit=unit, file='/tmp/.fac_home', status='old', iostat=ios)
            if (ios == 0) then
                read(unit, '(A)', iostat=ios) home_dir
                close(unit)
                call execute_command_line('rm -f /tmp/.fac_home', wait=.true.)
            else
                ! Last resort fallback
                home_dir = '~'
            end if
        end if
    end subroutine get_home_directory

end module config_module
