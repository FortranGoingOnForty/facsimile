module platform_module
    use iso_c_binding
    implicit none
    private

    public :: get_temp_dir, get_home_dir, get_path_separator, is_windows
    public :: get_config_dir, get_cwd
    public :: platform_copy_to_clipboard, platform_paste_from_clipboard
    public :: detect_system_pkg_mgr, detect_priv_prefix
    public :: platform_sleep_ms

    interface
        subroutine fac_sleep_ms_c(ms) bind(C, name='fac_sleep_ms_f')
            import :: c_int
            integer(c_int), value :: ms
        end subroutine

        subroutine get_temp_dir_c(buffer, buffer_len, result_len) bind(C, name='get_temp_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        subroutine get_home_dir_c(buffer, buffer_len, result_len) bind(C, name='get_home_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        function get_path_separator_c() bind(C, name='get_path_separator_f') result(sep)
            import :: c_char
            character(kind=c_char) :: sep
        end function

        function is_windows_c() bind(C, name='is_windows_f') result(res)
            import :: c_int
            integer(c_int) :: res
        end function

        subroutine get_config_dir_c(buffer, buffer_len, result_len) bind(C, name='get_config_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        subroutine get_cwd_c(buffer, buffer_len, result_len) bind(C, name='get_cwd_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        function copy_to_clipboard_c(text, text_len) bind(C, name='copy_to_clipboard_f') result(res)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), value :: text_len
            integer(c_int) :: res
        end function

        function paste_from_clipboard_c(buffer, buffer_len, result_len) bind(C, name='paste_from_clipboard_f') result(res)
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
            integer(c_int) :: res
        end function
    end interface

contains

    !> Pause briefly. Only for UI feedback that would otherwise be replaced
    !> before it could be seen; never for polling.
    subroutine platform_sleep_ms(ms)
        integer, intent(in) :: ms

        call fac_sleep_ms_c(int(ms, c_int))
    end subroutine platform_sleep_ms

    function get_temp_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_temp_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_home_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_home_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_path_separator() result(sep)
        character(len=1) :: sep
        sep = get_path_separator_c()
    end function

    function is_windows() result(res)
        logical :: res
        res = (is_windows_c() /= 0)
    end function

    function get_config_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_config_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_cwd() result(path)
        character(len=:), allocatable :: path
        character(len=1024) :: buffer
        integer(c_int) :: result_len

        call get_cwd_c(buffer, 1024_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function platform_copy_to_clipboard(text) result(success)
        character(len=*), intent(in) :: text
        logical :: success
        integer(c_int) :: res

        res = copy_to_clipboard_c(text, int(len_trim(text), c_int))
        success = (res /= 0)
    end function

    function platform_paste_from_clipboard() result(text)
        character(len=:), allocatable :: text
        character(len=100000), save :: buffer  ! save prevents stack overflow warning
        integer(c_int) :: result_len, res

        res = paste_from_clipboard_c(buffer, 100000_c_int, result_len)
        if (res /= 0 .and. result_len > 0) then
            text = trim(buffer(1:result_len))
        else
            text = ''
        end if
    end function

    ! True if `name` is an executable on PATH (POSIX `command -v`).
    ! cmdstat is mandatory: on shells where `command -v <missing>` exits 127
    ! (e.g. FreeBSD /bin/sh), execute_command_line would otherwise abort the
    ! program with "Invalid command line" instead of reporting not-found.
    function have_command(name) result(present)
        character(len=*), intent(in) :: name
        logical :: present
        integer :: st, cmd_st

        st = -1
        cmd_st = 0
        call execute_command_line('command -v ' // trim(name) // &
            ' >/dev/null 2>&1', wait=.true., exitstat=st, cmdstat=cmd_st)
        present = (cmd_st == 0 .and. st == 0)
    end function

    ! Detect the system package manager, native managers before brew so a
    ! Linux box with Homebrew still prefers apt/dnf/etc. Cached after first
    ! probe. Returns a lowercase token, or 'none' if nothing is found.
    function detect_system_pkg_mgr() result(mgr)
        character(len=:), allocatable :: mgr
        character(len=8), save :: cached = ''
        logical, save :: done = .false.

        if (done) then
            mgr = trim(cached)
            return
        end if

        if (have_command('pkg')) then
            cached = 'pkg'
        else if (have_command('pkg_add')) then
            cached = 'pkg_add'
        else if (have_command('apt')) then
            cached = 'apt'
        else if (have_command('dnf')) then
            cached = 'dnf'
        else if (have_command('yum')) then
            cached = 'yum'
        else if (have_command('pacman')) then
            cached = 'pacman'
        else if (have_command('zypper')) then
            cached = 'zypper'
        else if (have_command('apk')) then
            cached = 'apk'
        else if (have_command('xbps-install')) then
            cached = 'xbps'
        else if (have_command('brew')) then
            cached = 'brew'
        else
            cached = 'none'
        end if

        done = .true.
        mgr = trim(cached)
    end function

    ! Privilege-escalation prefix for system-package installs: prefer doas,
    ! then sudo, else empty (assume root / manual). Cached after first probe.
    ! Includes a trailing space so it can be concatenated directly.
    function detect_priv_prefix() result(prefix)
        character(len=:), allocatable :: prefix
        character(len=8), save :: cached = ''
        logical, save :: done = .false.

        ! Cache the bare tool name (no trailing space); the space is appended
        ! on every return so the cached path matches the first-call path.
        if (.not. done) then
            if (have_command('doas')) then
                cached = 'doas'
            else if (have_command('sudo')) then
                cached = 'sudo'
            else
                cached = ''
            end if
            done = .true.
        end if

        if (len_trim(cached) > 0) then
            prefix = trim(cached) // ' '
        else
            prefix = ''
        end if
    end function

end module platform_module
