module platform_module
    use iso_fortran_env, only: int64
    use iso_c_binding
    implicit none
    private

    public :: get_temp_dir, get_home_dir, get_path_separator, is_windows
    public :: get_config_dir, get_cwd
    public :: canonical_path
    public :: platform_copy_to_clipboard, platform_paste_from_clipboard
    public :: detect_system_pkg_mgr, detect_priv_prefix
    public :: platform_sleep_ms, platform_now_ms

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

    !> One spelling per file path.
    !>
    !> Two tabs on the same file are matched by comparing their stored
    !> filenames as plain strings -- on every non-cursor keystroke, from
    !> sync_buffer_to_all_instances. So `./a.c`, `a.c`, `dir//a.c` and
    !> `dir/./a.c` are four different files to that comparison, and two tabs
    !> holding the same document silently diverge until whichever saves last
    !> wins. Normalising once, where the name is stored, keeps the hot path a
    !> bare string compare.
    !>
    !> Lexical only: no getcwd, no readlink, no stat. A relative path and an
    !> absolute one still compare unequal. Resolving that needs the working
    !> directory, and the workspace file deliberately stores paths relative to
    !> the workspace root -- rewriting them here would fight that. This handles
    !> the spellings the editor actually generates for itself.
    function canonical_path(path) result(out)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: out
        character(len=:), allocatable :: work
        integer :: i, n, seg_start
        ! Segments as BOUNDS into `work`, not as copies of it.
        !
        ! This was an allocatable array of deferred-length characters, which
        ! gfortran reports as "used uninitialized" however thoroughly it is
        ! initialised -- a long-standing false positive for that one shape.
        ! Indices sidestep it, and they are the better representation anyway:
        ! nothing here needs a segment's text except to compare it, so the
        ! copies were only ever costing allocations.
        integer, allocatable :: seg_a(:), seg_b(:)
        integer :: n_segs
        logical :: absolute

        work = trim(adjustl(path))
        if (len(work) == 0) then
            out = ''
            return
        end if

        absolute = (work(1:1) == '/')

        ! Split on '/', dropping empty segments (which collapses '//') and
        ! '.' segments, and cancelling a '..' against the segment before it.
        n = 0
        do i = 1, len(work)
            if (work(i:i) == '/') n = n + 1
        end do
        allocate(seg_a(n + 1), seg_b(n + 1))
        n_segs = 0
        seg_start = 1
        do i = 1, len(work) + 1
            if (i > len(work)) then
                call push_segment(work, seg_a, seg_b, n_segs, absolute, &
                                  seg_start, len(work))
            else if (work(i:i) == '/') then
                if (i > seg_start) &
                    call push_segment(work, seg_a, seg_b, n_segs, absolute, &
                                      seg_start, i - 1)
                seg_start = i + 1
            end if
        end do

        out = ''
        do i = 1, n_segs
            if (len(out) > 0) then
                out = out // '/' // work(seg_a(i):seg_b(i))
            else
                out = work(seg_a(i):seg_b(i))
            end if
        end do

        if (absolute) then
            out = '/' // out
        else if (len(out) == 0) then
            ! Everything cancelled out: '.' is the honest answer, not ''.
            out = '.'
        end if

    end function canonical_path

    !> Record one path segment, applying '.' and '..'.
    !>
    !> `a`/`b` are the segment's bounds in `work`; the kept segments are
    !> accumulated as bounds too. A module procedure taking what it touches,
    !> rather than the contained one it used to be, which reached into the
    !> frame above by host association.
    subroutine push_segment(work, seg_a, seg_b, n_segs, absolute, a, b)
        character(len=*), intent(in) :: work
        integer, intent(inout) :: seg_a(:), seg_b(:)
        integer, intent(inout) :: n_segs
        logical, intent(in) :: absolute
        integer, intent(in) :: a, b

        if (b < a) return
        if (work(a:b) == '.') return
        if (work(a:b) == '..') then
            ! Cancel against a real segment, but never walk above an
            ! absolute root, and keep leading '..' on a relative path
            ! because there is nothing here to cancel them against.
            if (n_segs > 0) then
                if (work(seg_a(n_segs):seg_b(n_segs)) /= '..') then
                    n_segs = n_segs - 1
                    return
                end if
            end if
            if (absolute) return
        end if
        n_segs = n_segs + 1
        seg_a(n_segs) = a
        seg_b(n_segs) = b
    end subroutine push_segment

    !> Pause briefly. Only for UI feedback that would otherwise be replaced
    !> before it could be seen; never for polling.
    subroutine platform_sleep_ms(ms)
        integer, intent(in) :: ms

        call fac_sleep_ms_c(int(ms, c_int))
    end subroutine platform_sleep_ms

    !> Milliseconds from an arbitrary origin, for measuring elapsed time.
    !>
    !> Only differences are meaningful -- the origin is whatever system_clock
    !> counts from. Deliberately not date_and_time: a wall clock can step
    !> backwards over an NTP correction or a daylight-saving change, and a
    !> deadline computed across that would either fire at once or never.
    function platform_now_ms() result(ms)
        integer(int64) :: ms
        integer(int64) :: ticks, rate

        call system_clock(ticks, rate)
        if (rate <= 0) then
            ms = 0
        else
            ms = ticks * 1000_int64 / rate
        end if
    end function platform_now_ms

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
