! User settings in ~/.config/fac/settings.json.
!
! fac had no configuration system: config_module resolves the XDG directory
! and nothing else, and state.json carries three fields via hand-rolled
! index() matching. This is the first real one.
!
! Keys are flat dotted paths -- "ai.enabled", "ai.local.model" -- so the file
! is still a flat JSON object and the reader stays a line scanner. That is a
! deliberate constraint: a nested parser would mean either json_module (which
! leaks every parsed object and cannot decode \uXXXX) or a new one, and
! neither is worth it for a settings file.
!
! Defaults are supplied at each call site rather than held in a table, so
! there is no second list to drift out of step with the code that reads it.
module settings_module
    use config_module, only: get_config_dir, ensure_config_dir
    implicit none
    private

    public :: settings_load, settings_save, settings_path
    public :: settings_get_logical, settings_get_integer, settings_get_string
    public :: settings_set_logical, settings_set_integer, settings_set_string
    public :: settings_is_loaded, settings_reset

    integer, parameter :: MAX_SETTINGS = 128
    integer, parameter :: KEY_LEN = 64
    integer, parameter :: VAL_LEN = 512

    type :: setting_t
        character(len=KEY_LEN) :: key = ''
        character(len=VAL_LEN) :: value = ''
    end type setting_t

    type(setting_t) :: g_settings(MAX_SETTINGS)
    integer :: g_count = 0
    logical :: g_loaded = .false.

contains

    function settings_path() result(path)
        character(len=:), allocatable :: path
        character(len=:), allocatable :: dir

        call get_config_dir(dir)
        path = trim(dir) // '/settings.json'
    end function settings_path

    function settings_is_loaded() result(res)
        logical :: res
        res = g_loaded
    end function settings_is_loaded

    ! Drop everything in memory. Mainly for tests; also the right thing after
    ! a corrupt file is moved aside.
    subroutine settings_reset()
        g_count = 0
        g_loaded = .false.
    end subroutine settings_reset

    ! Read the file if it is there. A missing file is the normal case, not an
    ! error -- every key then falls back to its caller-supplied default. A
    ! file we cannot make sense of is moved aside rather than half-read, so a
    ! later save cannot silently destroy settings we failed to parse.
    subroutine settings_load()
        character(len=:), allocatable :: path
        character(len=1024) :: line
        character(len=:), allocatable :: key, val
        integer :: unit, ios, brace_depth
        logical :: exists, saw_open, malformed

        g_count = 0
        g_loaded = .true.
        path = settings_path()

        inquire(file=path, exist=exists)
        if (.not. exists) return

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        brace_depth = 0
        saw_open = .false.
        malformed = .false.

        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, '{') > 0) then
                brace_depth = brace_depth + 1
                saw_open = .true.
            end if
            if (index(line, '}') > 0) brace_depth = brace_depth - 1

            call parse_pair(line, key, val)
            if (len(key) > 0) call put(key, val)
        end do
        close(unit)

        ! A file with no object at all, or unbalanced braces, is not something
        ! we wrote; treat it as corrupt rather than merging with defaults.
        if (.not. saw_open .or. brace_depth /= 0) malformed = .true.
        if (malformed) then
            g_count = 0
            call move_aside(path)
        end if
    end subroutine settings_load

    ! Rename the unreadable file so the user can recover it, and so the next
    ! save starts from a clean slate instead of appending to nonsense.
    subroutine move_aside(path)
        character(len=*), intent(in) :: path
        integer :: unit_in, unit_out, ios
        character(len=1024) :: line
        logical :: ok

        open(newunit=unit_in, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        open(newunit=unit_out, file=path // '.corrupted', status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            close(unit_in)
            return
        end if
        do
            read(unit_in, '(a)', iostat=ios) line
            if (ios /= 0) exit
            write(unit_out, '(a)') trim(line)
        end do
        close(unit_in)
        close(unit_out)

        open(newunit=unit_in, file=path, status='old', iostat=ios)
        if (ios == 0) close(unit_in, status='delete')
        ok = .true.
    end subroutine move_aside

    ! Write atomically: a full file at a temporary name, then rename over the
    ! target. docs/config_spec.md requires this and nothing in the tree did it
    ! except backup_module; a torn settings file would lose every preference
    ! at once.
    subroutine settings_save(success)
        logical, intent(out), optional :: success
        character(len=:), allocatable :: path, tmp
        integer :: unit, ios, i
        logical :: ok

        ok = .false.
        if (present(success)) success = .false.

        call ensure_config_dir(ok)
        if (.not. ok) return

        path = settings_path()
        tmp = path // '.tmp'

        open(newunit=unit, file=tmp, status='replace', action='write', iostat=ios)
        if (ios /= 0) return

        write(unit, '(a)') '{'
        do i = 1, g_count
            if (i < g_count) then
                write(unit, '(a)') '  "' // trim(g_settings(i)%key) // '": ' // &
                                   trim(g_settings(i)%value) // ','
            else
                write(unit, '(a)') '  "' // trim(g_settings(i)%key) // '": ' // &
                                   trim(g_settings(i)%value)
            end if
        end do
        write(unit, '(a)') '}'
        close(unit, iostat=ios)
        if (ios /= 0) return

        call rename_over(tmp, path, ok)
        if (present(success)) success = ok
    end subroutine settings_save

    ! Fortran has no rename intrinsic. Copy-then-delete is not atomic, so use
    ! the C library through a tiny helper rather than shelling out -- the
    ! config spec forbids running commands as part of config handling.
    subroutine rename_over(from, to, ok)
        use iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: from, to
        logical, intent(out) :: ok

        interface
            function c_rename(old, new) result(res) bind(C, name='rename')
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: old(*), new(*)
                integer(c_int) :: res
            end function
        end interface

        ok = c_rename(from // c_null_char, to // c_null_char) == 0
    end subroutine rename_over

    ! ------------------------------------------------------------------
    ! Typed access
    ! ------------------------------------------------------------------

    function settings_get_logical(key, default_value) result(v)
        character(len=*), intent(in) :: key
        logical, intent(in) :: default_value
        logical :: v
        character(len=:), allocatable :: raw

        v = default_value
        raw = get_raw(key)
        if (len(raw) == 0) return
        if (raw == 'true') then
            v = .true.
        else if (raw == 'false') then
            v = .false.
        end if
    end function settings_get_logical

    function settings_get_integer(key, default_value) result(v)
        character(len=*), intent(in) :: key
        integer, intent(in) :: default_value
        integer :: v
        character(len=:), allocatable :: raw
        integer :: ios, tmp

        v = default_value
        raw = get_raw(key)
        if (len(raw) == 0) return
        read(raw, *, iostat=ios) tmp
        if (ios == 0) v = tmp
    end function settings_get_integer

    function settings_get_string(key, default_value) result(v)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: default_value
        character(len=:), allocatable :: v
        character(len=:), allocatable :: raw

        v = default_value
        raw = get_raw(key)
        if (len(raw) < 2) return
        if (raw(1:1) /= '"' .or. raw(len(raw):len(raw)) /= '"') return
        v = unquote(raw(2:len(raw)-1))
    end function settings_get_string

    subroutine settings_set_logical(key, v)
        character(len=*), intent(in) :: key
        logical, intent(in) :: v

        if (v) then
            call put(key, 'true')
        else
            call put(key, 'false')
        end if
    end subroutine settings_set_logical

    subroutine settings_set_integer(key, v)
        character(len=*), intent(in) :: key
        integer, intent(in) :: v
        character(len=32) :: s

        write(s, '(i0)') v
        call put(key, trim(s))
    end subroutine settings_set_integer

    subroutine settings_set_string(key, v)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: v

        call put(key, '"' // quote(v) // '"')
    end subroutine settings_set_string

    ! ------------------------------------------------------------------
    ! Internals
    ! ------------------------------------------------------------------

    function get_raw(key) result(raw)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: raw
        integer :: i

        raw = ''
        if (.not. g_loaded) call settings_load()
        do i = 1, g_count
            if (trim(g_settings(i)%key) == key) then
                raw = trim(g_settings(i)%value)
                return
            end if
        end do
    end function get_raw

    subroutine put(key, raw_value)
        character(len=*), intent(in) :: key, raw_value
        integer :: i

        if (len_trim(key) == 0 .or. len_trim(key) > KEY_LEN) return
        if (len_trim(raw_value) > VAL_LEN) return

        do i = 1, g_count
            if (trim(g_settings(i)%key) == key) then
                g_settings(i)%value = raw_value
                return
            end if
        end do

        if (g_count >= MAX_SETTINGS) return
        g_count = g_count + 1
        g_settings(g_count)%key = key
        g_settings(g_count)%value = raw_value
    end subroutine put

    ! Pull "key": value out of one line. Values are kept in their raw JSON
    ! form (quotes included for strings) so writing back is byte-identical.
    subroutine parse_pair(line, key, val)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key, val
        integer :: q1, q2, colon, i, last

        key = ''
        val = ''

        q1 = index(line, '"')
        if (q1 <= 0) return
        q2 = index(line(q1+1:), '"')
        if (q2 <= 0) return
        q2 = q1 + q2

        colon = index(line(q2+1:), ':')
        if (colon <= 0) return
        colon = q2 + colon

        key = line(q1+1:q2-1)

        ! Value runs to the end of the line, minus a trailing comma
        i = colon + 1
        do while (i <= len(line))
            if (line(i:i) /= ' ' .and. line(i:i) /= achar(9)) exit
            i = i + 1
        end do
        last = len_trim(line)
        if (last >= i) then
            if (line(last:last) == ',') last = last - 1
        end if
        if (last >= i) val = line(i:last)
    end subroutine parse_pair

    ! Minimal JSON string escaping. Settings values are paths, model names
    ! and hosts; anything needing \uXXXX does not belong in this file.
    function quote(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        do i = 1, len(s)
            select case(s(i:i))
            case('"')
                out = out // '\"'
            case('\')
                out = out // '\\'
            case default
                if (iachar(s(i:i)) >= 32) out = out // s(i:i)
            end select
        end do
    end function quote

    function unquote(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        i = 1
        do while (i <= len(s))
            if (s(i:i) == '\' .and. i < len(s)) then
                out = out // s(i+1:i+1)
                i = i + 2
            else
                out = out // s(i:i)
                i = i + 1
            end if
        end do
    end function unquote

end module settings_module
