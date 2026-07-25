program test_settings
    ! ~/.config/fac/settings.json -- fac's first real configuration file.
    !
    ! The behaviours worth pinning are the ones that decide whether a user
    ! ever loses preferences: a missing file must be normal rather than an
    ! error, a file we cannot parse must be moved aside rather than merged
    ! with defaults and then overwritten, and a save must be atomic so a
    ! crash mid-write cannot leave a truncated file behind.
    !
    ! Runs against a temporary HOME so the developer's real settings are
    ! never touched.
    use settings_module
    implicit none

    integer :: nfail
    character(len=:), allocatable :: home, path

    nfail = 0
    call setup_home(home)
    path = settings_path()
    call check(index(path, trim(home)) == 1, &
               'settings live under the configured home', path)

    call test_missing_file_is_normal()
    call test_round_trip()
    call test_types()
    call test_overwrite()
    call test_reload_from_disk()
    call test_corrupt_file_moved_aside()
    call test_atomic_write_leaves_no_tmp()

    call cleanup_home(home)

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All settings tests passed'

contains

    subroutine setup_home(h)
        character(len=:), allocatable, intent(out) :: h
        character(len=256) :: buf
        integer :: ios

        h = '/tmp/fac_settings_test'
        call execute_command_line('rm -rf ' // h // ' && mkdir -p ' // h, &
                                  wait=.true., exitstat=ios)
        call set_env('HOME', h)
        call set_env('XDG_CONFIG_HOME', h // '/.config')
        call get_environment_variable('HOME', buf)
        if (trim(buf) /= h) then
            print '(a)', 'SKIP: cannot override HOME in this environment'
            stop 0
        end if
    end subroutine setup_home

    subroutine set_env(name, val)
        use iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: name, val
        interface
            function c_setenv(n, v, o) result(r) bind(C, name='setenv')
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: n(*), v(*)
                integer(c_int), value :: o
                integer(c_int) :: r
            end function
        end interface
        integer :: ignored
        ignored = int(c_setenv(name // c_null_char, val // c_null_char, 1_c_int))
    end subroutine set_env

    subroutine cleanup_home(h)
        character(len=*), intent(in) :: h
        integer :: ios
        call execute_command_line('rm -rf ' // h, wait=.true., exitstat=ios)
    end subroutine cleanup_home

    subroutine write_file(p, contents)
        character(len=*), intent(in) :: p, contents
        integer :: unit, ios
        call execute_command_line('mkdir -p "$(dirname ' // p // ')"', &
                                  wait=.true., exitstat=ios)
        open(newunit=unit, file=p, status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        write(unit, '(a)') contents
        close(unit)
    end subroutine write_file

    logical function file_exists(p)
        character(len=*), intent(in) :: p
        inquire(file=p, exist=file_exists)
    end function file_exists

    ! --- A file that isn't there is the ordinary first-run case ---
    subroutine test_missing_file_is_normal()
        integer :: ios
        call execute_command_line('rm -f ' // settings_path(), wait=.true., exitstat=ios)
        call settings_reset()
        call settings_load()
        call check(settings_get_logical('ai.enabled', .false.) .eqv. .false., &
                   'missing file falls back to the default', '')
        call check(settings_get_integer('ai.debounce_ms', 150) == 150, &
                   'missing integer key falls back', '')
        call check(settings_get_string('ai.local.model', 'none') == 'none', &
                   'missing string key falls back', '')
    end subroutine test_missing_file_is_normal

    ! --- Values survive a save/load cycle ---
    subroutine test_round_trip()
        logical :: ok

        call settings_reset()
        call settings_load()
        call settings_set_logical('ai.enabled', .true.)
        call settings_set_integer('ai.debounce_ms', 175)
        call settings_set_string('ai.local.model', 'qwen2.5-coder:1.5b-base')
        call settings_save(ok)
        call check(ok, 'save reports success', '')
        call check(file_exists(settings_path()), 'the file exists after save', '')

        call settings_reset()
        call settings_load()
        call check(settings_get_logical('ai.enabled', .false.), &
                   'logical round-trips', '')
        call check(settings_get_integer('ai.debounce_ms', 0) == 175, &
                   'integer round-trips', int_str(settings_get_integer('ai.debounce_ms', 0)))
        call check(settings_get_string('ai.local.model', '') == &
                   'qwen2.5-coder:1.5b-base', 'string round-trips', &
                   settings_get_string('ai.local.model', ''))
    end subroutine test_round_trip

    ! --- A value of the wrong shape must not be coerced ---
    subroutine test_types()
        logical :: ok

        call settings_reset()
        call settings_load()
        call settings_set_string('ai.host', '127.0.0.1')
        call settings_save(ok)
        call settings_reset()
        call settings_load()

        call check(settings_get_integer('ai.host', 42) == 42, &
                   'a string key read as integer keeps the default', '')
        call check(settings_get_logical('ai.host', .true.), &
                   'a string key read as logical keeps the default', '')
        call check(settings_get_string('ai.host', '') == '127.0.0.1', &
                   'dotted keys with digits survive', &
                   settings_get_string('ai.host', ''))
    end subroutine test_types

    subroutine test_overwrite()
        logical :: ok

        call settings_reset()
        call settings_load()
        call settings_set_integer('ai.debounce_ms', 250)
        call settings_save(ok)
        call settings_reset()
        call settings_load()
        call check(settings_get_integer('ai.debounce_ms', 0) == 250, &
                   'setting an existing key replaces it', &
                   int_str(settings_get_integer('ai.debounce_ms', 0)))
        call check(settings_get_logical('ai.enabled', .false.), &
                   'other keys are untouched by the overwrite', '')
    end subroutine test_overwrite

    ! --- A file written by hand is read the same way ---
    subroutine test_reload_from_disk()
        call write_file(settings_path(), &
            '{' // new_line('a') // &
            '  "ai.enabled": false,' // new_line('a') // &
            '  "ai.remote.enabled": true,' // new_line('a') // &
            '  "ai.local.model": "starcoder2:3b"' // new_line('a') // &
            '}')
        call settings_reset()
        call settings_load()
        call check(.not. settings_get_logical('ai.enabled', .true.), &
                   'hand-written false is read', '')
        call check(settings_get_logical('ai.remote.enabled', .false.), &
                   'hand-written true is read', '')
        call check(settings_get_string('ai.local.model', '') == 'starcoder2:3b', &
                   'hand-written string is read', &
                   settings_get_string('ai.local.model', ''))
    end subroutine test_reload_from_disk

    ! --- Garbage must not be merged with defaults and then saved over ---
    subroutine test_corrupt_file_moved_aside()
        call write_file(settings_path(), 'this is not json at all')
        call settings_reset()
        call settings_load()

        call check(settings_get_logical('ai.enabled', .false.) .eqv. .false., &
                   'a corrupt file yields defaults', '')
        call check(file_exists(settings_path() // '.corrupted'), &
                   'the corrupt file is preserved for the user', '')
        call check(.not. file_exists(settings_path()), &
                   'and moved out of the way', '')

        ! unbalanced braces count as corrupt too
        call write_file(settings_path(), '{ "ai.enabled": true')
        call settings_reset()
        call settings_load()
        call check(settings_get_logical('ai.enabled', .false.) .eqv. .false., &
                   'unbalanced braces are treated as corrupt', '')
    end subroutine test_corrupt_file_moved_aside

    ! --- Atomicity: the temp file must never be left lying around ---
    subroutine test_atomic_write_leaves_no_tmp()
        logical :: ok

        call settings_reset()
        call settings_load()
        call settings_set_logical('ai.enabled', .true.)
        call settings_save(ok)
        call check(ok, 'save succeeds', '')
        call check(.not. file_exists(settings_path() // '.tmp'), &
                   'no .tmp file survives a successful save', '')
        call check(file_exists(settings_path()), 'the real file is in place', '')
    end subroutine test_atomic_write_leaves_no_tmp

    function int_str(v) result(s)
        integer, intent(in) :: v
        character(len=16) :: s
        write(s, '(i0)') v
    end function int_str

    subroutine check(ok, name, got)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name, got

        if (ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // trim(got) // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_settings
