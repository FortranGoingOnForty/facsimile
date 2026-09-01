program test_theme
    use fgof_screen_types, only: screen_style, SCREEN_COLOR_MONO, &
        SCREEN_COLOR_256, SCREEN_COLOR_TRUECOLOR
    use settings_module, only: settings_reset, settings_set_string
    use theme_module
    implicit none

    integer :: nfail
    logical :: ok
    character(len=:), allocatable :: message
    character(len=:), allocatable :: sequence
    character(len=64), allocatable :: names(:)
    integer :: count
    type(screen_style) :: style

    nfail = 0
    call setup_config()

    call theme_init()
    call check(theme_current_id() == 'steel', 'steel is the default theme')
    call check(theme_color_mode() == SCREEN_COLOR_256, &
               'xterm-256color selects the 256-color renderer')
    style = theme_style(THEME_TAB_ACTIVE)
    call check(style%inverse, 'the active tab retains reverse-video semantics')
    style = theme_style(THEME_DISABLED)
    call check(style%dim .and. style%italic, &
               'disabled controls have a distinct text variant')

    call theme_list(names, count)
    call check(count >= 4, 'the four built-in themes are discoverable')
    call theme_select('paper', .false., ok, message)
    call check(ok .and. theme_current_id() == 'paper', &
               'a built-in light theme can be selected')

    call settings_set_string('ui.color_mode', 'truecolor')
    call theme_reload(ok, message)
    call check(ok .and. theme_color_mode() == SCREEN_COLOR_TRUECOLOR, &
               'an explicit truecolor capability can be selected')
    sequence = theme_background_sgr(THEME_CURRENT_LINE)
    call check(index(sequence, achar(27) // '[0m') == 0 .and. &
               index(sequence, achar(27) // '[48;2;') > 0, &
               'background overlays preserve an existing syntax foreground')

    call write_custom_theme()
    call theme_select('test-modern', .false., ok, message)
    call check(ok .and. theme_current_name() == 'Test Modern', &
               'a custom TOML theme loads from the config directory')
    style = theme_style(THEME_TAB_ACTIVE)
    call check(style%bold .and. .not. style%inverse, &
               'custom attrs replace inherited attrs')
    call check(style%fg_truecolor .and. all(style%fg_rgb == [255, 112, 136]), &
               'custom palette references set the visible foreground')
    call check(style%bg_truecolor .and. all(style%bg_rgb == [1, 2, 3]), &
               'custom literal colors set the visible background')

    style = theme_style(THEME_PANEL_SELECTION)
    call check(style%inverse, 'custom selections may opt into reverse video')
    call check(all(style%bg_rgb == [255, 112, 136]) .and. &
               all(style%fg_rgb == [1, 2, 3]), &
               'inverse styles preserve visible fg/bg semantics internally')

    style = theme_style(THEME_TAB_HOVER)
    call check(style%inverse .and. all(style%fg_rgb == [52, 65, 80]) .and. &
               all(style%bg_rgb == [215, 222, 232]), &
               'adding inverse preserves inherited visible colors')
    style = theme_style(THEME_SELECTION)
    call check(.not. style%inverse .and. all(style%fg_rgb == [29, 35, 44]) .and. &
               all(style%bg_rgb == [143, 169, 196]), &
               'removing inverse preserves inherited visible colors')

    call theme_select('missing-theme', .false., ok, message)
    call check(.not. ok .and. theme_current_id() == 'test-modern', &
               'an invalid theme leaves the active theme unchanged')

    call settings_set_string('ui.icons', 'ascii')
    call settings_set_string('ui.color_mode', 'auto')
    call set_env('NO_COLOR', '1')
    call theme_reload(ok, message)
    call check(ok .and. theme_color_mode() == SCREEN_COLOR_MONO, &
               'NO_COLOR selects the monochrome renderer in auto mode')
    call check(theme_glyph('directory_closed') == '+', &
               'ASCII icon mode avoids Unicode glyphs')
    call check(.not. theme_shadows_enabled(), &
               'drop shadows are disabled in monochrome mode')
    style = theme_style(THEME_TAB_ACTIVE)
    call check(.not. style%fg_truecolor .and. .not. style%bg_truecolor .and. &
               style%fg < 0 .and. style%bg < 0, &
               'monochrome downgrade removes all colors')

    call unset_env('NO_COLOR')
    call execute_command_line('rm -rf /tmp/fac_theme_test', wait=.true.)

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All theme tests passed'

contains

    subroutine setup_config()
        integer :: ios
        call execute_command_line( &
            'rm -rf /tmp/fac_theme_test && mkdir -p /tmp/fac_theme_test/config/fac/themes', &
            wait=.true., exitstat=ios)
        call set_env('HOME', '/tmp/fac_theme_test')
        call set_env('XDG_CONFIG_HOME', '/tmp/fac_theme_test/config')
        call set_env('TERM', 'xterm-256color')
        call unset_env('COLORTERM')
        call unset_env('NO_COLOR')
        call settings_reset()
    end subroutine setup_config

    subroutine write_custom_theme()
        integer :: unit, ios
        open(newunit=unit, file='/tmp/fac_theme_test/config/fac/themes/test-modern.toml', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            call check(.false., 'custom theme fixture can be created')
            return
        end if
        write(unit, '(a)') '[metadata]'
        write(unit, '(a)') 'name = "Test Modern"'
        write(unit, '(a)') 'extends = "steel"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[palette]'
        write(unit, '(a)') 'coral = "#ff7088"'
        write(unit, '(a)') 'ink = "#010203"'
        write(unit, '(a)') ''
        write(unit, '(a)') '[styles.tab.active]'
        write(unit, '(a)') 'fg = "coral"'
        write(unit, '(a)') 'bg = "ink"'
        write(unit, '(a)') 'attrs = ["bold"]'
        write(unit, '(a)') ''
        write(unit, '(a)') '[styles.panel.selection]'
        write(unit, '(a)') 'fg = "coral"'
        write(unit, '(a)') 'bg = "ink"'
        write(unit, '(a)') 'attrs = ["inverse"]'
        write(unit, '(a)') ''
        write(unit, '(a)') '[styles.tab.hover]'
        write(unit, '(a)') 'attrs = ["inverse"]'
        write(unit, '(a)') ''
        write(unit, '(a)') '[styles.selection]'
        write(unit, '(a)') 'attrs = ["bold"]'
        close(unit)
    end subroutine write_custom_theme

    subroutine check(condition, name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: name
        if (condition) then
            print '(a)', 'ok   ' // trim(name)
        else
            print '(a)', 'FAIL ' // trim(name)
            nfail = nfail + 1
        end if
    end subroutine check

    subroutine set_env(name, value)
        use iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: name, value
        interface
            function c_setenv(n, v, overwrite) result(rc) bind(C, name='setenv')
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: n(*), v(*)
                integer(c_int), value :: overwrite
                integer(c_int) :: rc
            end function c_setenv
        end interface
        integer :: ignored
        ignored = int(c_setenv(name // c_null_char, value // c_null_char, 1_c_int))
    end subroutine set_env

    subroutine unset_env(name)
        use iso_c_binding, only: c_char, c_null_char
        character(len=*), intent(in) :: name
        interface
            function c_unsetenv(n) result(rc) bind(C, name='unsetenv')
                import :: c_char
                character(kind=c_char), intent(in) :: n(*)
                integer :: rc
            end function c_unsetenv
        end interface
        integer :: ignored
        ignored = c_unsetenv(name // c_null_char)
    end subroutine unset_env

end program test_theme
