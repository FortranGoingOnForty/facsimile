module theme_module
    use fgof_screen_types, only: screen_style, SCREEN_COLOR_MONO, &
        SCREEN_COLOR_BASIC, SCREEN_COLOR_256, SCREEN_COLOR_TRUECOLOR
    use fgof_toml, only: TOML_KIND_STRING, toml_array, toml_document, &
        toml_error, toml_table, parse_file
    use config_module, only: get_config_dir
    use settings_module, only: settings_get_logical, settings_get_string, &
        settings_set_string, settings_save
    use dir_scan_module, only: dir_entry_t, list_directory
    implicit none
    private

    integer, parameter, public :: THEME_EDITOR = 1
    integer, parameter, public :: THEME_EDITOR_BG = 2
    integer, parameter, public :: THEME_MUTED = 3
    integer, parameter, public :: THEME_ACCENT = 4
    integer, parameter, public :: THEME_SELECTION = 5
    integer, parameter, public :: THEME_SELECTION_INACTIVE = 6
    integer, parameter, public :: THEME_TAB_BAR = 7
    integer, parameter, public :: THEME_TAB_ACTIVE = 8
    integer, parameter, public :: THEME_TAB_INACTIVE = 9
    integer, parameter, public :: THEME_TAB_MODIFIED = 10
    integer, parameter, public :: THEME_TAB_ORPHAN = 11
    integer, parameter, public :: THEME_TAB_HOVER = 12
    integer, parameter, public :: THEME_TAB_DRAG = 13
    integer, parameter, public :: THEME_STATUS = 14
    integer, parameter, public :: THEME_STATUS_ACCENT = 15
    integer, parameter, public :: THEME_PANEL = 16
    integer, parameter, public :: THEME_PANEL_HEADER = 17
    integer, parameter, public :: THEME_PANEL_FOOTER = 18
    integer, parameter, public :: THEME_PANEL_SELECTION = 19
    integer, parameter, public :: THEME_BORDER = 20
    integer, parameter, public :: THEME_BORDER_FOCUS = 21
    integer, parameter, public :: THEME_SHADOW = 22
    integer, parameter, public :: THEME_ERROR = 23
    integer, parameter, public :: THEME_WARNING = 24
    integer, parameter, public :: THEME_INFO = 25
    integer, parameter, public :: THEME_HINT = 26
    integer, parameter, public :: THEME_SUCCESS = 27
    integer, parameter, public :: THEME_GIT_ADDED = 28
    integer, parameter, public :: THEME_GIT_MODIFIED = 29
    integer, parameter, public :: THEME_GIT_DELETED = 30
    integer, parameter, public :: THEME_DIRECTORY = 31
    integer, parameter, public :: THEME_EXECUTABLE = 32
    integer, parameter, public :: THEME_GHOST = 33
    integer, parameter, public :: THEME_LINE_NUMBER = 34
    integer, parameter, public :: THEME_LINE_NUMBER_ACTIVE = 35
    integer, parameter, public :: THEME_SYNTAX_KEYWORD = 36
    integer, parameter, public :: THEME_SYNTAX_STRING = 37
    integer, parameter, public :: THEME_SYNTAX_COMMENT = 38
    integer, parameter, public :: THEME_SYNTAX_NUMBER = 39
    integer, parameter, public :: THEME_SYNTAX_TYPE = 40
    integer, parameter, public :: THEME_SYNTAX_FUNCTION = 41
    integer, parameter, public :: THEME_SYNTAX_PREPROCESSOR = 42
    integer, parameter, public :: THEME_SEARCH_MATCH = 43
    integer, parameter, public :: THEME_CURRENT_LINE = 44
    integer, parameter, public :: THEME_DISABLED = 45
    integer, parameter, public :: THEME_ROLE_COUNT = 45

    integer, parameter, public :: ICONS_ASCII = 0
    integer, parameter, public :: ICONS_UNICODE = 1
    integer, parameter, public :: ICONS_NERD = 2

    character(len=32), parameter :: ROLE_NAMES(THEME_ROLE_COUNT) = [character(len=32) :: &
        'editor', 'editor.background', 'muted', 'accent', 'selection', &
        'selection.inactive', 'tab.bar', 'tab.active', 'tab.inactive', &
        'tab.modified', 'tab.orphan', 'tab.hover', 'tab.drag', 'status', &
        'status.accent', 'panel', 'panel.header', 'panel.footer', &
        'panel.selection', 'border', 'border.focus', 'shadow', 'diagnostic.error', &
        'diagnostic.warning', 'diagnostic.info', 'diagnostic.hint', 'success', &
        'git.added', 'git.modified', 'git.deleted', 'directory', 'executable', &
        'ghost', 'line_number', 'line_number.active', 'syntax.keyword', &
        'syntax.string', 'syntax.comment', 'syntax.number', 'syntax.type', &
        'syntax.function', 'syntax.preprocessor', 'search.match', 'current_line', &
        'disabled']

    type, public :: theme_t
        character(len=64) :: name = 'Facsimile Steel'
        character(len=32) :: id = 'steel'
        logical :: light = .false.
        type(screen_style) :: styles(THEME_ROLE_COUNT)
    end type theme_t

    type(theme_t), save :: current_theme
    logical, save :: initialized = .false.
    integer, save :: current_color_mode = SCREEN_COLOR_TRUECOLOR
    integer, save :: current_icons = ICONS_UNICODE
    logical, save :: current_shadows = .true.

    public :: theme_init, theme_reload, theme_select
    public :: theme_current, theme_current_name, theme_current_id
    public :: theme_style, theme_sgr, theme_background_sgr, theme_reset, theme_paint
    public :: theme_color_mode, theme_icon_mode, theme_glyph
    public :: theme_list, theme_shadows_enabled, theme_role_name

contains

    subroutine theme_init()
        character(len=:), allocatable :: requested
        character(len=:), allocatable :: message
        logical :: ok

        call detect_capabilities()
        requested = settings_get_string('ui.theme', 'steel')
        call theme_select(requested, .false., ok, message)
        if (.not. ok) call load_builtin('steel', current_theme, ok)
        initialized = .true.
    end subroutine theme_init

    subroutine theme_reload(ok, message)
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: message
        character(len=32) :: selected

        if (.not. initialized) call theme_init()
        selected = current_theme%id
        call detect_capabilities()
        call theme_select(trim(selected), .false., ok, message)
    end subroutine theme_reload

    subroutine theme_select(name, persist, ok, message)
        character(len=*), intent(in) :: name
        logical, intent(in) :: persist
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: message
        type(theme_t) :: candidate
        character(len=:), allocatable :: normalized
        logical :: saved

        normalized = lowercase(trim(name))
        call load_builtin(normalized, candidate, ok)
        if (.not. ok) call load_custom_theme(trim(name), candidate, ok, message)
        if (.not. ok) then
            if (.not. allocated(message)) message = 'theme not found: ' // trim(name)
            return
        end if

        current_theme = candidate
        initialized = .true.
        message = 'Theme: ' // trim(candidate%name)
        if (persist) then
            call settings_set_string('ui.theme', trim(candidate%id))
            call settings_save(saved)
            if (.not. saved) then
                ok = .false.
                message = 'Theme applied, but settings could not be saved'
            end if
        end if
    end subroutine theme_select

    function theme_current() result(theme)
        type(theme_t) :: theme
        if (.not. initialized) call theme_init()
        theme = current_theme
    end function theme_current

    function theme_current_name() result(name)
        character(len=:), allocatable :: name
        if (.not. initialized) call theme_init()
        name = trim(current_theme%name)
    end function theme_current_name

    function theme_current_id() result(name)
        character(len=:), allocatable :: name
        if (.not. initialized) call theme_init()
        name = trim(current_theme%id)
    end function theme_current_id

    function theme_style(role) result(style)
        integer, intent(in) :: role
        type(screen_style) :: style

        if (.not. initialized) call theme_init()
        style = blank_style()
        if (role >= 1 .and. role <= THEME_ROLE_COUNT) style = current_theme%styles(role)
        call downgrade_style(style)
    end function theme_style

    function theme_sgr(role) result(sequence)
        integer, intent(in) :: role
        character(len=:), allocatable :: sequence
        type(screen_style) :: style

        style = theme_style(role)
        sequence = achar(27) // '[0m'
        if (style%bold) sequence = sequence // achar(27) // '[1m'
        if (style%dim) sequence = sequence // achar(27) // '[2m'
        if (style%italic) sequence = sequence // achar(27) // '[3m'
        if (style%underline) sequence = sequence // achar(27) // '[4m'
        if (style%inverse) sequence = sequence // achar(27) // '[7m'
        if (style%strikethrough) sequence = sequence // achar(27) // '[9m'
        if (current_color_mode == SCREEN_COLOR_MONO) return
        if (style%fg_truecolor) then
            sequence = sequence // rgb_sgr(style%fg_rgb, .false.)
        else if (style%fg >= 0) then
            sequence = sequence // indexed_sgr(style%fg, .false.)
        end if
        if (style%bg_truecolor) then
            sequence = sequence // rgb_sgr(style%bg_rgb, .true.)
        else if (style%bg >= 0) then
            sequence = sequence // indexed_sgr(style%bg, .true.)
        end if
    end function theme_sgr

    function theme_background_sgr(role) result(sequence)
        integer, intent(in) :: role
        character(len=:), allocatable :: sequence
        type(screen_style) :: style

        style = theme_style(role)
        sequence = ''
        if (current_color_mode == SCREEN_COLOR_MONO) return
        if (style%inverse .and. style%fg_truecolor) then
            sequence = rgb_sgr(style%fg_rgb, .true.)
        else if (style%inverse .and. style%fg >= 0) then
            sequence = indexed_sgr(style%fg, .true.)
        else if (style%bg_truecolor) then
            sequence = rgb_sgr(style%bg_rgb, .true.)
        else if (style%bg >= 0) then
            sequence = indexed_sgr(style%bg, .true.)
        end if
    end function theme_background_sgr

    function theme_reset() result(sequence)
        character(len=:), allocatable :: sequence
        sequence = achar(27) // '[0m'
    end function theme_reset

    function theme_paint(role, text) result(styled)
        integer, intent(in) :: role
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: styled
        styled = theme_sgr(role) // text // theme_reset()
    end function theme_paint

    integer function theme_color_mode() result(mode)
        if (.not. initialized) call theme_init()
        mode = current_color_mode
    end function theme_color_mode

    integer function theme_icon_mode() result(mode)
        if (.not. initialized) call theme_init()
        mode = current_icons
    end function theme_icon_mode

    logical function theme_shadows_enabled() result(enabled)
        if (.not. initialized) call theme_init()
        enabled = current_shadows .and. current_color_mode /= SCREEN_COLOR_MONO
    end function theme_shadows_enabled

    function theme_role_name(role) result(name)
        integer, intent(in) :: role
        character(len=:), allocatable :: name
        name = ''
        if (role >= 1 .and. role <= THEME_ROLE_COUNT) name = trim(ROLE_NAMES(role))
    end function theme_role_name

    function theme_glyph(kind) result(glyph)
        character(len=*), intent(in) :: kind
        character(len=:), allocatable :: glyph

        if (.not. initialized) call theme_init()
        select case (trim(kind))
        case ('add')
            glyph = '+'
        case ('close')
            if (current_icons /= ICONS_ASCII) then
                glyph = '×'
            else
                glyph = 'x'
            end if
        case ('chevron_right')
            if (current_icons /= ICONS_ASCII) then
                glyph = '›'
            else
                glyph = '>'
            end if
        case ('chevron_left')
            if (current_icons /= ICONS_ASCII) then
                glyph = '‹'
            else
                glyph = '<'
            end if
        case ('directory_open')
            if (current_icons == ICONS_NERD) then
                glyph = ''
            else if (current_icons == ICONS_UNICODE) then
                glyph = '▾'
            else
                glyph = '-'
            end if
        case ('directory_closed')
            if (current_icons == ICONS_NERD) then
                glyph = ''
            else if (current_icons == ICONS_UNICODE) then
                glyph = '▸'
            else
                glyph = '+'
            end if
        case ('file')
            if (current_icons == ICONS_NERD) then
                glyph = ''
            else if (current_icons == ICONS_UNICODE) then
                glyph = '·'
            else
                glyph = ' '
            end if
        case ('symbol')
            if (current_icons == ICONS_NERD) then
                glyph = '◆'
            else if (current_icons == ICONS_UNICODE) then
                glyph = '◆'
            else
                glyph = '#'
            end if
        case ('modified')
            if (current_icons /= ICONS_ASCII) then
                glyph = '●'
            else
                glyph = '*'
            end if
        case ('error')
            if (current_icons /= ICONS_ASCII) then
                glyph = '●'
            else
                glyph = 'E'
            end if
        case ('warning')
            if (current_icons /= ICONS_ASCII) then
                glyph = '▲'
            else
                glyph = 'W'
            end if
        case ('info')
            if (current_icons /= ICONS_ASCII) then
                glyph = '●'
            else
                glyph = 'i'
            end if
        case ('success')
            if (current_icons /= ICONS_ASCII) then
                glyph = '✓'
            else
                glyph = '+'
            end if
        case ('grab')
            if (current_icons /= ICONS_ASCII) then
                glyph = '⋮'
            else
                glyph = '|'
            end if
        case default
            glyph = '?'
        end select
    end function theme_glyph

    subroutine theme_list(names, count)
        character(len=64), allocatable, intent(out) :: names(:)
        integer, intent(out) :: count
        type(dir_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: config_dir
        character(len=64), allocatable :: grown(:)
        integer :: n_entries
        integer :: i
        integer :: dot
        logical :: ok

        allocate(names(4))
        names = [character(len=64) :: 'steel', 'graphite', 'paper', 'legacy']
        count = 4
        call get_config_dir(config_dir)
        call list_directory(trim(config_dir) // '/themes', entries, n_entries, ok)
        if (.not. ok) return
        do i = 1, n_entries
            if (entries(i)%is_dir) cycle
            dot = len_trim(entries(i)%name) - 4
            if (dot < 1) cycle
            if (entries(i)%name(dot:) /= '.toml') cycle
            allocate(grown(count + 1))
            grown(:count) = names
            grown(count + 1) = entries(i)%name(:dot - 1)
            call move_alloc(grown, names)
            count = count + 1
        end do
    end subroutine theme_list

    subroutine detect_capabilities()
        character(len=128) :: value
        character(len=:), allocatable :: configured
        integer :: status

        current_color_mode = SCREEN_COLOR_256
        call get_environment_variable('TERM', value)
        if (trim(value) == 'dumb') current_color_mode = SCREEN_COLOR_MONO
        call get_environment_variable('COLORTERM', value)
        if (index(lowercase(trim(value)), 'truecolor') > 0 .or. &
            index(lowercase(trim(value)), '24bit') > 0) current_color_mode = SCREEN_COLOR_TRUECOLOR
        call get_environment_variable('NO_COLOR', value, status=status)
        if (status == 0) current_color_mode = SCREEN_COLOR_MONO

        configured = lowercase(settings_get_string('ui.color_mode', 'auto'))
        select case (configured)
        case ('mono')
            current_color_mode = SCREEN_COLOR_MONO
        case ('basic')
            current_color_mode = SCREEN_COLOR_BASIC
        case ('256')
            current_color_mode = SCREEN_COLOR_256
        case ('truecolor')
            current_color_mode = SCREEN_COLOR_TRUECOLOR
        end select

        configured = lowercase(settings_get_string('ui.icons', 'unicode'))
        select case (configured)
        case ('ascii')
            current_icons = ICONS_ASCII
        case ('nerd')
            current_icons = ICONS_NERD
        case default
            current_icons = ICONS_UNICODE
        end select
        current_shadows = settings_get_logical('ui.shadows', .true.)
    end subroutine detect_capabilities

    subroutine load_builtin(id, theme, ok)
        character(len=*), intent(in) :: id
        type(theme_t), intent(out) :: theme
        logical, intent(out) :: ok

        select case (lowercase(trim(id)))
        case ('steel', 'facsimile steel')
            call build_steel(theme)
            ok = .true.
        case ('graphite', 'facsimile graphite')
            call build_graphite(theme)
            ok = .true.
        case ('paper', 'facsimile paper')
            call build_paper(theme)
            ok = .true.
        case ('legacy')
            call build_legacy(theme)
            ok = .true.
        case default
            theme = empty_theme()
            ok = .false.
        end select
    end subroutine load_builtin

    subroutine build_steel(theme)
        type(theme_t), intent(out) :: theme
        theme = empty_theme()
        theme%name = 'Facsimile Steel'
        theme%id = 'steel'
        call establish_roles(theme, '#c8d0da', '#1d232c', '#76808d', '#8fa9c4', &
            '#1d232c', '#8fa9c4', '#d7dee8', '#29313d', '#344150', '#11161d', &
            '#f0788f', '#e6b86a', '#66b6c9', '#85c79a')
        call set_rgb(theme, THEME_SYNTAX_KEYWORD, '#e78fb3', '', bold=.true.)
        call set_rgb(theme, THEME_SYNTAX_STRING, '#a8c77d', '')
        call set_rgb(theme, THEME_SYNTAX_NUMBER, '#d9aa72', '')
        call set_rgb(theme, THEME_SYNTAX_TYPE, '#73c6c8', '')
        call set_rgb(theme, THEME_SYNTAX_FUNCTION, '#91b9e4', '')
        call set_rgb(theme, THEME_SYNTAX_PREPROCESSOR, '#d2a8e3', '')
    end subroutine build_steel

    subroutine build_graphite(theme)
        type(theme_t), intent(out) :: theme
        theme = empty_theme()
        theme%name = 'Facsimile Graphite'
        theme%id = 'graphite'
        call establish_roles(theme, '#d4d4d2', '#202120', '#858985', '#76b7b2', &
            '#171817', '#8ebcb8', '#e4e4df', '#2b2d2b', '#3a3d3a', '#121312', &
            '#e06c75', '#d9ad66', '#66a7c5', '#8abd7d')
        call set_rgb(theme, THEME_SYNTAX_KEYWORD, '#d58dae', '', bold=.true.)
        call set_rgb(theme, THEME_SYNTAX_STRING, '#a8bd78', '')
        call set_rgb(theme, THEME_SYNTAX_NUMBER, '#d6a56f', '')
        call set_rgb(theme, THEME_SYNTAX_TYPE, '#70b8b1', '')
        call set_rgb(theme, THEME_SYNTAX_FUNCTION, '#76a8c8', '')
        call set_rgb(theme, THEME_SYNTAX_PREPROCESSOR, '#c39bd3', '')
    end subroutine build_graphite

    subroutine build_paper(theme)
        type(theme_t), intent(out) :: theme
        theme = empty_theme()
        theme%name = 'Facsimile Paper'
        theme%id = 'paper'
        theme%light = .true.
        call establish_roles(theme, '#282c34', '#f5f6f4', '#70757d', '#537da6', &
            '#f7f8f6', '#8da9c4', '#20242a', '#e6e9eb', '#d5dde5', '#c9ced3', &
            '#c4425d', '#9a681c', '#257a94', '#367c4c')
        call set_rgb(theme, THEME_SYNTAX_KEYWORD, '#a33b70', '', bold=.true.)
        call set_rgb(theme, THEME_SYNTAX_STRING, '#4f772d', '')
        call set_rgb(theme, THEME_SYNTAX_NUMBER, '#9a5b13', '')
        call set_rgb(theme, THEME_SYNTAX_TYPE, '#176b73', '')
        call set_rgb(theme, THEME_SYNTAX_FUNCTION, '#315f91', '')
        call set_rgb(theme, THEME_SYNTAX_PREPROCESSOR, '#76518c', '')
    end subroutine build_paper

    subroutine build_legacy(theme)
        type(theme_t), intent(out) :: theme
        integer :: i
        theme = empty_theme()
        theme%name = 'Legacy'
        theme%id = 'legacy'
        do i = 1, THEME_ROLE_COUNT
            theme%styles(i) = blank_style()
        end do
        theme%styles(THEME_MUTED)%fg = 8
        theme%styles(THEME_ACCENT)%fg = 4
        theme%styles(THEME_TAB_ACTIVE)%inverse = .true.
        theme%styles(THEME_SELECTION)%inverse = .true.
        theme%styles(THEME_STATUS)%inverse = .true.
        theme%styles(THEME_PANEL)%bg = 235
        theme%styles(THEME_PANEL_HEADER)%bg = 237
        theme%styles(THEME_PANEL_HEADER)%bold = .true.
        theme%styles(THEME_PANEL_SELECTION)%bg = 240
        theme%styles(THEME_DIRECTORY)%fg = 4
        theme%styles(THEME_EXECUTABLE)%fg = 2
        theme%styles(THEME_ERROR)%fg = 1
        theme%styles(THEME_WARNING)%fg = 3
        theme%styles(THEME_INFO)%fg = 6
        theme%styles(THEME_SUCCESS)%fg = 2
        theme%styles(THEME_DISABLED)%fg = 8
        theme%styles(THEME_DISABLED)%dim = .true.
        theme%styles(THEME_DISABLED)%italic = .true.
    end subroutine build_legacy

    subroutine establish_roles(theme, fg, bg, muted, accent, accent_fg, selection_bg, &
                               panel_fg, panel_bg, panel_alt, shadow, error, warning, info, success)
        type(theme_t), intent(inout) :: theme
        character(len=*), intent(in) :: fg, bg, muted, accent, accent_fg, selection_bg
        character(len=*), intent(in) :: panel_fg, panel_bg, panel_alt, shadow
        character(len=*), intent(in) :: error, warning, info, success
        integer :: i

        do i = 1, THEME_ROLE_COUNT
            call set_rgb(theme, i, fg, bg)
        end do
        call set_rgb(theme, THEME_EDITOR, fg, bg)
        call set_rgb(theme, THEME_EDITOR_BG, '', bg)
        call set_rgb(theme, THEME_MUTED, muted, bg, dim=.true.)
        call set_rgb(theme, THEME_ACCENT, accent, bg, bold=.true.)
        ! Keep reverse-video as the semantic selection flag used by terminal
        ! emulators and accessibility tooling. The stored colors are swapped
        ! so the rendered result remains accent text on the selection fill.
        call set_rgb(theme, THEME_SELECTION, selection_bg, accent_fg, inverse=.true.)
        call set_rgb(theme, THEME_SELECTION_INACTIVE, panel_fg, panel_alt)
        call set_rgb(theme, THEME_TAB_BAR, muted, bg)
        call set_rgb(theme, THEME_TAB_ACTIVE, selection_bg, accent_fg, inverse=.true.)
        call set_rgb(theme, THEME_TAB_INACTIVE, muted, bg)
        call set_rgb(theme, THEME_TAB_MODIFIED, warning, bg, bold=.true.)
        call set_rgb(theme, THEME_TAB_ORPHAN, muted, bg, dim=.true.)
        call set_rgb(theme, THEME_TAB_HOVER, panel_fg, panel_alt)
        call set_rgb(theme, THEME_TAB_DRAG, accent_fg, success, bold=.true.)
        call set_rgb(theme, THEME_STATUS, panel_fg, panel_bg)
        call set_rgb(theme, THEME_STATUS_ACCENT, accent_fg, selection_bg, bold=.true.)
        call set_rgb(theme, THEME_PANEL, panel_fg, panel_bg)
        call set_rgb(theme, THEME_PANEL_HEADER, fg, panel_alt, bold=.true.)
        call set_rgb(theme, THEME_PANEL_FOOTER, muted, panel_alt)
        call set_rgb(theme, THEME_PANEL_SELECTION, panel_alt, panel_fg, inverse=.true.)
        call set_rgb(theme, THEME_BORDER, muted, panel_bg)
        call set_rgb(theme, THEME_BORDER_FOCUS, accent, panel_bg)
        call set_rgb(theme, THEME_SHADOW, shadow, shadow)
        call set_rgb(theme, THEME_ERROR, error, bg, bold=.true.)
        call set_rgb(theme, THEME_WARNING, warning, bg)
        call set_rgb(theme, THEME_INFO, info, bg)
        call set_rgb(theme, THEME_HINT, muted, bg, italic=.true.)
        call set_rgb(theme, THEME_SUCCESS, success, bg)
        call set_rgb(theme, THEME_GIT_ADDED, success, panel_bg)
        call set_rgb(theme, THEME_GIT_MODIFIED, warning, panel_bg)
        call set_rgb(theme, THEME_GIT_DELETED, error, panel_bg)
        call set_rgb(theme, THEME_DIRECTORY, accent, panel_bg)
        call set_rgb(theme, THEME_EXECUTABLE, success, panel_bg)
        call set_rgb(theme, THEME_GHOST, muted, bg, italic=.true.)
        call set_rgb(theme, THEME_LINE_NUMBER, muted, bg, dim=.true.)
        call set_rgb(theme, THEME_LINE_NUMBER_ACTIVE, accent, bg, bold=.true.)
        call set_rgb(theme, THEME_SYNTAX_COMMENT, muted, bg, italic=.true.)
        call set_rgb(theme, THEME_SEARCH_MATCH, accent_fg, warning, bold=.true.)
        call set_rgb(theme, THEME_CURRENT_LINE, fg, panel_bg)
        call set_rgb(theme, THEME_DISABLED, muted, panel_bg, dim=.true., italic=.true.)
    end subroutine establish_roles

    subroutine load_custom_theme(name, theme, ok, message)
        character(len=*), intent(in) :: name
        type(theme_t), intent(out) :: theme
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: message
        type(toml_document) :: document
        type(toml_error) :: error
        type(toml_table) :: palette
        character(len=:), allocatable :: config_dir
        character(len=:), allocatable :: path
        character(len=:), allocatable :: base
        character(len=:), allocatable :: value
        integer :: role

        ok = .false.
        if (.not. safe_theme_name(name)) then
            message = 'Invalid theme name'
            return
        end if
        call get_config_dir(config_dir)
        path = trim(config_dir) // '/themes/' // trim(name) // '.toml'
        call parse_file(path, document, error)
        if (error%failed) then
            message = trim(path) // ':' // int_text(error%line) // ':' // &
                int_text(error%column) // ': ' // error%message
            return
        end if

        base = document%get_string('metadata.extends', 'steel')
        call load_builtin(base, theme, ok)
        if (.not. ok) then
            message = 'Custom themes may extend steel, graphite, paper, or legacy'
            return
        end if
        theme%id = trim(name)
        theme%name = document%get_string('metadata.name', trim(name))
        palette = document%get_table('palette')
        do role = 1, THEME_ROLE_COUNT
            ! Attributes come first because inverse video swaps the terminal's
            ! stored foreground/background. apply_color preserves the schema's
            ! user-facing meaning: fg and bg always describe visible colors.
            if (document%has_key('styles.' // trim(ROLE_NAMES(role)) // '.attrs')) then
                call apply_attrs(document%get_array('styles.' // trim(ROLE_NAMES(role)) // '.attrs'), &
                    theme%styles(role), ok)
                if (.not. ok) then
                    message = 'Invalid attribute for styles.' // trim(ROLE_NAMES(role))
                    return
                end if
            end if
            if (document%has_key('styles.' // trim(ROLE_NAMES(role)) // '.fg')) then
                value = document%get_string('styles.' // trim(ROLE_NAMES(role)) // '.fg', '')
                call apply_color(value, palette, theme%styles(role), .false., ok)
                if (.not. ok) then
                    message = 'Invalid foreground for styles.' // trim(ROLE_NAMES(role))
                    return
                end if
            end if
            if (document%has_key('styles.' // trim(ROLE_NAMES(role)) // '.bg')) then
                value = document%get_string('styles.' // trim(ROLE_NAMES(role)) // '.bg', '')
                call apply_color(value, palette, theme%styles(role), .true., ok)
                if (.not. ok) then
                    message = 'Invalid background for styles.' // trim(ROLE_NAMES(role))
                    return
                end if
            end if
        end do
        ok = .true.
        message = ''
    end subroutine load_custom_theme

    subroutine apply_color(value, palette, style, background, ok)
        character(len=*), intent(in) :: value
        type(toml_table), intent(in) :: palette
        type(screen_style), intent(inout) :: style
        logical, intent(in) :: background
        logical, intent(out) :: ok
        character(len=:), allocatable :: resolved
        integer :: i
        integer :: rgb(3)
        logical :: stored_as_background

        stored_as_background = background .neqv. style%inverse

        resolved = trim(value)
        if (len(resolved) > 0 .and. resolved(1:1) /= '#') then
            do i = 1, palette%length()
                if (palette%keys(i)%text == resolved .and. &
                    palette%values(i)%kind == TOML_KIND_STRING) then
                    resolved = palette%values(i)%string_value%text
                    exit
                end if
            end do
        end if
        if (resolved == 'default' .or. len(resolved) == 0) then
            if (stored_as_background) then
                style%bg = -1
                style%bg_truecolor = .false.
            else
                style%fg = -1
                style%fg_truecolor = .false.
            end if
            ok = .true.
            return
        end if
        call parse_hex(resolved, rgb, ok)
        if (.not. ok) return
        if (stored_as_background) then
            style%bg_truecolor = .true.
            style%bg_rgb = rgb
        else
            style%fg_truecolor = .true.
            style%fg_rgb = rgb
        end if
    end subroutine apply_color

    subroutine apply_attrs(attrs, style, ok)
        type(toml_array), intent(in) :: attrs
        type(screen_style), intent(inout) :: style
        logical, intent(out) :: ok
        character(len=:), allocatable :: attr
        integer :: i
        logical :: was_inverse

        was_inverse = style%inverse
        style%bold = .false.
        style%dim = .false.
        style%italic = .false.
        style%underline = .false.
        style%inverse = .false.
        style%strikethrough = .false.
        ok = .true.
        do i = 1, attrs%length()
            if (attrs%values(i)%kind /= TOML_KIND_STRING) then
                ok = .false.
                return
            end if
            attr = attrs%values(i)%string_value%text
            select case (attr)
            case ('bold'); style%bold = .true.
            case ('dim'); style%dim = .true.
            case ('italic'); style%italic = .true.
            case ('underline'); style%underline = .true.
            case ('inverse'); style%inverse = .true.
            case ('strikethrough'); style%strikethrough = .true.
            case default
                ok = .false.
                return
            end select
        end do
        if (style%inverse .neqv. was_inverse) call swap_style_colors(style)
    end subroutine apply_attrs

    subroutine swap_style_colors(style)
        type(screen_style), intent(inout) :: style
        integer :: index
        integer :: rgb(3)
        logical :: truecolor

        index = style%fg
        style%fg = style%bg
        style%bg = index
        rgb = style%fg_rgb
        style%fg_rgb = style%bg_rgb
        style%bg_rgb = rgb
        truecolor = style%fg_truecolor
        style%fg_truecolor = style%bg_truecolor
        style%bg_truecolor = truecolor
    end subroutine swap_style_colors

    function empty_theme() result(theme)
        type(theme_t) :: theme
        integer :: i
        theme%name = 'Facsimile Steel'
        theme%id = 'steel'
        theme%light = .false.
        do i = 1, THEME_ROLE_COUNT
            theme%styles(i) = blank_style()
        end do
    end function empty_theme

    function blank_style() result(style)
        type(screen_style) :: style
        style%fg = -1
        style%bg = -1
        style%fg_truecolor = .false.
        style%bg_truecolor = .false.
        style%fg_rgb = 0
        style%bg_rgb = 0
        style%bold = .false.
        style%dim = .false.
        style%italic = .false.
        style%underline = .false.
        style%inverse = .false.
        style%strikethrough = .false.
    end function blank_style

    subroutine set_rgb(theme, role, fg, bg, bold, dim, italic, underline, inverse)
        type(theme_t), intent(inout) :: theme
        integer, intent(in) :: role
        character(len=*), intent(in) :: fg, bg
        logical, intent(in), optional :: bold, dim, italic, underline, inverse
        logical :: ok
        integer :: rgb(3)

        theme%styles(role) = blank_style()
        if (len_trim(fg) > 0) then
            call parse_hex(fg, rgb, ok)
            if (ok) then
                theme%styles(role)%fg_truecolor = .true.
                theme%styles(role)%fg_rgb = rgb
            end if
        end if
        if (len_trim(bg) > 0) then
            call parse_hex(bg, rgb, ok)
            if (ok) then
                theme%styles(role)%bg_truecolor = .true.
                theme%styles(role)%bg_rgb = rgb
            end if
        end if
        if (present(bold)) theme%styles(role)%bold = bold
        if (present(dim)) theme%styles(role)%dim = dim
        if (present(italic)) theme%styles(role)%italic = italic
        if (present(underline)) theme%styles(role)%underline = underline
        if (present(inverse)) theme%styles(role)%inverse = inverse
    end subroutine set_rgb

    subroutine downgrade_style(style)
        type(screen_style), intent(inout) :: style
        if (current_color_mode == SCREEN_COLOR_MONO) then
            style%fg = -1
            style%bg = -1
            style%fg_truecolor = .false.
            style%bg_truecolor = .false.
        else if (current_color_mode < SCREEN_COLOR_TRUECOLOR) then
            if (style%fg_truecolor) then
                style%fg = rgb_to_xterm(style%fg_rgb)
                style%fg_truecolor = .false.
            end if
            if (style%bg_truecolor) then
                style%bg = rgb_to_xterm(style%bg_rgb)
                style%bg_truecolor = .false.
            end if
            if (current_color_mode == SCREEN_COLOR_BASIC) then
                if (style%fg >= 0) style%fg = basic_index(style%fg)
                if (style%bg >= 0) style%bg = basic_index(style%bg)
            end if
        end if
    end subroutine downgrade_style

    function rgb_sgr(rgb, background) result(sequence)
        integer, intent(in) :: rgb(3)
        logical, intent(in) :: background
        character(len=:), allocatable :: sequence
        sequence = achar(27) // '[' // merge('48', '38', background) // ';2;' // &
            int_text(rgb(1)) // ';' // int_text(rgb(2)) // ';' // int_text(rgb(3)) // 'm'
    end function rgb_sgr

    function indexed_sgr(index, background) result(sequence)
        integer, intent(in) :: index
        logical, intent(in) :: background
        character(len=:), allocatable :: sequence
        integer :: value

        value = max(0, min(255, index))
        if (current_color_mode >= SCREEN_COLOR_256) then
            sequence = achar(27) // '[' // merge('48', '38', background) // ';5;' // &
                int_text(value) // 'm'
        else if (background) then
            if (value < 8) then
                sequence = achar(27) // '[' // int_text(40 + value) // 'm'
            else
                sequence = achar(27) // '[' // int_text(100 + value - 8) // 'm'
            end if
        else
            if (value < 8) then
                sequence = achar(27) // '[' // int_text(30 + value) // 'm'
            else
                sequence = achar(27) // '[' // int_text(90 + value - 8) // 'm'
            end if
        end if
    end function indexed_sgr

    subroutine parse_hex(text, rgb, ok)
        character(len=*), intent(in) :: text
        integer, intent(out) :: rgb(3)
        logical, intent(out) :: ok
        integer :: i
        integer :: hi
        integer :: lo

        rgb = 0
        ok = len_trim(text) == 7
        if (.not. ok) return
        if (text(1:1) /= '#') then
            ok = .false.
            return
        end if
        do i = 1, 3
            hi = hex_value(text(2 * i:2 * i))
            lo = hex_value(text(2 * i + 1:2 * i + 1))
            if (hi < 0 .or. lo < 0) then
                ok = .false.
                return
            end if
            rgb(i) = 16 * hi + lo
        end do
    end subroutine parse_hex

    integer function hex_value(ch) result(value)
        character(len=1), intent(in) :: ch
        select case (ch)
        case ('0':'9'); value = iachar(ch) - iachar('0')
        case ('a':'f'); value = iachar(ch) - iachar('a') + 10
        case ('A':'F'); value = iachar(ch) - iachar('A') + 10
        case default; value = -1
        end select
    end function hex_value

    integer function rgb_to_xterm(rgb) result(index)
        integer, intent(in) :: rgb(3)
        index = 16 + 36 * nint(real(rgb(1)) / 255.0 * 5.0) + &
            6 * nint(real(rgb(2)) / 255.0 * 5.0) + nint(real(rgb(3)) / 255.0 * 5.0)
    end function rgb_to_xterm

    integer function basic_index(index) result(value)
        integer, intent(in) :: index
        if (index < 16) then
            value = index
        else
            value = modulo(index, 8)
            if (index >= 244) value = value + 8
        end if
    end function basic_index

    logical function safe_theme_name(name) result(safe)
        character(len=*), intent(in) :: name
        integer :: i
        safe = len_trim(name) > 0 .and. len_trim(name) <= 32
        if (.not. safe) return
        do i = 1, len_trim(name)
            if (.not. ((name(i:i) >= 'a' .and. name(i:i) <= 'z') .or. &
                      (name(i:i) >= 'A' .and. name(i:i) <= 'Z') .or. &
                      (name(i:i) >= '0' .and. name(i:i) <= '9') .or. &
                      name(i:i) == '-' .or. name(i:i) == '_')) then
                safe = .false.
                return
            end if
        end do
    end function safe_theme_name

    function lowercase(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lower
        integer :: i
        lower = text
        do i = 1, len(lower)
            if (lower(i:i) >= 'A' .and. lower(i:i) <= 'Z') &
                lower(i:i) = achar(iachar(lower(i:i)) + 32)
        end do
    end function lowercase

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer
        write(buffer, '(i0)') value
        text = trim(buffer)
    end function int_text

end module theme_module
