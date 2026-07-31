module group_picker_module
    ! The tab-group dialog: name it, tick the files, confirm.
    !
    ! Two modes, one dialog. Creating a group and editing one are the same
    ! act -- choose files, name the result -- so edit mode is this same list
    ! opened with the group's current members already ticked. The mode reaches
    ! no further than the title and the footer hint.
    !
    ! Shaped after context_menu_module and for the same reasons: module-level
    ! singleton state (two of these cannot be open), geometry fixed when shown,
    ! and a separate render called every frame rather than drawing once inside
    ! show(). It deliberately does not `use editor_state_module` or
    ! `renderer_module` -- the caller decides what to do with the result, which
    ! keeps this module low in the dependency graph and testable with no editor.
    !
    ! The one design decision worth stating: **the ticked set is a set of
    ! paths, not a flag on each listed item**. Walking to ../ re-lists a
    ! different directory, so per-item flags would silently drop every tick the
    ! moment you moved -- which would make the ../ row pointless, since its
    ! whole purpose is assembling a group that spans directories. The footer
    ! shows the count so ticks that scrolled out of sight are still accounted
    ! for.
    use terminal_io_module, only: terminal_move_cursor, terminal_write, &
                                  terminal_hide_cursor
    use clickable_region_module, only: region_add, REGION_BLOCK, REGION_GP_ROW, &
                                       REGION_GP_NAME
    use utf8_module, only: clip_to_cells
    use dir_scan_module, only: dir_entry_t, list_directory
    implicit none
    private

    public :: group_picker_show, group_picker_show_edit
    public :: group_picker_preselect, group_picker_mark_dirty
    public :: group_picker_hide, is_group_picker_visible
    public :: group_picker_handle_key, group_picker_click
    public :: render_group_picker, group_picker_row_at
    public :: group_picker_result, group_picker_name
    public :: group_picker_count, group_picker_path, group_picker_dir
    public :: group_picker_item_count, group_picker_selected
    public :: group_picker_item_is_dir
    public :: GP_PENDING, GP_CANCELLED, GP_CONFIRMED

    integer, parameter :: GP_PENDING   = 0
    integer, parameter :: GP_CANCELLED = 1
    integer, parameter :: GP_CONFIRMED = 2

    integer, parameter :: GP_MAX_ITEMS  = 512
    integer, parameter :: GP_MAX_PICKED = 256
    integer, parameter :: GP_VISIBLE    = 12
    integer, parameter :: GP_WIDTH      = 62
    integer, parameter :: GP_NAME_MAX   = 48
    integer, parameter :: GP_FOCUS_NAME = 1
    integer, parameter :: GP_FOCUS_LIST = 2

    ! Two modes, one dialog. Editing a group is the same act as creating one --
    ! choose files, name the result -- differing only in what is already ticked
    ! and in what the caller does with the answer. A second dialog would be the
    ! same 700 lines with a different title.
    !
    ! The mode reaches no further than the title and the footer hint. Nothing
    ! about groups is known here: this is a file picker with a name field, and
    ! the meaning of the result lives in command_handler_module.
    integer, parameter :: GP_MODE_NEW  = 1
    integer, parameter :: GP_MODE_EDIT = 2

    character(len=*), parameter :: ESC   = char(27)
    character(len=*), parameter :: RESET = ESC // '[0m'

    ! A filled 256-colour panel, the same family references_panel and
    ! symbols_panel use. The picker predated that style and drew ASCII on
    ! whatever background the document happened to have, so it read as part of
    ! the text rather than as something floating over it.
    character(len=*), parameter :: BODY_BG   = ESC // '[48;5;235m'
    character(len=*), parameter :: CHROME_BG = ESC // '[48;5;237m'
    character(len=*), parameter :: SEL_BG    = ESC // '[48;5;240m'
    character(len=*), parameter :: SHADOW_BG = ESC // '[48;5;233m'
    character(len=*), parameter :: BORDER_FG = ESC // '[38;5;67m'
    character(len=*), parameter :: TITLE_FG  = ESC // '[1;38;5;81m'
    character(len=*), parameter :: DIR_FG    = ESC // '[38;5;75m'
    character(len=*), parameter :: TICK_FG   = ESC // '[38;5;114m'
    character(len=*), parameter :: HINT_FG   = ESC // '[38;5;245m'
    character(len=*), parameter :: TEXT_FG   = ESC // '[38;5;252m'
    character(len=*), parameter :: SEL_FG    = ESC // '[1;38;5;231m'

    ! Frame chrome resets first: the title and the selected row set bold, and a
    ! bare colour change does not clear intensity, so the bold would otherwise
    ! bleed into whichever border character came next.
    character(len=*), parameter :: FRAME = RESET // BODY_BG // BORDER_FG

    type :: gp_item_t
        character(len=256) :: name = ''
        logical :: is_dir = .false.
        logical :: is_up  = .false.        ! the ../ row
    end type gp_item_t

    type(gp_item_t) :: g_items(GP_MAX_ITEMS)
    integer :: g_n_items = 0
    logical :: g_clamped = .false.         ! the directory had more than we list

    character(len=512) :: g_dir = ''
    character(len=512) :: g_picked(GP_MAX_PICKED)
    integer :: g_n_picked = 0

    ! Ticked members with unsaved changes. Held separately from g_picked
    ! because unticking one CLOSES its tab, so the dialog has to say which
    ! rows have work in them before Enter rather than after.
    character(len=512) :: g_dirty(GP_MAX_PICKED)
    integer :: g_n_dirty = 0

    integer :: g_mode = GP_MODE_NEW

    character(len=GP_NAME_MAX) :: g_name = ''
    integer :: g_name_len = 0

    integer :: g_focus = GP_FOCUS_NAME
    integer :: g_sel = 1, g_scroll = 0
    integer :: g_row0 = 0, g_col0 = 0, g_width = 0, g_height = 0
    ! Kept so the drop shadow can be clipped to the screen rather than
    ! wrapping, which would smear it across the row below.
    integer :: g_screen_rows = 0, g_screen_cols = 0
    logical :: g_visible = .false.
    integer :: g_result = GP_PENDING
    character(len=64) :: g_note = ''

contains

    ! ---- lifecycle -------------------------------------------------------

    !> Open the dialog listing `dir`, with the name pre-filled from it.
    function group_picker_show(dir, screen_rows, screen_cols) result(shown)
        character(len=*), intent(in) :: dir
        integer, intent(in) :: screen_rows, screen_cols
        logical :: shown

        g_mode = GP_MODE_NEW
        shown = open_at(dir, screen_rows, screen_cols)
        if (.not. shown) return

        ! Pre-fill the name so the happy path is Enter-Enter.
        g_name = basename(trim(g_dir)) // '/'
        g_name_len = len_trim(g_name)
    end function group_picker_show

    !> Open the dialog on an EXISTING group: same list, same navigation, but
    !> named for the group rather than for the directory, and expecting the
    !> caller to tick the current members with group_picker_preselect.
    !>
    !> Ticking is left to the caller because a member may live outside `dir`
    !> entirely -- a group can span directories -- so it cannot be expressed as
    !> an index into what is currently listed.
    function group_picker_show_edit(dir, name, screen_rows, screen_cols) &
            result(shown)
        character(len=*), intent(in) :: dir, name
        integer, intent(in) :: screen_rows, screen_cols
        logical :: shown

        g_mode = GP_MODE_EDIT
        shown = open_at(dir, screen_rows, screen_cols)
        if (.not. shown) return

        g_name = name
        g_name_len = min(len_trim(name), GP_NAME_MAX)
    end function group_picker_show_edit

    !> Tick `path` outright, without it having to be a row in the current list.
    subroutine group_picker_preselect(path)
        character(len=*), intent(in) :: path

        if (len_trim(path) == 0) return
        if (picked_index(path) > 0) return
        if (g_n_picked >= GP_MAX_PICKED) return
        g_n_picked = g_n_picked + 1
        g_picked(g_n_picked) = path
    end subroutine group_picker_preselect

    !> Mark `path` as having unsaved changes, so its row can say so.
    subroutine group_picker_mark_dirty(path)
        character(len=*), intent(in) :: path

        if (len_trim(path) == 0) return
        if (g_n_dirty >= GP_MAX_PICKED) return
        g_n_dirty = g_n_dirty + 1
        g_dirty(g_n_dirty) = path
    end subroutine group_picker_mark_dirty

    logical function is_dirty(path)
        character(len=*), intent(in) :: path
        integer :: k

        is_dirty = .false.
        do k = 1, g_n_dirty
            if (trim(g_dirty(k)) == trim(path)) then
                is_dirty = .true.
                return
            end if
        end do
    end function is_dirty

    !> Everything both modes do: list the directory and place the box.
    function open_at(dir, screen_rows, screen_cols) result(shown)
        character(len=*), intent(in) :: dir
        integer, intent(in) :: screen_rows, screen_cols
        logical :: shown

        shown = .false.
        g_n_picked = 0
        g_n_dirty = 0
        g_result = GP_PENDING
        g_note = ''
        g_focus = GP_FOCUS_NAME

        call walk_to(dir)
        if (g_n_items == 0) return

        g_screen_rows = screen_rows
        g_screen_cols = screen_cols
        g_width = min(GP_WIDTH, max(24, screen_cols - 4))
        g_height = GP_VISIBLE + 7
        ! Biased a little left and up of dead centre, so the shadow has room
        ! to fall without being clipped on a snugly-sized terminal.
        g_col0 = max(1, (screen_cols - g_width) / 2 - 1)
        g_row0 = max(1, (screen_rows - g_height) / 2)
        if (g_row0 + g_height - 1 > screen_rows) g_row0 = max(1, screen_rows - g_height + 1)

        g_visible = .true.
        shown = .true.
    end function open_at

    subroutine group_picker_hide()
        g_visible = .false.
    end subroutine group_picker_hide

    logical function is_group_picker_visible()
        is_group_picker_visible = g_visible
    end function is_group_picker_visible

    integer function group_picker_result()
        group_picker_result = g_result
    end function group_picker_result

    function group_picker_name() result(text)
        character(len=:), allocatable :: text
        text = trim(g_name(1:max(0, g_name_len)))
    end function group_picker_name

    function group_picker_dir() result(text)
        character(len=:), allocatable :: text
        text = trim(g_dir)
    end function group_picker_dir

    integer function group_picker_count()
        group_picker_count = g_n_picked
    end function group_picker_count

    function group_picker_path(i) result(text)
        integer, intent(in) :: i
        character(len=:), allocatable :: text

        text = ''
        if (i >= 1 .and. i <= g_n_picked) text = trim(g_picked(i))
    end function group_picker_path

    ! Small accessors so the dialog can be driven and checked without a
    ! terminal: a test otherwise has to guess which rows are directories, and
    ! pressing space on one of those walks into it.
    integer function group_picker_item_count()
        group_picker_item_count = g_n_items
    end function group_picker_item_count

    integer function group_picker_selected()
        group_picker_selected = g_sel
    end function group_picker_selected

    logical function group_picker_item_is_dir(i)
        integer, intent(in) :: i

        group_picker_item_is_dir = .false.
        if (i >= 1 .and. i <= g_n_items) group_picker_item_is_dir = g_items(i)%is_dir
    end function group_picker_item_is_dir

    ! ---- directory listing ------------------------------------------------

    !> List `dir`, directories first then files, each alphabetically, with a
    !> ../ row on top. The ticked set is untouched: that is what lets a group
    !> span directories.
    subroutine walk_to(dir)
        character(len=*), intent(in) :: dir
        type(dir_entry_t), allocatable :: raw(:)
        integer :: n, i, j
        logical :: ok
        type(gp_item_t) :: swap

        g_dir = dir
        g_n_items = 0
        g_clamped = .false.
        g_sel = 1
        g_scroll = 0

        call list_directory(trim(dir), raw, n, ok)
        if (.not. ok) return

        if (trim(dir) /= '/') then
            g_n_items = 1
            g_items(1)%name = '..'
            g_items(1)%is_dir = .true.
            g_items(1)%is_up = .true.
        end if

        do i = 1, n
            if (g_n_items >= GP_MAX_ITEMS) then
                g_clamped = .true.
                exit
            end if
            g_n_items = g_n_items + 1
            g_items(g_n_items)%name = raw(i)%name
            g_items(g_n_items)%is_dir = raw(i)%is_dir
            g_items(g_n_items)%is_up = .false.
        end do

        ! Simple insertion sort over everything below the ../ row: directories
        ! before files, then by name. list_directory does not sort.
        do i = up_offset() + 2, g_n_items
            swap = g_items(i)
            j = i - 1
            do while (j >= up_offset() + 1)
                if (.not. before(g_items(j), swap)) then
                    g_items(j + 1) = g_items(j)
                    j = j - 1
                else
                    exit
                end if
            end do
            g_items(j + 1) = swap
        end do
    end subroutine walk_to

    integer function up_offset()
        up_offset = 0
        if (g_n_items >= 1) then
            if (g_items(1)%is_up) up_offset = 1
        end if
    end function up_offset

    logical function before(a, b)
        type(gp_item_t), intent(in) :: a, b

        if (a%is_dir .neqv. b%is_dir) then
            before = a%is_dir
        else
            before = lle(to_lower(trim(a%name)), to_lower(trim(b%name)))
        end if
    end function before

    ! ---- the picked set ---------------------------------------------------

    function full_path(i) result(p)
        integer, intent(in) :: i
        character(len=:), allocatable :: p

        p = ''
        if (i < 1 .or. i > g_n_items) return
        p = join(trim(g_dir), trim(g_items(i)%name))
    end function full_path

    integer function picked_index(path)
        character(len=*), intent(in) :: path
        integer :: k

        picked_index = 0
        do k = 1, g_n_picked
            if (trim(g_picked(k)) == trim(path)) then
                picked_index = k
                return
            end if
        end do
    end function picked_index

    subroutine toggle_pick(i)
        integer, intent(in) :: i
        character(len=:), allocatable :: p
        integer :: k, at

        if (i < 1 .or. i > g_n_items) return
        if (g_items(i)%is_dir) return        ! directories are walked, not ticked

        p = full_path(i)
        at = picked_index(p)
        if (at > 0) then
            do k = at, g_n_picked - 1
                g_picked(k) = g_picked(k + 1)
            end do
            g_n_picked = g_n_picked - 1
        else
            if (g_n_picked >= GP_MAX_PICKED) then
                g_note = 'selection is full'
                return
            end if
            g_n_picked = g_n_picked + 1
            g_picked(g_n_picked) = p
        end if
    end subroutine toggle_pick

    ! ---- input ------------------------------------------------------------

    function group_picker_handle_key(key) result(handled)
        character(len=*), intent(in) :: key
        logical :: handled
        logical :: is_space

        handled = .false.
        if (.not. g_visible) return
        handled = .true.                     ! a modal swallows by default

        ! trim() turns a space into '', so select case cannot see it. This is
        ! the same trap that made the command palette unable to type a space.
        is_space = .false.
        if (len(key) >= 1) is_space = (len_trim(key) == 0 .and. key(1:1) == ' ')

        if (.not. is_space) then
            select case (trim(key))
            case ('esc')
                g_result = GP_CANCELLED
                g_visible = .false.
                return
            case ('tab')
                g_focus = 3 - g_focus
                return
            end select
        end if

        if (g_focus == GP_FOCUS_NAME) then
            call name_key(key, is_space)
        else
            call list_key(key, is_space)
        end if
    end function group_picker_handle_key

    subroutine name_key(key, is_space)
        character(len=*), intent(in) :: key
        logical, intent(in) :: is_space
        integer :: c

        if (is_space) then
            call name_insert(' ')
            return
        end if
        select case (trim(key))
        case ('down')
            g_focus = GP_FOCUS_LIST
        case ('backspace')
            if (g_name_len > 0) g_name_len = g_name_len - 1
        case ('enter')
            call try_confirm()
        case default
            if (len_trim(key) == 1) then
                c = iachar(key(1:1))
                if (c >= 32 .and. c < 127) call name_insert(key(1:1))
            end if
        end select
    end subroutine name_key

    subroutine name_insert(ch)
        character(len=1), intent(in) :: ch

        if (g_name_len >= GP_NAME_MAX) return
        g_name_len = g_name_len + 1
        g_name(g_name_len:g_name_len) = ch
    end subroutine name_insert

    subroutine list_key(key, is_space)
        character(len=*), intent(in) :: key
        logical, intent(in) :: is_space

        if (is_space) then
            call activate(.true.)
            return
        end if
        select case (trim(key))
        case ('up')
            if (g_sel <= 1) then
                g_focus = GP_FOCUS_NAME      ! off the top returns to the name
            else
                g_sel = g_sel - 1
                call fix_scroll()
            end if
        case ('down')
            g_sel = min(g_n_items, g_sel + 1)
            call fix_scroll()
        case ('pageup')
            g_sel = max(1, g_sel - GP_VISIBLE)
            call fix_scroll()
        case ('pagedown')
            g_sel = min(g_n_items, g_sel + GP_VISIBLE)
            call fix_scroll()
        case ('home')
            g_sel = 1
            call fix_scroll()
        case ('end')
            g_sel = g_n_items
            call fix_scroll()
        case ('left')
            call walk_to(parent_of(trim(g_dir)))
        case ('right', 'enter')
            call activate(.false.)
        end select
    end subroutine list_key

    !> Space ticks; Enter walks into a directory or confirms on a file.
    subroutine activate(toggle_only)
        logical, intent(in) :: toggle_only

        if (g_sel < 1 .or. g_sel > g_n_items) return

        if (g_items(g_sel)%is_up) then
            call walk_to(parent_of(trim(g_dir)))
            return
        end if
        if (g_items(g_sel)%is_dir) then
            call walk_to(join(trim(g_dir), trim(g_items(g_sel)%name)))
            return
        end if

        if (toggle_only) then
            call toggle_pick(g_sel)
            ! Advance, so a run of files can be ticked quickly.
            if (g_sel < g_n_items) then
                g_sel = g_sel + 1
                call fix_scroll()
            end if
        else
            call try_confirm()
        end if
    end subroutine activate

    subroutine try_confirm()
        if (g_n_picked == 0) then
            g_note = 'tick at least one file (space)'
            return
        end if
        if (g_name_len == 0) then
            g_note = 'the group needs a name'
            g_focus = GP_FOCUS_NAME
            return
        end if
        g_result = GP_CONFIRMED
        g_visible = .false.
    end subroutine try_confirm

    subroutine fix_scroll()
        if (g_sel < g_scroll + 1) g_scroll = g_sel - 1
        if (g_sel > g_scroll + GP_VISIBLE) g_scroll = g_sel - GP_VISIBLE
        if (g_scroll < 0) g_scroll = 0
    end subroutine fix_scroll

    !> Resolve a click. Returns .true. if it landed on the dialog.
    function group_picker_click(row, col) result(handled)
        integer, intent(in) :: row, col
        logical :: handled
        integer :: idx

        handled = .false.
        if (.not. g_visible) return
        if (row < g_row0 .or. row > g_row0 + g_height - 1) return
        if (col < g_col0 .or. col > g_col0 + g_width - 1) return
        handled = .true.

        if (row == g_row0 + 1) then
            g_focus = GP_FOCUS_NAME
            return
        end if
        idx = group_picker_row_at(row, col)
        if (idx > 0) then
            g_focus = GP_FOCUS_LIST
            g_sel = idx
            call activate(.true.)
        end if
    end function group_picker_click

    !> Which item is drawn at (row, col), or 0. Kept next to the renderer
    !> because it repeats its row arithmetic, and the two must not drift.
    function group_picker_row_at(row, col) result(idx)
        integer, intent(in) :: row, col
        integer :: idx, first_row, off

        idx = 0
        if (.not. g_visible) return
        if (col < g_col0 .or. col > g_col0 + g_width - 1) return

        first_row = g_row0 + 3               ! border, name, separator
        off = row - first_row
        if (off < 0 .or. off > GP_VISIBLE - 1) return
        idx = g_scroll + off + 1
        if (idx > g_n_items) idx = 0
    end function group_picker_row_at

    ! ---- rendering --------------------------------------------------------

    subroutine render_group_picker()
        integer :: r, i, idx
        character(len=:), allocatable :: line
        character(len=64) :: foot

        if (.not. g_visible) return

        ! The box owns its rectangle, so clicks on it never reach the document.
        call region_add(REGION_BLOCK, g_row0, g_row0 + g_height - 1, &
                        g_col0, g_col0 + g_width - 1)

        ! Before the box, so the box overwrites any overlap rather than the
        ! shadow being painted over the frame it is supposed to sit under.
        call put_shadow()

        r = g_row0
        if (g_mode == GP_MODE_EDIT) then
            call put_edge(r, '╭', '╮', 'Edit Tab Group')
        else
            call put_edge(r, '╭', '╮', 'New Tab Group')
        end if

        r = r + 1
        line = ' Name  ' // trim(g_name(1:max(0, g_name_len)))
        if (g_focus == GP_FOCUS_NAME) then
            call put_row(r, line, SEL_BG // SEL_FG)
        else
            call put_row(r, line, CHROME_BG // TEXT_FG)
        end if
        call region_add(REGION_GP_NAME, r, r, g_col0, g_col0 + g_width - 1)

        r = r + 1
        call put_edge(r, '├', '┤', '')

        do i = 1, GP_VISIBLE
            r = r + 1
            idx = g_scroll + i
            if (idx > g_n_items) then
                call put_row(r, '', BODY_BG)
                cycle
            end if
            line = item_line(idx)
            if (idx == g_sel .and. g_focus == GP_FOCUS_LIST) then
                call put_row(r, line, SEL_BG // SEL_FG)
            else if (g_items(idx)%is_dir) then
                call put_row(r, line, BODY_BG // DIR_FG)
            else if (picked_index(full_path(idx)) > 0) then
                ! Ticked rows carry the accent colour too, so a glance down the
                ! list says what is in the group without reading each box.
                call put_row(r, line, BODY_BG // TICK_FG)
            else
                call put_row(r, line, BODY_BG // TEXT_FG)
            end if
            call region_add(REGION_GP_ROW, r, r, g_col0, g_col0 + g_width - 1, idx)
        end do

        r = r + 1
        call put_edge(r, '├', '┤', '')

        r = r + 1
        ! The count matters: ticks in other directories have scrolled out of
        ! sight, and without this the ../ row looks like it loses them.
        write(foot, '(i0,a)') g_n_picked, ' selected'
        line = ' ' // clip_dir(trim(g_dir), g_width - 16) // '  ' // trim(foot)
        call put_row(r, line, CHROME_BG // TEXT_FG)

        r = r + 1
        if (len_trim(g_note) > 0) then
            call put_row(r, ' ' // trim(g_note), CHROME_BG // TICK_FG)
        else
            if (g_mode == GP_MODE_EDIT) then
                line = ' tab focus  ·  space tick  ·  enter save  ·  esc cancel'
            else
                line = ' tab focus  ·  space tick  ·  enter create  ·  esc cancel'
            end if
            call put_row(r, line, CHROME_BG // HINT_FG)
        end if

        r = r + 1
        call put_edge(r, '╰', '╯', '')

        if (g_focus == GP_FOCUS_NAME) then
            ! +1 for the left border, +7 for the ' Name  ' label.
            call terminal_move_cursor(g_row0 + 1, g_col0 + 8 + g_name_len)
        else
            call terminal_hide_cursor()
        end if
    end subroutine render_group_picker

    !> One framed row: coloured border, filled body, padded to the full width.
    !>
    !> The border characters are written separately from the text on purpose.
    !> clip_to_cells counts DISPLAY CELLS, so an escape sequence embedded in
    !> the string would be counted as though it were visible and would silently
    !> shorten the row -- the text it clips must stay free of styling.
    subroutine put_row(row, text, body_style)
        integer, intent(in) :: row
        character(len=*), intent(in) :: text, body_style
        character(len=:), allocatable :: shown
        integer :: used, inner

        inner = max(0, g_width - 2)
        call clip_to_cells(text, inner, shown, used)
        call terminal_move_cursor(row, g_col0)
        call terminal_write(FRAME // '│')
        call terminal_write(body_style // shown // repeat(' ', max(0, inner - used)))
        call terminal_write(FRAME // '│' // RESET)
    end subroutine put_row

    !> A horizontal rule with the given corners, and optionally a title set
    !> into it.
    subroutine put_edge(row, left, right, title)
        integer, intent(in) :: row
        character(len=*), intent(in) :: left, right, title
        integer :: inner, pad

        inner = max(0, g_width - 2)
        call terminal_move_cursor(row, g_col0)
        call terminal_write(FRAME // left)
        if (len_trim(title) > 0 .and. inner > len_trim(title) + 2) then
            pad = inner - (len_trim(title) + 2)
            call terminal_write(repeat('─', pad / 2) // ' ')
            call terminal_write(TITLE_FG // trim(title) // FRAME)
            call terminal_write(' ' // repeat('─', pad - pad / 2))
        else
            call terminal_write(repeat('─', inner))
        end if
        call terminal_write(right // RESET)
    end subroutine put_edge

    !> A drop shadow, so the dialog reads as floating above the document
    !> rather than pasted into it. Two columns right and one row down, each
    !> run clipped to the screen -- an over-long write would wrap and smear
    !> the shadow across the far side of the row below.
    subroutine put_shadow()
        integer :: r, w, c

        c = g_col0 + g_width
        do r = g_row0 + 1, min(g_row0 + g_height, g_screen_rows)
            w = min(2, g_screen_cols - c + 1)
            if (w < 1) exit
            call terminal_move_cursor(r, c)
            call terminal_write(SHADOW_BG // repeat(' ', w) // RESET)
        end do

        r = g_row0 + g_height
        if (r <= g_screen_rows) then
            w = min(g_width, g_screen_cols - (g_col0 + 2) + 1)
            if (w > 0) then
                call terminal_move_cursor(r, g_col0 + 2)
                call terminal_write(SHADOW_BG // repeat(' ', w) // RESET)
            end if
        end if
    end subroutine put_shadow

    function item_line(idx) result(text)
        integer, intent(in) :: idx
        character(len=:), allocatable :: text

        ! The unticked box stays '[ ]': it is the affordance that says these
        ! rows can be ticked at all, and a bare glyph would not.
        if (g_items(idx)%is_up) then
            text = '  ▴ ..'
        else if (g_items(idx)%is_dir) then
            text = '  ▸ ' // trim(g_items(idx)%name) // '/'
        else if (picked_index(full_path(idx)) > 0) then
            ! The bullet says "unsaved work in here". It matters only in edit
            ! mode, where unticking this row closes the tab -- so the warning
            ! has to be readable BEFORE Enter, not in a prompt afterwards.
            if (is_dirty(full_path(idx))) then
                text = ' [✓] ' // trim(g_items(idx)%name) // ' •'
            else
                text = ' [✓] ' // trim(g_items(idx)%name)
            end if
        else
            text = ' [ ] ' // trim(g_items(idx)%name)
        end if
    end function item_line

    function clip_dir(path, cells) result(text)
        character(len=*), intent(in) :: path
        integer, intent(in) :: cells
        character(len=:), allocatable :: text

        if (len(path) <= cells) then
            text = path
        else
            text = '...' // path(len(path) - cells + 4:)
        end if
    end function clip_dir

    ! ---- small path helpers ----------------------------------------------
    !
    ! Duplicated rather than imported: fortress_fs_module sits far later in the
    ! build order and would drag a great deal in behind it, and the dependency
    ! rule this module follows is worth more than six lines of arithmetic.

    function parent_of(path) result(p)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: p
        integer :: k

        k = index(trim(path), '/', back=.true.)
        if (k <= 0) then
            p = '.'
        else if (k == 1) then
            p = '/'
        else
            p = path(1:k-1)
        end if
    end function parent_of

    function join(base, name) result(p)
        character(len=*), intent(in) :: base, name
        character(len=:), allocatable :: p

        if (trim(base) == '/') then
            p = '/' // trim(name)
        else
            p = trim(base) // '/' // trim(name)
        end if
    end function join

    function basename(path) result(b)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: b
        integer :: k

        k = index(trim(path), '/', back=.true.)
        if (k > 0 .and. k < len_trim(path)) then
            b = path(k+1:len_trim(path))
        else
            b = trim(path)
        end if
    end function basename

    function to_lower(s) result(o)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: o
        integer :: k, c

        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                o(k:k) = achar(c + 32)
            else
                o(k:k) = s(k:k)
            end if
        end do
    end function to_lower

end module group_picker_module
