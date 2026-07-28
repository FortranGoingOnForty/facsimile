module group_picker_module
    ! The "new tab group" dialog: name it, tick the files, create it.
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

    public :: group_picker_show, group_picker_hide, is_group_picker_visible
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

    character(len=*), parameter :: ESC     = char(27)
    character(len=*), parameter :: INVERSE = ESC // '[7m'
    character(len=*), parameter :: DIM     = ESC // '[90m'
    character(len=*), parameter :: RESET   = ESC // '[0m'

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

    character(len=GP_NAME_MAX) :: g_name = ''
    integer :: g_name_len = 0

    integer :: g_focus = GP_FOCUS_NAME
    integer :: g_sel = 1, g_scroll = 0
    integer :: g_row0 = 0, g_col0 = 0, g_width = 0, g_height = 0
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

        shown = .false.
        g_n_picked = 0
        g_result = GP_PENDING
        g_note = ''
        g_focus = GP_FOCUS_NAME

        call walk_to(dir)
        if (g_n_items == 0) return

        ! Pre-fill the name so the happy path is Enter-Enter.
        g_name = basename(trim(g_dir)) // '/'
        g_name_len = len_trim(g_name)

        g_width = min(GP_WIDTH, max(24, screen_cols - 4))
        g_height = GP_VISIBLE + 7
        g_col0 = max(1, (screen_cols - g_width) / 2)
        g_row0 = max(1, (screen_rows - g_height) / 2)
        if (g_row0 + g_height - 1 > screen_rows) g_row0 = max(1, screen_rows - g_height + 1)

        g_visible = .true.
        shown = .true.
    end function group_picker_show

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
        integer :: r, i, idx, used
        character(len=:), allocatable :: shown, line
        character(len=64) :: foot

        if (.not. g_visible) return

        ! The box owns its rectangle, so clicks on it never reach the document.
        call region_add(REGION_BLOCK, g_row0, g_row0 + g_height - 1, &
                        g_col0, g_col0 + g_width - 1)

        r = g_row0
        call put(r, '+' // dashes(' New Tab Group ') // '+')

        r = r + 1
        line = ' Name  ' // trim(g_name(1:max(0, g_name_len)))
        if (g_focus == GP_FOCUS_NAME) then
            call put_styled(r, line, INVERSE)
        else
            call put(r, line)
        end if
        call region_add(REGION_GP_NAME, r, r, g_col0, g_col0 + g_width - 1)

        r = r + 1
        call put(r, '+' // repeat('-', g_width - 2) // '+')

        do i = 1, GP_VISIBLE
            r = r + 1
            idx = g_scroll + i
            if (idx > g_n_items) then
                call put(r, '')
                cycle
            end if
            line = item_line(idx)
            if (idx == g_sel .and. g_focus == GP_FOCUS_LIST) then
                call put_styled(r, line, INVERSE)
            else if (g_items(idx)%is_dir) then
                call put_styled(r, line, DIM)
            else
                call put(r, line)
            end if
            call region_add(REGION_GP_ROW, r, r, g_col0, g_col0 + g_width - 1, idx)
        end do

        r = r + 1
        call put(r, '+' // repeat('-', g_width - 2) // '+')

        r = r + 1
        ! The count matters: ticks in other directories have scrolled out of
        ! sight, and without this the ../ row looks like it loses them.
        write(foot, '(i0,a)') g_n_picked, ' selected'
        call put(r, ' ' // clip_dir(trim(g_dir), g_width - 16) // '  ' // trim(foot))

        r = r + 1
        if (len_trim(g_note) > 0) then
            call put(r, ' ' // trim(g_note))
        else
            call put(r, ' tab focus | space tick | enter create | esc cancel')
        end if

        r = r + 1
        call put(r, '+' // repeat('-', g_width - 2) // '+')

        if (g_focus == GP_FOCUS_NAME) then
            call terminal_move_cursor(g_row0 + 1, g_col0 + 7 + g_name_len)
        else
            call terminal_hide_cursor()
        end if
    end subroutine render_group_picker

    function item_line(idx) result(text)
        integer, intent(in) :: idx
        character(len=:), allocatable :: text

        if (g_items(idx)%is_up) then
            text = '  ^ ..'
        else if (g_items(idx)%is_dir) then
            text = '  > ' // trim(g_items(idx)%name) // '/'
        else if (picked_index(full_path(idx)) > 0) then
            text = ' [x] ' // trim(g_items(idx)%name)
        else
            text = ' [ ] ' // trim(g_items(idx)%name)
        end if
    end function item_line

    ! Every row is written as one string with every cell painted, so document
    ! text cannot show through a gap.
    subroutine put(row, text)
        integer, intent(in) :: row
        character(len=*), intent(in) :: text

        call put_styled(row, text, '')
    end subroutine put

    subroutine put_styled(row, text, style)
        integer, intent(in) :: row
        character(len=*), intent(in) :: text, style
        character(len=:), allocatable :: shown
        integer :: used

        call clip_to_cells(text, g_width, shown, used)
        call terminal_move_cursor(row, g_col0)
        if (len(style) > 0) call terminal_write(style)
        call terminal_write(shown // repeat(' ', max(0, g_width - used)))
        if (len(style) > 0) call terminal_write(RESET)
    end subroutine put_styled

    function dashes(title) result(text)
        character(len=*), intent(in) :: title
        character(len=:), allocatable :: text
        integer :: pad

        pad = max(0, g_width - 2 - len(title))
        text = repeat('-', pad / 2) // title // repeat('-', pad - pad / 2)
    end function dashes

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
