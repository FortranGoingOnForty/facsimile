module context_menu_module
    ! The right-click context menu: a bordered box drawn at the pointer, over
    ! document text, that survives redraws and takes mouse and keyboard input.
    !
    ! This module owns geometry, drawing, hit-testing and keyboard navigation
    ! and nothing else. It deliberately does not `use editor_state_module` or
    ! `renderer_module`: the command handler decides which rows exist, what
    ! they are called, whether they are enabled and what each one does, and
    ! passes an opaque integer action back. That keeps this module low in the
    ! dependency graph -- the renderer uses it, so it cannot use the renderer
    ! -- and testable without an editor.
    !
    ! State is module-level rather than a component of editor_state_t. A
    ! context menu is a genuine singleton (two cannot be open), and adding a
    ! component to that derived type forces a rebuild on every contributor and
    ! a "Mismatch in components of derived type" for anyone who forgets.
    ! clickable_region_module keeps its table module-level for the same reason.
    !
    ! Shape follows completion_popup_module: geometry fixed at show time, a
    ! separate render called every frame. Emphatically NOT hover_tooltip_module,
    ! which draws once inside its show routine and is painted over by the next
    ! redraw while `visible` stays true.
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use clickable_region_module, only: region_add, REGION_BLOCK, REGION_CTX_ROW
    use modal_box_module, only: box_shadow
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_BORDER, THEME_DISABLED, THEME_PANEL, &
                            THEME_PANEL_SELECTION, theme_reset, theme_sgr
    implicit none
    private

    public :: context_menu_begin, context_menu_add_item, context_menu_add_separator
    public :: context_menu_show, context_menu_hide, is_context_menu_visible
    public :: context_menu_take, context_menu_hover, context_menu_select
    public :: render_context_menu, context_menu_handle_key
    public :: context_menu_selected, context_menu_kind
    public :: context_menu_row_action, context_menu_row_enabled
    public :: context_menu_row_at, context_menu_geometry, context_menu_row_count
    public :: CTX_MAX_ROWS

    integer, parameter :: CTX_MAX_ROWS  = 24
    integer, parameter :: CTX_MAX_LABEL = 48
    integer, parameter :: CTX_MAX_ACCEL = 16
    integer, parameter :: CTX_MIN_WIDTH = 18

    type :: ctx_row_t
        character(len=CTX_MAX_LABEL) :: label = ''
        character(len=CTX_MAX_ACCEL) :: accel = ''
        integer :: action = 0            ! opaque to this module
        logical :: separator = .false.
        logical :: enabled = .true.
    end type ctx_row_t

    type(ctx_row_t) :: g_rows(CTX_MAX_ROWS)
    integer :: g_n_rows = 0
    integer :: g_selected = 0            ! enabled, non-separator row; 0 = none
    integer :: g_kind = 0                ! opaque menu kind
    integer :: g_row0 = 0, g_col0 = 0, g_width = 0, g_height = 0
    integer :: g_max_row = 0, g_max_col = 0
    logical :: g_visible = .false.

contains

    !> Start building a menu of the given (caller-defined) kind.
    subroutine context_menu_begin(kind)
        integer, intent(in) :: kind

        g_n_rows = 0
        g_selected = 0
        g_kind = kind
        g_visible = .false.
    end subroutine context_menu_begin

    subroutine context_menu_add_item(label, accel, action, enabled)
        character(len=*), intent(in) :: label, accel
        integer, intent(in) :: action
        logical, intent(in), optional :: enabled

        if (g_n_rows >= CTX_MAX_ROWS) return
        g_n_rows = g_n_rows + 1
        g_rows(g_n_rows)%label = label
        g_rows(g_n_rows)%accel = accel
        g_rows(g_n_rows)%action = action
        g_rows(g_n_rows)%separator = .false.
        if (present(enabled)) then
            g_rows(g_n_rows)%enabled = enabled
        else
            g_rows(g_n_rows)%enabled = .true.
        end if
    end subroutine context_menu_add_item

    subroutine context_menu_add_separator()
        if (g_n_rows >= CTX_MAX_ROWS) return
        ! A leading separator would draw a stray rule under the top border
        if (g_n_rows == 0) return
        g_n_rows = g_n_rows + 1
        g_rows(g_n_rows)%label = ''
        g_rows(g_n_rows)%accel = ''
        g_rows(g_n_rows)%action = 0
        g_rows(g_n_rows)%separator = .true.
        g_rows(g_n_rows)%enabled = .false.
    end subroutine context_menu_add_separator

    !> Place and show the menu. The anchor is the pointer cell, which becomes
    !> the box's top-left corner. The allowed rectangle is passed in rather
    !> than derived, so the caller can exclude the tab bar, the status bar,
    !> the terminal panel and the file tree without this module knowing about
    !> any of them.
    !>
    !> Clamps, never flips: sliding the box is what the rest of the codebase
    !> does, and flipping it above the pointer would put a different row under
    !> the pointer than the one the user aimed at.
    !>
    !> Returns .false. and shows nothing when the space cannot hold the box.
    function context_menu_show(anchor_row, anchor_col, top_row, bottom_row, &
                               left_col, right_col) result(shown)
        integer, intent(in) :: anchor_row, anchor_col, top_row, bottom_row
        integer, intent(in) :: left_col, right_col
        logical :: shown
        integer :: i, need, avail_rows, avail_cols

        shown = .false.
        g_visible = .false.
        if (g_n_rows <= 0) return

        avail_rows = bottom_row - top_row + 1
        avail_cols = right_col - left_col + 1
        if (avail_rows < 3) return          ! two borders plus one row
        if (avail_cols < CTX_MIN_WIDTH) return

        ! Width from the widest row, measured in display cells
        g_width = CTX_MIN_WIDTH
        do i = 1, g_n_rows
            if (g_rows(i)%separator) cycle
            need = 2 + 1 + label_cells(i) + 2 + accel_cells(i) + 1
            if (need > g_width) g_width = need
        end do
        if (g_width > avail_cols) g_width = avail_cols

        g_height = g_n_rows + 2
        if (g_height > avail_rows) g_height = avail_rows

        g_row0 = anchor_row
        g_col0 = anchor_col
        g_max_row = bottom_row
        g_max_col = right_col
        if (g_row0 + g_height - 1 > bottom_row) g_row0 = bottom_row - g_height + 1
        if (g_row0 < top_row) g_row0 = top_row
        if (g_col0 + g_width - 1 > right_col) g_col0 = right_col - g_width + 1
        if (g_col0 < left_col) g_col0 = left_col

        g_selected = first_selectable()
        g_visible = .true.
        shown = .true.
    end function context_menu_show

    !> Move the highlight to the row under the pointer, if there is one.
    !> Returns .true. when the highlight moved, so the caller can ask for a
    !> redraw only when something actually changed rather than on every
    !> motion event.
    function context_menu_hover(row, col) result(moved)
        integer, intent(in) :: row, col
        logical :: moved
        integer :: idx

        moved = .false.
        if (.not. g_visible) return

        idx = context_menu_row_at(row, col)
        if (idx < 1) return                  ! off the menu, or on a separator
        if (.not. g_rows(idx)%enabled) return
        if (idx == g_selected) return

        g_selected = idx
        moved = .true.
    end function context_menu_hover

    !> Force the highlight onto a row and report whether it changed. Used to
    !> show which row a click landed on before acting on it.
    function context_menu_select(idx) result(moved)
        integer, intent(in) :: idx
        logical :: moved

        moved = .false.
        if (.not. g_visible) return
        if (.not. context_menu_row_enabled(idx)) return
        if (idx == g_selected) return

        g_selected = idx
        moved = .true.
    end function context_menu_select

    !> Take a row: report its action and the menu's kind, and close the menu,
    !> in one step. Atomic on purpose -- hiding clears the row list, so a
    !> caller that hid first and read afterwards would silently get action 0
    !> and do nothing.
    !>
    !> A disabled row reports enabled = .false. and leaves the menu open: the
    !> greying already says why, and the status bar that an explanation would
    !> use is underneath the box.
    subroutine context_menu_take(idx, action, kind, enabled)
        integer, intent(in) :: idx
        integer, intent(out) :: action, kind
        logical, intent(out) :: enabled

        action = 0
        kind = g_kind
        enabled = context_menu_row_enabled(idx)
        if (.not. enabled) return

        action = g_rows(idx)%action
        call context_menu_hide()
    end subroutine context_menu_take

    subroutine context_menu_hide()
        g_visible = .false.
        g_selected = 0
        g_n_rows = 0
    end subroutine context_menu_hide

    function is_context_menu_visible() result(vis)
        logical :: vis
        vis = g_visible
    end function is_context_menu_visible

    function context_menu_kind() result(k)
        integer :: k
        k = g_kind
    end function context_menu_kind

    function context_menu_selected() result(idx)
        integer :: idx
        idx = g_selected
    end function context_menu_selected

    function context_menu_row_count() result(n)
        integer :: n
        n = g_n_rows
    end function context_menu_row_count

    function context_menu_row_action(idx) result(action)
        integer, intent(in) :: idx
        integer :: action

        action = 0
        if (idx < 1) return
        if (idx > g_n_rows) return
        action = g_rows(idx)%action
    end function context_menu_row_action

    function context_menu_row_enabled(idx) result(ok)
        integer, intent(in) :: idx
        logical :: ok

        ok = .false.
        if (idx < 1) return
        if (idx > g_n_rows) return
        if (g_rows(idx)%separator) return
        ok = g_rows(idx)%enabled
    end function context_menu_row_enabled

    subroutine context_menu_geometry(row0, col0, width, height)
        integer, intent(out) :: row0, col0, width, height

        row0 = g_row0
        col0 = g_col0
        width = g_width
        height = g_height
    end subroutine context_menu_geometry

    !> Row index under a screen cell, or 0 for a miss. Borders count as a
    !> miss for activation, but the caller still swallows clicks on them --
    !> the whole box registers a blocking region.
    function context_menu_row_at(row, col) result(idx)
        integer, intent(in) :: row, col
        integer :: idx, i

        idx = 0
        if (.not. g_visible) return
        if (col < g_col0) return
        if (col > g_col0 + g_width - 1) return

        i = row - g_row0
        if (i < 1) return
        if (i > drawn_rows()) return
        if (g_rows(i)%separator) return
        idx = i
    end function context_menu_row_at

    !> Navigation keys only. `enter` is deliberately not handled: activating a
    !> row needs the editor and buffer, which this module must not know about,
    !> so the caller acts on a .false. return.
    function context_menu_handle_key(key) result(handled)
        character(len=*), intent(in) :: key
        logical :: handled

        handled = .false.
        if (.not. g_visible) return

        select case (trim(key))
        case ('up')
            call step_selection(-1)
            handled = .true.
        case ('down')
            call step_selection(1)
            handled = .true.
        case ('home')
            g_selected = first_selectable()
            handled = .true.
        case ('end')
            g_selected = last_selectable()
            handled = .true.
        case ('esc')
            call context_menu_hide()
            handled = .true.
        end select
    end function context_menu_handle_key

    !> Draw the box and claim its cells. Must be called after the carets in
    !> every render path: the caret renderers write reverse-video cells at
    !> every inactive cursor, which would otherwise punch holes through it.
    subroutine render_context_menu()
        integer :: i, n, r
        character(len=:), allocatable :: inner

        if (.not. g_visible) return
        if (g_width < 3) return

        n = drawn_rows()

        call box_shadow(g_row0, g_col0, g_height, g_width, &
                        g_max_row, g_max_col)

        call terminal_move_cursor(g_row0, g_col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '┌' // &
                            repeat('─', g_width - 2) // '┐' // theme_reset())

        do i = 1, n
            r = g_row0 + i
            call terminal_move_cursor(r, g_col0)
            if (g_rows(i)%separator) then
                call terminal_write(theme_sgr(THEME_BORDER) // '├' // &
                                    repeat('─', g_width - 2) // '┤' // theme_reset())
                cycle
            end if

            ! One write per row, no cursor moves inside it. The existing boxes
            ! place their right border with a separate move and leave the gap
            ! between text and border unpainted, so document text shows
            ! through; every cell here is written.
            inner = row_inner(i, g_width - 2)
            if (i == g_selected) then
                call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                    theme_sgr(THEME_PANEL_SELECTION) // inner // &
                    theme_sgr(THEME_BORDER) // '│' // theme_reset())
            else if (.not. g_rows(i)%enabled) then
                call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                    theme_sgr(THEME_DISABLED) // inner // &
                    theme_sgr(THEME_BORDER) // '│' // theme_reset())
            else
                call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                    theme_sgr(THEME_PANEL) // inner // &
                    theme_sgr(THEME_BORDER) // '│' // theme_reset())
            end if
        end do

        call terminal_move_cursor(g_row0 + n + 1, g_col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '└' // &
                            repeat('─', g_width - 2) // '┘' // theme_reset())

        ! Block first, rows after: region_at searches backwards, so the rows
        ! win over the block and the block catches the borders.
        call region_add(REGION_BLOCK, g_row0, g_row0 + n + 1, &
                        g_col0, g_col0 + g_width - 1)
        do i = 1, n
            if (g_rows(i)%separator) cycle
            ! Disabled rows claim their cells too, so clicking one is
            ! swallowed by the menu rather than reaching the document.
            call region_add(REGION_CTX_ROW, g_row0 + i, g_row0 + i, &
                            g_col0, g_col0 + g_width - 1, i)
        end do
    end subroutine render_context_menu

    ! ---- internals ----

    !> Rows that fit inside the clamped height (height = rows + 2 borders).
    function drawn_rows() result(n)
        integer :: n

        n = g_height - 2
        if (n > g_n_rows) n = g_n_rows
        if (n < 0) n = 0
    end function drawn_rows

    function label_cells(i) result(w)
        integer, intent(in) :: i
        integer :: w
        character(len=:), allocatable :: tmp

        call clip_to_cells(trim(g_rows(i)%label), CTX_MAX_LABEL * 2, tmp, w)
    end function label_cells

    function accel_cells(i) result(w)
        integer, intent(in) :: i
        integer :: w
        character(len=:), allocatable :: tmp

        call clip_to_cells(trim(g_rows(i)%accel), CTX_MAX_ACCEL * 2, tmp, w)
    end function accel_cells

    !> Exactly `cells` display columns: a leading space, the label, a gap, the
    !> right-aligned accelerator, a trailing space. Clipping is by display
    !> cell, never by byte -- a byte slice can split a multibyte sequence and
    !> emit a broken glyph.
    function row_inner(i, cells) result(s)
        integer, intent(in) :: i, cells
        character(len=:), allocatable :: s
        character(len=:), allocatable :: lab, acc
        integer :: lab_w, acc_w, room, gap

        ! The accelerator is short and identifies the row; the label yields.
        call clip_to_cells(trim(g_rows(i)%accel), max(0, cells - 4), acc, acc_w)
        room = cells - 2 - acc_w - 1
        if (room < 0) room = 0
        call clip_to_cells(trim(g_rows(i)%label), room, lab, lab_w)

        gap = cells - 2 - lab_w - acc_w
        if (gap < 1) gap = 1

        s = ' ' // lab // repeat(' ', gap) // acc // ' '
    end function row_inner

    function first_selectable() result(idx)
        integer :: idx, i, n

        idx = 0
        n = drawn_rows()
        do i = 1, n
            if (g_rows(i)%separator) cycle
            if (.not. g_rows(i)%enabled) cycle
            idx = i
            return
        end do
    end function first_selectable

    function last_selectable() result(idx)
        integer :: idx, i, n

        idx = 0
        n = drawn_rows()
        do i = n, 1, -1
            if (g_rows(i)%separator) cycle
            if (.not. g_rows(i)%enabled) cycle
            idx = i
            return
        end do
    end function last_selectable

    !> Move the selection by one enabled, non-separator row, wrapping at the
    !> ends. A menu with nothing selectable leaves g_selected at 0.
    subroutine step_selection(dir)
        integer, intent(in) :: dir
        integer :: i, n, probe

        n = drawn_rows()
        if (n <= 0) return
        if (first_selectable() == 0) then
            g_selected = 0
            return
        end if

        probe = g_selected
        if (probe < 1) probe = 1
        do i = 1, n
            probe = probe + dir
            if (probe > n) probe = 1
            if (probe < 1) probe = n
            if (g_rows(probe)%separator) cycle
            if (.not. g_rows(probe)%enabled) cycle
            g_selected = probe
            return
        end do
    end subroutine step_selection

end module context_menu_module
