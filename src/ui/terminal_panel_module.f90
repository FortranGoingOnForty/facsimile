module terminal_panel_module
    use iso_fortran_env, only: int32, int64
    use iso_c_binding
    use terminal_io_module, only: terminal_write, terminal_move_cursor, &
                                   terminal_flush
    implicit none
    private

    public :: terminal_panel_t
    public :: init_terminal_panel, cleanup_terminal_panel
    public :: toggle_terminal_panel, is_terminal_panel_visible
    public :: terminal_panel_is_alive, terminal_panel_restart
    public :: terminal_panel_is_dragging, terminal_panel_has_selection
    public :: terminal_panel_copy_selection
    public :: terminal_panel_poll, terminal_panel_render
    public :: terminal_panel_handle_key
    public :: terminal_panel_paste
    public :: terminal_panel_handle_mouse
    public :: terminal_panel_in_region
    public :: terminal_panel_scroll
    public :: terminal_panel_resize
    public :: get_terminal_panel_height
    public :: height_for, permille_for
    public :: terminal_panel_set_height, terminal_panel_nudge_height
    public :: terminal_panel_toggle_maximize, terminal_panel_is_maximized
    public :: terminal_panel_set_default_permille
    public :: terminal_panel_get_permille

    ! C PTY interface
    interface
        subroutine c_pty_spawn(shell, shell_len, rows, cols, &
                               handle, error) &
            bind(C, name='pty_spawn_f')
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: shell(*)
            integer(c_int), intent(in) :: shell_len
            integer(c_int), intent(in) :: rows, cols
            type(c_ptr), intent(out) :: handle
            integer(c_int), intent(out) :: error
        end subroutine

        function c_pty_read(handle, buffer, bufsize) &
            bind(C, name='pty_read_f') result(n)
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), intent(in) :: bufsize
            integer(c_int) :: n
        end function

        function c_pty_write(handle, data, len) &
            bind(C, name='pty_write_f') result(n)
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            character(kind=c_char), intent(in) :: data(*)
            integer(c_int), intent(in) :: len
            integer(c_int) :: n
        end function

        function c_pty_resize(handle, rows, cols) &
            bind(C, name='pty_resize_f') result(res)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: rows, cols
            integer(c_int) :: res
        end function

        function c_pty_is_running(handle) &
            bind(C, name='pty_is_running_f') result(res)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: res
        end function

        subroutine c_pty_close(handle) &
            bind(C, name='pty_close_f')
            import :: c_ptr
            type(c_ptr), intent(inout) :: handle
        end subroutine
    end interface

    ! C VT100 grid interface
    interface
        subroutine c_grid_create(handle, rows, cols) &
            bind(C, name='vt100_grid_create_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(out) :: handle
            integer(c_int), intent(in) :: rows, cols
        end subroutine

        subroutine c_grid_destroy(handle) &
            bind(C, name='vt100_grid_destroy_f')
            import :: c_ptr
            type(c_ptr), intent(inout) :: handle
        end subroutine

        subroutine c_grid_feed(handle, data, len) &
            bind(C, name='vt100_grid_feed_f')
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            character(kind=c_char), intent(in) :: data(*)
            integer(c_int), intent(in) :: len
        end subroutine

        subroutine c_grid_resize(handle, rows, cols) &
            bind(C, name='vt100_grid_resize_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: rows, cols
        end subroutine

        ! buf receives up to 4 UTF-8 bytes; nbytes is 0 for the second cell
        ! of a double-width glyph, which must produce no output.
        subroutine c_grid_get_cell(handle, row, col, &
                                    buf, nbytes, fg, bg, attr) &
            bind(C, name='vt100_grid_get_cell_f')
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: row, col
            character(kind=c_char), intent(out) :: buf(4)
            integer(c_int), intent(out) :: nbytes, fg, bg, attr
        end subroutine

        subroutine c_grid_get_cursor(handle, row, col) &
            bind(C, name='vt100_grid_get_cursor_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(out) :: row, col
        end subroutine

        function c_grid_app_cursor(handle) &
            bind(C, name='vt100_grid_app_cursor_f') result(m)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: m
        end function

        function c_grid_alt_screen(handle) &
                result(res) &
                bind(C, name='vt100_grid_alt_screen_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: res
        end function

        function c_grid_bracketed_paste(handle) &
            bind(C, name='vt100_grid_bracketed_paste_f') &
            result(m)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: m
        end function

        subroutine c_grid_scroll_view(handle, delta) &
            bind(C, name='vt100_grid_scroll_view_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: delta
        end subroutine

        subroutine c_grid_reset_view(handle) &
            bind(C, name='vt100_grid_reset_view_f')
            import :: c_ptr
            type(c_ptr), intent(inout) :: handle
        end subroutine

        function c_grid_view_offset(handle) &
            bind(C, name='vt100_grid_view_offset_f') result(o)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: o
        end function

        subroutine c_grid_get_view_cell(handle, row, col, &
                                         buf, nbytes, fg, bg, attr) &
            bind(C, name='vt100_grid_get_view_cell_f')
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: row, col
            character(kind=c_char), intent(out) :: buf(4)
            integer(c_int), intent(out) :: nbytes, fg, bg, attr
        end subroutine

        function c_grid_cpr_pending(handle) &
            bind(C, name='vt100_grid_cpr_pending_f') result(p)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: p
        end function

        subroutine c_grid_set_pty_fd(handle, fd) &
            bind(C, name='vt100_grid_set_pty_fd_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: fd
        end subroutine

        function c_pty_get_fd(handle) &
            bind(C, name='pty_get_fd_f') result(fd)
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: fd
        end function
    end interface

    integer, parameter :: MIN_HEIGHT = 5
    ! What the EDITOR must keep. The old code capped the panel at 80% of the
    ! screen, which says the same thing from the wrong end -- on a 24-row
    ! terminal it left the document 4 rows without ever saying so. Naming the
    ! floor after the thing it protects makes the clamp arguable.
    integer, parameter :: MIN_EDITOR_ROWS = 4
    ! Only the DEFAULT now. The height a user has chosen lives in
    ! panel%height_permille; see height_for.
    integer, parameter :: DEFAULT_PERMILLE = 300
    integer, parameter :: READ_BUF_SIZE = 8192

    character(len=1), parameter :: ESC_CH = achar(27)

    type :: terminal_panel_t
        logical :: visible = .false.
        logical :: focused = .false.
        ! The DERIVED row count, cached here because the renderer reads it
        ! several times a frame. height_permille is the thing that means
        ! something; this is what it works out to at the current screen size.
        integer :: height = 0
        ! The user's intent, as a fraction of the screen. Kept as a ratio
        ! rather than a row count so the panel holds its proportion when the
        ! window is resized -- which is the whole reason terminal_panel_resize
        ! can stay a one-liner instead of growing a special case.
        integer :: height_permille = DEFAULT_PERMILLE
        ! Where to go back to when un-maximising. 0 = not maximised.
        integer :: prev_permille = 0
        type(c_ptr) :: pty_handle = c_null_ptr
        type(c_ptr) :: grid_handle = c_null_ptr
        integer :: pty_rows = 0
        integer :: pty_cols = 0
        logical :: pty_alive = .false.
        logical :: has_new_output = .false.
        ! On-screen geometry (1-based), recorded each render so
        ! mouse coordinates can be mapped to grid cells.
        integer :: screen_start_row = 0  ! separator-bar row
        integer :: screen_cols = 0
        ! Where the shell's prompt ends (grid coordinates, 0-based), so an
        ! empty input line can be told from one with text on it. Updated
        ! whenever output moves to a new line -- character echo of what the
        ! user types deliberately does not move it, which is what makes the
        ! caret drifting right of it mean "there is text here". -1 = unset.
        integer :: prompt_row = -1
        integer :: prompt_col = -1
        ! Mouse text selection (grid coordinates, 0-based).
        logical :: sel_active = .false.
        ! A drag that began inside the panel keeps receiving events
        ! even once the pointer leaves it -- see the router.
        logical :: sel_dragging = .false.
        ! Dragging the top edge to resize. Separate from sel_dragging because
        ! the two are mutually exclusive and start from the same button press;
        ! which one begins is decided by the row the press landed on.
        logical :: resize_dragging = .false.
        integer :: sel_anchor_row = 0
        integer :: sel_anchor_col = 0
        integer :: sel_end_row = 0
        integer :: sel_end_col = 0
    end type terminal_panel_t

contains

    subroutine init_terminal_panel(panel)
        type(terminal_panel_t), intent(out) :: panel
        panel%visible = .false.
        panel%focused = .false.
        panel%height = 0
        panel%pty_handle = c_null_ptr
        panel%grid_handle = c_null_ptr
        panel%pty_rows = 0
        panel%pty_cols = 0
        panel%pty_alive = .false.
    end subroutine init_terminal_panel

    subroutine cleanup_terminal_panel(panel)
        type(terminal_panel_t), intent(inout) :: panel
        if (c_associated(panel%pty_handle)) then
            call c_pty_close(panel%pty_handle)
        end if
        if (c_associated(panel%grid_handle)) then
            call c_grid_destroy(panel%grid_handle)
        end if
        panel%visible = .false.
        panel%focused = .false.
        panel%pty_alive = .false.
    end subroutine cleanup_terminal_panel

    subroutine toggle_terminal_panel(panel, screen_rows, &
                                     screen_cols)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_rows, screen_cols
        character(len=512) :: shell_path
        integer :: shell_len
        integer(c_int) :: c_rows, c_cols, c_shell_len, c_error

        if (panel%visible) then
            ! Toggle off — hide but keep PTY alive
            panel%visible = .false.
            panel%focused = .false.
            return
        end if

        panel%height = height_for(panel%height_permille, screen_rows)
        panel%pty_rows = panel%height - 1  ! -1 for separator
        panel%pty_cols = screen_cols

        ! Spawn PTY if not already alive
        if (.not. c_associated(panel%pty_handle)) then
            ! Get shell from $SHELL
            call get_environment_variable('SHELL', shell_path, &
                                          shell_len)
            if (shell_len == 0) then
                shell_path = '/bin/sh'
                shell_len = 7
            end if

            c_rows = int(panel%pty_rows, c_int)
            c_cols = int(panel%pty_cols, c_int)
            c_shell_len = int(shell_len, c_int)

            call c_pty_spawn(shell_path, c_shell_len, &
                             c_rows, c_cols, &
                             panel%pty_handle, c_error)

            if (c_error /= 0) then
                panel%pty_handle = c_null_ptr
                panel%visible = .false.
                return
            end if
            panel%pty_alive = .true.

            ! Create grid and give it the PTY fd for inline responses
            call c_grid_create(panel%grid_handle, c_rows, c_cols)
            block
                integer(c_int) :: pty_fd
                pty_fd = c_pty_get_fd(panel%pty_handle)
                call c_grid_set_pty_fd(panel%grid_handle, pty_fd)
            end block
        end if

        panel%visible = .true.
        panel%focused = .true.

        ! A PTY kept alive while hidden may hold stale dimensions if the
        ! screen was resized in the meantime — reapply them on show
        call terminal_panel_resize(panel, screen_rows, screen_cols)
    end subroutine toggle_terminal_panel

    function is_terminal_panel_visible(panel) result(vis)
        type(terminal_panel_t), intent(in) :: panel
        logical :: vis
        vis = panel%visible
    end function is_terminal_panel_visible

    ! A shell can exit while the panel is still on screen -- `exit`, Ctrl-D,
    ! or an idle timeout such as bash's TMOUT. The panel then looks exactly
    ! as it did a moment earlier, so the state has to be queryable: without
    ! this the caller cannot tell a working terminal from a dead one, and
    ! keystrokes meant for the shell end up in the document.
    function terminal_panel_is_alive(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        res = panel%pty_alive .and. c_associated(panel%pty_handle)
    end function terminal_panel_is_alive

    ! Replace a dead shell with a fresh one, keeping the panel where it is.
    subroutine terminal_panel_restart(panel, screen_rows, screen_cols)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_rows, screen_cols

        if (c_associated(panel%pty_handle)) then
            call c_pty_close(panel%pty_handle)
            panel%pty_handle = c_null_ptr
        end if
        if (c_associated(panel%grid_handle)) then
            call c_grid_destroy(panel%grid_handle)
            panel%grid_handle = c_null_ptr
        end if
        panel%pty_alive = .false.
        panel%sel_active = .false.

        ! toggle_terminal_panel spawns when there is no live handle, but it
        ! toggles off first when visible -- so clear the flag and let it open.
        panel%visible = .false.
        call toggle_terminal_panel(panel, screen_rows, screen_cols)
    end subroutine terminal_panel_restart

    function get_terminal_panel_height(panel) result(h)
        type(terminal_panel_t), intent(in) :: panel
        integer :: h
        if (panel%visible) then
            h = panel%height
        else
            h = 0
        end if
    end function get_terminal_panel_height

    !> Rows for a given ratio at a given screen size.
    !>
    !> The single place the arithmetic lives. It used to be six lines
    !> duplicated between opening the panel and resizing it, which is how the
    !> two came to disagree about whether a chosen height survived.
    !>
    !> Both clamps can fight on a small screen -- a 10-row terminal cannot give
    !> the panel MIN_HEIGHT and still leave the editor MIN_EDITOR_ROWS. The
    !> editor wins, because a panel one row short is awkward whereas a document
    !> with no visible lines is useless.
    pure function height_for(permille, screen_rows) result(h)
        integer, intent(in) :: permille, screen_rows
        integer :: h, ceiling_h

        ! Rounded, not truncated, and permille_for rounds to match. Truncating
        ! both directions makes rows -> permille -> rows lose a row on screen
        ! sizes that do not divide evenly, so the panel would creep one row
        ! smaller every time the window was touched.
        h = (screen_rows * permille + 500) / 1000
        if (h < MIN_HEIGHT) h = MIN_HEIGHT

        ! Leave room for the status bar (1) and the document.
        ceiling_h = screen_rows - 1 - MIN_EDITOR_ROWS
        if (h > ceiling_h) h = ceiling_h

        ! Below this the panel has no usable rows at all: one goes to the
        ! separator bar, so 2 is the least that shows a single line of shell.
        if (h < 2) h = 2
    end function height_for

    !> The inverse, so a row count arrived at by dragging or by a keypress can
    !> be stored back as the ratio that is the real source of truth.
    pure function permille_for(rows, screen_rows) result(permille)
        integer, intent(in) :: rows, screen_rows
        integer :: permille

        if (screen_rows <= 0) then
            permille = DEFAULT_PERMILLE
        else
            permille = (rows * 1000 + screen_rows / 2) / screen_rows
        end if
        if (permille < 1) permille = 1
        if (permille > 1000) permille = 1000
    end function permille_for

    !> Set the panel's height from a ratio, resizing the pty and grid to match.
    !>
    !> `changed` reports whether the derived ROW COUNT moved, not whether the
    !> ratio did. A drag delivers an event per cell of pointer travel and most
    !> of them land on the row already showing; resizing the grid for those
    !> would be a calloc and a SIGWINCH to the shell for no visible change.
    subroutine terminal_panel_set_height(panel, permille, screen_rows, &
                                         screen_cols, changed)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: permille, screen_rows, screen_cols
        logical, intent(out) :: changed
        integer :: new_height

        changed = .false.
        if (.not. panel%visible) return

        new_height = height_for(permille, screen_rows)
        ! Store the clamped ratio, not the requested one. Otherwise dragging
        ! past the end accumulates an out-of-range intent that springs back
        ! the moment the window grows.
        panel%height_permille = permille_for(new_height, screen_rows)
        if (new_height == panel%height) return

        changed = .true.
        call terminal_panel_resize(panel, screen_rows, screen_cols)
    end subroutine terminal_panel_set_height

    !> Grow or shrink by whole rows.
    !>
    !> Steps in rows rather than in permille so a keypress feels the same on
    !> any screen -- a fixed ratio step moves two rows on a tall terminal and
    !> none at all on a short one.
    subroutine terminal_panel_nudge_height(panel, rows, screen_rows, &
                                           screen_cols, changed)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: rows, screen_rows, screen_cols
        logical, intent(out) :: changed

        panel%prev_permille = 0        ! a manual nudge ends "maximised"
        call terminal_panel_set_height(panel, &
            permille_for(panel%height + rows, screen_rows), &
            screen_rows, screen_cols, changed)
    end subroutine terminal_panel_nudge_height

    !> Toggle between as-tall-as-allowed and whatever it was before.
    subroutine terminal_panel_toggle_maximize(panel, screen_rows, &
                                              screen_cols, changed)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_rows, screen_cols
        logical, intent(out) :: changed
        integer :: target

        if (panel%prev_permille > 0) then
            target = panel%prev_permille
            panel%prev_permille = 0
        else
            ! Remember the ratio, not the row count: restoring onto a
            ! differently-sized window should give back the same proportion.
            panel%prev_permille = panel%height_permille
            target = 1000
        end if
        call terminal_panel_set_height(panel, target, screen_rows, &
                                       screen_cols, changed)
    end subroutine terminal_panel_toggle_maximize

    !> True while the panel is at its maximum by way of the maximise toggle.
    function terminal_panel_is_maximized(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        res = panel%visible .and. panel%prev_permille > 0
    end function terminal_panel_is_maximized

    !> The stored ratio, for persistence.
    function terminal_panel_get_permille(panel) result(permille)
        type(terminal_panel_t), intent(in) :: panel
        integer :: permille
        permille = panel%height_permille
    end function terminal_panel_get_permille

    !> Seed the default height. Called once at startup; a workspace that has a
    !> stored height overrides this afterwards.
    subroutine terminal_panel_set_default_permille(panel, permille)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: permille
        panel%height_permille = max(50, min(950, permille))
    end subroutine terminal_panel_set_default_permille

    ! Non-blocking read from PTY, feed to grid
    ! Also intercepts terminal queries and responds automatically
    subroutine terminal_panel_poll(panel)
        type(terminal_panel_t), intent(inout) :: panel
        character(len=1) :: read_buf(READ_BUF_SIZE)
        integer(c_int) :: bytes_read, c_bufsize, c_len

        if (.not. c_associated(panel%pty_handle)) return
        if (.not. panel%pty_alive) return

        panel%has_new_output = .false.
        c_bufsize = int(READ_BUF_SIZE, c_int)

        ! Read in a loop to drain available data
        do
            bytes_read = c_pty_read(panel%pty_handle, read_buf, &
                                     c_bufsize)
            if (bytes_read <= 0) exit

            ! Feed to grid
            c_len = bytes_read
            call c_grid_feed(panel%grid_handle, read_buf, c_len)
            panel%has_new_output = .true.
            call note_prompt_position(panel, read_buf, int(bytes_read))

            ! Check if grid needs a CPR response
            if (c_grid_cpr_pending(panel%grid_handle) /= 0) then
                block
                    integer(c_int) :: cr, cc, cpr_len, cpr_res
                    character(len=32) :: cpr_resp
                    call c_grid_get_cursor(panel%grid_handle, &
                        cr, cc)
                    ! CPR response: ESC [ row ; col R (1-based)
                    write(cpr_resp, '(a,i0,a,i0,a)') &
                        achar(27) // '[', int(cr)+1, ';', &
                        int(cc)+1, 'R'
                    cpr_len = int(len_trim(cpr_resp), c_int)
                    cpr_res = c_pty_write(panel%pty_handle, &
                        cpr_resp, cpr_len)
                end block
            end if
        end do

        ! Check if child is still running
        if (c_pty_is_running(panel%pty_handle) == 0) then
            panel%pty_alive = .false.
        end if
    end subroutine terminal_panel_poll


    ! Render the terminal panel to screen
    subroutine terminal_panel_render(panel, start_row, cols)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: start_row, cols
        integer :: r, c, grid_rows, grid_cols
        integer(c_int) :: c_row, c_col, c_fg, c_bg, c_attr
        character(kind=c_char) :: cell_buf(4)
        character(len=4) :: ch_utf8
        integer(c_int) :: c_nbytes
        integer :: nb, bi
        integer :: last_fg, last_bg, last_attr
        integer :: cursor_r, cursor_c
        integer(c_int) :: cc_row, cc_col
        character(len=32) :: ansi_seq
        logical :: at_bottom
        integer :: vo
        character(len=40) :: scroll_lbl

        if (.not. panel%visible) return
        if (.not. c_associated(panel%grid_handle)) return

        ! Cursor is only meaningful when viewing the live bottom
        vo = int(c_grid_view_offset(panel%grid_handle))
        at_bottom = (vo == 0)

        ! Record on-screen geometry for mouse coordinate mapping
        panel%screen_start_row = start_row
        panel%screen_cols = cols

        grid_rows = panel%pty_rows
        grid_cols = min(panel%pty_cols, cols)

        ! Draw separator bar. When scrolled back into history, show a
        ! bright SCROLLBACK marker so it's unmistakable the view is not
        ! the live prompt (and the live cursor is hidden).
        call terminal_move_cursor(start_row, 1)
        if (vo > 0 .and. cols > 24) then
            write(scroll_lbl, '(a,i0,a)') ' SCROLLBACK -', vo, ' '
            call terminal_write(ESC_CH // '[93m')   ! bright yellow
            call terminal_write(repeat('-', 6))
            call terminal_write(trim(scroll_lbl))
            call terminal_write(repeat('-', &
                max(0, cols - 6 - len_trim(scroll_lbl))))
        else if (.not. panel%pty_alive .and. cols > 40) then
            ! The shell exited but the panel is still up. Say so, and say how
            ! to get out -- otherwise it looks like a working terminal that
            ! has silently stopped accepting input.
            call terminal_write(ESC_CH // '[93m')
            call terminal_write(repeat('-', 6))
            call terminal_write(' PROCESS EXITED - Enter: new shell, Esc: close ')
            call terminal_write(repeat('-', max(0, cols - 6 - 47)))
        else
            call terminal_write(ESC_CH // '[90m')
            call terminal_write(repeat('-', min(cols, 6)))
            if (cols > 14) then
                if (panel%focused) then
                    call terminal_write(' TERMINAL ')
                else
                    call terminal_write(' terminal ')
                end if
                call terminal_write(repeat('-', cols - 16))
            end if
        end if
        call terminal_write(ESC_CH // '[0m')

        ! Get cursor position for highlighting
        call c_grid_get_cursor(panel%grid_handle, cc_row, cc_col)
        cursor_r = int(cc_row)
        cursor_c = int(cc_col)

        ! Render grid cells
        last_fg = -1
        last_bg = -1
        last_attr = -1

        do r = 0, grid_rows - 1
            call terminal_move_cursor(start_row + 1 + r, 1)

            ! Reset style at start of each row
            call terminal_write(ESC_CH // '[0m')
            last_fg = 0
            last_bg = 0
            last_attr = 0

            do c = 0, grid_cols - 1
                c_row = int(r, c_int)
                c_col = int(c, c_int)
                call c_grid_get_view_cell(panel%grid_handle, &
                    c_row, c_col, cell_buf, c_nbytes, c_fg, c_bg, c_attr)
                ! A cell may hold a multi-byte glyph now, or nothing at all
                ! when it is the trailing half of a double-width one.
                nb = int(c_nbytes)
                if (nb > 0) then
                    do bi = 1, nb
                        ch_utf8(bi:bi) = cell_buf(bi)
                    end do
                end if

                ! Emit ANSI codes only when style changes
                if (int(c_fg) /= last_fg .or. &
                    int(c_bg) /= last_bg .or. &
                    int(c_attr) /= last_attr) then

                    call terminal_write(ESC_CH // '[0m')
                    last_fg = int(c_fg)
                    last_bg = int(c_bg)
                    last_attr = int(c_attr)

                    ! Attributes
                    if (iand(last_attr, 1) /= 0) then
                        call terminal_write(ESC_CH // '[1m')
                    end if
                    if (iand(last_attr, 2) /= 0) then
                        call terminal_write(ESC_CH // '[2m')
                    end if
                    if (iand(last_attr, 4) /= 0) then
                        call terminal_write(ESC_CH // '[3m')
                    end if
                    if (iand(last_attr, 8) /= 0) then
                        call terminal_write(ESC_CH // '[4m')
                    end if
                    if (iand(last_attr, 16) /= 0) then
                        call terminal_write(ESC_CH // '[7m')
                    end if

                    ! Foreground
                    if (last_fg > 0 .and. last_fg <= 8) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[', 29 + last_fg, 'm'
                        call terminal_write(trim(ansi_seq))
                    else if (last_fg >= 9 .and. last_fg <= 16) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[', 81 + last_fg, 'm'
                        call terminal_write(trim(ansi_seq))
                    else if (last_fg > 16) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[38;5;', last_fg - 1, 'm'
                        call terminal_write(trim(ansi_seq))
                    end if

                    ! Background
                    if (last_bg > 0 .and. last_bg <= 8) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[', 39 + last_bg, 'm'
                        call terminal_write(trim(ansi_seq))
                    else if (last_bg >= 9 .and. last_bg <= 16) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[', 91 + last_bg, 'm'
                        call terminal_write(trim(ansi_seq))
                    else if (last_bg > 16) then
                        write(ansi_seq, '(a,i0,a)') &
                            ESC_CH // '[48;5;', last_bg - 1, 'm'
                        call terminal_write(trim(ansi_seq))
                    end if
                end if

                ! Reverse-video for a selected cell or the cursor
                if (nb > 0) then
                    if (cell_selected(panel, r, c) .or. &
                        (at_bottom .and. panel%focused .and. &
                         r == cursor_r .and. c == cursor_c)) then
                        call terminal_write(ESC_CH // '[7m')
                        call terminal_write(ch_utf8(1:nb))
                        call terminal_write(ESC_CH // '[27m')
                    else
                        call terminal_write(ch_utf8(1:nb))
                    end if
                end if
            end do

            ! Clear to end of row if grid is narrower than screen
            if (grid_cols < cols) then
                call terminal_write(ESC_CH // '[0m')
                call terminal_write( &
                    repeat(' ', cols - grid_cols))
                last_fg = 0
                last_bg = 0
                last_attr = 0
            end if
        end do

        ! Reset at end
        call terminal_write(ESC_CH // '[0m')
    end subroutine terminal_panel_render

    ! Handle a key press — translate to bytes and write to PTY
    ! Returns .true. if the key was consumed
    ! Remember where the prompt ends. Only output that moved to a new line
    ! counts: that is a shell drawing a fresh prompt. The echo of a character
    ! the user typed carries no newline, so the prompt mark stays put and the
    ! caret advances past it -- which is exactly the signal we want.
    subroutine note_prompt_position(panel, data, n)
        type(terminal_panel_t), intent(inout) :: panel
        character(len=1), intent(in) :: data(*)
        integer, intent(in) :: n
        integer :: i
        integer(c_int) :: cr, cc
        logical :: line_moved

        if (.not. c_associated(panel%grid_handle)) return

        line_moved = panel%prompt_row < 0        ! first output ever
        do i = 1, n
            if (data(i) == achar(10) .or. data(i) == achar(13)) then
                line_moved = .true.
                exit
            end if
        end do
        if (.not. line_moved) return

        call c_grid_get_cursor(panel%grid_handle, cr, cc)
        panel%prompt_row = int(cr)
        panel%prompt_col = int(cc)
    end subroutine note_prompt_position

    ! True when the shell's input line has nothing typed on it: the caret is
    ! still sitting where the prompt left it.
    function terminal_panel_input_is_empty(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        integer(c_int) :: cr, cc
        type(c_ptr) :: h        ! the C accessors take void**, so pass a copy

        res = .false.
        if (.not. c_associated(panel%grid_handle)) return
        if (panel%prompt_row < 0) return

        h = panel%grid_handle
        call c_grid_get_cursor(h, cr, cc)
        res = int(cr) == panel%prompt_row .and. int(cc) <= panel%prompt_col
    end function terminal_panel_input_is_empty

    ! True while a full-screen program owns the terminal. Escape belongs to
    ! that program then, never to the panel.
    function terminal_panel_in_fullscreen_app(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        type(c_ptr) :: h

        res = .false.
        if (.not. c_associated(panel%grid_handle)) return
        h = panel%grid_handle
        res = c_grid_alt_screen(h) /= 0
    end function terminal_panel_in_fullscreen_app

    function terminal_panel_handle_key(panel, key_str) &
        result(handled)
        type(terminal_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key_str
        logical :: handled
        character(len=8) :: send_buf
        integer :: send_len
        integer(c_int) :: c_len, res

        handled = .false.
        if (.not. c_associated(panel%pty_handle)) return
        if (.not. panel%pty_alive) return

        send_len = 0

        select case(trim(key_str))
        ! Control characters
        case('enter')
            send_buf(1:1) = achar(13); send_len = 1
        case('tab')
            send_buf(1:1) = achar(9); send_len = 1
        case('backspace', 'ctrl-h')
            send_buf(1:1) = achar(127); send_len = 1
        case('delete')
            send_buf(1:4) = achar(27) // '[3~'
            send_len = 4
        case('esc')
            ! Escape closes the panel, but only from a bare prompt. With text
            ! typed it belongs to the shell (vi-mode, clearing a completion),
            ! and inside a full-screen program it always does.
            if (.not. terminal_panel_in_fullscreen_app(panel) .and. &
                terminal_panel_input_is_empty(panel)) then
                panel%visible = .false.
                panel%focused = .false.
                handled = .true.
                return
            end if
            send_buf(1:1) = achar(27); send_len = 1

        ! Arrow keys (application mode sends ESC O, normal sends ESC [)
        case('up')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OA'
            else
                send_buf(1:3) = achar(27) // '[A'
            end if
            send_len = 3
        case('down')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OB'
            else
                send_buf(1:3) = achar(27) // '[B'
            end if
            send_len = 3
        case('right')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OC'
            else
                send_buf(1:3) = achar(27) // '[C'
            end if
            send_len = 3
        case('left')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OD'
            else
                send_buf(1:3) = achar(27) // '[D'
            end if
            send_len = 3
        case('home')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OH'
            else
                send_buf(1:3) = achar(27) // '[H'
            end if
            send_len = 3
        case('end')
            if (c_associated(panel%grid_handle) .and. &
                c_grid_app_cursor(panel%grid_handle) /= 0) then
                send_buf(1:3) = achar(27) // 'OF'
            else
                send_buf(1:3) = achar(27) // '[F'
            end if
            send_len = 3
        case('pageup')
            send_buf(1:4) = achar(27) // '[5~'
            send_len = 4
        case('pagedown')
            send_buf(1:4) = achar(27) // '[6~'
            send_len = 4

        ! Ctrl combos
        case('ctrl-a')
            send_buf(1:1) = achar(1); send_len = 1
        case('ctrl-b')
            send_buf(1:1) = achar(2); send_len = 1
        case('ctrl-c')
            send_buf(1:1) = achar(3); send_len = 1
        case('ctrl-d')
            send_buf(1:1) = achar(4); send_len = 1
        case('ctrl-e')
            send_buf(1:1) = achar(5); send_len = 1
        case('ctrl-f')
            send_buf(1:1) = achar(6); send_len = 1
        case('ctrl-g')
            send_buf(1:1) = achar(7); send_len = 1
        case('ctrl-k')
            send_buf(1:1) = achar(11); send_len = 1
        case('ctrl-l')
            send_buf(1:1) = achar(12); send_len = 1
        case('ctrl-n')
            send_buf(1:1) = achar(14); send_len = 1
        case('ctrl-o')
            send_buf(1:1) = achar(15); send_len = 1
        case('ctrl-p')
            send_buf(1:1) = achar(16); send_len = 1
        case('ctrl-r')
            send_buf(1:1) = achar(18); send_len = 1
        case('ctrl-s')
            send_buf(1:1) = achar(19); send_len = 1
        case('ctrl-t')
            send_buf(1:1) = achar(20); send_len = 1
        case('ctrl-u')
            send_buf(1:1) = achar(21); send_len = 1
        case('ctrl-w')
            send_buf(1:1) = achar(23); send_len = 1
        case('ctrl-z')
            send_buf(1:1) = achar(26); send_len = 1
        case('ctrl-\\')
            send_buf(1:1) = achar(28); send_len = 1
        case('ctrl-]')
            send_buf(1:1) = achar(29); send_len = 1

        ! Missing ctrl keys used by readline/shells
        case('ctrl-j')
            send_buf(1:1) = achar(10); send_len = 1
        case('ctrl-v')
            send_buf(1:1) = achar(22); send_len = 1
        case('ctrl-x')
            send_buf(1:1) = achar(24); send_len = 1
        case('ctrl-y')
            send_buf(1:1) = achar(25); send_len = 1

        ! Alt+letter: ESC followed by the letter byte
        case('alt-a')
            send_buf(1:2) = achar(27) // 'a'; send_len = 2
        case('alt-b')
            send_buf(1:2) = achar(27) // 'b'; send_len = 2
        case('alt-c')
            send_buf(1:2) = achar(27) // 'c'; send_len = 2
        case('alt-d')
            send_buf(1:2) = achar(27) // 'd'; send_len = 2
        case('alt-e')
            send_buf(1:2) = achar(27) // 'e'; send_len = 2
        case('alt-f')
            send_buf(1:2) = achar(27) // 'f'; send_len = 2
        case('alt-g')
            send_buf(1:2) = achar(27) // 'g'; send_len = 2
        case('alt-h')
            send_buf(1:2) = achar(27) // 'h'; send_len = 2
        case('alt-i')
            send_buf(1:2) = achar(27) // 'i'; send_len = 2
        case('alt-j')
            send_buf(1:2) = achar(27) // 'j'; send_len = 2
        case('alt-k')
            send_buf(1:2) = achar(27) // 'k'; send_len = 2
        case('alt-l')
            send_buf(1:2) = achar(27) // 'l'; send_len = 2
        case('alt-m')
            send_buf(1:2) = achar(27) // 'm'; send_len = 2
        case('alt-n')
            send_buf(1:2) = achar(27) // 'n'; send_len = 2
        case('alt-o')
            send_buf(1:2) = achar(27) // 'o'; send_len = 2
        case('alt-p')
            send_buf(1:2) = achar(27) // 'p'; send_len = 2
        case('alt-q')
            send_buf(1:2) = achar(27) // 'q'; send_len = 2
        case('alt-r')
            send_buf(1:2) = achar(27) // 'r'; send_len = 2
        case('alt-s')
            send_buf(1:2) = achar(27) // 's'; send_len = 2
        case('alt-u')
            send_buf(1:2) = achar(27) // 'u'; send_len = 2
        case('alt-v')
            send_buf(1:2) = achar(27) // 'v'; send_len = 2
        case('alt-w')
            send_buf(1:2) = achar(27) // 'w'; send_len = 2
        case('alt-x')
            send_buf(1:2) = achar(27) // 'x'; send_len = 2
        case('alt-y')
            send_buf(1:2) = achar(27) // 'y'; send_len = 2
        case('alt-z')
            send_buf(1:2) = achar(27) // 'z'; send_len = 2

        ! Alt+punctuation
        case('alt-.')
            send_buf(1:2) = achar(27) // '.'; send_len = 2
        case('alt-backspace')
            send_buf(1:2) = achar(27) // achar(127); send_len = 2

        ! Alt+arrows (CSI modifier form: ESC [ 1 ; 3 A/B/C/D)
        case('alt-left')
            send_buf(1:6) = achar(27) // '[1;3D'; send_len = 6
        case('alt-right')
            send_buf(1:6) = achar(27) // '[1;3C'; send_len = 6
        case('alt-up')
            send_buf(1:6) = achar(27) // '[1;3A'; send_len = 6
        case('alt-down')
            send_buf(1:6) = achar(27) // '[1;3B'; send_len = 6

        ! Shift+tab (reverse completion)
        case('shift-tab')
            send_buf(1:3) = achar(27) // '[Z'; send_len = 3

        ! Insert key
        case('insert')
            send_buf(1:4) = achar(27) // '[2~'; send_len = 4

        ! Function keys: F1-F4 use SS3 encoding (ESC O P/Q/R/S),
        ! F5+ use vt220 CSI encoding (ESC [ N N ~)
        case('f1')
            send_buf(1:3) = achar(27) // 'OP'; send_len = 3
        case('f2')
            send_buf(1:3) = achar(27) // 'OQ'; send_len = 3
        case('f3')
            send_buf(1:3) = achar(27) // 'OR'; send_len = 3
        case('f4')
            send_buf(1:3) = achar(27) // 'OS'; send_len = 3
        case('f6')
            send_buf(1:5) = achar(27) // '[17~'; send_len = 5
        case('f7')
            send_buf(1:5) = achar(27) // '[18~'; send_len = 5
        case('f8')
            send_buf(1:5) = achar(27) // '[19~'; send_len = 5
        case('f9')
            send_buf(1:5) = achar(27) // '[20~'; send_len = 5
        case('f10')
            send_buf(1:5) = achar(27) // '[21~'; send_len = 5
        case('f11')
            send_buf(1:5) = achar(27) // '[23~'; send_len = 5
        case('f12')
            send_buf(1:5) = achar(27) // '[24~'; send_len = 5

        case(' ', 'space')
            send_buf(1:1) = ' '; send_len = 1

        case default
            ! Single printable character
            if (len_trim(key_str) == 1) then
                send_buf(1:1) = key_str(1:1)
                send_len = 1
            end if
        end select

        if (send_len > 0) then
            c_len = int(send_len, c_int)
            res = c_pty_write(panel%pty_handle, send_buf, c_len)
            ! Typing invalidates selection and snaps view to bottom
            panel%sel_active = .false.
            if (c_associated(panel%grid_handle)) then
                call c_grid_reset_view(panel%grid_handle)
            end if
            handled = .true.
        end if
    end function terminal_panel_handle_key

    ! Scroll the terminal view through scrollback history.
    ! delta>0 scrolls up (older lines), delta<0 scrolls down.
    subroutine terminal_panel_scroll(panel, delta)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: delta
        integer(c_int) :: c_delta
        if (.not. c_associated(panel%grid_handle)) return
        c_delta = int(delta, c_int)
        call c_grid_scroll_view(panel%grid_handle, c_delta)
    end subroutine terminal_panel_scroll

    ! Write pasted text to the PTY as a single chunk. When the
    ! child enabled bracketed paste (mode 2004) the text is wrapped
    ! in ESC[200~/ESC[201~ so the shell inserts it literally
    ! (highlighted, not executed) instead of running each line.
    subroutine terminal_panel_paste(panel, text)
        type(terminal_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: payload
        integer :: total, off
        integer(c_int) :: c_len, written

        if (.not. c_associated(panel%pty_handle)) return
        if (.not. panel%pty_alive) return
        if (len(text) == 0) return

        if (c_associated(panel%grid_handle) .and. &
            c_grid_bracketed_paste(panel%grid_handle) /= 0) then
            payload = ESC_CH // '[200~' // text // &
                      ESC_CH // '[201~'
        else
            payload = text
        end if

        ! Sending input invalidates any selection highlight
        panel%sel_active = .false.

        total = len(payload)
        off = 1
        do while (off <= total)
            c_len = int(total - off + 1, c_int)
            written = c_pty_write(panel%pty_handle, &
                payload(off:), c_len)
            if (written <= 0) exit
            off = off + int(written)
        end do
    end subroutine terminal_panel_paste

    !> Re-derive the panel's rows for a new screen size.
    !>
    !> This APPLIES the stored ratio rather than recomputing from a constant,
    !> which is the whole of the "keep its proportion" behaviour: the window
    !> poll already calls this on every size change, so a panel taking a third
    !> goes on taking a third for free. It used to recompute from a fixed
    !> percentage, which silently threw away any chosen height the next time
    !> the window was touched -- and toggle_terminal_panel calls this too, so
    !> that included merely opening the panel.
    subroutine terminal_panel_resize(panel, screen_rows, &
                                     screen_cols)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_rows, screen_cols
        integer(c_int) :: c_rows, c_cols, res

        if (.not. panel%visible) return

        panel%height = height_for(panel%height_permille, screen_rows)
        panel%pty_rows = panel%height - 1
        panel%pty_cols = screen_cols
        c_rows = int(panel%pty_rows, c_int)
        c_cols = int(panel%pty_cols, c_int)

        if (c_associated(panel%grid_handle)) then
            call c_grid_resize(panel%grid_handle, c_rows, c_cols)
        end if
        if (c_associated(panel%pty_handle)) then
            res = c_pty_resize(panel%pty_handle, c_rows, c_cols)
        end if
    end subroutine terminal_panel_resize

    ! True if a screen row (1-based) falls inside the panel area
    function terminal_panel_in_region(panel, screen_row) &
        result(inside)
        type(terminal_panel_t), intent(in) :: panel
        integer, intent(in) :: screen_row
        logical :: inside
        inside = .false.
        if (.not. panel%visible) return
        if (panel%screen_start_row <= 0) return
        inside = (screen_row >= panel%screen_start_row)
    end function terminal_panel_in_region

    ! Normalize the selection so (sr,sc) precedes (er,ec)
    subroutine selection_bounds(panel, sr, sc, er, ec)
        type(terminal_panel_t), intent(in) :: panel
        integer, intent(out) :: sr, sc, er, ec
        if (panel%sel_anchor_row < panel%sel_end_row .or. &
            (panel%sel_anchor_row == panel%sel_end_row .and. &
             panel%sel_anchor_col <= panel%sel_end_col)) then
            sr = panel%sel_anchor_row
            sc = panel%sel_anchor_col
            er = panel%sel_end_row
            ec = panel%sel_end_col
        else
            sr = panel%sel_end_row
            sc = panel%sel_end_col
            er = panel%sel_anchor_row
            ec = panel%sel_anchor_col
        end if
    end subroutine selection_bounds

    ! True if grid cell (r,c) is within the active selection
    function cell_selected(panel, r, c) result(sel)
        type(terminal_panel_t), intent(in) :: panel
        integer, intent(in) :: r, c
        logical :: sel
        integer :: sr, sc, er, ec
        sel = .false.
        if (.not. panel%sel_active) return
        call selection_bounds(panel, sr, sc, er, ec)
        if (r < sr .or. r > er) return
        if (sr == er) then
            sel = (c >= sc .and. c <= ec)
        else if (r == sr) then
            sel = (c >= sc)
        else if (r == er) then
            sel = (c <= ec)
        else
            sel = .true.
        end if
    end function cell_selected

    ! Extract the selected grid text, trimming trailing spaces
    ! per row and joining rows with newlines.
    subroutine extract_selection(panel, text)
        type(terminal_panel_t), intent(inout) :: panel
        character(len=:), allocatable, intent(out) :: text
        integer :: sr, sc, er, ec, r, c0, c1, c, bi
        integer(c_int) :: c_row, c_col, c_fg, c_bg, c_attr, c_nbytes
        character(kind=c_char) :: cell_buf(4)
        character(len=:), allocatable :: line

        text = ''
        if (.not. panel%sel_active) return
        if (.not. c_associated(panel%grid_handle)) return
        call selection_bounds(panel, sr, sc, er, ec)

        do r = sr, er
            if (r == sr) then
                c0 = sc
            else
                c0 = 0
            end if
            if (r == er) then
                c1 = ec
            else
                c1 = panel%pty_cols - 1
            end if

            line = ''
            do c = c0, c1
                c_row = int(r, c_int)
                c_col = int(c, c_int)
                call c_grid_get_view_cell(panel%grid_handle, &
                    c_row, c_col, cell_buf, c_nbytes, c_fg, c_bg, c_attr)
                ! Copied text is bytes, not cells: a three-byte icon must come
                ! out as three bytes, and a double-width continuation as none.
                do bi = 1, int(c_nbytes)
                    line = line // cell_buf(bi)
                end do
            end do

            ! Trim trailing spaces from this row's slice
            if (r < er) then
                text = text // trim_trailing(line) // achar(10)
            else
                text = text // trim_trailing(line)
            end if
        end do
    end subroutine extract_selection

    ! Strip trailing spaces but keep at least an empty string
    function trim_trailing(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: n
        n = len(s)
        do while (n > 0)
            if (s(n:n) /= ' ') exit
            n = n - 1
        end do
        out = s(1:n)
    end function trim_trailing

    ! Handle a mouse event over the terminal panel. srow/scol are
    ! 1-based screen coordinates. Sets focus, tracks selection, and

    !> True while the left button is down on a drag that started in the panel.
    !>
    !> The router only forwards mouse events whose row falls inside the panel,
    !> so a drag that ended above it -- which is what selecting the last few
    !> lines looks like -- never delivered its release, and the copy that fires
    !> on release simply never happened. The selection stayed on screen, so it
    !> looked like copy was broken rather than unreached.
    function terminal_panel_is_dragging(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        res = panel%visible .and. panel%sel_dragging
    end function terminal_panel_is_dragging

    function terminal_panel_has_selection(panel) result(res)
        type(terminal_panel_t), intent(in) :: panel
        logical :: res
        res = panel%visible .and. panel%sel_active
    end function terminal_panel_has_selection

    !> Copy the current selection. Returns how many characters were taken, so
    !> the caller can say so -- a silent copy is indistinguishable from one
    !> that did not happen.
    subroutine terminal_panel_copy_selection(panel, n_copied)
        use clipboard_module, only: copy_to_clipboard
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(out) :: n_copied
        character(len=:), allocatable :: sel_text

        n_copied = 0
        if (.not. panel%sel_active) return
        call extract_selection(panel, sel_text)
        if (len(sel_text) == 0) return
        call copy_to_clipboard(sel_text)
        n_copied = len(sel_text)
    end subroutine terminal_panel_copy_selection

    ! copies to the clipboard on release. Returns copied=.true. if
    ! text was placed on the clipboard.
    subroutine terminal_panel_handle_mouse(panel, event_type, &
        button, srow, scol, copied)
        use clipboard_module, only: copy_to_clipboard
        type(terminal_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: event_type
        integer, intent(in) :: button, srow, scol
        logical, intent(out) :: copied
        integer :: gr, gc
        character(len=:), allocatable :: sel_text

        copied = .false.
        if (.not. panel%visible) return

        ! Map screen coords to grid coords (0-based), clamped
        gr = srow - (panel%screen_start_row + 1)
        gc = scol - 1
        if (gr < 0) gr = 0
        if (gr > panel%pty_rows - 1) gr = panel%pty_rows - 1
        if (gc < 0) gc = 0
        if (gc > panel%pty_cols - 1) gc = panel%pty_cols - 1

        ! Left button: plain click is code 0, a left-button drag has
        ! the SGR motion bit set (code 32) — both have low bits 0.
        select case(trim(event_type))
        case('mouse-click')
            if (iand(button, 3) == 0) then
                ! Focus terminal and start a fresh selection anchor
                panel%focused = .true.
                panel%sel_dragging = .true.
                panel%sel_active = .false.
                panel%sel_anchor_row = gr
                panel%sel_anchor_col = gc
                panel%sel_end_row = gr
                panel%sel_end_col = gc
            end if
        case('mouse-drag')
            if (iand(button, 3) == 0) then
                panel%sel_end_row = gr
                panel%sel_end_col = gc
                if (gr /= panel%sel_anchor_row .or. &
                    gc /= panel%sel_anchor_col) then
                    panel%sel_active = .true.
                end if
            end if
        case('mouse-release')
            panel%sel_dragging = .false.
            if (panel%sel_active) then
                call extract_selection(panel, sel_text)
                if (len(sel_text) > 0) then
                    call copy_to_clipboard(sel_text)
                    copied = .true.
                end if
            end if
        end select
    end subroutine terminal_panel_handle_mouse

end module terminal_panel_module
