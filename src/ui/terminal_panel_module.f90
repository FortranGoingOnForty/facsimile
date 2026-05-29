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
    public :: terminal_panel_poll, terminal_panel_render
    public :: terminal_panel_handle_key
    public :: terminal_panel_resize
    public :: get_terminal_panel_height

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

        subroutine c_grid_get_cell(handle, row, col, &
                                    ch, fg, bg, attr) &
            bind(C, name='vt100_grid_get_cell_f')
            import :: c_ptr, c_char, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(in) :: row, col
            character(kind=c_char), intent(out) :: ch
            integer(c_int), intent(out) :: fg, bg, attr
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
    integer, parameter :: HEIGHT_PERCENT = 30
    integer, parameter :: READ_BUF_SIZE = 8192

    character(len=1), parameter :: ESC_CH = achar(27)

    type :: terminal_panel_t
        logical :: visible = .false.
        logical :: focused = .false.
        integer :: height = 0
        type(c_ptr) :: pty_handle = c_null_ptr
        type(c_ptr) :: grid_handle = c_null_ptr
        integer :: pty_rows = 0
        integer :: pty_cols = 0
        logical :: pty_alive = .false.
        logical :: has_new_output = .false.
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

        ! Calculate height
        panel%height = max(MIN_HEIGHT, &
            screen_rows * HEIGHT_PERCENT / 100)
        ! Cap at 80% of screen
        if (panel%height > screen_rows * 80 / 100) then
            panel%height = screen_rows * 80 / 100
        end if

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
    end subroutine toggle_terminal_panel

    function is_terminal_panel_visible(panel) result(vis)
        type(terminal_panel_t), intent(in) :: panel
        logical :: vis
        vis = panel%visible
    end function is_terminal_panel_visible

    function get_terminal_panel_height(panel) result(h)
        type(terminal_panel_t), intent(in) :: panel
        integer :: h
        if (panel%visible) then
            h = panel%height
        else
            h = 0
        end if
    end function get_terminal_panel_height

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
        character(len=1) :: ch
        integer :: last_fg, last_bg, last_attr
        integer :: cursor_r, cursor_c
        integer(c_int) :: cc_row, cc_col
        character(len=32) :: ansi_seq

        if (.not. panel%visible) return
        if (.not. c_associated(panel%grid_handle)) return

        grid_rows = panel%pty_rows
        grid_cols = min(panel%pty_cols, cols)

        ! Draw separator bar
        call terminal_move_cursor(start_row, 1)
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
                call c_grid_get_cell(panel%grid_handle, &
                    c_row, c_col, ch, c_fg, c_bg, c_attr)

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

                ! Render cursor position with reverse video
                if (panel%focused .and. r == cursor_r .and. &
                    c == cursor_c) then
                    call terminal_write(ESC_CH // '[7m')
                    call terminal_write(ch)
                    call terminal_write(ESC_CH // '[27m')
                else
                    call terminal_write(ch)
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
            handled = .true.
        end if
    end function terminal_panel_handle_key

    ! Resize the terminal panel
    subroutine terminal_panel_resize(panel, screen_rows, &
                                     screen_cols)
        type(terminal_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_rows, screen_cols
        integer(c_int) :: c_rows, c_cols, res

        if (.not. panel%visible) return

        panel%height = max(MIN_HEIGHT, &
            screen_rows * HEIGHT_PERCENT / 100)
        if (panel%height > screen_rows * 80 / 100) then
            panel%height = screen_rows * 80 / 100
        end if

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

end module terminal_panel_module
