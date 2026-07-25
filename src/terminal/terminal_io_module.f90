module terminal_io_module
    use iso_c_binding
    use iso_fortran_env, only: output_unit, input_unit
    use raw_mode_module, only: raw_enable_raw_mode => enable_raw_mode, &
                               raw_disable_raw_mode => disable_raw_mode, &
                               raw_input_available => input_available, &
                               raw_read_char_timeout => read_char_timeout, &
                               raw_read_char_escape => read_char_escape, &
                               raw_input_available_count => input_available_count, &
                               raw_get_terminal_size => get_terminal_size
    implicit none
    private

    public :: terminal_init, terminal_cleanup, terminal_clear_screen
    public :: terminal_move_cursor, terminal_hide_cursor, terminal_show_cursor
    public :: terminal_get_size, terminal_enable_raw_mode, terminal_disable_raw_mode
    public :: terminal_write, terminal_flush, terminal_enable_mouse, terminal_disable_mouse
    public :: terminal_set_motion_tracking
    public :: terminal_input_available, terminal_read_char
    public :: terminal_read_char_escape, terminal_input_available_count
    public :: terminal_consume_escape, terminal_consume_csi
    public :: ESC_STANDALONE, ESC_MOUSE, ESC_OTHER

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CSI = ESC // '['

    ! Results of terminal_consume_escape
    integer, parameter :: ESC_STANDALONE = 0   ! a real ESC keypress
    integer, parameter :: ESC_MOUSE = 1        ! a mouse report, now swallowed
    integer, parameter :: ESC_OTHER = 2        ! some other sequence, swallowed

    ! C output buffer interface
    interface
        subroutine c_term_buf_write(data, len) bind(C, name='term_buf_write')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: data(*)
            integer(c_int), value, intent(in) :: len
        end subroutine c_term_buf_write

        subroutine c_term_buf_flush() bind(C, name='term_buf_flush')
        end subroutine c_term_buf_flush
    end interface

contains

    subroutine terminal_init()
        call terminal_enable_raw_mode()
        ! Enter alternate screen buffer so shell history stays intact
        call buf_write_str(ESC // '[?1049h')
        call c_term_buf_flush()
        call terminal_enable_mouse()
        ! Bracketed paste: outer terminal wraps pasted text in
        ! ESC[200~/ESC[201~ so it arrives as one event, not a
        ! keystroke stream that could auto-execute.
        call buf_write_str(ESC // '[?2004h')
        ! Kitty keyboard protocol, flag 1 (disambiguate escape codes). Ctrl+/
        ! and Ctrl+Shift+/ collapse onto the same control byte (0x1F) in the
        ! legacy encoding, so ctrl-/ for comments and ctrl-? for help can only
        ! coexist if the terminal reports modifiers separately. Pushed, not
        ! set, so terminal_cleanup restores whatever was there before;
        ! terminals that do not implement it ignore the sequence and keep the
        ! legacy path (where f1 is the help fallback).
        call buf_write_str(ESC // '[>1u')
        call terminal_clear_screen()
        call terminal_hide_cursor()
    end subroutine terminal_init

    subroutine terminal_cleanup()
        call terminal_show_cursor()
        call terminal_disable_mouse()
        ! Pop the keyboard-protocol flags pushed in terminal_init
        call buf_write_str(ESC // '[<u')
        ! Disable bracketed paste
        call buf_write_str(ESC // '[?2004l')
        ! Leave alternate screen buffer to restore original shell view
        call buf_write_str(ESC // '[?1049l')
        call c_term_buf_flush()
        call terminal_disable_raw_mode()
    end subroutine terminal_cleanup

    subroutine terminal_clear_screen()
        call buf_write_str(CSI // '2J')
        call buf_write_str(CSI // 'H')
        call c_term_buf_flush()
    end subroutine terminal_clear_screen

    subroutine terminal_move_cursor(row, col)
        integer, intent(in) :: row, col
        character(len=32) :: seq
        write(seq, '(a,i0,a,i0,a)') CSI, row, ';', col, 'H'
        call buf_write_str(trim(seq))
    end subroutine terminal_move_cursor

    subroutine terminal_hide_cursor()
        call buf_write_str(CSI // '?25l')
        call c_term_buf_flush()
    end subroutine terminal_hide_cursor

    subroutine terminal_show_cursor()
        call buf_write_str(CSI // '?25h')
        call c_term_buf_flush()
    end subroutine terminal_show_cursor

    subroutine terminal_get_size(rows, cols)
        integer, intent(out) :: rows, cols
        call raw_get_terminal_size(rows, cols)
    end subroutine terminal_get_size

    subroutine terminal_enable_raw_mode()
        logical :: success
        success = raw_enable_raw_mode()
        if (.not. success) then
            call buf_write_str(ESC // '[12l')
            call c_term_buf_flush()
        end if
    end subroutine terminal_enable_raw_mode

    subroutine terminal_disable_raw_mode()
        logical :: success
        success = raw_disable_raw_mode()
        if (.not. success) then
            call buf_write_str(ESC // '[12h')
            call c_term_buf_flush()
        end if
    end subroutine terminal_disable_raw_mode

    function terminal_input_available() result(available)
        logical :: available
        available = raw_input_available()
    end function terminal_input_available

    function terminal_read_char() result(ch)
        integer :: ch
        ch = raw_read_char_timeout()
    end function terminal_read_char

    function terminal_read_char_escape() result(ch)
        integer :: ch
        ch = raw_read_char_escape()
    end function terminal_read_char_escape

    function terminal_input_available_count() result(count)
        integer :: count
        count = raw_input_available_count()
    end function terminal_input_available_count

    ! Call this the moment a raw-byte input loop reads byte 27, to find out
    ! whether it was a real ESC keypress or the start of a longer sequence.
    !
    ! A mouse report (SGR: ESC [ < b ; c ; r M|m) arrives as a burst of
    ! ordinary bytes. A prompt loop that treats 27 as "cancel" and exits
    ! leaves the remaining bytes in the tty, and the main loop then reads
    ! them as printable keys and types them into the document -- moving the
    ! mouse while Ctrl-G was open used to write "[<0;30;10M" into the file.
    ! Returning ESC_MOUSE lets those loops ignore the event and stay open.
    !
    ! Continuation bytes are read with the 5ms reader, not the 50ms one: the
    ! whole sequence arrives in a single burst, so a longer wait would only
    ! stall a genuine lone ESC.
    function terminal_consume_escape() result(kind)
        integer :: kind
        integer :: ch

        ch = terminal_read_char_escape()

        ! Nothing followed, or a second ESC: a real keypress either way
        if (ch == -1 .or. ch == 27) then
            kind = ESC_STANDALONE
            return
        end if

        if (ch /= iachar('[') .and. ch /= iachar('O')) then
            ! ESC + a single byte, e.g. an alt-chord. Consumed, not a cancel.
            kind = ESC_OTHER
            return
        end if

        kind = terminal_consume_csi(terminal_read_char_escape())
    end function terminal_consume_escape

    ! The tail of terminal_consume_escape, for loops that have already read
    ! ESC and the '['/'O' introducer and so cannot call it. first_byte is the
    ! byte immediately after the introducer.
    function terminal_consume_csi(first_byte) result(kind)
        integer, intent(in) :: first_byte
        integer :: kind
        integer :: ch, i

        ch = first_byte

        ! SGR mouse (mode 1006), what fac asks for: runs to 'M' or 'm'
        if (ch == iachar('<')) then
            do
                ch = terminal_read_char_escape()
                if (ch == -1) exit
                if (ch == iachar('M') .or. ch == iachar('m')) exit
            end do
            kind = ESC_MOUSE
            return
        end if

        ! Legacy X10 mouse, in case a terminal ignored the 1006 request:
        ! ESC [ M then exactly three bytes, which are binary and so cannot
        ! be found by scanning for a final byte.
        if (ch == iachar('M')) then
            do i = 1, 3
                if (terminal_read_char_escape() == -1) exit
            end do
            kind = ESC_MOUSE
            return
        end if

        ! Any other CSI/SS3 sequence: parameter bytes, then a final byte in
        ! 0x40-0x7E. ch already holds the first byte after the introducer.
        do
            if (ch == -1) exit
            if (ch >= 64 .and. ch <= 126) exit
            ch = terminal_read_char_escape()
        end do
        kind = ESC_OTHER
    end function terminal_consume_csi

    subroutine terminal_write(text)
        character(len=*), intent(in) :: text
        call buf_write_str(text)
    end subroutine terminal_write

    subroutine terminal_flush()
        call c_term_buf_flush()
    end subroutine terminal_flush

    subroutine terminal_enable_mouse()
        call buf_write_str(CSI // '?1000h')
        call buf_write_str(CSI // '?1002h')
        call buf_write_str(CSI // '?1006h')
        ! Disable alternate scroll: in the alternate screen some
        ! terminals turn the wheel into arrow keys, which would feed
        ! shell history instead of our scrollback. Force real mouse
        ! wheel events (button 64/65) instead.
        call buf_write_str(CSI // '?1007l')
        call c_term_buf_flush()
    end subroutine terminal_enable_mouse

    !> Turn any-motion reporting (mode 1003) on or off.
    !>
    !> The editor normally runs in mode 1002, where the terminal reports
    !> motion only while a button is held. Mode 1003 reports every pointer
    !> movement, which is what a hover highlight needs -- and is why it is not
    !> left on: it would put an event in the input stream for every pixel of
    !> mouse travel for the whole session. The context menu turns it on while
    !> it is open and off again when it closes.
    subroutine terminal_set_motion_tracking(on)
        logical, intent(in) :: on

        if (on) then
            call buf_write_str(CSI // '?1003h')
        else
            call buf_write_str(CSI // '?1003l')
            ! 1003 and 1002 are separate modes; re-assert 1002 so dragging
            ! still reports motion after the menu has gone.
            call buf_write_str(CSI // '?1002h')
        end if
        call c_term_buf_flush()
    end subroutine terminal_set_motion_tracking

    subroutine terminal_disable_mouse()
        call buf_write_str(CSI // '?1006l')
        ! 1003 before 1002: the context menu turns any-motion reporting on
        ! while it is open, and quitting with one up would otherwise leave the
        ! shell receiving an event for every pointer movement -- unusable over
        ! a slow link, and it outlives the editor.
        call buf_write_str(CSI // '?1003l')
        call buf_write_str(CSI // '?1002l')
        call buf_write_str(CSI // '?1000l')
        ! Restore alternate scroll (the common terminal default) so
        ! the wheel keeps working in pagers like less after we exit.
        call buf_write_str(CSI // '?1007h')
        call c_term_buf_flush()
    end subroutine terminal_disable_mouse

    ! Internal: write a Fortran string to the C output buffer
    subroutine buf_write_str(str)
        character(len=*), intent(in) :: str
        call c_term_buf_write(str, int(len(str), c_int))
    end subroutine buf_write_str

end module terminal_io_module
