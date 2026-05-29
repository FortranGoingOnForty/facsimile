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
    public :: terminal_input_available, terminal_read_char
    public :: terminal_read_char_escape, terminal_input_available_count

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CSI = ESC // '['

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
        call terminal_clear_screen()
        call terminal_hide_cursor()
    end subroutine terminal_init

    subroutine terminal_cleanup()
        call terminal_show_cursor()
        call terminal_disable_mouse()
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
        call c_term_buf_flush()
    end subroutine terminal_enable_mouse

    subroutine terminal_disable_mouse()
        call buf_write_str(CSI // '?1006l')
        call buf_write_str(CSI // '?1002l')
        call buf_write_str(CSI // '?1000l')
        call c_term_buf_flush()
    end subroutine terminal_disable_mouse

    ! Internal: write a Fortran string to the C output buffer
    subroutine buf_write_str(str)
        character(len=*), intent(in) :: str
        call c_term_buf_write(str, int(len(str), c_int))
    end subroutine buf_write_str

end module terminal_io_module
