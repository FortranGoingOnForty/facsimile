module terminal_io_module
    use iso_c_binding
    use iso_fortran_env, only: output_unit, input_unit
    use raw_mode_module, only: raw_enable_raw_mode => enable_raw_mode, &
                               raw_disable_raw_mode => disable_raw_mode, &
                               raw_input_available => input_available, &
                               raw_read_char_timeout => read_char_timeout, &
                               raw_read_char_escape => read_char_escape, &
                               raw_input_available_count => input_available_count
    implicit none
    private

    public :: terminal_init, terminal_cleanup, terminal_clear_screen
    public :: terminal_move_cursor, terminal_hide_cursor, terminal_show_cursor
    public :: terminal_get_size, terminal_enable_raw_mode, terminal_disable_raw_mode
    public :: terminal_write, terminal_enable_mouse, terminal_disable_mouse
    public :: terminal_input_available, terminal_read_char
    public :: terminal_read_char_escape, terminal_input_available_count

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CSI = ESC // '['

contains

    subroutine terminal_init()
        call terminal_enable_raw_mode()
        call terminal_enable_mouse()
        call terminal_clear_screen()
        call terminal_hide_cursor()
    end subroutine terminal_init

    subroutine terminal_cleanup()
        call terminal_show_cursor()
        call terminal_disable_mouse()
        call terminal_clear_screen()
        call terminal_move_cursor(1, 1)
        call terminal_disable_raw_mode()
    end subroutine terminal_cleanup

    subroutine terminal_clear_screen()
        write(output_unit, '(a)', advance='no') CSI // '2J'
        write(output_unit, '(a)', advance='no') CSI // 'H'
        flush(output_unit)
    end subroutine terminal_clear_screen

    subroutine terminal_move_cursor(row, col)
        integer, intent(in) :: row, col
        character(len=32) :: seq

        write(seq, '(a,i0,a,i0,a)') CSI, row, ';', col, 'H'
        write(output_unit, '(a)', advance='no') trim(seq)
        flush(output_unit)
    end subroutine terminal_move_cursor

    subroutine terminal_hide_cursor()
        write(output_unit, '(a)', advance='no') CSI // '?25l'
        flush(output_unit)
    end subroutine terminal_hide_cursor

    subroutine terminal_show_cursor()
        write(output_unit, '(a)', advance='no') CSI // '?25h'
        flush(output_unit)
    end subroutine terminal_show_cursor

    subroutine terminal_get_size(rows, cols)
        integer, intent(out) :: rows, cols
        character(len=32) :: response
        integer :: ios, r, c

        ! Request cursor position after moving to bottom-right
        write(output_unit, '(a)', advance='no') CSI // '999;999H'
        write(output_unit, '(a)', advance='no') CSI // '6n'
        flush(output_unit)

        ! Read response (format: ESC[row;colR)
        read(input_unit, '(a)', iostat=ios) response

        ! Parse response
        if (ios == 0 .and. response(1:2) == ESC // '[') then
            ! Parse the response manually to handle variable width
            call parse_cursor_response(response(3:), r, c)
            rows = r
            cols = c
        else
            ! Fallback to default
            rows = 24
            cols = 80
        end if
    end subroutine terminal_get_size

    subroutine terminal_enable_raw_mode()
        logical :: success

        success = raw_enable_raw_mode()
        if (.not. success) then
            ! Fallback - just disable echo
            write(output_unit, '(a)', advance='no') ESC // '[12l'
            flush(output_unit)
        end if
    end subroutine terminal_enable_raw_mode

    subroutine terminal_disable_raw_mode()
        logical :: success

        success = raw_disable_raw_mode()
        if (.not. success) then
            ! Fallback - re-enable echo
            write(output_unit, '(a)', advance='no') ESC // '[12h'
            flush(output_unit)
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

    ! Fast read for escape sequences (5ms timeout)
    function terminal_read_char_escape() result(ch)
        integer :: ch
        ch = raw_read_char_escape()
    end function terminal_read_char_escape

    ! Get count of available input bytes
    function terminal_input_available_count() result(count)
        integer :: count
        count = raw_input_available_count()
    end function terminal_input_available_count

    subroutine terminal_write(text)
        character(len=*), intent(in) :: text
        write(output_unit, '(a)', advance='no') text
        flush(output_unit)
    end subroutine terminal_write

    subroutine parse_cursor_response(response, row, col)
        character(len=*), intent(in) :: response
        integer, intent(out) :: row, col
        integer :: semicolon_pos, r_pos, ios

        row = 24
        col = 80

        ! Find semicolon position
        semicolon_pos = index(response, ';')
        if (semicolon_pos == 0) return

        ! Find 'R' position
        r_pos = index(response, 'R')
        if (r_pos == 0) return

        ! Parse row and column
        read(response(1:semicolon_pos-1), '(i10)', iostat=ios) row
        if (ios /= 0) return

        read(response(semicolon_pos+1:r_pos-1), '(i10)', iostat=ios) col
        if (ios /= 0) return
    end subroutine parse_cursor_response

    subroutine terminal_enable_mouse()
        ! Enable mouse tracking modes:
        ! 1000 - Enable normal mouse tracking
        ! 1002 - Enable button-motion tracking (for drag)
        ! 1006 - Enable SGR extended mode (for large terminals)
        write(output_unit, '(a)', advance='no') CSI // '?1000h'
        write(output_unit, '(a)', advance='no') CSI // '?1002h'
        write(output_unit, '(a)', advance='no') CSI // '?1006h'
        flush(output_unit)
    end subroutine terminal_enable_mouse

    subroutine terminal_disable_mouse()
        ! Disable mouse tracking modes
        write(output_unit, '(a)', advance='no') CSI // '?1006l'
        write(output_unit, '(a)', advance='no') CSI // '?1002l'
        write(output_unit, '(a)', advance='no') CSI // '?1000l'
        flush(output_unit)
    end subroutine terminal_disable_mouse

end module terminal_io_module