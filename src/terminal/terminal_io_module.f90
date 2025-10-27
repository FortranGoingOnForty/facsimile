module terminal_io_module
    use iso_c_binding
    use iso_fortran_env, only: output_unit, input_unit
    implicit none
    private

    public :: terminal_init, terminal_cleanup, terminal_clear_screen
    public :: terminal_move_cursor, terminal_hide_cursor, terminal_show_cursor
    public :: terminal_get_size, terminal_enable_raw_mode, terminal_disable_raw_mode
    public :: terminal_write

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: CSI = ESC // '['

    ! Terminal settings (platform specific - using C bindings)
    interface
        function tcgetattr(fd, termios_ptr) bind(C, name="tcgetattr")
            use iso_c_binding
            integer(c_int), value :: fd
            type(c_ptr), value :: termios_ptr
            integer(c_int) :: tcgetattr
        end function tcgetattr

        function tcsetattr(fd, optional_actions, termios_ptr) bind(C, name="tcsetattr")
            use iso_c_binding
            integer(c_int), value :: fd
            integer(c_int), value :: optional_actions
            type(c_ptr), value :: termios_ptr
            integer(c_int) :: tcsetattr
        end function tcsetattr
    end interface

contains

    subroutine terminal_init()
        call terminal_enable_raw_mode()
        call terminal_clear_screen()
        call terminal_hide_cursor()
    end subroutine terminal_init

    subroutine terminal_cleanup()
        call terminal_show_cursor()
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
        ! Platform-specific implementation needed
        ! For now, this is a stub - would need proper termios handling
    end subroutine terminal_enable_raw_mode

    subroutine terminal_disable_raw_mode()
        ! Platform-specific implementation needed
        ! For now, this is a stub - would need proper termios handling
    end subroutine terminal_disable_raw_mode

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

end module terminal_io_module