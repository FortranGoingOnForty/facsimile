module raw_mode_module
    use iso_c_binding
    implicit none
    private

    public :: enable_raw_mode, disable_raw_mode, input_available, read_char_timeout
    public :: read_char_escape, input_available_count, get_terminal_size
    public :: take_terminal_resize_event

    ! C function interfaces
    interface
        function c_enable_raw_mode() bind(C, name="enable_raw_mode")
            import :: c_int
            integer(c_int) :: c_enable_raw_mode
        end function c_enable_raw_mode

        function c_disable_raw_mode() bind(C, name="disable_raw_mode")
            import :: c_int
            integer(c_int) :: c_disable_raw_mode
        end function c_disable_raw_mode

        function c_input_available() bind(C, name="input_available")
            import :: c_int
            integer(c_int) :: c_input_available
        end function c_input_available

        function c_input_available_count() bind(C, name="input_available_count")
            import :: c_int
            integer(c_int) :: c_input_available_count
        end function c_input_available_count

        function c_read_char_timeout() bind(C, name="read_char_timeout")
            import :: c_int
            integer(c_int) :: c_read_char_timeout
        end function c_read_char_timeout

        function c_read_char_escape() bind(C, name="read_char_escape")
            import :: c_int
            integer(c_int) :: c_read_char_escape
        end function c_read_char_escape

        subroutine c_get_terminal_size(rows, cols) bind(C, name="get_terminal_size")
            import :: c_int
            integer(c_int), intent(out) :: rows, cols
        end subroutine c_get_terminal_size

        function c_take_terminal_resize_event() bind(C, name="take_terminal_resize_event") result(pending)
            import :: c_int
            integer(c_int) :: pending
        end function c_take_terminal_resize_event
    end interface

contains

    function enable_raw_mode() result(success)
        logical :: success
        integer(c_int) :: result

        result = c_enable_raw_mode()
        success = (result == 0)
    end function enable_raw_mode

    function disable_raw_mode() result(success)
        logical :: success
        integer(c_int) :: result

        result = c_disable_raw_mode()
        success = (result == 0)
    end function disable_raw_mode

    function input_available() result(available)
        logical :: available
        integer(c_int) :: result

        result = c_input_available()
        available = (result > 0)
    end function input_available

    function input_available_count() result(count)
        integer :: count
        integer(c_int) :: result

        result = c_input_available_count()
        count = result
    end function input_available_count

    function read_char_timeout() result(ch)
        integer :: ch
        integer(c_int) :: result

        result = c_read_char_timeout()
        ch = result
    end function read_char_timeout

    ! Fast read for escape sequences (5ms timeout instead of 50ms)
    function read_char_escape() result(ch)
        integer :: ch
        integer(c_int) :: result

        result = c_read_char_escape()
        ch = result
    end function read_char_escape

    ! Get terminal size using ioctl (no escape sequences)
    subroutine get_terminal_size(rows, cols)
        integer, intent(out) :: rows, cols
        integer(c_int) :: c_rows, c_cols

        call c_get_terminal_size(c_rows, c_cols)
        rows = c_rows
        cols = c_cols
    end subroutine get_terminal_size

    function take_terminal_resize_event() result(pending)
        logical :: pending

        pending = c_take_terminal_resize_event() /= 0
    end function take_terminal_resize_event

end module raw_mode_module
