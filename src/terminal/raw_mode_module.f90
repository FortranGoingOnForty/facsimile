module raw_mode_module
    use iso_c_binding
    implicit none
    private

    public :: enable_raw_mode, disable_raw_mode, input_available, read_char_timeout

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

        function c_read_char_timeout() bind(C, name="read_char_timeout")
            import :: c_int
            integer(c_int) :: c_read_char_timeout
        end function c_read_char_timeout
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

    function read_char_timeout() result(ch)
        integer :: ch
        integer(c_int) :: result

        result = c_read_char_timeout()
        ch = result
    end function read_char_timeout

end module raw_mode_module