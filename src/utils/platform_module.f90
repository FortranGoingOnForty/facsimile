module platform_module
    use iso_c_binding
    implicit none
    private

    public :: get_temp_dir, get_home_dir, get_path_separator, is_windows
    public :: get_config_dir, get_cwd
    public :: platform_copy_to_clipboard, platform_paste_from_clipboard

    interface
        subroutine get_temp_dir_c(buffer, buffer_len, result_len) bind(C, name='get_temp_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        subroutine get_home_dir_c(buffer, buffer_len, result_len) bind(C, name='get_home_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        function get_path_separator_c() bind(C, name='get_path_separator_f') result(sep)
            import :: c_char
            character(kind=c_char) :: sep
        end function

        function is_windows_c() bind(C, name='is_windows_f') result(res)
            import :: c_int
            integer(c_int) :: res
        end function

        subroutine get_config_dir_c(buffer, buffer_len, result_len) bind(C, name='get_config_dir_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        subroutine get_cwd_c(buffer, buffer_len, result_len) bind(C, name='get_cwd_f')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
        end subroutine

        function copy_to_clipboard_c(text, text_len) bind(C, name='copy_to_clipboard_f') result(res)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), value :: text_len
            integer(c_int) :: res
        end function

        function paste_from_clipboard_c(buffer, buffer_len, result_len) bind(C, name='paste_from_clipboard_f') result(res)
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
            integer(c_int), intent(out) :: result_len
            integer(c_int) :: res
        end function
    end interface

contains

    function get_temp_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_temp_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_home_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_home_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_path_separator() result(sep)
        character(len=1) :: sep
        sep = get_path_separator_c()
    end function

    function is_windows() result(res)
        logical :: res
        res = (is_windows_c() /= 0)
    end function

    function get_config_dir() result(path)
        character(len=:), allocatable :: path
        character(len=512) :: buffer
        integer(c_int) :: result_len

        call get_config_dir_c(buffer, 512_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function get_cwd() result(path)
        character(len=:), allocatable :: path
        character(len=1024) :: buffer
        integer(c_int) :: result_len

        call get_cwd_c(buffer, 1024_c_int, result_len)
        path = trim(buffer(1:result_len))
    end function

    function platform_copy_to_clipboard(text) result(success)
        character(len=*), intent(in) :: text
        logical :: success
        integer(c_int) :: res

        res = copy_to_clipboard_c(text, int(len_trim(text), c_int))
        success = (res /= 0)
    end function

    function platform_paste_from_clipboard() result(text)
        character(len=:), allocatable :: text
        character(len=100000) :: buffer
        integer(c_int) :: result_len, res

        res = paste_from_clipboard_c(buffer, 100000_c_int, result_len)
        if (res /= 0 .and. result_len > 0) then
            text = trim(buffer(1:result_len))
        else
            text = ''
        end if
    end function

end module platform_module
