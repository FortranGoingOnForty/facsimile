module regex_module
    use iso_c_binding
    implicit none
    private

    public :: regex_compile, regex_match, regex_free, regex_free_all
    public :: REG_EXTENDED, REG_ICASE, REG_NOSUB

    ! POSIX regex flags (from regex.h)
    integer(c_int), parameter :: REG_EXTENDED = 1  ! Use Extended Regular Expressions
    integer(c_int), parameter :: REG_ICASE = 2     ! Ignore case
    integer(c_int), parameter :: REG_NOSUB = 4     ! Don't store match positions

    ! C function interfaces
    interface
        function compile_regex_c(pattern, cflags) bind(C, name="compile_regex")
            use iso_c_binding
            character(kind=c_char), dimension(*), intent(in) :: pattern
            integer(c_int), value :: cflags
            integer(c_int) :: compile_regex_c
        end function compile_regex_c

        function match_regex_c(id, text, match_start, match_len) bind(C, name="match_regex")
            use iso_c_binding
            integer(c_int), value :: id
            character(kind=c_char), dimension(*), intent(in) :: text
            integer(c_int), intent(out) :: match_start
            integer(c_int), intent(out) :: match_len
            integer(c_int) :: match_regex_c
        end function match_regex_c

        subroutine free_regex_c(id) bind(C, name="free_regex")
            use iso_c_binding
            integer(c_int), value :: id
        end subroutine free_regex_c

        subroutine free_all_regex_c() bind(C, name="free_all_regex")
            use iso_c_binding
        end subroutine free_all_regex_c
    end interface

contains

    ! Compile a regex pattern
    ! Returns regex ID (>= 0) on success, -1 on error
    function regex_compile(pattern, case_sensitive) result(regex_id)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: case_sensitive
        integer :: regex_id
        integer(c_int) :: cflags
        character(len=len_trim(pattern)+1, kind=c_char) :: c_pattern

        ! Set flags
        cflags = REG_EXTENDED
        if (.not. case_sensitive) then
            cflags = cflags + REG_ICASE
        end if

        ! Convert Fortran string to C string (null-terminated)
        c_pattern = trim(pattern) // c_null_char

        ! Call C function
        regex_id = compile_regex_c(c_pattern, cflags)
    end function regex_compile

    ! Match a regex against text
    ! Returns .true. if match found, .false. otherwise
    ! match_start and match_len are 1-based indices (Fortran style)
    function regex_match(regex_id, text, match_start, match_len) result(found)
        integer, intent(in) :: regex_id
        character(len=*), intent(in) :: text
        integer, intent(out) :: match_start, match_len
        logical :: found
        integer(c_int) :: result, c_start, c_len
        character(len=len(text)+1, kind=c_char) :: c_text

        ! Convert Fortran string to C string
        c_text = text // c_null_char

        ! Call C function
        result = match_regex_c(regex_id, c_text, c_start, c_len)

        if (result == 1) then
            ! Match found - convert C indices (0-based) to Fortran (1-based)
            match_start = c_start + 1
            match_len = c_len
            found = .true.
        else
            match_start = 0
            match_len = 0
            found = .false.
        end if
    end function regex_match

    ! Free a compiled regex
    subroutine regex_free(regex_id)
        integer, intent(in) :: regex_id
        call free_regex_c(regex_id)
    end subroutine regex_free

    ! Free all compiled regexes
    subroutine regex_free_all()
        call free_all_regex_c()
    end subroutine regex_free_all

end module regex_module
