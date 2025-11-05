module utf8_module
    use iso_fortran_env, only: int8
    implicit none
    private

    public :: utf8_char_count, utf8_byte_to_char_index, utf8_char_to_byte_index
    public :: utf8_char_at, utf8_display_width, utf8_is_valid_start

contains

    ! Count number of UTF-8 characters (not bytes) in a string
    pure function utf8_char_count(str) result(count)
        character(len=*), intent(in) :: str
        integer :: count
        integer :: i, byte_len, char_len

        count = 0
        byte_len = len(str)
        i = 1

        do while (i <= byte_len)
            char_len = utf8_char_byte_length(str, i)
            if (char_len > 0) then
                count = count + 1
                i = i + char_len
            else
                ! Invalid UTF-8, treat as single byte
                count = count + 1
                i = i + 1
            end if
        end do
    end function utf8_char_count

    ! Convert character index to byte index
    ! char_idx is 1-based character position
    ! Returns byte position (1-based), or 0 if out of bounds
    pure function utf8_char_to_byte_index(str, char_idx) result(byte_idx)
        character(len=*), intent(in) :: str
        integer, intent(in) :: char_idx
        integer :: byte_idx
        integer :: i, char_count, char_len, byte_len

        byte_idx = 0
        if (char_idx < 1) return

        byte_len = len(str)
        char_count = 0
        i = 1

        do while (i <= byte_len)
            char_count = char_count + 1
            if (char_count == char_idx) then
                byte_idx = i
                return
            end if

            char_len = utf8_char_byte_length(str, i)
            if (char_len > 0) then
                i = i + char_len
            else
                i = i + 1
            end if
        end do

        ! If char_idx == char_count + 1, return position after last char
        if (char_idx == char_count + 1) then
            byte_idx = byte_len + 1
        end if
    end function utf8_char_to_byte_index

    ! Convert byte index to character index
    ! byte_idx is 1-based byte position
    ! Returns character position (1-based)
    pure function utf8_byte_to_char_index(str, byte_idx) result(char_idx)
        character(len=*), intent(in) :: str
        integer, intent(in) :: byte_idx
        integer :: char_idx
        integer :: i, char_len, byte_len

        char_idx = 0
        if (byte_idx < 1) return

        byte_len = len(str)
        if (byte_idx > byte_len + 1) return

        char_idx = 1
        i = 1

        do while (i < byte_idx .and. i <= byte_len)
            char_len = utf8_char_byte_length(str, i)
            if (char_len > 0) then
                i = i + char_len
            else
                i = i + 1
            end if
            if (i <= byte_idx) char_idx = char_idx + 1
        end do
    end function utf8_byte_to_char_index

    ! Get the UTF-8 character at a given character index
    ! Returns empty string if out of bounds
    function utf8_char_at(str, char_idx) result(char_str)
        character(len=*), intent(in) :: str
        integer, intent(in) :: char_idx
        character(len=:), allocatable :: char_str
        integer :: byte_idx, char_len

        byte_idx = utf8_char_to_byte_index(str, char_idx)
        if (byte_idx == 0 .or. byte_idx > len(str)) then
            allocate(character(len=0) :: char_str)
            return
        end if

        char_len = utf8_char_byte_length(str, byte_idx)
        if (char_len <= 0) char_len = 1

        ! Make sure we don't go past end of string
        char_len = min(char_len, len(str) - byte_idx + 1)

        allocate(character(len=char_len) :: char_str)
        char_str = str(byte_idx:byte_idx+char_len-1)
    end function utf8_char_at

    ! Calculate display width of a UTF-8 string
    ! (accounts for wide characters like CJK)
    pure function utf8_display_width(str) result(width)
        character(len=*), intent(in) :: str
        integer :: width
        integer :: i, char_len, byte_len, code_point

        width = 0
        byte_len = len(str)
        i = 1

        do while (i <= byte_len)
            char_len = utf8_char_byte_length(str, i)
            if (char_len > 0) then
                code_point = utf8_decode_char(str, i, char_len)
                width = width + utf8_char_width(code_point)
                i = i + char_len
            else
                ! Invalid UTF-8, count as 1 wide
                width = width + 1
                i = i + 1
            end if
        end do
    end function utf8_display_width

    ! Determine byte length of UTF-8 character starting at position i
    ! Returns 0 if invalid UTF-8 start byte
    pure function utf8_char_byte_length(str, i) result(char_len)
        character(len=*), intent(in) :: str
        integer, intent(in) :: i
        integer :: char_len
        integer :: byte_val, str_len

        str_len = len(str)

        if (i < 1 .or. i > str_len) then
            char_len = 0
            return
        end if

        byte_val = iachar(str(i:i))

        ! ASCII: 0xxxxxxx
        if (byte_val < 128) then
            char_len = 1
        ! 2-byte: 110xxxxx
        else if (iand(byte_val, int(b'11100000')) == int(b'11000000')) then
            char_len = 2
        ! 3-byte: 1110xxxx
        else if (iand(byte_val, int(b'11110000')) == int(b'11100000')) then
            char_len = 3
        ! 4-byte: 11110xxx
        else if (iand(byte_val, int(b'11111000')) == int(b'11110000')) then
            char_len = 4
        else
            ! Invalid or continuation byte
            char_len = 0
        end if

        ! Make sure we don't read past end of string
        if (char_len > 0) then
            char_len = min(char_len, str_len - i + 1)
        end if
    end function utf8_char_byte_length

    ! Check if byte is a valid UTF-8 start byte (not continuation)
    pure function utf8_is_valid_start(byte_val) result(is_start)
        integer, intent(in) :: byte_val
        logical :: is_start

        ! Continuation bytes are 10xxxxxx
        is_start = iand(byte_val, int(b'11000000')) /= int(b'10000000')
    end function utf8_is_valid_start

    ! Decode UTF-8 character to Unicode code point
    pure function utf8_decode_char(str, pos, len) result(code_point)
        character(len=*), intent(in) :: str
        integer, intent(in) :: pos, len
        integer :: code_point
        integer :: byte1, byte2, byte3, byte4

        if (len < 1 .or. pos + len - 1 > len_trim(str)) then
            code_point = 0
            return
        end if

        byte1 = iachar(str(pos:pos))

        select case(len)
        case(1)
            code_point = byte1
        case(2)
            byte2 = iachar(str(pos+1:pos+1))
            code_point = ior(ishft(iand(byte1, int(b'00011111')), 6), &
                            iand(byte2, int(b'00111111')))
        case(3)
            byte2 = iachar(str(pos+1:pos+1))
            byte3 = iachar(str(pos+2:pos+2))
            code_point = ior(ior(ishft(iand(byte1, int(b'00001111')), 12), &
                                ishft(iand(byte2, int(b'00111111')), 6)), &
                            iand(byte3, int(b'00111111')))
        case(4)
            byte2 = iachar(str(pos+1:pos+1))
            byte3 = iachar(str(pos+2:pos+2))
            byte4 = iachar(str(pos+3:pos+3))
            code_point = ior(ior(ior(ishft(iand(byte1, int(b'00000111')), 18), &
                                    ishft(iand(byte2, int(b'00111111')), 12)), &
                                ishft(iand(byte3, int(b'00111111')), 6)), &
                            iand(byte4, int(b'00111111')))
        case default
            code_point = 0
        end select
    end function utf8_decode_char

    ! Get display width of a Unicode code point
    ! Returns 0 for combining chars, 1 for normal, 2 for wide (CJK)
    pure function utf8_char_width(code_point) result(width)
        integer, intent(in) :: code_point
        integer :: width

        ! Simplified width calculation
        ! Full implementation would need Unicode data tables

        ! Control characters
        if (code_point < 32 .or. (code_point >= 127 .and. code_point < 160)) then
            width = 0
        ! CJK Unified Ideographs and other wide chars
        else if ((code_point >= int(z'1100') .and. code_point <= int(z'115F')) .or. &  ! Hangul Jamo
                 (code_point >= int(z'2E80') .and. code_point <= int(z'A4CF')) .or. &  ! CJK
                 (code_point >= int(z'AC00') .and. code_point <= int(z'D7A3')) .or. &  ! Hangul Syllables
                 (code_point >= int(z'F900') .and. code_point <= int(z'FAFF')) .or. &  ! CJK Compatibility
                 (code_point >= int(z'FE10') .and. code_point <= int(z'FE19')) .or. &  ! Vertical forms
                 (code_point >= int(z'FE30') .and. code_point <= int(z'FE6F')) .or. &  ! CJK Compatibility Forms
                 (code_point >= int(z'FF00') .and. code_point <= int(z'FF60')) .or. &  ! Fullwidth Forms
                 (code_point >= int(z'FFE0') .and. code_point <= int(z'FFE6')) .or. &  ! Fullwidth Forms
                 (code_point >= int(z'20000') .and. code_point <= int(z'2FFFD')) .or. & ! CJK Extension B-F
                 (code_point >= int(z'30000') .and. code_point <= int(z'3FFFD'))) then
            width = 2
        ! Combining characters (simplified - just a few ranges)
        else if ((code_point >= int(z'0300') .and. code_point <= int(z'036F')) .or. &  ! Combining Diacriticals
                 (code_point >= int(z'1AB0') .and. code_point <= int(z'1AFF')) .or. &  ! Combining Diacriticals Extended
                 (code_point >= int(z'1DC0') .and. code_point <= int(z'1DFF')) .or. &  ! Combining Diacriticals Supplement
                 (code_point >= int(z'20D0') .and. code_point <= int(z'20FF')) .or. &  ! Combining Diacriticals for Symbols
                 (code_point >= int(z'FE20') .and. code_point <= int(z'FE2F'))) then   ! Combining Half Marks
            width = 0
        else
            ! Normal width
            width = 1
        end if
    end function utf8_char_width

end module utf8_module
