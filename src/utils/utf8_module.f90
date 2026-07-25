module utf8_module
    use iso_fortran_env, only: int8
    implicit none
    private

    public :: utf8_char_count, utf8_byte_to_char_index, utf8_char_to_byte_index
    public :: utf8_char_at, utf8_display_width, utf8_is_valid_start
    public :: utf8_char_col_to_utf16, utf16_to_utf8_char_col
    public :: clip_to_cells

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

    ! LSP interop: Language Server Protocol positions default to UTF-16
    ! code units. A code point outside the BMP (4-byte UTF-8) occupies 2
    ! units; everything else occupies 1.

    ! Convert a 1-based char column to the 0-based UTF-16 unit offset of
    ! that column (i.e. units occupied by the chars before it)
    pure function utf8_char_col_to_utf16(str, char_col) result(units)
        character(len=*), intent(in) :: str
        integer, intent(in) :: char_col
        integer :: units
        integer :: i, cidx, char_len

        units = 0
        cidx = 1
        i = 1
        do while (i <= len(str) .and. cidx < char_col)
            char_len = utf8_char_byte_length(str, i)
            if (char_len <= 0) char_len = 1
            if (char_len == 4) then
                units = units + 2
            else
                units = units + 1
            end if
            i = i + char_len
            cidx = cidx + 1
        end do
    end function utf8_char_col_to_utf16

    ! Convert a 0-based UTF-16 unit offset to a 1-based char column.
    ! Offsets past the end of the line map to char_count + 1.
    pure function utf16_to_utf8_char_col(str, utf16_units) result(char_col)
        character(len=*), intent(in) :: str
        integer, intent(in) :: utf16_units
        integer :: char_col
        integer :: i, units, char_len

        char_col = 1
        units = 0
        i = 1
        do while (i <= len(str) .and. units < utf16_units)
            char_len = utf8_char_byte_length(str, i)
            if (char_len <= 0) char_len = 1
            if (char_len == 4) then
                units = units + 2
            else
                units = units + 1
            end if
            i = i + char_len
            char_col = char_col + 1
        end do
    end function utf16_to_utf8_char_col

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
    pure function utf8_decode_char(str, pos, nbytes) result(code_point)
        character(len=*), intent(in) :: str
        integer, intent(in) :: pos, nbytes
        integer :: code_point
        integer :: byte1, byte2, byte3, byte4

        ! Bound by len(str), not len_trim: a trailing space (or a lone ' '
        ! passed per-char by the renderer) has len_trim 0 and would decode
        ! as code point 0 - a control char with display width 0. That
        ! undercount made rendered rows overflow their pane width.
        if (nbytes < 1 .or. pos + nbytes - 1 > len(str)) then
            code_point = 0
            return
        end if

        byte1 = iachar(str(pos:pos))

        select case(nbytes)
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
                 (code_point >= int(z'30000') .and. code_point <= int(z'3FFFD')) .or. & ! CJK Extension G
                 ! Emoji. Terminals draw these two cells wide, so leaving them
                 ! at 1 put every column after an emoji one cell out -- the
                 ! caret, the ghost-text clip, and the shifted line tail alike.
                 (code_point >= int(z'1F300') .and. code_point <= int(z'1F64F')) .or. & ! pictographs, emoticons
                 (code_point >= int(z'1F680') .and. code_point <= int(z'1F6FF')) .or. & ! transport & map
                 (code_point >= int(z'1F900') .and. code_point <= int(z'1F9FF')) .or. & ! supplemental symbols
                 (code_point >= int(z'1FA70') .and. code_point <= int(z'1FAFF'))) then  ! symbols extended-A
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

    ! Longest prefix of `text` that fits in `cells` display columns, cutting
    ! only on character boundaries and dropping a wide character that would
    ! straddle the limit. `used` reports the width actually taken, so callers
    ! can pad the remainder exactly.
    !
    ! Lives here rather than in renderer_module because UI modules that must
    ! compile before the renderer need it too, and it depends on nothing else.
    subroutine clip_to_cells(text, cells, clipped, used)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cells
        character(len=:), allocatable, intent(out) :: clipped
        integer, intent(out) :: used
        character(len=:), allocatable :: ch
        integer :: ci, nchars, w, last_byte, start_byte

        used = 0
        last_byte = 0
        nchars = utf8_char_count(text)

        do ci = 1, nchars
            ch = utf8_char_at(text, ci)
            if (len(ch) == 0) exit
            w = utf8_display_width(ch)
            if (used + w > cells) exit
            used = used + w
            start_byte = utf8_char_to_byte_index(text, ci)
            if (start_byte <= 0) exit
            last_byte = start_byte + len(ch) - 1
        end do

        if (last_byte <= 0) then
            clipped = ''
        else
            clipped = text(1:last_byte)
        end if
    end subroutine clip_to_cells

end module utf8_module
