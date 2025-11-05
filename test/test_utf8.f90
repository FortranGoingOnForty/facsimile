program test_utf8
    use utf8_module
    implicit none

    character(len=:), allocatable :: test_str
    integer :: char_count, byte_idx, char_idx, width

    ! Test 1: ASCII string
    test_str = "Hello"
    char_count = utf8_char_count(test_str)
    width = utf8_display_width(test_str)
    print *, "Test 1: 'Hello'"
    print *, "  Bytes:", len_trim(test_str), "Chars:", char_count, "Width:", width
    if (char_count /= 5 .or. width /= 5) then
        print *, "  FAILED!"
        stop 1
    else
        print *, "  PASSED"
    end if

    ! Test 2: Box drawing characters
    test_str = "├──"
    char_count = utf8_char_count(test_str)
    width = utf8_display_width(test_str)
    print *, "Test 2: '├──' (box chars)"
    print *, "  Bytes:", len_trim(test_str), "Chars:", char_count, "Width:", width
    if (char_count /= 3 .or. width /= 3) then
        print *, "  FAILED! Expected 3 chars, 3 width"
        stop 1
    else
        print *, "  PASSED"
    end if

    ! Test 3: Mixed content
    test_str = "    ├── Tab 1"
    char_count = utf8_char_count(test_str)
    width = utf8_display_width(test_str)
    print *, "Test 3: '    ├── Tab 1'"
    print *, "  Bytes:", len_trim(test_str), "Chars:", char_count, "Width:", width
    if (char_count /= 13) then
        print *, "  FAILED! Expected 13 chars, got", char_count
        stop 1
    else
        print *, "  PASSED"
    end if

    ! Test 4: Character to byte index conversion
    test_str = "├──"
    byte_idx = utf8_char_to_byte_index(test_str, 2)  ! 2nd character
    print *, "Test 4: Char to byte index"
    print *, "  2nd char in '├──' starts at byte:", byte_idx
    if (byte_idx /= 4) then  ! First char is 3 bytes, so 2nd starts at byte 4
        print *, "  FAILED! Expected byte 4, got", byte_idx
        stop 1
    else
        print *, "  PASSED"
    end if

    ! Test 5: Byte to character index conversion
    test_str = "├──"
    char_idx = utf8_byte_to_char_index(test_str, 4)  ! Byte 4
    print *, "Test 5: Byte to char index"
    print *, "  Byte 4 in '├──' is char:", char_idx
    if (char_idx /= 2) then
        print *, "  FAILED! Expected char 2, got", char_idx
        stop 1
    else
        print *, "  PASSED"
    end if

    print *, ""
    print *, "All UTF-8 tests passed!"

end program test_utf8
