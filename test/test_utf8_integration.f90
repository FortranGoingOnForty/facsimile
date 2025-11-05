program test_utf8_integration
    use test_framework
    use utf8_module
    use text_buffer_module
    implicit none

    type(test_suite) :: suite

    ! Initialize test suite
    suite%name = "UTF-8 Integration Tests"
    allocate(suite%tests(10))

    ! UTF-8 Module Tests
    suite%tests(1) = test_case("UTF-8 ASCII char count", test_utf8_ascii_count)
    suite%tests(2) = test_case("UTF-8 box char count", test_utf8_box_char_count)
    suite%tests(3) = test_case("UTF-8 mixed content count", test_utf8_mixed_count)
    suite%tests(4) = test_case("UTF-8 char to byte index", test_char_to_byte_index)
    suite%tests(5) = test_case("UTF-8 byte to char index", test_byte_to_char_index)
    suite%tests(6) = test_case("UTF-8 display width", test_display_width)
    suite%tests(7) = test_case("UTF-8 char extraction", test_char_at)

    ! Buffer UTF-8 Tests
    suite%tests(8) = test_case("Buffer UTF-8 line char count", test_buffer_char_count)
    suite%tests(9) = test_case("Buffer char to byte conversion", test_buffer_conversions)
    suite%tests(10) = test_case("Buffer UTF-8 char extraction", test_buffer_char_at)

    ! Run all tests
    call run_tests(suite)

    ! Exit with proper code
    if (suite%num_failed > 0) then
        stop 1
    end if

contains

    subroutine test_utf8_ascii_count()
        integer :: count
        count = utf8_char_count("Hello")
        call assert_equals(5, count, "ASCII string char count")
    end subroutine test_utf8_ascii_count

    subroutine test_utf8_box_char_count()
        integer :: count
        ! Box drawing chars: ├ (3 bytes) ─ (3 bytes) ─ (3 bytes) = 9 bytes, 3 chars
        count = utf8_char_count("├──")
        call assert_equals(3, count, "Box drawing chars count")
    end subroutine test_utf8_box_char_count

    subroutine test_utf8_mixed_count()
        integer :: count
        ! "    ├── Tab" = 4 spaces + 3 box chars + space + 3 letters = 11 chars
        count = utf8_char_count("    ├── Tab")
        call assert_equals(11, count, "Mixed content char count")
    end subroutine test_utf8_mixed_count

    subroutine test_char_to_byte_index()
        integer :: byte_idx
        ! In "├──", char 2 (second ─) starts at byte 4
        byte_idx = utf8_char_to_byte_index("├──", 2)
        call assert_equals(4, byte_idx, "Char 2 to byte index")

        ! Char 3 starts at byte 7
        byte_idx = utf8_char_to_byte_index("├──", 3)
        call assert_equals(7, byte_idx, "Char 3 to byte index")
    end subroutine test_char_to_byte_index

    subroutine test_byte_to_char_index()
        integer :: char_idx
        ! In "├──", byte 4 is start of char 2
        char_idx = utf8_byte_to_char_index("├──", 4)
        call assert_equals(2, char_idx, "Byte 4 to char index")

        ! Byte 7 is start of char 3
        char_idx = utf8_byte_to_char_index("├──", 7)
        call assert_equals(3, char_idx, "Byte 7 to char index")
    end subroutine test_byte_to_char_index

    subroutine test_display_width()
        integer :: width
        ! ASCII: 1 char = 1 column
        width = utf8_display_width("Hello")
        call assert_equals(5, width, "ASCII display width")

        ! Box chars: 1 char = 1 column
        width = utf8_display_width("├──")
        call assert_equals(3, width, "Box chars display width")
    end subroutine test_display_width

    subroutine test_char_at()
        character(len=:), allocatable :: char_str

        ! Get second character from "├──"
        char_str = utf8_char_at("├──", 2)
        call assert_true(allocated(char_str), "Char extraction allocated")
        call assert_equals(3, len(char_str), "Second box char is 3 bytes")

        if (allocated(char_str)) deallocate(char_str)
    end subroutine test_char_at

    subroutine test_buffer_char_count()
        type(buffer_t) :: buffer
        integer :: count

        ! Create buffer with UTF-8 content
        call init_buffer(buffer, "├── Box chars" // char(10) // "Normal line")

        ! Line 1 should have 13 characters
        count = buffer_get_line_char_count(buffer, 1)
        call assert_equals(13, count, "Buffer line 1 char count")

        ! Line 2 should have 11 characters
        count = buffer_get_line_char_count(buffer, 2)
        call assert_equals(11, count, "Buffer line 2 char count")

        call cleanup_buffer(buffer)
    end subroutine test_buffer_char_count

    subroutine test_buffer_conversions()
        type(buffer_t) :: buffer
        integer :: byte_col, char_col

        call init_buffer(buffer, "├──")

        ! Char 2 should be at byte 4
        byte_col = buffer_char_to_byte_col(buffer, 1, 2)
        call assert_equals(4, byte_col, "Buffer char to byte")

        ! Byte 4 should be char 2
        char_col = buffer_byte_to_char_col(buffer, 1, 4)
        call assert_equals(2, char_col, "Buffer byte to char")

        call cleanup_buffer(buffer)
    end subroutine test_buffer_conversions

    subroutine test_buffer_char_at()
        type(buffer_t) :: buffer
        character(len=:), allocatable :: char_str

        call init_buffer(buffer, "├──")

        ! Get first character
        char_str = buffer_char_at(buffer, 1, 1)
        call assert_true(allocated(char_str), "Buffer char_at allocated")
        call assert_equals(3, len(char_str), "First box char is 3 bytes")

        if (allocated(char_str)) deallocate(char_str)
        call cleanup_buffer(buffer)
    end subroutine test_buffer_char_at

end program test_utf8_integration
