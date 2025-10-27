program test_buffer_only
    ! Simple test to verify buffer module works correctly
    use text_buffer_module
    use iso_fortran_env, only: output_unit
    implicit none

    type(buffer_t) :: buffer
    character(len=:), allocatable :: line
    integer :: line_count

    print *, "Testing buffer module..."

    ! Test 1: Initialize empty buffer
    call init_buffer(buffer)
    print *, "✓ Buffer initialized"

    ! Test 2: Insert some text
    call buffer_insert(buffer, 1, "Hello World")
    print *, "✓ Text inserted"

    ! Test 3: Get buffer size
    print *, "Buffer logical size:", buffer%size
    print *, "Buffer gap start:", buffer%gap_start
    print *, "Buffer gap end:", buffer%gap_end

    ! Test 4: Get line
    line = buffer_get_line(buffer, 1)
    print *, "Line 1: '", line, "'"
    if (line == "Hello World") then
        print *, "✓ Line retrieval works"
    else
        print *, "✗ Line retrieval failed"
    end if

    ! Test 5: Insert newline
    call buffer_insert(buffer, 6, char(10))  ! After "Hello"
    print *, "✓ Newline inserted"

    ! Test 6: Get line count
    line_count = buffer_get_line_count(buffer)
    print *, "Line count:", line_count
    if (line_count == 2) then
        print *, "✓ Line count correct"
    else
        print *, "✗ Line count wrong"
    end if

    ! Test 7: Get second line
    line = buffer_get_line(buffer, 2)
    print *, "Line 2: '", line, "'"
    if (line == " World") then
        print *, "✓ Second line correct"
    else
        print *, "✗ Second line wrong"
    end if

    ! Clean up
    call cleanup_buffer(buffer)
    print *, "✓ Buffer cleaned up"

    print *, ""
    print *, "Buffer module tests complete!"

end program test_buffer_only