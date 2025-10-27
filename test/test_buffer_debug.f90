program test_buffer_debug
    use text_buffer_module
    use iso_fortran_env, only: output_unit
    implicit none

    type(buffer_t) :: buffer
    character :: ch

    print *, "Testing buffer initialization..."

    ! Initialize buffer
    call init_buffer(buffer)

    ! Check if buffer%data is allocated
    if (allocated(buffer%data)) then
        print *, "Buffer data allocated"
        print *, "Buffer data length:", len(buffer%data)
        print *, "Buffer size field:", buffer%size
        print *, "Gap start:", buffer%gap_start
        print *, "Gap end:", buffer%gap_end
    else
        print *, "ERROR: Buffer data not allocated!"
    end if

    ! Try to get a character
    print *, "Attempting to get character at position 1..."
    ch = buffer_get_char(buffer, 1)
    print *, "Character retrieved (as integer):", ichar(ch)

    ! Try to insert text
    print *, "Attempting to insert text..."
    call buffer_insert(buffer, 1, "Test")
    print *, "Text inserted"

    ! Check buffer state after insert
    print *, "After insert:"
    print *, "Gap start:", buffer%gap_start
    print *, "Gap end:", buffer%gap_end

    ! Clean up
    call cleanup_buffer(buffer)
    print *, "Buffer cleaned up"

end program test_buffer_debug