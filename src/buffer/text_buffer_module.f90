module text_buffer_module
    use iso_fortran_env, only: int32, int64, error_unit
    use utf8_module
    implicit none
    private

    public :: buffer_t, init_buffer, cleanup_buffer, copy_buffer
    public :: buffer_insert, buffer_delete, buffer_get_char
    public :: buffer_get_line, buffer_get_line_count, buffer_get_line_char_count
    public :: buffer_load_file, buffer_save_file, buffer_load_file_as_hex
    public :: buffer_signature
    public :: buffer_move_gap
    public :: buffer_char_at, buffer_byte_to_char_col, buffer_char_to_byte_col
    public :: buffer_to_string

    integer, parameter :: INITIAL_SIZE = 8192
    integer, parameter :: GROW_FACTOR = 2

    type :: buffer_t
        character(len=:), allocatable :: data
        integer(int32) :: gap_start = 1
        integer(int32) :: gap_end = 1
        integer(int32) :: size = 0
        logical :: modified = .false.
    end type buffer_t

contains

    !> A stand-in for the buffer's exact contents, cheap enough to keep.
    !>
    !> Answers "is this still the text that was last saved?" without holding a
    !> second copy of every open file. The modified flag alone cannot answer
    !> it: it only ever goes one way, so undoing an edit back to the original
    !> left the file marked dirty when nothing about it had changed.
    !>
    !> FNV-1a, mixed with the length at the end so two texts of different
    !> lengths can never agree. A collision would show a stale asterisk and
    !> nothing worse.
    function buffer_signature(buffer) result(sig)
        type(buffer_t), intent(in) :: buffer
        integer(int64) :: sig
        character(len=:), allocatable :: text
        integer :: i

        text = buffer_to_string(buffer)
        sig = 1469598103934665603_int64
        do i = 1, len(text)
            sig = ieor(sig, int(iachar(text(i:i)), int64))
            sig = sig * 1099511628211_int64
        end do
        sig = ieor(sig, int(len(text), int64))
    end function buffer_signature

    subroutine init_buffer(buffer, initial_content)
        type(buffer_t), intent(out) :: buffer
        character(len=*), intent(in), optional :: initial_content
        integer :: content_len, alloc_size

        if (present(initial_content)) then
            content_len = len(initial_content)
            alloc_size = max(INITIAL_SIZE, content_len * 2)
        else
            content_len = 0
            alloc_size = INITIAL_SIZE
        end if

        ! Allocate the buffer data
        allocate(character(len=alloc_size) :: buffer%data)
        buffer%data = repeat(' ', alloc_size)  ! Initialize with spaces
        buffer%size = alloc_size

        if (present(initial_content) .and. content_len > 0) then
            buffer%data(1:content_len) = initial_content
            buffer%gap_start = content_len + 1
            buffer%gap_end = buffer%size + 1
        else
            buffer%gap_start = 1
            buffer%gap_end = buffer%size + 1
        end if

        buffer%modified = .false.
    end subroutine init_buffer

    subroutine cleanup_buffer(buffer)
        type(buffer_t), intent(inout) :: buffer
        if (allocated(buffer%data)) deallocate(buffer%data)
        buffer%gap_start = 1
        buffer%gap_end = 1
        buffer%size = 0
    end subroutine cleanup_buffer

    subroutine buffer_move_gap(buffer, position)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: position
        integer :: gap_size, move_size, i
        character :: ch

        if (position == buffer%gap_start) return

        gap_size = buffer%gap_end - buffer%gap_start

        if (position < buffer%gap_start) then
            ! Move gap left - copy character by character to avoid allocation issues
            move_size = buffer%gap_start - position
            do i = 1, move_size
                ch = buffer%data(position + i - 1:position + i - 1)
                buffer%data(buffer%gap_end - move_size + i - 1:buffer%gap_end - move_size + i - 1) = ch
            end do
            buffer%gap_start = position
            buffer%gap_end = position + gap_size
        else
            ! Move gap right - copy character by character to avoid allocation issues
            move_size = position - buffer%gap_start
            do i = 1, move_size
                ch = buffer%data(buffer%gap_end + i - 1:buffer%gap_end + i - 1)
                buffer%data(buffer%gap_start + i - 1:buffer%gap_start + i - 1) = ch
            end do
            buffer%gap_start = position
            buffer%gap_end = position + gap_size
        end if
    end subroutine buffer_move_gap

    subroutine buffer_insert(buffer, position, text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: position
        character(len=*), intent(in) :: text
        integer :: text_len, gap_size, new_size
        character(len=:), allocatable :: new_data

        text_len = len(text)
        if (text_len == 0) return

        call buffer_move_gap(buffer, position)
        gap_size = buffer%gap_end - buffer%gap_start

        ! Grow buffer if needed
        if (text_len > gap_size) then
            new_size = buffer%size * GROW_FACTOR
            do while (text_len > new_size - (buffer%size - gap_size))
                new_size = new_size * GROW_FACTOR
            end do

            allocate(character(len=new_size) :: new_data)
            new_data = repeat(' ', new_size)

            ! Copy data before gap
            if (buffer%gap_start > 1) then
                new_data(1:buffer%gap_start-1) = buffer%data(1:buffer%gap_start-1)
            end if

            ! Copy data after gap
            if (buffer%gap_end <= buffer%size) then
                new_data(new_size-(buffer%size-buffer%gap_end+1)+1:new_size) = &
                    buffer%data(buffer%gap_end:buffer%size)
            end if

            buffer%gap_end = new_size - (buffer%size - buffer%gap_end) + 1
            deallocate(buffer%data)
            buffer%data = new_data
            buffer%size = new_size
        end if

        ! Insert text at gap start
        buffer%data(buffer%gap_start:buffer%gap_start+text_len-1) = text
        buffer%gap_start = buffer%gap_start + text_len
        buffer%modified = .true.
    end subroutine buffer_insert

    subroutine buffer_delete(buffer, position, count)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: position, count

        if (count <= 0) return

        call buffer_move_gap(buffer, position)
        buffer%gap_end = min(buffer%gap_end + count, buffer%size + 1)
        buffer%modified = .true.
    end subroutine buffer_delete

    function buffer_get_char(buffer, position) result(ch)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: position
        character :: ch
        integer :: actual_pos

        ! Check bounds
        if (position < 1 .or. position > buffer%size - (buffer%gap_end - buffer%gap_start)) then
            ch = char(0)
            return
        end if

        if (.not. allocated(buffer%data)) then
            ch = char(0)
            return
        end if

        if (position < buffer%gap_start) then
            if (position <= len(buffer%data)) then
                ch = buffer%data(position:position)
            else
                ch = char(0)
            end if
        else
            actual_pos = position + (buffer%gap_end - buffer%gap_start)
            if (actual_pos > 0 .and. actual_pos <= len(buffer%data)) then
                ch = buffer%data(actual_pos:actual_pos)
            else
                ch = char(0)
            end if
        end if
    end function buffer_get_char

    function buffer_get_line(buffer, line_num) result(line)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        character(len=:), allocatable :: line
        integer :: current_line, pos, start_pos, end_pos, logical_size
        character :: ch

        line = ''
        current_line = 1
        pos = 1
        start_pos = 1
        logical_size = buffer%size - (buffer%gap_end - buffer%gap_start)

        ! Find start of requested line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then  ! LF
                current_line = current_line + 1
                start_pos = pos + 1
            end if
            if (ch == char(0)) return  ! Null terminator
            pos = pos + 1
            if (pos > logical_size) return
        end do

        ! Find end of line
        end_pos = start_pos
        do
            ch = buffer_get_char(buffer, end_pos)
            if (ch == char(10) .or. ch == char(0)) exit
            end_pos = end_pos + 1
            if (end_pos > logical_size) exit
        end do

        ! Extract line
        if (allocated(line)) deallocate(line)
        if (end_pos > start_pos) then
            allocate(character(len=end_pos-start_pos) :: line)
            do pos = start_pos, end_pos - 1
                line(pos-start_pos+1:pos-start_pos+1) = buffer_get_char(buffer, pos)
            end do
        else
            ! Empty line
            allocate(character(len=0) :: line)
        end if
    end function buffer_get_line

    function buffer_get_line_count(buffer) result(count)
        type(buffer_t), intent(in) :: buffer
        integer :: count
        integer :: pos, logical_size
        character :: ch

        count = 1
        pos = 1
        logical_size = buffer%size - (buffer%gap_end - buffer%gap_start)

        do while (pos <= logical_size)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) count = count + 1
            if (ch == char(0)) exit  ! Stop at null terminator
            pos = pos + 1
        end do
    end function buffer_get_line_count

    subroutine buffer_load_file(buffer, filename, status)
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer :: unit, filesize, ios, i, null_count, check_size
        character(len=:), allocatable :: content
        character :: ch

        status = -1
        open(newunit=unit, file=filename, status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)

        if (ios /= 0) then
            write(error_unit, *) 'Error opening file: ', trim(filename)
            return
        end if

        inquire(unit=unit, size=filesize)
        if (filesize > 0) then
            ! Check if file is binary by reading first chunk
            check_size = min(512, filesize)
            null_count = 0

            ! Read first chunk to check for binary content
            do i = 1, check_size
                read(unit, iostat=ios) ch
                if (ios /= 0) exit
                if (iachar(ch) == 0) null_count = null_count + 1
            end do

            ! If more than 1% null bytes, it's likely binary
            if (null_count > check_size / 100) then
                close(unit)
                write(error_unit, '(A)') ''
                write(error_unit, '(A)') 'Error: Cannot open binary file: ' // trim(filename)
                write(error_unit, '(A)') ''
                write(error_unit, '(A)') 'This appears to be a binary file (contains null bytes).'
                write(error_unit, '(A)') 'Binary files like .mod, .o, .a, executables, and images cannot be edited as text.'
                write(error_unit, '(A)') ''
                write(error_unit, '(A)') 'To view binary files, try:'
                write(error_unit, '(A)') '  xxd ' // trim(filename) // '     # Hex dump'
                write(error_unit, '(A)') '  file ' // trim(filename) // '    # File type info'
                write(error_unit, '(A)') ''
                status = -2  ! Special status for binary files
                return
            end if

            ! Rewind to beginning to read full file
            rewind(unit)

            allocate(character(len=filesize) :: content)
            read(unit, iostat=ios) content
            if (ios == 0) then
                call init_buffer(buffer, content)
                status = 0
            end if
            deallocate(content)
        else
            call init_buffer(buffer)
            status = 0
        end if

        close(unit)
    end subroutine buffer_load_file

    subroutine buffer_save_file(buffer, filename, status)
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer :: unit, ios, pos
        character :: ch

        status = -1

        ! A buffer whose data was never allocated holds no text -- it is not an
        ! empty document, it is a document that was never loaded. Writing it
        ! would open the file with status='replace' and then write nothing,
        ! truncating a real file to zero bytes and reporting success, because
        ! both write loops below iterate zero times and leave ios at 0.
        !
        ! Refuse before the open, not after: by the time the file is open the
        ! damage is already done.
        if (.not. allocated(buffer%data)) then
            status = -3
            return
        end if

        open(newunit=unit, file=filename, status='replace', action='write', &
             form='unformatted', access='stream', iostat=ios)

        if (ios /= 0) then
            write(error_unit, *) 'Error creating file: ', trim(filename)
            return
        end if

        ! Write content, skipping gap
        ! Write content before gap
        do pos = 1, buffer%gap_start - 1
            ch = buffer%data(pos:pos)
            write(unit, iostat=ios) ch
            if (ios /= 0) exit
        end do

        ! Write content after gap
        if (ios == 0 .and. buffer%gap_end <= buffer%size) then
            do pos = buffer%gap_end, buffer%size
                ch = buffer%data(pos:pos)
                if (ch /= char(0)) then  ! Only write non-null characters
                    write(unit, iostat=ios) ch
                    if (ios /= 0) exit
                end if
            end do
        end if

        close(unit)
        if (ios == 0) then
            buffer%modified = .false.
            status = 0
        end if
    end subroutine buffer_save_file

    ! Copy buffer contents from source to destination
    subroutine copy_buffer(dest, src)
        type(buffer_t), intent(inout) :: dest
        type(buffer_t), intent(in) :: src

        ! Only reallocate if sizes differ
        if (allocated(dest%data)) then
            if (len(dest%data) /= src%size) then
                deallocate(dest%data)
                allocate(character(len=src%size) :: dest%data)
            end if
        else
            allocate(character(len=src%size) :: dest%data)
        end if

        ! Copy all fields
        dest%data = src%data
        dest%gap_start = src%gap_start
        dest%gap_end = src%gap_end
        dest%size = src%size
        dest%modified = src%modified
    end subroutine copy_buffer

    ! ========================================================================
    ! UTF-8 Helper Functions
    ! ========================================================================

    ! Get the number of UTF-8 characters (not bytes) in a line
    function buffer_get_line_char_count(buffer, line_num) result(char_count)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer :: char_count
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        char_count = utf8_char_count(line)
        if (allocated(line)) deallocate(line)
    end function buffer_get_line_char_count

    ! Get character at a specific character position (not byte) in a line
    ! Returns empty string if out of bounds
    function buffer_char_at(buffer, line_num, char_col) result(char_str)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, char_col
        character(len=:), allocatable :: char_str
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        char_str = utf8_char_at(line, char_col)
        if (allocated(line)) deallocate(line)
    end function buffer_char_at

    ! Convert byte column to character column in a line
    function buffer_byte_to_char_col(buffer, line_num, byte_col) result(char_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, byte_col
        integer :: char_col
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        char_col = utf8_byte_to_char_index(line, byte_col)
        if (allocated(line)) deallocate(line)
    end function buffer_byte_to_char_col

    ! Convert character column to byte column in a line
    function buffer_char_to_byte_col(buffer, line_num, char_col) result(byte_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, char_col
        integer :: byte_col
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, line_num)
        byte_col = utf8_char_to_byte_index(line, char_col)
        if (allocated(line)) deallocate(line)
    end function buffer_char_to_byte_col

    ! Load binary file as hex display (like xxd format)
    subroutine buffer_load_file_as_hex(buffer, filename, status)
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer :: unit, filesize, ios, i, line_count, bytes_read, byte_count
        character(len=:), allocatable :: hex_content
        character(len=16) :: byte_buffer
        character(len=100) :: hex_line
        character :: ch
        integer :: line_offset

        status = -1
        open(newunit=unit, file=filename, status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)

        if (ios /= 0) then
            write(error_unit, *) 'Error opening file: ', trim(filename)
            return
        end if

        inquire(unit=unit, size=filesize)
        if (filesize > 0) then
            ! Estimate hex content size (each byte becomes ~4 chars + formatting)
            ! Format: "00000000: 00 01 02 ... 0f  ................\n"
            ! Each line = 8 (offset) + 2 (: ) + 48 (hex) + 2 (  ) + 16 (ascii) + 1 (newline) = 77 chars
            line_count = (filesize + 15) / 16  ! Round up to nearest 16-byte line
            allocate(character(len=line_count * 80) :: hex_content)
            hex_content = ''

            line_offset = 0
            bytes_read = 0

            do while (bytes_read < filesize)
                ! Clear byte buffer and read up to 16 bytes
                byte_buffer = repeat(' ', 16)
                byte_count = 0
                do i = 1, 16
                    if (bytes_read >= filesize) exit
                    read(unit, iostat=ios) ch
                    if (ios /= 0) exit
                    byte_buffer(i:i) = ch
                    bytes_read = bytes_read + 1
                    byte_count = byte_count + 1
                end do

                if (byte_count == 0) exit

                ! Build hex line (format similar to xxd)
                hex_line = repeat(' ', 100)  ! Clear output line
                call format_hex_line(byte_buffer, byte_count, line_offset, hex_line)
                hex_content = trim(hex_content) // trim(hex_line) // char(10)
                line_offset = line_offset + 16
            end do

            close(unit)

            ! Initialize buffer with hex content
            call init_buffer(buffer, trim(hex_content))
            status = 0
        else
            close(unit)
            call init_buffer(buffer, "Empty file")
            status = 0
        end if
    end subroutine buffer_load_file_as_hex

    ! Format a line in xxd-style hex display
    subroutine format_hex_line(bytes, count, offset, output)
        character(len=*), intent(in) :: bytes
        integer, intent(in) :: count, offset
        character(len=*), intent(out) :: output
        integer :: i, byte_val, pos
        character :: ch

        ! Build the line: "00000000: 01 02 03 04 05 06 07 08  09 0a 0b 0c 0d 0e 0f 10  ................"
        output = ''
        pos = 1

        ! Add offset (8 hex digits + ": ")
        write(output(pos:pos+9), '(Z8.8,A)') offset, ': '
        pos = 11

        ! Add hex bytes (3 chars each: "XX ", plus extra space after 8th byte)
        do i = 1, 16
            if (i <= count) then
                byte_val = iachar(bytes(i:i))
                write(output(pos:pos+2), '(Z2.2,A)') byte_val, ' '
            else
                output(pos:pos+2) = '   '
            end if
            pos = pos + 3

            ! Extra space after 8th byte
            if (i == 8) then
                output(pos:pos) = ' '
                pos = pos + 1
            end if
        end do

        ! Add ASCII representation
        output(pos:pos) = ' '
        pos = pos + 1

        do i = 1, count
            ch = bytes(i:i)
            byte_val = iachar(ch)
            if (byte_val >= 32 .and. byte_val <= 126) then
                ! Printable ASCII
                output(pos:pos) = ch
            else
                ! Non-printable - use dot
                output(pos:pos) = '.'
            end if
            pos = pos + 1
        end do
    end subroutine format_hex_line

    ! Convert buffer contents to a string
    function buffer_to_string(buffer) result(str)
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: str
        integer :: content_len

        if (buffer%gap_start == 1) then
            ! No content before gap
            content_len = buffer%size - buffer%gap_end + 1
            if (content_len > 0) then
                allocate(character(len=content_len) :: str)
                str = buffer%data(buffer%gap_end:buffer%size)
            else
                allocate(character(len=0) :: str)
                str = ""
            end if
        else if (buffer%gap_end > buffer%size) then
            ! No gap (full buffer)
            content_len = buffer%gap_start - 1
            allocate(character(len=content_len) :: str)
            str = buffer%data(1:content_len)
        else
            ! Content on both sides of gap
            content_len = (buffer%gap_start - 1) + (buffer%size - buffer%gap_end + 1)
            allocate(character(len=content_len) :: str)
            str = buffer%data(1:buffer%gap_start-1) // buffer%data(buffer%gap_end:buffer%size)
        end if
    end function buffer_to_string

end module text_buffer_module