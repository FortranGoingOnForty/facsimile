module text_buffer_module
    use iso_fortran_env, only: int32, int64, error_unit
    implicit none
    private

    public :: buffer_t, init_buffer, cleanup_buffer
    public :: buffer_insert, buffer_delete, buffer_get_char
    public :: buffer_get_line, buffer_get_line_count
    public :: buffer_load_file, buffer_save_file
    public :: buffer_move_gap

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

    subroutine init_buffer(buffer, initial_content)
        type(buffer_t), intent(out) :: buffer
        character(len=*), intent(in), optional :: initial_content
        integer :: content_len

        if (present(initial_content)) then
            content_len = len(initial_content)
            buffer%size = max(INITIAL_SIZE, content_len * 2)
        else
            content_len = 0
            buffer%size = INITIAL_SIZE
        end if

        allocate(character(len=buffer%size) :: buffer%data)
        buffer%data = ' '  ! Initialize with spaces

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
        integer :: gap_size, move_size
        character(len=:), allocatable :: temp

        if (position == buffer%gap_start) return

        gap_size = buffer%gap_end - buffer%gap_start

        if (position < buffer%gap_start) then
            ! Move gap left
            move_size = buffer%gap_start - position
            allocate(character(len=move_size) :: temp)
            temp = buffer%data(position:buffer%gap_start-1)
            buffer%data(buffer%gap_end-move_size:buffer%gap_end-1) = temp
            deallocate(temp)
            buffer%gap_start = position
            buffer%gap_end = position + gap_size
        else
            ! Move gap right
            move_size = position - buffer%gap_start
            allocate(character(len=move_size) :: temp)
            temp = buffer%data(buffer%gap_end:buffer%gap_end+move_size-1)
            buffer%data(buffer%gap_start:buffer%gap_start+move_size-1) = temp
            deallocate(temp)
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
            new_data = ' '

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

        if (position < buffer%gap_start) then
            ch = buffer%data(position:position)
        else if (position >= buffer%gap_start) then
            ch = buffer%data(position + (buffer%gap_end - buffer%gap_start):&
                            position + (buffer%gap_end - buffer%gap_start))
        else
            ch = char(0)
        end if
    end function buffer_get_char

    function buffer_get_line(buffer, line_num) result(line)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        character(len=:), allocatable :: line
        integer :: current_line, pos, start_pos, end_pos
        character :: ch

        line = ''
        current_line = 1
        pos = 1
        start_pos = 1

        ! Find start of requested line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then  ! LF
                current_line = current_line + 1
                start_pos = pos + 1
            end if
            pos = pos + 1
            if (pos > buffer%size) return
        end do

        ! Find end of line
        end_pos = start_pos
        do
            ch = buffer_get_char(buffer, end_pos)
            if (ch == char(10) .or. ch == char(0)) exit
            end_pos = end_pos + 1
            if (end_pos > buffer%size) exit
        end do

        ! Extract line
        allocate(character(len=end_pos-start_pos) :: line)
        do pos = start_pos, end_pos - 1
            line(pos-start_pos+1:pos-start_pos+1) = buffer_get_char(buffer, pos)
        end do
    end function buffer_get_line

    function buffer_get_line_count(buffer) result(count)
        type(buffer_t), intent(in) :: buffer
        integer :: count
        integer :: pos
        character :: ch

        count = 1
        pos = 1

        do while (pos <= buffer%size)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) count = count + 1
            pos = pos + 1
        end do
    end function buffer_get_line_count

    subroutine buffer_load_file(buffer, filename, status)
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        integer :: unit, filesize, ios
        character(len=:), allocatable :: content

        status = -1
        open(newunit=unit, file=filename, status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)

        if (ios /= 0) then
            write(error_unit, *) 'Error opening file: ', trim(filename)
            return
        end if

        inquire(unit=unit, size=filesize)
        if (filesize > 0) then
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
        open(newunit=unit, file=filename, status='replace', action='write', &
             form='unformatted', access='stream', iostat=ios)

        if (ios /= 0) then
            write(error_unit, *) 'Error creating file: ', trim(filename)
            return
        end if

        ! Write content, skipping gap
        do pos = 1, buffer%size
            if (pos >= buffer%gap_start .and. pos < buffer%gap_end) cycle
            ch = buffer_get_char(buffer, pos)
            if (ch /= ' ' .or. pos < buffer%gap_start) then
                write(unit, iostat=ios) ch
                if (ios /= 0) exit
            end if
        end do

        close(unit)
        if (ios == 0) then
            buffer%modified = .false.
            status = 0
        end if
    end subroutine buffer_save_file

end module text_buffer_module