module mock_terminal_module
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: mock_init, mock_cleanup, mock_reset
    public :: mock_queue_input, mock_queue_key_sequence
    public :: mock_get_output, mock_get_cursor_position
    public :: mock_read_char, mock_input_available
    public :: mock_terminal_write

    ! Mock terminal state
    character(len=:), allocatable :: output_buffer
    integer, allocatable :: input_queue(:)
    integer :: input_pos = 0
    integer :: input_size = 0
    integer :: cursor_row = 1
    integer :: cursor_col = 1
    integer :: screen_rows = 24
    integer :: screen_cols = 80
    logical :: raw_mode = .false.

contains

    subroutine mock_init(rows, cols)
        integer, intent(in), optional :: rows, cols

        if (present(rows)) screen_rows = rows
        if (present(cols)) screen_cols = cols

        call mock_reset()
    end subroutine mock_init

    subroutine mock_cleanup()
        if (allocated(output_buffer)) deallocate(output_buffer)
        if (allocated(input_queue)) deallocate(input_queue)
    end subroutine mock_cleanup

    subroutine mock_reset()
        ! Reset all state
        if (allocated(output_buffer)) deallocate(output_buffer)
        if (allocated(input_queue)) deallocate(input_queue)

        allocate(character(len=0) :: output_buffer)
        allocate(input_queue(1000))
        input_pos = 0
        input_size = 0
        cursor_row = 1
        cursor_col = 1
    end subroutine mock_reset

    subroutine mock_queue_input(text)
        character(len=*), intent(in) :: text
        integer :: i

        do i = 1, len(text)
            if (input_size < size(input_queue)) then
                input_size = input_size + 1
                input_queue(input_size) = iachar(text(i:i))
            end if
        end do
    end subroutine mock_queue_input

    subroutine mock_queue_key_sequence(key_name)
        character(len=*), intent(in) :: key_name

        select case(key_name)
        case('up')
            call queue_escape_sequence([27, 91, 65])
        case('down')
            call queue_escape_sequence([27, 91, 66])
        case('right')
            call queue_escape_sequence([27, 91, 67])
        case('left')
            call queue_escape_sequence([27, 91, 68])
        case('home')
            call queue_escape_sequence([27, 91, 72])
        case('end')
            call queue_escape_sequence([27, 91, 70])
        case('pageup')
            call queue_escape_sequence([27, 91, 53, 126])
        case('pagedown')
            call queue_escape_sequence([27, 91, 54, 126])
        case('enter')
            input_size = input_size + 1
            input_queue(input_size) = 10
        case('tab')
            input_size = input_size + 1
            input_queue(input_size) = 9
        case('backspace')
            input_size = input_size + 1
            input_queue(input_size) = 127
        case('delete')
            call queue_escape_sequence([27, 91, 51, 126])
        case('ctrl-a')
            input_size = input_size + 1
            input_queue(input_size) = 1
        case('ctrl-e')
            input_size = input_size + 1
            input_queue(input_size) = 5
        case('ctrl-k')
            input_size = input_size + 1
            input_queue(input_size) = 11
        case('ctrl-u')
            input_size = input_size + 1
            input_queue(input_size) = 21
        case('ctrl-s')
            input_size = input_size + 1
            input_queue(input_size) = 19
        case('ctrl-q')
            input_size = input_size + 1
            input_queue(input_size) = 17
        case('ctrl-x')
            input_size = input_size + 1
            input_queue(input_size) = 24
        case('ctrl-c')
            input_size = input_size + 1
            input_queue(input_size) = 3
        case('ctrl-v')
            input_size = input_size + 1
            input_queue(input_size) = 22
        case('ctrl-z')
            input_size = input_size + 1
            input_queue(input_size) = 26
        case('ctrl-y')
            input_size = input_size + 1
            input_queue(input_size) = 25
        case('ctrl-d')
            input_size = input_size + 1
            input_queue(input_size) = 4
        case('ctrl-j')
            input_size = input_size + 1
            input_queue(input_size) = 10
        case('ctrl-l')
            input_size = input_size + 1
            input_queue(input_size) = 12
        case('ctrl-t')
            input_size = input_size + 1
            input_queue(input_size) = 20
        case('esc')
            input_size = input_size + 1
            input_queue(input_size) = 27
        end select
    end subroutine mock_queue_key_sequence

    subroutine queue_escape_sequence(seq)
        integer, intent(in) :: seq(:)
        integer :: i

        do i = 1, size(seq)
            if (input_size < size(input_queue)) then
                input_size = input_size + 1
                input_queue(input_size) = seq(i)
            end if
        end do
    end subroutine queue_escape_sequence

    function mock_input_available() result(available)
        logical :: available
        available = (input_pos < input_size)
    end function mock_input_available

    function mock_read_char() result(ch)
        integer :: ch

        if (input_pos < input_size) then
            input_pos = input_pos + 1
            ch = input_queue(input_pos)
        else
            ch = -1  ! No input available
        end if
    end function mock_read_char

    subroutine mock_terminal_write(text)
        character(len=*), intent(in) :: text
        output_buffer = output_buffer // text
    end subroutine mock_terminal_write

    function mock_get_output() result(output)
        character(len=:), allocatable :: output
        if (allocated(output_buffer)) then
            output = output_buffer
        else
            allocate(character(len=0) :: output)
        end if
    end function mock_get_output

    subroutine mock_get_cursor_position(row, col)
        integer, intent(out) :: row, col
        row = cursor_row
        col = cursor_col
    end subroutine mock_get_cursor_position

end module mock_terminal_module