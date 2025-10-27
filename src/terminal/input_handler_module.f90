module input_handler_module
    use iso_fortran_env, only: input_unit, int8
    implicit none
    private

    public :: get_key_input, key_type

    ! Key type constants
    enum, bind(C)
        enumerator :: KEY_NORMAL = 0
        enumerator :: KEY_CTRL
        enumerator :: KEY_ALT
        enumerator :: KEY_SPECIAL
    end enum

    type :: key_type
        integer :: type = KEY_NORMAL
        character(len=32) :: value = ''
    end type key_type

    character(len=*), parameter :: ESC = char(27)

contains

    subroutine get_key_input(key_str, status)
        character(len=*), intent(out) :: key_str
        integer, intent(out) :: status
        character :: ch
        integer :: ios

        key_str = ''
        status = -1

        ! Read single character (non-blocking would be better)
        read(input_unit, '(a1)', advance='no', iostat=ios) ch

        if (ios /= 0) then
            return
        end if

        status = 0

        ! Check for special keys
        select case(iachar(ch))
        case(27)  ! ESC
            call handle_escape_sequence(key_str)
        case(9)  ! Tab
            key_str = 'tab'
        case(10, 13)  ! Enter
            key_str = 'enter'
        case(1:8, 11:12, 14:26)  ! Ctrl keys (excluding Tab, Enter, and ESC)
            write(key_str, '(a,a)') 'ctrl-', achar(iachar('a') + iachar(ch) - 1)
        case(127)  ! Backspace
            key_str = 'backspace'
        case default
            key_str = ch
        end select

    end subroutine get_key_input

    subroutine handle_escape_sequence(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch1, ch2, ch3
        integer :: ios

        key_str = 'esc'

        ! Try to read next character (with timeout would be better)
        read(input_unit, '(a1)', advance='no', iostat=ios) ch1
        if (ios /= 0) return

        if (ch1 == '[') then
            ! CSI sequence
            read(input_unit, '(a1)', advance='no', iostat=ios) ch2
            if (ios /= 0) return

            select case(ch2)
            case('A')
                key_str = 'up'
            case('B')
                key_str = 'down'
            case('C')
                key_str = 'right'
            case('D')
                key_str = 'left'
            case('H')
                key_str = 'home'
            case('F')
                key_str = 'end'
            case('3')
                ! Could be delete
                read(input_unit, '(a1)', advance='no', iostat=ios) ch3
                if (ios == 0 .and. ch3 == '~') then
                    key_str = 'delete'
                end if
            case('5')
                ! Could be page up
                read(input_unit, '(a1)', advance='no', iostat=ios) ch3
                if (ios == 0 .and. ch3 == '~') then
                    key_str = 'pageup'
                end if
            case('6')
                ! Could be page down
                read(input_unit, '(a1)', advance='no', iostat=ios) ch3
                if (ios == 0 .and. ch3 == '~') then
                    key_str = 'pagedown'
                end if
            case('1')
                ! Could be modified arrow key
                call handle_modified_key(key_str)
            end select
        else if (ch1 == 'A') then
            ! Could be Alt-Shift-Up
            key_str = 'alt-shift-up'
        else if (ch1 == 'B') then
            ! Could be Alt-Shift-Down
            key_str = 'alt-shift-down'
        else if (ch1 >= 'a' .and. ch1 <= 'z') then
            ! Alt+letter
            write(key_str, '(a,a)') 'alt-', ch1
        else if (ch1 >= 'A' .and. ch1 <= 'Z') then
            ! Alt+Shift+letter
            write(key_str, '(a,a)') 'alt-shift-', achar(iachar(ch1) - iachar('A') + iachar('a'))
        end if

    end subroutine handle_escape_sequence

    subroutine handle_modified_key(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch
        character(len=10) :: modifier_seq
        integer :: ios, modifier

        modifier_seq = ''

        ! Read modifier sequence (e.g., ";5" for Ctrl)
        do
            read(input_unit, '(a1)', advance='no', iostat=ios) ch
            if (ios /= 0) exit
            if (ch >= 'A' .and. ch <= 'D') then
                ! End of sequence, it's an arrow key
                exit
            end if
            modifier_seq = trim(modifier_seq) // ch
        end do

        ! Parse modifier
        if (len_trim(modifier_seq) > 1) then
            read(modifier_seq(2:), '(i10)', iostat=ios) modifier
            if (ios == 0) then
                select case(modifier)
                case(2)  ! Shift
                    key_str = 'shift-'
                case(3)  ! Alt
                    key_str = 'alt-'
                case(5)  ! Ctrl
                    key_str = 'ctrl-'
                case(6)  ! Ctrl+Shift
                    key_str = 'ctrl-shift-'
                case(7)  ! Alt+Ctrl
                    key_str = 'alt-ctrl-'
                case default
                    key_str = ''
                end select

                ! Append the arrow key
                select case(ch)
                case('A')
                    key_str = trim(key_str) // 'up'
                case('B')
                    key_str = trim(key_str) // 'down'
                case('C')
                    key_str = trim(key_str) // 'right'
                case('D')
                    key_str = trim(key_str) // 'left'
                end select
            end if
        end if
    end subroutine handle_modified_key

end module input_handler_module