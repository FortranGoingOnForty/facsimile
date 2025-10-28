module input_handler_module
    use iso_fortran_env, only: input_unit, int8, error_unit
    use terminal_io_module, only: terminal_read_char
    implicit none
    private

    public :: get_key_input, key_type, mouse_event_t

    ! Key type constants
    enum, bind(C)
        enumerator :: KEY_NORMAL = 0
        enumerator :: KEY_CTRL
        enumerator :: KEY_ALT
        enumerator :: KEY_SPECIAL
        enumerator :: KEY_MOUSE
    end enum

    type :: key_type
        integer :: type = KEY_NORMAL
        character(len=32) :: value = ''
    end type key_type

    type :: mouse_event_t
        integer :: button   ! 0=left, 1=middle, 2=right
        integer :: row      ! Terminal row (1-based)
        integer :: col      ! Terminal column (1-based)
        logical :: pressed  ! True=press, False=release
        logical :: shift    ! Shift modifier
        logical :: alt      ! Alt modifier
        logical :: ctrl     ! Ctrl modifier
    end type mouse_event_t

    character(len=*), parameter :: ESC = char(27)

contains

    subroutine get_key_input(key_str, status)
        character(len=*), intent(out) :: key_str
        integer, intent(out) :: status
        character :: ch
        integer :: char_code

        key_str = ''
        status = -1

        ! Read single character using raw mode function
        char_code = terminal_read_char()

        if (char_code < 0) then
            return
        end if

        ch = achar(char_code)
        status = 0

        ! Check for special keys
        select case(iachar(ch))
        case(27)  ! ESC
            call handle_escape_sequence(key_str)
        case(9)  ! Tab
            key_str = 'tab'
        case(10, 13)  ! Enter
            key_str = 'enter'
        case(8)  ! Ctrl-H (backspace)
            key_str = 'backspace'
        case(26)  ! Ctrl-Z
            key_str = 'ctrl-z'
        case(31)  ! Ctrl-/ (also ctrl-?)
            key_str = 'ctrl-?'
        case(7)  ! Ctrl-G (goto)
            key_str = 'ctrl-g'
        case(1:6, 11:12, 14:25)  ! Ctrl keys (excluding Ctrl-G, Ctrl-H, Ctrl-Z, Tab, Enter, and ESC)
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
        integer :: char_code, ios

        key_str = 'esc'

        ! Try to read next character (with timeout)
        char_code = terminal_read_char()
        if (char_code < 0) return
        ch1 = achar(char_code)

        if (ch1 == '[') then
            ! CSI sequence
            char_code = terminal_read_char()
            if (char_code < 0) return
            ch2 = achar(char_code)

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
                char_code = terminal_read_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        key_str = 'delete'
                    end if
                end if
            case('5')
                ! Could be page up
                char_code = terminal_read_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    ios = 0
                else
                    ios = -1
                end if
                if (ios == 0 .and. ch3 == '~') then
                    key_str = 'pageup'
                else if (ios == 0 .and. ch3 == ';') then
                    ! Modified page up (e.g., shift+pageup)
                    call handle_modified_special_key(key_str, 5)
                end if
            case('6')
                ! Could be page down
                char_code = terminal_read_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    ios = 0
                else
                    ios = -1
                end if
                if (ios == 0 .and. ch3 == '~') then
                    key_str = 'pagedown'
                else if (ios == 0 .and. ch3 == ';') then
                    ! Modified page down (e.g., shift+pagedown)
                    call handle_modified_special_key(key_str, 6)
                end if
            case('1')
                ! Could be modified arrow key or home/end
                call handle_modified_key(key_str)
            case('<')
                ! Mouse event in SGR mode
                call handle_mouse_event(key_str)
            end select
        else if (ch1 == achar(27)) then
            ! ESC ESC - likely Alt+something
            char_code = terminal_read_char()
            if (char_code >= 0) then
                ch2 = achar(char_code)
                if (ch2 == '[') then
                    ! ESC ESC [ - Alt+arrow keys
                    char_code = terminal_read_char()
                    if (char_code >= 0) then
                        ch3 = achar(char_code)
                        select case(ch3)
                        case('A')
                            key_str = 'alt-up'
                        case('B')
                            key_str = 'alt-down'
                        case('C')
                            key_str = 'alt-right'
                        case('D')
                            key_str = 'alt-left'
                        end select
                    end if
                end if
            end if
        else if (ch1 == 'A') then
            ! Could be Alt-Shift-Up
            key_str = 'alt-shift-up'
        else if (ch1 == 'B') then
            ! Could be Alt-Shift-Down
            key_str = 'alt-shift-down'
        else if (ch1 == achar(127)) then
            ! Alt+Backspace (ESC followed by DEL/127)
            key_str = 'alt-backspace'
        else if (ch1 == achar(8)) then
            ! Alt+Backspace (ESC followed by Ctrl-H)
            key_str = 'alt-backspace'
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
        integer :: ios, modifier, char_code

        modifier_seq = ''

        ! Read modifier sequence (e.g., ";5" for Ctrl)
        do
            char_code = terminal_read_char()
            if (char_code >= 0) then
                ch = achar(char_code)
                ios = 0
            else
                ios = -1
            end if
            if (ios /= 0) exit
            if ((ch >= 'A' .and. ch <= 'D') .or. ch == 'H' .or. ch == 'F' .or. ch == '~') then
                ! End of sequence
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
                case(4)  ! Alt+Shift
                    key_str = 'alt-shift-'
                case(5)  ! Ctrl
                    key_str = 'ctrl-'
                case(6)  ! Ctrl+Shift
                    key_str = 'ctrl-shift-'
                case(7)  ! Alt+Ctrl
                    key_str = 'alt-ctrl-'
                case(8)  ! Alt+Shift (or Option+Shift)
                    key_str = 'alt-shift-'
                case(9)  ! Alt+Cmd (or Option+Cmd on macOS)
                    key_str = 'opt-meta-'
                case default
                    key_str = ''
                end select

                ! Append the key type
                select case(ch)
                case('A')
                    key_str = trim(key_str) // 'up'
                case('B')
                    key_str = trim(key_str) // 'down'
                case('C')
                    key_str = trim(key_str) // 'right'
                case('D')
                    key_str = trim(key_str) // 'left'
                case('H')
                    key_str = trim(key_str) // 'home'
                case('F')
                    key_str = trim(key_str) // 'end'
                case('Z')
                    ! Shift+Z could be ctrl-shift-z for redo
                    if (index(key_str, 'ctrl-shift') == 1) then
                        key_str = 'ctrl-shift-z'
                    else
                        key_str = trim(key_str) // 'Z'
                    end if
                case('~')
                    ! Check what special key it is based on the beginning of modifier_seq
                    if (index(modifier_seq, ';') == 1 .and. len_trim(modifier_seq) > 1) then
                        ! Already read the ;2 or ;5 etc, the key type should be before
                        key_str = trim(key_str) // 'unknown'
                    end if
                end select
            end if
        end if
    end subroutine handle_modified_key

    subroutine handle_modified_special_key(key_str, key_code)
        character(len=*), intent(out) :: key_str
        integer, intent(in) :: key_code
        character :: ch
        character(len=10) :: modifier_seq
        integer :: ios, modifier, char_code

        modifier_seq = ''

        ! Read modifier sequence (already past the semicolon)
        do
            char_code = terminal_read_char()
            if (char_code >= 0) then
                ch = achar(char_code)
                ios = 0
            else
                ios = -1
            end if
            if (ios /= 0) exit
            if (ch == '~') then
                ! End of sequence
                exit
            end if
            modifier_seq = trim(modifier_seq) // ch
        end do

        ! Parse modifier
        if (len_trim(modifier_seq) > 0) then
            read(modifier_seq, '(i10)', iostat=ios) modifier
            if (ios == 0) then
                select case(modifier)
                case(2)  ! Shift
                    key_str = 'shift-'
                case(3)  ! Alt
                    key_str = 'alt-'
                case(4)  ! Alt+Shift
                    key_str = 'alt-shift-'
                case(5)  ! Ctrl
                    key_str = 'ctrl-'
                case(6)  ! Ctrl+Shift
                    key_str = 'ctrl-shift-'
                case default
                    key_str = ''
                end select

                ! Append the key type based on key_code
                select case(key_code)
                case(5)
                    key_str = trim(key_str) // 'pageup'
                case(6)
                    key_str = trim(key_str) // 'pagedown'
                end select
            end if
        end if
    end subroutine handle_modified_special_key

    subroutine handle_mouse_event(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch
        character(len=100) :: buffer
        integer :: i, ios, button, col, row, char_code
        integer :: semicolon1, semicolon2
        logical :: is_release

        buffer = ''
        i = 1

        ! Read until 'M' (press) or 'm' (release)
        do
            char_code = terminal_read_char()
            if (char_code >= 0) then
                ch = achar(char_code)
                ios = 0
            else
                ios = -1
            end if
            if (ios /= 0) exit
            if (ch == 'M' .or. ch == 'm') then
                is_release = (ch == 'm')
                exit
            end if
            if (i <= 100) then
                buffer(i:i) = ch
                i = i + 1
            end if
        end do

        ! Parse the mouse event format: button;col;row
        semicolon1 = index(buffer, ';')
        if (semicolon1 > 0) then
            semicolon2 = index(buffer(semicolon1+1:), ';') + semicolon1
            if (semicolon2 > semicolon1) then
                read(buffer(1:semicolon1-1), '(i10)', iostat=ios) button
                if (ios == 0) then
                    read(buffer(semicolon1+1:semicolon2-1), '(i10)', iostat=ios) col
                    if (ios == 0) then
                        read(buffer(semicolon2+1:i-1), '(i10)', iostat=ios) row
                        if (ios == 0) then
                            ! Format mouse event as key string
                            if (is_release) then
                                write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-release:', button, ':', row, ':', col
                            else
                                ! Check modifiers in button code
                                if (iand(button, 4) /= 0) then  ! Shift
                                    write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-shift:', button, ':', row, ':', col
                                else if (iand(button, 8) /= 0) then  ! Alt
                                    write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-alt:', button, ':', row, ':', col
                                else if (iand(button, 16) /= 0) then  ! Ctrl
                                    write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-ctrl:', button, ':', row, ':', col
                                else if (iand(button, 32) /= 0) then  ! Mouse motion (drag)
                                    write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-drag:', button, ':', row, ':', col
                                else
                                    write(key_str, '(a,i0,a,i0,a,i0)') 'mouse-click:', button, ':', row, ':', col
                                end if
                            end if
                            return
                        end if
                    end if
                end if
            end if
        end if

        key_str = ''
    end subroutine handle_mouse_event

end module input_handler_module