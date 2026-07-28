module input_handler_module
    use iso_fortran_env, only: input_unit, int8, error_unit
    use terminal_io_module, only: terminal_read_char, terminal_read_char_escape
    implicit none
    private

    public :: get_key_input, key_type, mouse_event_t
    public :: get_paste_text
    public :: decode_csi_u  ! exposed for unit tests
    public :: modifier_prefix  ! exposed for unit tests
    ! Splits the "event:button:row:col" strings this module emits. It
    ! lives here, with the code that formats them, so any module above
    ! can read a mouse event -- the command palette runs its own input
    ! loop and could not reach the copy that was in command_handler.
    public :: parse_mouse_event

    ! Holds the most recent bracketed-paste payload; retrieved by
    ! the command handler when a 'paste' key event is delivered.
    character(len=:), allocatable :: g_paste_buffer

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

    ! Pushback queue for bytes the CSI lookahead consumed but did not use.
    !
    ! Kitty-protocol key events (CSI <code> ; <mods> u) and the legacy CSI
    ! sequences share a prefix, so the only way to tell them apart is to scan
    ! ahead to the final byte. When the sequence turns out to be a legacy one,
    ! the scanned bytes are pushed back here and the original hand-rolled
    ! parser re-reads them unchanged -- the legacy path stays byte-for-byte
    ! what it always was.
    integer, parameter :: PUSHBACK_CAP = 64
    character(len=PUSHBACK_CAP) :: g_pushback = ''
    integer :: g_pushback_len = 0
    integer :: g_pushback_pos = 1

contains

    ! Queue bytes to be handed back before the next terminal read.
    subroutine push_back(bytes)
        character(len=*), intent(in) :: bytes
        integer :: n

        n = min(len(bytes), PUSHBACK_CAP)
        if (n <= 0) return
        g_pushback(1:n) = bytes(1:n)
        g_pushback_len = n
        g_pushback_pos = 1
    end subroutine push_back

    ! Next byte of an escape sequence: pushback first, then the terminal.
    function next_escape_char() result(code)
        integer :: code

        if (g_pushback_pos <= g_pushback_len) then
            code = iachar(g_pushback(g_pushback_pos:g_pushback_pos))
            g_pushback_pos = g_pushback_pos + 1
            if (g_pushback_pos > g_pushback_len) then
                g_pushback_len = 0
                g_pushback_pos = 1
            end if
            return
        end if

        code = terminal_read_char_escape()
    end function next_escape_char

    subroutine get_key_input(key_str, status)
        character(len=*), intent(out) :: key_str
        integer, intent(out) :: status
        character :: ch
        integer :: char_code

        key_str = ''
        status = -1

        ! Read single character using raw mode function (50ms timeout when idle).
        ! Pushback first: a CSI lookahead may have left bytes behind if the
        ! sequence it was scanning turned out to be malformed.
        if (g_pushback_pos <= g_pushback_len) then
            char_code = next_escape_char()
        else
            char_code = terminal_read_char()
        end if

        if (char_code < 0) then
            return
        end if

        ch = achar(char_code)
        status = 0

        ! Check for special keys
        select case(iachar(ch))
        case(0)  ! Ctrl-Space (NULL character)
            key_str = 'ctrl-space'
        case(27)  ! ESC
            call handle_escape_sequence(key_str)
            ! A blank name means the sequence was understood but maps to
            ! nothing we bind (a media key, an unparsable modifier). Report
            ! no key at all -- an empty key_str would otherwise reach the
            ! command layer as a space and type one.
            if (len_trim(key_str) == 0) status = -1
        case(9)  ! Tab
            key_str = 'tab'
        case(10, 13)  ! Enter (LF or CR)
            key_str = 'enter'
        case(8)  ! Ctrl-H
            key_str = 'ctrl-h'
        case(26)  ! Ctrl-Z
            key_str = 'ctrl-z'
        case(31)  ! Ctrl-/ and Ctrl-? (both send ASCII 31)
            key_str = 'ctrl-/'
        case(7)  ! Ctrl-G (goto)
            key_str = 'ctrl-g'
        case(29)  ! Ctrl-] (for redo)
            key_str = 'ctrl-]'
        case(1:6, 11:12, 14:25, 28)  ! Ctrl keys (excluding Ctrl-G, Ctrl-H, Ctrl-Z, Tab, Enter, ESC, and Ctrl-])
            write(key_str, '(a,a)') 'ctrl-', achar(iachar('a') + iachar(ch) - 1)
        case(127)  ! Backspace
            key_str = 'backspace'
        case default
            key_str = ch
            ! Assemble a full UTF-8 sequence into one key event: the command
            ! layer treats cursor columns as whole characters, so a multibyte
            ! char must never arrive as separate single-byte keystrokes.
            block
                integer :: nbytes, k, cont
                nbytes = 1
                if (iachar(ch) >= 192 .and. iachar(ch) <= 223) nbytes = 2
                if (iachar(ch) >= 224 .and. iachar(ch) <= 239) nbytes = 3
                if (iachar(ch) >= 240 .and. iachar(ch) <= 247) nbytes = 4
                do k = 2, nbytes
                    cont = next_escape_char()
                    if (cont < 0) exit
                    key_str(k:k) = achar(cont)
                end do
            end block
        end select

    end subroutine get_key_input

    ! Return the most recent bracketed-paste payload
    function get_paste_text() result(text)
        character(len=:), allocatable :: text
        if (allocated(g_paste_buffer)) then
            text = g_paste_buffer
        else
            text = ''
        end if
    end function get_paste_text

    ! Read raw bytes after ESC[200~ until the ESC[201~ terminator,
    ! storing the payload in g_paste_buffer. Emits key_str='paste'.
    subroutine capture_bracketed_paste(key_str)
        character(len=*), intent(out) :: key_str
        integer :: cc, n, cap, misses
        character(len=:), allocatable :: buf, tmp
        character(len=6), parameter :: term_seq = ESC // '[201~'

        cap = 256
        allocate(character(len=cap) :: buf)
        n = 0
        misses = 0

        do
            cc = next_escape_char()
            if (cc < 0) then
                ! Tolerate brief gaps mid-paste; bail if truly idle
                misses = misses + 1
                if (misses > 40) exit
                cycle
            end if
            misses = 0

            if (n == cap) then
                cap = cap * 2
                allocate(character(len=cap) :: tmp)
                tmp(1:n) = buf(1:n)
                call move_alloc(tmp, buf)
            end if
            n = n + 1
            buf(n:n) = achar(cc)

            if (n >= 6) then
                if (buf(n-5:n) == term_seq) then
                    n = n - 6  ! strip terminator
                    exit
                end if
            end if
        end do

        if (allocated(g_paste_buffer)) deallocate(g_paste_buffer)
        g_paste_buffer = buf(1:n)
        key_str = 'paste'
    end subroutine capture_bracketed_paste

    ! Scan a CSI sequence whose first parameter byte is already in hand and
    ! decode it if it turns out to be a kitty key event. Anything else is
    ! pushed back (minus that first byte, which the caller still holds) so the
    ! legacy parser sees exactly the stream it would have seen.
    subroutine try_csi_u(first_ch, key_str, handled)
        character, intent(in) :: first_ch
        character(len=*), intent(out) :: key_str
        logical, intent(out) :: handled
        character(len=PUSHBACK_CAP) :: params
        integer :: n, code
        character :: c
        logical :: decoded

        handled = .false.
        params = ''
        params(1:1) = first_ch
        n = 1

        do
            if (n >= PUSHBACK_CAP - 1) exit
            code = next_escape_char()
            if (code < 0) then
                ! Timed out mid-sequence; hand back what we took.
                if (n > 1) call push_back(params(2:n))
                return
            end if
            c = achar(code)
            ! 0x30-0x3F is the CSI parameter-byte range (digits, ';', ':', ...)
            if (code >= 48 .and. code <= 63) then
                n = n + 1
                params(n:n) = c
                cycle
            end if

            if (c == 'u') then
                ! Whatever we make of it, a 'u'-terminated sequence IS a key
                ! event and has been fully read. Falling through to the legacy
                ! parser here would leave it re-reading the NEXT keystroke's
                ! bytes as if they belonged to this sequence, which corrupts
                ! everything typed afterwards.
                call decode_csi_u(params(1:n), key_str, decoded)
                handled = .true.
                if (.not. decoded) key_str = ''
                return
            end if

            call push_back(params(2:n) // c)
            return
        end do

        ! Absurdly long parameter run - not a key event we understand.
        if (n > 1) call push_back(params(2:n))
    end subroutine try_csi_u

    ! Decode the parameters of a kitty CSI-u key event:
    !     <codepoint>[:<shifted>:<base>] [ ; <mods>[:<event>] ] [ ; <text> ]
    ! Returns '' for events this editor has no name for (key releases,
    ! keypad/media keys), which the caller treats as "no key".
    subroutine decode_csi_u(params, key_str, ok)
        character(len=*), intent(in) :: params
        character(len=*), intent(out) :: key_str
        logical, intent(out) :: ok
        character(len=:), allocatable :: base_name, prefix
        integer :: codepoint, mods, event, bits
        logical :: shift, alt, ctrl

        key_str = ''
        ok = .false.

        codepoint = param_int(params, 1, -1)
        if (codepoint < 0) return

        mods = param_int(params, 2, 1)
        if (mods < 1) mods = 1
        bits = mods - 1
        shift = iand(bits, 1) /= 0
        alt = iand(bits, 2) /= 0
        ctrl = iand(bits, 4) /= 0

        ! Sub-parameter of field 2 is the event type when flag 2 is on:
        ! 1 press, 2 repeat, 3 release. Only releases are dropped.
        event = param_subint(params, 2, 1)
        if (event == 3) return

        ! Ctrl-only chords keep the meaning their control byte had before the
        ! protocol was negotiated, so nothing that used to work changes.
        if (ctrl .and. .not. shift .and. .not. alt) then
            select case(codepoint)
            case(iachar('i'))
                key_str = 'tab'
                ok = .true.
                return
            case(iachar('m'), iachar('j'))
                key_str = 'enter'
                ok = .true.
                return
            case(iachar('['))
                key_str = 'esc'
                ok = .true.
                return
            end select
        end if

        base_name = csi_u_key_name(codepoint)
        if (len(base_name) == 0) return

        ! Unmodified (and shift-only, which the terminal usually delivers as
        ! plain text anyway) resolves to the bare key.
        if (.not. ctrl .and. .not. alt) then
            if (.not. shift) then
                ! Bare printable keys must arrive as the character itself,
                ! the way the legacy path delivers them to the text layer.
                if (codepoint == 32) then
                    key_str = ' '
                else
                    key_str = base_name
                end if
            else if (codepoint >= iachar('a') .and. codepoint <= iachar('z')) then
                key_str = achar(codepoint - 32)
            else if (len(base_name) > 1) then
                key_str = 'shift-' // base_name
            else
                key_str = base_name
            end if
            ok = .true.
            return
        end if

        ! Order matches the legacy modifier names: alt-, then ctrl-, then shift-
        prefix = ''
        if (alt) prefix = prefix // 'alt-'
        if (ctrl) prefix = prefix // 'ctrl-'
        if (shift) prefix = prefix // 'shift-'

        ! The one legacy name that is spelled out rather than punctuated
        if (prefix == 'alt-shift-' .and. codepoint == iachar("'")) then
            key_str = 'alt-shift-apostrophe'
            ok = .true.
            return
        end if

        key_str = prefix // base_name
        ok = .true.
    end subroutine decode_csi_u

    ! Name for a codepoint in the vocabulary the command layer already speaks.
    function csi_u_key_name(codepoint) result(name)
        integer, intent(in) :: codepoint
        character(len=:), allocatable :: name

        select case(codepoint)
        case(27)
            name = 'esc'
        case(13)
            name = 'enter'
        case(9)
            name = 'tab'
        case(127, 8)
            name = 'backspace'
        case(32)
            name = 'space'
        case(33:126)
            name = achar(codepoint)

        ! Kitty functional keys live in the Unicode private-use area. F1-F12
        ! and the keypad have names the command layer already knows; the rest
        ! (F13+, media keys, lone modifiers) map to nothing and are ignored.
        case(57364:57375)
            name = fkey_name(codepoint - 57363)
        case(57399:57408)
            name = achar(iachar('0') + codepoint - 57399)   ! keypad 0-9
        case(57409)
            name = '.'
        case(57410)
            name = '/'
        case(57411)
            name = '*'
        case(57412)
            name = '-'
        case(57413)
            name = '+'
        case(57414)
            name = 'enter'
        case(57415)
            name = '='
        case(57416)
            name = ','
        case(57417)
            name = 'left'
        case(57418)
            name = 'right'
        case(57419)
            name = 'up'
        case(57420)
            name = 'down'
        case(57421)
            name = 'pageup'
        case(57422)
            name = 'pagedown'
        case(57423)
            name = 'home'
        case(57424)
            name = 'end'
        case(57425)
            name = 'insert'
        case(57426)
            name = 'delete'

        case default
            ! Keypad, media and lone-modifier keys live in kitty's private-use
            ! range; nothing here is bound, so report no key at all.
            name = ''
        end select
    end function csi_u_key_name

    pure function fkey_name(n) result(name)
        integer, intent(in) :: n
        character(len=:), allocatable :: name
        character(len=3) :: digits

        write(digits, '(i0)') n
        name = 'f' // trim(digits)
    end function fkey_name

    ! Value of the idx-th ';'-separated parameter, up to any ':' sub-parameter.
    function param_int(params, idx, default_value) result(val)
        character(len=*), intent(in) :: params
        integer, intent(in) :: idx, default_value
        integer :: val
        character(len=:), allocatable :: field
        integer :: colon, ios

        val = default_value
        field = param_field(params, idx)
        if (len(field) == 0) return
        colon = index(field, ':')
        if (colon > 0) field = field(1:colon-1)
        if (len(field) == 0) return
        read(field, *, iostat=ios) val
        if (ios /= 0) val = default_value
    end function param_int

    ! Value of the first ':' sub-parameter of the idx-th parameter.
    function param_subint(params, idx, default_value) result(val)
        character(len=*), intent(in) :: params
        integer, intent(in) :: idx, default_value
        integer :: val
        character(len=:), allocatable :: field
        integer :: colon, colon2, ios

        val = default_value
        field = param_field(params, idx)
        colon = index(field, ':')
        if (colon <= 0) return
        field = field(colon+1:)
        colon2 = index(field, ':')
        if (colon2 > 0) field = field(1:colon2-1)
        if (len(field) == 0) return
        read(field, *, iostat=ios) val
        if (ios /= 0) val = default_value
    end function param_subint

    function param_field(params, idx) result(field)
        character(len=*), intent(in) :: params
        integer, intent(in) :: idx
        character(len=:), allocatable :: field
        integer :: i, start, count

        field = ''
        start = 1
        count = 1
        do i = 1, len(params)
            if (params(i:i) == ';') then
                if (count == idx) then
                    field = params(start:i-1)
                    return
                end if
                count = count + 1
                start = i + 1
            end if
        end do
        if (count == idx) field = params(start:len(params))
    end function param_field

    subroutine handle_escape_sequence(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch, ch1, ch2, ch3
        integer :: char_code, ios
        logical :: csi_u_handled

        key_str = 'esc'

        ! Try to read next character (with fast 5ms timeout for escape sequences)
        char_code = next_escape_char()
        if (char_code < 0) return
        ch1 = achar(char_code)

        if (ch1 == '[') then
            ! CSI sequence (or Alt+[ if no valid sequence follows)
            char_code = next_escape_char()
            if (char_code < 0) then
                ! Timeout - no character follows, this is Alt+[
                key_str = 'alt-['
                return
            end if
            ch2 = achar(char_code)

            ! A kitty keyboard-protocol key event is CSI <number> ... 'u', and
            ! the legacy sequences below start the same way, so scan ahead to
            ! the final byte. Non-'u' sequences are pushed back untouched and
            ! fall through to the original parser.
            if (ch2 >= '0' .and. ch2 <= '9') then
                call try_csi_u(ch2, key_str, csi_u_handled)
                if (csi_u_handled) return
            end if

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
            case('Z')
                ! Shift+Tab sends ESC[Z
                key_str = 'shift-tab'
            case('3')
                ! Delete key: ESC [ 3 ~ or ESC [ 3 ; modifier ~
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        key_str = 'delete'
                    else if (ch3 == ';') then
                        call handle_modified_key(key_str, 3)
                    end if
                end if
            case('5')
                ! Could be page up
                char_code = next_escape_char()
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
                char_code = next_escape_char()
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
                ! Could be function key (F1-F9) or modified arrow/home/end
                ! Check next character
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        ! F1: ESC [ 1 1 ~ (alternate format)
                        key_str = 'f1'
                    else if (ch3 == '0') then
                        ! F10 might be ESC [ 2 1 ~, check for tilde
                        char_code = next_escape_char()
                        if (char_code >= 0 .and. achar(char_code) == '~') then
                            key_str = 'f10'
                        end if
                    else if (ch3 == '1' .or. ch3 == '2' .or. ch3 == '3' .or. ch3 == '4' .or. &
                             ch3 == '5' .or. ch3 == '7' .or. ch3 == '8' .or. ch3 == '9') then
                        ! Function keys F1-F8: ESC [ 1 X ~ or ESC [ 1 X ; modifier ~
                        char_code = next_escape_char()
                        if (char_code >= 0) then
                            ch = achar(char_code)
                            if (ch == '~') then
                                ! Unmodified F1-F8
                                select case(ch3)
                                case('1')
                                    key_str = 'f1'
                                case('2')
                                    key_str = 'f2'
                                case('3')
                                    key_str = 'f3'
                                case('4')
                                    key_str = 'f4'
                                case('5')
                                    key_str = 'f5'
                                case('7')
                                    key_str = 'f6'
                                case('8')
                                    key_str = 'f7'
                                case('9')
                                    key_str = 'f8'
                                end select
                            else if (ch == ';') then
                                ! Modified F1-F8: ESC [ 1 X ; modifier ~
                                call handle_modified_function_key(key_str, '1', ch3)
                            end if
                        end if
                    else if (ch3 == ';') then
                        ! Modified arrow key or home/end: ESC [ 1 ; 2 A format
                        ! Pass key_code=1 so ~ terminator resolves to 'home'
                        call handle_modified_key(key_str, 1)
                    end if
                end if
            case('2')
                ! Could be F9-F12 or alternate modified keys
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '0' .or. ch3 == '1' .or. ch3 == '3' .or. ch3 == '4') then
                        ! Function keys F9-F12: ESC [ 2 X ~ or ESC [ 2 X ; modifier ~
                        char_code = next_escape_char()
                        if (char_code >= 0) then
                            ch = achar(char_code)
                            if (ch == '~') then
                                ! Unmodified F9-F12
                                select case(ch3)
                                case('0')
                                    key_str = 'f9'
                                case('1')
                                    key_str = 'f10'
                                case('3')
                                    key_str = 'f11'
                                case('4')
                                    key_str = 'f12'
                                end select
                            else if (ch == ';') then
                                ! Modified F9-F12: ESC [ 2 X ; modifier ~
                                call handle_modified_function_key(key_str, '2', ch3)
                            else if (ch3 == '0' .and. ch == '0') then
                                ! ESC [ 2 0 0 ~ : bracketed paste begins
                                char_code = next_escape_char()
                                if (char_code >= 0 .and. &
                                    achar(char_code) == '~') then
                                    call capture_bracketed_paste(key_str)
                                end if
                            end if
                        end if
                    else if (ch3 == ';') then
                        ! ESC [ 2 ; A format (shift+arrow)
                        char_code = next_escape_char()
                        if (char_code >= 0) then
                            ch = achar(char_code)
                            key_str = 'shift-'
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
                    else
                        ! Direct format ESC [ 2 A (we already read the 'A' in ch3)
                        key_str = 'shift-'
                        select case(ch3)
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
            case('4')
                ! End key: ESC [ 4 ~ or ESC [ 4 ; modifier ~
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        key_str = 'end'
                    else if (ch3 == ';') then
                        call handle_modified_key(key_str, 4)
                    end if
                end if
            case('7')
                ! rxvt Home: ESC [ 7 ~ or ESC [ 7 ; modifier ~
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        key_str = 'home'
                    else if (ch3 == ';') then
                        call handle_modified_key(key_str, 1)
                    end if
                end if
            case('8')
                ! rxvt End: ESC [ 8 ~ or ESC [ 8 ; modifier ~
                char_code = next_escape_char()
                if (char_code >= 0) then
                    ch3 = achar(char_code)
                    if (ch3 == '~') then
                        key_str = 'end'
                    else if (ch3 == ';') then
                        call handle_modified_key(key_str, 4)
                    end if
                end if
            case('<')
                ! Mouse event in SGR mode
                call handle_mouse_event(key_str)
            end select
        else if (ch1 == 'O') then
            ! SS3 sequence (e.g., function keys F1-F4)
            char_code = next_escape_char()
            if (char_code < 0) then
                ! Timeout - this is just Alt+O
                key_str = 'alt-o'
                return
            end if
            ch2 = achar(char_code)
            select case(ch2)
            case('H')
                key_str = 'home'
            case('F')
                key_str = 'end'
            case('P')
                key_str = 'f1'
            case('Q')
                key_str = 'f2'
            case('R')
                key_str = 'f3'
            case('S')
                key_str = 'f4'
            case default
                ! Unknown SS3 sequence - return as Alt+ch2
                write(key_str, '(a,a)') 'alt-', ch2
            end select
        else if (ch1 == achar(27)) then
            ! ESC ESC - likely Alt+something
            char_code = next_escape_char()
            if (char_code >= 0) then
                ch2 = achar(char_code)
                if (ch2 == '[') then
                    ! ESC ESC [ - Alt+arrow keys or Alt+modified keys
                    char_code = next_escape_char()
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
                        case('3')
                            ! Could be Alt+Delete (ESC ESC [ 3 ~)
                            char_code = next_escape_char()
                            if (char_code >= 0 .and. achar(char_code) == '~') then
                                key_str = 'alt-delete'
                            end if
                        case('1', '2', '4', '7', '8')
                            ! ESC ESC [ 1 ; modifier format (Alt+Shift+arrow, etc)
                            call handle_alt_modified_key(key_str)
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
        else if (ch1 >= '0' .and. ch1 <= '9') then
            ! Alt+number (for tab switching)
            write(key_str, '(a,a)') 'alt-', ch1
        else if (ch1 == "'") then
            ! Alt+apostrophe for cycle quotes
            key_str = "alt-'"
        else if (ch1 == '"') then
            ! Alt+Shift+apostrophe (double quote) for remove brackets
            key_str = "alt-shift-apostrophe"
        else if (ch1 == '[') then
            ! Alt+[ for jump to matching bracket
            key_str = "alt-["
        else if (ch1 == ']') then
            ! Alt+] for jump to matching bracket
            key_str = "alt-]"
        else if (ch1 == '.') then
            ! Alt+. for code actions
            key_str = "alt-."
        else if (ch1 == '\') then
            ! Alt+backslash for deep completion. Without this the chord only
            ! decodes under the kitty protocol, so the key silently does
            ! nothing on terminals that decline the negotiation.
            key_str = "alt-\"
        end if

    end subroutine handle_escape_sequence


    !> Modifier prefix for a legacy CSI parameter, decoded from bits.
    !>
    !> The parameter is 1 plus a bitmask: shift 1, alt 2, ctrl 4, super 8, and
    !> above that hyper, meta and the lock states. A case table over 2..9 could
    !> not express the combinations above 9, and worse, it fell through to an
    !> empty prefix -- so the caller went on to append the terminator and
    !> produced a BARE arrow. super+ctrl+left simply moved the caret.
    !>
    !> Order matches decode_csi_u and every binding already in the codebase:
    !> super, alt, ctrl, shift. Nothing here spells a chord 'ctrl-alt-'.
    pure function modifier_prefix(modifier) result(prefix)
        integer, intent(in) :: modifier
        character(len=:), allocatable :: prefix
        integer :: bits

        bits = max(0, modifier - 1)
        prefix = ''
        if (iand(bits, 8) /= 0) prefix = prefix // 'super-'
        if (iand(bits, 2) /= 0) prefix = prefix // 'alt-'
        if (iand(bits, 4) /= 0) prefix = prefix // 'ctrl-'
        if (iand(bits, 1) /= 0) prefix = prefix // 'shift-'
        ! Bit 16 hyper, 32 meta, 64 caps lock, 128 num lock: reported by some
        ! terminals and deliberately ignored, so that num lock does not make a
        ! chord unrecognisable.
    end function modifier_prefix

    subroutine handle_modified_key(key_str, caller_key_code)
        character(len=*), intent(out) :: key_str
        integer, intent(in), optional :: caller_key_code
        character :: ch, terminator
        character(len=10) :: modifier_seq
        integer :: ios, modifier, char_code, read_count

        key_str = ''  ! Initialize to empty
        modifier_seq = ''
        ch = ''  ! Initialize
        terminator = ''  ! To store the final character
        read_count = 0

        ! Read modifier sequence (e.g., ";2" for Shift)
        do
            read_count = read_count + 1
            if (read_count > 20) exit  ! Safety limit

            char_code = next_escape_char()
            if (char_code >= 0) then
                ch = achar(char_code)
                ios = 0
            else
                ios = -1
                exit
            end if
            if ((ch >= 'A' .and. ch <= 'D') .or. ch == 'H' .or. ch == 'F' .or. ch == '~' .or. ch == 'Z') then
                ! End of sequence - save the terminator
                terminator = ch
                exit
            end if
            modifier_seq = trim(modifier_seq) // ch
        end do

        ! If we didn't get a terminator, return
        if (terminator == '') return

        ! Parse modifier
        if (len_trim(modifier_seq) >= 1 .and. modifier_seq(1:1) == ';') then
            ! Standard format with leading ';': ";2" where 2 is the modifier
            if (len_trim(modifier_seq) >= 2) then
                read(modifier_seq(2:len_trim(modifier_seq)), '(i10)', iostat=ios) modifier
            else
                ios = -1
            end if
        else if (len_trim(modifier_seq) >= 1 .and. &
                 modifier_seq(1:1) >= '1' .and. modifier_seq(1:1) <= '9') then
            ! Format without leading ';' (already consumed by caller): "2" or "3" etc
            read(modifier_seq(1:len_trim(modifier_seq)), '(i10)', iostat=ios) modifier
        else
            ios = -1
        end if

        if (ios == 0) then
            key_str = modifier_prefix(modifier)

            ! Append the key type using the terminator character
            select case(terminator)
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
                ! Tilde-terminated key: resolve using caller's key code
                if (present(caller_key_code)) then
                    select case(caller_key_code)
                    case(1)
                        key_str = trim(key_str) // 'home'
                    case(2)
                        key_str = trim(key_str) // 'insert'
                    case(3)
                        key_str = trim(key_str) // 'delete'
                    case(4)
                        key_str = trim(key_str) // 'end'
                    case(5)
                        key_str = trim(key_str) // 'pageup'
                    case(6)
                        key_str = trim(key_str) // 'pagedown'
                    case default
                        key_str = ''
                    end select
                else
                    key_str = ''
                end if
            end select
        end if
    end subroutine handle_modified_key

    subroutine handle_alt_modified_key(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch, terminator
        character(len=10) :: modifier_seq
        integer :: ios, modifier, char_code, read_count

        key_str = ''
        modifier_seq = ''
        terminator = ''
        read_count = 0

        ! For ESC ESC [ 1 ; modifier format, we already read the '1'
        ! Read the rest of the sequence (should be ";modifier" then key)
        do
            read_count = read_count + 1
            if (read_count > 20) exit

            char_code = next_escape_char()
            if (char_code < 0) exit

            ch = achar(char_code)
            if ((ch >= 'A' .and. ch <= 'D') .or. ch == 'H' .or. ch == 'F') then
                terminator = ch
                exit
            end if
            modifier_seq = trim(modifier_seq) // ch
        end do

        if (terminator == '') return

        ! Parse modifier from sequence like ";4" (Alt+Shift)
        if (len_trim(modifier_seq) > 1 .and. modifier_seq(1:1) == ';') then
            if (len_trim(modifier_seq) >= 2) then
                read(modifier_seq(2:len_trim(modifier_seq)), '(i10)', iostat=ios) modifier
            else
                return
            end if

            ! Build the key string with alt- prefix
            select case(modifier)
            case(2)  ! Alt+Shift (ESC ESC [ 1 ; 2 is Alt+Shift)
                key_str = 'alt-shift-'
            case(3)  ! Alt+Alt? (unusual)
                key_str = 'alt-'
            case(4)  ! Alt+Shift (alternate)
                key_str = 'alt-shift-'
            case(5)  ! Alt+Ctrl
                key_str = 'alt-ctrl-'
            case(6)  ! Alt+Ctrl+Shift
                key_str = 'alt-ctrl-shift-'
            case default
                ! Unknown modifier with Alt
                key_str = 'alt-'
            end select

            ! Append the key
            select case(terminator)
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
            end select
        end if
    end subroutine handle_alt_modified_key

    subroutine handle_modified_special_key(key_str, key_code)
        character(len=*), intent(out) :: key_str
        integer, intent(in) :: key_code
        character :: ch
        character(len=10) :: modifier_seq
        integer :: ios, modifier, char_code

        modifier_seq = ''

        ! Read modifier sequence (already past the semicolon)
        do
            char_code = next_escape_char()
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
                key_str = modifier_prefix(modifier)

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

    subroutine handle_modified_function_key(key_str, series, fkey_code)
        character(len=*), intent(out) :: key_str
        character, intent(in) :: series      ! '1' for ESC[1X~, '2' for ESC[2X~
        character, intent(in) :: fkey_code   ! The X in ESC[1X~ or ESC[2X~
        character :: modifier_ch
        integer :: char_code, modifier
        character(len=10) :: base_key

        key_str = ''

        ! Determine base function key from series and code
        if (series == '1') then
            ! ESC[1X~ format: 1=F1, 2=F2, 3=F3, 4=F4, 5=F5, 7=F6, 8=F7, 9=F8
            select case(fkey_code)
            case('1')
                base_key = 'f1'
            case('2')
                base_key = 'f2'
            case('3')
                base_key = 'f3'
            case('4')
                base_key = 'f4'
            case('5')
                base_key = 'f5'
            case('7')
                base_key = 'f6'
            case('8')
                base_key = 'f7'
            case('9')
                base_key = 'f8'
            case default
                return
            end select
        else if (series == '2') then
            ! ESC[2X~ format: 0=F9, 1=F10, 3=F11, 4=F12
            select case(fkey_code)
            case('0')
                base_key = 'f9'
            case('1')
                base_key = 'f10'
            case('3')
                base_key = 'f11'
            case('4')
                base_key = 'f12'
            case default
                return
            end select
        else
            return
        end if

        ! Read modifier (should be a digit 2-8)
        char_code = next_escape_char()
        if (char_code < 0) return
        modifier_ch = achar(char_code)

        ! Read terminating ~
        char_code = next_escape_char()
        if (char_code < 0 .or. achar(char_code) /= '~') return

        ! Parse modifier: 2=Shift, 3=Alt, 4=Alt+Shift, 5=Ctrl, 6=Ctrl+Shift, 7=Alt+Ctrl, 8=Alt+Shift
        read(modifier_ch, '(i1)') modifier

        key_str = modifier_prefix(modifier) // trim(base_key)
    end subroutine handle_modified_function_key

    subroutine handle_mouse_event(key_str)
        character(len=*), intent(out) :: key_str
        character :: ch
        character(len=100) :: buffer
        integer :: i, ios, button, col, row, char_code
        integer :: semicolon1, semicolon2
        logical :: is_release

        buffer = ''
        i = 1
        is_release = .false.

        ! Read until 'M' (press) or 'm' (release)
        do
            char_code = next_escape_char()
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
                                ! Wheel events carry bit 6. Tested before the
                                ! modifier bits so a modified wheel (ctrl 80,
                                ! alt 72, shift 68) still reads as a wheel
                                ! rather than falling into the shift/alt/ctrl
                                ! click branches, which have no handler and
                                ! dropped it. The low two bits pick the
                                ! direction: 0 up, 1 down, 2 and 3 horizontal,
                                ! which nothing binds.
                                !
                                ! Coordinates are carried now. Without them a
                                ! wheel tick could only ever move the active
                                ! pane, so scrolling over the tab bar, the
                                ! tree or an inactive pane moved the document
                                ! the pointer was not even over.
                                if (iand(button, 64) /= 0) then
                                    select case (iand(button, 3))
                                    case (0)
                                        write(key_str, '(a,i0,a,i0,a,i0)') &
                                            'mouse-scroll-up:', button, ':', row, ':', col
                                    case (1)
                                        write(key_str, '(a,i0,a,i0,a,i0)') &
                                            'mouse-scroll-down:', button, ':', row, ':', col
                                    case default
                                        key_str = ''   ! horizontal wheel
                                    end select
                                ! Check modifiers in button code
                                else if (iand(button, 4) /= 0) then  ! Shift
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

    subroutine parse_mouse_event(key_str, event_type, button, &
        row, col, ok)
        character(len=*), intent(in) :: key_str
        character(len=*), intent(out) :: event_type
        integer, intent(out) :: button, row, col
        logical, intent(out) :: ok
        integer :: c1, c2, c3, ios

        ok = .false.
        button = 0; row = 0; col = 0; event_type = ''
        c1 = index(key_str, ':')
        if (c1 == 0) return
        c2 = index(key_str(c1+1:), ':') + c1
        if (c2 == c1) return
        c3 = index(key_str(c2+1:), ':') + c2
        if (c3 == c2) return

        event_type = key_str(1:c1-1)
        read(key_str(c1+1:c2-1), '(i10)', iostat=ios) button
        if (ios /= 0) return
        read(key_str(c2+1:c3-1), '(i10)', iostat=ios) row
        if (ios /= 0) return
        read(key_str(c3+1:), '(i10)', iostat=ios) col
        if (ios /= 0) return
        ok = .true.
    end subroutine parse_mouse_event

end module input_handler_module
