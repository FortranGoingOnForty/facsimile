program test_completion_sanitize
    ! The airtight layer. This is the heaviest suite in the project on
    ! purpose: it is the definition of "no leakage of hallucinations", and
    ! nothing reaches ghost state until it passes.
    !
    ! The governing rule is REJECT OVER REPAIR. A rejection costs the user
    ! nothing -- no ghost appears, exactly as before the feature existed. A
    ! repaired-but-wrong completion costs them a silent defect in their code.
    !
    ! The single most important group is terminal control bytes: ghost text is
    ! written straight to the terminal, so an ESC in model output is EXECUTED.
    ! An attacker-influenced or merely confused model could clear the screen,
    ! move the cursor, or emit OSC 52 to write the system clipboard.
    use completion_sanitize_module
    implicit none

    character, parameter :: ESC = achar(27)
    character, parameter :: NL  = achar(10)
    character, parameter :: TAB = achar(9)

    integer :: nfail
    nfail = 0

    call test_accepts_ordinary_code()
    call test_control_bytes()
    call test_invalid_utf8()
    call test_special_tokens()
    call test_chat_shapes()
    call test_repetition()
    call test_suffix_echo()
    call test_budgets()
    call test_line_modes()
    call test_whitespace()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All completion-sanitizer tests passed'

contains

    ! --- what must get through ---
    subroutine test_accepts_ordinary_code()
        call accepts('return a + b;', '', 1, 'return a + b;', 'a plain statement')
        call accepts('    return n;', '', 1, '    return n;', 'leading indent kept')
        call accepts('x = vector<int>();', '', 1, 'x = vector<int>();', &
                     'angle brackets are ordinary code')
        call accepts('a && b || !c', '', 1, 'a && b || !c', 'operators pass')
        call accepts('caf' // char(195) // char(169) // ' = 1', '', 1, &
                     'caf' // char(195) // char(169) // ' = 1', 'UTF-8 passes')
        call accepts('s[i++] = ' // char(39) // 'x' // char(39) // ';', '', 1, &
                     's[i++] = ' // char(39) // 'x' // char(39) // ';', &
                     'quotes and brackets pass')
        call accepts('while (n) {' // NL // '    n--;' // NL // '}', '', 3, &
                     'while (n) {' // NL // '    n--;' // NL // '}', &
                     'a multi-line block in block mode')
        call accepts('a' // TAB // 'b', '', 1, 'a' // TAB // 'b', &
                     'tab is allowed (it is real whitespace)')
    end subroutine test_accepts_ordinary_code

    ! --- THE critical group: bytes the terminal would execute ---
    subroutine test_control_bytes()
        call rejects(ESC // '[2J', '', 1, SAN_CONTROL_BYTES, 'clear-screen sequence')
        call rejects('ok' // ESC // '[31mred', '', 1, SAN_CONTROL_BYTES, &
                     'colour sequence mid-text')
        call rejects(ESC // '[999;999H', '', 1, SAN_CONTROL_BYTES, &
                     'cursor teleport')
        call rejects(ESC // ']52;c;cGFzc3dk' // achar(7), '', 1, SAN_CONTROL_BYTES, &
                     'OSC 52 clipboard write')
        call rejects(ESC // ']0;title' // achar(7), '', 1, SAN_CONTROL_BYTES, &
                     'OSC window title')
        call rejects(ESC // '[?1049h', '', 1, SAN_CONTROL_BYTES, &
                     'alternate screen switch')
        call rejects(ESC // '[?1000h', '', 1, SAN_CONTROL_BYTES, &
                     'mouse reporting enable')
        call rejects('a' // achar(0) // 'b', '', 1, SAN_CONTROL_BYTES, &
                     'NUL (the gap buffer sentinel)')
        call rejects('a' // achar(13) // 'b', '', 1, SAN_CONTROL_BYTES, &
                     'carriage return')
        call rejects('a' // achar(7), '', 1, SAN_CONTROL_BYTES, 'bell')
        call rejects('a' // achar(8) // 'b', '', 1, SAN_CONTROL_BYTES, 'backspace')
        call rejects('a' // achar(14) // 'b', '', 1, SAN_CONTROL_BYTES, &
                     'shift-out (permanently garbles the charset)')
        call rejects('a' // achar(15) // 'b', '', 1, SAN_CONTROL_BYTES, 'shift-in')
        call rejects('a' // achar(11) // 'b', '', 1, SAN_CONTROL_BYTES, 'vertical tab')
        call rejects('a' // achar(12) // 'b', '', 1, SAN_CONTROL_BYTES, 'form feed')
        call rejects('a' // achar(127) // 'b', '', 1, SAN_CONTROL_BYTES, 'DEL')
        call rejects('a' // achar(1) // 'b', '', 1, SAN_CONTROL_BYTES, 'SOH')
    end subroutine test_control_bytes

    ! --- invalid UTF-8 desynchronises every character-column walk ---
    subroutine test_invalid_utf8()
        call rejects('a' // char(255) // 'b', '', 1, SAN_BAD_UTF8, '0xFF is never valid')
        call rejects('a' // char(254) // 'b', '', 1, SAN_BAD_UTF8, '0xFE is never valid')
        call rejects('a' // char(128) // 'b', '', 1, SAN_BAD_UTF8, &
                     'a continuation byte as lead')
        call rejects('a' // char(195), '', 1, SAN_BAD_UTF8, &
                     'truncated 2-byte sequence')
        call rejects('a' // char(228) // char(184), '', 1, SAN_BAD_UTF8, &
                     'truncated 3-byte sequence')
        call rejects('a' // char(195) // 'z', '', 1, SAN_BAD_UTF8, &
                     'lead byte followed by ASCII')
        ! Overlong encoding of '/' -- a classic filter bypass
        call rejects('a' // char(192) // char(175) // 'b', '', 1, SAN_BAD_UTF8, &
                     'overlong 2-byte encoding')
        ! U+D800 encoded in UTF-8, which is illegal
        call rejects('a' // char(237) // char(160) // char(128), '', 1, SAN_BAD_UTF8, &
                     'surrogate encoded as UTF-8')
        ! Past U+10FFFF
        call rejects('a' // char(245) // char(128) // char(128) // char(128), '', 1, &
                     SAN_BAD_UTF8, 'codepoint beyond U+10FFFF')
    end subroutine test_invalid_utf8

    ! --- a special token means the template is wrong ---
    subroutine test_special_tokens()
        call rejects('<think>let me reason</think>x = 1', '', 1, SAN_SPECIAL_TOKEN, &
                     'thinking trace (qwen3.5 does this)')
        call rejects('x = 1<|endoftext|>', '', 1, SAN_SPECIAL_TOKEN, 'endoftext')
        call rejects('<|fim_middle|>x', '', 1, SAN_SPECIAL_TOKEN, 'fim_middle leaked')
        call rejects('a<|im_start|>b', '', 1, SAN_SPECIAL_TOKEN, 'chat template token')
        call rejects('<fim_suffix>y', '', 1, SAN_SPECIAL_TOKEN, 'bare fim tag')
    end subroutine test_special_tokens

    ! --- chat-shaped output is a restatement, not a continuation ---
    subroutine test_chat_shapes()
        call rejects('```c' // NL // 'int x;' // NL // '```', '', 3, SAN_CHAT_SHAPE, &
                     'fenced code block')
        call rejects('```' // NL // 'x', '', 3, SAN_CHAT_SHAPE, 'bare fence')
        call rejects('Certainly! It looks like you are defining', '', 1, &
                     SAN_CHAT_SHAPE, 'the exact prose a prompt-only request returned')
        call rejects('Here is the completion:', '', 1, SAN_CHAT_SHAPE, 'here is')
        call rejects('Sure, here you go', '', 1, SAN_CHAT_SHAPE, 'sure')
        call rejects('This function returns the length', '', 1, SAN_CHAT_SHAPE, &
                     'explanatory prose')
        call rejects('  ' // NL // '```python', '', 3, SAN_CHAT_SHAPE, &
                     'fence after leading blank lines')

        ! ...but code that merely starts with one of those words is fine
        call accepts('sure_thing = 1;', '', 1, 'sure_thing = 1;', &
                     'an identifier starting with a stop word is not prose')
        call accepts('here_doc(x);', '', 1, 'here_doc(x);', &
                     'here_doc is code, not a lead-in')
    end subroutine test_chat_shapes

    ! --- base models loop at low temperature ---
    subroutine test_repetition()
        call rejects('a = 1;' // NL // 'a = 1;' // NL // 'a = 1;', '', 5, &
                     SAN_REPETITION, 'three identical consecutive lines')
        call rejects('x;' // NL // 'x;' // NL // 'x;' // NL // 'x;' // NL // 'x;', &
                     '', 8, SAN_REPETITION, 'five identical lines')

        ! two in a row is a legitimate pattern
        call accepts('a = 1;' // NL // 'a = 1;', '', 3, 'a = 1;' // NL // 'a = 1;', &
                     'two identical lines are allowed')
        call accepts('i++;' // NL // 'j++;' // NL // 'i++;', '', 3, &
                     'i++;' // NL // 'j++;' // NL // 'i++;', &
                     'alternating lines are not a loop')
    end subroutine test_repetition

    ! --- the classic FIM failure ---
    subroutine test_suffix_echo()
        ! caret inside foo(|), model completes "bar)" -> accept would give
        ! foo(bar)) without this
        call accepts('bar)', ')', 1, 'bar', 'trailing ) that is already there is dropped')
        call accepts('n;' // NL // '}', NL // '}', 2, 'n;', &
                     'a closing brace already below is dropped')
        call rejects(')', ')', 1, SAN_DUPLICATES_SUFFIX, &
                     'a completion that is only the existing suffix')
        call accepts('a + b', '', 1, 'a + b', 'nothing after the cursor, nothing dropped')
        call accepts('x);', ');', 1, 'x', 'multi-char suffix echo dropped')
    end subroutine test_suffix_echo

    subroutine test_budgets()
        character(len=:), allocatable :: big
        integer :: i

        big = ''
        do i = 1, 300
            big = big // 'abcdefgh'          ! 2400 bytes, no repetition trigger
        end do
        call rejects(big, '', 1, SAN_TOO_LONG, 'past the accept budget rejects')

        call accepts(repeat('x', 100), '', 1, repeat('x', 100), &
                     'a long but reasonable line passes')
    end subroutine test_budgets

    subroutine test_line_modes()
        call accepts('one' // NL // 'two' // NL // 'three', '', 1, 'one', &
                     'single-line mode cuts at the first newline')
        call accepts('one' // NL // 'two' // NL // 'three', '', 2, &
                     'one' // NL // 'two', 'block mode honours the line cap')
        call accepts('one' // NL // 'two', '', 9, 'one' // NL // 'two', &
                     'a cap larger than the text is fine')
    end subroutine test_line_modes

    subroutine test_whitespace()
        call rejects('', '', 1, SAN_EMPTY, 'empty input')
        call rejects('   ', '', 1, SAN_EMPTY, 'spaces only')
        call rejects(NL // NL, '', 3, SAN_EMPTY, 'newlines only')
        call rejects(TAB // ' ' // TAB, '', 1, SAN_EMPTY, 'tabs and spaces only')
        call accepts('x = 1;   ', '', 1, 'x = 1;', 'trailing spaces trimmed')
        call accepts('x = 1;' // NL // NL, '', 3, 'x = 1;', 'trailing blank lines dropped')
    end subroutine test_whitespace

    ! ------------------------------------------------------------------

    subroutine accepts(raw, after, max_lines, want, name)
        character(len=*), intent(in) :: raw, after, want, name
        integer, intent(in) :: max_lines
        character(len=:), allocatable :: out
        integer :: code

        call sanitize_completion(raw, after, max_lines, out, code)
        if (code == SAN_OK .and. out == want) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (code=' // sanitize_reason(code) // &
                  ' out="' // visible(out) // '" want="' // visible(want) // '")'
            nfail = nfail + 1
        end if
    end subroutine accepts

    subroutine rejects(raw, after, max_lines, want_code, name)
        character(len=*), intent(in) :: raw, after, name
        integer, intent(in) :: max_lines, want_code
        character(len=:), allocatable :: out
        integer :: code

        call sanitize_completion(raw, after, max_lines, out, code)
        if (code == want_code .and. len(out) == 0) then
            print '(a)', 'PASS: rejected -- ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (expected ' // sanitize_reason(want_code) // &
                  ', got ' // sanitize_reason(code) // ' out="' // visible(out) // '")'
            nfail = nfail + 1
        end if
    end subroutine rejects

    ! Never print a raw control byte from a failing case into the terminal --
    ! that is the very hazard under test.
    function visible(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i, b

        out = ''
        do i = 1, len(s)
            b = iachar(s(i:i))
            if (b < 32 .or. b == 127) then
                out = out // '<' // hex2(b) // '>'
            else
                out = out // s(i:i)
            end if
        end do
    end function visible

    function hex2(v) result(t)
        integer, intent(in) :: v
        character(len=2) :: t
        character(len=*), parameter :: D = '0123456789abcdef'
        t = D(v/16+1:v/16+1) // D(mod(v,16)+1:mod(v,16)+1)
    end function hex2

end program test_completion_sanitize
