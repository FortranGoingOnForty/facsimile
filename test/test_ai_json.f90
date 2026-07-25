program test_ai_json
    ! JSON for model traffic. Two properties matter:
    !
    !   1. Requests carry arbitrary user source. Any byte that cannot appear
    !      literally in a JSON string must be escaped, or Go rejects the whole
    !      request with 400 and completion fails silently and permanently on
    !      exactly the files that trip it.
    !
    !   2. Responses carry arbitrary model output, and ollama HTML-escapes
    !      < > & through Go's encoding/json. A decoder without \uXXXX support
    !      puts the literal text a>b into the editor. This was verified
    !      against a live model before the module was written.
    use ai_json_module
    implicit none

    integer :: nfail
    character(len=:), allocatable :: out
    logical :: ok

    nfail = 0

    call test_escape()
    call test_decode_basic()
    call test_decode_html_escapes()
    call test_decode_unicode()
    call test_decode_rejects()
    call test_find_key()
    call test_real_ollama_shapes()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All ai_json tests passed'

contains

    subroutine test_escape()
        call eq(ai_json_escape('plain'), 'plain', 'plain text is untouched')
        call eq(ai_json_escape('a"b'), 'a\"b', 'quote is escaped')
        call eq(ai_json_escape('a\b'), 'a\\b', 'backslash is escaped')
        call eq(ai_json_escape('a' // achar(10) // 'b'), 'a\nb', 'newline short form')
        call eq(ai_json_escape('a' // achar(9) // 'b'), 'a\tb', 'tab short form')
        call eq(ai_json_escape('a' // achar(13) // 'b'), 'a\rb', 'CR short form')

        ! The ones json_module gets wrong: raw control bytes are invalid JSON.
        ! Expected values are built from achar so no literal escape text in
        ! this file can be mangled by an editor or a patch tool.
        call eq(ai_json_escape('a' // achar(27) // 'b'), 'a' // u00('1b') // 'b', &
                'ESC is escaped, not emitted as a raw byte')
        call eq(ai_json_escape('a' // achar(0) // 'b'), 'a' // u00('00') // 'b', &
                'NUL is escaped (the gap buffer uses it as a sentinel)')
        call eq(ai_json_escape('a' // achar(1) // 'b'), 'a' // u00('01') // 'b', &
                'SOH is escaped')
        call eq(ai_json_escape('a' // achar(31) // 'b'), 'a' // u00('1f') // 'b', &
                'US is escaped')
        call eq(ai_json_escape('a' // achar(127) // 'b'), 'a' // u00('7f') // 'b', &
                'DEL is escaped')

        ! UTF-8 must pass through byte for byte
        call eq(ai_json_escape('caf' // char(195) // char(169)), &
                'caf' // char(195) // char(169), 'UTF-8 passes through verbatim')
        call eq(ai_json_escape(''), '', 'empty string')

        ! < > & are NOT our job to escape -- we are not producing HTML
        call eq(ai_json_escape('a<b>c&d'), 'a<b>c&d', 'angle brackets pass through')
    end subroutine test_escape

    subroutine test_decode_basic()
        call ai_json_decode_string('"hello"', out, ok)
        call check(ok .and. out == 'hello', 'plain string decodes', out)

        call ai_json_decode_string('"a\nb"', out, ok)
        call check(ok .and. out == 'a' // achar(10) // 'b', 'newline decodes', out)

        call ai_json_decode_string('"a\tb"', out, ok)
        call check(ok .and. out == 'a' // achar(9) // 'b', 'tab decodes', out)

        call ai_json_decode_string('"a\"b"', out, ok)
        call check(ok .and. out == 'a"b', 'escaped quote decodes', out)

        call ai_json_decode_string('"a\\b"', out, ok)
        call check(ok .and. out == 'a\b', 'escaped backslash decodes', out)

        call ai_json_decode_string('"a\/b"', out, ok)
        call check(ok .and. out == 'a/b', 'escaped solidus decodes', out)

        call ai_json_decode_string('""', out, ok)
        call check(ok .and. len(out) == 0, 'empty string decodes', out)
    end subroutine test_decode_basic

    ! The bug that motivated this module. Inputs are assembled from achar so
    ! the escape sequences under test cannot themselves be mangled in this
    ! file -- if they were, these assertions would silently become vacuous.
    subroutine test_decode_html_escapes()
        call ai_json_decode_string('"a' // u('003e') // 'b?1:-1"', out, ok)
        call check(ok .and. out == 'a>b?1:-1', &
                   'ollama''s \u003e becomes > (verified live response)', out)

        call ai_json_decode_string('"x ' // u('003c') // ' y"', out, ok)
        call check(ok .and. out == 'x < y', '\u003c becomes <', out)

        call ai_json_decode_string('"a ' // u('0026') // u('0026') // ' b"', out, ok)
        call check(ok .and. out == 'a && b', '\u0026 becomes &', out)

        call ai_json_decode_string('"vector' // u('003c') // 'int' // u('003e') // ' v"', &
                                   out, ok)
        call check(ok .and. out == 'vector<int> v', &
                   'a C++ template survives intact', out)
    end subroutine test_decode_html_escapes

    subroutine test_decode_unicode()
        call ai_json_decode_string('"caf' // u('00e9') // '"', out, ok)
        call check(ok .and. out == 'caf' // char(195) // char(169), &
                   '2-byte UTF-8 from \u00e9', out)

        call ai_json_decode_string('"' // u('4e2d') // '"', out, ok)
        call check(ok .and. out == char(228) // char(184) // char(173), &
                   '3-byte UTF-8 from \u4e2d', out)

        ! Surrogate pair -> U+1F600, four UTF-8 bytes
        call ai_json_decode_string('"' // u('d83d') // u('de00') // '"', out, ok)
        call check(ok .and. len(out) == 4 .and. &
                   out == char(240) // char(159) // char(152) // char(128), &
                   'surrogate pair recombines into 4-byte UTF-8', out)

        call ai_json_decode_string('"a' // u('0041') // 'b"', out, ok)
        call check(ok .and. out == 'aAb', 'ASCII via \u decodes', out)
    end subroutine test_decode_unicode

    ! An escape we do not understand means the scanner has lost sync with the
    ! framing. Passing it through would push a literal backslash sequence
    ! towards the buffer, so the whole value is rejected.
    subroutine test_decode_rejects()
        call ai_json_decode_string('"a\qb"', out, ok)
        call check(.not. ok, 'unknown escape rejects the value', out)

        call ai_json_decode_string('"a' // u('d83d') // 'b"', out, ok)
        call check(.not. ok, 'high surrogate with no low surrogate rejects', out)

        call ai_json_decode_string('"a' // u('de00') // 'b"', out, ok)
        call check(.not. ok, 'lone low surrogate rejects', out)

        call ai_json_decode_string('"a' // achar(92) // 'u00"', out, ok)
        call check(.not. ok, 'truncated \u rejects', out)

        call ai_json_decode_string('"a' // u('ZZZZ') // '"', out, ok)
        call check(.not. ok, 'non-hex \u rejects', out)

        call ai_json_decode_string('no quotes', out, ok)
        call check(.not. ok, 'unquoted input rejects', out)

        call ai_json_decode_string('"unterminated', out, ok)
        call check(.not. ok, 'unterminated string rejects', out)
    end subroutine test_decode_rejects

    subroutine test_find_key()
        character(len=*), parameter :: doc = &
            '{"model":"m","done":true,"n":42,"nested":{"model":"WRONG"},"tail":"end"}'

        call ai_json_get_string(doc, 'model', out, ok)
        call check(ok .and. out == 'm', 'top-level string found', out)

        call check(ai_json_get_logical(doc, 'done', .false.), 'boolean read', '')
        call check(ai_json_get_integer(doc, 'n', 0) == 42, 'integer read', '')

        call ai_json_get_string(doc, 'tail', out, ok)
        call check(ok .and. out == 'end', 'key after a nested object found', out)

        ! The important one: a key inside a nested object must not shadow the
        ! real top-level value.
        call ai_json_get_string(doc, 'model', out, ok)
        call check(out /= 'WRONG', 'nested key does not shadow the top-level one', out)

        call ai_json_get_string(doc, 'absent', out, ok)
        call check(.not. ok, 'missing key reports not-found', out)
    end subroutine test_find_key

    ! Shapes taken from real ollama responses.
    subroutine test_real_ollama_shapes()
        character(len=:), allocatable :: gen
        character(len=*), parameter :: show = &
            '{"details":{"family":"qwen2"},"capabilities":["completion","insert"]}'
        character(len=*), parameter :: nofim = &
            '{"capabilities":["completion","vision","tools"]}'
        character(len=*), parameter :: err = '{"error":"model ''x'' not found"}'

        ! Built at runtime so the \u003e under test is assembled from achar
        gen = '{"model":"qwen2.5-coder:1.5b-base","created_at":"2026-07-25T03:05:53Z",' // &
              '"response":"if (a ' // u('003e') // ' b)' // achar(92) // 'n' // &
              '        return b;","done":true,' // &
              '"done_reason":"length","context":[151659,396,912,1548],' // &
              '"total_duration":548644015,"eval_count":4}'

        call ai_json_get_string(gen, 'response', out, ok)
        call check(ok .and. out == 'if (a > b)' // achar(10) // '        return b;', &
                   'a real generate response decodes, > and all', out)
        call check(ai_json_get_logical(gen, 'done', .false.), 'done flag read', '')
        call ai_json_get_string(gen, 'done_reason', out, ok)
        call check(ok .and. out == 'length', 'done_reason read', out)
        call check(ai_json_get_integer(gen, 'eval_count', 0) == 4, &
                   'eval_count read past the context array', '')

        ! The context array is thousands of ints in practice; the scanner must
        ! step over it without descending.
        call ai_json_get_string(gen, 'model', out, ok)
        call check(ok .and. out == 'qwen2.5-coder:1.5b-base', 'model name read', out)

        call check(ai_json_array_has_string(show, 'capabilities', 'insert'), &
                   'insert capability detected', '')
        call check(.not. ai_json_array_has_string(nofim, 'capabilities', 'insert'), &
                   'a model without FIM is detected as such', '')

        call ai_json_get_string(err, 'error', out, ok)
        call check(ok .and. index(out, 'not found') > 0, 'error body reads', out)
    end subroutine test_real_ollama_shapes

    ! The six characters  \ u 0 0 X X, assembled so the source contains no
    ! escape sequence that anything could reinterpret.
    ! The six characters  \ u X X X X
    function u(hhhh) result(t)
        character(len=4), intent(in) :: hhhh
        character(len=6) :: t
        t = achar(92) // 'u' // hhhh
    end function u

    function u00(hh) result(t)
        character(len=2), intent(in) :: hh
        character(len=6) :: t
        t = achar(92) // 'u00' // hh
    end function u00

    subroutine eq(got, want, name)
        character(len=*), intent(in) :: got, want, name
        call check(got == want, name, got)
    end subroutine eq

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // got // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ai_json
