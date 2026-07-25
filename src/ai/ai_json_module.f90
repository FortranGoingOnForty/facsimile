! JSON for the model backend: a request builder and a field extractor.
!
! This is deliberately NOT a general JSON library, and deliberately not
! json_module, which serves LSP and stays as it is. Three independent reasons,
! each sufficient on its own:
!
!   * json_module's escape_string handles only " \ \b \f \n \r \t and passes
!     every other byte through, including 0x00-0x1F. Source buffers contain
!     ESC in string literals, and fac's gap buffer uses char(0) as a sentinel.
!     Raw control bytes in a JSON string are invalid per RFC 8259 and Go
!     rejects the whole request with 400 -- silent, permanent completion
!     failure on the files that trip it.
!
!   * Go's encoding/json HTML-escapes < > & by default, so ollama really does
!     return "a>b" for "a>b". json_module keeps unknown escapes verbatim,
!     which would put the literal text a>b into the ghost. Verified
!     against a live model, not theorised.
!
!   * It leaks every parsed object and builds strings one character at a time.
!     On a path that fires several times a second, that is an always-on leak.
!
! Everything here is single-pass with one allocation, and unknown escapes
! reject the whole value rather than being passed through -- an unrecognised
! escape means the scanner has lost sync with the framing, and continuing
! risks pushing a literal backslash sequence towards a buffer.
module ai_json_module
    implicit none
    private

    public :: ai_json_escape, ai_json_find_key, ai_json_decode_string
    public :: ai_json_get_string, ai_json_get_logical, ai_json_get_integer
    public :: ai_json_array_has_string

contains

    ! JSON-escape a string. UTF-8 passes through untouched; every byte that
    ! cannot appear literally becomes a short escape or \u00XX.
    function ai_json_escape(src) result(out)
        character(len=*), intent(in) :: src
        character(len=:), allocatable :: out
        character(len=:), allocatable :: buf
        integer :: i, n, b
        character(len=4), parameter :: HEX = '0123'   ! placeholder, see hex_digit

        allocate(character(len=6 * len(src) + 2) :: buf)
        n = 0

        do i = 1, len(src)
            b = iachar(src(i:i))
            select case(b)
            case(34)    ! "
                buf(n+1:n+2) = '\"'; n = n + 2
            case(92)    ! backslash
                buf(n+1:n+2) = '\\'; n = n + 2
            case(8)
                buf(n+1:n+2) = '\b'; n = n + 2
            case(12)
                buf(n+1:n+2) = '\f'; n = n + 2
            case(10)
                buf(n+1:n+2) = '\n'; n = n + 2
            case(13)
                buf(n+1:n+2) = '\r'; n = n + 2
            case(9)
                buf(n+1:n+2) = '\t'; n = n + 2
            case default
                if (b < 32 .or. b == 127) then
                    buf(n+1:n+4) = '\u00'
                    buf(n+5:n+5) = hex_digit(b / 16)
                    buf(n+6:n+6) = hex_digit(mod(b, 16))
                    n = n + 6
                else
                    n = n + 1
                    buf(n:n) = src(i:i)
                end if
            end select
        end do

        out = buf(1:n)
        if (.false.) print *, HEX     ! silence unused-parameter warning
    end function ai_json_escape

    pure function hex_digit(v) result(c)
        integer, intent(in) :: v
        character :: c
        if (v < 10) then
            c = achar(iachar('0') + v)
        else
            c = achar(iachar('a') + v - 10)
        end if
    end function hex_digit

    ! Locate a TOP-LEVEL key and return the byte span of its raw value.
    ! Scanning only at depth 1 means a key nested inside "details" or an
    ! element of the multi-thousand-entry "context" array can never be
    ! mistaken for the real one -- and skipping those costs nothing, because
    ! we never descend into them.
    subroutine ai_json_find_key(doc, key, vstart, vend, ok)
        character(len=*), intent(in) :: doc, key
        integer, intent(out) :: vstart, vend
        logical, intent(out) :: ok
        integer :: i, n, depth, ks, ke, j
        logical :: in_string, escaped

        ok = .false.
        vstart = 0
        vend = 0
        n = len(doc)
        depth = 0
        in_string = .false.
        escaped = .false.
        i = 1

        do while (i <= n)
            if (in_string) then
                if (escaped) then
                    escaped = .false.
                else if (doc(i:i) == '\') then
                    escaped = .true.
                else if (doc(i:i) == '"') then
                    in_string = .false.
                end if
                i = i + 1
                cycle
            end if

            select case(doc(i:i))
            case('{', '[')
                depth = depth + 1
                i = i + 1
                cycle
            case('}', ']')
                depth = depth - 1
                i = i + 1
                cycle
            case('"')
                ! A string at depth 1 may be the key we want
                ks = i + 1
                j = ks
                do while (j <= n)
                    if (doc(j:j) == '\') then
                        j = j + 2
                        cycle
                    end if
                    if (doc(j:j) == '"') exit
                    j = j + 1
                end do
                if (j > n) return
                ke = j - 1

                if (depth == 1) then
                    if (ke >= ks) then
                        if (doc(ks:ke) == key) then
                            ! Expect optional whitespace then ':'
                            j = j + 1
                            do while (j <= n)
                                if (doc(j:j) /= ' ' .and. doc(j:j) /= achar(9) .and. &
                                    doc(j:j) /= achar(10) .and. doc(j:j) /= achar(13)) exit
                                j = j + 1
                            end do
                            if (j <= n) then
                                if (doc(j:j) == ':') then
                                    call span_of_value(doc, j + 1, vstart, vend, ok)
                                    return
                                end if
                            end if
                        end if
                    end if
                end if
                i = j + 1
                cycle
            case default
                i = i + 1
            end select
        end do
    end subroutine ai_json_find_key

    ! Byte span of the value starting at or after `from`.
    subroutine span_of_value(doc, from, vstart, vend, ok)
        character(len=*), intent(in) :: doc
        integer, intent(in) :: from
        integer, intent(out) :: vstart, vend
        logical, intent(out) :: ok
        integer :: i, n, depth
        logical :: in_string, escaped

        ok = .false.
        vstart = 0
        vend = 0
        n = len(doc)
        i = from

        do while (i <= n)
            if (doc(i:i) /= ' ' .and. doc(i:i) /= achar(9) .and. &
                doc(i:i) /= achar(10) .and. doc(i:i) /= achar(13)) exit
            i = i + 1
        end do
        if (i > n) return
        vstart = i

        if (doc(i:i) == '"') then
            i = i + 1
            do while (i <= n)
                if (doc(i:i) == '\') then
                    i = i + 2
                    cycle
                end if
                if (doc(i:i) == '"') then
                    vend = i
                    ok = .true.
                    return
                end if
                i = i + 1
            end do
            return
        end if

        if (doc(i:i) == '{' .or. doc(i:i) == '[') then
            depth = 0
            in_string = .false.
            escaped = .false.
            do while (i <= n)
                if (in_string) then
                    if (escaped) then
                        escaped = .false.
                    else if (doc(i:i) == '\') then
                        escaped = .true.
                    else if (doc(i:i) == '"') then
                        in_string = .false.
                    end if
                else
                    select case(doc(i:i))
                    case('"')
                        in_string = .true.
                    case('{', '[')
                        depth = depth + 1
                    case('}', ']')
                        depth = depth - 1
                        if (depth == 0) then
                            vend = i
                            ok = .true.
                            return
                        end if
                    end select
                end if
                i = i + 1
            end do
            return
        end if

        ! Bare literal: number, true, false, null
        do while (i <= n)
            select case(doc(i:i))
            case(',', '}', ']', ' ', achar(9), achar(10), achar(13))
                exit
            end select
            i = i + 1
        end do
        vend = i - 1
        ok = vend >= vstart
    end subroutine span_of_value

    ! Decode a JSON string value INCLUDING the surrounding quotes.
    ! Handles \uXXXX with UTF-16 surrogate recombination. Any escape we do not
    ! recognise fails the whole decode rather than being passed through.
    subroutine ai_json_decode_string(raw, out, ok)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: ok
        character(len=:), allocatable :: buf
        integer :: i, n, w, cp, lo

        ok = .false.
        out = ''
        n = len(raw)
        if (n < 2) return
        if (raw(1:1) /= '"' .or. raw(n:n) /= '"') return

        ! Unescaping never grows the byte count: \uXXXX is 6 bytes in and at
        ! most 3 out; a surrogate pair is 12 in and 4 out.
        allocate(character(len=max(1, n)) :: buf)
        w = 0
        i = 2

        do while (i <= n - 1)
            if (raw(i:i) /= '\') then
                w = w + 1
                buf(w:w) = raw(i:i)
                i = i + 1
                cycle
            end if

            if (i + 1 > n - 1) return
            select case(raw(i+1:i+1))
            case('"')
                w = w + 1; buf(w:w) = '"';        i = i + 2
            case('\')
                w = w + 1; buf(w:w) = '\';        i = i + 2
            case('/')
                w = w + 1; buf(w:w) = '/';        i = i + 2
            case('b')
                w = w + 1; buf(w:w) = achar(8);   i = i + 2
            case('f')
                w = w + 1; buf(w:w) = achar(12);  i = i + 2
            case('n')
                w = w + 1; buf(w:w) = achar(10);  i = i + 2
            case('r')
                w = w + 1; buf(w:w) = achar(13);  i = i + 2
            case('t')
                w = w + 1; buf(w:w) = achar(9);   i = i + 2
            case('u')
                if (i + 5 > n - 1) return
                cp = hex4(raw(i+2:i+5))
                if (cp < 0) return
                i = i + 6
                if (cp >= 55296 .and. cp <= 56319) then
                    ! High surrogate: a low surrogate must follow
                    if (i + 5 > n - 1) return
                    if (raw(i:i+1) /= '\u') return
                    lo = hex4(raw(i+2:i+5))
                    if (lo < 56320 .or. lo > 57343) return
                    cp = 65536 + (cp - 55296) * 1024 + (lo - 56320)
                    i = i + 6
                else if (cp >= 56320 .and. cp <= 57343) then
                    return      ! lone low surrogate
                end if
                call utf8_encode_into(cp, buf, w)
            case default
                ! Unknown escape: the scanner has lost sync with the framing.
                return
            end select
        end do

        out = buf(1:w)
        ok = .true.
    end subroutine ai_json_decode_string

    pure function hex4(s) result(v)
        character(len=4), intent(in) :: s
        integer :: v, i, d

        v = 0
        do i = 1, 4
            select case(s(i:i))
            case('0':'9')
                d = iachar(s(i:i)) - iachar('0')
            case('a':'f')
                d = iachar(s(i:i)) - iachar('a') + 10
            case('A':'F')
                d = iachar(s(i:i)) - iachar('A') + 10
            case default
                v = -1
                return
            end select
            v = v * 16 + d
        end do
    end function hex4

    subroutine utf8_encode_into(cp, buf, w)
        integer, intent(in) :: cp
        character(len=*), intent(inout) :: buf
        integer, intent(inout) :: w

        if (cp < 128) then
            w = w + 1; buf(w:w) = achar(cp)
        else if (cp < 2048) then
            w = w + 1; buf(w:w) = achar(192 + cp / 64)
            w = w + 1; buf(w:w) = achar(128 + mod(cp, 64))
        else if (cp < 65536) then
            w = w + 1; buf(w:w) = achar(224 + cp / 4096)
            w = w + 1; buf(w:w) = achar(128 + mod(cp / 64, 64))
            w = w + 1; buf(w:w) = achar(128 + mod(cp, 64))
        else
            w = w + 1; buf(w:w) = achar(240 + cp / 262144)
            w = w + 1; buf(w:w) = achar(128 + mod(cp / 4096, 64))
            w = w + 1; buf(w:w) = achar(128 + mod(cp / 64, 64))
            w = w + 1; buf(w:w) = achar(128 + mod(cp, 64))
        end if
    end subroutine utf8_encode_into

    ! ------------------------------------------------------------------
    ! Convenience readers
    ! ------------------------------------------------------------------

    subroutine ai_json_get_string(doc, key, val, ok)
        character(len=*), intent(in) :: doc, key
        character(len=:), allocatable, intent(out) :: val
        logical, intent(out) :: ok
        integer :: vs, ve

        val = ''
        call ai_json_find_key(doc, key, vs, ve, ok)
        if (.not. ok) return
        call ai_json_decode_string(doc(vs:ve), val, ok)
    end subroutine ai_json_get_string

    function ai_json_get_logical(doc, key, default_value) result(v)
        character(len=*), intent(in) :: doc, key
        logical, intent(in) :: default_value
        logical :: v
        integer :: vs, ve
        logical :: ok

        v = default_value
        call ai_json_find_key(doc, key, vs, ve, ok)
        if (.not. ok) return
        if (doc(vs:ve) == 'true') then
            v = .true.
        else if (doc(vs:ve) == 'false') then
            v = .false.
        end if
    end function ai_json_get_logical

    function ai_json_get_integer(doc, key, default_value) result(v)
        character(len=*), intent(in) :: doc, key
        integer, intent(in) :: default_value
        integer :: v
        integer :: vs, ve, ios, tmp
        logical :: ok

        v = default_value
        call ai_json_find_key(doc, key, vs, ve, ok)
        if (.not. ok) return
        read(doc(vs:ve), *, iostat=ios) tmp
        if (ios == 0) v = tmp
    end function ai_json_get_integer

    ! True when a top-level array contains the given string. Used to check
    ! ollama's "capabilities" for "insert" -- the flag that says a model can
    ! actually fill in the middle rather than guessing from the prefix alone.
    function ai_json_array_has_string(doc, key, needle) result(found)
        character(len=*), intent(in) :: doc, key, needle
        logical :: found
        integer :: vs, ve
        logical :: ok

        found = .false.
        call ai_json_find_key(doc, key, vs, ve, ok)
        if (.not. ok) return
        if (ve <= vs) return
        if (doc(vs:vs) /= '[') return
        found = index(doc(vs:ve), '"' // needle // '"') > 0
    end function ai_json_array_has_string

end module ai_json_module
