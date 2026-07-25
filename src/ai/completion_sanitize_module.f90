! The airtight layer between a language model and the editor.
!
! Design principle: REJECT OVER REPAIR. A rejected completion costs the user
! nothing -- they see no ghost, exactly as before the feature existed. A
! repaired-but-wrong completion costs them a silent defect in their code.
! Where reconciliation against surrounding text is unavoidable it is
! subtractive only: we delete from the model's text, never add to it.
!
! Ordering below is load-bearing. Byte validity comes before anything that
! inspects content, because the cheapest and most dangerous failures are
! bytes the terminal will execute.
module completion_sanitize_module
    implicit none
    private

    public :: sanitize_completion, sanitize_reason
    public :: SAN_OK, SAN_EMPTY, SAN_CONTROL_BYTES, SAN_BAD_UTF8, SAN_SPECIAL_TOKEN
    public :: SAN_CHAT_SHAPE, SAN_TOO_LONG, SAN_REPETITION, SAN_DUPLICATES_SUFFIX

    integer, parameter :: SAN_OK               = 0
    integer, parameter :: SAN_EMPTY            = 1
    integer, parameter :: SAN_CONTROL_BYTES    = 2
    integer, parameter :: SAN_BAD_UTF8         = 3
    integer, parameter :: SAN_SPECIAL_TOKEN    = 4
    integer, parameter :: SAN_CHAT_SHAPE       = 5
    integer, parameter :: SAN_TOO_LONG         = 6
    integer, parameter :: SAN_REPETITION       = 7
    integer, parameter :: SAN_DUPLICATES_SUFFIX = 8

    integer, parameter :: MAX_ACCEPT_BYTES = 2048

contains

    function sanitize_reason(code) result(text)
        integer, intent(in) :: code
        character(len=:), allocatable :: text

        select case(code)
        case(SAN_OK);                text = 'ok'
        case(SAN_EMPTY);             text = 'empty after trimming'
        case(SAN_CONTROL_BYTES);     text = 'contains terminal control bytes'
        case(SAN_BAD_UTF8);          text = 'invalid UTF-8'
        case(SAN_SPECIAL_TOKEN);     text = 'contains model special tokens'
        case(SAN_CHAT_SHAPE);        text = 'chat-shaped response, not a completion'
        case(SAN_TOO_LONG);          text = 'longer than the accept budget'
        case(SAN_REPETITION);        text = 'degenerate repetition'
        case(SAN_DUPLICATES_SUFFIX); text = 'only repeats text already after the cursor'
        case default;                text = 'rejected'
        end select
    end function sanitize_reason

    ! raw        : decoded model output
    ! line_after : buffer text from the cursor to end of line ("" at EOL)
    ! max_lines  : 1 for single-line mode, >1 for a block
    ! out        : text safe to show as ghost and to insert verbatim on accept
    subroutine sanitize_completion(raw, line_after, max_lines, out, code)
        character(len=*), intent(in) :: raw, line_after
        integer, intent(in) :: max_lines
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: code
        character(len=:), allocatable :: work

        out = ''
        code = SAN_EMPTY
        if (len(raw) == 0) return

        ! 1. Terminal control bytes. Ghost text is written straight to the
        !    terminal, so an ESC here is EXECUTED: cursor jumps, screen
        !    clears, OSC 52 writes the system clipboard. One predicate covers
        !    ESC, CSI, OSC, CR, BEL, SO/SI and NUL. Rejecting rather than
        !    stripping keeps this trivially auditable, and a completion
        !    carrying a control byte is garbage regardless.
        if (has_control_bytes(raw)) then
            code = SAN_CONTROL_BYTES
            return
        end if

        ! 2. UTF-8 validity. Invalid sequences desynchronise every utf8_*
        !    walk in the editor, and the damage reaches the saved file.
        if (.not. is_valid_utf8(raw)) then
            code = SAN_BAD_UTF8
            return
        end if

        ! 3. Special tokens mean the prompt template is wrong; everything
        !    after one is untrustworthy.
        if (has_special_token(raw)) then
            code = SAN_SPECIAL_TOKEN
            return
        end if

        work = strip_bom(raw)

        ! 4. Chat-shaped output. A fenced block or a conversational opener is
        !    a RESTATEMENT, not a continuation -- stripping the fence yields
        !    plausible-looking but wrong insertions, so reject outright. This
        !    is the backstop for a prompt-only request falling into the
        !    model's chat template.
        if (is_chat_shaped(work)) then
            code = SAN_CHAT_SHAPE
            return
        end if

        call trim_trailing_ws(work)
        if (len(work) == 0) then
            code = SAN_EMPTY
            return
        end if

        ! 5. Repetition. Base FIM models loop pathologically at low
        !    temperature; a completion that repeats itself is never wanted.
        if (is_repetitive(work)) then
            code = SAN_REPETITION
            return
        end if

        ! 6. Subtractive reconciliation with the text after the cursor. The
        !    classic FIM failure is completing "bar)" when "bar)" is already
        !    there, giving foo(bar)bar) on accept.
        call drop_suffix_echo(work, line_after)
        if (len(work) == 0) then
            code = SAN_DUPLICATES_SUFFIX
            return
        end if

        ! 7. Line budget
        if (max_lines <= 1) then
            work = first_line(work)
        else
            work = first_n_lines(work, max_lines)
        end if
        call trim_trailing_ws(work)
        if (len(work) == 0) then
            code = SAN_EMPTY
            return
        end if

        ! 8. Hard byte cap. Truncating a statement in half is worse than
        !    showing nothing, so this rejects rather than trims.
        if (len(work) > MAX_ACCEPT_BYTES) then
            code = SAN_TOO_LONG
            return
        end if

        if (is_blank(work)) then
            code = SAN_EMPTY
            return
        end if

        out = work
        code = SAN_OK
    end subroutine sanitize_completion

    ! ------------------------------------------------------------------

    pure function has_control_bytes(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        integer :: i, b

        res = .true.
        do i = 1, len(s)
            b = iachar(s(i:i))
            if (b < 32 .and. b /= 10 .and. b /= 9) return
            if (b == 127) return
        end do
        res = .false.
    end function has_control_bytes

    ! Full validity, not just lead-byte shape: rejects overlongs, encoded
    ! surrogates and anything past U+10FFFF, all of which break the editor's
    ! character-column arithmetic.
    pure function is_valid_utf8(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        integer :: i, n, b, need, cp, j, cb

        res = .false.
        n = len(s)
        i = 1
        do while (i <= n)
            b = iachar(s(i:i))
            if (b < 128) then
                i = i + 1
                cycle
            else if (b >= 194 .and. b <= 223) then
                need = 1; cp = b - 192
            else if (b >= 224 .and. b <= 239) then
                need = 2; cp = b - 224
            else if (b >= 240 .and. b <= 244) then
                need = 3; cp = b - 240
            else
                return          ! continuation byte as lead, or C0/C1/F5+
            end if

            if (i + need > n) return
            do j = 1, need
                cb = iachar(s(i+j:i+j))
                if (cb < 128 .or. cb > 191) return
                cp = cp * 64 + (cb - 128)
            end do

            ! Overlong encodings
            if (need == 1 .and. cp < 128) return
            if (need == 2 .and. cp < 2048) return
            if (need == 3 .and. cp < 65536) return
            ! Surrogates must never appear in UTF-8, and nothing above U+10FFFF
            if (cp >= 55296 .and. cp <= 57343) return
            if (cp > 1114111) return

            i = i + need + 1
        end do
        res = .true.
    end function is_valid_utf8

    pure function has_special_token(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res

        res = index(s, '<think>') > 0 .or. index(s, '</think>') > 0 .or. &
              index(s, '<|') > 0 .or. index(s, '|>') > 0 .or. &
              index(s, '<fim_') > 0 .or. index(s, 'fim_middle') > 0 .or. &
              index(s, 'fim_prefix') > 0 .or. index(s, 'fim_suffix') > 0 .or. &
              index(s, 'endoftext') > 0
    end function has_special_token

    function is_chat_shaped(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        character(len=:), allocatable :: head
        integer :: i

        res = .false.
        i = 1
        do while (i <= len(s))
            if (s(i:i) /= ' ' .and. s(i:i) /= achar(9) .and. s(i:i) /= achar(10)) exit
            i = i + 1
        end do
        if (i > len(s)) return

        if (i + 2 <= len(s)) then
            if (s(i:i+2) == '```') then
                res = .true.
                return
            end if
        end if

        head = lower(s(i:min(len(s), i + 31)))
        res = starts_with(head, 'here is') .or. starts_with(head, 'here''s') .or. &
              starts_with(head, 'sure,') .or. starts_with(head, 'sure!') .or. &
              starts_with(head, 'certainly') .or. starts_with(head, 'of course') .or. &
              starts_with(head, 'it looks like') .or. starts_with(head, 'i can help') .or. &
              starts_with(head, 'the following') .or. starts_with(head, 'to complete') .or. &
              starts_with(head, 'this function') .or. starts_with(head, 'this code')
    end function is_chat_shaped

    pure function starts_with(s, p) result(res)
        character(len=*), intent(in) :: s, p
        logical :: res
        res = .false.
        if (len(s) < len(p)) return
        res = s(1:len(p)) == p
    end function starts_with

    pure function lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 65 .and. c <= 90) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function lower

    ! Three or more identical consecutive non-blank lines, or a short line
    ! repeated to fill the budget.
    function is_repetitive(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        character(len=:), allocatable :: prev, cur
        integer :: pos, run, total, occurrences

        res = .false.
        prev = ''
        run = 1
        total = 0
        pos = 1

        do while (pos <= len(s))
            cur = next_line(s, pos)
            total = total + 1
            if (len_trim(cur) > 0) then
                if (cur == prev) then
                    run = run + 1
                    if (run >= 3) then
                        res = .true.
                        return
                    end if
                else
                    run = 1
                end if
                prev = cur
            end if
        end do

        ! A single short line repeated many times as a substring
        if (total >= 4 .and. len_trim(first_line(s)) > 0) then
            cur = trim(first_line(s))
            if (len(cur) >= 3) then
                occurrences = count_occurrences(s, cur)
                if (occurrences >= 4) res = .true.
            end if
        end if
    end function is_repetitive

    pure function count_occurrences(hay, needle) result(n)
        character(len=*), intent(in) :: hay, needle
        integer :: n, pos, hit

        n = 0
        pos = 1
        do
            if (pos > len(hay)) exit
            hit = index(hay(pos:), needle)
            if (hit <= 0) exit
            n = n + 1
            pos = pos + hit + len(needle) - 1
        end do
    end function count_occurrences

    ! Delete a trailing run of the completion that merely reproduces what is
    ! already immediately after the cursor.
    subroutine drop_suffix_echo(work, line_after)
        character(len=:), allocatable, intent(inout) :: work
        character(len=*), intent(in) :: line_after
        integer :: n, k

        if (len(line_after) == 0 .or. len(work) == 0) return

        ! Longest suffix of `work` that is a prefix of `line_after`
        n = min(len(work), len(line_after))
        do k = n, 1, -1
            if (work(len(work)-k+1:) == line_after(1:k)) then
                work = work(1:len(work)-k)
                return
            end if
        end do
    end subroutine drop_suffix_echo

    pure function first_line(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: nl

        nl = index(s, achar(10))
        if (nl <= 0) then
            out = s
        else
            out = s(1:nl-1)
        end if
    end function first_line

    function first_n_lines(s, n) result(out)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: out
        integer :: pos, count, last

        pos = 1
        count = 0
        last = len(s)
        do while (pos <= len(s))
            if (s(pos:pos) == achar(10)) then
                count = count + 1
                if (count >= n) then
                    last = pos - 1
                    exit
                end if
            end if
            pos = pos + 1
        end do
        out = s(1:max(0, last))
    end function first_n_lines

    function next_line(s, pos) result(out)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos
        character(len=:), allocatable :: out
        integer :: start

        start = pos
        do while (pos <= len(s))
            if (s(pos:pos) == achar(10)) exit
            pos = pos + 1
        end do
        out = s(start:pos-1)
        pos = pos + 1
    end function next_line

    subroutine trim_trailing_ws(s)
        character(len=:), allocatable, intent(inout) :: s
        integer :: n

        n = len(s)
        do while (n > 0)
            if (s(n:n) /= ' ' .and. s(n:n) /= achar(9) .and. s(n:n) /= achar(10)) exit
            n = n - 1
        end do
        s = s(1:n)
    end subroutine trim_trailing_ws

    pure function is_blank(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        integer :: i

        res = .true.
        do i = 1, len(s)
            if (s(i:i) /= ' ' .and. s(i:i) /= achar(9) .and. s(i:i) /= achar(10)) then
                res = .false.
                return
            end if
        end do
    end function is_blank

    function strip_bom(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out

        out = s
        if (len(s) >= 3) then
            if (iachar(s(1:1)) == 239 .and. iachar(s(2:2)) == 187 .and. &
                iachar(s(3:3)) == 191) out = s(4:)
        end if
    end function strip_bom

end module completion_sanitize_module
