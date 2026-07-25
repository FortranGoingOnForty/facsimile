program test_completion_context
    ! Prefix/suffix windows for fill-in-the-middle, and the request body built
    ! from them.
    !
    ! The property that matters is that the model sees code on BOTH sides of
    ! the caret. Prefix-only completion cannot close a brace it can see or
    ! stop where the existing code resumes, and the request must always carry
    ! a suffix field or ollama falls back to the model's chat template and
    ! returns prose.
    use text_buffer_module
    use completion_context_module
    use ollama_client_module
    implicit none

    character, parameter :: NL = achar(10)
    integer :: nfail
    type(buffer_t) :: buf

    nfail = 0

    call test_split_at_cursor()
    call test_line_after()
    call test_budget_snapping()
    call test_edges()
    call test_request_body()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All completion-context tests passed'

contains

    subroutine load(text)
        character(len=*), intent(in) :: text
        call init_buffer(buf)
        if (len(text) > 0) call buffer_insert(buf, 1, text)
    end subroutine load

    subroutine test_split_at_cursor()
        character(len=:), allocatable :: p, s

        call load('int a;' // NL // 'int b;' // NL // 'int c;')
        ! caret at line 2, column 5 -> "int " | "b;"
        call build_fim_context(buf, 2, 5, 4000, 4000, p, s)
        call eq(p, 'int a;' // NL // 'int ', 'prefix carries earlier lines and the partial line')
        call eq(s, 'b;' // NL // 'int c;', 'suffix carries the rest of the line and later lines')
    end subroutine test_split_at_cursor

    subroutine test_line_after()
        character(len=:), allocatable :: a

        call load('foo(bar)' // NL // 'x')
        a = context_line_after_cursor(buf, 1, 5)
        call eq(a, 'bar)', 'text after the caret on its own line')

        a = context_line_after_cursor(buf, 1, 9)
        call eq(a, '', 'nothing after a caret at end of line')

        a = context_line_after_cursor(buf, 2, 1)
        call eq(a, 'x', 'whole line when the caret is at column 1')
    end subroutine test_line_after

    ! Windows are cut on line boundaries: a prefix ending mid-statement gives
    ! a base model a fragment it tries to finish literally.
    subroutine test_budget_snapping()
        character(len=:), allocatable :: p, s
        integer :: i
        character(len=:), allocatable :: doc

        doc = ''
        do i = 1, 40
            doc = doc // 'line_of_about_thirty_chars_' // char(48 + mod(i, 10)) // NL
        end do
        doc = doc // 'CARET' // NL
        do i = 1, 40
            doc = doc // 'after_' // char(48 + mod(i, 10)) // NL
        end do
        call load(doc)

        ! Tight budget: only whole lines may be kept
        call build_fim_context(buf, 41, 1, 120, 120, p, s)
        call check(len(p) <= 120, 'prefix respects its budget', int_str(len(p)))
        call check(len(s) <= 121, 'suffix respects its budget', int_str(len(s)))
        call check(len(p) == 0 .or. p(1:1) /= NL, 'prefix does not start mid-line', p)
        ! every retained prefix line must be complete
        call check(index(p, 'line_of_about_thirty_chars_') == 0 .or. &
                   count_char(p, NL) >= 1, 'prefix keeps whole lines', p)
    end subroutine test_budget_snapping

    subroutine test_edges()
        character(len=:), allocatable :: p, s

        call load('only line')
        call build_fim_context(buf, 1, 1, 4000, 4000, p, s)
        call eq(p, '', 'caret at the very start has an empty prefix')
        call eq(s, 'only line', 'and the whole line as suffix')

        call build_fim_context(buf, 1, 10, 4000, 4000, p, s)
        call eq(p, 'only line', 'caret at the very end has the whole line as prefix')
        call eq(s, '', 'and an empty suffix')

        call load('')
        call build_fim_context(buf, 1, 1, 4000, 4000, p, s)
        call eq(p, '', 'empty buffer, empty prefix')
        call eq(s, '', 'empty buffer, empty suffix')

        ! Out-of-range lines must not crash or invent text
        call load('a' // NL // 'b')
        call build_fim_context(buf, 99, 1, 4000, 4000, p, s)
        call eq(p, '', 'a line past the end yields nothing')
    end subroutine test_edges

    ! The request shape IS the safety control.
    subroutine test_request_body()
        character(len=:), allocatable :: body

        body = ollama_generate_body('m', 'int a = ', ';' // NL, 24, 15, '5m')

        call check(index(body, '"suffix":') > 0, &
                   'suffix is always present (without it ollama uses the chat template)', &
                   body)
        call check(index(body, '"prompt":"int a = "') > 0, 'prompt carries the prefix', body)
        call check(index(body, '"stream":false') > 0, 'non-streaming', body)
        call check(index(body, '"keep_alive":"5m"') > 0, &
                   'keep_alive is set (a cold load measured 58s)', body)
        call check(index(body, '"num_predict":24') > 0, 'token cap set', body)

        ! An empty suffix still has to be sent
        body = ollama_generate_body('m', 'x', '', 8, 10, '1m')
        call check(index(body, '"suffix":""') > 0, &
                   'an empty suffix is still sent explicitly', body)

        ! Newlines in the code must be escaped, not embedded raw
        body = ollama_generate_body('m', 'a' // NL // 'b', 'c', 8, 10, '1m')
        call check(index(body, 'a\nb') > 0, 'newlines in code are escaped', body)
        call check(index(body, 'a' // NL // 'b') == 0, &
                   'and no raw newline reaches the JSON string', body)

        ! Capability gate
        call check(ollama_model_supports_fim('{"capabilities":["completion","insert"]}'), &
                   'insert capability is recognised', '')
        call check(.not. ollama_model_supports_fim('{"capabilities":["completion","tools"]}'), &
                   'a model without insert is refused', '')
        call check(.not. ollama_model_supports_fim('{}'), &
                   'a response with no capabilities is refused', '')
    end subroutine test_request_body

    pure function count_char(s, c) result(n)
        character(len=*), intent(in) :: s
        character, intent(in) :: c
        integer :: n, i
        n = 0
        do i = 1, len(s)
            if (s(i:i) == c) n = n + 1
        end do
    end function count_char

    function int_str(v) result(t)
        integer, intent(in) :: v
        character(len=16) :: t
        write(t, '(i0)') v
    end function int_str

    subroutine eq(got, want, name)
        character(len=*), intent(in) :: got, want, name
        call check(got == want, name, got)
    end subroutine eq

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got
        character(len=:), allocatable :: shown
        integer :: i

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            shown = ''
            do i = 1, min(len(got), 90)
                if (iachar(got(i:i)) == 10) then
                    shown = shown // '\n'
                else
                    shown = shown // got(i:i)
                end if
            end do
            print '(a)', 'FAIL: ' // name // ' (got: "' // shown // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_completion_context
