program test_completion_prompt
    ! Prompt assembly, tested as a pure function against golden strings.
    !
    ! Making this pure is the whole point: prompt content is the thing most
    ! worth iterating on, and iterating on it is only practical if you can see
    ! exactly what changed without a model in the loop.
    !
    ! The measurements steer what is worth spending here. prompt_eval is
    ! 7-32 ms on GPU against 112 ms+ of generation, so context is cheap and
    ! tokens are not -- hence a symbol digest and a header line, and stop
    ! sequences rather than a bigger token budget.
    use text_buffer_module
    use completion_prompt_module
    implicit none

    character, parameter :: NL = achar(10)
    integer :: nfail
    type(buffer_t) :: buf
    type(prompt_options_t) :: opts

    nfail = 0

    call test_stop_sequences()
    call test_header_line()
    call test_comment_block()
    call test_symbol_digest()
    call test_assembly()
    call test_languages_without_line_comments()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All completion-prompt tests passed'

contains

    subroutine load(text)
        character(len=*), intent(in) :: text
        call init_buffer(buf)
        if (len(text) > 0) call buffer_insert(buf, 1, text)
    end subroutine load

    ! Overrun is the dominant failure of a base FIM model asked for many
    ! tokens: it keeps going and starts writing the next function.
    subroutine test_stop_sequences()
        call has(completion_stop_json('a.c'), '\n}', 'C stops at a closing brace at column 1')
        call has(completion_stop_json('a.rs'), '\n}', 'Rust likewise')
        call has(completion_stop_json('a.go'), '\n}', 'Go likewise')
        call has(completion_stop_json('a.py'), '\ndef ', 'Python stops at the next def')
        call has(completion_stop_json('a.py'), '\nclass ', 'and the next class')
        call has(completion_stop_json('a.f90'), '\nend subroutine', &
                 'Fortran stops at end subroutine')
        call has(completion_stop_json('a.f90'), '\nend function', 'and end function')

        ! Universal: a run of blank lines means the model has moved on
        call has(completion_stop_json('a.c'), '\n\n\n', 'a blank-line run always stops')
        call has(completion_stop_json('a.unknownext'), '\n\n\n', &
                 'an unknown language still stops on blank lines')

        ! A wrong stop sequence is worse than none
        call check(index(completion_stop_json('a.unknownext'), 'def') == 0, &
                   'an unknown language gets no language-specific stop', &
                   completion_stop_json('a.unknownext'))
        call check(len(completion_stop_json('')) == 0, &
                   'no filename yields no stops at all', '')
    end subroutine test_stop_sequences

    subroutine test_header_line()
        character(len=:), allocatable :: p, s

        call load('int x;')
        opts = prompt_options_t()
        opts%include_symbols = .false.
        call build_completion_prompt(buf, '/home/u/proj/thing.c', 1, 7, opts, '', p, s)
        call check(index(p, '// thing.c') == 1, &
                   'the header names the file in its own comment syntax', p)
        call check(index(p, '/home/u') == 0, &
                   'and uses the basename, not the whole path', p)

        call load('x = 1')
        call build_completion_prompt(buf, 'thing.py', 1, 6, opts, '', p, s)
        call check(index(p, '# thing.py') == 1, 'Python uses #', p)

        opts%include_header = .false.
        call build_completion_prompt(buf, 'thing.py', 1, 6, opts, '', p, s)
        call check(index(p, '# thing.py') == 0, 'and it can be turned off', p)
    end subroutine test_header_line

    ! This is what makes "write the comment, get the code" work.
    subroutine test_comment_block()
        character(len=:), allocatable :: c

        call load('/* not this one */' // NL // 'int a;' // NL // &
                  '// describe it' // NL // '// on two lines' // NL // 'int f')
        c = comment_block_above(buf, 'x.c', 5)
        call check(c == '// describe it' // NL // '// on two lines', &
                   'the contiguous comment run above the caret', c)

        c = comment_block_above(buf, 'x.c', 2)
        call check(len(c) == 0, 'a block comment is not a line comment', c)

        call load('a' // NL // 'b')
        c = comment_block_above(buf, 'x.c', 2)
        call check(len(c) == 0, 'no comment above means nothing', c)

        c = comment_block_above(buf, 'x.c', 1)
        call check(len(c) == 0, 'line 1 has nothing above it', c)

        call load('# one' // NL // '# two' // NL // 'x')
        c = comment_block_above(buf, 'x.py', 3)
        call check(c == '# one' // NL // '# two', 'Python comments too', c)
    end subroutine test_comment_block

    ! On a large file the prefix window cannot reach a function defined 800
    ! lines up, and the model then invents a plausible call to something that
    ! does not exist.
    subroutine test_symbol_digest()
        character(len=:), allocatable :: d

        call load('int helper(int a) {' // NL // '    return a;' // NL // '}' // NL // &
                  'static void run(void) {' // NL // '    helper(1);' // NL // '}')
        d = buffer_symbol_digest(buf, 'x.c', 20)
        call check(index(d, 'int helper(int a) {') > 0, 'a C definition is picked up', d)
        call check(index(d, 'static void run(void) {') > 0, 'and another', d)
        call check(index(d, 'return a;') == 0, &
                   'indented bodies are not, only column-1 definitions', d)

        call load('def alpha(x):' // NL // '    return x' // NL // &
                  'class Beta:' // NL // '    pass')
        d = buffer_symbol_digest(buf, 'x.py', 20)
        call check(index(d, 'def alpha(x):') > 0, 'a Python def', d)
        call check(index(d, 'class Beta:') > 0, 'and a class', d)
        call check(index(d, 'pass') == 0, 'not the indented body', d)

        call load('subroutine one()' // NL // 'end subroutine' // NL // &
                  'function two() result(r)' // NL // 'end function')
        d = buffer_symbol_digest(buf, 'x.f90', 20)
        call check(index(d, 'subroutine one()') > 0, 'a Fortran subroutine', d)
        call check(index(d, 'function two() result(r)') > 0, 'and a function', d)

        ! Regression: a one-line definition ends in '}', not '{'. Missing
        ! this left the digest EMPTY on exactly the files where it matters,
        ! and the model then invented a call to a random nearby symbol --
        ! caught by the quality harness, not by the golden strings.
        call load('int helper_far_away(int x) { return x * 2; }' // NL // &
                  'static int filler(void) { return 0; }' // NL // &
                  'int use(int n) {')
        d = buffer_symbol_digest(buf, 'x.c', 20)
        call check(index(d, 'int helper_far_away(int x)') > 0, &
                   'a one-line definition ending in } is picked up', d)
        call check(index(d, 'static int filler(void)') > 0, 'and another', d)

        ! a prototype counts too: it names something that exists
        call load('int declared_only(int a);' // NL // 'int use(void) {')
        d = buffer_symbol_digest(buf, 'x.c', 20)
        call check(index(d, 'int declared_only(int a);') > 0, &
                   'a prototype ending in ; is picked up', d)

        ! ...but a plain statement at column 1 is not a definition
        call load('total = 0;' // NL // 'int f(void) {')
        d = buffer_symbol_digest(buf, 'x.c', 20)
        call check(index(d, 'total = 0;') == 0, &
                   'an assignment with no parameter list is not a definition', d)

        ! bounded
        call load('int a() {' // NL // 'int b() {' // NL // 'int c() {')
        d = buffer_symbol_digest(buf, 'x.c', 2)
        call check(count_lines(d) <= 2, 'the digest is capped', d)
    end subroutine test_symbol_digest

    subroutine test_assembly()
        character(len=:), allocatable :: p, s

        call load('int helper(void) {' // NL // '    return 1;' // NL // '}' // NL // &
                  '// add one to n' // NL // 'int bump(int n) {' // NL // &
                  '    ret' // NL // '}')
        opts = prompt_options_t()
        ! caret at end of "    ret" (line 6, col 8)
        call build_completion_prompt(buf, 'bump.c', 6, 8, opts, '', p, s)

        call check(index(p, '// bump.c') == 1, 'header comes first', p)
        call check(index(p, '// int helper(void) {') > 0, &
                   'the symbol digest is present and commented out', p)
        call check(index(p, '// add one to n') > 0, &
                   'the comment above the caret is in the window', p)
        call check(index(p, '    ret') > 0, 'and the partial line the caret sits in', p)
        call check(index(s, '}') > 0, 'the suffix carries what follows the caret', s)

        ! A caller-supplied digest (LSP symbols) takes precedence
        call build_completion_prompt(buf, 'bump.c', 6, 8, opts, &
                                     'int from_lsp(void)', p, s)
        call check(index(p, '// int from_lsp(void)') > 0, &
                   'a supplied symbol list is used when present', p)
        call check(index(p, '// int helper(void) {') == 0, &
                   'and replaces the syntactic scan rather than adding to it', p)
    end subroutine test_assembly

    ! In html or css a '#' line would be literal text and would poison the
    ! completion rather than inform it.
    subroutine test_languages_without_line_comments()
        character(len=:), allocatable :: p, s

        call load('<p>hi</p>')
        opts = prompt_options_t()
        call build_completion_prompt(buf, 'page.html', 1, 4, opts, '', p, s)
        call check(index(p, 'page.html') == 0, &
                   'no header is added for a language with no line comment', p)
        call check(p == '<p>', 'the prefix is just the window', p)

        call load('a { }')
        call build_completion_prompt(buf, 'style.css', 1, 3, opts, 'x', p, s)
        call check(index(p, 'style.css') == 0, 'nor for css', p)
    end subroutine test_languages_without_line_comments

    ! ------------------------------------------------------------------

    pure function count_lines(s) result(n)
        character(len=*), intent(in) :: s
        integer :: n, i
        n = 0
        if (len_trim(s) == 0) return
        n = 1
        do i = 1, len(s)
            if (s(i:i) == achar(10)) n = n + 1
        end do
    end function count_lines

    subroutine has(hay, needle, name)
        character(len=*), intent(in) :: hay, needle, name
        call check(index(hay, needle) > 0, name, hay)
    end subroutine has

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got
        character(len=:), allocatable :: shown
        integer :: i

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            shown = ''
            do i = 1, min(len(got), 140)
                if (iachar(got(i:i)) == 10) then
                    shown = shown // '|'
                else
                    shown = shown // got(i:i)
                end if
            end do
            print '(a)', 'FAIL: ' // name // ' (got: "' // shown // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_completion_prompt
