program test_syntax_wolf_string
    ! Regression tests: multiline string mode must EXIT at the closing
    ! delimiter. string_delimiter is fixed width (len=4), so storing """
    ! blank-padded it to '""" '; process_multiline_string searched for
    ! triple-quote-plus-space, a closing """ at end of line never matched,
    ! and every line after the first triple-quoted string in a .lu file
    ! rendered as string. Survived a shipped sprint because nothing
    ! exercised the close.
    use syntax_highlighter_module
    implicit none

    type(syntax_highlighter_t) :: hl
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: sample
    integer :: nfail

    nfail = 0

    ! --- wolf: triple-quoted block followed by ordinary code (align.lu shape)
    call init_highlighter(hl, 'align.lu')

    call tokenize_line(hl, 'let rows = """', tokens)
    call check(hl%in_multiline_string, &
               'wolf: unterminated """ enters multiline string mode')

    call tokenize_line(hl, '    espresso,340', tokens)
    call check(covers(tokens, TOKEN_STRING, 1, 16), &
               'wolf: interior line is one string token')
    call check(hl%in_multiline_string, &
               'wolf: interior line stays in multiline string mode')

    call tokenize_line(hl, '    """', tokens)
    call check(.not. hl%in_multiline_string, &
               'wolf: closing """ at end of line EXITS multiline string mode')
    call check(covers(tokens, TOKEN_STRING, 1, 7), &
               'wolf: closing line is a string token up to the delimiter')

    ! The whole point: code after the close is NOT string
    call tokenize_line(hl, 'var width = 0', tokens)
    call check(.not. has_type(tokens, TOKEN_STRING), &
               'wolf: line after close has no string token')
    call check(covers(tokens, TOKEN_KEYWORD, 1, 3), &
               'wolf: var is a keyword again after the close')

    call tokenize_line(hl, 'for row in rows.lines() {', tokens)
    call check(.not. has_type(tokens, TOKEN_STRING), &
               'wolf: later line has no string token either')

    ! Close mid-line: code after the delimiter on the same line highlights
    call tokenize_line(hl, 'let s = """', tokens)
    call check(hl%in_multiline_string, 'wolf: second block opens')
    call tokenize_line(hl, '""" // done', tokens)
    call check(.not. hl%in_multiline_string, &
               'wolf: close at start of line exits')
    call check(covers(tokens, TOKEN_STRING, 1, 3), &
               'wolf: mid-line close string token ends at the delimiter')
    call check(has_type(tokens, TOKEN_COMMENT), &
               'wolf: comment after mid-line close is a comment')

    ! Triple-quoted string opened and closed on one line never sets state
    call tokenize_line(hl, 'let t = """abc"""', tokens)
    call check(.not. hl%in_multiline_string, &
               'wolf: one-line """abc""" does not enter multiline mode')

    ! --- wolf: string interpolation, [gram.lex.str].
    ! Every wolf string is an f-string: STR_PART ::= STR_TEXT | '{{' | '}}'
    ! | INTERP, so a `{expr}` hole is code inside a literal. Its boundary
    ! braces get an accent while its expression uses ordinary code tokens.

    ! The '"..."' form: literal run, hole, literal run
    !                    123456789012345678901
    call tokenize_line(hl, 'let g = "hi {name}"', tokens)
    call check(covers(tokens, TOKEN_STRING, 9, 12), &
               'wolf: text before a hole is string')
    call check(covers(tokens, TOKEN_INTERP, 13, 13) .and. &
               covers(tokens, TOKEN_INTERP, 18, 18), &
               'wolf: interpolation braces retain their accent')
    call check(covers(tokens, TOKEN_PLAIN, 14, 17), &
               'wolf: an interpolated identifier is ordinary code')
    call check(covers(tokens, TOKEN_STRING, 19, 19), &
               'wolf: the closing quote is string again')

    ! '{{' and '}}' are literal braces, not a hole
    call tokenize_line(hl, 'let g = "{{literal}}"', tokens)
    call check(.not. has_type(tokens, TOKEN_INTERP), &
               'wolf: {{ and }} are literal text, not an interpolation')
    call check(covers(tokens, TOKEN_STRING, 9, 21), &
               'wolf: a string of doubled braces is one string token')

    ! Brace balance inside the expression: calls and indexing
    call tokenize_line(hl, 'let g = "{a.b(c[0])}"', tokens)
    call check(covers(tokens, TOKEN_INTERP, 10, 10) .and. &
               covers(tokens, TOKEN_INTERP, 20, 20), &
               'wolf: {a.b(c[0])} closes at its own brace')
    call check(at_type(tokens, TOKEN_OPERATOR, 12) .and. &
               at_type(tokens, TOKEN_NUMBER, 17), &
               'wolf: operators and numbers inside a hole are code')

    ! ... and a nested brace does not end the hole early
    call tokenize_line(hl, 'let g = "{ {k} }"', tokens)
    call check(covers(tokens, TOKEN_INTERP, 10, 10) .and. &
               covers(tokens, TOKEN_INTERP, 16, 16), &
               'wolf: a nested { } inside the expression is balanced')
    call check(count_type(tokens, TOKEN_INTERP) == 2, &
               'wolf: structural braces inside a hole are ordinary code')

    ! An escaped quote does not close the string, and the hole after it is
    ! still found
    call tokenize_line(hl, 'let g = "\"{x}"', tokens)
    call check(covers(tokens, TOKEN_INTERP, 12, 12) .and. &
               covers(tokens, TOKEN_INTERP, 14, 14), &
               'wolf: an escaped quote does not hide the next hole')

    ! An escape swallows the brace it precedes, so no hole opens
    call tokenize_line(hl, 'let g = "\{x}"', tokens)
    call check(.not. has_type(tokens, TOKEN_INTERP), &
               'wolf: an escaped brace does not open an interpolation')

    ! Unterminated '{': tokenizes to end of line and no further
    call tokenize_line(hl, 'let g = "hi {name', tokens)
    call check(covers(tokens, TOKEN_INTERP, 13, 13) .and. &
               covers(tokens, TOKEN_PLAIN, 14, 17), &
               'wolf: an unterminated { tokenizes its expression to end of line')
    call check(.not. hl%in_interp, &
               'wolf: a one-line string never carries a hole past its line')
    call check(.not. hl%in_multiline_string, &
               'wolf: an unterminated one-line string stays one line')

    call tokenize_line(hl, 'var width = 0', tokens)
    call check(.not. has_type(tokens, TOKEN_INTERP), &
               'wolf: the line after an unterminated { is ordinary code')
    call check(covers(tokens, TOKEN_KEYWORD, 1, 3), &
               'wolf: var is a keyword again after an unterminated {')

    ! A real expression in a hole uses the ordinary Wolf token classes.
    sample = 'let g = "value {if n > 2 { n * 3 } else { 0 }}"'
    call tokenize_line(hl, sample, tokens)
    call check(at_type(tokens, TOKEN_KEYWORD, index(sample, 'if')) .and. &
               at_type(tokens, TOKEN_KEYWORD, index(sample, 'else')), &
               'wolf: control words inside a hole are keywords')
    call check(at_type(tokens, TOKEN_OPERATOR, index(sample, '>')) .and. &
               at_type(tokens, TOKEN_OPERATOR, index(sample, '*')), &
               'wolf: operators inside a hole retain their colors')
    call check(at_type(tokens, TOKEN_NUMBER, index(sample, '2')) .and. &
               at_type(tokens, TOKEN_NUMBER, index(sample, '3')), &
               'wolf: numeric literals inside a hole are numbers')
    call check(count_type(tokens, TOKEN_INTERP) == 2, &
               'wolf: only the outer interpolation braces use the accent')

    ! A string literal inside the expression is still a string, and its
    ! braces do not close the interpolation around it.
    sample = 'let g = "value {choose("yes }", n + 2)} tail"'
    call tokenize_line(hl, sample, tokens)
    call check(at_type(tokens, TOKEN_STRING, index(sample, '"yes }"')), &
               'wolf: a quoted literal inside a hole remains a string')
    call check(at_type(tokens, TOKEN_OPERATOR, index(sample, '+')) .and. &
               at_type(tokens, TOKEN_NUMBER, index(sample, '2')), &
               'wolf: code after an inner string stays in the expression')
    call check(covers(tokens, TOKEN_STRING, len(sample)-5, len(sample)), &
               'wolf: text after the hole returns to string highlighting')

    ! Adjacent holes with format specifications are common in real Wolf code.
    sample = 'print("{k:<10}{totals[k]:>5}")'
    call tokenize_line(hl, sample, tokens)
    call check(count_type(tokens, TOKEN_INTERP) == 4, &
               'wolf: adjacent formatted holes keep distinct boundaries')
    call check(at_type(tokens, TOKEN_OPERATOR, index(sample, '<')) .and. &
               at_type(tokens, TOKEN_OPERATOR, index(sample, '>')), &
               'wolf: format alignment operators use their code color')
    call check(at_type(tokens, TOKEN_NUMBER, index(sample, '10')) .and. &
               at_type(tokens, TOKEN_NUMBER, index(sample, '5')), &
               'wolf: format widths are numeric literals')

    ! The '"""..."""' form: same holes, carried across lines
    call tokenize_line(hl, 'let rows = """', tokens)
    call check(hl%in_multiline_string, 'wolf: block string opens for interp')

    !                     123456789012345678901
    call tokenize_line(hl, '    total {sum} items', tokens)
    call check(covers(tokens, TOKEN_STRING, 1, 10), &
               'wolf: block-string text before a hole is string')
    call check(covers(tokens, TOKEN_INTERP, 11, 11) .and. &
               covers(tokens, TOKEN_INTERP, 15, 15), &
               'wolf: {sum} has accented boundaries inside a """ block')
    call check(covers(tokens, TOKEN_PLAIN, 12, 14), &
               'wolf: block-string interpolation contents are ordinary code')
    call check(covers(tokens, TOKEN_STRING, 16, 21), &
               'wolf: block-string text after a hole is string again')
    call check(hl%in_multiline_string, &
               'wolf: a hole does not end the block string')

    ! Doubled braces are literal in the block form too
    call tokenize_line(hl, '    {{not a hole}}', tokens)
    call check(.not. has_type(tokens, TOKEN_INTERP), &
               'wolf: {{ }} in a """ block is literal text')

    ! An expression really can continue on the next line inside a block
    call tokenize_line(hl, '    {a +', tokens)
    call check(covers(tokens, TOKEN_INTERP, 5, 5) .and. &
               at_type(tokens, TOKEN_OPERATOR, 8), &
               'wolf: an unclosed hole tokenizes to end of line in a block')
    call check(hl%in_interp, &
               'wolf: an unclosed hole in a """ block carries to the next line')

    call tokenize_line(hl, '     b} tail', tokens)
    call check(covers(tokens, TOKEN_PLAIN, 6, 6) .and. &
               covers(tokens, TOKEN_INTERP, 7, 7), &
               'wolf: the continued hole is code through its closing brace')
    call check(covers(tokens, TOKEN_STRING, 8, 12), &
               'wolf: text after the continued hole is string again')
    call check(.not. hl%in_interp, 'wolf: the continued hole is closed')

    call tokenize_line(hl, '    """', tokens)
    call check(.not. hl%in_multiline_string, &
               'wolf: the block with holes in it still closes')

    call tokenize_line(hl, 'var n = 1', tokens)
    call check(.not. has_type(tokens, TOKEN_STRING), &
               'wolf: code after a block with holes is not string')

    ! --- python: same machinery; triple forms were unreachable because
    ! '"' was listed ahead of '"""' and process_string takes the first match
    call init_highlighter(hl, 'doc.py')

    call tokenize_line(hl, 'def f():', tokens)
    call check(covers(tokens, TOKEN_KEYWORD, 1, 3), 'py: def is a keyword')

    call tokenize_line(hl, '    """Docstring opens here', tokens)
    call check(hl%in_multiline_string, &
               'py: unterminated """ enters multiline string mode')

    call tokenize_line(hl, '    and closes here."""', tokens)
    call check(.not. hl%in_multiline_string, &
               'py: closing """ exits multiline string mode')

    call tokenize_line(hl, '    return 42', tokens)
    call check(.not. has_type(tokens, TOKEN_STRING), &
               'py: code after docstring has no string token')
    call check(has_type(tokens, TOKEN_KEYWORD), &
               'py: return is a keyword after the docstring')

    ! Single-quote pairs still tokenize as ordinary one-line strings
    call tokenize_line(hl, "x = 'a' + 'b'", tokens)
    call check(.not. hl%in_multiline_string, &
               'py: one-line quotes never enter multiline mode')
    call check(covers(tokens, TOKEN_STRING, 5, 7), &
               'py: first quoted literal is one string token')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All wolf multiline string tests passed'

contains

    logical function has_type(toks, tok_type)
        type(token_t), intent(in) :: toks(:)
        integer, intent(in) :: tok_type
        integer :: i
        has_type = .false.
        do i = 1, size(toks)
            if (toks(i)%type == tok_type) then
                has_type = .true.
                return
            end if
        end do
    end function has_type

    logical function covers(toks, tok_type, start_col, end_col)
        type(token_t), intent(in) :: toks(:)
        integer, intent(in) :: tok_type, start_col, end_col
        integer :: i
        covers = .false.
        do i = 1, size(toks)
            if (toks(i)%type == tok_type .and. &
                toks(i)%start_col == start_col .and. &
                toks(i)%end_col == end_col) then
                covers = .true.
                return
            end if
        end do
    end function covers

    logical function at_type(toks, tok_type, col)
        type(token_t), intent(in) :: toks(:)
        integer, intent(in) :: tok_type, col
        integer :: i
        at_type = .false.
        do i = 1, size(toks)
            if (toks(i)%type == tok_type .and. &
                col >= toks(i)%start_col .and. col <= toks(i)%end_col) then
                at_type = .true.
                return
            end if
        end do
    end function at_type

    integer function count_type(toks, tok_type)
        type(token_t), intent(in) :: toks(:)
        integer, intent(in) :: tok_type
        integer :: i
        count_type = 0
        do i = 1, size(toks)
            if (toks(i)%type == tok_type) count_type = count_type + 1
        end do
    end function count_type

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name
        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name
            nfail = nfail + 1
        end if
    end subroutine check

end program test_syntax_wolf_string
