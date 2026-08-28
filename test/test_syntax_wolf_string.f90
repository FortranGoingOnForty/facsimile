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
