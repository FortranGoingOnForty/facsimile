program test_syntax_preproc
    ! Regression tests: C preprocessor lines must not keyword-highlight
    ! header names. '#include <float.h>' colored 'float' as a keyword
    ! because tokenize_line had no preprocessor handling at all - the
    ! line was tokenized as plain words and 'float' hit the keyword list.
    use syntax_highlighter_module
    implicit none

    type(syntax_highlighter_t) :: hl
    type(token_t), allocatable :: tokens(:)
    integer :: nfail

    nfail = 0

    ! --- C ---
    call init_highlighter(hl, 'foo.c')

    call tokenize_line(hl, '#include <float.h>', tokens)
    call check(.not. has_type(tokens, TOKEN_KEYWORD), &
               'c: no keyword token in #include <float.h>')
    call check(covers(tokens, TOKEN_PREPROCESSOR, 1, 8), &
               'c: #include is one preprocessor token')
    call check(covers(tokens, TOKEN_STRING, 10, 18), &
               'c: <float.h> is one string token')

    call tokenize_line(hl, '#include <stdio.h>', tokens)
    call check(covers(tokens, TOKEN_STRING, 10, 18), &
               'c: <stdio.h> is one string token')

    ! Unclosed operand while the line is still being typed
    call tokenize_line(hl, '#include <float', tokens)
    call check(.not. has_type(tokens, TOKEN_KEYWORD), &
               'c: no keyword token in unclosed #include <float')
    call check(covers(tokens, TOKEN_STRING, 10, 15), &
               'c: unclosed <float colored as string to end of line')

    ! Quoted form keeps normal string handling
    call tokenize_line(hl, '#include "local.h"', tokens)
    call check(covers(tokens, TOKEN_PREPROCESSOR, 1, 8), &
               'c: #include "..." directive token')
    call check(has_type(tokens, TOKEN_STRING), &
               'c: "local.h" is a string token')

    ! Leading whitespace and blanks after '#' are legal
    call tokenize_line(hl, '  #  include <string.h>', tokens)
    call check(covers(tokens, TOKEN_PREPROCESSOR, 3, 12), &
               'c: "  #  include" directive token spans # to word end')
    call check(covers(tokens, TOKEN_STRING, 14, 23), &
               'c: <string.h> string token after padded directive')

    ! Non-include directives: only the directive itself is preprocessor
    call tokenize_line(hl, '#define FLOAT_MAX 10', tokens)
    call check(covers(tokens, TOKEN_PREPROCESSOR, 1, 7), &
               'c: #define directive token')
    call check(has_type(tokens, TOKEN_NUMBER), &
               'c: number after #define still highlighted')
    call check(.not. has_type(tokens, TOKEN_KEYWORD), &
               'c: FLOAT_MAX not keyword-colored')

    ! '#' not opening the line is untouched
    call tokenize_line(hl, 'int x; // #include <float.h>', tokens)
    call check(has_type(tokens, TOKEN_COMMENT), &
               'c: trailing comment still a comment')
    call check(.not. has_type(tokens, TOKEN_PREPROCESSOR), &
               'c: # inside comment not a directive')

    ! Ordinary code is unaffected
    call tokenize_line(hl, 'float x = 1.0;', tokens)
    call check(covers(tokens, TOKEN_KEYWORD, 1, 5), &
               'c: float still a keyword in code')

    ! --- C++ ---
    call init_highlighter(hl, 'foo.cpp')
    call tokenize_line(hl, '#include <string>', tokens)
    call check(.not. has_type(tokens, TOKEN_TYPE), &
               'cpp: <string> not type-colored')
    call check(covers(tokens, TOKEN_STRING, 10, 17), &
               'cpp: <string> is one string token')

    ! --- Python: '#' stays a comment ---
    call init_highlighter(hl, 'foo.py')
    call tokenize_line(hl, '#include <float.h>', tokens)
    call check(has_type(tokens, TOKEN_COMMENT), &
               'py: # line still a comment')
    call check(.not. has_type(tokens, TOKEN_PREPROCESSOR), &
               'py: no directive token outside C family')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All syntax preprocessor tests passed'

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

end program test_syntax_preproc
