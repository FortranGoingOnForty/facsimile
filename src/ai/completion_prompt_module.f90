! Assemble the prompt for a fill-in-the-middle request.
!
! Pure: buffer in, two strings out. No editor state, no I/O, no network -- so
! everything here is testable against golden strings without a model running,
! which is the only practical way to iterate on prompt content.
!
! The measurements steer what this spends. prompt_eval is 7-32 ms on GPU
! against 112 ms+ of generation, so context is cheap and tokens are not: it is
! worth prepending a symbol digest and a header line, and it is not worth
! raising num_predict to cover a model that overruns. That is what the stop
! sequences are for.
module completion_prompt_module
    use text_buffer_module, only: buffer_t, buffer_get_line, buffer_get_line_count
    use completion_context_module, only: build_fim_context
    use comment_syntax_module, only: comment_syntax_t, get_comment_syntax
    implicit none
    private

    public :: prompt_options_t, build_completion_prompt
    public :: completion_stop_json, buffer_symbol_digest, comment_block_above

    type :: prompt_options_t
        integer :: prefix_bytes = 6000
        integer :: suffix_bytes = 2000
        logical :: include_header = .true.
        logical :: include_symbols = .true.
        integer :: max_symbol_lines = 20
    end type prompt_options_t

contains

    ! prefix/suffix for the FIM request. extra_symbols, when non-empty, is a
    ! caller-supplied digest (LSP document symbols) that takes precedence over
    ! the syntactic scan.
    subroutine build_completion_prompt(buffer, filename, line, col, opts, &
                                       extra_symbols, prefix, suffix)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: filename, extra_symbols
        integer, intent(in) :: line, col
        type(prompt_options_t), intent(in) :: opts
        character(len=:), allocatable, intent(out) :: prefix, suffix
        character(len=:), allocatable :: window_prefix, header, digest
        type(comment_syntax_t) :: syn
        character(len=:), allocatable :: tok

        call build_fim_context(buffer, line, col, opts%prefix_bytes, &
                               opts%suffix_bytes, window_prefix, suffix)

        syn = get_comment_syntax(filename)
        tok = trim(syn%line)

        header = ''
        digest = ''

        ! Only prepend commented context when the language HAS a line comment.
        ! In html or css a '#' line would be literal text and would poison the
        ! completion rather than inform it.
        if (len(tok) > 0) then
            if (opts%include_header .and. len_trim(filename) > 0) then
                header = tok // ' ' // trim(basename_of(filename)) // achar(10)
            end if

            if (opts%include_symbols) then
                if (len_trim(extra_symbols) > 0) then
                    digest = comment_wrap(extra_symbols, tok, opts%max_symbol_lines)
                else
                    digest = comment_wrap(buffer_symbol_digest(buffer, filename, &
                                                               opts%max_symbol_lines), &
                                          tok, opts%max_symbol_lines)
                end if
            end if
        end if

        prefix = header // digest // window_prefix
    end subroutine build_completion_prompt

    ! Stop sequences as a JSON array fragment, or '' when the language is
    ! unknown. Overrun is the dominant quality problem with base FIM models:
    ! asked for 96 tokens they will carry on past the end of the construct and
    ! start writing the next function. Stopping them is far cheaper and more
    ! reliable than trying to make them stop on their own.
    function completion_stop_json(filename) result(json)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: json
        character(len=:), allocatable :: ext

        json = ''
        ext = lower(extension_of(filename))
        if (len(ext) == 0) return

        select case(ext)
        case('.c', '.h', '.cpp', '.cc', '.cxx', '.hpp', '.hxx', '.java', '.cs', &
             '.go', '.rs', '.js', '.jsx', '.ts', '.tsx', '.swift', '.kt', '.scala', &
             '.php', '.d', '.zig', '.dart')
            ! A closing brace at column 1 ends the enclosing definition
            json = '"\n}","\n\n\n"'
        case('.py', '.pyw', '.pyi')
            json = '"\ndef ","\nclass ","\n\n\n"'
        case('.f90', '.f95', '.f03', '.f08', '.f18', '.f', '.for')
            json = '"\nend subroutine","\nend function","\nend module","\n\n\n"'
        case('.rb')
            json = '"\ndef ","\nclass ","\n\n\n"'
        case('.lua')
            json = '"\nfunction ","\n\n\n"'
        case('.sh', '.bash', '.zsh')
            json = '"\n}","\n\n\n"'
        case default
            ! Unknown language: a blank-line run is the only safe universal
            ! stop. A wrong stop sequence is worse than none.
            json = '"\n\n\n"'
        end select
    end function completion_stop_json

    ! The contiguous run of comment lines immediately above `line`. This is
    ! what makes "write the comment, get the code" work, so it is worth
    ! guaranteeing rather than hoping the window reached it.
    function comment_block_above(buffer, filename, line) result(text)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line
        character(len=:), allocatable :: text
        character(len=:), allocatable :: src, tok
        type(comment_syntax_t) :: syn
        integer :: i, first

        text = ''
        syn = get_comment_syntax(filename)
        tok = trim(syn%line)
        if (len(tok) == 0) return
        if (line <= 1) return

        first = line
        do i = line - 1, 1, -1
            src = buffer_get_line(buffer, i)
            if (.not. line_starts_with_token(src, tok)) exit
            first = i
        end do

        if (first >= line) return
        do i = first, line - 1
            if (len(text) > 0) text = text // achar(10)
            text = text // buffer_get_line(buffer, i)
        end do
    end function comment_block_above

    ! A compact list of what this file defines. On a large file the prefix
    ! window cannot reach a function defined 800 lines up, and the model then
    ! invents a plausible-looking call to something that does not exist.
    !
    ! Syntactic rather than semantic: a line that starts at column 1 and looks
    ! like a definition. Language-agnostic and synchronous, which is the point
    ! -- LSP symbols are better when available but need async plumbing.
    function buffer_symbol_digest(buffer, filename, max_lines) result(text)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: filename
        integer, intent(in) :: max_lines
        character(len=:), allocatable :: text
        character(len=:), allocatable :: src, trimmed
        integer :: i, n, kept

        text = ''
        kept = 0
        n = buffer_get_line_count(buffer)

        do i = 1, n
            if (kept >= max_lines) exit
            src = buffer_get_line(buffer, i)
            if (len(src) == 0) cycle
            ! Top level only: definitions start at column 1
            if (src(1:1) == ' ' .or. src(1:1) == achar(9)) cycle
            if (.not. looks_like_definition(src, filename)) cycle

            trimmed = squeeze(src)
            if (len(trimmed) == 0) cycle
            if (len(text) > 0) text = text // achar(10)
            text = text // trimmed
            kept = kept + 1
        end do
    end function buffer_symbol_digest

    ! ------------------------------------------------------------------

    function looks_like_definition(src, filename) result(res)
        character(len=*), intent(in) :: src, filename
        logical :: res
        character(len=:), allocatable :: ext, s

        res = .false.
        s = src
        ext = lower(extension_of(filename))

        select case(ext)
        case('.py', '.pyw', '.pyi')
            res = starts(s, 'def ') .or. starts(s, 'class ') .or. starts(s, 'async def ')
        case('.f90', '.f95', '.f03', '.f08', '.f18')
            res = contains_word(lower(s), 'subroutine ') .or. &
                  contains_word(lower(s), 'function ') .or. &
                  starts(lower(s), 'module ') .or. starts(lower(s), 'type ')
        case('.go')
            res = starts(s, 'func ') .or. starts(s, 'type ')
        case('.rs')
            res = starts(s, 'fn ') .or. starts(s, 'pub fn ') .or. &
                  starts(s, 'struct ') .or. starts(s, 'enum ') .or. starts(s, 'impl ')
        case('.rb')
            res = starts(s, 'def ') .or. starts(s, 'class ') .or. starts(s, 'module ')
        case default
            ! C family and friends: a line at column 1 ending in '{' or ')',
            ! or a typedef/struct. Deliberately loose -- a false positive costs
            ! one line of context, a false negative costs a hallucinated call.
            if (starts(s, '#') .or. starts(s, '//')) return
            ! A definition opening a body ends in '{'; a one-line definition
            ! or a prototype ends in '}' or ';' and still has a parameter
            ! list. Missing the one-line form was leaving the digest empty on
            ! exactly the files where it matters -- a false positive costs one
            ! line of context, a false negative costs a hallucinated call.
            res = ends_with(s, '{') .or. ends_with(s, ')') .or. &
                  starts(s, 'typedef ') .or. starts(s, 'struct ') .or. &
                  starts(s, 'enum ') .or. starts(s, 'class ')
            if (.not. res .and. index(s, '(') > 0) then
                res = ends_with(s, '}') .or. ends_with(s, ';')
            end if
        end select
    end function looks_like_definition

    ! Prefix each line with the language's comment token so the digest reads as
    ! context rather than as code the model should continue.
    function comment_wrap(text, tok, max_lines) result(out)
        character(len=*), intent(in) :: text, tok
        integer, intent(in) :: max_lines
        character(len=:), allocatable :: out
        integer :: pos, start, kept

        out = ''
        if (len_trim(text) == 0) return

        kept = 0
        start = 1
        pos = 1
        do while (pos <= len(text) .and. kept < max_lines)
            if (text(pos:pos) == achar(10)) then
                if (pos > start) out = out // tok // ' ' // text(start:pos-1) // achar(10)
                kept = kept + 1
                start = pos + 1
            end if
            pos = pos + 1
        end do
        if (start <= len(text) .and. kept < max_lines) then
            out = out // tok // ' ' // text(start:) // achar(10)
        end if
    end function comment_wrap

    pure function line_starts_with_token(src, tok) result(res)
        character(len=*), intent(in) :: src, tok
        logical :: res
        integer :: i

        res = .false.
        i = 1
        do while (i <= len(src))
            if (src(i:i) /= ' ' .and. src(i:i) /= achar(9)) exit
            i = i + 1
        end do
        if (i > len(src)) return
        if (i + len(tok) - 1 > len(src)) return
        res = src(i:i+len(tok)-1) == tok
    end function line_starts_with_token

    pure function starts(s, p) result(res)
        character(len=*), intent(in) :: s, p
        logical :: res
        res = .false.
        if (len(s) < len(p)) return
        res = s(1:len(p)) == p
    end function starts

    pure function ends_with(s, p) result(res)
        character(len=*), intent(in) :: s, p
        logical :: res
        character(len=:), allocatable :: t
        integer :: n

        res = .false.
        n = len_trim(s)
        if (n < len(p)) return
        t = s(1:n)
        res = t(n-len(p)+1:n) == p
    end function ends_with

    pure function contains_word(s, w) result(res)
        character(len=*), intent(in) :: s, w
        logical :: res
        res = index(s, w) > 0
    end function contains_word

    ! Collapse runs of whitespace so the digest stays compact
    function squeeze(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i
        logical :: prev_space

        out = ''
        prev_space = .false.
        do i = 1, len_trim(s)
            if (s(i:i) == ' ' .or. s(i:i) == achar(9)) then
                if (.not. prev_space .and. len(out) > 0) out = out // ' '
                prev_space = .true.
            else
                out = out // s(i:i)
                prev_space = .false.
            end if
        end do
    end function squeeze

    pure function basename_of(path) result(base)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: base
        integer :: slash

        slash = index(path, '/', back=.true.)
        if (slash > 0) then
            base = path(slash+1:)
        else
            base = path
        end if
    end function basename_of

    pure function extension_of(path) result(ext)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: ext
        character(len=:), allocatable :: base
        integer :: dot, slash

        slash = index(path, '/', back=.true.)
        if (slash > 0) then
            base = path(slash+1:)
        else
            base = path
        end if
        dot = index(base, '.', back=.true.)
        if (dot > 0) then
            ext = base(dot:)
        else
            ext = ''
        end if
    end function extension_of

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

end module completion_prompt_module
