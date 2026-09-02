module syntax_highlighter_module
    use iso_fortran_env, only: int32
    use theme_module, only: THEME_SYNTAX_COMMENT, THEME_SYNTAX_OPERATOR, &
        THEME_SYNTAX_FUNCTION, THEME_SYNTAX_KEYWORD, THEME_SYNTAX_NUMBER, &
        THEME_SYNTAX_PREPROCESSOR, THEME_SYNTAX_STRING, THEME_SYNTAX_TYPE, &
        theme_sgr
    implicit none
    private

    public :: syntax_highlighter_t
    public :: token_t
    public :: init_highlighter, cleanup_highlighter
    public :: tokenize_line, get_token_color
    public :: detect_language
    public :: TOKEN_PLAIN, TOKEN_KEYWORD, TOKEN_STRING, TOKEN_NUMBER
    public :: TOKEN_COMMENT, TOKEN_OPERATOR, TOKEN_TYPE, TOKEN_FUNCTION
    public :: TOKEN_PREPROCESSOR, TOKEN_INTERP

    ! Token types as integer parameters
    integer, parameter :: TOKEN_PLAIN = 0
    integer, parameter :: TOKEN_KEYWORD = 1
    integer, parameter :: TOKEN_STRING = 2
    integer, parameter :: TOKEN_NUMBER = 3
    integer, parameter :: TOKEN_COMMENT = 4
    integer, parameter :: TOKEN_OPERATOR = 5
    integer, parameter :: TOKEN_TYPE = 6
    integer, parameter :: TOKEN_FUNCTION = 7
    integer, parameter :: TOKEN_PREPROCESSOR = 8
    ! A `{expr}` interpolation inside a string. Its own class because the
    ! span is code, not text: the reader needs to see where the string stops
    ! being literal. sagitta paints the same span `string.interp`; this
    ! editor has no interpolation colour of its own yet, so get_token_color
    ! lends it the preprocessor role -- the existing "this stretch belongs
    ! to another layer" colour -- until one is added.
    integer, parameter :: TOKEN_INTERP = 9

    ! Token structure
    type :: token_t
        integer :: type = TOKEN_PLAIN
        integer :: start_col
        integer :: end_col
    end type token_t

    ! Language definition
    type :: language_def_t
        character(len=32) :: name = ""
        character(len=16), allocatable :: extensions(:)
        character(len=64), allocatable :: keywords(:)
        character(len=64), allocatable :: types(:)
        character(len=8) :: comment_single = ""
        character(len=8) :: comment_start = ""
        character(len=8) :: comment_end = ""
        character(len=4), allocatable :: string_delimiters(:)
        character(len=4), allocatable :: operators(:)
        logical :: case_sensitive = .true.
        ! '#' at the start of a line begins a preprocessor directive (C/C++)
        logical :: has_preprocessor = .false.
        ! Every string literal is an f-string: `{expr}` inside one is code,
        ! `{{` and `}}` are literal braces (wolf, [gram.lex.str]).
        logical :: interpolated_strings = .false.
    end type language_def_t

    ! Main highlighter type
    type :: syntax_highlighter_t
        type(language_def_t) :: current_lang
        logical :: enabled = .false.
        logical :: in_multiline_comment = .false.
        logical :: in_multiline_string = .false.
        character(len=4) :: string_delimiter = ""
        ! Interpolation state, carried between lines the same way the
        ! multiline-string state is -- an unclosed `{` inside a """ block
        ! really does continue on the next line, and interp_depth is the
        ! brace balance that says which `}` ends it.
        logical :: in_interp = .false.
        integer :: interp_depth = 0
    end type syntax_highlighter_t

contains

    subroutine init_highlighter(highlighter, filename)
        type(syntax_highlighter_t), intent(out) :: highlighter
        character(len=*), intent(in), optional :: filename

        highlighter%enabled = .false.
        highlighter%in_multiline_comment = .false.
        highlighter%in_multiline_string = .false.
        highlighter%in_interp = .false.
        highlighter%interp_depth = 0

        if (present(filename)) then
            call detect_language(highlighter, filename)
        end if
    end subroutine init_highlighter

    subroutine cleanup_highlighter(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        if (allocated(highlighter%current_lang%extensions)) &
            deallocate(highlighter%current_lang%extensions)
        if (allocated(highlighter%current_lang%keywords)) &
            deallocate(highlighter%current_lang%keywords)
        if (allocated(highlighter%current_lang%types)) &
            deallocate(highlighter%current_lang%types)
        if (allocated(highlighter%current_lang%string_delimiters)) &
            deallocate(highlighter%current_lang%string_delimiters)
        if (allocated(highlighter%current_lang%operators)) &
            deallocate(highlighter%current_lang%operators)
    end subroutine cleanup_highlighter

    subroutine detect_language(highlighter, filename)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: extension, basename
        integer :: dot_pos, slash_pos

        ! Strip directory to get the basename
        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            basename = filename(slash_pos+1:)
        else
            basename = filename
        end if

        ! Extensionless filenames matched by name (Makefile, etc.)
        select case(basename)
        case('Makefile', 'makefile', 'GNUmakefile')
            call load_makefile_syntax(highlighter)
            return
        end select

        ! Find file extension
        dot_pos = index(basename, '.', back=.true.)
        if (dot_pos > 0) then
            extension = basename(dot_pos:)

            select case(extension)
            case('.f90', '.f95', '.f03', '.f08', '.f18')
                call load_fortran_syntax(highlighter)
            case('.py', '.pyw')
                call load_python_syntax(highlighter)
            case('.c', '.h')
                call load_c_syntax(highlighter)
            case('.cpp', '.cc', '.cxx', '.hpp', '.hxx')
                call load_cpp_syntax(highlighter)
            case('.rs')
                call load_rust_syntax(highlighter)
            case('.lu', '.wolfi')
                call load_wolf_syntax(highlighter)
            case('.go')
                call load_go_syntax(highlighter)
            case('.js', '.jsx', '.mjs')
                call load_javascript_syntax(highlighter)
            case('.ts', '.tsx')
                call load_typescript_syntax(highlighter)
            case('.sh', '.bash')
                call load_bash_syntax(highlighter)
            case('.md', '.markdown')
                call load_markdown_syntax(highlighter)
            case('.mk', '.mak')
                call load_makefile_syntax(highlighter)
            case default
                highlighter%enabled = .false.
            end select
        end if
    end subroutine detect_language

    subroutine tokenize_line(highlighter, line, tokens)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), allocatable, intent(out) :: tokens(:)
        integer :: i, line_len, token_count, first_nonblank
        character :: ch

        if (.not. highlighter%enabled) then
            allocate(tokens(1))
            tokens(1)%type = TOKEN_PLAIN
            tokens(1)%start_col = 1
            tokens(1)%end_col = len(line)
            return
        end if

        line_len = len(line)
        allocate(tokens(max(1, line_len)))
        token_count = 0
        i = 1

        first_nonblank = 0
        do i = 1, line_len
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                first_nonblank = i
                exit
            end if
        end do
        i = 1

        ! Handle multiline comment continuation
        if (highlighter%in_multiline_comment) then
            call process_multiline_comment(highlighter, line, tokens, token_count, i)
        end if

        ! Handle multiline string continuation
        if (highlighter%in_multiline_string) then
            call process_multiline_string(highlighter, line, tokens, token_count, i)
        end if

        ! Process rest of line
        do while (i <= line_len)
            ch = line(i:i)

            ! Check for multiline comment start
            if (check_multiline_comment_start(highlighter, line, i)) then
                call process_multiline_comment_start(highlighter, line, tokens, token_count, i)

            ! Check for single-line comment
            else if (check_comment_start(highlighter, line, i)) then
                token_count = token_count + 1
                tokens(token_count)%type = TOKEN_COMMENT
                tokens(token_count)%start_col = i
                tokens(token_count)%end_col = line_len
                exit
            end if

            ! Check for preprocessor directive ('#' opening the line).
            ! ch, not line(i:i): comment processing above may have advanced
            ! i past the end of the line, and .and. is not short-circuit.
            if (highlighter%current_lang%has_preprocessor .and. &
                i == first_nonblank .and. ch == '#') then
                call process_preprocessor(line, tokens, token_count, i)

            ! Check for string
            else if (check_string_start(highlighter, line, i)) then
                call process_string(highlighter, line, tokens, token_count, i)

            ! Check for number (guard the next-char peek: .and. is not
            ! short-circuit, so a trailing '.' would read out of bounds)
            else if (is_digit(ch) .or. (ch == '.' .and. next_is_digit(line, i, line_len))) then
                call process_number(line, tokens, token_count, i)

            ! Check for word (keyword, type, identifier)
            else if (is_alpha(ch) .or. ch == '_') then
                call process_word(highlighter, line, tokens, token_count, i)

            ! Check for operator
            else if (is_operator_char(highlighter, ch)) then
                token_count = token_count + 1
                tokens(token_count)%type = TOKEN_OPERATOR
                tokens(token_count)%start_col = i
                tokens(token_count)%end_col = i
                i = i + 1

            ! Plain character (space, etc.)
            else
                token_count = token_count + 1
                tokens(token_count)%type = TOKEN_PLAIN
                tokens(token_count)%start_col = i
                tokens(token_count)%end_col = i
                i = i + 1
            end if
        end do

        ! Resize tokens array
        if (token_count > 0) then
            tokens = tokens(1:token_count)
        else
            ! tokens is already allocated, just resize to 1 element
            deallocate(tokens)
            allocate(tokens(1))
            tokens(1)%type = TOKEN_PLAIN
            tokens(1)%start_col = 1
            tokens(1)%end_col = max(1, line_len)
        end if
    end subroutine tokenize_line

    function get_token_color(tok_type) result(color)
        integer, intent(in) :: tok_type
        character(len=:), allocatable :: color

        select case(tok_type)
        case(TOKEN_KEYWORD)
            color = theme_sgr(THEME_SYNTAX_KEYWORD)
        case(TOKEN_STRING)
            color = theme_sgr(THEME_SYNTAX_STRING)
        case(TOKEN_NUMBER)
            color = theme_sgr(THEME_SYNTAX_NUMBER)
        case(TOKEN_COMMENT)
            color = theme_sgr(THEME_SYNTAX_COMMENT)
        case(TOKEN_OPERATOR)
            color = theme_sgr(THEME_SYNTAX_OPERATOR)
        case(TOKEN_TYPE)
            color = theme_sgr(THEME_SYNTAX_TYPE)
        case(TOKEN_FUNCTION)
            color = theme_sgr(THEME_SYNTAX_FUNCTION)
        case(TOKEN_PREPROCESSOR)
            color = theme_sgr(THEME_SYNTAX_PREPROCESSOR)
        case(TOKEN_INTERP)
            ! Borrowed, not chosen: the preprocessor role is the existing
            ! "this stretch is handled by another layer" colour, and it is
            ! distinct from the string colour in every shipped theme. A
            ! dedicated syntax.interp role would be one line here.
            color = theme_sgr(THEME_SYNTAX_PREPROCESSOR)
        case(TOKEN_PLAIN)
            color = ""
        case default
            color = ""
        end select
    end function get_token_color

    ! Language-specific loading routines
    subroutine load_fortran_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "fortran"
        highlighter%current_lang%case_sensitive = .false.

        ! Keywords
        allocate(highlighter%current_lang%keywords(58))
        highlighter%current_lang%keywords = [ &
            "program     ", "end         ", "subroutine  ", "function    ", &
            "module      ", "use         ", "implicit    ", "none        ", &
            "if          ", "then        ", "else        ", "elseif      ", &
            "endif       ", "do          ", "while       ", "enddo       ", &
            "select      ", "case        ", "default     ", "endselect   ", &
            "where       ", "elsewhere   ", "endwhere    ", "forall      ", &
            "call        ", "return      ", "contains    ", "interface   ", &
            "abstract    ", "allocate    ", "deallocate  ", "allocatable ", &
            "intent      ", "in          ", "out         ", "inout       ", &
            "optional    ", "parameter   ", "save        ", "pointer     ", &
            "target      ", "public      ", "private     ", "protected   ", &
            "bind        ", "import      ", "only        ", "operator    ", &
            "assignment  ", "generic     ", "final       ", "extends     ", &
            "class       ", "type        ", "endtype     ", "enum        ", &
            "enumerator  ", "namelist    " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(10))
        highlighter%current_lang%types = [ &
            "integer     ", "real        ", "complex     ", "logical     ", &
            "character   ", "double      ", "precision   ", "int32       ", &
            "int64       ", "real64      " &
        ]

        ! Comments
        highlighter%current_lang%comment_single = "!"

        ! String delimiters
        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['"', "'"]

        ! Operators
        allocate(highlighter%current_lang%operators(20))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "**  ", "=   ", &
            "==  ", "/=  ", "<   ", ">   ", "<=  ", ">=  ", &
            ".and", ".or.", ".not", ".eq.", ".ne.", ".lt.", &
            ".gt.", ".le." &
        ]

        highlighter%enabled = .true.
    end subroutine load_fortran_syntax

    subroutine load_python_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "python"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(35))
        highlighter%current_lang%keywords = [ &
            "def         ", "class       ", "if          ", "elif        ", &
            "else        ", "for         ", "while       ", "break       ", &
            "continue    ", "return      ", "yield       ", "import      ", &
            "from        ", "as          ", "try         ", "except      ", &
            "finally     ", "raise       ", "with        ", "assert      ", &
            "lambda      ", "pass        ", "del         ", "global      ", &
            "nonlocal    ", "in          ", "is          ", "and         ", &
            "or          ", "not         ", "True        ", "False       ", &
            "None        ", "async       ", "await       " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(8))
        highlighter%current_lang%types = [ &
            "int         ", "float       ", "str         ", "bool        ", &
            "list        ", "dict        ", "tuple       ", "set         " &
        ]

        highlighter%current_lang%comment_single = "#"

        ! String delimiters. Triple forms FIRST: process_string takes the
        ! first delimiter that matches, so listing '"' ahead of '"""' made
        ! the triple-quoted (multiline) form unreachable.
        allocate(highlighter%current_lang%string_delimiters(4))
        highlighter%current_lang%string_delimiters = ['""" ', "''' ", '"   ', "'   "]

        ! Operators
        allocate(highlighter%current_lang%operators(15))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "//  ", "%   ", "**  ", &
            "=   ", "==  ", "!=  ", "<   ", ">   ", "<=  ", ">=  ", &
            "&   " &
        ]

        highlighter%enabled = .true.
    end subroutine load_python_syntax

    ! Helper functions
    function is_alpha(ch) result(res)
        character(len=1), intent(in) :: ch
        logical :: res
        res = (ch >= 'a' .and. ch <= 'z') .or. (ch >= 'A' .and. ch <= 'Z')
    end function is_alpha

    function is_digit(ch) result(res)
        character(len=1), intent(in) :: ch
        logical :: res
        res = (ch >= '0' .and. ch <= '9')
    end function is_digit

    function is_alnum(ch) result(res)
        character(len=1), intent(in) :: ch
        logical :: res
        res = is_alpha(ch) .or. is_digit(ch) .or. ch == '_'
    end function is_alnum

    ! True if the character after position i is a digit. Bounds-guarded so
    ! it never reads past the end of the line (.and. is not short-circuit).
    function next_is_digit(line, i, line_len) result(res)
        character(len=*), intent(in) :: line
        integer, intent(in) :: i, line_len
        logical :: res
        res = .false.
        if (i + 1 <= line_len) res = is_digit(line(i+1:i+1))
    end function next_is_digit

    function is_operator_char(highlighter, ch) result(res)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=1), intent(in) :: ch
        logical :: res
        integer :: i

        res = .false.
        if (.not. allocated(highlighter%current_lang%operators)) return

        do i = 1, size(highlighter%current_lang%operators)
            if (index(trim(highlighter%current_lang%operators(i)), ch) > 0) then
                res = .true.
                exit
            end if
        end do
    end function is_operator_char

    function check_comment_start(highlighter, line, pos) result(res)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        logical :: res
        integer :: comment_len

        res = .false.
        if (highlighter%current_lang%comment_single /= "") then
            comment_len = len_trim(highlighter%current_lang%comment_single)
            if (pos + comment_len - 1 <= len(line)) then
                res = line(pos:pos+comment_len-1) == trim(highlighter%current_lang%comment_single)
            end if
        end if
    end function check_comment_start

    function check_multiline_comment_start(highlighter, line, pos) result(res)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        logical :: res
        integer :: comment_len

        res = .false.
        if (highlighter%current_lang%comment_start /= "") then
            comment_len = len_trim(highlighter%current_lang%comment_start)
            if (pos + comment_len - 1 <= len(line)) then
                res = line(pos:pos+comment_len-1) == trim(highlighter%current_lang%comment_start)
            end if
        end if
    end function check_multiline_comment_start

    function check_string_start(highlighter, line, pos) result(res)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        logical :: res
        integer :: i, delim_len

        res = .false.
        if (.not. allocated(highlighter%current_lang%string_delimiters)) return

        do i = 1, size(highlighter%current_lang%string_delimiters)
            delim_len = len_trim(highlighter%current_lang%string_delimiters(i))
            if (pos + delim_len - 1 <= len(line)) then
                if (line(pos:pos+delim_len-1) == trim(highlighter%current_lang%string_delimiters(i))) then
                    res = .true.
                    exit
                end if
            end if
        end do
    end function check_string_start

    ! The delimiter that opens a string at `pos`, longest form first because
    ! the list is ordered that way and the first match wins.
    function matched_delimiter(highlighter, line, pos) result(delimiter)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        character(len=:), allocatable :: delimiter
        integer :: i, delim_len

        ! Defensive default - check_string_start has already said one matches
        delimiter = '"'

        do i = 1, size(highlighter%current_lang%string_delimiters)
            delim_len = len_trim(highlighter%current_lang%string_delimiters(i))
            if (pos + delim_len - 1 <= len(line)) then
                if (line(pos:pos+delim_len-1) == trim(highlighter%current_lang%string_delimiters(i))) then
                    delimiter = trim(highlighter%current_lang%string_delimiters(i))
                    exit
                end if
            end if
        end do
    end function matched_delimiter

    subroutine process_string(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: start_pos, line_len
        character(len=:), allocatable :: delimiter
        logical :: found_end, is_multiline

        ! Languages whose strings are f-strings need the body split into
        ! literal runs and interpolations, which is a different walk.
        if (highlighter%current_lang%interpolated_strings) then
            call process_interp_string(highlighter, line, tokens, token_count, pos)
            return
        end if

        line_len = len(line)
        start_pos = pos

        delimiter = matched_delimiter(highlighter, line, pos)

        ! Check if this is a multiline-capable delimiter
        ! Python: """ or ''' (length 3)
        ! JavaScript/TypeScript template literals (```) would need length 3
        ! Single backticks should NOT be multiline (breaks markdown with Ctrl+\`)
        is_multiline = (len(delimiter) >= 3)

        ! Move past opening delimiter
        pos = pos + len(delimiter)

        ! Find closing delimiter
        found_end = .false.
        do while (pos <= line_len)
            ! Skip escaped character (but NOT for backticks - markdown inline code is literal)
            if (line(pos:pos) == '\' .and. pos < line_len .and. delimiter /= '`') then
                pos = pos + 2
            else if (pos + len(delimiter) - 1 <= line_len) then
                if (line(pos:pos+len(delimiter)-1) == delimiter) then
                    pos = pos + len(delimiter)
                    found_end = .true.
                    exit
                else
                    pos = pos + 1
                end if
            else
                pos = pos + 1
            end if
        end do

        ! Add string token
        token_count = token_count + 1
        tokens(token_count)%type = TOKEN_STRING
        tokens(token_count)%start_col = start_pos
        tokens(token_count)%end_col = min(pos - 1, line_len)

        ! Handle unclosed string
        if (.not. found_end) then
            if (is_multiline) then
                ! Enter multiline string mode
                highlighter%in_multiline_string = .true.
                highlighter%string_delimiter = delimiter
            end if
            pos = line_len + 1
        end if
    end subroutine process_string

    ! ------------------------------------------------------------------
    ! Interpolated strings (wolf).
    !
    ! [gram.lex.str]: STR_PART ::= STR_TEXT | '{{' | '}}' | INTERP, and
    ! INTERP ::= '{' expr FORMAT_SPEC? '}'. So the body of a string is
    ! literal text with holes in it, and the holes are code. Painting the
    ! whole literal one colour hides where the code is; painting the holes
    ! shows it. This is a line tokenizer, not a parser -- the expression
    ! inside a hole is one span, not re-lexed -- so the only thing that has
    ! to be got right is where each hole starts and stops.
    ! ------------------------------------------------------------------

    ! Bounds-safe two-character compare. Fortran's .and. does not
    ! short-circuit, so the length test cannot ride in the same expression
    ! as the substring compare.
    function two_char_at(line, pos, pair) result(res)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        character(len=*), intent(in) :: pair
        logical :: res

        res = .false.
        if (pos < 1) return
        if (pos + 1 > len(line)) return
        res = (line(pos:pos+1) == pair)
    end function two_char_at

    ! Bounds-safe compare against a string delimiter of any length.
    function delimiter_at(line, pos, delimiter) result(res)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        character(len=*), intent(in) :: delimiter
        logical :: res

        res = .false.
        if (len(delimiter) == 0) return
        if (pos < 1) return
        if (pos + len(delimiter) - 1 > len(line)) return
        res = (line(pos:pos+len(delimiter)-1) == delimiter)
    end function delimiter_at

    ! One token, if it covers at least one column and there is room.
    subroutine emit_token(tokens, token_count, tok_type, start_col, end_col)
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count
        integer, intent(in) :: tok_type, start_col, end_col

        if (end_col < start_col) return
        if (token_count >= size(tokens)) return
        token_count = token_count + 1
        tokens(token_count)%type = tok_type
        tokens(token_count)%start_col = start_col
        tokens(token_count)%end_col = end_col
    end subroutine emit_token

    ! Consume one `{expr}` from `pos` and emit it as a single TOKEN_INTERP.
    ! Brace balance decides which `}` ends it, so `{a.b(c[0])}` closes at
    ! its own brace and `{ {k: v} }` at the outer one. An expression that
    ! does not close on this line paints to the end of the line and leaves
    ! in_interp set -- inside a """ block that is the truth, and a one-line
    ! string clears the flag when it ends at the newline.
    subroutine process_interp_span(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: line_len, start_col

        line_len = len(line)
        start_col = pos

        do while (pos <= line_len)
            select case (line(pos:pos))
            case ('{')
                highlighter%interp_depth = highlighter%interp_depth + 1
                pos = pos + 1
            case ('}')
                if (highlighter%interp_depth <= 1) then
                    pos = pos + 1
                    call emit_token(tokens, token_count, TOKEN_INTERP, start_col, pos - 1)
                    highlighter%in_interp = .false.
                    highlighter%interp_depth = 0
                    return
                end if
                highlighter%interp_depth = highlighter%interp_depth - 1
                pos = pos + 1
            case default
                pos = pos + 1
            end select
        end do

        call emit_token(tokens, token_count, TOKEN_INTERP, start_col, line_len)
    end subroutine process_interp_span

    ! Walk a string body from `pos`, emitting a TOKEN_STRING run for every
    ! stretch of literal text and a TOKEN_INTERP span for every hole.
    ! `run_start` is where the current literal run began: the opening
    ! delimiter for a string that starts on this line, `pos` for a
    ! continuation line inside a """ block.
    subroutine scan_interp_body(highlighter, line, tokens, token_count, pos, &
                                run_start_in, delimiter, closed)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer, intent(in) :: run_start_in
        character(len=*), intent(in) :: delimiter
        logical, intent(out) :: closed
        integer :: line_len, run_start

        line_len = len(line)
        run_start = run_start_in
        closed = .false.

        do while (pos <= line_len)
            if (highlighter%in_interp) then
                call process_interp_span(highlighter, line, tokens, token_count, pos)
                run_start = pos
                cycle
            end if

            if (delimiter_at(line, pos, delimiter)) then
                pos = pos + len(delimiter)
                closed = .true.
                exit
            else if (line(pos:pos) == '\') then
                ! An escape hides the next character, `\"` and `\{` alike
                if (pos < line_len) then
                    pos = pos + 2
                else
                    pos = pos + 1
                end if
            else if (two_char_at(line, pos, '{{') .or. two_char_at(line, pos, '}}')) then
                ! A literal brace, and part of the text around the holes
                pos = pos + 2
            else if (line(pos:pos) == '{') then
                call emit_token(tokens, token_count, TOKEN_STRING, run_start, pos - 1)
                highlighter%in_interp = .true.
                highlighter%interp_depth = 0
                ! The branch at the top of the loop consumes the span
            else
                pos = pos + 1
            end if
        end do

        if (closed) then
            call emit_token(tokens, token_count, TOKEN_STRING, run_start, pos - 1)
        else if (.not. highlighter%in_interp) then
            call emit_token(tokens, token_count, TOKEN_STRING, run_start, line_len)
        end if

        ! A one-line string ends at the newline whatever it was in the middle
        ! of, so nothing it opened can bleed onto the next line.
        if (closed .or. len(delimiter) < 3) then
            highlighter%in_interp = .false.
            highlighter%interp_depth = 0
        end if
    end subroutine scan_interp_body

    ! A string that opens on this line, in a language whose strings
    ! interpolate. Same contract as process_string: `pos` lands after the
    ! literal, and an unclosed multiline form leaves the highlighter in
    ! multiline string mode.
    subroutine process_interp_string(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: start_pos, line_len
        character(len=:), allocatable :: delimiter
        logical :: closed

        line_len = len(line)
        start_pos = pos
        delimiter = matched_delimiter(highlighter, line, pos)

        pos = pos + len(delimiter)
        call scan_interp_body(highlighter, line, tokens, token_count, pos, &
                              start_pos, delimiter, closed)

        if (.not. closed) then
            if (len(delimiter) >= 3) then
                highlighter%in_multiline_string = .true.
                highlighter%string_delimiter = delimiter
            end if
            pos = line_len + 1
        end if
    end subroutine process_interp_string

    ! A continuation line of a """ block in a language whose strings
    ! interpolate: the same walk, resumed, carrying any interpolation that
    ! was still open at the end of the previous line.
    subroutine process_multiline_interp(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        character(len=:), allocatable :: delimiter
        logical :: closed

        delimiter = trim(highlighter%string_delimiter)

        call scan_interp_body(highlighter, line, tokens, token_count, pos, &
                              pos, delimiter, closed)

        if (closed) then
            highlighter%in_multiline_string = .false.
            highlighter%string_delimiter = ""
        else
            pos = len(line) + 1
        end if
    end subroutine process_multiline_interp

    subroutine process_number(line, tokens, token_count, pos)
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: start_pos
        logical :: has_dot, has_e

        start_pos = pos
        has_dot = .false.
        has_e = .false.

        do while (pos <= len(line))
            if (is_digit(line(pos:pos))) then
                pos = pos + 1
            else if (line(pos:pos) == '.' .and. .not. has_dot .and. .not. has_e) then
                has_dot = .true.
                pos = pos + 1
            else if ((line(pos:pos) == 'e' .or. line(pos:pos) == 'E') .and. .not. has_e) then
                has_e = .true.
                pos = pos + 1
                if (pos <= len(line)) then
                    if (line(pos:pos) == '+' .or. line(pos:pos) == '-') then
                        pos = pos + 1
                    end if
                end if
            else
                exit
            end if
        end do

        token_count = token_count + 1
        tokens(token_count)%type = TOKEN_NUMBER
        tokens(token_count)%start_col = start_pos
        tokens(token_count)%end_col = pos - 1
    end subroutine process_number

    subroutine process_word(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(in) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: start_pos, end_pos, i
        character(len=:), allocatable :: word
        logical :: is_keyword, is_type

        start_pos = pos

        ! Find end of word. NB: Fortran does not short-circuit .and., so the
        ! bounds check and the substring access must be in separate tests or
        ! line(pos:pos) is evaluated out of bounds when the word ends the line.
        do while (pos <= len(line))
            if (.not. is_alnum(line(pos:pos))) exit
            pos = pos + 1
        end do
        end_pos = pos - 1

        word = line(start_pos:end_pos)

        ! Check if it's a keyword
        is_keyword = .false.
        if (allocated(highlighter%current_lang%keywords)) then
            do i = 1, size(highlighter%current_lang%keywords)
                if (compare_word(word, highlighter%current_lang%keywords(i), &
                                highlighter%current_lang%case_sensitive)) then
                    is_keyword = .true.
                    exit
                end if
            end do
        end if

        ! Check if it's a type
        is_type = .false.
        if (.not. is_keyword .and. allocated(highlighter%current_lang%types)) then
            do i = 1, size(highlighter%current_lang%types)
                if (compare_word(word, highlighter%current_lang%types(i), &
                                highlighter%current_lang%case_sensitive)) then
                    is_type = .true.
                    exit
                end if
            end do
        end if

        token_count = token_count + 1
        tokens(token_count)%start_col = start_pos
        tokens(token_count)%end_col = end_pos

        if (is_keyword) then
            tokens(token_count)%type = TOKEN_KEYWORD
        else if (is_type) then
            tokens(token_count)%type = TOKEN_TYPE
        else
            tokens(token_count)%type = TOKEN_PLAIN
        end if
    end subroutine process_word

    ! '#' plus the directive word is one TOKEN_PREPROCESSOR token. The
    ! <header> operand of an include-like directive is emitted as a string
    ! token so header names that collide with keywords (<float.h>) are not
    ! keyword-colored; the "header.h" form already gets string handling.
    subroutine process_preprocessor(line, tokens, token_count, pos)
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: line_len, start_pos, word_start, word_end, close_off
        character(len=:), allocatable :: directive

        line_len = len(line)
        start_pos = pos
        pos = pos + 1

        ! Blanks are legal between '#' and the directive word
        do while (pos <= line_len)
            if (line(pos:pos) /= ' ' .and. line(pos:pos) /= char(9)) exit
            pos = pos + 1
        end do

        word_start = pos
        do while (pos <= line_len)
            if (.not. is_alnum(line(pos:pos))) exit
            pos = pos + 1
        end do
        word_end = pos - 1

        token_count = token_count + 1
        tokens(token_count)%type = TOKEN_PREPROCESSOR
        tokens(token_count)%start_col = start_pos
        tokens(token_count)%end_col = max(start_pos, word_end)

        if (word_end < word_start) return
        directive = line(word_start:word_end)
        if (directive /= 'include' .and. directive /= 'include_next' .and. &
            directive /= 'import') return

        do while (pos <= line_len)
            if (line(pos:pos) /= ' ' .and. line(pos:pos) /= char(9)) exit
            pos = pos + 1
        end do
        if (pos > line_len) return
        if (line(pos:pos) /= '<') return

        ! To the closing '>', or end of line while still being typed
        close_off = index(line(pos+1:line_len), '>')
        token_count = token_count + 1
        tokens(token_count)%type = TOKEN_STRING
        tokens(token_count)%start_col = pos
        if (close_off > 0) then
            tokens(token_count)%end_col = pos + close_off
            pos = pos + close_off + 1
        else
            tokens(token_count)%end_col = line_len
            pos = line_len + 1
        end if
    end subroutine process_preprocessor

    function compare_word(word1, word2, case_sensitive) result(match)
        character(len=*), intent(in) :: word1, word2
        logical, intent(in) :: case_sensitive
        logical :: match

        if (case_sensitive) then
            match = trim(word1) == trim(word2)
        else
            match = to_lower(trim(word1)) == to_lower(trim(word2))
        end if
    end function compare_word

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str
        do i = 1, len(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                lower_str(i:i) = char(ichar(str(i:i)) + 32)
            end if
        end do
    end function to_lower

    ! C language support
    subroutine load_c_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "c"
        highlighter%current_lang%case_sensitive = .true.
        highlighter%current_lang%has_preprocessor = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(32))
        highlighter%current_lang%keywords = [ &
            "auto        ", "break       ", "case        ", "char        ", &
            "const       ", "continue    ", "default     ", "do          ", &
            "double      ", "else        ", "enum        ", "extern      ", &
            "float       ", "for         ", "goto        ", "if          ", &
            "inline      ", "int         ", "long        ", "register    ", &
            "return      ", "short       ", "signed      ", "sizeof      ", &
            "static      ", "struct      ", "switch      ", "typedef     ", &
            "union       ", "unsigned    ", "void        ", "while       " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(8))
        highlighter%current_lang%types = [ &
            "size_t      ", "uint32_t    ", "int32_t     ", "uint64_t    ", &
            "int64_t     ", "bool        ", "FILE        ", "NULL        " &
        ]

        highlighter%current_lang%comment_single = "//"
        highlighter%current_lang%comment_start = "/*"
        highlighter%current_lang%comment_end = "*/"

        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['"', "'"]

        allocate(highlighter%current_lang%operators(20))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "%   ", "=   ", &
            "==  ", "!=  ", "<   ", ">   ", "<=  ", ">=  ", &
            "&&  ", "||  ", "!   ", "&   ", "|   ", "^   ", &
            "<<  ", ">>  " &
        ]

        highlighter%enabled = .true.
    end subroutine load_c_syntax

    ! C++ language support
    subroutine load_cpp_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "cpp"
        highlighter%current_lang%case_sensitive = .true.
        highlighter%current_lang%has_preprocessor = .true.

        ! Keywords (C++ specific + C keywords)
        allocate(highlighter%current_lang%keywords(48))
        highlighter%current_lang%keywords = [ &
            "auto        ", "break       ", "case        ", "char        ", &
            "const       ", "continue    ", "default     ", "do          ", &
            "double      ", "else        ", "enum        ", "extern      ", &
            "float       ", "for         ", "goto        ", "if          ", &
            "inline      ", "int         ", "long        ", "register    ", &
            "return      ", "short       ", "signed      ", "sizeof      ", &
            "static      ", "struct      ", "switch      ", "typedef     ", &
            "union       ", "unsigned    ", "void        ", "while       ", &
            "class       ", "namespace   ", "template    ", "typename    ", &
            "new         ", "delete      ", "this        ", "friend      ", &
            "virtual     ", "override    ", "final       ", "public      ", &
            "private     ", "protected   ", "try         ", "catch       " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(12))
        highlighter%current_lang%types = [ &
            "std         ", "string      ", "vector      ", "map         ", &
            "set         ", "pair        ", "unique_ptr  ", "shared_ptr  ", &
            "nullptr     ", "true        ", "false       ", "bool        " &
        ]

        highlighter%current_lang%comment_single = "//"
        highlighter%current_lang%comment_start = "/*"
        highlighter%current_lang%comment_end = "*/"

        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['"', "'"]

        allocate(highlighter%current_lang%operators(22))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "%   ", "=   ", &
            "==  ", "!=  ", "<   ", ">   ", "<=  ", ">=  ", &
            "&&  ", "||  ", "!   ", "&   ", "|   ", "^   ", &
            "<<  ", ">>  ", "::  ", "->  " &
        ]

        highlighter%enabled = .true.
    end subroutine load_cpp_syntax

    ! Rust language support
    subroutine load_rust_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "rust"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(40))
        highlighter%current_lang%keywords = [ &
            "as          ", "async       ", "await       ", "break       ", &
            "const       ", "continue    ", "crate       ", "dyn         ", &
            "else        ", "enum        ", "extern      ", "false       ", &
            "fn          ", "for         ", "if          ", "impl        ", &
            "in          ", "let         ", "loop        ", "match       ", &
            "mod         ", "move        ", "mut         ", "pub         ", &
            "ref         ", "return      ", "self        ", "Self        ", &
            "static      ", "struct      ", "super       ", "trait       ", &
            "true        ", "type        ", "unsafe      ", "use         ", &
            "where       ", "while       ", "async       ", "await       " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(16))
        highlighter%current_lang%types = [ &
            "i8          ", "i16         ", "i32         ", "i64         ", &
            "i128        ", "u8          ", "u16         ", "u32         ", &
            "u64         ", "u128        ", "f32         ", "f64         ", &
            "bool        ", "char        ", "str         ", "String      " &
        ]

        highlighter%current_lang%comment_single = "//"
        highlighter%current_lang%comment_start = "/*"
        highlighter%current_lang%comment_end = "*/"

        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['"', "'"]

        allocate(highlighter%current_lang%operators(20))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "%   ", "=   ", &
            "==  ", "!=  ", "<   ", ">   ", "<=  ", ">=  ", &
            "&&  ", "||  ", "!   ", "&   ", "|   ", "^   ", &
            "::  ", "->  " &
        ]

        highlighter%enabled = .true.
    end subroutine load_rust_syntax

    ! Wolf. The keyword list is the closed reserved set from the language's
    ! own grammar -- all 50 of them, no more and no less. Contextual keywords
    ! (c, rc, pool, from, timeout, noalias, pkg, reg, self) are deliberately
    ! absent: each is an ordinary identifier everywhere except one position,
    ! and a table that cannot see position would mis-colour them far more
    ! often than not.
    subroutine load_wolf_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "wolf"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(50))
        highlighter%current_lang%keywords = [ &
            "as      ", "asm     ", "assume  ", "borrow  ", "break   ", &
            "comptime", "const   ", "continue", "copy    ", "defer   ", &
            "distinct", "dyn     ", "else    ", "enum    ", "errdefer", &
            "export  ", "extern  ", "false   ", "fn      ", "for     ", &
            "freeze  ", "handle  ", "if      ", "impl    ", "import  ", &
            "in      ", "let     ", "loop    ", "match   ", "move    ", &
            "mut     ", "proc    ", "pub     ", "region  ", "return  ", &
            "scope   ", "select  ", "shared  ", "spawn   ", "struct  ", &
            "take    ", "trait   ", "true    ", "type    ", "unsafe  ", &
            "use     ", "var     ", "weak    ", "when    ", "while   " &
        ]

        ! Types. Small on purpose: the language has no fixed-width scalar
        ! inventory yet, so i32/u64/f64 are NOT types here. Inventing them
        ! would teach a user a language that does not exist.
        allocate(highlighter%current_lang%types(4))
        highlighter%current_lang%types = [ &
            "bool", "int ", "str ", "Self" &
        ]

        ! Line comments only -- wolf has no block comment form, so a '/*' is
        ! two operator tokens and comment_start/comment_end stay empty.
        highlighter%current_lang%comment_single = "//"

        ! Triple quote FIRST: process_string takes the first delimiter that
        ! matches, and only a delimiter of length >= 3 turns on multiline
        ! mode. Listing '"' first would make the block-string form unreachable.
        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['""" ', '"   ']

        ! Every wolf string is an f-string ([gram.lex.str]), both forms.
        highlighter%current_lang%interpolated_strings = .true.

        allocate(highlighter%current_lang%operators(37))
        highlighter%current_lang%operators = [ &
            "<=> ", "<<= ", ">>= ", "..= ", "==  ", "!=  ", &
            "<=  ", ">=  ", "&&  ", "||  ", "<<  ", ">>  ", &
            "..  ", "->  ", "=>  ", "+=  ", "-=  ", "*=  ", &
            "/=  ", "%=  ", "&=  ", "|=  ", "^=  ", "+   ", &
            "-   ", "*   ", "/   ", "%   ", "&   ", "|   ", &
            "^   ", "!   ", "<   ", ">   ", "=   ", "?   ", &
            "@   " &
        ]

        highlighter%enabled = .true.
    end subroutine load_wolf_syntax

    ! Go language support
    subroutine load_go_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "go"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(25))
        highlighter%current_lang%keywords = [ &
            "break       ", "case        ", "chan        ", "const       ", &
            "continue    ", "default     ", "defer       ", "else        ", &
            "fallthrough ", "for         ", "func        ", "go          ", &
            "goto        ", "if          ", "import      ", "interface   ", &
            "map         ", "package     ", "range       ", "return      ", &
            "select      ", "struct      ", "switch      ", "type        ", &
            "var         " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(15))
        highlighter%current_lang%types = [ &
            "bool        ", "byte        ", "complex64   ", "complex128  ", &
            "error       ", "float32     ", "float64     ", "int         ", &
            "int8        ", "int16       ", "int32       ", "int64       ", &
            "string      ", "uint        ", "nil         " &
        ]

        highlighter%current_lang%comment_single = "//"
        highlighter%current_lang%comment_start = "/*"
        highlighter%current_lang%comment_end = "*/"

        allocate(highlighter%current_lang%string_delimiters(3))
        highlighter%current_lang%string_delimiters = ['"', "'", '`']

        allocate(highlighter%current_lang%operators(18))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "%   ", "=   ", &
            "==  ", "!=  ", "<   ", ">   ", "<=  ", ">=  ", &
            "&&  ", "||  ", "!   ", "&   ", "|   ", ":=  " &
        ]

        highlighter%enabled = .true.
    end subroutine load_go_syntax

    ! JavaScript language support
    subroutine load_javascript_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "javascript"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(38))
        highlighter%current_lang%keywords = [ &
            "async       ", "await       ", "break       ", "case        ", &
            "catch       ", "class       ", "const       ", "continue    ", &
            "debugger    ", "default     ", "delete      ", "do          ", &
            "else        ", "export      ", "extends     ", "finally     ", &
            "for         ", "function    ", "if          ", "import      ", &
            "in          ", "instanceof  ", "let         ", "new         ", &
            "return      ", "super       ", "switch      ", "this        ", &
            "throw       ", "try         ", "typeof      ", "var         ", &
            "void        ", "while       ", "with        ", "yield       ", &
            "true        ", "false       " &
        ]

        ! Types
        allocate(highlighter%current_lang%types(10))
        highlighter%current_lang%types = [ &
            "null        ", "undefined   ", "Boolean     ", "Number      ", &
            "String      ", "Symbol      ", "Object      ", "Array       ", &
            "Function    ", "Promise     " &
        ]

        highlighter%current_lang%comment_single = "//"
        highlighter%current_lang%comment_start = "/*"
        highlighter%current_lang%comment_end = "*/"

        allocate(highlighter%current_lang%string_delimiters(3))
        highlighter%current_lang%string_delimiters = ['"', "'", '`']

        allocate(highlighter%current_lang%operators(20))
        highlighter%current_lang%operators = [ &
            "+   ", "-   ", "*   ", "/   ", "%   ", "=   ", &
            "==  ", "=== ", "!=  ", "!== ", "<   ", ">   ", &
            "<=  ", ">=  ", "&&  ", "||  ", "!   ", "?   ", &
            ":   ", "=>  " &
        ]

        highlighter%enabled = .true.
    end subroutine load_javascript_syntax

    ! TypeScript language support (extends JavaScript)
    subroutine load_typescript_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        ! Start with JavaScript syntax
        call load_javascript_syntax(highlighter)
        highlighter%current_lang%name = "typescript"

        ! Add TypeScript-specific keywords
        deallocate(highlighter%current_lang%keywords)
        allocate(highlighter%current_lang%keywords(45))
        highlighter%current_lang%keywords = [ &
            "async       ", "await       ", "break       ", "case        ", &
            "catch       ", "class       ", "const       ", "continue    ", &
            "debugger    ", "default     ", "delete      ", "do          ", &
            "else        ", "export      ", "extends     ", "finally     ", &
            "for         ", "function    ", "if          ", "import      ", &
            "in          ", "instanceof  ", "let         ", "new         ", &
            "return      ", "super       ", "switch      ", "this        ", &
            "throw       ", "try         ", "typeof      ", "var         ", &
            "void        ", "while       ", "with        ", "yield       ", &
            "true        ", "false       ", "enum        ", "interface   ", &
            "type        ", "namespace   ", "module      ", "declare     ", &
            "abstract    " &
        ]

        ! Add TypeScript types
        deallocate(highlighter%current_lang%types)
        allocate(highlighter%current_lang%types(15))
        highlighter%current_lang%types = [ &
            "null        ", "undefined   ", "boolean     ", "number      ", &
            "string      ", "symbol      ", "object      ", "any         ", &
            "unknown     ", "never       ", "void        ", "Array       ", &
            "Function    ", "Promise     ", "ReadonlyArra" &
        ]

        highlighter%enabled = .true.
    end subroutine load_typescript_syntax

    ! Bash/Shell script support
    subroutine load_bash_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "bash"
        highlighter%current_lang%case_sensitive = .true.

        ! Keywords
        allocate(highlighter%current_lang%keywords(30))
        highlighter%current_lang%keywords = [ &
            "if          ", "then        ", "else        ", "elif        ", &
            "fi          ", "for         ", "while       ", "do          ", &
            "done        ", "case        ", "esac        ", "function    ", &
            "return      ", "break       ", "continue    ", "exit        ", &
            "export      ", "source      ", "alias       ", "unset       ", &
            "shift       ", "local       ", "declare     ", "readonly    ", &
            "echo        ", "printf      ", "read        ", "cd          ", &
            "pwd         ", "ls          " &
        ]

        ! Built-in variables/types
        allocate(highlighter%current_lang%types(10))
        highlighter%current_lang%types = [ &
            "$0          ", "$1          ", "$@          ", "$*          ", &
            "$#          ", "$?          ", "$$          ", "$!          ", &
            "true        ", "false       " &
        ]

        highlighter%current_lang%comment_single = "#"

        allocate(highlighter%current_lang%string_delimiters(3))
        highlighter%current_lang%string_delimiters = ['"', "'", '`']

        allocate(highlighter%current_lang%operators(15))
        highlighter%current_lang%operators = [ &
            "=   ", "==  ", "!=  ", "<   ", ">   ", &
            "-eq ", "-ne ", "-lt ", "-gt ", "-le ", &
            "-ge ", "&&  ", "||  ", "|   ", "&   " &
        ]

        highlighter%enabled = .true.
    end subroutine load_bash_syntax

    ! Markdown support (special handling needed)
    subroutine load_markdown_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "markdown"
        highlighter%current_lang%case_sensitive = .true.

        ! Headers and emphasis markers as "keywords"
        allocate(highlighter%current_lang%keywords(8))
        highlighter%current_lang%keywords = [ &
            "#           ", "##          ", "###         ", "####        ", &
            "#####       ", "######      ", "*           ", "_           " &
        ]

        ! Code block languages as "types"
        allocate(highlighter%current_lang%types(10))
        highlighter%current_lang%types = [ &
            "```         ", "```python   ", "```bash     ", "```fortran  ", &
            "```c        ", "```cpp      ", "```rust     ", "```go       ", &
            "```javascri ", "```typescri " &
        ]

        ! No traditional comments in markdown
        highlighter%current_lang%comment_single = ""

        ! Links and code spans
        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['`', '[']

        ! List markers and special chars
        allocate(highlighter%current_lang%operators(6))
        highlighter%current_lang%operators = [ &
            "-   ", "+   ", "*   ", ">   ", "|   ", "!   " &
        ]

        highlighter%enabled = .true.
    end subroutine load_markdown_syntax

    ! Makefile support
    subroutine load_makefile_syntax(highlighter)
        type(syntax_highlighter_t), intent(inout) :: highlighter

        highlighter%current_lang%name = "makefile"
        highlighter%current_lang%case_sensitive = .true.

        ! Directives and built-in functions treated as keywords
        allocate(highlighter%current_lang%keywords(24))
        highlighter%current_lang%keywords = [ &
            "include     ", "-include    ", "sinclude    ", "define      ", &
            "endef       ", "ifdef       ", "ifndef      ", "ifeq        ", &
            "ifneq       ", "else        ", "endif       ", "export      ", &
            "unexport    ", "override    ", "vpath       ", "wildcard    ", &
            "patsubst    ", "foreach     ", "shell       ", "subst       ", &
            "filter      ", "notdir      ", "basename    ", "addprefix   " &
        ]

        ! Automatic variables and common special targets as types
        allocate(highlighter%current_lang%types(12))
        highlighter%current_lang%types = [ &
            "$@          ", "$<          ", "$^          ", "$?          ", &
            "$*          ", "$+          ", "$|          ", ".PHONY      ", &
            ".DEFAULT    ", ".PRECIOUS   ", ".SUFFIXES   ", ".NOTPARALLEL" &
        ]

        highlighter%current_lang%comment_single = "#"

        allocate(highlighter%current_lang%string_delimiters(2))
        highlighter%current_lang%string_delimiters = ['"', "'"]

        allocate(highlighter%current_lang%operators(7))
        highlighter%current_lang%operators = [ &
            "=   ", ":=  ", "?=  ", "+=  ", ":   ", "@   ", "|   " &
        ]

        highlighter%enabled = .true.
    end subroutine load_makefile_syntax

    ! Handle multiline comment start (when we encounter /* on a line)
    subroutine process_multiline_comment_start(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: start_pos, end_pos, comment_start_len, comment_end_len, line_len

        line_len = len(line)
        start_pos = pos
        comment_start_len = len_trim(highlighter%current_lang%comment_start)
        comment_end_len = len_trim(highlighter%current_lang%comment_end)

        ! Skip past the opening delimiter
        pos = pos + comment_start_len

        ! Look for closing delimiter on the same line
        end_pos = index(line(pos:), trim(highlighter%current_lang%comment_end))

        if (end_pos > 0) then
            ! Found closing delimiter on same line - this is a complete comment
            end_pos = pos + end_pos - 1 + comment_end_len - 1

            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_COMMENT
            tokens(token_count)%start_col = start_pos
            tokens(token_count)%end_col = end_pos

            pos = end_pos + 1
        else
            ! Closing delimiter not found - rest of line is comment, enter multiline mode
            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_COMMENT
            tokens(token_count)%start_col = start_pos
            tokens(token_count)%end_col = line_len

            highlighter%in_multiline_comment = .true.
            pos = line_len + 1
        end if
    end subroutine process_multiline_comment_start

    ! Handle multiline comment continuation (when we start a line in comment mode)
    subroutine process_multiline_comment(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: end_pos, comment_end_len, line_len

        line_len = len(line)
        comment_end_len = len_trim(highlighter%current_lang%comment_end)

        ! Look for closing delimiter
        end_pos = index(line(pos:), trim(highlighter%current_lang%comment_end))

        if (end_pos > 0) then
            ! Found closing delimiter on this line
            end_pos = pos + end_pos - 1 + comment_end_len - 1

            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_COMMENT
            tokens(token_count)%start_col = pos
            tokens(token_count)%end_col = end_pos

            ! Exit multiline comment mode
            highlighter%in_multiline_comment = .false.
            pos = end_pos + 1
        else
            ! Closing delimiter not found - entire rest of line is comment
            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_COMMENT
            tokens(token_count)%start_col = pos
            tokens(token_count)%end_col = line_len

            ! Stay in multiline comment mode
            pos = line_len + 1
        end if
    end subroutine process_multiline_comment

    subroutine process_multiline_string(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: end_pos, delim_len, line_len

        ! f-string bodies need the interpolations picked out of the run, so
        ! the continuation line takes the same walk the opening line took.
        if (highlighter%current_lang%interpolated_strings) then
            call process_multiline_interp(highlighter, line, tokens, token_count, pos)
            return
        end if

        line_len = len(line)
        ! string_delimiter is fixed width (len=4), so a stored """ arrives
        ! blank-padded to '""" '. trim at both uses or the search looks for
        ! triple-quote-plus-space, never matches a closing """ at end of
        ! line, and multiline string mode never exits.
        delim_len = len_trim(highlighter%string_delimiter)

        ! Look for closing delimiter
        end_pos = index(line(pos:), trim(highlighter%string_delimiter))

        if (end_pos > 0) then
            ! Found closing delimiter on this line
            end_pos = pos + end_pos - 1 + delim_len - 1

            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_STRING
            tokens(token_count)%start_col = pos
            tokens(token_count)%end_col = end_pos

            ! Exit multiline string mode
            highlighter%in_multiline_string = .false.
            highlighter%string_delimiter = ""
            pos = end_pos + 1
        else
            ! Closing delimiter not found - entire rest of line is string
            token_count = token_count + 1
            tokens(token_count)%type = TOKEN_STRING
            tokens(token_count)%start_col = pos
            tokens(token_count)%end_col = line_len

            ! Stay in multiline string mode
            pos = line_len + 1
        end if
    end subroutine process_multiline_string

end module syntax_highlighter_module
