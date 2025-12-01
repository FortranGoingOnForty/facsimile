module syntax_highlighter_module
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: syntax_highlighter_t
    public :: token_t
    public :: init_highlighter, cleanup_highlighter
    public :: tokenize_line, get_token_color
    public :: detect_language
    public :: TOKEN_PLAIN, TOKEN_KEYWORD, TOKEN_STRING, TOKEN_NUMBER
    public :: TOKEN_COMMENT, TOKEN_OPERATOR, TOKEN_TYPE, TOKEN_FUNCTION
    public :: TOKEN_PREPROCESSOR

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
    end type language_def_t

    ! Main highlighter type
    type :: syntax_highlighter_t
        type(language_def_t) :: current_lang
        logical :: enabled = .false.
        logical :: in_multiline_comment = .false.
        logical :: in_multiline_string = .false.
        character(len=4) :: string_delimiter = ""
    end type syntax_highlighter_t

    ! Color mapping (ANSI escape codes)
    character(len=*), parameter :: COLOR_KEYWORD = char(27) // '[1;34m'     ! Bold Blue
    character(len=*), parameter :: COLOR_STRING = char(27) // '[32m'        ! Green
    character(len=*), parameter :: COLOR_NUMBER = char(27) // '[35m'        ! Magenta
    character(len=*), parameter :: COLOR_COMMENT = char(27) // '[90m'       ! Gray
    character(len=*), parameter :: COLOR_OPERATOR = char(27) // '[33m'      ! Yellow
    character(len=*), parameter :: COLOR_TYPE = char(27) // '[36m'          ! Cyan
    character(len=*), parameter :: COLOR_FUNCTION = char(27) // '[1;36m'    ! Bold Cyan
    character(len=*), parameter :: COLOR_PREPROC = char(27) // '[95m'       ! Light Magenta
    character(len=*), parameter :: COLOR_RESET = char(27) // '[0m'

contains

    subroutine init_highlighter(highlighter, filename)
        type(syntax_highlighter_t), intent(out) :: highlighter
        character(len=*), intent(in), optional :: filename

        highlighter%enabled = .false.
        highlighter%in_multiline_comment = .false.
        highlighter%in_multiline_string = .false.

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
        character(len=:), allocatable :: extension
        integer :: dot_pos

        ! Find file extension
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            extension = filename(dot_pos:)

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
            case default
                highlighter%enabled = .false.
            end select
        end if
    end subroutine detect_language

    subroutine tokenize_line(highlighter, line, tokens)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), allocatable, intent(out) :: tokens(:)
        integer :: i, line_len, token_count

        if (.not. highlighter%enabled) then
            allocate(tokens(1))
            tokens(1)%type = TOKEN_PLAIN
            tokens(1)%start_col = 1
            tokens(1)%end_col = len(line)
            return
        end if

        line_len = len(line)
        allocate(tokens(line_len))  ! Worst case: each char is a token
        token_count = 0
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

            ! Check for single-line comment
            if (check_comment_start(highlighter, line, i)) then
                token_count = token_count + 1
                tokens(token_count)%type = TOKEN_COMMENT
                tokens(token_count)%start_col = i
                tokens(token_count)%end_col = line_len
                exit
            end if

            ! Check for string
            if (check_string_start(highlighter, line, i)) then
                call process_string(highlighter, line, tokens, token_count, i)

            ! Check for number
            else if (is_digit(ch) .or. (ch == '.' .and. i + 1 <= line_len .and. is_digit(line(i+1:i+1)))) then
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
            color = COLOR_KEYWORD
        case(TOKEN_STRING)
            color = COLOR_STRING
        case(TOKEN_NUMBER)
            color = COLOR_NUMBER
        case(TOKEN_COMMENT)
            color = COLOR_COMMENT
        case(TOKEN_OPERATOR)
            color = COLOR_OPERATOR
        case(TOKEN_TYPE)
            color = COLOR_TYPE
        case(TOKEN_FUNCTION)
            color = COLOR_FUNCTION
        case(TOKEN_PREPROCESSOR)
            color = COLOR_PREPROC
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

        ! String delimiters
        allocate(highlighter%current_lang%string_delimiters(4))
        highlighter%current_lang%string_delimiters = ['"   ', "'   ", '""" ', "''' "]

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

    subroutine process_string(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        integer :: i, start_pos, delim_len, line_len
        character(len=:), allocatable :: delimiter
        logical :: found_end

        line_len = len(line)
        start_pos = pos

        ! Initialize delimiter (defensive programming - should always be set in loop below)
        delimiter = '"'  ! Default fallback

        ! Find which delimiter matches
        do i = 1, size(highlighter%current_lang%string_delimiters)
            delim_len = len_trim(highlighter%current_lang%string_delimiters(i))
            if (pos + delim_len - 1 <= line_len) then
                if (line(pos:pos+delim_len-1) == trim(highlighter%current_lang%string_delimiters(i))) then
                    delimiter = trim(highlighter%current_lang%string_delimiters(i))
                    exit
                end if
            end if
        end do

        ! Move past opening delimiter
        pos = pos + len(delimiter)

        ! Find closing delimiter
        found_end = .false.
        do while (pos <= line_len)
            if (line(pos:pos) == '\' .and. pos < line_len) then
                ! Skip escaped character
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
            pos = line_len + 1
        end if
    end subroutine process_string

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
                if (pos <= len(line) .and. (line(pos:pos) == '+' .or. line(pos:pos) == '-')) then
                    pos = pos + 1
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

        ! Find end of word
        do while (pos <= len(line) .and. is_alnum(line(pos:pos)))
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

    ! Stub implementations for multiline handling
    subroutine process_multiline_comment(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        ! TODO: Implement multiline comment handling
    end subroutine process_multiline_comment

    subroutine process_multiline_string(highlighter, line, tokens, token_count, pos)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: line
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count, pos
        ! TODO: Implement multiline string handling
    end subroutine process_multiline_string

end module syntax_highlighter_module