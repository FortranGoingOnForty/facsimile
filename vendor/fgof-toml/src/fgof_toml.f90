module fgof_toml
  use, intrinsic :: ieee_arithmetic, only : ieee_negative_inf, ieee_positive_inf, ieee_quiet_nan, ieee_value
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: TOML_KIND_NONE = 0
  integer, parameter, public :: TOML_KIND_STRING = 1
  integer, parameter, public :: TOML_KIND_INTEGER = 2
  integer, parameter, public :: TOML_KIND_FLOAT = 3
  integer, parameter, public :: TOML_KIND_BOOLEAN = 4
  integer, parameter, public :: TOML_KIND_DATETIME = 5
  integer, parameter, public :: TOML_KIND_ARRAY = 6
  integer, parameter, public :: TOML_KIND_TABLE = 7
  character(len=1), parameter :: BACKSLASH = achar(92)

  type, public :: toml_string
    character(len=:), allocatable :: text
  end type toml_string

  type, public :: toml_value
    integer :: kind = TOML_KIND_NONE
    type(toml_string) :: string_value
    integer(int64) :: integer_value = 0_int64
    real(real64) :: float_value = 0.0_real64
    logical :: boolean_value = .false.
    logical :: table_explicit = .false.
    logical :: table_inline = .false.
    type(toml_value), allocatable :: array_values(:)
    type(toml_string), allocatable :: table_keys(:)
    type(toml_value), allocatable :: table_values(:)
  end type toml_value

  type, public :: toml_array
    type(toml_value), allocatable :: values(:)
  contains
    procedure :: length => array_length
  end type toml_array

  type, public :: toml_table
    type(toml_string), allocatable :: keys(:)
    type(toml_value), allocatable :: values(:)
  contains
    procedure :: length => table_length
  end type toml_table

  type, public :: toml_error
    logical :: failed = .false.
    integer :: line = 0
    integer :: column = 0
    character(len=:), allocatable :: message
  end type toml_error

  type, public :: toml_document
    type(toml_value) :: root
  contains
    procedure :: has_key => document_has_key
    procedure :: get_string => document_get_string
    procedure :: get_integer => document_get_integer
    procedure :: get_float => document_get_float
    procedure :: get_boolean => document_get_boolean
    procedure :: get_table => document_get_table
    procedure :: get_array => document_get_array
  end type toml_document

  public :: clear_toml_error
  public :: parse_file
  public :: parse_string

contains

  subroutine parse_file(path, document, error)
    character(len=*), intent(in) :: path
    type(toml_document), intent(out) :: document
    type(toml_error), intent(out) :: error
    character(len=:), allocatable :: content
    integer :: io_status
    integer :: size_bytes
    integer :: unit

    call clear_toml_error(error)
    inquire(file=path, size=size_bytes, iostat=io_status)
    if (io_status /= 0 .or. size_bytes < 0) then
      call set_error(error, 0, 0, "failed to stat TOML file")
      document%root = make_table_value()
      return
    end if

    allocate(character(len=size_bytes) :: content)
    open(newunit=unit, file=path, access="stream", form="unformatted", &
         action="read", status="old", iostat=io_status)
    if (io_status /= 0) then
      call set_error(error, 0, 0, "failed to open TOML file")
      document%root = make_table_value()
      return
    end if

    if (size_bytes > 0) read(unit, iostat=io_status) content
    close(unit)
    if (io_status /= 0 .and. size_bytes > 0) then
      call set_error(error, 0, 0, "failed to read TOML file")
      document%root = make_table_value()
      return
    end if

    call parse_string(content, document, error)
  end subroutine parse_file

  subroutine parse_string(content, document, error)
    character(len=*), intent(in) :: content
    type(toml_document), intent(out) :: document
    type(toml_error), intent(out) :: error
    type(toml_string), allocatable :: current_path(:)
    character(len=:), allocatable :: array_buffer
    character(len=:), allocatable :: multiline_buffer
    integer :: array_start_line
    integer :: line_end
    integer :: line_no
    integer :: multiline_start_line
    integer :: line_start
    character(len=:), allocatable :: line
    logical :: accumulating_array
    logical :: accumulating_multiline

    call clear_toml_error(error)
    document%root = make_table_value()
    allocate(current_path(0))

    accumulating_array = .false.
    accumulating_multiline = .false.
    array_buffer = ""
    multiline_buffer = ""
    array_start_line = 0
    multiline_start_line = 0
    line_no = 1
    line_start = 1
    do while (line_start <= len(content))
      line_end = index(content(line_start:), new_line('a'))
      if (line_end == 0) then
        line = content(line_start:)
        line_start = len(content) + 1
      else
        line = content(line_start:line_start + line_end - 2)
        line_start = line_start + line_end
      end if

      line = trim_carriage_return(line)
      if (accumulating_multiline) then
        multiline_buffer = multiline_buffer // new_line('a') // line
        if (multiline_assignment_complete(multiline_buffer)) then
          call parse_line(multiline_buffer, multiline_start_line, current_path, document, error)
          if (error%failed) return
          accumulating_multiline = .false.
          multiline_buffer = ""
        end if
      else if (accumulating_array) then
        array_buffer = array_buffer // " " // trim_toml_whitespace(strip_comment(line))
        if (array_assignment_complete(array_buffer)) then
          call parse_line(array_buffer, array_start_line, current_path, document, error)
          if (error%failed) return
          accumulating_array = .false.
          array_buffer = ""
        end if
      else if (starts_multiline_assignment(line) .and. .not. multiline_assignment_complete(line)) then
        accumulating_multiline = .true.
        multiline_start_line = line_no
        multiline_buffer = line
      else if (starts_array_assignment(line) .and. .not. array_assignment_complete(line)) then
        accumulating_array = .true.
        array_start_line = line_no
        array_buffer = trim_toml_whitespace(strip_comment(line))
      else
        call parse_line(line, line_no, current_path, document, error)
        if (error%failed) return
      end if
      line_no = line_no + 1
    end do

    if (accumulating_multiline) then
      call set_error(error, multiline_start_line, 1, "unterminated multiline string")
    else if (accumulating_array) then
      call set_error(error, array_start_line, 1, "unterminated array")
    end if
  end subroutine parse_string

  subroutine parse_line(raw_line, line_no, current_path, document, error)
    character(len=*), intent(in) :: raw_line
    integer, intent(in) :: line_no
    type(toml_string), allocatable, intent(inout) :: current_path(:)
    type(toml_document), intent(inout) :: document
    type(toml_error), intent(inout) :: error
    character(len=:), allocatable :: key_text
    character(len=:), allocatable :: line
    character(len=:), allocatable :: value_text
    integer :: equals_at
    type(toml_string), allocatable :: key_parts(:)
    type(toml_value) :: value

    line = trim_toml_whitespace(strip_comment(raw_line))
    if (len(line) == 0) return

    if (starts_with(line, "[[")) then
      call parse_array_table_header(line, line_no, current_path, document%root, error)
      return
    end if

    if (line(1:1) == "[") then
      call parse_table_header(line, line_no, current_path, document%root, error)
      return
    end if

    equals_at = find_top_level_char(line, "=")
    if (equals_at == 0) then
      call set_error(error, line_no, 1, "expected key/value assignment")
      return
    end if

    key_text = line(:equals_at - 1)
    value_text = line(equals_at + 1:)
    call parse_key_path(trim_toml_whitespace(key_text), key_parts, error, line_no, 1)
    if (error%failed) return

    call parse_value(trim_toml_whitespace(value_text), value, error, line_no, equals_at + 1)
    if (error%failed) return
    call set_value_in_context(document%root, current_path, key_parts, value, line_no, 1, error)
  end subroutine parse_line

  subroutine parse_array_table_header(line, line_no, current_path, root, error)
    character(len=*), intent(in) :: line
    integer, intent(in) :: line_no
    type(toml_string), allocatable, intent(inout) :: current_path(:)
    type(toml_value), intent(inout) :: root
    type(toml_error), intent(inout) :: error
    type(toml_string), allocatable :: path(:)
    character(len=:), allocatable :: inner
    integer :: line_length

    line_length = len_trim(line)
    if (line_length < 5 .or. line(line_length - 1:line_length) /= "]]" ) then
      call set_error(error, line_no, 1, "unterminated array-of-tables header")
      return
    end if

    inner = trim_toml_whitespace(line(3:line_length - 2))
    call parse_key_path(inner, path, error, line_no, 3)
    if (error%failed) return
    if (size(path) == 0) then
      call set_error(error, line_no, 3, "empty array-of-tables header")
      return
    end if

    call append_array_table_path(root, path, line_no, 3, error)
    if (error%failed) return
    call copy_path(path, current_path)
  end subroutine parse_array_table_header

  subroutine parse_table_header(line, line_no, current_path, root, error)
    character(len=*), intent(in) :: line
    integer, intent(in) :: line_no
    type(toml_string), allocatable, intent(inout) :: current_path(:)
    type(toml_value), intent(inout) :: root
    type(toml_error), intent(inout) :: error
    type(toml_string), allocatable :: path(:)
    character(len=:), allocatable :: inner
    integer :: line_length

    line_length = len_trim(line)
    if (line_length < 3 .or. line(line_length:line_length) /= "]") then
      call set_error(error, line_no, 1, "unterminated table header")
      return
    end if

    inner = trim_toml_whitespace(line(2:line_length - 1))
    call parse_key_path(inner, path, error, line_no, 2)
    if (error%failed) return
    if (size(path) == 0) then
      call set_error(error, line_no, 2, "empty table header")
      return
    end if

    call define_table_path(root, path, line_no, 2, error)
    if (error%failed) return
    call copy_path(path, current_path)
  end subroutine parse_table_header

  recursive subroutine parse_value(text, value, error, line_no, column)
    character(len=*), intent(in) :: text
    type(toml_value), intent(out) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: clean
    character(len=:), allocatable :: parsed
    integer :: end_at

    clean = trim_toml_whitespace(text)
    if (len(clean) == 0) then
      call set_error(error, line_no, column, "expected value")
      return
    end if

    select case (clean(1:1))
    case ('"', "'")
      call parse_quoted_string(clean, parsed, end_at, error, line_no, column)
      if (error%failed) return
      if (len(trim_toml_whitespace(clean(end_at + 1:))) /= 0) then
        call set_error(error, line_no, column + end_at, "unexpected text after string")
        return
      end if
      value = make_string_value(parsed)
    case ("[")
      call parse_array(clean, value, error, line_no, column)
    case ("{")
      call parse_inline_table(clean, value, error, line_no, column)
    case default
      call parse_bare_value(clean, value, error, line_no, column)
    end select
  end subroutine parse_value

  subroutine parse_bare_value(text, value, error, line_no, column)
    character(len=*), intent(in) :: text
    type(toml_value), intent(out) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: normalized
    integer :: io_status
    integer(int64) :: integer_value
    real(real64) :: float_value

    select case (text)
    case ("true")
      value = make_boolean_value(.true.)
      return
    case ("false")
      value = make_boolean_value(.false.)
      return
    case ("inf", "+inf")
      value = make_float_value(ieee_value(0.0_real64, ieee_positive_inf))
      return
    case ("-inf")
      value = make_float_value(ieee_value(0.0_real64, ieee_negative_inf))
      return
    case ("nan", "+nan", "-nan")
      value = make_float_value(ieee_like_nan())
      return
    end select

    if (invalid_underscore_placement(text)) then
      call set_error(error, line_no, column, "invalid underscore placement")
      return
    end if

    normalized = remove_underscores(text)
    if (is_prefixed_integer_literal(text)) then
      call parse_prefixed_integer(normalized, integer_value, io_status)
      if (io_status == 0) then
        value = make_integer_value(integer_value)
        return
      end if
      call set_error(error, line_no, column, "invalid integer literal")
      return
    else if (has_integer_prefix(text)) then
      call set_error(error, line_no, column, "invalid integer literal")
      return
    else if (is_decimal_integer_literal(text)) then
      read(normalized, *, iostat=io_status) integer_value
      if (io_status == 0) then
        value = make_integer_value(integer_value)
        return
      end if
      call set_error(error, line_no, column, "invalid integer literal")
      return
    else if (is_float_literal(text)) then
      read(normalized, *, iostat=io_status) float_value
      if (io_status == 0) then
        value = make_float_value(float_value)
        return
      end if
      call set_error(error, line_no, column, "invalid float literal")
      return
    else if (is_datetime_literal(text)) then
      value = make_datetime_value(text)
      return
    else if (looks_like_number_or_datetime(text)) then
      call set_error(error, line_no, column, "invalid numeric or datetime literal")
      return
    end if

    call set_error(error, line_no, column, "unrecognized TOML value")
  end subroutine parse_bare_value

  subroutine parse_array(text, value, error, line_no, column)
    character(len=*), intent(in) :: text
    type(toml_value), intent(out) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: body
    type(toml_string), allocatable :: items(:)
    type(toml_value), allocatable :: values(:)
    integer :: close_at
    integer :: i

    close_at = matching_end(text, 1, "[", "]")
    if (close_at == 0) then
      call set_error(error, line_no, column, "unterminated array")
      return
    end if
    if (len(trim_toml_whitespace(text(close_at + 1:))) /= 0) then
      call set_error(error, line_no, column + close_at, "unexpected text after array")
      return
    end if

    body = trim_toml_whitespace(text(2:close_at - 1))
    call split_top_level(body, ",", items, error, line_no, column + 1, allow_trailing=.true.)
    if (error%failed) return

    allocate(values(size(items)))
    do i = 1, size(items)
      call parse_value(trim_toml_whitespace(string_text(items(i))), values(i), error, line_no, column + 1)
      if (error%failed) return
    end do
    value = make_array_value(values)
  end subroutine parse_array

  subroutine parse_inline_table(text, value, error, line_no, column)
    character(len=*), intent(in) :: text
    type(toml_value), intent(out) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: body
    character(len=:), allocatable :: pair_text
    type(toml_string), allocatable :: pairs(:)
    type(toml_string), allocatable :: key_parts(:)
    type(toml_value) :: pair_value
    integer :: close_at
    integer :: equals_at
    integer :: i

    close_at = matching_end(text, 1, "{", "}")
    if (close_at == 0) then
      call set_error(error, line_no, column, "unterminated inline table")
      return
    end if
    if (len(trim_toml_whitespace(text(close_at + 1:))) /= 0) then
      call set_error(error, line_no, column + close_at, "unexpected text after inline table")
      return
    end if

    value = make_table_value()
    body = trim_toml_whitespace(text(2:close_at - 1))
    call split_top_level(body, ",", pairs, error, line_no, column + 1, allow_trailing=.false.)
    if (error%failed) return

    do i = 1, size(pairs)
      pair_text = trim_toml_whitespace(string_text(pairs(i)))
      equals_at = find_top_level_char(pair_text, "=")
      if (equals_at == 0) then
        call set_error(error, line_no, column + 1, "expected inline table key/value")
        return
      end if
      call parse_key_path(trim_toml_whitespace(pair_text(:equals_at - 1)), key_parts, error, line_no, column + 1)
      if (error%failed) return
      call parse_value(trim_toml_whitespace(pair_text(equals_at + 1:)), pair_value, error, line_no, column + equals_at)
      if (error%failed) return
      call set_value_at_path(value, key_parts, pair_value, line_no, column + 1, error)
      if (error%failed) return
    end do
    value%table_inline = .true.
  end subroutine parse_inline_table

  subroutine parse_key_path(text, parts, error, line_no, column)
    character(len=*), intent(in) :: text
    type(toml_string), allocatable, intent(out) :: parts(:)
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: key
    integer :: end_at
    integer :: pos

    allocate(parts(0))
    pos = 1
    do
      call skip_spaces(text, pos)
      if (pos > len_trim(text)) exit

      if (text(pos:pos) == '"' .or. text(pos:pos) == "'") then
        call parse_quoted_string(text(pos:), key, end_at, error, line_no, column + pos - 1)
        if (error%failed) return
        pos = pos + end_at
      else
        call parse_bare_key(text, pos, key, error, line_no, column)
        if (error%failed) return
      end if
      call append_string(parts, key)

      call skip_spaces(text, pos)
      if (pos > len_trim(text)) exit
      if (text(pos:pos) /= ".") then
        call set_error(error, line_no, column + pos - 1, "expected dotted key separator")
        return
      end if
      pos = pos + 1
    end do
  end subroutine parse_key_path

  subroutine parse_bare_key(text, pos, key, error, line_no, column)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(out) :: key
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    integer :: start

    start = pos
    do while (pos <= len_trim(text))
      if (.not. bare_key_char(text(pos:pos))) exit
      pos = pos + 1
    end do
    if (pos == start) then
      call set_error(error, line_no, column + pos - 1, "expected key")
      key = ""
      return
    end if
    key = text(start:pos - 1)
  end subroutine parse_bare_key

  subroutine parse_quoted_string(text, value, end_at, error, line_no, column)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(out) :: value
    integer, intent(out) :: end_at
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    character(len=:), allocatable :: body
    character(len=3) :: delimiter
    character(len=1) :: quote
    integer :: pos

    value = ""
    end_at = 0
    if (len(text) == 0) then
      call set_error(error, line_no, column, "expected string")
      return
    end if

    quote = text(1:1)
    if (starts_with(text, repeat(quote, 3))) then
      delimiter = repeat(quote, 3)
      pos = find_multiline_close(text, delimiter, 4)
      if (pos == 0) then
        call set_error(error, line_no, column, "unterminated multiline string")
        return
      end if
      body = text(4:pos - 1)
      call trim_initial_newline(body)
      if (quote == '"') then
        call parse_basic_string_body(body, value, error, line_no, column + 3, multiline=.true.)
      else
        value = body
      end if
      if (error%failed) return
      end_at = pos + 2
      return
    end if

    pos = 2
    do while (pos <= len(text))
      if (text(pos:pos) == quote .and. (quote /= '"' .or. .not. escaped_position(text, pos))) then
        end_at = pos
        body = text(2:pos - 1)
        if (quote == '"') then
          call parse_basic_string_body(body, value, error, line_no, column + 1, multiline=.false.)
        else
          value = body
        end if
        return
      end if
      pos = pos + 1
    end do

    call set_error(error, line_no, column, "unterminated string")
  end subroutine parse_quoted_string

  subroutine parse_basic_string_body(body, value, error, line_no, column, multiline)
    character(len=*), intent(in) :: body
    character(len=:), allocatable, intent(out) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    logical, intent(in) :: multiline
    integer :: pos

    value = ""
    pos = 1
    do while (pos <= len(body))
      if (body(pos:pos) == BACKSLASH) then
        if (multiline .and. line_ending_escape_at(body, pos)) then
          call skip_line_ending_escape(body, pos)
        else
          call append_basic_escape(body, pos, value, error, line_no, column + pos - 1)
          if (error%failed) return
        end if
      else
        value = value // body(pos:pos)
        pos = pos + 1
      end if
    end do
  end subroutine parse_basic_string_body

  subroutine append_basic_escape(text, pos, value, error, line_no, column)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    character(len=:), allocatable, intent(inout) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column

    if (pos + 1 > len(text)) then
      call set_error(error, line_no, column + pos - 1, "unterminated escape sequence")
      return
    end if

    select case (text(pos + 1:pos + 1))
    case ('"')
      value = value // '"'
    case (BACKSLASH)
      value = value // BACKSLASH
    case ("b")
      value = value // char(8)
    case ("t")
      value = value // char(9)
    case ("n")
      value = value // new_line('a')
    case ("f")
      value = value // char(12)
    case ("r")
      value = value // char(13)
    case ("u")
      call append_unicode_escape(text, pos, 4, value, error, line_no, column)
      return
    case ("U")
      call append_unicode_escape(text, pos, 8, value, error, line_no, column)
      return
    case default
      call set_error(error, line_no, column + pos - 1, "unsupported string escape")
      return
    end select
    pos = pos + 2
  end subroutine append_basic_escape

  subroutine append_unicode_escape(text, pos, digit_count, value, error, line_no, column)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    integer, intent(in) :: digit_count
    character(len=:), allocatable, intent(inout) :: value
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    integer :: codepoint
    integer :: digit
    integer :: i

    if (pos + digit_count + 1 > len(text)) then
      call set_error(error, line_no, column + pos - 1, "unterminated unicode escape")
      return
    end if

    codepoint = 0
    do i = pos + 2, pos + digit_count + 1
      digit = hex_digit_value(text(i:i))
      if (digit < 0) then
        call set_error(error, line_no, column + i - 1, "invalid unicode escape digit")
        return
      end if
      codepoint = codepoint * 16 + digit
    end do

    if (.not. valid_unicode_codepoint(codepoint)) then
      call set_error(error, line_no, column + pos - 1, "invalid unicode codepoint")
      return
    end if

    value = value // utf8_from_codepoint(codepoint)
    pos = pos + digit_count + 2
  end subroutine append_unicode_escape

  integer function hex_digit_value(ch) result(value)
    character(len=*), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    if (code >= iachar("0") .and. code <= iachar("9")) then
      value = code - iachar("0")
    else if (code >= iachar("a") .and. code <= iachar("f")) then
      value = 10 + code - iachar("a")
    else if (code >= iachar("A") .and. code <= iachar("F")) then
      value = 10 + code - iachar("A")
    else
      value = -1
    end if
  end function hex_digit_value

  logical function valid_unicode_codepoint(codepoint) result(valid)
    integer, intent(in) :: codepoint

    valid = codepoint >= 0 .and. codepoint <= int(z'10ffff') .and. &
            .not. (codepoint >= int(z'd800') .and. codepoint <= int(z'dfff'))
  end function valid_unicode_codepoint

  function utf8_from_codepoint(codepoint) result(text)
    integer, intent(in) :: codepoint
    character(len=:), allocatable :: text

    if (codepoint <= int(z'7f')) then
      text = achar(codepoint)
    else if (codepoint <= int(z'7ff')) then
      text = achar(int(z'c0') + codepoint / 64) // &
             achar(int(z'80') + modulo(codepoint, 64))
    else if (codepoint <= int(z'ffff')) then
      text = achar(int(z'e0') + codepoint / 4096) // &
             achar(int(z'80') + modulo(codepoint / 64, 64)) // &
             achar(int(z'80') + modulo(codepoint, 64))
    else
      text = achar(int(z'f0') + codepoint / 262144) // &
             achar(int(z'80') + modulo(codepoint / 4096, 64)) // &
             achar(int(z'80') + modulo(codepoint / 64, 64)) // &
             achar(int(z'80') + modulo(codepoint, 64))
    end if
  end function utf8_from_codepoint

  logical function starts_multiline_assignment(line) result(matches)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: stripped
    character(len=:), allocatable :: value_text
    integer :: equals_at

    matches = .false.
    stripped = trim_toml_whitespace(strip_comment(line))
    equals_at = find_top_level_char(stripped, "=")
    if (equals_at == 0) return
    value_text = trim_toml_whitespace(stripped(equals_at + 1:))
    matches = starts_with(value_text, '"""') .or. starts_with(value_text, "'''")
  end function starts_multiline_assignment

  logical function multiline_assignment_complete(line) result(complete)
    character(len=*), intent(in) :: line
    character(len=3) :: delimiter
    character(len=:), allocatable :: stripped
    character(len=:), allocatable :: value_text
    integer :: equals_at

    complete = .true.
    stripped = trim_toml_whitespace(strip_comment(line))
    equals_at = find_top_level_char(stripped, "=")
    if (equals_at == 0) return
    value_text = trim_toml_whitespace(stripped(equals_at + 1:))
    if (starts_with(value_text, '"""')) then
      delimiter = '"""'
    else if (starts_with(value_text, "'''")) then
      delimiter = "'''"
    else
      return
    end if
    complete = find_multiline_close(value_text, delimiter, 4) /= 0
  end function multiline_assignment_complete

  logical function starts_array_assignment(line) result(matches)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: stripped
    character(len=:), allocatable :: value_text
    integer :: equals_at

    matches = .false.
    stripped = trim_toml_whitespace(strip_comment(line))
    equals_at = find_top_level_char(stripped, "=")
    if (equals_at == 0) return
    value_text = trim_toml_whitespace(stripped(equals_at + 1:))
    matches = starts_with(value_text, "[")
  end function starts_array_assignment

  logical function array_assignment_complete(line) result(complete)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: stripped
    character(len=:), allocatable :: value_text
    integer :: close_at
    integer :: equals_at

    complete = .true.
    stripped = trim_toml_whitespace(strip_comment(line))
    equals_at = find_top_level_char(stripped, "=")
    if (equals_at == 0) return
    value_text = trim_toml_whitespace(stripped(equals_at + 1:))
    if (.not. starts_with(value_text, "[")) return

    close_at = matching_end(value_text, 1, "[", "]")
    if (close_at == 0) then
      complete = .false.
    else
      complete = len(trim_toml_whitespace(value_text(close_at + 1:))) == 0
    end if
  end function array_assignment_complete

  integer function find_multiline_close(text, delimiter, start) result(position)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: delimiter
    integer, intent(in) :: start
    integer :: i

    position = 0
    do i = start, len(text) - len(delimiter) + 1
      if (substring_matches(text, i, delimiter)) then
        if (delimiter(1:1) /= '"' .or. .not. escaped_position(text, i)) then
          position = i
          return
        end if
      end if
    end do
  end function find_multiline_close

  logical function escaped_position(text, position) result(escaped)
    character(len=*), intent(in) :: text
    integer, intent(in) :: position
    integer :: count
    integer :: i

    count = 0
    i = position - 1
    do while (i >= 1)
      if (text(i:i) /= BACKSLASH) exit
      count = count + 1
      i = i - 1
    end do
    escaped = modulo(count, 2) == 1
  end function escaped_position

  subroutine trim_initial_newline(text)
    character(len=:), allocatable, intent(inout) :: text

    if (len(text) == 0) return
    if (len(text) >= 2) then
      if (text(1:1) == char(13) .and. text(2:2) == new_line('a')) then
        text = text(3:)
        return
      end if
    end if
    if (text(1:1) == new_line('a')) text = text(2:)
  end subroutine trim_initial_newline

  logical function line_ending_escape_at(text, pos) result(matches)
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos
    integer :: i

    matches = .false.
    i = pos + 1
    do while (i <= len(text))
      if (text(i:i) /= " " .and. text(i:i) /= char(9)) exit
      i = i + 1
    end do
    if (i <= len(text)) matches = text(i:i) == new_line('a')
  end function line_ending_escape_at

  subroutine skip_line_ending_escape(text, pos)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos

    pos = pos + 1
    do while (pos <= len(text))
      if (text(pos:pos) /= " " .and. text(pos:pos) /= char(9)) exit
      pos = pos + 1
    end do
    if (pos <= len(text)) then
      if (text(pos:pos) == new_line('a')) pos = pos + 1
    end if
    do while (pos <= len(text))
      if (text(pos:pos) /= " " .and. text(pos:pos) /= char(9) .and. &
          text(pos:pos) /= new_line('a') .and. text(pos:pos) /= char(13)) return
      pos = pos + 1
    end do
  end subroutine skip_line_ending_escape

  subroutine split_top_level(text, separator, parts, error, line_no, column, allow_trailing)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: separator
    type(toml_string), allocatable, intent(out) :: parts(:)
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    logical, intent(in) :: allow_trailing
    integer :: end_pos
    integer :: start

    allocate(parts(0))
    if (len_trim(text) == 0) return

    start = 1
    do
      end_pos = find_next_top_level_separator(text, separator, start)
      if (end_pos == 0) then
        call append_part_from_span(parts, text, start, len(text), error, line_no, column, .true.)
        return
      end if

      call append_part_from_span(parts, text, start, end_pos - 1, error, line_no, column, .false.)
      if (error%failed) return
      start = end_pos + len(separator)
      if (start > len(text)) then
        if (allow_trailing) then
          return
        else
          call set_error(error, line_no, column + len(text), "trailing list separator")
          return
        end if
      end if
    end do
  end subroutine split_top_level

  subroutine append_part_from_span(parts, text, first, last, error, line_no, column, final_part)
    type(toml_string), allocatable, intent(inout) :: parts(:)
    character(len=*), intent(in) :: text
    integer, intent(in) :: first
    integer, intent(in) :: last
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    logical, intent(in) :: final_part
    character(len=:), allocatable :: part

    if (last < first) then
      if (.not. final_part) call set_error(error, line_no, column + first - 1, "empty list item")
      return
    end if
    part = trim_toml_whitespace(text(first:last))
    if (len(part) == 0) then
      if (.not. final_part) call set_error(error, line_no, column + first - 1, "empty list item")
      return
    end if
    call append_string(parts, part)
  end subroutine append_part_from_span

  recursive subroutine ensure_table_path(table, path, line_no, column, error)
    type(toml_value), intent(inout) :: table
    type(toml_string), intent(in) :: path(:)
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error
    integer :: entry_index
    type(toml_string), allocatable :: remaining_path(:)
    type(toml_value) :: child

    if (size(path) == 0) return
    if (table%kind /= TOML_KIND_TABLE) then
      call set_error(error, line_no, column, "table path collides with scalar value")
      return
    end if

    entry_index = table_entry_index(table, string_text(path(1)))
    if (entry_index == 0) then
      child = make_table_value()
      call append_table_entry(table, string_text(path(1)), child)
      entry_index = table_entry_index(table, string_text(path(1)))
    else if (table%table_values(entry_index)%kind /= TOML_KIND_TABLE) then
      call set_error(error, line_no, column, "table path collides with scalar value")
      return
    end if

    if (size(path) == 1) return
    call path_tail(path, remaining_path)
    call ensure_table_path(table%table_values(entry_index), remaining_path, line_no, column, error)
  end subroutine ensure_table_path

  recursive subroutine define_table_path(table, path, line_no, column, error)
    type(toml_value), intent(inout) :: table
    type(toml_string), intent(in) :: path(:)
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error
    integer :: entry_index
    type(toml_value) :: child
    type(toml_string), allocatable :: remaining_path(:)

    if (size(path) == 0) return
    if (table%kind /= TOML_KIND_TABLE .or. table%table_inline) then
      call set_error(error, line_no, column, "table path collides with non-table value")
      return
    end if

    entry_index = table_entry_index(table, string_text(path(1)))
    if (size(path) == 1) then
      if (entry_index == 0) then
        child = make_table_value(explicit=.true.)
        call append_table_entry(table, string_text(path(1)), child)
      else if (table%table_values(entry_index)%kind /= TOML_KIND_TABLE .or. &
               table%table_values(entry_index)%table_inline) then
        call set_error(error, line_no, column, "table path collides with non-table value")
      else if (table%table_values(entry_index)%table_explicit) then
        call set_error(error, line_no, column, "table already defined")
      else
        table%table_values(entry_index)%table_explicit = .true.
      end if
      return
    end if

    if (entry_index == 0) then
      child = make_table_value()
      call append_table_entry(table, string_text(path(1)), child)
      entry_index = table_entry_index(table, string_text(path(1)))
    else if (table%table_values(entry_index)%kind /= TOML_KIND_TABLE .or. &
             table%table_values(entry_index)%table_inline) then
      call set_error(error, line_no, column, "table path collides with non-table value")
      return
    end if

    call path_tail(path, remaining_path)
    call define_table_path(table%table_values(entry_index), remaining_path, line_no, column, error)
  end subroutine define_table_path

  recursive subroutine set_value_at_path(table, path, value, line_no, column, error)
    type(toml_value), intent(inout) :: table
    type(toml_string), intent(in) :: path(:)
    type(toml_value), intent(in) :: value
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error
    integer :: entry_index
    type(toml_string), allocatable :: remaining_path(:)
    type(toml_value) :: child

    if (size(path) == 0) then
      call set_error(error, line_no, column, "empty key path")
      return
    end if
    if (table%kind /= TOML_KIND_TABLE) then
      call set_error(error, line_no, column, "key path collides with scalar value")
      return
    end if

    entry_index = table_entry_index(table, string_text(path(1)))
    if (size(path) == 1) then
      if (entry_index /= 0) then
        call set_error(error, line_no, column, "duplicate TOML key")
        return
      end if
      call append_table_entry(table, string_text(path(1)), value)
      return
    end if

    if (entry_index == 0) then
      child = make_table_value()
      call append_table_entry(table, string_text(path(1)), child)
      entry_index = table_entry_index(table, string_text(path(1)))
    else if (table%table_values(entry_index)%kind /= TOML_KIND_TABLE .or. &
             table%table_values(entry_index)%table_inline) then
      call set_error(error, line_no, column, "key path collides with scalar value")
      return
    end if

    call path_tail(path, remaining_path)
    call set_value_at_path(table%table_values(entry_index), remaining_path, value, line_no, column, error)
  end subroutine set_value_at_path

  subroutine set_value_in_context(root, context_path, key_path, value, line_no, column, error)
    type(toml_value), intent(inout) :: root
    type(toml_string), intent(in) :: context_path(:)
    type(toml_string), intent(in) :: key_path(:)
    type(toml_value), intent(in) :: value
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error

    if (size(context_path) == 0) then
      call set_value_at_path(root, key_path, value, line_no, column, error)
    else
      call set_value_in_context_at(root, context_path, key_path, value, line_no, column, error)
    end if
  end subroutine set_value_in_context

  recursive subroutine set_value_in_context_at(container, context_path, key_path, value, line_no, column, error)
    type(toml_value), intent(inout) :: container
    type(toml_string), intent(in) :: context_path(:)
    type(toml_string), intent(in) :: key_path(:)
    type(toml_value), intent(in) :: value
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error
    integer :: entry_index
    integer :: last_index
    type(toml_string), allocatable :: remaining_path(:)

    if (container%kind /= TOML_KIND_TABLE) then
      call set_error(error, line_no, column, "current table path collides with scalar value")
      return
    end if
    if (size(context_path) == 0) then
      call set_value_at_path(container, key_path, value, line_no, column, error)
      return
    end if

    entry_index = table_entry_index(container, string_text(context_path(1)))
    if (entry_index == 0) then
      call set_error(error, line_no, column, "current table path does not exist")
      return
    end if

    if (size(context_path) == 1) then
      select case (container%table_values(entry_index)%kind)
      case (TOML_KIND_TABLE)
        call set_value_at_path(container%table_values(entry_index), key_path, value, line_no, column, error)
      case (TOML_KIND_ARRAY)
        if (.not. allocated(container%table_values(entry_index)%array_values)) then
          call set_error(error, line_no, column, "array-of-tables path has no entries")
          return
        end if
        last_index = size(container%table_values(entry_index)%array_values)
        if (last_index == 0 .or. container%table_values(entry_index)%array_values(last_index)%kind /= TOML_KIND_TABLE) then
          call set_error(error, line_no, column, "array-of-tables path has no table entry")
          return
        end if
        call set_value_at_path(container%table_values(entry_index)%array_values(last_index), key_path, value, &
                               line_no, column, error)
      case default
        call set_error(error, line_no, column, "current table path collides with scalar value")
      end select
      return
    end if

    call path_tail(context_path, remaining_path)
    select case (container%table_values(entry_index)%kind)
    case (TOML_KIND_TABLE)
      call set_value_in_context_at(container%table_values(entry_index), remaining_path, key_path, value, &
                                   line_no, column, error)
    case (TOML_KIND_ARRAY)
      last_index = size(container%table_values(entry_index)%array_values)
      if (last_index == 0 .or. container%table_values(entry_index)%array_values(last_index)%kind /= TOML_KIND_TABLE) then
        call set_error(error, line_no, column, "array-of-tables path has no table entry")
        return
      end if
      call set_value_in_context_at(container%table_values(entry_index)%array_values(last_index), remaining_path, &
                                   key_path, value, line_no, column, error)
    case default
      call set_error(error, line_no, column, "current table path collides with scalar value")
    end select
  end subroutine set_value_in_context_at

  recursive subroutine append_array_table_path(container, path, line_no, column, error)
    type(toml_value), intent(inout) :: container
    type(toml_string), intent(in) :: path(:)
    integer, intent(in) :: line_no
    integer, intent(in) :: column
    type(toml_error), intent(inout) :: error
    integer :: entry_index
    integer :: last_index
    type(toml_value) :: child
    type(toml_string), allocatable :: remaining_path(:)

    if (container%kind /= TOML_KIND_TABLE) then
      call set_error(error, line_no, column, "array-of-tables path collides with scalar value")
      return
    end if
    if (size(path) == 0) then
      call set_error(error, line_no, column, "empty array-of-tables path")
      return
    end if

    entry_index = table_entry_index(container, string_text(path(1)))
    if (size(path) == 1) then
      if (entry_index == 0) then
        child = make_empty_array_value()
        call append_table_entry(container, string_text(path(1)), child)
        entry_index = table_entry_index(container, string_text(path(1)))
      else if (container%table_values(entry_index)%kind /= TOML_KIND_ARRAY) then
        call set_error(error, line_no, column, "array-of-tables path collides with non-array value")
        return
      end if
      call append_array_value(container%table_values(entry_index), make_table_value())
      return
    end if

    call path_tail(path, remaining_path)
    if (entry_index == 0) then
      child = make_table_value()
      call append_table_entry(container, string_text(path(1)), child)
      entry_index = table_entry_index(container, string_text(path(1)))
    end if

    select case (container%table_values(entry_index)%kind)
    case (TOML_KIND_TABLE)
      call append_array_table_path(container%table_values(entry_index), remaining_path, line_no, column, error)
    case (TOML_KIND_ARRAY)
      last_index = size(container%table_values(entry_index)%array_values)
      if (last_index == 0 .or. container%table_values(entry_index)%array_values(last_index)%kind /= TOML_KIND_TABLE) then
        call set_error(error, line_no, column, "array-of-tables path has no parent table entry")
        return
      end if
      call append_array_table_path(container%table_values(entry_index)%array_values(last_index), remaining_path, &
                                   line_no, column, error)
    case default
      call set_error(error, line_no, column, "array-of-tables path collides with scalar value")
    end select
  end subroutine append_array_table_path

  recursive subroutine lookup_value(table, path, found, value)
    type(toml_value), intent(in) :: table
    type(toml_string), intent(in) :: path(:)
    logical, intent(out) :: found
    type(toml_value), intent(out) :: value
    integer :: entry_index
    type(toml_string), allocatable :: remaining_path(:)

    found = .false.
    value = make_none_value()
    if (size(path) == 0 .or. table%kind /= TOML_KIND_TABLE) return

    entry_index = table_entry_index(table, string_text(path(1)))
    if (entry_index == 0) return

    if (size(path) == 1) then
      found = .true.
      call copy_toml_value(table%table_values(entry_index), value)
    else
      call path_tail(path, remaining_path)
      call lookup_value(table%table_values(entry_index), remaining_path, found, value)
    end if
  end subroutine lookup_value

  subroutine path_tail(path, tail)
    type(toml_string), intent(in) :: path(:)
    type(toml_string), allocatable, intent(out) :: tail(:)

    if (size(path) <= 1) then
      allocate(tail(0))
    else
      allocate(tail(size(path) - 1))
      call copy_string_items(path(2:size(path)), tail)
    end if
  end subroutine path_tail

  logical function document_has_key(this, path) result(found)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    type(toml_error) :: error
    type(toml_string), allocatable :: parts(:)
    type(toml_value) :: value

    call clear_toml_error(error)
    call parse_key_path(path, parts, error, 0, 0)
    if (error%failed) then
      found = .false.
      return
    end if
    call lookup_value(this%root, parts, found, value)
  end function document_has_key

  function document_get_string(this, path, default) result(text)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    character(len=*), intent(in), optional :: default
    character(len=:), allocatable :: text
    type(toml_value) :: value
    logical :: found

    call document_lookup(this, path, found, value)
    if (found .and. (value%kind == TOML_KIND_STRING .or. value%kind == TOML_KIND_DATETIME)) then
      text = string_text(value%string_value)
    else if (present(default)) then
      text = default
    else
      text = ""
    end if
  end function document_get_string

  integer(int64) function document_get_integer(this, path, default) result(number)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    integer(int64), intent(in), optional :: default
    type(toml_value) :: value
    logical :: found

    call document_lookup(this, path, found, value)
    if (found .and. value%kind == TOML_KIND_INTEGER) then
      number = value%integer_value
    else if (present(default)) then
      number = default
    else
      number = 0_int64
    end if
  end function document_get_integer

  real(real64) function document_get_float(this, path, default) result(number)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    real(real64), intent(in), optional :: default
    type(toml_value) :: value
    logical :: found

    call document_lookup(this, path, found, value)
    if (found .and. value%kind == TOML_KIND_FLOAT) then
      number = value%float_value
    else if (present(default)) then
      number = default
    else
      number = 0.0_real64
    end if
  end function document_get_float

  logical function document_get_boolean(this, path, default) result(value_result)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    logical, intent(in), optional :: default
    type(toml_value) :: value
    logical :: found

    call document_lookup(this, path, found, value)
    if (found .and. value%kind == TOML_KIND_BOOLEAN) then
      value_result = value%boolean_value
    else if (present(default)) then
      value_result = default
    else
      value_result = .false.
    end if
  end function document_get_boolean

  function document_get_table(this, path) result(table)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    type(toml_table) :: table
    type(toml_value) :: value
    logical :: found

    call clear_table(table)
    call document_lookup(this, path, found, value)
    if (found .and. value%kind == TOML_KIND_TABLE) then
      call copy_string_items(value%table_keys, table%keys)
      call copy_value_items(value%table_values, table%values)
    end if
  end function document_get_table

  function document_get_array(this, path) result(array)
    class(toml_document), intent(in) :: this
    character(len=*), intent(in) :: path
    type(toml_array) :: array
    type(toml_value) :: value
    logical :: found

    call clear_array(array)
    call document_lookup(this, path, found, value)
    if (found .and. value%kind == TOML_KIND_ARRAY) call copy_value_items(value%array_values, array%values)
  end function document_get_array

  subroutine document_lookup(document, path, found, value)
    type(toml_document), intent(in) :: document
    character(len=*), intent(in) :: path
    logical, intent(out) :: found
    type(toml_value), intent(out) :: value
    type(toml_error) :: error
    type(toml_string), allocatable :: parts(:)

    call clear_toml_error(error)
    call parse_key_path(path, parts, error, 0, 0)
    if (error%failed) then
      found = .false.
      value = make_none_value()
      return
    end if
    call lookup_value(document%root, parts, found, value)
  end subroutine document_lookup

  integer function array_length(this) result(count)
    class(toml_array), intent(in) :: this

    count = 0
    if (allocated(this%values)) count = size(this%values)
  end function array_length

  integer function table_length(this) result(count)
    class(toml_table), intent(in) :: this

    count = 0
    if (allocated(this%keys)) count = size(this%keys)
  end function table_length

  function make_none_value() result(value)
    type(toml_value) :: value

    value%kind = TOML_KIND_NONE
  end function make_none_value

  function make_string_value(text) result(value)
    character(len=*), intent(in) :: text
    type(toml_value) :: value

    value%kind = TOML_KIND_STRING
    value%string_value%text = text
  end function make_string_value

  function make_datetime_value(text) result(value)
    character(len=*), intent(in) :: text
    type(toml_value) :: value

    value%kind = TOML_KIND_DATETIME
    value%string_value%text = text
  end function make_datetime_value

  function make_integer_value(number) result(value)
    integer(int64), intent(in) :: number
    type(toml_value) :: value

    value%kind = TOML_KIND_INTEGER
    value%integer_value = number
  end function make_integer_value

  function make_float_value(number) result(value)
    real(real64), intent(in) :: number
    type(toml_value) :: value

    value%kind = TOML_KIND_FLOAT
    value%float_value = number
  end function make_float_value

  function make_boolean_value(flag) result(value)
    logical, intent(in) :: flag
    type(toml_value) :: value

    value%kind = TOML_KIND_BOOLEAN
    value%boolean_value = flag
  end function make_boolean_value

  function make_array_value(values) result(value)
    type(toml_value), intent(in) :: values(:)
    type(toml_value) :: value

    value = make_none_value()
    value%kind = TOML_KIND_ARRAY
    call copy_value_items(values, value%array_values)
  end function make_array_value

  function make_empty_array_value() result(value)
    type(toml_value) :: value

    value = make_none_value()
    value%kind = TOML_KIND_ARRAY
    allocate(value%array_values(0))
  end function make_empty_array_value

  function make_table_value(explicit, inline) result(value)
    logical, intent(in), optional :: explicit
    logical, intent(in), optional :: inline
    type(toml_value) :: value

    value%kind = TOML_KIND_TABLE
    if (present(explicit)) value%table_explicit = explicit
    if (present(inline)) value%table_inline = inline
    allocate(value%table_keys(0))
    allocate(value%table_values(0))
  end function make_table_value

  subroutine clear_array(array)
    type(toml_array), intent(out) :: array

    allocate(array%values(0))
  end subroutine clear_array

  subroutine clear_table(table)
    type(toml_table), intent(out) :: table

    allocate(table%keys(0))
    allocate(table%values(0))
  end subroutine clear_table

  subroutine clear_toml_error(error)
    type(toml_error), intent(out) :: error

    error%failed = .false.
    error%line = 0
    error%column = 0
    error%message = ""
  end subroutine clear_toml_error

  subroutine set_error(error, line, column, message)
    type(toml_error), intent(inout) :: error
    integer, intent(in) :: line
    integer, intent(in) :: column
    character(len=*), intent(in) :: message

    if (error%failed) return
    error%failed = .true.
    error%line = line
    error%column = column
    error%message = message
  end subroutine set_error

  subroutine append_table_entry(table, key, value)
    type(toml_value), intent(inout) :: table
    character(len=*), intent(in) :: key
    type(toml_value), intent(in) :: value
    type(toml_string), allocatable :: next_keys(:)
    type(toml_value), allocatable :: next_values(:)
    integer :: count

    count = 0
    if (allocated(table%table_keys)) count = size(table%table_keys)
    allocate(next_keys(count + 1))
    allocate(next_values(count + 1))
    if (count > 0) call copy_existing_entries(table, next_keys, next_values, count)
    next_keys(count + 1)%text = key
    call copy_toml_value(value, next_values(count + 1))
    call move_alloc(next_keys, table%table_keys)
    call move_alloc(next_values, table%table_values)
  end subroutine append_table_entry

  subroutine append_array_value(array, value)
    type(toml_value), intent(inout) :: array
    type(toml_value), intent(in) :: value
    type(toml_value), allocatable :: next_values(:)
    integer :: count

    count = 0
    if (allocated(array%array_values)) count = size(array%array_values)
    allocate(next_values(count + 1))
    if (count > 0) call copy_value_items_into(array%array_values, next_values(:count))
    call copy_toml_value(value, next_values(count + 1))
    call move_alloc(next_values, array%array_values)
  end subroutine append_array_value

  subroutine copy_existing_entries(table, keys, values, count)
    type(toml_value), intent(in) :: table
    type(toml_string), intent(inout) :: keys(:)
    type(toml_value), intent(inout) :: values(:)
    integer, intent(in) :: count
    integer :: i

    do i = 1, count
      keys(i)%text = string_text(table%table_keys(i))
      call copy_toml_value(table%table_values(i), values(i))
    end do
  end subroutine copy_existing_entries

  recursive subroutine copy_toml_value(source, destination)
    type(toml_value), intent(in) :: source
    type(toml_value), intent(out) :: destination
    integer :: i

    destination%kind = source%kind
    destination%string_value%text = string_text(source%string_value)
    destination%integer_value = source%integer_value
    destination%float_value = source%float_value
    destination%boolean_value = source%boolean_value
    destination%table_explicit = source%table_explicit
    destination%table_inline = source%table_inline

    if (allocated(source%array_values)) then
      allocate(destination%array_values(size(source%array_values)))
      do i = 1, size(source%array_values)
        call copy_toml_value(source%array_values(i), destination%array_values(i))
      end do
    end if

    if (allocated(source%table_keys)) then
      allocate(destination%table_keys(size(source%table_keys)))
      allocate(destination%table_values(size(source%table_keys)))
      do i = 1, size(source%table_keys)
        destination%table_keys(i)%text = string_text(source%table_keys(i))
        call copy_toml_value(source%table_values(i), destination%table_values(i))
      end do
    end if
  end subroutine copy_toml_value

  subroutine copy_value_items(source, destination)
    type(toml_value), intent(in) :: source(:)
    type(toml_value), allocatable, intent(out) :: destination(:)
    integer :: i

    allocate(destination(size(source)))
    do i = 1, size(source)
      call copy_toml_value(source(i), destination(i))
    end do
  end subroutine copy_value_items

  subroutine copy_value_items_into(source, destination)
    type(toml_value), intent(in) :: source(:)
    type(toml_value), intent(inout) :: destination(:)
    integer :: i

    do i = 1, size(source)
      call copy_toml_value(source(i), destination(i))
    end do
  end subroutine copy_value_items_into

  subroutine copy_string_items(source, destination)
    type(toml_string), intent(in) :: source(:)
    type(toml_string), allocatable, intent(out) :: destination(:)
    integer :: i

    allocate(destination(size(source)))
    do i = 1, size(source)
      destination(i)%text = string_text(source(i))
    end do
  end subroutine copy_string_items

  subroutine copy_string_items_into(source, destination)
    type(toml_string), intent(in) :: source(:)
    type(toml_string), intent(inout) :: destination(:)
    integer :: i

    do i = 1, size(source)
      destination(i)%text = string_text(source(i))
    end do
  end subroutine copy_string_items_into

  integer function table_entry_index(table, key) result(index_value)
    type(toml_value), intent(in) :: table
    character(len=*), intent(in) :: key
    integer :: i

    index_value = 0
    if (.not. allocated(table%table_keys)) return
    do i = 1, size(table%table_keys)
      if (string_text(table%table_keys(i)) == key) then
        index_value = i
        return
      end if
    end do
  end function table_entry_index

  subroutine append_string(items, text)
    type(toml_string), allocatable, intent(inout) :: items(:)
    character(len=*), intent(in) :: text
    type(toml_string), allocatable :: next_items(:)
    integer :: count

    count = 0
    if (allocated(items)) count = size(items)
    allocate(next_items(count + 1))
    if (count > 0) call copy_string_items_into(items, next_items(:count))
    next_items(count + 1)%text = text
    call move_alloc(next_items, items)
  end subroutine append_string

  subroutine copy_path(source, destination)
    type(toml_string), intent(in) :: source(:)
    type(toml_string), allocatable, intent(out) :: destination(:)

    allocate(destination(size(source)))
    if (size(source) > 0) call copy_string_items_into(source, destination)
  end subroutine copy_path

  function string_text(value) result(text)
    type(toml_string), intent(in) :: value
    character(len=:), allocatable :: text

    if (allocated(value%text)) then
      text = value%text
    else
      text = ""
    end if
  end function string_text

  function strip_comment(line) result(stripped)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: stripped
    character(len=3) :: delimiter
    character(len=1) :: quote
    integer :: i
    logical :: escaped
    logical :: in_string
    logical :: multiline

    in_string = .false.
    escaped = .false.
    multiline = .false.
    quote = ""
    i = 1
    do while (i <= len(line))
      if (in_string) then
        if (multiline) then
          delimiter = repeat(quote, 3)
          if (substring_matches(line, i, delimiter) .and. &
              (quote /= '"' .or. .not. escaped_position(line, i))) then
            in_string = .false.
            multiline = .false.
            i = i + 3
          else
            i = i + 1
          end if
        else
          if (quote == '"' .and. line(i:i) == BACKSLASH .and. .not. escaped) then
            escaped = .true.
            i = i + 1
            cycle
          end if
          if (line(i:i) == quote .and. .not. escaped) in_string = .false.
          escaped = .false.
          i = i + 1
        end if
      else if (line(i:i) == '"' .or. line(i:i) == "'") then
        in_string = .true.
        quote = line(i:i)
        if (substring_matches(line, i, repeat(quote, 3))) then
          multiline = .true.
          i = i + 3
        else
          multiline = .false.
          i = i + 1
        end if
      else if (line(i:i) == "#") then
        stripped = line(:i - 1)
        return
      else
        i = i + 1
      end if
    end do
    stripped = line
  end function strip_comment

  integer function find_top_level_char(text, needle) result(position)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: needle

    position = find_next_top_level_separator(text, needle, 1)
  end function find_top_level_char

  integer function find_next_top_level_separator(text, separator, start) result(position)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: separator
    integer, intent(in) :: start
    character(len=1) :: quote
    integer :: brace_depth
    integer :: bracket_depth
    integer :: i
    logical :: escaped
    logical :: in_string

    position = 0
    in_string = .false.
    escaped = .false.
    quote = ""
    bracket_depth = 0
    brace_depth = 0

    do i = start, len(text)
      if (in_string) then
        if (quote == '"' .and. text(i:i) == BACKSLASH .and. .not. escaped) then
          escaped = .true.
          cycle
        end if
        if (text(i:i) == quote .and. .not. escaped) in_string = .false.
        escaped = .false.
        cycle
      end if

      select case (text(i:i))
      case ('"', "'")
        in_string = .true.
        quote = text(i:i)
      case ("[")
        bracket_depth = bracket_depth + 1
      case ("]")
        bracket_depth = max(0, bracket_depth - 1)
      case ("{")
        brace_depth = brace_depth + 1
      case ("}")
        brace_depth = max(0, brace_depth - 1)
      case default
        if (bracket_depth == 0 .and. brace_depth == 0) then
          if (i + len(separator) - 1 <= len(text)) then
            if (text(i:i + len(separator) - 1) == separator) then
              position = i
              return
            end if
          end if
        end if
      end select
    end do
  end function find_next_top_level_separator

  integer function matching_end(text, start, open_char, close_char) result(position)
    character(len=*), intent(in) :: text
    integer, intent(in) :: start
    character(len=*), intent(in) :: open_char
    character(len=*), intent(in) :: close_char
    character(len=1) :: quote
    integer :: depth
    integer :: i
    logical :: escaped
    logical :: in_string

    position = 0
    depth = 0
    in_string = .false.
    escaped = .false.
    quote = ""
    do i = start, len(text)
      if (in_string) then
        if (quote == '"' .and. text(i:i) == BACKSLASH .and. .not. escaped) then
          escaped = .true.
          cycle
        end if
        if (text(i:i) == quote .and. .not. escaped) in_string = .false.
        escaped = .false.
      else if (text(i:i) == '"' .or. text(i:i) == "'") then
        in_string = .true.
        quote = text(i:i)
      else if (text(i:i) == open_char) then
        depth = depth + 1
      else if (text(i:i) == close_char) then
        depth = depth - 1
        if (depth == 0) then
          position = i
          return
        end if
      end if
    end do
  end function matching_end

  subroutine skip_spaces(text, pos)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos

    do while (pos <= len(text))
      if (text(pos:pos) /= " " .and. text(pos:pos) /= char(9)) return
      pos = pos + 1
    end do
  end subroutine skip_spaces

  logical function bare_key_char(ch) result(valid)
    character(len=*), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    valid = (code >= iachar("a") .and. code <= iachar("z")) .or. &
            (code >= iachar("A") .and. code <= iachar("Z")) .or. &
            (code >= iachar("0") .and. code <= iachar("9")) .or. &
            ch == "_" .or. ch == "-"
  end function bare_key_char

  logical function starts_with(text, prefix) result(matches)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: prefix

    matches = len(text) >= len(prefix)
    if (matches) matches = text(:len(prefix)) == prefix
  end function starts_with

  logical function substring_matches(text, position, needle) result(matches)
    character(len=*), intent(in) :: text
    integer, intent(in) :: position
    character(len=*), intent(in) :: needle

    matches = position >= 1 .and. position + len(needle) - 1 <= len(text)
    if (matches) matches = text(position:position + len(needle) - 1) == needle
  end function substring_matches

  function trim_carriage_return(text) result(trimmed)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: trimmed
    integer :: length

    length = len(text)
    if (length > 0) then
      if (text(length:length) == char(13)) then
        trimmed = text(:length - 1)
        return
      end if
    end if
    trimmed = text
  end function trim_carriage_return

  function trim_toml_whitespace(text) result(trimmed)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: trimmed
    integer :: first
    integer :: last

    first = 1
    do while (first <= len(text))
      if (.not. toml_whitespace(text(first:first))) exit
      first = first + 1
    end do

    last = len(text)
    do while (last >= first)
      if (.not. toml_whitespace(text(last:last))) exit
      last = last - 1
    end do

    if (last < first) then
      trimmed = ""
    else
      trimmed = text(first:last)
    end if
  end function trim_toml_whitespace

  logical function toml_whitespace(ch) result(matches)
    character(len=*), intent(in) :: ch

    matches = ch == " " .or. ch == char(9)
  end function toml_whitespace

  function remove_underscores(text) result(clean)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: clean
    integer :: i

    clean = ""
    do i = 1, len_trim(text)
      if (text(i:i) /= "_") clean = clean // text(i:i)
    end do
  end function remove_underscores

  logical function invalid_underscore_placement(text) result(invalid)
    character(len=*), intent(in) :: text
    integer :: i
    integer :: length

    invalid = .false.
    length = len_trim(text)
    do i = 1, length
      if (text(i:i) == "_") then
        if (i == 1 .or. i == length) then
          invalid = .true.
          return
        end if
        if (has_integer_prefix(text)) then
          invalid = integer_digit(text(i - 1:i - 1)) < 0 .or. integer_digit(text(i + 1:i + 1)) < 0
        else
          invalid = .not. decimal_digit(text(i - 1:i - 1)) .or. .not. decimal_digit(text(i + 1:i + 1))
        end if
        if (invalid) then
          return
        end if
      end if
    end do
  end function invalid_underscore_placement

  logical function is_decimal_integer_literal(text) result(valid)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: digits
    integer :: digit_count
    integer :: pos

    valid = .false.
    pos = 1
    if (len_trim(text) == 0) return
    if (text(pos:pos) == "+" .or. text(pos:pos) == "-") pos = pos + 1
    if (pos > len_trim(text)) return

    digits = ""
    digit_count = 0
    do while (pos <= len_trim(text))
      if (text(pos:pos) == "_") then
        pos = pos + 1
      else if (decimal_digit(text(pos:pos))) then
        digits = digits // text(pos:pos)
        digit_count = digit_count + 1
        pos = pos + 1
      else
        return
      end if
    end do

    valid = digit_count > 0 .and. valid_unsigned_decimal_digits(digits)
  end function is_decimal_integer_literal

  logical function is_float_literal(text) result(valid)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: integer_part
    integer :: digit_count
    integer :: pos
    logical :: saw_dot
    logical :: saw_exponent

    valid = .false.
    pos = 1
    if (len_trim(text) == 0) return
    if (text(pos:pos) == "+" .or. text(pos:pos) == "-") pos = pos + 1
    if (pos > len_trim(text)) return

    integer_part = ""
    digit_count = 0
    do while (pos <= len_trim(text))
      if (text(pos:pos) == "_") then
        pos = pos + 1
      else if (decimal_digit(text(pos:pos))) then
        integer_part = integer_part // text(pos:pos)
        digit_count = digit_count + 1
        pos = pos + 1
      else
        exit
      end if
    end do
    if (digit_count == 0 .or. .not. valid_unsigned_decimal_digits(integer_part)) return

    saw_dot = .false.
    if (pos <= len_trim(text)) then
      if (text(pos:pos) == ".") then
        saw_dot = .true.
        pos = pos + 1
        digit_count = 0
        do while (pos <= len_trim(text))
          if (text(pos:pos) == "_") then
            pos = pos + 1
          else if (decimal_digit(text(pos:pos))) then
            digit_count = digit_count + 1
            pos = pos + 1
          else
            exit
          end if
        end do
        if (digit_count == 0) return
      end if
    end if

    saw_exponent = .false.
    if (pos <= len_trim(text)) then
      if (text(pos:pos) == "e" .or. text(pos:pos) == "E") then
        saw_exponent = .true.
        pos = pos + 1
        if (pos <= len_trim(text)) then
          if (text(pos:pos) == "+" .or. text(pos:pos) == "-") pos = pos + 1
        end if
        digit_count = 0
        do while (pos <= len_trim(text))
          if (text(pos:pos) == "_") then
            pos = pos + 1
          else if (decimal_digit(text(pos:pos))) then
            digit_count = digit_count + 1
            pos = pos + 1
          else
            return
          end if
        end do
        if (digit_count == 0) return
      end if
    end if

    valid = pos > len_trim(text) .and. (saw_dot .or. saw_exponent)
  end function is_float_literal

  logical function valid_unsigned_decimal_digits(digits) result(valid)
    character(len=*), intent(in) :: digits

    valid = len(digits) > 0
    if (.not. valid) return
    valid = len(digits) == 1
    if (.not. valid) valid = digits(1:1) /= "0"
  end function valid_unsigned_decimal_digits

  logical function is_datetime_literal(text) result(valid)
    character(len=*), intent(in) :: text
    integer :: pos

    valid = .false.
    pos = 1
    if (parse_date_prefix(text, pos)) then
      if (pos > len_trim(text)) then
        valid = .true.
        return
      end if
      if (text(pos:pos) /= "T" .and. text(pos:pos) /= "t" .and. text(pos:pos) /= " ") return
      pos = pos + 1
      if (.not. parse_time_prefix(text, pos)) return
      valid = parse_timezone_suffix(text, pos)
      return
    end if

    pos = 1
    if (parse_time_prefix(text, pos)) valid = pos > len_trim(text)
  end function is_datetime_literal

  logical function parse_date_prefix(text, pos) result(valid)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    integer :: day
    integer :: month
    integer :: start
    integer :: year

    valid = .false.
    start = pos
    if (pos + 9 > len_trim(text)) return
    if (.not. digits_at(text, pos, 4)) return
    year = integer_from_digits(text(pos:pos + 3))
    pos = pos + 4
    if (text(pos:pos) /= "-") then
      pos = start
      return
    end if
    pos = pos + 1
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    month = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    if (text(pos:pos) /= "-") then
      pos = start
      return
    end if
    pos = pos + 1
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    day = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    valid = valid_date_parts(year, month, day)
    if (.not. valid) pos = start
  end function parse_date_prefix

  logical function parse_time_prefix(text, pos) result(valid)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    integer :: hour
    integer :: minute
    integer :: second
    integer :: start

    valid = .false.
    start = pos
    if (pos + 7 > len_trim(text)) return
    if (.not. digits_at(text, pos, 2)) return
    hour = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    if (text(pos:pos) /= ":") then
      pos = start
      return
    end if
    pos = pos + 1
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    minute = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    if (text(pos:pos) /= ":") then
      pos = start
      return
    end if
    pos = pos + 1
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    second = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    if (.not. valid_time_parts(hour, minute, second)) then
      pos = start
      return
    end if

    if (pos <= len_trim(text)) then
      if (text(pos:pos) /= ".") then
        valid = .true.
        return
      end if
      pos = pos + 1
      if (pos > len_trim(text)) then
        pos = start
        return
      end if
      if (.not. decimal_digit(text(pos:pos))) then
        pos = start
        return
      end if
      do while (pos <= len_trim(text))
        if (.not. decimal_digit(text(pos:pos))) exit
        pos = pos + 1
      end do
    end if
    valid = .true.
  end function parse_time_prefix

  logical function parse_timezone_suffix(text, pos) result(valid)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    integer :: hour
    integer :: minute
    integer :: start

    valid = .false.
    if (pos > len_trim(text)) then
      valid = .true.
      return
    end if
    if (text(pos:pos) == "Z" .or. text(pos:pos) == "z") then
      pos = pos + 1
      valid = pos > len_trim(text)
      return
    end if
    if (text(pos:pos) /= "+" .and. text(pos:pos) /= "-") return

    start = pos
    pos = pos + 1
    if (pos + 4 > len_trim(text)) then
      pos = start
      return
    end if
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    hour = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    if (text(pos:pos) /= ":") then
      pos = start
      return
    end if
    pos = pos + 1
    if (.not. digits_at(text, pos, 2)) then
      pos = start
      return
    end if
    minute = integer_from_digits(text(pos:pos + 1))
    pos = pos + 2
    valid = hour >= 0 .and. hour <= 23 .and. minute >= 0 .and. minute <= 59 .and. pos > len_trim(text)
    if (.not. valid) pos = start
  end function parse_timezone_suffix

  logical function valid_date_parts(year, month, day) result(valid)
    integer, intent(in) :: year
    integer, intent(in) :: month
    integer, intent(in) :: day
    integer :: month_days(12)

    month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    valid = month >= 1 .and. month <= 12
    if (.not. valid) return
    if (month == 2 .and. leap_year(year)) month_days(2) = 29
    valid = day >= 1 .and. day <= month_days(month)
  end function valid_date_parts

  logical function valid_time_parts(hour, minute, second) result(valid)
    integer, intent(in) :: hour
    integer, intent(in) :: minute
    integer, intent(in) :: second

    valid = hour >= 0 .and. hour <= 23 .and. minute >= 0 .and. minute <= 59 .and. &
            second >= 0 .and. second <= 60
  end function valid_time_parts

  logical function leap_year(year) result(leap)
    integer, intent(in) :: year

    leap = modulo(year, 4) == 0 .and. (modulo(year, 100) /= 0 .or. modulo(year, 400) == 0)
  end function leap_year

  logical function digits_at(text, pos, count) result(valid)
    character(len=*), intent(in) :: text
    integer, intent(in) :: pos
    integer, intent(in) :: count
    integer :: i

    valid = pos >= 1 .and. pos + count - 1 <= len_trim(text)
    if (.not. valid) return
    do i = pos, pos + count - 1
      if (.not. decimal_digit(text(i:i))) then
        valid = .false.
        return
      end if
    end do
  end function digits_at

  integer function integer_from_digits(text) result(value)
    character(len=*), intent(in) :: text
    integer :: i

    value = 0
    do i = 1, len(text)
      value = value * 10 + iachar(text(i:i)) - iachar("0")
    end do
  end function integer_from_digits

  logical function decimal_digit(ch) result(valid)
    character(len=*), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    valid = code >= iachar("0") .and. code <= iachar("9")
  end function decimal_digit

  logical function has_integer_prefix(text) result(prefixed)
    character(len=*), intent(in) :: text

    prefixed = .false.
    if (len_trim(text) < 2) return
    if (text(1:1) /= "0") return
    prefixed = any([text(2:2) == "x", text(2:2) == "X", &
                    text(2:2) == "o", text(2:2) == "O", &
                    text(2:2) == "b", text(2:2) == "B"])
  end function has_integer_prefix

  logical function is_prefixed_integer_literal(text) result(prefixed)
    character(len=*), intent(in) :: text
    integer :: base
    integer :: digit
    integer :: pos

    prefixed = .false.
    if (.not. has_integer_prefix(text) .or. len_trim(text) < 3) return
    select case (text(2:2))
    case ("x", "X")
      base = 16
    case ("o", "O")
      base = 8
    case ("b", "B")
      base = 2
    case default
      return
    end select

    do pos = 3, len_trim(text)
      if (text(pos:pos) == "_") cycle
      digit = integer_digit(text(pos:pos))
      if (digit < 0 .or. digit >= base) return
    end do
    prefixed = .true.
  end function is_prefixed_integer_literal

  logical function looks_like_number_or_datetime(text) result(matches)
    character(len=*), intent(in) :: text

    matches = .false.
    if (len_trim(text) == 0) return
    matches = decimal_digit(text(1:1)) .or. text(1:1) == "+" .or. text(1:1) == "-" .or. &
              index(text, ".") /= 0 .or. index(text, ":") /= 0 .or. &
              index(text, "-") /= 0 .or. index(text, "e") /= 0 .or. index(text, "E") /= 0
  end function looks_like_number_or_datetime

  subroutine parse_prefixed_integer(text, value, status)
    character(len=*), intent(in) :: text
    integer(int64), intent(out) :: value
    integer, intent(out) :: status
    integer :: base
    integer :: digit
    integer :: offset
    integer :: pos
    integer :: sign

    value = 0_int64
    status = 0
    sign = 1
    offset = 1
    if (text(1:1) == "-") then
      sign = -1
      offset = 2
    else if (text(1:1) == "+") then
      offset = 2
    end if

    select case (text(offset + 1:offset + 1))
    case ("x", "X")
      base = 16
    case ("o", "O")
      base = 8
    case ("b", "B")
      base = 2
    case default
      status = 1
      return
    end select

    do pos = offset + 2, len_trim(text)
      digit = integer_digit(text(pos:pos))
      if (digit < 0 .or. digit >= base) then
        status = 1
        return
      end if
      value = value * base + digit
    end do
    value = value * sign
  end subroutine parse_prefixed_integer

  integer function integer_digit(ch) result(digit)
    character(len=*), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    if (code >= iachar("0") .and. code <= iachar("9")) then
      digit = code - iachar("0")
    else if (code >= iachar("a") .and. code <= iachar("f")) then
      digit = 10 + code - iachar("a")
    else if (code >= iachar("A") .and. code <= iachar("F")) then
      digit = 10 + code - iachar("A")
    else
      digit = -1
    end if
  end function integer_digit

  real(real64) function ieee_like_nan() result(value)
    value = ieee_value(0.0_real64, ieee_quiet_nan)
  end function ieee_like_nan

end module fgof_toml
