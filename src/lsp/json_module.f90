module json_module
    ! Minimal JSON parser/builder for LSP communication
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private

    public :: json_value_t, json_object_t, json_array_t
    public :: json_parse, json_stringify
    public :: json_create_object, json_create_array
    public :: json_add_string, json_add_number, json_add_bool, json_add_null
    public :: json_add_object, json_add_array
    public :: json_get_string, json_get_number, json_get_bool
    public :: json_get_object, json_get_array
    public :: json_has_key
    public :: JSON_NULL, JSON_BOOL, JSON_NUMBER, JSON_STRING, JSON_ARRAY, JSON_OBJECT

    ! JSON value types
    integer, parameter :: JSON_NULL = 0
    integer, parameter :: JSON_BOOL = 1
    integer, parameter :: JSON_NUMBER = 2
    integer, parameter :: JSON_STRING = 3
    integer, parameter :: JSON_ARRAY = 4
    integer, parameter :: JSON_OBJECT = 5

    ! Forward declarations for recursive types
    type :: json_value_t
        integer :: value_type = JSON_NULL
        logical :: bool_value = .false.
        real(real64) :: number_value = 0.0
        character(len=:), allocatable :: string_value
        type(json_array_t), pointer :: array_value => null()
        type(json_object_t), pointer :: object_value => null()
    end type json_value_t

    type :: json_pair_t
        character(len=:), allocatable :: key
        type(json_value_t) :: value
    end type json_pair_t

    type :: json_object_t
        type(json_pair_t), allocatable :: pairs(:)
        integer :: count = 0
    end type json_object_t

    type :: json_array_t
        type(json_value_t), allocatable :: elements(:)
        integer :: count = 0
    end type json_array_t

contains

    function json_create_object() result(obj)
        type(json_value_t) :: obj

        obj%value_type = JSON_OBJECT
        allocate(obj%object_value)
        allocate(obj%object_value%pairs(0))
        obj%object_value%count = 0
    end function json_create_object

    function json_create_array() result(arr)
        type(json_value_t) :: arr

        arr%value_type = JSON_ARRAY
        allocate(arr%array_value)
        allocate(arr%array_value%elements(0))
        arr%array_value%count = 0
    end function json_create_array

    subroutine json_add_string(obj, key, value)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key, value
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value%value_type = JSON_STRING
        new_pairs(n + 1)%value%string_value = value

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_string

    subroutine json_add_number(obj, key, value)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value%value_type = JSON_NUMBER
        new_pairs(n + 1)%value%number_value = value

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_number

    subroutine json_add_bool(obj, key, value)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value%value_type = JSON_BOOL
        new_pairs(n + 1)%value%bool_value = value

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_bool

    subroutine json_add_null(obj, key)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value%value_type = JSON_NULL

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_null

    subroutine json_add_object(obj, key, child)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key
        type(json_value_t), intent(in) :: child
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return
        if (child%value_type /= JSON_OBJECT) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value = child

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_object

    subroutine json_add_array(obj, key, arr)
        type(json_value_t), intent(inout) :: obj
        character(len=*), intent(in) :: key
        type(json_value_t), intent(in) :: arr
        type(json_pair_t), allocatable :: new_pairs(:)
        integer :: n

        if (obj%value_type /= JSON_OBJECT) return
        if (arr%value_type /= JSON_ARRAY) return

        n = obj%object_value%count
        allocate(new_pairs(n + 1))

        if (n > 0) new_pairs(1:n) = obj%object_value%pairs

        new_pairs(n + 1)%key = key
        new_pairs(n + 1)%value = arr

        deallocate(obj%object_value%pairs)
        obj%object_value%pairs = new_pairs
        obj%object_value%count = n + 1
    end subroutine json_add_array

    function json_stringify(value) result(str)
        type(json_value_t), intent(in) :: value
        character(len=:), allocatable :: str

        select case(value%value_type)
        case(JSON_NULL)
            str = "null"
        case(JSON_BOOL)
            if (value%bool_value) then
                str = "true"
            else
                str = "false"
            end if
        case(JSON_NUMBER)
            str = number_to_string(value%number_value)
        case(JSON_STRING)
            str = '"' // escape_string(value%string_value) // '"'
        case(JSON_OBJECT)
            str = object_to_string(value%object_value)
        case(JSON_ARRAY)
            str = array_to_string(value%array_value)
        end select
    end function json_stringify

    function object_to_string(obj) result(str)
        type(json_object_t), pointer, intent(in) :: obj
        character(len=:), allocatable :: str
        integer :: i

        if (.not. associated(obj)) then
            str = "null"
            return
        end if

        str = "{"
        do i = 1, obj%count
            if (i > 1) str = str // ","
            str = str // '"' // obj%pairs(i)%key // '":' // &
                  json_stringify(obj%pairs(i)%value)
        end do
        str = str // "}"
    end function object_to_string

    function array_to_string(arr) result(str)
        type(json_array_t), pointer, intent(in) :: arr
        character(len=:), allocatable :: str
        integer :: i

        if (.not. associated(arr)) then
            str = "null"
            return
        end if

        str = "["
        do i = 1, arr%count
            if (i > 1) str = str // ","
            str = str // json_stringify(arr%elements(i))
        end do
        str = str // "]"
    end function array_to_string

    function escape_string(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i
        character :: ch

        escaped = ""
        do i = 1, len(str)
            ch = str(i:i)
            select case(ch)
            case('"')
                escaped = escaped // '\"'
            case('\')
                escaped = escaped // '\\'
            case(char(8))  ! backspace
                escaped = escaped // '\b'
            case(char(12)) ! form feed
                escaped = escaped // '\f'
            case(char(10)) ! newline
                escaped = escaped // '\n'
            case(char(13)) ! carriage return
                escaped = escaped // '\r'
            case(char(9))  ! tab
                escaped = escaped // '\t'
            case default
                escaped = escaped // ch
            end select
        end do
    end function escape_string

    function number_to_string(num) result(str)
        real(real64), intent(in) :: num
        character(len=:), allocatable :: str
        character(len=32) :: buffer

        if (num == int(num)) then
            write(buffer, '(i0)') int(num)
        else
            write(buffer, '(f0.6)') num
        end if
        str = trim(buffer)
    end function number_to_string

    function json_has_key(obj, key) result(has)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        logical :: has
        integer :: i

        has = .false.
        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                has = .true.
                return
            end if
        end do
    end function json_has_key

    function json_get_string(obj, key, default) result(value)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: value
        integer :: i

        if (present(default)) then
            value = default
        else
            value = ""
        end if

        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                if (obj%object_value%pairs(i)%value%value_type == JSON_STRING) then
                    value = obj%object_value%pairs(i)%value%string_value
                end if
                return
            end if
        end do
    end function json_get_string

    function json_get_number(obj, key, default) result(value)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        real(real64), intent(in), optional :: default
        real(real64) :: value
        integer :: i

        if (present(default)) then
            value = default
        else
            value = 0.0
        end if

        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                if (obj%object_value%pairs(i)%value%value_type == JSON_NUMBER) then
                    value = obj%object_value%pairs(i)%value%number_value
                end if
                return
            end if
        end do
    end function json_get_number

    function json_get_bool(obj, key, default) result(value)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        logical, intent(in), optional :: default
        logical :: value
        integer :: i

        if (present(default)) then
            value = default
        else
            value = .false.
        end if

        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                if (obj%object_value%pairs(i)%value%value_type == JSON_BOOL) then
                    value = obj%object_value%pairs(i)%value%bool_value
                end if
                return
            end if
        end do
    end function json_get_bool

    function json_get_object(obj, key) result(value)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        type(json_value_t) :: value
        integer :: i

        value%value_type = JSON_NULL

        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                if (obj%object_value%pairs(i)%value%value_type == JSON_OBJECT) then
                    value = obj%object_value%pairs(i)%value
                end if
                return
            end if
        end do
    end function json_get_object

    function json_get_array(obj, key) result(value)
        type(json_value_t), intent(in) :: obj
        character(len=*), intent(in) :: key
        type(json_value_t) :: value
        integer :: i

        value%value_type = JSON_NULL

        if (obj%value_type /= JSON_OBJECT) return
        if (.not. associated(obj%object_value)) return

        do i = 1, obj%object_value%count
            if (obj%object_value%pairs(i)%key == key) then
                if (obj%object_value%pairs(i)%value%value_type == JSON_ARRAY) then
                    value = obj%object_value%pairs(i)%value
                end if
                return
            end if
        end do
    end function json_get_array

    ! Simple JSON parser (basic implementation)
    function json_parse(str) result(value)
        character(len=*), intent(in) :: str
        type(json_value_t) :: value
        integer :: pos

        pos = 1
        call skip_whitespace(str, pos)
        value = parse_value(str, pos)
    end function json_parse

    recursive function parse_value(str, pos) result(value)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: value

        call skip_whitespace(str, pos)

        if (pos > len(str)) then
            value%value_type = JSON_NULL
            return
        end if

        select case(str(pos:pos))
        case('{')
            value = parse_object(str, pos)
        case('[')
            value = parse_array(str, pos)
        case('"')
            value = parse_string(str, pos)
        case('t', 'f')  ! true or false
            value = parse_bool(str, pos)
        case('n')  ! null
            value = parse_null(str, pos)
        case default
            ! Try to parse as number
            value = parse_number(str, pos)
        end select
    end function parse_value

    function parse_object(str, pos) result(obj)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: obj

        obj = json_create_object()
        pos = pos + 1  ! skip '{'

        ! TODO: Implement object parsing
        ! This is a simplified version

        ! Find closing '}'
        do while (pos <= len(str) .and. str(pos:pos) /= '}')
            pos = pos + 1
        end do
        if (pos <= len(str)) pos = pos + 1  ! skip '}'
    end function parse_object

    function parse_array(str, pos) result(arr)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: arr

        arr = json_create_array()
        pos = pos + 1  ! skip '['

        ! TODO: Implement array parsing

        ! Find closing ']'
        do while (pos <= len(str) .and. str(pos:pos) /= ']')
            pos = pos + 1
        end do
        if (pos <= len(str)) pos = pos + 1  ! skip ']'
    end function parse_array

    function parse_string(str, pos) result(value)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: value
        integer :: start_pos

        value%value_type = JSON_STRING
        pos = pos + 1  ! skip opening '"'
        start_pos = pos

        ! Find closing '"' (ignoring escaped quotes)
        do while (pos <= len(str))
            if (str(pos:pos) == '\' .and. pos < len(str)) then
                pos = pos + 2  ! skip escaped character
            else if (str(pos:pos) == '"') then
                value%string_value = str(start_pos:pos-1)
                pos = pos + 1  ! skip closing '"'
                return
            else
                pos = pos + 1
            end if
        end do

        value%string_value = str(start_pos:)
    end function parse_string

    function parse_number(str, pos) result(value)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: value
        integer :: start_pos
        character :: ch

        value%value_type = JSON_NUMBER
        start_pos = pos

        ! Parse sign
        if (pos <= len(str)) then
            ch = str(pos:pos)
            if (ch == '-' .or. ch == '+') pos = pos + 1
        end if

        ! Parse digits
        do while (pos <= len(str))
            ch = str(pos:pos)
            if ((ch >= '0' .and. ch <= '9') .or. ch == '.' .or. &
                ch == 'e' .or. ch == 'E' .or. ch == '+' .or. ch == '-') then
                pos = pos + 1
            else
                exit
            end if
        end do

        read(str(start_pos:pos-1), *) value%number_value
    end function parse_number

    function parse_bool(str, pos) result(value)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: value

        value%value_type = JSON_BOOL

        if (str(pos:min(pos+3, len(str))) == 'true') then
            value%bool_value = .true.
            pos = pos + 4
        else if (str(pos:min(pos+4, len(str))) == 'false') then
            value%bool_value = .false.
            pos = pos + 5
        end if
    end function parse_bool

    function parse_null(str, pos) result(value)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos
        type(json_value_t) :: value

        value%value_type = JSON_NULL
        if (str(pos:min(pos+3, len(str))) == 'null') then
            pos = pos + 4
        end if
    end function parse_null

    subroutine skip_whitespace(str, pos)
        character(len=*), intent(in) :: str
        integer, intent(inout) :: pos

        do while (pos <= len(str))
            select case(str(pos:pos))
            case(' ', char(9), char(10), char(13))
                pos = pos + 1
            case default
                return
            end select
        end do
    end subroutine skip_whitespace

end module json_module