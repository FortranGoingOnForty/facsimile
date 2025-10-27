module bracket_matching_module
    use text_buffer_module
    implicit none
    private

    public :: find_matching_bracket, is_opening_bracket, is_closing_bracket
    public :: get_bracket_pair

contains

    logical function is_opening_bracket(ch)
        character(len=1), intent(in) :: ch
        is_opening_bracket = (ch == '(' .or. ch == '[' .or. ch == '{')
    end function is_opening_bracket

    logical function is_closing_bracket(ch)
        character(len=1), intent(in) :: ch
        is_closing_bracket = (ch == ')' .or. ch == ']' .or. ch == '}')
    end function is_closing_bracket

    character(len=1) function get_bracket_pair(ch)
        character(len=1), intent(in) :: ch

        select case(ch)
        case('(')
            get_bracket_pair = ')'
        case(')')
            get_bracket_pair = '('
        case('[')
            get_bracket_pair = ']'
        case(']')
            get_bracket_pair = '['
        case('{')
            get_bracket_pair = '}'
        case('}')
            get_bracket_pair = '{'
        case default
            get_bracket_pair = ''
        end select
    end function get_bracket_pair

    subroutine find_matching_bracket(buffer, start_line, start_col, &
                                    found, match_line, match_col)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: match_line, match_col
        character(len=:), allocatable :: line
        character(len=1) :: start_char, target_char
        integer :: depth, current_line, current_col
        integer :: line_count, line_len
        logical :: search_forward

        found = .false.
        match_line = 0
        match_col = 0

        ! Get the character at the starting position
        line = buffer_get_line(buffer, start_line)
        if (start_col < 1 .or. start_col > len(line)) then
            if (allocated(line)) deallocate(line)
            return
        end if

        start_char = line(start_col:start_col)
        if (allocated(line)) deallocate(line)

        ! Check if it's a bracket
        if (.not. is_opening_bracket(start_char) .and. &
            .not. is_closing_bracket(start_char)) then
            return
        end if

        ! Get the matching bracket character
        target_char = get_bracket_pair(start_char)
        if (target_char == '') return

        ! Determine search direction
        search_forward = is_opening_bracket(start_char)

        ! Initialize depth counter
        depth = 1
        line_count = buffer_get_line_count(buffer)

        if (search_forward) then
            ! Search forward for closing bracket
            current_line = start_line
            current_col = start_col + 1

            do while (current_line <= line_count)
                line = buffer_get_line(buffer, current_line)
                line_len = len(line)

                ! If we're starting a new line, reset column
                if (current_line > start_line) then
                    current_col = 1
                end if

                do while (current_col <= line_len)
                    if (line(current_col:current_col) == start_char) then
                        depth = depth + 1
                    else if (line(current_col:current_col) == target_char) then
                        depth = depth - 1
                        if (depth == 0) then
                            found = .true.
                            match_line = current_line
                            match_col = current_col
                            if (allocated(line)) deallocate(line)
                            return
                        end if
                    end if
                    current_col = current_col + 1
                end do

                if (allocated(line)) deallocate(line)
                current_line = current_line + 1
            end do
        else
            ! Search backward for opening bracket
            current_line = start_line
            current_col = start_col - 1

            do while (current_line >= 1)
                line = buffer_get_line(buffer, current_line)
                line_len = len(line)

                ! If we're starting a new line, reset column to end
                if (current_line < start_line) then
                    current_col = line_len
                end if

                do while (current_col >= 1)
                    if (line(current_col:current_col) == start_char) then
                        depth = depth + 1
                    else if (line(current_col:current_col) == target_char) then
                        depth = depth - 1
                        if (depth == 0) then
                            found = .true.
                            match_line = current_line
                            match_col = current_col
                            if (allocated(line)) deallocate(line)
                            return
                        end if
                    end if
                    current_col = current_col - 1
                end do

                if (allocated(line)) deallocate(line)
                current_line = current_line - 1
            end do
        end if
    end subroutine find_matching_bracket

end module bracket_matching_module