module command_handler_module
    use iso_fortran_env, only: int32
    use editor_state_module, only: editor_state_t, cursor_t
    use text_buffer_module
    use renderer_module, only: update_viewport
    use yank_stack_module
    use clipboard_module
    implicit none
    private

    public :: handle_key_command, init_command_handler, cleanup_command_handler

    type(yank_stack_t) :: yank_stack

contains

    subroutine init_command_handler()
        call init_yank_stack(yank_stack)
    end subroutine init_command_handler

    subroutine cleanup_command_handler()
        call cleanup_yank_stack(yank_stack)
    end subroutine cleanup_command_handler

    subroutine handle_key_command(key_str, editor, buffer, should_quit)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(out) :: should_quit
        integer :: line_count

        should_quit = .false.
        line_count = buffer_get_line_count(buffer)

        select case(trim(key_str))
        ! File operations
        case('ctrl-q')
            should_quit = .true.

        ! Navigation
        case('up')
            call move_cursor_up(editor%cursors(editor%active_cursor), line_count)
            call update_viewport(editor)

        case('down')
            call move_cursor_down(editor%cursors(editor%active_cursor), line_count)
            call update_viewport(editor)

        case('left')
            call move_cursor_left(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('right')
            call move_cursor_right(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('home', 'ctrl-a')
            call move_cursor_home(editor%cursors(editor%active_cursor))
            call update_viewport(editor)

        case('end', 'ctrl-e')
            call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('pageup')
            call move_cursor_page_up(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        case('pagedown')
            call move_cursor_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        case('alt-left')
            call move_cursor_word_left(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-right')
            call move_cursor_word_right(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-up')
            call move_line_up(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-down')
            call move_line_down(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-shift-up')
            call duplicate_line_up(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-shift-down')
            call duplicate_line_down(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        ! Text modification
        case('backspace')
            call handle_backspace(editor%cursors(editor%active_cursor), buffer)

        case('delete')
            call handle_delete(editor%cursors(editor%active_cursor), buffer)

        case('enter')
            call handle_enter(editor%cursors(editor%active_cursor), buffer)

        case('tab')
            call handle_tab(editor%cursors(editor%active_cursor), buffer)

        ! Editing keybinds
        case('ctrl-k')
            call kill_line_forward(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-u')
            call kill_line_backward(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-y')
            call yank_text(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-x')
            call cut_selection_or_line(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-c')
            call copy_selection_or_line(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-v')
            call paste_clipboard(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-s')
            call save_file(editor, buffer)

        case("ctrl-'", "ctrl-apostrophe")
            call cycle_quotes(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-opt-backspace', 'ctrl-alt-backspace')
            call remove_brackets(editor%cursors(editor%active_cursor), buffer)

        case default
            ! Regular character input
            if (len_trim(key_str) == 1) then
                call insert_char(editor%cursors(editor%active_cursor), buffer, key_str(1:1))
            end if
        end select
    end subroutine handle_key_command

    subroutine move_cursor_up(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            cursor%column = cursor%desired_column
            ! Will adjust column in boundary check
        end if
    end subroutine move_cursor_up

    subroutine move_cursor_down(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        if (cursor%line < line_count) then
            cursor%line = cursor%line + 1
            cursor%column = cursor%desired_column
            ! Will adjust column in boundary check
        end if
    end subroutine move_cursor_down

    subroutine move_cursor_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = len(line) + 1
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
        end if
    end subroutine move_cursor_left

    subroutine move_cursor_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= len(line)) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to beginning of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = 1
        end if

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_right

    subroutine move_cursor_home(cursor)
        type(cursor_t), intent(inout) :: cursor
        cursor%column = 1
        cursor%desired_column = 1
    end subroutine move_cursor_home

    subroutine move_cursor_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = len(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_end

    subroutine move_cursor_page_up(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        page_size = editor%screen_rows - 2  ! Minus status bar and margin
        cursor%line = max(1, cursor%line - page_size)
        editor%viewport_line = max(1, editor%viewport_line - page_size)
    end subroutine move_cursor_page_up

    subroutine move_cursor_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        page_size = editor%screen_rows - 2  ! Minus status bar and margin
        cursor%line = min(line_count, cursor%line + page_size)
        editor%viewport_line = min(max(1, line_count - page_size), editor%viewport_line + page_size)
    end subroutine move_cursor_page_down

    function get_buffer_position(cursor, buffer) result(pos)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer :: pos
        integer :: line_num
        character(len=:), allocatable :: line

        pos = 0
        ! Calculate byte position in buffer
        do line_num = 1, cursor%line - 1
            line = buffer_get_line(buffer, line_num)
            pos = pos + len(line) + 1  ! +1 for newline
            if (allocated(line)) deallocate(line)
        end do
        pos = pos + cursor%column
    end function get_buffer_position

    subroutine insert_char(cursor, buffer, ch)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=1), intent(in) :: ch
        integer :: pos

        pos = get_buffer_position(cursor, buffer)
        call buffer_insert(buffer, pos, ch)
        cursor%column = cursor%column + 1
        cursor%desired_column = cursor%column
    end subroutine insert_char

    subroutine handle_backspace(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos

        if (cursor%column > 1) then
            cursor%column = cursor%column - 1
            pos = get_buffer_position(cursor, buffer)
            call buffer_delete(buffer, pos, 1)
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Join with previous line
            call move_cursor_left(cursor, buffer)
            pos = get_buffer_position(cursor, buffer)
            call buffer_delete(buffer, pos, 1)  ! Delete the newline
        end if
    end subroutine handle_backspace

    subroutine handle_delete(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos
        character(len=:), allocatable :: line
        integer :: line_count

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        pos = get_buffer_position(cursor, buffer)
        if (cursor%column <= len(line)) then
            call buffer_delete(buffer, pos, 1)
        else if (cursor%line < line_count) then
            ! Delete newline to join with next line
            call buffer_delete(buffer, pos, 1)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine handle_delete

    subroutine handle_enter(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos

        pos = get_buffer_position(cursor, buffer)
        call buffer_insert(buffer, pos, char(10))  ! Insert newline
        cursor%line = cursor%line + 1
        cursor%column = 1
        cursor%desired_column = 1
    end subroutine handle_enter

    subroutine handle_tab(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, spaces_to_add, i

        pos = get_buffer_position(cursor, buffer)
        spaces_to_add = 4 - mod(cursor%column - 1, 4)

        do i = 1, spaces_to_add
            call buffer_insert(buffer, pos + i - 1, ' ')
        end do

        cursor%column = cursor%column + spaces_to_add
        cursor%desired_column = cursor%column
    end subroutine handle_tab

    subroutine kill_line_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, delete_count
        character(len=:), allocatable :: line, killed_text

        line = buffer_get_line(buffer, cursor%line)
        pos = get_buffer_position(cursor, buffer)
        delete_count = len(line) - cursor%column + 1

        if (delete_count > 0) then
            ! Save killed text to yank stack
            allocate(character(len=delete_count) :: killed_text)
            killed_text = line(cursor%column:len(line))
            call push_yank(yank_stack, killed_text)
            call buffer_delete(buffer, pos, delete_count)
            deallocate(killed_text)
        else if (cursor%line < buffer_get_line_count(buffer)) then
            ! Delete the newline and save it
            call push_yank(yank_stack, char(10))
            call buffer_delete(buffer, pos, 1)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine kill_line_forward

    subroutine kill_line_backward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: pos, delete_count
        character(len=:), allocatable :: line, killed_text

        if (cursor%column > 1) then
            line = buffer_get_line(buffer, cursor%line)
            pos = get_buffer_position(cursor, buffer)
            delete_count = cursor%column - 1

            ! Save killed text to yank stack
            allocate(character(len=delete_count) :: killed_text)
            killed_text = line(1:cursor%column-1)
            call push_yank(yank_stack, killed_text)

            call buffer_delete(buffer, pos - delete_count, delete_count)
            cursor%column = 1
            cursor%desired_column = 1

            deallocate(killed_text)
            if (allocated(line)) deallocate(line)
        end if
    end subroutine kill_line_backward

    subroutine yank_text(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: yanked_text
        integer :: pos, i

        yanked_text = pop_yank(yank_stack)
        if (len(yanked_text) > 0) then
            pos = get_buffer_position(cursor, buffer)
            call buffer_insert(buffer, pos, yanked_text)

            ! Move cursor past inserted text
            do i = 1, len(yanked_text)
                if (yanked_text(i:i) == char(10)) then
                    cursor%line = cursor%line + 1
                    cursor%column = 1
                else
                    cursor%column = cursor%column + 1
                end if
            end do
            cursor%desired_column = cursor%column
        end if

        if (allocated(yanked_text)) deallocate(yanked_text)
    end subroutine yank_text

    subroutine move_cursor_word_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: i
        logical :: in_word, found_word

        line = buffer_get_line(buffer, cursor%line)

        if (cursor%column > 1) then
            i = cursor%column - 1
            in_word = .false.
            found_word = .false.

            ! Move left through whitespace
            do while (i > 0 .and. is_whitespace(line(i:i)))
                i = i - 1
            end do

            ! Move left through word characters
            do while (i > 0 .and. .not. is_whitespace(line(i:i)))
                i = i - 1
                found_word = .true.
            end do

            if (found_word .and. i > 0) then
                cursor%column = i + 1
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column

        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = len(line) + 1
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_left

    subroutine move_cursor_word_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: i, line_len, line_count
        logical :: found_word

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= line_len) then
            i = cursor%column
            found_word = .false.

            ! Move right through current word
            do while (i <= line_len .and. .not. is_whitespace(line(i:i)))
                i = i + 1
                found_word = .true.
            end do

            ! Move right through whitespace
            do while (i <= line_len .and. is_whitespace(line(i:i)))
                i = i + 1
            end do

            cursor%column = i
            cursor%desired_column = cursor%column

        else if (cursor%line < line_count) then
            ! Move to beginning of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = 1
        end if

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_right

    function is_whitespace(ch) result(is_space)
        character(len=1), intent(in) :: ch
        logical :: is_space

        is_space = (ch == ' ' .or. ch == char(9))  ! Space or tab
    end function is_whitespace

    subroutine save_file(editor, buffer)
        use iso_fortran_env, only: error_unit
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: status

        if (allocated(editor%filename)) then
            call buffer_save_file(buffer, editor%filename, status)
            if (status /= 0) then
                write(error_unit, *) 'Error saving file'
            end if
        else
            write(error_unit, *) 'No filename specified'
        end if
    end subroutine save_file

    subroutine move_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, prev_line
        integer :: current_line_start, current_line_end
        integer :: prev_line_start, prev_line_end
        integer :: line_count

        line_count = buffer_get_line_count(buffer)
        if (cursor%line <= 1) return  ! Can't move first line up

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        prev_line = buffer_get_line(buffer, cursor%line - 1)

        ! Calculate positions
        call get_line_positions(buffer, cursor%line - 1, prev_line_start, prev_line_end)
        call get_line_positions(buffer, cursor%line, current_line_start, current_line_end)

        ! Delete both lines (current first, then previous)
        call buffer_delete(buffer, current_line_start, current_line_end - current_line_start + 1)
        call buffer_delete(buffer, prev_line_start, prev_line_end - prev_line_start + 1)

        ! Insert in swapped order
        call buffer_insert(buffer, prev_line_start, current_line // char(10) // prev_line)
        if (cursor%line == line_count) then
            ! If we were on the last line, don't add extra newline
            call buffer_insert(buffer, prev_line_start + len(current_line) + len(prev_line) + 1, char(10))
        end if

        ! Update cursor position
        cursor%line = cursor%line - 1

        if (allocated(current_line)) deallocate(current_line)
        if (allocated(prev_line)) deallocate(prev_line)
    end subroutine move_line_up

    subroutine move_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, next_line
        integer :: current_line_start, current_line_end
        integer :: next_line_start, next_line_end
        integer :: line_count

        line_count = buffer_get_line_count(buffer)
        if (cursor%line >= line_count) return  ! Can't move last line down

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        next_line = buffer_get_line(buffer, cursor%line + 1)

        ! Calculate positions
        call get_line_positions(buffer, cursor%line, current_line_start, current_line_end)
        call get_line_positions(buffer, cursor%line + 1, next_line_start, next_line_end)

        ! Delete both lines (next first, then current)
        call buffer_delete(buffer, next_line_start, next_line_end - next_line_start + 1)
        call buffer_delete(buffer, current_line_start, current_line_end - current_line_start + 1)

        ! Insert in swapped order
        call buffer_insert(buffer, current_line_start, next_line // char(10) // current_line)
        if (cursor%line + 1 == line_count) then
            ! If next was the last line, don't add extra newline
            call buffer_insert(buffer, current_line_start + len(next_line) + len(current_line) + 1, char(10))
        end if

        ! Update cursor position
        cursor%line = cursor%line + 1

        if (allocated(current_line)) deallocate(current_line)
        if (allocated(next_line)) deallocate(next_line)
    end subroutine move_line_down

    subroutine duplicate_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: line_start, line_end

        line = buffer_get_line(buffer, cursor%line)
        call get_line_positions(buffer, cursor%line, line_start, line_end)

        ! Insert duplicate above current line
        call buffer_insert(buffer, line_start, line // char(10))

        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_up

    subroutine duplicate_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: line_start, line_end

        line = buffer_get_line(buffer, cursor%line)
        call get_line_positions(buffer, cursor%line, line_start, line_end)

        ! Insert duplicate below current line
        call buffer_insert(buffer, line_end + 1, char(10) // line)

        ! Move cursor to duplicated line
        cursor%line = cursor%line + 1

        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_down

    subroutine get_line_positions(buffer, line_num, start_pos, end_pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer, intent(out) :: start_pos, end_pos
        integer :: current_line, pos
        character :: ch

        current_line = 1
        start_pos = 1
        pos = 1

        ! Find start of requested line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then
                current_line = current_line + 1
                start_pos = pos + 1
            end if
            pos = pos + 1
        end do

        ! Find end of line
        end_pos = start_pos
        do
            ch = buffer_get_char(buffer, end_pos)
            if (ch == char(10) .or. ch == char(0)) exit
            end_pos = end_pos + 1
        end do
        end_pos = end_pos - 1  ! Point to last character, not newline
    end subroutine get_line_positions

    subroutine cycle_quotes(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: quote_start, quote_end, pos
        character :: current_quote, new_quote
        character(len=3) :: quote_cycle = '"' // "'" // '`'
        integer :: quote_index, i

        line = buffer_get_line(buffer, cursor%line)

        ! Find quotes around cursor position
        quote_start = 0
        quote_end = 0

        ! Search backward for opening quote
        do i = cursor%column - 1, 1, -1
            if (i <= len(line)) then
                if (line(i:i) == '"' .or. line(i:i) == "'" .or. line(i:i) == '`') then
                    quote_start = i
                    current_quote = line(i:i)
                    exit
                end if
            end if
        end do

        ! Search forward for closing quote (must match opening)
        if (quote_start > 0) then
            do i = cursor%column, len(line)
                if (line(i:i) == current_quote) then
                    quote_end = i
                    exit
                end if
            end do
        end if

        ! If we found matching quotes, cycle them
        if (quote_start > 0 .and. quote_end > 0 .and. quote_start < quote_end) then
            ! Find current quote in cycle
            quote_index = index(quote_cycle, current_quote)
            if (quote_index > 0) then
                ! Get next quote in cycle
                quote_index = mod(quote_index, 3) + 1
                new_quote = quote_cycle(quote_index:quote_index)

                ! Replace closing quote first (so positions don't change)
                call replace_char_at_position(buffer, cursor%line, quote_end, new_quote)
                ! Replace opening quote
                call replace_char_at_position(buffer, cursor%line, quote_start, new_quote)
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine cycle_quotes

    subroutine remove_brackets(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: bracket_start, bracket_end, i
        character :: open_bracket, close_bracket
        character(len=3) :: open_brackets = '([{'
        character(len=3) :: close_brackets = ')]}'
        integer :: bracket_type

        line = buffer_get_line(buffer, cursor%line)

        ! Find brackets around cursor position
        bracket_start = 0
        bracket_end = 0

        ! Search backward for opening bracket
        do i = cursor%column - 1, 1, -1
            if (i <= len(line)) then
                bracket_type = index(open_brackets, line(i:i))
                if (bracket_type > 0) then
                    bracket_start = i
                    open_bracket = line(i:i)
                    close_bracket = close_brackets(bracket_type:bracket_type)
                    exit
                end if
            end if
        end do

        ! Search forward for matching closing bracket
        if (bracket_start > 0) then
            do i = cursor%column, len(line)
                if (line(i:i) == close_bracket) then
                    bracket_end = i
                    exit
                end if
            end do
        end if

        ! If we found matching brackets, remove them
        if (bracket_start > 0 .and. bracket_end > 0 .and. bracket_start < bracket_end) then
            ! Delete closing bracket first (so positions don't change)
            call delete_char_at_position(buffer, cursor%line, bracket_end)
            ! Delete opening bracket
            call delete_char_at_position(buffer, cursor%line, bracket_start)

            ! Adjust cursor position if needed
            if (cursor%column > bracket_start) then
                cursor%column = cursor%column - 1
                if (cursor%column > bracket_end - 1) then
                    cursor%column = cursor%column - 1
                end if
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine remove_brackets

    subroutine replace_char_at_position(buffer, line_num, col_pos, new_char)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_num, col_pos
        character(len=1), intent(in) :: new_char
        integer :: buffer_pos, line_start, current_line, pos
        character :: ch

        ! Calculate buffer position for line_num:col_pos
        current_line = 1
        buffer_pos = 0
        pos = 1

        ! Find start of target line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then
                current_line = current_line + 1
            end if
            pos = pos + 1
        end do

        ! Add column offset
        buffer_pos = pos + col_pos - 1

        ! Delete old character and insert new one
        call buffer_delete(buffer, buffer_pos, 1)
        call buffer_insert(buffer, buffer_pos, new_char)
    end subroutine replace_char_at_position

    subroutine delete_char_at_position(buffer, line_num, col_pos)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_num, col_pos
        integer :: buffer_pos, current_line, pos
        character :: ch

        ! Calculate buffer position for line_num:col_pos
        current_line = 1
        buffer_pos = 0
        pos = 1

        ! Find start of target line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then
                current_line = current_line + 1
            end if
            pos = pos + 1
        end do

        ! Add column offset
        buffer_pos = pos + col_pos - 1

        ! Delete the character
        call buffer_delete(buffer, buffer_pos, 1)
    end subroutine delete_char_at_position

    subroutine copy_selection_or_line(cursor, buffer)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text_to_copy

        if (cursor%has_selection) then
            ! Copy selected text (not implemented yet)
            text_to_copy = ''  ! Would get selection
        else
            ! Copy current line
            text_to_copy = buffer_get_line(buffer, cursor%line)
        end if

        if (allocated(text_to_copy)) then
            call copy_to_clipboard(text_to_copy)
            deallocate(text_to_copy)
        end if
    end subroutine copy_selection_or_line

    subroutine cut_selection_or_line(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text_to_cut
        integer :: line_start, line_end

        if (cursor%has_selection) then
            ! Cut selected text (not implemented yet)
            text_to_cut = ''  ! Would get selection and delete it
        else
            ! Cut current line
            text_to_cut = buffer_get_line(buffer, cursor%line) // char(10)
            call copy_to_clipboard(text_to_cut)

            ! Delete the line
            call get_line_positions(buffer, cursor%line, line_start, line_end)
            call buffer_delete(buffer, line_start, line_end - line_start + 2)  ! +2 for newline

            ! Adjust cursor
            cursor%column = 1
            cursor%desired_column = 1
        end if

        if (allocated(text_to_cut)) then
            deallocate(text_to_cut)
        end if
    end subroutine cut_selection_or_line

    subroutine paste_clipboard(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: clipboard_text
        integer :: pos, i

        clipboard_text = paste_from_clipboard()
        if (len(clipboard_text) > 0) then
            pos = get_buffer_position(cursor, buffer)
            call buffer_insert(buffer, pos, clipboard_text)

            ! Move cursor past inserted text
            do i = 1, len(clipboard_text)
                if (clipboard_text(i:i) == char(10)) then
                    cursor%line = cursor%line + 1
                    cursor%column = 1
                else
                    cursor%column = cursor%column + 1
                end if
            end do
            cursor%desired_column = cursor%column
        end if

        if (allocated(clipboard_text)) deallocate(clipboard_text)
    end subroutine paste_clipboard

end module command_handler_module