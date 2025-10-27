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
    character(len=:), allocatable :: search_pattern  ! For ctrl-d functionality

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

        case('ctrl-d')
            call select_next_match(editor, buffer)

        case default
            ! Check for mouse events
            if (index(key_str, 'mouse-') == 1) then
                call handle_mouse_event_action(key_str, editor, buffer)
            ! Regular character input
            else if (len_trim(key_str) == 1) then
                ! Handle character input for all cursors
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, key_str(1:1))
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, key_str(1:1))
                end if
            end if
        end select
    end subroutine handle_key_command

    subroutine move_cursor_up(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        cursor%has_selection = .false.  ! Clear selection
        if (cursor%line > 1) then
            cursor%line = cursor%line - 1
            cursor%column = cursor%desired_column
            ! Will adjust column in boundary check
        end if
    end subroutine move_cursor_up

    subroutine move_cursor_down(cursor, line_count)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: line_count

        cursor%has_selection = .false.  ! Clear selection
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

        cursor%has_selection = .false.  ! Clear selection
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

        cursor%has_selection = .false.  ! Clear selection
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
        integer :: pos, selection_start_pos, delete_count

        ! If there's a selection, delete it first
        if (cursor%has_selection) then
            if (cursor%selection_start_line == cursor%line) then
                ! Single-line selection
                selection_start_pos = get_buffer_position_at(buffer, &
                                        cursor%selection_start_line, cursor%selection_start_col)
                delete_count = cursor%column - cursor%selection_start_col
                call buffer_delete(buffer, selection_start_pos, delete_count)

                ! Move cursor to selection start
                cursor%column = cursor%selection_start_col
                cursor%has_selection = .false.
            end if
        end if

        pos = get_buffer_position(cursor, buffer)
        call buffer_insert(buffer, pos, ch)
        cursor%column = cursor%column + 1
        cursor%desired_column = cursor%column
    end subroutine insert_char

    function get_buffer_position_at(buffer, line_num, col) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        integer :: pos
        integer :: current_line
        character :: ch

        pos = 1
        current_line = 1

        ! Find position of line
        do while (current_line < line_num)
            ch = buffer_get_char(buffer, pos)
            if (ch == char(10)) then
                current_line = current_line + 1
            end if
            pos = pos + 1
        end do

        ! Add column offset
        pos = pos + col - 1
    end function get_buffer_position_at

    subroutine insert_char_multiple_cursors(editor, buffer, ch)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=1), intent(in) :: ch
        integer :: i, offset
        integer, allocatable :: line_offsets(:)

        ! Track line offsets as we modify the buffer
        allocate(line_offsets(size(editor%cursors)))
        line_offsets = 0

        ! Process cursors from bottom to top to avoid position conflicts
        do i = size(editor%cursors), 1, -1
            ! Apply offset from previous insertions on the same line
            if (i < size(editor%cursors)) then
                if (editor%cursors(i)%line == editor%cursors(i+1)%line) then
                    ! Same line - adjust column by previous insertions
                    editor%cursors(i)%column = editor%cursors(i)%column + line_offsets(i+1)
                    if (editor%cursors(i)%has_selection) then
                        editor%cursors(i)%selection_start_col = &
                            editor%cursors(i)%selection_start_col + line_offsets(i+1)
                    end if
                end if
            end if

            ! Insert character at this cursor
            call insert_char(editor%cursors(i), buffer, ch)

            ! Track offset for this line
            if (editor%cursors(i)%has_selection) then
                ! Selection was replaced - net change is 1 char minus selection length
                line_offsets(i) = 1
            else
                ! Simple insertion
                line_offsets(i) = 1
            end if

            ! Pass offset to previous cursors on same line
            if (i > 1) then
                if (editor%cursors(i-1)%line == editor%cursors(i)%line) then
                    line_offsets(i-1) = line_offsets(i)
                end if
            end if
        end do

        deallocate(line_offsets)
    end subroutine insert_char_multiple_cursors

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

    subroutine handle_mouse_event_action(key_str, editor, buffer)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer :: button, row, col
        integer :: colon1, colon2, colon3
        character(len=100) :: event_type
        integer :: ios, line_count
        logical :: is_alt_click
        type(cursor_t), allocatable :: new_cursors(:)
        integer :: i, cursor_exists

        line_count = buffer_get_line_count(buffer)

        ! Parse the mouse event string format: "mouse-type:button:row:col"
        colon1 = index(key_str, ':')
        if (colon1 == 0) return

        event_type = key_str(1:colon1-1)
        colon2 = index(key_str(colon1+1:), ':') + colon1
        if (colon2 == colon1) return

        colon3 = index(key_str(colon2+1:), ':') + colon2
        if (colon3 == colon2) return

        ! Parse button, row, and col
        read(key_str(colon1+1:colon2-1), '(i10)', iostat=ios) button
        if (ios /= 0) return

        read(key_str(colon2+1:colon3-1), '(i10)', iostat=ios) row
        if (ios /= 0) return

        read(key_str(colon3+1:), '(i10)', iostat=ios) col
        if (ios /= 0) return

        ! Handle different mouse event types
        select case(trim(event_type))
        case('mouse-click')
            ! Regular click - move cursor to position
            if (button == 0) then  ! Left click
                call position_cursor_at_screen(editor%cursors(editor%active_cursor), &
                                              editor, buffer, row, col)
                ! Clear other cursors (single cursor mode)
                if (allocated(editor%cursors)) then
                    if (size(editor%cursors) > 1) then
                        deallocate(editor%cursors)
                        allocate(editor%cursors(1))
                        call init_cursor(editor%cursors(1))
                        call position_cursor_at_screen(editor%cursors(1), &
                                                      editor, buffer, row, col)
                        editor%active_cursor = 1
                    end if
                end if
            end if

        case('mouse-alt')
            ! Alt+click - add or remove cursor
            if (button == 8) then  ! Alt + left click (button code includes alt modifier)
                ! Check if cursor already exists at this position
                cursor_exists = 0
                do i = 1, size(editor%cursors)
                    if (is_cursor_at_screen_pos(editor%cursors(i), editor, row, col)) then
                        cursor_exists = i
                        exit
                    end if
                end do

                if (cursor_exists > 0) then
                    ! Remove the cursor
                    if (size(editor%cursors) > 1) then
                        allocate(new_cursors(size(editor%cursors) - 1))
                        do i = 1, cursor_exists - 1
                            new_cursors(i) = editor%cursors(i)
                        end do
                        do i = cursor_exists + 1, size(editor%cursors)
                            new_cursors(i-1) = editor%cursors(i)
                        end do
                        deallocate(editor%cursors)
                        editor%cursors = new_cursors
                        if (editor%active_cursor >= cursor_exists) then
                            editor%active_cursor = max(1, editor%active_cursor - 1)
                        end if
                    end if
                else
                    ! Add a new cursor
                    allocate(new_cursors(size(editor%cursors) + 1))
                    do i = 1, size(editor%cursors)
                        new_cursors(i) = editor%cursors(i)
                    end do
                    call init_cursor(new_cursors(size(new_cursors)))
                    call position_cursor_at_screen(new_cursors(size(new_cursors)), &
                                                  editor, buffer, row, col)
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = size(editor%cursors)
                end if
            end if

        case('mouse-shift')
            ! Shift+click - create selection (future implementation)
            ! For now, just move cursor
            if (button == 4) then  ! Shift + left click
                call position_cursor_at_screen(editor%cursors(editor%active_cursor), &
                                              editor, buffer, row, col)
            end if

        end select
    end subroutine handle_mouse_event_action

    subroutine position_cursor_at_screen(cursor, editor, buffer, screen_row, screen_col)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: screen_row, screen_col
        integer :: target_line, target_col
        character(len=:), allocatable :: line
        integer :: line_count

        line_count = buffer_get_line_count(buffer)

        ! Convert screen position to buffer position
        target_line = editor%viewport_line + screen_row - 1
        target_col = editor%viewport_column + screen_col - 1

        ! Clamp to valid range
        if (target_line < 1) target_line = 1
        if (target_line > line_count) target_line = line_count

        ! Get line and adjust column
        line = buffer_get_line(buffer, target_line)
        if (target_col < 1) target_col = 1
        if (target_col > len(line) + 1) target_col = len(line) + 1

        ! Set cursor position
        cursor%line = target_line
        cursor%column = target_col
        cursor%desired_column = target_col

        if (allocated(line)) deallocate(line)
    end subroutine position_cursor_at_screen

    function is_cursor_at_screen_pos(cursor, editor, screen_row, screen_col) result(at_pos)
        type(cursor_t), intent(in) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: screen_row, screen_col
        logical :: at_pos
        integer :: cursor_screen_row, cursor_screen_col

        cursor_screen_row = cursor%line - editor%viewport_line + 1
        cursor_screen_col = cursor%column - editor%viewport_column + 1

        at_pos = (cursor_screen_row == screen_row .and. cursor_screen_col == screen_col)
    end function is_cursor_at_screen_pos

    subroutine init_cursor(cursor)
        type(cursor_t), intent(out) :: cursor
        cursor%line = 1
        cursor%column = 1
        cursor%desired_column = 1
        cursor%has_selection = .false.
        cursor%selection_start_line = 1
        cursor%selection_start_col = 1
    end subroutine init_cursor

    subroutine select_next_match(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        type(cursor_t) :: main_cursor
        character(len=:), allocatable :: selected_text, line
        integer :: word_start, word_end
        integer :: i, search_line, search_col
        logical :: found_match
        type(cursor_t), allocatable :: new_cursors(:)

        main_cursor = editor%cursors(editor%active_cursor)

        ! If no selection, select current word
        if (.not. main_cursor%has_selection) then
            call select_word_at_cursor(editor%cursors(editor%active_cursor), buffer)

            ! Store the selected word as search pattern
            if (editor%cursors(editor%active_cursor)%has_selection) then
                selected_text = get_selection_text(editor%cursors(editor%active_cursor), buffer)
                if (allocated(search_pattern)) deallocate(search_pattern)
                allocate(character(len=len(selected_text)) :: search_pattern)
                search_pattern = selected_text
            end if
        else
            ! We have a selection, find next occurrence
            if (.not. allocated(search_pattern)) then
                selected_text = get_selection_text(main_cursor, buffer)
                allocate(character(len=len(selected_text)) :: search_pattern)
                search_pattern = selected_text
            end if

            ! Search for next occurrence starting from last cursor position
            call find_next_occurrence(buffer, search_pattern, &
                                     editor%cursors(size(editor%cursors))%line, &
                                     editor%cursors(size(editor%cursors))%selection_start_col + 1, &
                                     search_line, search_col, found_match)

            if (found_match) then
                ! Add new cursor with selection at found position
                allocate(new_cursors(size(editor%cursors) + 1))
                do i = 1, size(editor%cursors)
                    new_cursors(i) = editor%cursors(i)
                end do

                ! Initialize new cursor at found position
                call init_cursor(new_cursors(size(new_cursors)))
                new_cursors(size(new_cursors))%line = search_line
                new_cursors(size(new_cursors))%column = search_col + len(search_pattern)
                new_cursors(size(new_cursors))%desired_column = search_col + len(search_pattern)
                new_cursors(size(new_cursors))%has_selection = .true.
                new_cursors(size(new_cursors))%selection_start_line = search_line
                new_cursors(size(new_cursors))%selection_start_col = search_col

                deallocate(editor%cursors)
                editor%cursors = new_cursors
                editor%active_cursor = size(editor%cursors)
            end if
        end if

        if (allocated(selected_text)) deallocate(selected_text)
        if (allocated(line)) deallocate(line)
    end subroutine select_next_match

    subroutine select_word_at_cursor(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: word_start, word_end, i

        line = buffer_get_line(buffer, cursor%line)
        if (cursor%column > len(line)) then
            if (allocated(line)) deallocate(line)
            return
        end if

        ! Find word boundaries
        word_start = cursor%column
        word_end = cursor%column

        ! Check if we're on a word character
        if (cursor%column <= len(line)) then
            if (.not. is_word_char(line(cursor%column:cursor%column))) then
                if (allocated(line)) deallocate(line)
                return
            end if
        else
            if (allocated(line)) deallocate(line)
            return
        end if

        ! Find start of word
        do i = cursor%column - 1, 1, -1
            if (.not. is_word_char(line(i:i))) exit
            word_start = i
        end do

        ! Find end of word
        do i = cursor%column, len(line)
            if (.not. is_word_char(line(i:i))) exit
            word_end = i
        end do

        ! Set selection
        cursor%has_selection = .true.
        cursor%selection_start_line = cursor%line
        cursor%selection_start_col = word_start
        cursor%column = word_end + 1
        cursor%desired_column = cursor%column

        if (allocated(line)) deallocate(line)
    end subroutine select_word_at_cursor

    function is_word_char(ch) result(is_word)
        character(len=1), intent(in) :: ch
        logical :: is_word

        is_word = (ch >= 'a' .and. ch <= 'z') .or. &
                  (ch >= 'A' .and. ch <= 'Z') .or. &
                  (ch >= '0' .and. ch <= '9') .or. &
                  ch == '_'
    end function is_word_char

    function get_selection_text(cursor, buffer) result(text)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text, line

        if (.not. cursor%has_selection) then
            text = ''
            return
        end if

        ! For now, only handle single-line selections
        if (cursor%selection_start_line == cursor%line) then
            line = buffer_get_line(buffer, cursor%line)
            if (cursor%selection_start_col <= len(line)) then
                allocate(character(len=cursor%column - cursor%selection_start_col) :: text)
                text = line(cursor%selection_start_col:min(cursor%column - 1, len(line)))
            else
                text = ''
            end if
            if (allocated(line)) deallocate(line)
        else
            text = ''
        end if
    end function get_selection_text

    subroutine find_next_occurrence(buffer, pattern, start_line, start_col, &
                                   found_line, found_col, found)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        integer, intent(out) :: found_line, found_col
        logical, intent(out) :: found
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos

        found = .false.
        line_count = buffer_get_line_count(buffer)

        ! Search from current position to end of file
        do current_line = start_line, line_count
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                pos = index(line(min(start_col, len(line)+1):), pattern)
                if (pos > 0) then
                    found = .true.
                    found_line = current_line
                    found_col = start_col + pos - 1
                    if (allocated(line)) deallocate(line)
                    return
                end if
            else
                pos = index(line, pattern)
                if (pos > 0) then
                    found = .true.
                    found_line = current_line
                    found_col = pos
                    if (allocated(line)) deallocate(line)
                    return
                end if
            end if

            if (allocated(line)) deallocate(line)
        end do

        ! Wrap around and search from beginning
        do current_line = 1, start_line - 1
            line = buffer_get_line(buffer, current_line)
            pos = index(line, pattern)

            if (pos > 0) then
                found = .true.
                found_line = current_line
                found_col = pos
                if (allocated(line)) deallocate(line)
                return
            end if

            if (allocated(line)) deallocate(line)
        end do
    end subroutine find_next_occurrence

end module command_handler_module