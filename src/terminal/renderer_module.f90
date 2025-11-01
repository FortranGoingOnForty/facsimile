module renderer_module
    use iso_fortran_env, only: int32, output_unit
    use terminal_io_module
    use text_buffer_module
    use editor_state_module, only: editor_state_t, cursor_t
    use bracket_matching_module
    use file_tree_module
    use file_tree_renderer_module
    implicit none
    private

    public :: render_screen, update_viewport, init_renderer, cleanup_renderer
    public :: render_status_bar, render_cursor
    public :: show_line_numbers, LINE_NUMBER_WIDTH
    public :: render_screen_with_tree
    public :: tree_state

    ! Configuration
    logical :: show_line_numbers = .true.
    logical :: highlight_current_line = .true.
    integer, parameter :: LINE_NUMBER_WIDTH = 5  ! Width for line number display

    ! Bracket matching state
    integer :: bracket_line = 0
    integer :: bracket_col = 0
    integer :: matching_bracket_line = 0
    integer :: matching_bracket_col = 0

    ! Screen buffer for double buffering
    type :: screen_buffer_t
        character(len=:), allocatable :: lines(:)
        integer :: rows
        integer :: cols
        logical :: needs_full_redraw
    end type screen_buffer_t

    type(screen_buffer_t) :: screen_buffer

    ! File tree state (for fuss mode)
    type(tree_state_t) :: tree_state

contains

    subroutine init_renderer(rows, cols)
        integer, intent(in) :: rows, cols
        integer :: i

        screen_buffer%rows = rows
        screen_buffer%cols = cols
        screen_buffer%needs_full_redraw = .true.

        allocate(character(len=cols) :: screen_buffer%lines(rows))
        do i = 1, rows
            screen_buffer%lines(i) = repeat(' ', cols)
        end do
    end subroutine init_renderer

    subroutine cleanup_renderer()
        if (allocated(screen_buffer%lines)) deallocate(screen_buffer%lines)
    end subroutine cleanup_renderer

    subroutine render_screen(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer :: screen_row, buffer_line, line_count
        character(len=:), allocatable :: line_content
        character(len=1) :: ch, cursor_char
        integer :: col, buffer_pos, line_start_pos
        integer :: content_width
        integer :: start_row, row_offset_val
        character(len=16) :: line_num_str
        logical :: found_match
        type(cursor_t) :: cursor

        call terminal_hide_cursor()

        ! Render tab bar if there are any tabs
        call render_tab_bar(editor)

        ! Check if cursor is on a bracket and find its match
        cursor = editor%cursors(editor%active_cursor)
        line_content = buffer_get_line(buffer, cursor%line)
        if (cursor%column >= 1 .and. cursor%column <= len(line_content)) then
            cursor_char = line_content(cursor%column:cursor%column)
            if (is_opening_bracket(cursor_char) .or. is_closing_bracket(cursor_char)) then
                bracket_line = cursor%line
                bracket_col = cursor%column
                call find_matching_bracket(buffer, bracket_line, bracket_col, &
                                         found_match, matching_bracket_line, matching_bracket_col)
                if (.not. found_match) then
                    matching_bracket_line = 0
                    matching_bracket_col = 0
                end if
            else
                bracket_line = 0
                bracket_col = 0
                matching_bracket_line = 0
                matching_bracket_col = 0
            end if
        else
            bracket_line = 0
            bracket_col = 0
            matching_bracket_line = 0
            matching_bracket_col = 0
        end if
        if (allocated(line_content)) deallocate(line_content)

        ! Get total lines in buffer
        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers)
        if (show_line_numbers) then
            content_width = editor%screen_cols - LINE_NUMBER_WIDTH - 1  ! -1 for separator
        else
            content_width = editor%screen_cols
        end if

        ! Determine starting row based on whether tabs exist
        if (size(editor%tabs) > 0) then
            start_row = 2  ! Tab bar at row 1
            row_offset_val = 2
        else
            start_row = 1  ! No tab bar
            row_offset_val = 1
        end if

        ! Render all panes for the active tab
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            allocated(editor%tabs(editor%active_tab_index)%panes)) then
            call render_all_panes(buffer, editor)
        else
            ! Fallback to simple rendering if no tabs/panes
            ! Clear and render each visible line
            do screen_row = start_row, editor%screen_rows - 1  ! Last row for status bar
                buffer_line = editor%viewport_line + screen_row - row_offset_val

                call terminal_move_cursor(screen_row, 1)

                ! Render line number if enabled
                if (show_line_numbers) then
                    if (buffer_line <= line_count) then
                        ! Format line number, right-aligned
                        write(line_num_str, '(i5)') buffer_line

                        ! Highlight current line number
                        if (buffer_line == editor%cursors(editor%active_cursor)%line) then
                            call terminal_write(char(27) // '[1;33m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                              // char(27) // '[0m ')
                        else
                            call terminal_write(char(27) // '[90m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                              // char(27) // '[0m ')
                        end if
                    else
                        ! Empty line number area for lines beyond file
                        call terminal_write(repeat(' ', LINE_NUMBER_WIDTH + 1))
                    end if
                end if

                if (buffer_line <= line_count) then
                    ! Render actual line content with selections
                    call render_line_with_selections(buffer, editor, buffer_line, &
                                                    editor%viewport_column, content_width)
                else
                    ! Render empty line indicator
                    if (buffer_line == line_count + 1 .and. line_count == 0) then
                        ! Empty file
                        call terminal_write('~' // repeat(' ', content_width - 1))
                    else
                        ! Beyond file content
                        call terminal_write('~' // repeat(' ', content_width - 1))
                    end if
                end if
            end do
        end if

        ! Render status bar
        call render_status_bar(editor, buffer)

        ! Position cursor for panes or regular view
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0 .and. &
            allocated(editor%tabs(editor%active_tab_index)%panes)) then
            call render_cursor_for_panes(editor)
        else
            call render_cursor(editor)
        end if
    end subroutine render_screen

    subroutine render_line(buffer, line_num, start_col, width)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, start_col, width
        character(len=:), allocatable :: line
        character(len=:), allocatable :: visible_part
        integer :: line_len, end_col

        ! Get the line content
        line = buffer_get_line(buffer, line_num)
        line_len = len(line)

        ! Calculate visible portion
        if (start_col > line_len) then
            ! Line is scrolled past its end
            visible_part = repeat(' ', width)
        else
            end_col = min(start_col + width - 1, line_len)
            if (end_col >= start_col) then
                visible_part = line(start_col:end_col)
                ! Pad with spaces if needed
                if (len(visible_part) < width) then
                    visible_part = visible_part // repeat(' ', width - len(visible_part))
                end if
            else
                visible_part = repeat(' ', width)
            end if
        end if

        ! Write the visible part
        call terminal_write(visible_part)

        if (allocated(line)) deallocate(line)
        if (allocated(visible_part)) deallocate(visible_part)
    end subroutine render_line

    subroutine render_line_with_selections(buffer, editor, line_num, start_col, width)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_num, start_col, width
        character(len=:), allocatable :: line
        integer :: i, col, line_len
        integer :: sel_start_line, sel_start_col, sel_end_line, sel_end_col
        logical :: in_selection, is_bracket_match, is_current_line
        character :: ch

        line = buffer_get_line(buffer, line_num)
        line_len = len(line)

        ! Check if this is the current line
        is_current_line = (line_num == editor%cursors(editor%active_cursor)%line) .and. highlight_current_line

        ! Render each character with selection highlighting
        do col = start_col, min(start_col + width - 1, line_len + 1)
            in_selection = .false.
            is_bracket_match = .false.

            ! Check if this position is in any cursor's selection
            do i = 1, size(editor%cursors)
                if (editor%cursors(i)%has_selection) then
                    ! Determine selection bounds (handle both directions)
                    if (editor%cursors(i)%line < editor%cursors(i)%selection_start_line .or. &
                        (editor%cursors(i)%line == editor%cursors(i)%selection_start_line .and. &
                         editor%cursors(i)%column < editor%cursors(i)%selection_start_col)) then
                        ! Cursor is before selection start (selecting upward)
                        sel_start_line = editor%cursors(i)%line
                        sel_start_col = editor%cursors(i)%column
                        sel_end_line = editor%cursors(i)%selection_start_line
                        sel_end_col = editor%cursors(i)%selection_start_col
                    else
                        ! Cursor is after selection start (selecting downward)
                        sel_start_line = editor%cursors(i)%selection_start_line
                        sel_start_col = editor%cursors(i)%selection_start_col
                        sel_end_line = editor%cursors(i)%line
                        sel_end_col = editor%cursors(i)%column
                    end if

                    ! Check if this position is selected
                    if (line_num > sel_start_line .and. line_num < sel_end_line) then
                        ! Fully selected line (between start and end)
                        in_selection = .true.
                        exit
                    else if (line_num == sel_start_line .and. line_num == sel_end_line) then
                        ! Single-line selection
                        if (col >= sel_start_col .and. col < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_start_line .and. line_num < sel_end_line) then
                        ! First line of multi-line selection
                        if (col >= sel_start_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_end_line .and. line_num > sel_start_line) then
                        ! Last line of multi-line selection
                        if (col < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    end if
                end if
            end do

            ! Check if this position is a bracket or its match
            if ((line_num == bracket_line .and. col == bracket_col) .or. &
                (line_num == matching_bracket_line .and. col == matching_bracket_col)) then
                is_bracket_match = .true.
            end if

            ! Render character with or without highlighting
            if (col <= line_len) then
                ch = line(col:col)
            else
                ch = ' '
            end if

            if (in_selection) then
                ! Highlight selected text with reverse video
                call terminal_write(char(27) // '[7m' // ch // char(27) // '[0m')
            else if (is_bracket_match) then
                ! Highlight matching brackets with cyan background
                call terminal_write(char(27) // '[46m' // ch // char(27) // '[0m')
            else if (is_current_line) then
                ! Subtle background for current line (dark gray)
                call terminal_write(char(27) // '[48;5;236m' // ch // char(27) // '[0m')
            else
                call terminal_write(ch)
            end if
        end do

        ! Fill remaining width with spaces
        do col = max(line_len + 1, start_col), start_col + width - 1
            in_selection = .false.

            ! Check if end of line position is in selection
            do i = 1, size(editor%cursors)
                if (editor%cursors(i)%has_selection) then
                    ! Determine selection bounds (handle both directions)
                    if (editor%cursors(i)%line < editor%cursors(i)%selection_start_line .or. &
                        (editor%cursors(i)%line == editor%cursors(i)%selection_start_line .and. &
                         editor%cursors(i)%column < editor%cursors(i)%selection_start_col)) then
                        sel_start_line = editor%cursors(i)%line
                        sel_start_col = editor%cursors(i)%column
                        sel_end_line = editor%cursors(i)%selection_start_line
                        sel_end_col = editor%cursors(i)%selection_start_col
                    else
                        sel_start_line = editor%cursors(i)%selection_start_line
                        sel_start_col = editor%cursors(i)%selection_start_col
                        sel_end_line = editor%cursors(i)%line
                        sel_end_col = editor%cursors(i)%column
                    end if

                    ! Check if this position is selected (multi-line aware)
                    if (line_num > sel_start_line .and. line_num < sel_end_line) then
                        ! Fully selected line
                        in_selection = .true.
                        exit
                    else if (line_num == sel_start_line .and. line_num == sel_end_line) then
                        ! Single-line selection
                        if (col >= sel_start_col .and. col < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_start_line .and. line_num < sel_end_line) then
                        ! First line of multi-line selection
                        if (col >= sel_start_col) then
                            in_selection = .true.
                            exit
                        end if
                    else if (line_num == sel_end_line .and. line_num > sel_start_line) then
                        ! Last line of multi-line selection
                        if (col < sel_end_col) then
                            in_selection = .true.
                            exit
                        end if
                    end if
                end if
            end do

            if (in_selection) then
                call terminal_write(char(27) // '[7m ' // char(27) // '[0m')
            else if (is_current_line) then
                call terminal_write(char(27) // '[48;5;236m ' // char(27) // '[0m')
            else
                call terminal_write(' ')
            end if
        end do

        if (allocated(line)) deallocate(line)
    end subroutine render_line_with_selections

    subroutine render_status_bar(editor, buffer)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        character(len=256) :: status_left, status_center, status_right, status_bar
        integer :: padding_len, left_pad, right_pad
        type(cursor_t) :: cursor

        cursor = editor%cursors(editor%active_cursor)

        ! Move to status bar position
        call terminal_move_cursor(editor%screen_rows, 1)

        ! Prepare status bar content
        if (allocated(editor%filename)) then
            write(status_left, '(a,a,a,a)') ' ctrl-b:fuss | ', trim(editor%filename), &
                   merge(' [modified]', '           ', buffer%modified), ' '
        else
            write(status_left, '(a,a,a)') ' ctrl-b:fuss | [No Name]', &
                   merge(' [modified]', '           ', buffer%modified), ' '
        end if

        ! Add help hint in center
        status_center = 'ctrl-/:help'

        if (size(editor%cursors) > 1) then
            write(status_right, '(a,i0,a,a,i0,a,i0,a)') '[', size(editor%cursors), ' cursors] ', &
                   'Ln ', cursor%line, ', Col ', cursor%column, ' '
        else
            write(status_right, '(a,i0,a,i0,a)') 'Ln ', cursor%line, ', Col ', cursor%column, ' '
        end if

        ! Create full status bar with center text
        padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_center) - len_trim(status_right)
        if (padding_len > 0) then
            ! Distribute padding around center text
            left_pad = padding_len / 2
            right_pad = padding_len - left_pad
            status_bar = trim(status_left) // repeat(' ', left_pad) // &
                        trim(status_center) // repeat(' ', right_pad) // trim(status_right)
        else
            ! Not enough space, just show left and right
            padding_len = editor%screen_cols - len_trim(status_left) - len_trim(status_right)
            if (padding_len > 0) then
                status_bar = trim(status_left) // repeat(' ', padding_len) // trim(status_right)
            else
                status_bar = status_left(1:editor%screen_cols)
            end if
        end if

        ! Render with inverse video
        call terminal_write(char(27) // '[7m')  ! Inverse video
        call terminal_write(status_bar(1:editor%screen_cols))
        call terminal_write(char(27) // '[0m')  ! Reset attributes
    end subroutine render_status_bar

    subroutine render_cursor(editor)
        type(editor_state_t), intent(in) :: editor
        type(cursor_t) :: cursor
        integer :: screen_row, screen_col
        integer :: i
        integer :: col_offset, row_offset, min_row

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Account for tab bar offset - when tabs exist, row 1 is tab bar, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2  ! Tab bar takes row 1
            min_row = 2     ! Cursor cannot be in row 1 (tab bar)
        else
            row_offset = 1  ! No tab bar
            min_row = 1     ! Cursor can be in row 1
        end if

        ! For multiple cursors, show them all with block cursor for inactive ones
        if (size(editor%cursors) > 1) then
            ! First draw all inactive cursors
            do i = 1, size(editor%cursors)
                if (i /= editor%active_cursor) then
                    cursor = editor%cursors(i)

                    ! Calculate screen position from buffer position
                    screen_row = cursor%line - editor%viewport_line + row_offset
                    screen_col = cursor%column - editor%viewport_column + 1 + col_offset

                    ! Ensure cursor is within screen bounds and not in tab bar
                    if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                        screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                        ! Inactive cursor - draw with reverse video block
                        call terminal_move_cursor(screen_row, screen_col)
                        call terminal_write(char(27) // '[7m ')  ! Inverse video space
                        call terminal_write(char(27) // '[0m')   ! Reset
                    end if
                end if
            end do

            ! Then position terminal cursor at active cursor location
            cursor = editor%cursors(editor%active_cursor)
            screen_row = cursor%line - editor%viewport_line + row_offset
            screen_col = cursor%column - editor%viewport_column + 1 + col_offset

            if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                call terminal_move_cursor(screen_row, screen_col)
                call terminal_show_cursor()
            end if
        else
            ! Single cursor mode
            cursor = editor%cursors(editor%active_cursor)

            ! Calculate screen position from buffer position
            screen_row = cursor%line - editor%viewport_line + row_offset
            screen_col = cursor%column - editor%viewport_column + 1 + col_offset

            ! Ensure cursor is within screen bounds and not in tab bar
            if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
                screen_col >= 1 .and. screen_col <= editor%screen_cols) then
                call terminal_move_cursor(screen_row, screen_col)
                call terminal_show_cursor()
            end if
        end if
    end subroutine render_cursor

    subroutine update_viewport(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t) :: cursor
        integer :: margin = 3  ! Lines to keep visible above/below cursor
        integer :: tab_idx, pane_idx
        integer :: pane_height, pane_width
        integer :: screen_width, screen_height

        ! If we have panes, update the active pane's viewport
        if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
            tab_idx = editor%active_tab_index
            if (allocated(editor%tabs(tab_idx)%panes)) then
                pane_idx = editor%tabs(tab_idx)%active_pane_index
                if (pane_idx > 0 .and. pane_idx <= size(editor%tabs(tab_idx)%panes)) then

                    if (allocated(editor%tabs(tab_idx)%panes(pane_idx)%cursors) .and. &
                        editor%tabs(tab_idx)%panes(pane_idx)%active_cursor > 0) then
                        cursor = editor%tabs(tab_idx)%panes(pane_idx)%cursors(&
                                 editor%tabs(tab_idx)%panes(pane_idx)%active_cursor)

                        ! Calculate pane dimensions
                        screen_width = editor%screen_cols
                        screen_height = editor%screen_rows - 3
                        pane_height = int((editor%tabs(tab_idx)%panes(pane_idx)%y_end - &
                                          editor%tabs(tab_idx)%panes(pane_idx)%y_start) * real(screen_height))
                        pane_width = int((editor%tabs(tab_idx)%panes(pane_idx)%x_end - &
                                         editor%tabs(tab_idx)%panes(pane_idx)%x_start) * real(screen_width))

                        ! Account for line numbers in pane width
                        if (show_line_numbers) then
                            pane_width = pane_width - LINE_NUMBER_WIDTH - 1
                        end if

                        ! Vertical scrolling for pane
                        if (cursor%line < editor%tabs(tab_idx)%panes(pane_idx)%viewport_line + margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = max(1, cursor%line - margin)
                        else if (cursor%line > editor%tabs(tab_idx)%panes(pane_idx)%viewport_line + pane_height - margin - 1) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_line = cursor%line - pane_height + margin + 1
                        end if

                        ! Horizontal scrolling for pane
                        if (cursor%column < editor%tabs(tab_idx)%panes(pane_idx)%viewport_column + margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = max(1, cursor%column - margin)
                        else if (cursor%column > editor%tabs(tab_idx)%panes(pane_idx)%viewport_column + pane_width - margin) then
                            editor%tabs(tab_idx)%panes(pane_idx)%viewport_column = cursor%column - pane_width + margin
                        end if

                        ! Also update legacy editor viewport for compatibility
                        editor%viewport_line = editor%tabs(tab_idx)%panes(pane_idx)%viewport_line
                        editor%viewport_column = editor%tabs(tab_idx)%panes(pane_idx)%viewport_column
                    end if
                    return
                end if
            end if
        end if

        ! Fallback to original behavior if no panes
        cursor = editor%cursors(editor%active_cursor)

        ! Vertical scrolling
        if (cursor%line < editor%viewport_line + margin) then
            editor%viewport_line = max(1, cursor%line - margin)
        else if (cursor%line > editor%viewport_line + editor%screen_rows - margin - 2) then
            ! -2 for status bar and margin
            editor%viewport_line = cursor%line - editor%screen_rows + margin + 2
        end if

        ! Horizontal scrolling
        if (cursor%column < editor%viewport_column + margin) then
            editor%viewport_column = max(1, cursor%column - margin)
        else if (cursor%column > editor%viewport_column + editor%screen_cols - margin) then
            editor%viewport_column = cursor%column - editor%screen_cols + margin
        end if
    end subroutine update_viewport

    ! Render screen with split panes (tree on left, editor on right)
    subroutine render_screen_with_tree(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer :: tree_width, editor_start_col, editor_width
        integer :: separator_col
        integer :: row

        call terminal_hide_cursor()

        ! Clear screen first to avoid artifacts
        do row = 1, editor%screen_rows
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat(' ', editor%screen_cols))
        end do

        ! Calculate split: 30% for tree, 70% for editor
        tree_width = editor%screen_cols * 30 / 100
        separator_col = tree_width + 1
        editor_start_col = tree_width + 2
        editor_width = editor%screen_cols - editor_start_col + 1

        ! Render tab bar if there are any tabs (positioned in editor pane area)
        call render_tab_bar(editor, editor_start_col, editor_width)

        ! Render file tree in left pane (start at row 2 for tab bar)
        call render_file_tree(tree_state, 2, editor%screen_rows - 1, 2, tree_width - 2, editor%fuss_hints_expanded)

        ! Render vertical separator (start at row 2 for tab bar)
        call render_vertical_separator(separator_col, 2, editor%screen_rows - 1)

        ! Render editor in right pane
        call render_editor_pane(buffer, editor, editor_start_col, editor_width)

        ! Render status bar (full width)
        call render_status_bar(editor, buffer)

        ! Position cursor in editor pane
        call render_cursor_in_pane(editor, editor_start_col, editor_width)

        call terminal_show_cursor()
    end subroutine render_screen_with_tree

    subroutine render_vertical_separator(col, start_row, end_row)
        integer, intent(in) :: col, start_row, end_row
        integer :: row

        do row = start_row, end_row
            call terminal_move_cursor(row, col)
            call terminal_write(char(27) // '[90m│' // char(27) // '[0m')  ! Gray vertical line
        end do
    end subroutine render_vertical_separator

    subroutine render_editor_pane(buffer, editor, start_col, width)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: start_col, width
        integer :: screen_row, buffer_line, line_count
        integer :: adjusted_width, line_num_width
        integer :: start_row
        character(len=16) :: line_num_str
        character(len=:), allocatable :: padding

        line_count = buffer_get_line_count(buffer)

        ! Calculate content width (accounting for line numbers if enabled)
        if (show_line_numbers) then
            line_num_width = LINE_NUMBER_WIDTH + 1
            adjusted_width = width - line_num_width
        else
            line_num_width = 0
            adjusted_width = width
        end if

        ! Determine starting row (account for tab bar)
        if (size(editor%tabs) > 0) then
            start_row = 2  ! Tab bar at row 1
        else
            start_row = 1  ! No tab bar
        end if

        ! Render each visible line in the editor pane
        do screen_row = start_row, editor%screen_rows - 1
            buffer_line = editor%viewport_line + screen_row - start_row

            ! Position cursor at start of this line in the pane
            call terminal_move_cursor(screen_row, start_col)

            ! Render line number if enabled
            if (show_line_numbers) then
                if (buffer_line <= line_count) then
                    write(line_num_str, '(i5)') buffer_line
                    if (buffer_line == editor%cursors(editor%active_cursor)%line) then
                        call terminal_write(char(27) // '[1;33m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                          // char(27) // '[0m ')
                    else
                        call terminal_write(char(27) // '[90m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                          // char(27) // '[0m ')
                    end if
                else
                    call terminal_write(repeat(' ', line_num_width))
                end if
            end if

            ! Render content (render_line_with_selections will write exactly adjusted_width chars)
            if (buffer_line <= line_count) then
                call render_line_with_selections(buffer, editor, buffer_line, &
                                                editor%viewport_column, adjusted_width)
            else
                ! Empty line beyond file content
                padding = '~' // repeat(' ', adjusted_width - 1)
                call terminal_write(padding)
            end if
        end do
    end subroutine render_editor_pane

    subroutine render_all_panes(buffer, editor)
        use editor_state_module, only: pane_t
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        type(pane_t) :: pane
        integer :: i, tab_idx, n_panes
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_width, screen_height

        ! Get active tab
        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        n_panes = size(editor%tabs(tab_idx)%panes)
        if (n_panes == 0) return

        ! Get screen dimensions
        screen_width = editor%screen_cols
        screen_height = editor%screen_rows - 3  ! Account for tab bar, status bar

        ! If only one pane, render full screen
        if (n_panes == 1) then
            call render_editor_pane(buffer, editor, 1, screen_width)
            return
        end if

        ! Clear the editor area first with background
        do i = 2, editor%screen_rows - 1
            call terminal_move_cursor(i, 1)
            call terminal_write(repeat(' ', screen_width))
        end do

        ! Render each pane with gaps
        do i = 1, n_panes
            pane = editor%tabs(tab_idx)%panes(i)

            ! Calculate actual screen coordinates with gap consideration
            ! Add 1 column gap on right side of each pane except the last
            pane_col = 1 + int(pane%x_start * real(screen_width))
            if (i < n_panes) then
                ! Reserve 1 column for the border/gap
                pane_width = int((pane%x_end - pane%x_start) * real(screen_width)) - 1
            else
                ! Last pane uses full width
                pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
            end if
            pane_row = 2 + int(pane%y_start * real(screen_height))
            pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

            ! Render the pane content
            call render_single_pane(buffer, editor, i, pane_col, pane_row, pane_width, pane_height)

            ! Draw vertical separator between panes
            if (i < n_panes) then
                call render_pane_separator(pane_col + pane_width, pane_row, pane_height)
            end if
        end do
    end subroutine render_all_panes

    subroutine render_single_pane(buffer, editor, pane_idx, col, row, width, height)
        use editor_state_module, only: pane_t
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_idx, col, row, width, height
        type(pane_t) :: pane
        integer :: screen_row, buffer_line, content_start_row, content_height
        integer :: tab_idx

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(pane_idx)

        ! No offset needed, use full height
        content_start_row = row
        content_height = height

        ! Clear the pane area with subtle background for inactive panes
        do screen_row = content_start_row, content_start_row + content_height - 1
            call terminal_move_cursor(screen_row, col)
            if (.not. pane%is_active) then
                ! Subtle dark background for inactive panes
                call terminal_write(char(27) // '[48;5;234m')  ! Very dark gray
            end if
            call terminal_write(repeat(' ', width))
            call terminal_write(char(27) // '[0m')
        end do

        ! Render buffer content with pane's viewport
        do screen_row = content_start_row, content_start_row + content_height - 1
            buffer_line = pane%viewport_line + (screen_row - content_start_row)
            if (buffer_line > 0 .and. buffer_line <= buffer_get_line_count(buffer)) then
                call render_buffer_line_in_pane(buffer, editor, pane_idx, buffer_line, &
                                               screen_row, col, width)
            else
                ! Render empty line indicator for lines beyond file
                call terminal_move_cursor(screen_row, col)

                ! Render empty line number area if line numbers are enabled
                if (show_line_numbers) then
                    if (.not. pane%is_active) then
                        call terminal_write(char(27) // '[48;5;234m')  ! Dark gray for inactive
                    end if
                    call terminal_write(repeat(' ', LINE_NUMBER_WIDTH + 1))
                end if

                ! Render the ~ indicator
                if (.not. pane%is_active) then
                    call terminal_write(char(27) // '[48;5;234m')  ! Dark gray for inactive
                end if
                call terminal_write('~')

                ! Calculate remaining width accounting for line numbers
                if (show_line_numbers) then
                    if (width > LINE_NUMBER_WIDTH + 2) then
                        call terminal_write(repeat(' ', width - LINE_NUMBER_WIDTH - 2))
                    end if
                else
                    if (width > 1) then
                        call terminal_write(repeat(' ', width - 1))
                    end if
                end if
                call terminal_write(char(27) // '[0m')
            end if
        end do
    end subroutine render_single_pane

    subroutine render_buffer_line_in_pane(buffer, editor, pane_idx, line_num, screen_row, col, width)
        use editor_state_module, only: pane_t
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_idx, line_num, screen_row, col, width
        character(len=:), allocatable :: line
        type(pane_t) :: pane
        integer :: tab_idx, start_col, end_col, i
        integer :: content_width, content_col
        character(len=5) :: line_num_str
        logical :: is_current_line

        tab_idx = editor%active_tab_index
        pane = editor%tabs(tab_idx)%panes(pane_idx)

        ! Move to position
        call terminal_move_cursor(screen_row, col)

        ! Render line number if enabled
        if (show_line_numbers) then
            write(line_num_str, '(i5)') line_num
            ! Check if this line has any cursor
            is_current_line = .false.
            if (allocated(pane%cursors)) then
                do i = 1, size(pane%cursors)
                    if (pane%cursors(i)%line == line_num) then
                        is_current_line = .true.
                        exit
                    end if
                end do
            end if

            ! Apply pane background for inactive panes
            if (.not. pane%is_active) then
                call terminal_write(char(27) // '[48;5;234m')  ! Dark gray background
            end if

            if (is_current_line .and. pane%is_active) then
                call terminal_write(char(27) // '[1;33m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                  // char(27) // '[0m ')
            else
                call terminal_write(char(27) // '[90m' // adjustl(line_num_str(1:LINE_NUMBER_WIDTH)) &
                                  // char(27) // '[0m ')
            end if

            ! Continue with pane background for content
            if (.not. pane%is_active) then
                call terminal_write(char(27) // '[48;5;234m')  ! Dark gray background
            end if

            content_width = width - LINE_NUMBER_WIDTH - 1
            content_col = col + LINE_NUMBER_WIDTH + 1
        else
            content_width = width
            content_col = col
        end if

        ! Get the line content
        line = buffer_get_line(buffer, line_num)
        if (.not. allocated(line)) return

        ! Calculate visible portion based on horizontal scroll
        start_col = pane%viewport_column
        end_col = min(start_col + content_width - 1, len(line))

        ! Check if this is the current line with a cursor
        is_current_line = .false.
        if (allocated(pane%cursors)) then
            do i = 1, size(pane%cursors)
                if (pane%cursors(i)%line == line_num) then
                    is_current_line = .true.
                    exit
                end if
            end do
        end if

        ! Set background based on pane state
        if (pane%is_active) then
            if (is_current_line) then
                ! Highlight current line in active pane
                call terminal_write(char(27) // '[48;5;237m')  ! Slightly brighter gray
            end if
        else
            ! Inactive pane gets subtle dark background
            call terminal_write(char(27) // '[48;5;234m')  ! Very dark gray
        end if

        ! Render the visible portion of the line
        if (start_col <= len(line)) then
            call terminal_write(line(start_col:min(end_col, len(line))))
        end if

        ! Fill remaining width with the same background
        if (end_col - start_col + 1 < content_width) then
            call terminal_write(repeat(' ', content_width - (end_col - start_col + 1)))
        end if

        ! Reset attributes
        call terminal_write(char(27) // '[0m')
    end subroutine render_buffer_line_in_pane

    subroutine render_pane_separator(col, start_row, height)
        integer, intent(in) :: col, start_row, height
        integer :: row

        ! Draw vertical separator with distinct visual
        do row = start_row, start_row + height - 1
            call terminal_move_cursor(row, col)
            ! Use reverse video for a solid separator
            call terminal_write(char(27) // '[7m ')  ! Reverse video space
            call terminal_write(char(27) // '[0m')   ! Reset
        end do
    end subroutine render_pane_separator

    subroutine render_cursor_for_panes(editor)
        use editor_state_module, only: pane_t
        type(editor_state_t), intent(in) :: editor
        type(pane_t) :: pane
        type(cursor_t) :: cursor
        integer :: tab_idx, pane_idx
        integer :: pane_col, pane_row, pane_width, pane_height
        integer :: screen_row, screen_col
        integer :: screen_width, screen_height
        integer :: col_offset

        tab_idx = editor%active_tab_index
        if (tab_idx < 1 .or. tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return

        pane_idx = editor%tabs(tab_idx)%active_pane_index
        if (pane_idx < 1 .or. pane_idx > size(editor%tabs(tab_idx)%panes)) return

        pane = editor%tabs(tab_idx)%panes(pane_idx)
        if (.not. allocated(pane%cursors)) return
        if (pane%active_cursor < 1 .or. pane%active_cursor > size(pane%cursors)) return

        cursor = pane%cursors(pane%active_cursor)

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Calculate pane screen coordinates
        screen_width = editor%screen_cols
        screen_height = editor%screen_rows - 3  ! Account for tab bar, status bar

        pane_col = 1 + int(pane%x_start * real(screen_width))
        pane_width = int((pane%x_end - pane%x_start) * real(screen_width))
        if (pane_idx < size(editor%tabs(tab_idx)%panes)) then
            pane_width = pane_width - 1  ! Reserve space for separator
        end if
        pane_row = 2 + int(pane%y_start * real(screen_height))
        pane_height = int((pane%y_end - pane%y_start) * real(screen_height))

        ! Calculate cursor position within the pane, accounting for line numbers
        screen_row = pane_row + (cursor%line - pane%viewport_line)
        screen_col = pane_col + col_offset + (cursor%column - pane%viewport_column)

        ! Ensure cursor is within pane boundaries
        if (screen_row >= pane_row .and. screen_row < pane_row + pane_height .and. &
            screen_col >= pane_col + col_offset .and. screen_col < pane_col + pane_width) then
            call terminal_move_cursor(screen_row, screen_col)
        else
            ! Cursor is out of view, position at top-left of pane content area
            call terminal_move_cursor(pane_row, pane_col + col_offset)
        end if

        ! Always show cursor
        call terminal_show_cursor()
    end subroutine render_cursor_for_panes

    subroutine render_cursor_in_pane(editor, pane_start_col, pane_width)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: pane_start_col, pane_width
        type(cursor_t) :: cursor
        integer :: screen_row, screen_col, col_offset, row_offset, min_row

        ! Calculate column offset for line numbers
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1
        else
            col_offset = 0
        end if

        ! Account for tab bar offset - when tabs exist, row 1 is tab bar, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2  ! Tab bar takes row 1
            min_row = 2     ! Cursor cannot be in row 1 (tab bar)
        else
            row_offset = 1  ! No tab bar
            min_row = 1     ! Cursor can be in row 1
        end if

        cursor = editor%cursors(editor%active_cursor)

        ! Calculate screen position within the editor pane
        screen_row = cursor%line - editor%viewport_line + row_offset
        screen_col = pane_start_col + col_offset + cursor%column - editor%viewport_column

        ! Ensure cursor is within pane bounds and not in tab bar
        if (screen_row >= min_row .and. screen_row < editor%screen_rows .and. &
            screen_col >= pane_start_col .and. screen_col <= pane_start_col + pane_width) then
            call terminal_move_cursor(screen_row, screen_col)
            call terminal_show_cursor()
        end if
    end subroutine render_cursor_in_pane

    ! Render tab bar at top of screen
    ! Optional start_col and width parameters for positioning in split view
    subroutine render_tab_bar(editor, start_col, width)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in), optional :: start_col, width
        integer :: i, col, tab_count
        character(len=:), allocatable :: tab_label, filename_only
        character(len=256) :: temp_label
        integer :: slash_pos, last_slash
        character(len=1) :: modified_marker
        integer :: start_column, max_width

        tab_count = size(editor%tabs)
        if (tab_count == 0) return  ! No tabs to display

        ! Use provided start_col and width, or default to full screen
        if (present(start_col)) then
            start_column = start_col
        else
            start_column = 1
        end if

        if (present(width)) then
            max_width = width
        else
            max_width = editor%screen_cols
        end if

        ! Move to top row at starting column and clear the tab bar area
        call terminal_move_cursor(1, start_column)
        call terminal_write(repeat(' ', max_width))

        ! Render each tab
        col = start_column
        do i = 1, tab_count
            ! Extract filename from full path
            filename_only = editor%tabs(i)%filename
            last_slash = 0
            do slash_pos = len(editor%tabs(i)%filename), 1, -1
                if (editor%tabs(i)%filename(slash_pos:slash_pos) == '/') then
                    last_slash = slash_pos
                    exit
                end if
            end do
            if (last_slash > 0 .and. last_slash < len(editor%tabs(i)%filename)) then
                filename_only = editor%tabs(i)%filename(last_slash+1:)
            end if

            ! Add modified marker
            if (editor%tabs(i)%modified) then
                modified_marker = '*'
            else
                modified_marker = ' '
            end if

            ! Build tab label: [1: file.txt*]
            write(temp_label, '(A,I0,A,A,A,A)') '[', i, ': ', trim(filename_only), modified_marker, ']'
            tab_label = trim(temp_label)

            ! Check if we have room for this tab
            if (col + len(tab_label) > start_column + max_width) exit

            ! Position cursor
            call terminal_move_cursor(1, col)

            ! Highlight active tab
            if (i == editor%active_tab_index) then
                call terminal_write(char(27) // '[7m')  ! Reverse video
            end if

            call terminal_write(tab_label)

            if (i == editor%active_tab_index) then
                call terminal_write(char(27) // '[0m')  ! Reset
            end if

            col = col + len(tab_label) + 1  ! +1 for space between tabs
        end do
    end subroutine render_tab_bar

end module renderer_module