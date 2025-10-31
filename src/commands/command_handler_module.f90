module command_handler_module
    use iso_fortran_env, only: int32, error_unit
    use iso_c_binding, only: c_int
    use editor_state_module, only: editor_state_t, cursor_t, switch_to_tab_with_buffer, close_tab, create_tab
    use text_buffer_module
    use renderer_module, only: update_viewport, render_screen, tree_state
    use yank_stack_module
    use clipboard_module
    use help_display_module, only: show_help
    use goto_prompt_module, only: show_goto_prompt
    use search_prompt_module, only: show_search_prompt, search_forward, search_backward, &
                                     current_search_pattern
    use replace_prompt_module, only: show_replace_prompt
    use undo_stack_module
    use terminal_io_module, only: terminal_move_cursor, terminal_write, terminal_clear_screen
    use bracket_matching_module, only: find_matching_bracket
    use file_tree_module
    use git_ops_module
    use text_prompt_module, only: show_text_prompt
    implicit none
    private

    public :: handle_key_command, init_command_handler, cleanup_command_handler
    public :: save_initial_state_for_undo

    type(yank_stack_t) :: yank_stack
    type(undo_stack_t) :: undo_stack
    character(len=:), allocatable :: search_pattern  ! For ctrl-d functionality
    logical :: last_action_was_edit = .false.

contains

    subroutine init_command_handler()
        call init_yank_stack(yank_stack)
        call init_undo_stack(undo_stack)
        last_action_was_edit = .false.
    end subroutine init_command_handler

    subroutine save_initial_state_for_undo(buffer, editor)
        use undo_stack_module, only: save_initial_undo_state
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor
        call save_initial_undo_state(undo_stack, buffer, editor%cursors(editor%active_cursor))
    end subroutine save_initial_state_for_undo

    subroutine cleanup_command_handler()
        call cleanup_yank_stack(yank_stack)
        call cleanup_undo_stack(undo_stack)
        if (allocated(search_pattern)) deallocate(search_pattern)
    end subroutine cleanup_command_handler

    subroutine save_undo_state(buffer, editor)
        type(buffer_t), intent(in) :: buffer
        type(editor_state_t), intent(in) :: editor

        ! Save current state to undo stack
        call push_undo_state(undo_stack, buffer, editor%cursors(editor%active_cursor))
    end subroutine save_undo_state

    subroutine handle_key_command(key_str, editor, buffer, should_quit)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(out) :: should_quit
        integer :: line_count, i, j, insert_line
        logical :: is_edit_action
        type(cursor_t), allocatable :: new_cursors(:)
        integer, allocatable :: original_lines(:)
        character(len=:), allocatable :: line

        should_quit = .false.
        line_count = buffer_get_line_count(buffer)
        is_edit_action = .false.

        ! Ignore empty key strings (from terminal position reports, etc)
        if (len_trim(key_str) == 0 .and. key_str(1:1) /= ' ') then
            return
        end if

        ! Route input when in fuss mode (except ctrl-b and ctrl-q which work in both modes)
        if (editor%fuss_mode_active .and. trim(key_str) /= 'ctrl-b' .and. trim(key_str) /= 'ctrl-q') then
            call handle_fuss_input(key_str, editor, buffer)
            return
        end if

        select case(trim(key_str))
        ! File operations
        case('ctrl-q')
            should_quit = .true.

        case('ctrl-b')
            ! Toggle fuss mode (file tree)
            call toggle_fuss_mode(editor)

        case('esc')
            ! ESC - Clear selections and return to single cursor mode
            if (size(editor%cursors) > 1) then
                ! Keep only the active cursor
                allocate(new_cursors(1))
                new_cursors(1) = editor%cursors(editor%active_cursor)
                new_cursors(1)%has_selection = .false.
                deallocate(editor%cursors)
                editor%cursors = new_cursors
                editor%active_cursor = 1
            else
                ! Single cursor - just clear selection
                editor%cursors(editor%active_cursor)%has_selection = .false.
            end if

        case('ctrl-?', 'ctrl-/')
            ! Show help menu
            ! ctrl-?: Standard (Ctrl+Shift+/)
            ! ctrl-/: Alternative (Ctrl+/)
            call show_help(editor)
            ! Screen will be redrawn automatically by main loop

        case('ctrl-g')
            ! Go to line:column
            call show_goto_prompt(editor, buffer)
            call update_viewport(editor)

        case('ctrl-l')
            ! Clear and redraw screen
            call terminal_clear_screen()
            ! Screen will be redrawn automatically by main loop

        ! Undo/Redo
        case('ctrl-z')
            ! Undo
            if (can_undo(undo_stack)) then
                call perform_undo(undo_stack, buffer, editor%cursors(editor%active_cursor))
                ! If we have multiple cursors, reset to single cursor mode
                ! (Undo only tracks one cursor's state)
                if (size(editor%cursors) > 1) then
                    allocate(new_cursors(1))
                    new_cursors(1) = editor%cursors(editor%active_cursor)
                    ! Clamp cursor to actual line length after undo
                    line = buffer_get_line(buffer, new_cursors(1)%line)
                    if (new_cursors(1)%column > len(line) + 1) then
                        new_cursors(1)%column = len(line) + 1
                    end if
                    new_cursors(1)%desired_column = new_cursors(1)%column
                    if (allocated(line)) deallocate(line)
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = 1
                end if
                call update_viewport(editor)
            end if

        case('ctrl-shift-z', 'ctrl-]')
            ! Redo
            ! ctrl-shift-z: Standard redo (WezTerm may intercept - disable in config)
            ! ctrl-]: Alternative redo binding
            if (can_redo(undo_stack)) then
                call perform_redo(undo_stack, buffer, editor%cursors(editor%active_cursor))
                ! If we have multiple cursors, reset to single cursor mode
                ! (Undo only tracks one cursor's state)
                if (size(editor%cursors) > 1) then
                    allocate(new_cursors(1))
                    new_cursors(1) = editor%cursors(editor%active_cursor)
                    deallocate(editor%cursors)
                    editor%cursors = new_cursors
                    editor%active_cursor = 1
                end if
                call update_viewport(editor)
            end if

        case('ctrl-y')
            ! Yank (paste from yank stack)
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call yank_text(editor%cursors(editor%active_cursor), buffer)
            is_edit_action = .true.

        ! Navigation
        case('up')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_up(editor%cursors(i), buffer, line_count)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_up(editor%cursors(editor%active_cursor), buffer, line_count)
            end if
            call update_viewport(editor)

        case('down')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_down(editor%cursors(i), buffer, line_count)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_down(editor%cursors(editor%active_cursor), buffer, line_count)
            end if
            call update_viewport(editor)

        case('left')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_left(editor%cursors(i), buffer)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_left(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        case('right')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_right(editor%cursors(i), buffer)
                end do
                ! Remove duplicate cursors that ended up at same position
                call deduplicate_cursors(editor)
            else
                call move_cursor_right(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        ! Selection with shift+motion
        case('shift-up')
            call extend_selection_up(editor%cursors(editor%active_cursor), buffer, line_count)
            call update_viewport(editor)

        case('shift-down')
            call extend_selection_down(editor%cursors(editor%active_cursor), buffer, line_count)
            call update_viewport(editor)

        case('shift-left')
            call extend_selection_left(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('shift-right')
            call extend_selection_right(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('home', 'ctrl-a')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_smart_home(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_smart_home(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        case('end', 'ctrl-e')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_end(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        case('shift-home', 'ctrl-shift-a')
            call extend_selection_home(editor%cursors(editor%active_cursor))
            call update_viewport(editor)

        case('shift-end', 'ctrl-shift-e')
            call extend_selection_end(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('pageup')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_page_up(editor%cursors(i), editor, line_count)
                end do
            else
                call move_cursor_page_up(editor%cursors(editor%active_cursor), editor, line_count)
            end if
            call update_viewport(editor)

        case('pagedown')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_page_down(editor%cursors(i), editor, line_count)
                end do
            else
                call move_cursor_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            end if
            call update_viewport(editor)

        case('mouse-scroll-up')
            ! Scroll viewport up by 3 lines (don't move cursor)
            editor%viewport_line = max(1, editor%viewport_line - 3)

        case('mouse-scroll-down')
            ! Scroll viewport down by 3 lines (don't move cursor)
            editor%viewport_line = min(max(1, line_count - editor%screen_rows + 2), &
                                      editor%viewport_line + 3)

        case('ctrl-home')
            ! Jump to beginning of file
            editor%cursors(editor%active_cursor)%line = 1
            editor%cursors(editor%active_cursor)%column = 1
            editor%cursors(editor%active_cursor)%desired_column = 1
            editor%cursors(editor%active_cursor)%has_selection = .false.
            call update_viewport(editor)

        case('ctrl-end')
            ! Jump to end of file
            line_count = buffer_get_line_count(buffer)
            editor%cursors(editor%active_cursor)%line = line_count
            call move_cursor_end(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('shift-pageup')
            call extend_selection_page_up(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        case('shift-pagedown')
            call extend_selection_page_down(editor%cursors(editor%active_cursor), editor, line_count)
            call update_viewport(editor)

        case('alt-left')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_word_left(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_word_left(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        case('alt-right')
            if (size(editor%cursors) > 1) then
                ! Move all cursors
                do i = 1, size(editor%cursors)
                    call move_cursor_word_right(editor%cursors(i), buffer)
                end do
            else
                call move_cursor_word_right(editor%cursors(editor%active_cursor), buffer)
            end if
            call update_viewport(editor)

        case('alt-shift-left')
            call extend_selection_word_left(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-shift-right')
            call extend_selection_word_right(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)

        case('alt-up')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call move_line_up(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-down')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call move_line_down(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-shift-up')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call duplicate_line_up(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        case('alt-shift-down')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call duplicate_line_down(editor%cursors(editor%active_cursor), buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        ! Tab navigation
        case('alt-1')
            if (size(editor%tabs) >= 1) call switch_to_tab_with_buffer(editor, 1, buffer)
        case('alt-2')
            if (size(editor%tabs) >= 2) call switch_to_tab_with_buffer(editor, 2, buffer)
        case('alt-3')
            if (size(editor%tabs) >= 3) call switch_to_tab_with_buffer(editor, 3, buffer)
        case('alt-4')
            if (size(editor%tabs) >= 4) call switch_to_tab_with_buffer(editor, 4, buffer)
        case('alt-5')
            if (size(editor%tabs) >= 5) call switch_to_tab_with_buffer(editor, 5, buffer)
        case('alt-6')
            if (size(editor%tabs) >= 6) call switch_to_tab_with_buffer(editor, 6, buffer)
        case('alt-7')
            if (size(editor%tabs) >= 7) call switch_to_tab_with_buffer(editor, 7, buffer)
        case('alt-8')
            if (size(editor%tabs) >= 8) call switch_to_tab_with_buffer(editor, 8, buffer)
        case('alt-9')
            if (size(editor%tabs) >= 9) call switch_to_tab_with_buffer(editor, 9, buffer)
        case('alt-0')
            if (size(editor%tabs) >= 10) call switch_to_tab_with_buffer(editor, 10, buffer)

        case('ctrl-alt-left')
            ! Previous tab
            if (size(editor%tabs) > 0) then
                if (editor%active_tab_index > 1) then
                    call switch_to_tab_with_buffer(editor, editor%active_tab_index - 1, buffer)
                else
                    call switch_to_tab_with_buffer(editor, size(editor%tabs), buffer)  ! Wrap to last tab
                end if
            end if

        case('ctrl-alt-right')
            ! Next tab
            if (size(editor%tabs) > 0) then
                if (editor%active_tab_index < size(editor%tabs)) then
                    call switch_to_tab_with_buffer(editor, editor%active_tab_index + 1, buffer)
                else
                    call switch_to_tab_with_buffer(editor, 1, buffer)  ! Wrap to first tab
                end if
            end if

        ! Text modification
        case('backspace')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call handle_backspace(editor%cursors(i), buffer)
                end do
            else
                call handle_backspace(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('delete')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call handle_delete(editor%cursors(i), buffer)
                end do
            else
                call handle_delete(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('enter')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Sort cursors and apply from bottom to top to avoid position shifts
                call sort_cursors_by_position(editor)
                ! Save original line numbers before any insertions
                allocate(original_lines(size(editor%cursors)))
                do i = 1, size(editor%cursors)
                    original_lines(i) = editor%cursors(i)%line
                end do

                ! Process in reverse order (bottom to top)
                do i = size(editor%cursors), 1, -1
                    ! Save the line where we're inserting
                    insert_line = original_lines(i)

                    call handle_enter(editor%cursors(i), buffer)

                    ! Adjust ALL other cursors that were BELOW where we inserted
                    ! (cursors at same line are handled by their own handle_enter)
                    do j = 1, size(editor%cursors)
                        if (j /= i .and. original_lines(j) > insert_line) then
                            ! This cursor was below where we inserted, shift it down
                            editor%cursors(j)%line = editor%cursors(j)%line + 1
                        end if
                    end do
                end do
                deallocate(original_lines)
            else
                call handle_enter(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('tab')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    if (editor%cursors(i)%has_selection) then
                        call indent_selection(editor%cursors(i), buffer)
                    else
                        call handle_tab(editor%cursors(i), buffer)
                    end if
                end do
            else
                if (editor%cursors(editor%active_cursor)%has_selection) then
                    call indent_selection(editor%cursors(editor%active_cursor), buffer)
                else
                    call handle_tab(editor%cursors(editor%active_cursor), buffer)
                end if
            end if
            is_edit_action = .true.

        case('shift-tab')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (editor%cursors(editor%active_cursor)%has_selection) then
                call dedent_selection(editor%cursors(editor%active_cursor), buffer)
            else
                call dedent_current_line(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        ! Editing keybinds
        case('ctrl-k')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call kill_line_forward(editor%cursors(i), buffer)
                end do
            else
                call kill_line_forward(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-u')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call kill_line_backward(editor%cursors(i), buffer)
                end do
            else
                call kill_line_backward(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-w')
            ! Close current tab
            if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                call close_tab(editor, editor%active_tab_index)

                ! If tabs remain, copy the new active tab's buffer to display
                if (size(editor%tabs) > 0 .and. editor%active_tab_index > 0) then
                    call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%buffer)
                    editor%modified = editor%tabs(editor%active_tab_index)%modified
                ! If no tabs left, open fuss mode
                else
                    editor%fuss_mode_active = .true.
                    if (allocated(editor%workspace_path)) then
                        call init_tree_state(tree_state, editor%workspace_path)
                    end if
                end if
            end if

        case('alt-d', 'alt-delete')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call delete_word_forward(editor%cursors(i), buffer)
                end do
            else
                call delete_word_forward(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('alt-backspace')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call delete_word_backward(editor%cursors(i), buffer)
                end do
            else
                call delete_word_backward(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-t')
            ! Create new empty tab
            call create_tab(editor, '[Untitled]')
            ! Switch to the new tab (it's already active after create_tab)
            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                ! Update editor state with the new tab
                if (allocated(editor%filename)) deallocate(editor%filename)
                allocate(character(len=10) :: editor%filename)
                editor%filename = '[Untitled]'

                ! Reset cursor to top
                editor%cursors(editor%active_cursor)%line = 1
                editor%cursors(editor%active_cursor)%column = 1
                editor%cursors(editor%active_cursor)%desired_column = 1
                editor%viewport_line = 1
                editor%viewport_column = 1
            end if

        case('ctrl-j')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call join_lines(editor%cursors(i), buffer)
                end do
            else
                call join_lines(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-x')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call cut_selection_or_line(editor%cursors(i), buffer)
                end do
            else
                call cut_selection_or_line(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-c')
            ! Copy only needs active cursor (copies to shared clipboard)
            call copy_selection_or_line(editor%cursors(editor%active_cursor), buffer)

        case('ctrl-v')
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            if (size(editor%cursors) > 1) then
                ! Apply to all cursors
                do i = 1, size(editor%cursors)
                    call paste_clipboard(editor%cursors(i), buffer)
                end do
            else
                call paste_clipboard(editor%cursors(editor%active_cursor), buffer)
            end if
            is_edit_action = .true.

        case('ctrl-s')
            call save_file(editor, buffer)

        case("ctrl-'", "ctrl-apostrophe", "alt-'")
            ! Cycle quotes: " -> ' -> `
            ! ctrl-': Doesn't work (terminals send plain apostrophe)
            ! alt-': Alternative binding (Option+' on Mac)
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call cycle_quotes(editor%cursors(editor%active_cursor), buffer)
            is_edit_action = .true.

        case('ctrl-opt-backspace', 'ctrl-alt-backspace', 'alt-shift-backspace', 'alt-shift-apostrophe')
            ! Remove surrounding brackets/quotes
            ! ctrl-alt-backspace: Doesn't work (terminals send alt-backspace)
            ! alt-shift-backspace: Doesn't work (terminals send alt-backspace)
            ! alt-shift-': Alternative binding (Alt+Shift+' = Alt+")
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call remove_brackets(editor%cursors(editor%active_cursor), buffer)
            is_edit_action = .true.

        case('ctrl-d')
            call select_next_match(editor, buffer)

        case('alt-[', 'alt-]')
            ! Jump to matching bracket
            call jump_to_matching_bracket(editor, buffer)

        case('opt-meta-up', 'ctrl-alt-up', 'alt-ctrl-up')
            ! Add cursor on line above
            ! opt-meta-up: Doesn't work (terminals don't send Cmd)
            ! ctrl-alt-up: Alternative binding that works
            call add_cursor_above(editor, buffer)

        case('opt-meta-down', 'ctrl-alt-down', 'alt-ctrl-down')
            ! Add cursor on line below
            ! opt-meta-down: Doesn't work (terminals don't send Cmd)
            ! ctrl-alt-down: Alternative binding that works
            call add_cursor_below(editor, buffer)

        ! Search commands
        case('ctrl-f')
            ! Search forward (Ctrl+F)
            call show_search_prompt(editor, buffer)
            call update_viewport(editor)

        case('ctrl-r')
            ! Find and replace
            if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
            call show_replace_prompt(editor, buffer)
            call update_viewport(editor)
            is_edit_action = .true.

        case('n')
            ! Only use 'n' for search navigation if we have an active search
            if (allocated(current_search_pattern)) then
                call search_forward(editor, buffer)
                call update_viewport(editor)
            else
                ! No active search, treat as regular character with multi-cursor support
                if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, 'n')
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, 'n')
                end if
                is_edit_action = .true.
            end if

        case('N')
            ! Only use 'N' for search navigation if we have an active search
            if (allocated(current_search_pattern)) then
                call search_backward(editor, buffer)
                call update_viewport(editor)
            else
                ! No active search, treat as regular character with multi-cursor support
                if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, 'N')
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, 'N')
                end if
                is_edit_action = .true.
            end if

        case default
            ! Check for mouse events
            if (index(key_str, 'mouse-') == 1) then
                call handle_mouse_event_action(key_str, editor, buffer)
            ! Regular character input (including space)
            ! Check for single char: either len_trim=1, or it's a space (trim removes it)
            else if (len_trim(key_str) == 1 .or. (len_trim(key_str) == 0 .and. key_str(1:1) == ' ')) then
                if (.not. last_action_was_edit) call save_undo_state(buffer, editor)
                ! Handle character input for all cursors
                if (size(editor%cursors) > 1) then
                    call insert_char_multiple_cursors(editor, buffer, key_str(1:1))
                else
                    call insert_char(editor%cursors(editor%active_cursor), buffer, key_str(1:1))
                end if
                is_edit_action = .true.
            end if
        end select

        ! Update edit action state
        last_action_was_edit = is_edit_action
    end subroutine handle_key_command

    subroutine move_cursor_up(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: current_line, target_line

        cursor%has_selection = .false.  ! Clear selection
        if (cursor%line > 1) then
            ! Check if current line is empty
            current_line = buffer_get_line(buffer, cursor%line)

            cursor%line = cursor%line - 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from an empty line, go to end of target line
            if (len(current_line) == 0) then
                cursor%column = len(target_line) + 1
                cursor%desired_column = cursor%column
            else
                ! Normal behavior - use desired column
                cursor%column = cursor%desired_column
                ! Clamp to line bounds
                if (cursor%column > len(target_line) + 1) then
                    cursor%column = len(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine move_cursor_up

    subroutine move_cursor_down(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: current_line, target_line

        cursor%has_selection = .false.  ! Clear selection
        if (cursor%line < line_count) then
            ! Check if current line is empty
            current_line = buffer_get_line(buffer, cursor%line)

            cursor%line = cursor%line + 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from an empty line, go to column 1 of target line
            if (len(current_line) == 0) then
                cursor%column = 1
                cursor%desired_column = 1
            else
                ! Normal behavior - use desired column
                cursor%column = cursor%desired_column
                ! Clamp to line bounds
                if (cursor%column > len(target_line) + 1) then
                    cursor%column = len(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine move_cursor_down

    subroutine move_cursor_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        ! If we have a selection, move to START of selection (leftmost/earliest position)
        if (cursor%has_selection) then
            ! Find which end is further left (start of selection)
            if (cursor%selection_start_line < cursor%line .or. &
                (cursor%selection_start_line == cursor%line .and. cursor%selection_start_col < cursor%column)) then
                ! selection_start is the start - move there
                cursor%line = cursor%selection_start_line
                cursor%column = cursor%selection_start_col
            end if
            ! Otherwise cursor is already at the start
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

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

        ! If we have a selection, move to END of selection (rightmost/latest position)
        if (cursor%has_selection) then
            ! Find which end is further right (end of selection)
            if (cursor%selection_start_line > cursor%line .or. &
                (cursor%selection_start_line == cursor%line .and. cursor%selection_start_col > cursor%column)) then
                ! selection_start is the end - move there
                cursor%line = cursor%selection_start_line
                cursor%column = cursor%selection_start_col
            end if
            ! Otherwise cursor is already at the end
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= len(line)) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_right

    subroutine move_cursor_smart_home(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: first_non_whitespace, i

        cursor%has_selection = .false.  ! Clear selection

        ! Get the current line
        line = buffer_get_line(buffer, cursor%line)

        ! Find the first non-whitespace character
        first_non_whitespace = 1
        do i = 1, len(line)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then  ! Not space or tab
                first_non_whitespace = i
                exit
            end if
        end do

        ! Smart home behavior:
        ! If we're already at the first non-whitespace, go to column 1
        ! If we're at column 1, go to first non-whitespace
        ! Otherwise, go to first non-whitespace
        if (cursor%column == first_non_whitespace .and. first_non_whitespace > 1) then
            cursor%column = 1
        else
            cursor%column = first_non_whitespace
        end if

        cursor%desired_column = cursor%column

        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_smart_home

    subroutine move_cursor_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        cursor%has_selection = .false.  ! Clear selection
        line = buffer_get_line(buffer, cursor%line)
        cursor%column = len(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_end

    subroutine move_cursor_page_up(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        cursor%has_selection = .false.  ! Clear selection
        page_size = editor%screen_rows - 2  ! Leave room for status bar
        cursor%line = max(1, cursor%line - page_size)
        cursor%column = cursor%desired_column
    end subroutine move_cursor_page_up

    subroutine move_cursor_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        cursor%has_selection = .false.  ! Clear selection
        page_size = editor%screen_rows - 2  ! Leave room for status bar
        cursor%line = min(line_count, cursor%line + page_size)
        cursor%column = cursor%desired_column
    end subroutine move_cursor_page_down

    subroutine move_cursor_word_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_len
        logical :: in_word

        cursor%has_selection = .false.  ! Clear selection
        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        pos = cursor%column

        ! Handle empty lines
        if (line_len == 0) then
            if (cursor%line > 1) then
                ! Move to end of previous line
                cursor%line = cursor%line - 1
                if (allocated(line)) deallocate(line)
                line = buffer_get_line(buffer, cursor%line)
                cursor%column = len(line) + 1
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
            return
        end if

        if (pos > 1 .and. line_len > 0) then
            ! Simple algorithm: move left one position at a time until we find a word start
            ! A word start is: a word char that's either at position 1 OR preceded by a non-word char

            pos = pos - 1  ! Move left one position

            ! Skip any whitespace
            do while (pos > 1 .and. pos <= line_len)
                if (line(pos:pos) /= ' ') exit
                pos = pos - 1
            end do

            ! If we're on a word character, go to the start of this word
            if (pos >= 1 .and. pos <= line_len) then
                if (is_word_char(line(pos:pos))) then
                    ! Move to the start of the current word
                    do while (pos > 1)
                        if (pos-1 < 1) exit  ! Safety check
                        if (.not. is_word_char(line(pos-1:pos-1))) exit
                        pos = pos - 1
                    end do
                end if
            end if

            ! Clamp to valid range
            if (pos < 1) pos = 1
            if (pos > line_len + 1) pos = line_len + 1

            cursor%column = pos
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            if (allocated(line)) deallocate(line)
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = len(line) + 1
        else
            cursor%column = 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_left

    subroutine move_cursor_word_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_count, line_len
        logical :: in_word

        cursor%has_selection = .false.  ! Clear selection
        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)
        line_len = len(line)
        pos = cursor%column

        if (pos <= line_len) then
            ! VSCode-style word navigation: stop after each word OR punctuation group
            if (line(pos:pos) == ' ') then
                ! On whitespace - skip all whitespace
                do while (pos < line_len)
                    if (pos+1 <= line_len .and. line(pos+1:pos+1) == ' ') then
                        pos = pos + 1
                    else
                        exit
                    end if
                end do
                pos = pos + 1  ! Move past whitespace
            else if (is_word_char(line(pos:pos))) then
                ! We're on a word character - skip to end of this word
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (.not. is_word_char(line(pos+1:pos+1))) exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1  ! Move past the word
            else
                ! We're on punctuation - skip to end of punctuation group
                ! e.g., "##" should be treated as one group
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        ! Stop if next char is word char or space
                        if (is_word_char(line(pos+1:pos+1)) .or. line(pos+1:pos+1) == ' ') exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1  ! Move past punctuation
            end if

            cursor%column = pos
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
        else
            cursor%column = len(line) + 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine move_cursor_word_right

    function is_word_char(ch) result(is_word)
        character, intent(in) :: ch
        logical :: is_word

        is_word = (ch >= 'a' .and. ch <= 'z') .or. &
                  (ch >= 'A' .and. ch <= 'Z') .or. &
                  (ch >= '0' .and. ch <= '9') .or. &
                  ch == '_'
    end function is_word_char

    subroutine handle_backspace(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
            return
        end if

        if (cursor%column > 1) then
            ! Delete character before cursor
            cursor%column = cursor%column - 1
            call buffer_delete_at_cursor(buffer, cursor)
            cursor%desired_column = cursor%column
        else if (cursor%line > 1) then
            ! Join with previous line
            call join_line_with_previous(cursor, buffer)
        end if
    end subroutine handle_backspace

    subroutine handle_delete(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
            return
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        if (cursor%column <= len(line)) then
            ! Delete character at cursor
            call buffer_delete_at_cursor(buffer, cursor)
        else if (cursor%line < line_count) then
            ! Join with next line
            call join_line_with_next(cursor, buffer)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine handle_delete

    subroutine handle_enter(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line
        integer :: indent_level, i

        ! Delete selection if one exists
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
        end if

        ! Get the current line to determine indentation
        current_line = buffer_get_line(buffer, cursor%line)

        ! Count leading spaces/tabs for auto-indent
        indent_level = 0
        do i = 1, len(current_line)
            if (current_line(i:i) == ' ') then
                indent_level = indent_level + 1
            else if (current_line(i:i) == char(9)) then  ! Tab
                indent_level = indent_level + 4  ! Treat tab as 4 spaces
            else
                exit  ! Found non-whitespace character
            end if
        end do

        ! Insert the newline
        call buffer_insert_newline(buffer, cursor)
        cursor%line = cursor%line + 1
        cursor%column = 1

        ! Insert the same indentation on the new line
        do i = 1, indent_level
            call buffer_insert_char(buffer, cursor, ' ')
            cursor%column = cursor%column + 1
        end do

        cursor%desired_column = cursor%column

        if (allocated(current_line)) deallocate(current_line)
    end subroutine handle_enter

    subroutine handle_tab(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: i

        ! Insert 4 spaces
        do i = 1, 4
            call buffer_insert_char(buffer, cursor, ' ')
            cursor%column = cursor%column + 1
        end do
        cursor%desired_column = cursor%column
    end subroutine handle_tab

    subroutine indent_selection(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: start_line, end_line, i
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) return

        ! Get the range of lines to indent
        start_line = min(cursor%selection_start_line, cursor%line)
        end_line = max(cursor%selection_start_line, cursor%line)

        ! Indent each line in the selection
        do i = start_line, end_line
            line = buffer_get_line(buffer, i)
            ! Insert 4 spaces at the beginning of the line
            call buffer_insert_text_at(buffer, i, 1, "    ")
            if (allocated(line)) deallocate(line)
        end do

        ! Adjust cursor position if needed
        if (cursor%column > 1) then
            cursor%column = cursor%column + 4
        end if
        if (cursor%selection_start_col > 1) then
            cursor%selection_start_col = cursor%selection_start_col + 4
        end if
    end subroutine indent_selection

    subroutine dedent_selection(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: start_line, end_line, i, spaces_to_remove
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) return

        ! Get the range of lines to dedent
        start_line = min(cursor%selection_start_line, cursor%line)
        end_line = max(cursor%selection_start_line, cursor%line)

        ! Dedent each line in the selection
        do i = start_line, end_line
            line = buffer_get_line(buffer, i)
            spaces_to_remove = 0

            ! Count how many spaces we can remove (max 4)
            do while (spaces_to_remove < 4 .and. spaces_to_remove < len(line))
                if (line(spaces_to_remove + 1:spaces_to_remove + 1) == ' ') then
                    spaces_to_remove = spaces_to_remove + 1
                else
                    exit
                end if
            end do

            ! Remove the spaces
            if (spaces_to_remove > 0) then
                call buffer_delete_range(buffer, i, 1, i, spaces_to_remove + 1)

                ! Adjust cursor position for current line
                if (i == cursor%line .and. cursor%column > spaces_to_remove) then
                    cursor%column = cursor%column - spaces_to_remove
                else if (i == cursor%line .and. cursor%column <= spaces_to_remove) then
                    cursor%column = 1
                end if

                if (i == cursor%selection_start_line .and. cursor%selection_start_col > spaces_to_remove) then
                    cursor%selection_start_col = cursor%selection_start_col - spaces_to_remove
                else if (i == cursor%selection_start_line .and. cursor%selection_start_col <= spaces_to_remove) then
                    cursor%selection_start_col = 1
                end if
            end if

            if (allocated(line)) deallocate(line)
        end do
    end subroutine dedent_selection

    subroutine dedent_current_line(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: spaces_to_remove

        line = buffer_get_line(buffer, cursor%line)
        spaces_to_remove = 0

        ! Count how many spaces we can remove (max 4)
        do while (spaces_to_remove < 4 .and. spaces_to_remove < len(line))
            if (line(spaces_to_remove + 1:spaces_to_remove + 1) == ' ') then
                spaces_to_remove = spaces_to_remove + 1
            else
                exit
            end if
        end do

        ! Remove the spaces
        if (spaces_to_remove > 0) then
            call buffer_delete_range(buffer, cursor%line, 1, cursor%line, spaces_to_remove + 1)

            ! Adjust cursor position
            if (cursor%column > spaces_to_remove) then
                cursor%column = cursor%column - spaces_to_remove
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine dedent_current_line

    subroutine insert_char(cursor, buffer, ch)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character, intent(in) :: ch
        character :: closing_char
        logical :: should_auto_close, should_wrap
        integer :: start_line, start_col, end_line, end_col

        ! Check if we should auto-close or wrap brackets/quotes
        should_auto_close = .false.
        should_wrap = .false.
        select case(ch)
        case('(')
            closing_char = ')'
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        case('[')
            closing_char = ']'
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        case('{')
            closing_char = '}'
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        case('"')
            closing_char = '"'
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        case("'")
            closing_char = "'"
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        case('`')
            closing_char = '`'
            should_auto_close = .true.
            if (cursor%has_selection) should_wrap = .true.
        end select

        ! If we should wrap, don't delete - wrap the selection instead
        if (should_wrap) then
            ! Find selection bounds
            if (cursor%line < cursor%selection_start_line .or. &
                (cursor%line == cursor%selection_start_line .and. cursor%column < cursor%selection_start_col)) then
                start_line = cursor%line
                start_col = cursor%column
                end_line = cursor%selection_start_line
                end_col = cursor%selection_start_col
            else
                start_line = cursor%selection_start_line
                start_col = cursor%selection_start_col
                end_line = cursor%line
                end_col = cursor%column
            end if

            ! Insert closing character at end
            cursor%line = end_line
            cursor%column = end_col
            call buffer_insert_char(buffer, cursor, closing_char)

            ! Insert opening character at start
            cursor%line = start_line
            cursor%column = start_col
            call buffer_insert_char(buffer, cursor, ch)

            ! Position cursor after the opening bracket (inside the wrapped text)
            cursor%column = start_col + 1
            cursor%has_selection = .false.
            cursor%desired_column = cursor%column
            return
        end if

        ! Delete selection if one exists (normal behavior)
        if (cursor%has_selection) then
            call delete_selection(cursor, buffer)
        end if

        ! Insert the character
        call buffer_insert_char(buffer, cursor, ch)
        cursor%column = cursor%column + 1

        ! If auto-close is enabled, insert the closing character
        if (should_auto_close) then
            call buffer_insert_char(buffer, cursor, closing_char)
            ! Don't move cursor forward - stay between the brackets/quotes
        end if

        cursor%desired_column = cursor%column
    end subroutine insert_char

    subroutine insert_char_multiple_cursors(editor, buffer, ch)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character, intent(in) :: ch
        integer :: i
        integer :: offset_adjust
        integer :: cursors_before

        ! Sort cursors by position to handle offset adjustments
        call sort_cursors_by_position(editor)

        offset_adjust = 0
        do i = 1, size(editor%cursors)
            ! For cursors with selection, delete selection first
            if (editor%cursors(i)%has_selection) then
                call delete_selection(editor%cursors(i), buffer)
                editor%cursors(i)%has_selection = .false.
            end if

            ! Insert character
            call buffer_insert_char(buffer, editor%cursors(i), ch)
            editor%cursors(i)%column = editor%cursors(i)%column + 1
            editor%cursors(i)%desired_column = editor%cursors(i)%column
        end do
    end subroutine insert_char_multiple_cursors

    subroutine delete_selection(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        integer :: start_line, start_col, end_line, end_col
        integer :: i
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) return

        ! Determine start and end of selection
        if (cursor%line < cursor%selection_start_line .or. &
            (cursor%line == cursor%selection_start_line .and. &
             cursor%column < cursor%selection_start_col)) then
            start_line = cursor%line
            start_col = cursor%column
            end_line = cursor%selection_start_line
            end_col = cursor%selection_start_col
        else
            start_line = cursor%selection_start_line
            start_col = cursor%selection_start_col
            end_line = cursor%line
            end_col = cursor%column
        end if

        ! Delete the selection
        if (start_line == end_line) then
            ! Single-line selection
            line = buffer_get_line(buffer, start_line)
            cursor%line = start_line
            cursor%column = start_col
            do i = start_col, end_col - 1
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            if (allocated(line)) deallocate(line)
        else
            ! Multi-line selection
            ! Delete from start_col to end of first line
            cursor%line = start_line
            cursor%column = start_col
            line = buffer_get_line(buffer, start_line)
            do i = start_col, len(line)
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            if (allocated(line)) deallocate(line)

            ! Delete entire lines in between
            do i = start_line + 1, end_line - 1
                ! After deleting from first line, the next line moves up
                ! So we keep deleting line at position start_line + 1
                if (buffer_get_line_count(buffer) > start_line) then
                    ! Delete the newline to join with next line
                    line = buffer_get_line(buffer, start_line)
                    cursor%column = len(line) + 1
                    call buffer_delete_at_cursor(buffer, cursor)  ! Delete newline
                    if (allocated(line)) deallocate(line)

                    ! Delete all content of the joined line
                    line = buffer_get_line(buffer, start_line)
                    cursor%column = len(line)
                    do while (cursor%column > start_col .and. cursor%column > 0)
                        call buffer_delete_at_cursor(buffer, cursor)
                        cursor%column = cursor%column - 1
                    end do
                    if (allocated(line)) deallocate(line)
                end if
            end do

            ! Delete from beginning of last line to end_col
            if (buffer_get_line_count(buffer) > start_line) then
                line = buffer_get_line(buffer, start_line)
                cursor%column = len(line) + 1
                call buffer_delete_at_cursor(buffer, cursor)  ! Delete newline
                if (allocated(line)) deallocate(line)

                ! Delete from start to end_col
                cursor%column = start_col
                do i = 1, end_col - 1
                    if (cursor%column <= buffer_get_line_count(buffer)) then
                        call buffer_delete_at_cursor(buffer, cursor)
                    end if
                end do
            end if

            cursor%line = start_line
            cursor%column = start_col
        end if

        cursor%has_selection = .false.
    end subroutine delete_selection

    function get_selection_text(cursor, buffer) result(text)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text
        integer :: start_line, start_col, end_line, end_col
        integer :: i
        character(len=:), allocatable :: line

        if (.not. cursor%has_selection) then
            allocate(character(len=0) :: text)
            return
        end if

        ! Determine start and end of selection
        if (cursor%line < cursor%selection_start_line .or. &
            (cursor%line == cursor%selection_start_line .and. &
             cursor%column < cursor%selection_start_col)) then
            start_line = cursor%line
            start_col = cursor%column
            end_line = cursor%selection_start_line
            end_col = cursor%selection_start_col
        else
            start_line = cursor%selection_start_line
            start_col = cursor%selection_start_col
            end_line = cursor%line
            end_col = cursor%column
        end if

        ! Extract text based on selection
        if (start_line == end_line) then
            ! Single-line selection
            line = buffer_get_line(buffer, start_line)
            if (allocated(line) .and. start_col <= len(line) + 1 .and. end_col <= len(line) + 1) then
                if (end_col > start_col) then
                    allocate(character(len=end_col - start_col) :: text)
                    text = line(start_col:end_col - 1)
                else
                    allocate(character(len=0) :: text)
                end if
            else
                allocate(character(len=0) :: text)
            end if
            if (allocated(line)) deallocate(line)
        else
            ! Multi-line selection
            text = ""

            ! First line (from start_col to end)
            line = buffer_get_line(buffer, start_line)
            if (allocated(line)) then
                if (start_col <= len(line)) then
                    text = text // line(start_col:)
                end if
                text = text // char(10)  ! newline
                deallocate(line)
            end if

            ! Middle lines (complete lines)
            do i = start_line + 1, end_line - 1
                line = buffer_get_line(buffer, i)
                if (allocated(line)) then
                    text = text // line // char(10)
                    deallocate(line)
                end if
            end do

            ! Last line (from beginning to end_col)
            line = buffer_get_line(buffer, end_line)
            if (allocated(line)) then
                if (end_col > 1 .and. end_col <= len(line) + 1) then
                    text = text // line(1:end_col - 1)
                end if
                deallocate(line)
            end if
        end if
    end function get_selection_text

    subroutine sort_cursors_by_position(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t) :: temp
        integer :: i, j
        logical :: swapped

        ! Simple bubble sort for small number of cursors
        do i = 1, size(editor%cursors) - 1
            swapped = .false.
            do j = 1, size(editor%cursors) - i
                if (editor%cursors(j)%line > editor%cursors(j+1)%line .or. &
                    (editor%cursors(j)%line == editor%cursors(j+1)%line .and. &
                     editor%cursors(j)%column > editor%cursors(j+1)%column)) then
                    temp = editor%cursors(j)
                    editor%cursors(j) = editor%cursors(j+1)
                    editor%cursors(j+1) = temp
                    if (editor%active_cursor == j) then
                        editor%active_cursor = j + 1
                    else if (editor%active_cursor == j + 1) then
                        editor%active_cursor = j
                    end if
                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit
        end do
    end subroutine sort_cursors_by_position

    subroutine deduplicate_cursors(editor)
        type(editor_state_t), intent(inout) :: editor
        type(cursor_t), allocatable :: unique_cursors(:)
        integer :: i, j, unique_count
        logical :: is_duplicate

        if (size(editor%cursors) <= 1) return

        ! Count unique cursors
        unique_count = 0
        do i = 1, size(editor%cursors)
            is_duplicate = .false.
            do j = 1, i-1
                if (editor%cursors(i)%line == editor%cursors(j)%line .and. &
                    editor%cursors(i)%column == editor%cursors(j)%column) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) then
                unique_count = unique_count + 1
            end if
        end do

        ! If we have duplicates, create new array with only unique cursors
        if (unique_count < size(editor%cursors)) then
            allocate(unique_cursors(unique_count))
            unique_count = 0
            do i = 1, size(editor%cursors)
                is_duplicate = .false.
                do j = 1, i-1
                    if (editor%cursors(i)%line == editor%cursors(j)%line .and. &
                        editor%cursors(i)%column == editor%cursors(j)%column) then
                        is_duplicate = .true.
                        exit
                    end if
                end do
                if (.not. is_duplicate) then
                    unique_count = unique_count + 1
                    unique_cursors(unique_count) = editor%cursors(i)
                    ! Adjust active cursor index
                    if (i == editor%active_cursor) then
                        editor%active_cursor = unique_count
                    end if
                end if
            end do
            deallocate(editor%cursors)
            editor%cursors = unique_cursors
        end if
    end subroutine deduplicate_cursors

    subroutine join_line_with_previous(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: prev_line
        integer :: new_column

        prev_line = buffer_get_line(buffer, cursor%line - 1)
        new_column = len(prev_line) + 1

        ! Move to end of previous line
        cursor%line = cursor%line - 1
        cursor%column = new_column

        ! Delete the newline
        call buffer_delete_at_cursor(buffer, cursor)

        cursor%desired_column = cursor%column
        if (allocated(prev_line)) deallocate(prev_line)
    end subroutine join_line_with_previous

    subroutine join_line_with_next(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer

        ! Delete the newline at end of current line
        call buffer_delete_at_cursor(buffer, cursor)
    end subroutine join_line_with_next

    subroutine kill_line_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        character(len=:), allocatable :: killed_text
        integer :: i

        line = buffer_get_line(buffer, cursor%line)

        if (cursor%column <= len(line)) then
            ! Kill from cursor to end of line
            killed_text = line(cursor%column:)
            do i = cursor%column, len(line)
                call buffer_delete_at_cursor(buffer, cursor)
            end do
        else
            ! At end of line - kill the newline
            killed_text = char(10)  ! newline
            call buffer_delete_at_cursor(buffer, cursor)
        end if

        ! Add to yank stack
        if (len(killed_text) > 0) then
            call push_yank(yank_stack, killed_text)
        end if

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
        if (allocated(killed_text)) deallocate(killed_text)
    end subroutine kill_line_forward

    subroutine kill_line_backward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        character(len=:), allocatable :: killed_text
        integer :: i, start_col

        line = buffer_get_line(buffer, cursor%line)
        start_col = cursor%column

        if (cursor%column > 1) then
            ! Kill from start of line to cursor
            killed_text = line(1:cursor%column-1)
            cursor%column = 1
            do i = 1, start_col - 1
                call buffer_delete_at_cursor(buffer, cursor)
            end do
            cursor%desired_column = 1
        end if

        ! Add to yank stack
        if (allocated(killed_text)) then
            if (len(killed_text) > 0) then
                call push_yank(yank_stack, killed_text)
            end if
        end if

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
        if (allocated(killed_text)) deallocate(killed_text)
    end subroutine kill_line_backward

    subroutine yank_text(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text
        integer :: i

        text = pop_yank(yank_stack)
        if (allocated(text)) then
            do i = 1, len(text)
                if (text(i:i) == char(10)) then
                    call buffer_insert_newline(buffer, cursor)
                    cursor%line = cursor%line + 1
                    cursor%column = 1
                else
                    call buffer_insert_char(buffer, cursor, text(i:i))
                    cursor%column = cursor%column + 1
                end if
            end do
            cursor%desired_column = cursor%column
            buffer%modified = .true.
            deallocate(text)
        end if
    end subroutine yank_text

    subroutine move_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, prev_line
        integer :: saved_column, original_line, total_lines

        if (cursor%line <= 1) return

        ! Save state
        saved_column = cursor%column
        original_line = cursor%line
        total_lines = buffer_get_line_count(buffer)

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        prev_line = buffer_get_line(buffer, cursor%line - 1)

        ! Delete current line entirely (including newline)
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Move to previous line (now current_line position after delete)
        cursor%line = cursor%line - 1
        cursor%column = 1

        ! Delete previous line entirely (including newline)
        call delete_entire_line(buffer, cursor)

        ! Now insert current_line first, then prev_line
        cursor%column = 1
        call insert_line_text(buffer, cursor, current_line)
        call buffer_insert_newline(buffer, cursor)

        cursor%line = cursor%line + 1
        cursor%column = 1
        call insert_line_text(buffer, cursor, prev_line)
        ! Add newline if we're not at the last line
        if (original_line < total_lines) then
            call buffer_insert_newline(buffer, cursor)
        end if

        ! Restore cursor to moved line
        cursor%line = cursor%line - 1
        cursor%column = min(saved_column, len(current_line) + 1)
        cursor%desired_column = cursor%column

        buffer%modified = .true.
        if (allocated(current_line)) deallocate(current_line)
        if (allocated(prev_line)) deallocate(prev_line)
    end subroutine move_line_up

    subroutine move_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, next_line
        integer :: line_count, saved_column, original_line, total_lines

        line_count = buffer_get_line_count(buffer)
        if (cursor%line >= line_count) return

        ! Save state
        saved_column = cursor%column
        original_line = cursor%line
        total_lines = line_count

        ! Get both lines
        current_line = buffer_get_line(buffer, cursor%line)
        next_line = buffer_get_line(buffer, cursor%line + 1)

        ! Delete current line entirely (including newline)
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Delete next line entirely (including newline)
        ! After deleting current line, next line is now at cursor%line
        cursor%column = 1
        call delete_entire_line(buffer, cursor)

        ! Now insert next_line first, then current_line
        cursor%column = 1
        call insert_line_text(buffer, cursor, next_line)
        call buffer_insert_newline(buffer, cursor)

        cursor%line = cursor%line + 1
        cursor%column = 1
        call insert_line_text(buffer, cursor, current_line)
        ! Add newline if we're not at the last line
        if (original_line + 1 < total_lines) then
            call buffer_insert_newline(buffer, cursor)
        end if

        ! Restore cursor position on moved line
        cursor%column = min(saved_column, len(current_line) + 1)
        cursor%desired_column = cursor%column

        buffer%modified = .true.
        if (allocated(current_line)) deallocate(current_line)
        if (allocated(next_line)) deallocate(next_line)
    end subroutine move_line_down

    subroutine duplicate_line_up(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line

        line = buffer_get_line(buffer, cursor%line)

        ! Move to start of line
        cursor%column = 1
        ! Insert newline before
        call buffer_insert_newline(buffer, cursor)
        ! Insert the duplicated text
        call insert_line_text(buffer, cursor, line)
        ! Stay on original line
        cursor%line = cursor%line + 1

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_up

    subroutine duplicate_line_down(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: saved_column

        line = buffer_get_line(buffer, cursor%line)
        saved_column = cursor%column

        ! Move to end of line
        cursor%column = len(line) + 1
        ! Insert newline
        call buffer_insert_newline(buffer, cursor)
        cursor%line = cursor%line + 1
        cursor%column = 1
        ! Insert the duplicated text
        call insert_line_text(buffer, cursor, line)

        ! Return to original position
        cursor%line = cursor%line - 1
        cursor%column = saved_column
        cursor%desired_column = saved_column

        buffer%modified = .true.
        if (allocated(line)) deallocate(line)
    end subroutine duplicate_line_down

    subroutine delete_entire_line(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=:), allocatable :: line
        integer :: i

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = 1

        ! Delete all characters in line
        do i = 1, len(line)
            call buffer_delete_at_cursor(buffer, cursor)
        end do

        ! Delete the newline if not the last line
        if (cursor%line < buffer_get_line_count(buffer)) then
            call buffer_delete_at_cursor(buffer, cursor)
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_entire_line

    subroutine insert_line_text(buffer, cursor, text)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursor
        character(len=*), intent(in) :: text
        integer :: i

        do i = 1, len(text)
            call buffer_insert_char(buffer, cursor, text(i:i))
            cursor%column = cursor%column + 1
        end do
    end subroutine insert_line_text

    subroutine get_line_positions(buffer, line_num, start_pos, end_pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer, intent(out) :: start_pos, end_pos
        integer :: current_line, i

        current_line = 1
        start_pos = 1

        ! Find start of requested line
        do i = 1, buffer%size
            if (current_line == line_num) then
                start_pos = i
                exit
            end if
            if (i < buffer%gap_start .or. i >= buffer%gap_end) then
                if (buffer_get_char_at(buffer, i) == char(10)) then
                    current_line = current_line + 1
                end if
            end if
        end do

        ! Find end of line
        end_pos = start_pos
        do i = start_pos, buffer%size
            if (buffer_get_char_at(buffer, i) == char(10)) then
                end_pos = i
                exit
            end if
            end_pos = i
        end do
    end subroutine get_line_positions

    function buffer_get_char_at(buffer, pos) result(ch)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: pos
        character :: ch

        if (pos < buffer%gap_start) then
            ch = buffer%data(pos:pos)
        else
            ch = buffer%data(pos + (buffer%gap_end - buffer%gap_start):&
                           pos + (buffer%gap_end - buffer%gap_start))
        end if
    end function buffer_get_char_at

    subroutine cut_selection_or_line(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text

        if (cursor%has_selection) then
            ! Get selected text
            text = get_selection_text(cursor, buffer)

            ! Copy to clipboard
            if (allocated(text)) then
                call copy_to_clipboard(text)
            end if

            ! Delete the selection
            call delete_selection(cursor, buffer)
        else
            ! Get current line
            text = buffer_get_line(buffer, cursor%line)

            ! Copy to clipboard
            call copy_to_clipboard(text)

            ! Delete the line
            cursor%column = 1
            call delete_entire_line(buffer, cursor)
        end if

        buffer%modified = .true.
        if (allocated(text)) deallocate(text)
    end subroutine cut_selection_or_line

    subroutine copy_selection_or_line(cursor, buffer)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text

        if (cursor%has_selection) then
            ! Get selected text
            text = get_selection_text(cursor, buffer)
        else
            ! Get current line
            text = buffer_get_line(buffer, cursor%line)
        end if

        ! Copy to clipboard
        if (allocated(text)) then
            call copy_to_clipboard(text)
            deallocate(text)
        end if
    end subroutine copy_selection_or_line

    subroutine paste_clipboard(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: text
        integer :: i

        ! Get text from clipboard
        text = paste_from_clipboard()

        if (allocated(text)) then
            ! Insert text at cursor position
            do i = 1, len(text)
                if (text(i:i) == char(10)) then
                    call buffer_insert_newline(buffer, cursor)
                    cursor%line = cursor%line + 1
                    cursor%column = 1
                else
                    call buffer_insert_char(buffer, cursor, text(i:i))
                    cursor%column = cursor%column + 1
                end if
            end do
            cursor%desired_column = cursor%column
            buffer%modified = .true.
            deallocate(text)
        end if
    end subroutine paste_clipboard

    subroutine save_file(editor, buffer)
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer :: ios, temp_unit
        character(len=256) :: temp_filename, command
        character(len=1024) :: error_msg
        logical :: file_exists, has_write_permission

        if (.not. allocated(editor%filename)) return

        ! Check if this is an untitled file
        if (trim(editor%filename) == '[Untitled]') then
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('Cannot save: Please use Save As or provide a filename (file is untitled)')
            return
        end if

        ! First try normal save
        call buffer_save_file(buffer, editor%filename, ios)

        if (ios == 0) then
            buffer%modified = .false.
            return
        end if

        ! Check if file exists and we have write permission
        inquire(file=editor%filename, exist=file_exists)

        ! If save failed, try sudo save
        write(temp_filename, '(a,i0)') '/tmp/facsimile_sudo_', getpid()

        ! Save to temporary file
        call buffer_save_file(buffer, temp_filename, ios)
        if (ios /= 0) then
            ! Can't even save to /tmp, serious problem
            write(error_unit, *) 'Error: Cannot save file even to /tmp'
            return
        end if

        ! Use sudo to move the file
        write(command, '(a,a,a,a,a)') 'sudo mv ', trim(temp_filename), ' ', &
                                       trim(editor%filename), ' 2>/dev/null'

        ! Show message to user
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('[sudo] password required to save file')

        ! Execute sudo command
        call execute_command_line(command, exitstat=ios)

        if (ios == 0) then
            buffer%modified = .false.
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('File saved with sudo                  ')
        else
            ! Clean up temp file
            write(command, '(a,a)') 'rm -f ', trim(temp_filename)
            call execute_command_line(command)
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write('Save failed - permission denied        ')
        end if
    end subroutine save_file

    function getpid() result(pid)
        integer :: pid
        interface
            function c_getpid() bind(C, name="getpid")
                use iso_c_binding, only: c_int
                integer(c_int) :: c_getpid
            end function
        end interface
        pid = c_getpid()
    end function getpid

    subroutine cycle_quotes(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: quote_start, quote_end
        character :: current_quote, new_quote

        line = buffer_get_line(buffer, cursor%line)

        ! Find surrounding quotes
        call find_surrounding_quotes(line, cursor%column, quote_start, quote_end, current_quote)

        if (quote_start > 0 .and. quote_end > 0) then
            ! Determine next quote type
            select case(current_quote)
            case('"')
                new_quote = "'"
            case("'")
                new_quote = '`'
            case('`')
                new_quote = '"'
            case default
                return
            end select

            ! Replace quotes
            cursor%column = quote_start
            call buffer_delete_at_cursor(buffer, cursor)
            call buffer_insert_char(buffer, cursor, new_quote)

            cursor%column = quote_end
            call buffer_delete_at_cursor(buffer, cursor)
            call buffer_insert_char(buffer, cursor, new_quote)

            ! Restore cursor position
            cursor%column = quote_end
            buffer%modified = .true.
        end if

        if (allocated(line)) deallocate(line)
    end subroutine cycle_quotes

    subroutine find_surrounding_quotes(line, pos, start_pos, end_pos, quote_char)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: start_pos, end_pos
        character, intent(out) :: quote_char
        integer :: i

        start_pos = 0
        end_pos = 0
        quote_char = ' '

        ! Search backward for opening quote
        do i = pos - 1, 1, -1
            if (line(i:i) == '"' .or. line(i:i) == "'" .or. line(i:i) == '`') then
                start_pos = i
                quote_char = line(i:i)
                exit
            end if
        end do

        if (start_pos > 0) then
            ! Search forward for closing quote
            do i = pos, len(line)
                if (line(i:i) == quote_char) then
                    end_pos = i
                    exit
                end if
            end do
        end if
    end subroutine find_surrounding_quotes

    subroutine remove_brackets(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: bracket_start, bracket_end
        character :: open_bracket, close_bracket

        line = buffer_get_line(buffer, cursor%line)

        ! Find surrounding brackets
        call find_surrounding_brackets(line, cursor%column, bracket_start, bracket_end, &
                                       open_bracket, close_bracket)

        if (bracket_start > 0 .and. bracket_end > 0) then
            ! Delete closing bracket first (to maintain positions)
            cursor%column = bracket_end
            call buffer_delete_at_cursor(buffer, cursor)

            ! Delete opening bracket
            cursor%column = bracket_start
            call buffer_delete_at_cursor(buffer, cursor)

            buffer%modified = .true.
        end if

        if (allocated(line)) deallocate(line)
    end subroutine remove_brackets

    subroutine find_surrounding_brackets(line, pos, start_pos, end_pos, open_br, close_br)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: start_pos, end_pos
        character, intent(out) :: open_br, close_br
        integer :: i

        start_pos = 0
        end_pos = 0
        open_br = ' '
        close_br = ' '

        ! Search backward for opening bracket
        do i = pos - 1, 1, -1
            select case(line(i:i))
            case('(')
                start_pos = i
                open_br = '('
                close_br = ')'
                exit
            case('[')
                start_pos = i
                open_br = '['
                close_br = ']'
                exit
            case('{')
                start_pos = i
                open_br = '{'
                close_br = '}'
                exit
            end select
        end do

        if (start_pos > 0) then
            ! Search forward for matching closing bracket
            do i = pos, len(line)
                if (line(i:i) == close_br) then
                    end_pos = i
                    exit
                end if
            end do
        end if
    end subroutine find_surrounding_brackets

    subroutine init_cursor(cursor)
        type(cursor_t), intent(out) :: cursor
        cursor%line = 1
        cursor%column = 1
        cursor%desired_column = 1
        cursor%has_selection = .false.
        cursor%selection_start_line = 1
        cursor%selection_start_col = 1
    end subroutine init_cursor

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
                ! Clear selection
                editor%cursors(editor%active_cursor)%has_selection = .false.
            end if

        case('mouse-drag')
            ! Mouse drag - extend selection
            if (.not. editor%cursors(editor%active_cursor)%has_selection) then
                ! Start selection from current position
                editor%cursors(editor%active_cursor)%has_selection = .true.
                editor%cursors(editor%active_cursor)%selection_start_line = &
                    editor%cursors(editor%active_cursor)%line
                editor%cursors(editor%active_cursor)%selection_start_col = &
                    editor%cursors(editor%active_cursor)%column
            end if
            ! Move cursor to drag position (extends selection)
            call position_cursor_at_screen(editor%cursors(editor%active_cursor), &
                                          editor, buffer, row, col)
            call update_viewport(editor)

        case('mouse-release')
            ! Mouse button released - nothing special to do
            continue

        case('mouse-scroll-up')
            ! Scroll up by 3 lines
            editor%viewport_line = max(1, editor%viewport_line - 3)

        case('mouse-scroll-down')
            ! Scroll down by 3 lines
            editor%viewport_line = min(buffer_get_line_count(buffer) - editor%screen_rows + 2, &
                                      editor%viewport_line + 3)

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

        end select
    end subroutine handle_mouse_event_action

    subroutine position_cursor_at_screen(cursor, editor, buffer, screen_row, screen_col)
        use renderer_module, only: show_line_numbers, LINE_NUMBER_WIDTH
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: screen_row, screen_col
        integer :: target_line, target_col, col_offset, row_offset
        character(len=:), allocatable :: line
        integer :: line_count

        line_count = buffer_get_line_count(buffer)

        ! Account for tab bar offset - when tabs exist, row 1 is tab bar, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2  ! Tab bar takes row 1
        else
            row_offset = 1  ! No tab bar
        end if

        ! Ignore clicks on the tab bar
        if (size(editor%tabs) > 0 .and. screen_row < row_offset) then
            return  ! Don't move cursor if clicking on tab bar
        end if

        ! Account for line number display offset
        if (show_line_numbers) then
            col_offset = LINE_NUMBER_WIDTH + 1  ! +1 for separator space
        else
            col_offset = 0
        end if

        ! Convert screen position to buffer position
        ! When tab bar exists: screen_row 2 = viewport_line, screen_row 3 = viewport_line + 1, etc.
        target_line = editor%viewport_line + screen_row - row_offset
        target_col = editor%viewport_column + max(1, screen_col - col_offset)

        ! Clamp to valid range
        if (target_line < 1) target_line = 1
        if (target_line > line_count) target_line = line_count

        ! Get line and adjust column to valid positions only
        line = buffer_get_line(buffer, target_line)
        if (target_col < 1) target_col = 1
        ! Clamp column to actual line length + 1 (position after last char)
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
        integer :: cursor_screen_row, cursor_screen_col, row_offset

        ! Account for tab bar - when tabs exist, content starts at row 2
        if (size(editor%tabs) > 0) then
            row_offset = 2
        else
            row_offset = 1
        end if

        cursor_screen_row = cursor%line - editor%viewport_line + row_offset
        cursor_screen_col = cursor%column - editor%viewport_column + 1
        at_pos = (cursor_screen_row == screen_row .and. cursor_screen_col == screen_col)
    end function is_cursor_at_screen_pos

    subroutine toggle_cursor_at_position(editor, buffer, row, col)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: row, col
        integer :: i, cursor_exists
        type(cursor_t), allocatable :: new_cursors(:)

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
    end subroutine toggle_cursor_at_position

    subroutine handle_mouse_click(editor, buffer, row, col)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: row, col

        ! Clear multiple cursors
        if (allocated(editor%cursors)) then
            if (size(editor%cursors) > 1) then
                deallocate(editor%cursors)
                allocate(editor%cursors(1))
                call init_cursor(editor%cursors(1))
                editor%active_cursor = 1
            end if
        end if

        ! Move cursor to click position
        call position_cursor_at_screen(editor%cursors(1), editor, buffer, row, col)
        call update_viewport(editor)
    end subroutine handle_mouse_click

    subroutine select_next_match(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), allocatable :: new_cursors(:)
        character(len=:), allocatable :: word
        integer :: i
        integer :: found_line, found_col
        logical :: found

        ! If no pattern selected yet, select word at cursor
        if (.not. allocated(search_pattern)) then
            call select_word_at_cursor(editor%cursors(editor%active_cursor), buffer)
            word = get_selected_text(editor%cursors(editor%active_cursor), buffer)
            if (allocated(word)) then
                search_pattern = word
            end if
        else
            ! Search for next occurrence
            call find_next_occurrence(buffer, search_pattern, &
                                     editor%cursors(size(editor%cursors))%line, &
                                     editor%cursors(size(editor%cursors))%column, &
                                     found, found_line, found_col)

            if (found) then
                ! Add a new cursor at the found position
                allocate(new_cursors(size(editor%cursors) + 1))
                do i = 1, size(editor%cursors)
                    new_cursors(i) = editor%cursors(i)
                end do

                ! Initialize new cursor
                call init_cursor(new_cursors(size(new_cursors)))
                new_cursors(size(new_cursors))%line = found_line
                new_cursors(size(new_cursors))%column = found_col
                new_cursors(size(new_cursors))%has_selection = .true.
                new_cursors(size(new_cursors))%selection_start_line = found_line
                new_cursors(size(new_cursors))%selection_start_col = found_col
                new_cursors(size(new_cursors))%column = found_col + len(search_pattern)

                deallocate(editor%cursors)
                editor%cursors = new_cursors
                editor%active_cursor = size(editor%cursors)
            end if
        end if
    end subroutine select_next_match

    subroutine select_word_at_cursor(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: word_start, word_end

        line = buffer_get_line(buffer, cursor%line)

        ! Find word boundaries
        call find_word_boundaries(line, cursor%column, word_start, word_end)

        if (word_start > 0 .and. word_end >= word_start) then
            ! Select the word
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = word_start
            cursor%column = word_end + 1
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine select_word_at_cursor

    function get_selected_text(cursor, buffer) result(text)
        type(cursor_t), intent(in) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: text
        character(len=:), allocatable :: line
        integer :: start_col, end_col

        if (.not. cursor%has_selection) then
            allocate(character(len=0) :: text)
            return
        end if

        ! For single-line selection only (for now)
        if (cursor%selection_start_line == cursor%line) then
            line = buffer_get_line(buffer, cursor%line)
            start_col = min(cursor%selection_start_col, cursor%column)
            end_col = max(cursor%selection_start_col, cursor%column) - 1

            if (start_col <= len(line) .and. end_col <= len(line)) then
                text = line(start_col:end_col)
            else
                allocate(character(len=0) :: text)
            end if
            if (allocated(line)) deallocate(line)
        else
            allocate(character(len=0) :: text)
        end if
    end function get_selected_text

    subroutine find_word_boundaries(line, pos, word_start, word_end)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(out) :: word_start, word_end
        integer :: i

        word_start = 0
        word_end = 0

        ! Check if we're on a word character
        if (pos <= len(line)) then
            if (.not. is_word_char(line(pos:pos))) then
                return
            end if

            ! Find start of word
            word_start = pos
            do i = pos - 1, 1, -1
                if (is_word_char(line(i:i))) then
                    word_start = i
                else
                    exit
                end if
            end do

            ! Find end of word
            word_end = pos
            do i = pos + 1, len(line)
                if (is_word_char(line(i:i))) then
                    word_end = i
                else
                    exit
                end if
            end do
        end if
    end subroutine find_word_boundaries

    subroutine find_next_occurrence(buffer, pattern, start_line, start_col, &
                                    found, found_line, found_col)
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: start_line, start_col
        logical, intent(out) :: found
        integer, intent(out) :: found_line, found_col
        character(len=:), allocatable :: line
        integer :: line_count, current_line, pos
        integer :: search_col

        found = .false.
        found_line = 0
        found_col = 0
        line_count = buffer_get_line_count(buffer)

        ! Search from current position to end
        do current_line = start_line, line_count
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                search_col = start_col + 1
            else
                search_col = 1
            end if

            pos = index(line(search_col:), pattern)
            if (pos > 0) then
                found = .true.
                found_line = current_line
                found_col = search_col + pos - 1
                if (allocated(line)) deallocate(line)
                return
            end if
            if (allocated(line)) deallocate(line)
        end do

        ! Wrap around to beginning
        do current_line = 1, start_line
            line = buffer_get_line(buffer, current_line)

            if (current_line == start_line) then
                ! Search only up to start position
                if (start_col > 1) then
                    pos = index(line(1:start_col-1), pattern)
                else
                    pos = 0
                end if
            else
                pos = index(line, pattern)
            end if

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

    ! ========================================================================
    ! Buffer Helper Functions - Wrappers for cursor-based operations
    ! ========================================================================

    subroutine buffer_delete_at_cursor(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: pos

        ! Convert cursor position to buffer position
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        if (pos > 0 .and. pos <= get_buffer_content_size(buffer)) then
            call buffer_delete(buffer, pos, 1)
        end if
    end subroutine buffer_delete_at_cursor

    subroutine buffer_insert_char(buffer, cursor, ch)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        character, intent(in) :: ch
        integer :: pos

        ! Convert cursor position to buffer position
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, ch)
    end subroutine buffer_insert_char

    subroutine buffer_insert_newline(buffer, cursor)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(in) :: cursor
        integer :: pos

        ! Convert cursor position to buffer position
        pos = get_buffer_position(buffer, cursor%line, cursor%column)
        call buffer_insert(buffer, pos, char(10))
    end subroutine buffer_insert_newline

    subroutine buffer_insert_text_at(buffer, line, column, text)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: text
        integer :: pos

        ! Convert line/column to buffer position
        pos = get_buffer_position(buffer, line, column)
        call buffer_insert(buffer, pos, text)
    end subroutine buffer_insert_text_at

    subroutine buffer_delete_range(buffer, start_line, start_col, end_line, end_col)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: start_line, start_col, end_line, end_col
        integer :: start_pos, end_pos, count

        ! Convert positions to buffer positions
        start_pos = get_buffer_position(buffer, start_line, start_col)
        end_pos = get_buffer_position(buffer, end_line, end_col)
        count = end_pos - start_pos

        if (count > 0) then
            call buffer_delete(buffer, start_pos, count)
        end if
    end subroutine buffer_delete_range

    function get_buffer_position(buffer, line, column) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line, column
        integer :: pos
        integer :: current_line, i, col_in_line
        character :: ch

        pos = 1
        current_line = 1
        col_in_line = 1

        ! Find the position for the given line and column
        do i = 1, get_buffer_content_size(buffer)
            if (current_line == line .and. col_in_line == column) then
                pos = i
                return
            end if

            ch = buffer_get_char(buffer, i)
            if (ch == char(10)) then
                if (current_line == line) then
                    ! We're at the end of the target line
                    pos = i
                    return
                end if
                current_line = current_line + 1
                col_in_line = 1
            else
                col_in_line = col_in_line + 1
            end if
        end do

        ! If we reach here, we're at the end of the buffer
        pos = get_buffer_content_size(buffer) + 1
    end function get_buffer_position

    function get_buffer_content_size(buffer) result(size)
        type(buffer_t), intent(in) :: buffer
        integer :: size

        size = buffer%size - (buffer%gap_end - buffer%gap_start)
    end function get_buffer_content_size

    ! ========================================================================
    ! Multiple Cursor Addition Above/Below
    ! ========================================================================

    subroutine add_cursor_above(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), allocatable :: new_cursors(:)
        type(cursor_t) :: active_cursor
        integer :: i, new_line

        active_cursor = editor%cursors(editor%active_cursor)
        new_line = active_cursor%line - 1

        ! Check if we can add a cursor above
        if (new_line < 1) return

        ! Allocate space for additional cursor
        allocate(new_cursors(size(editor%cursors) + 1))

        ! Copy existing cursors
        do i = 1, size(editor%cursors)
            new_cursors(i) = editor%cursors(i)
        end do

        ! Add new cursor above
        new_cursors(size(new_cursors))%line = new_line
        new_cursors(size(new_cursors))%column = active_cursor%column
        new_cursors(size(new_cursors))%desired_column = active_cursor%desired_column
        new_cursors(size(new_cursors))%has_selection = .false.

        ! Replace cursors array
        call move_alloc(new_cursors, editor%cursors)

        ! Set the new cursor as active
        editor%active_cursor = size(editor%cursors)
    end subroutine add_cursor_above

    subroutine add_cursor_below(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        type(cursor_t), allocatable :: new_cursors(:)
        type(cursor_t) :: active_cursor
        integer :: i, new_line, line_count

        active_cursor = editor%cursors(editor%active_cursor)
        line_count = buffer_get_line_count(buffer)
        new_line = active_cursor%line + 1

        ! Check if we can add a cursor below
        if (new_line > line_count) return

        ! Allocate space for additional cursor
        allocate(new_cursors(size(editor%cursors) + 1))

        ! Copy existing cursors
        do i = 1, size(editor%cursors)
            new_cursors(i) = editor%cursors(i)
        end do

        ! Add new cursor below
        new_cursors(size(new_cursors))%line = new_line
        new_cursors(size(new_cursors))%column = active_cursor%column
        new_cursors(size(new_cursors))%desired_column = active_cursor%desired_column
        new_cursors(size(new_cursors))%has_selection = .false.

        ! Replace cursors array
        call move_alloc(new_cursors, editor%cursors)

        ! Set the new cursor as active
        editor%active_cursor = size(editor%cursors)
    end subroutine add_cursor_below

    subroutine jump_to_matching_bracket(editor, buffer)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        logical :: found
        integer :: match_line, match_col

        ! Find matching bracket from current cursor position
        call find_matching_bracket(buffer, &
                                  editor%cursors(editor%active_cursor)%line, &
                                  editor%cursors(editor%active_cursor)%column, &
                                  found, match_line, match_col)

        if (found) then
            ! Jump to the matching bracket
            editor%cursors(editor%active_cursor)%line = match_line
            editor%cursors(editor%active_cursor)%column = match_col
            editor%cursors(editor%active_cursor)%desired_column = match_col

            ! Update viewport to ensure cursor is visible
            call update_viewport(editor)
        end if
    end subroutine jump_to_matching_bracket

    ! ========================================================================
    ! Selection Extension Subroutines
    ! ========================================================================

    subroutine extend_selection_up(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: current_line, target_line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor up
        if (cursor%line > 1) then
            current_line = buffer_get_line(buffer, cursor%line)
            cursor%line = cursor%line - 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from empty line, go to end of target line
            if (len(current_line) == 0) then
                cursor%column = len(target_line) + 1
                cursor%desired_column = cursor%column
            else
                cursor%column = cursor%desired_column
                if (cursor%column > len(target_line) + 1) then
                    cursor%column = len(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine extend_selection_up

    subroutine extend_selection_down(cursor, buffer, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_count
        character(len=:), allocatable :: current_line, target_line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor down
        if (cursor%line < line_count) then
            current_line = buffer_get_line(buffer, cursor%line)
            cursor%line = cursor%line + 1
            target_line = buffer_get_line(buffer, cursor%line)

            ! If coming from empty line, go to column 1 of target line
            if (len(current_line) == 0) then
                cursor%column = 1
                cursor%desired_column = 1
            else
                cursor%column = cursor%desired_column
                if (cursor%column > len(target_line) + 1) then
                    cursor%column = len(target_line) + 1
                end if
            end if

            if (allocated(current_line)) deallocate(current_line)
            if (allocated(target_line)) deallocate(target_line)
        end if
    end subroutine extend_selection_down

    subroutine extend_selection_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        ! Move cursor left
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
    end subroutine extend_selection_left

    subroutine extend_selection_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: line_count

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)

        ! Move cursor right
        if (cursor%column <= len(line)) then
            cursor%column = cursor%column + 1
            cursor%desired_column = cursor%column
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
            cursor%desired_column = cursor%column
        end if

        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_right

    subroutine extend_selection_home(cursor)
        type(cursor_t), intent(inout) :: cursor

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        cursor%column = 1
        cursor%desired_column = 1
    end subroutine extend_selection_home

    subroutine extend_selection_end(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        cursor%column = len(line) + 1
        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_end

    subroutine extend_selection_page_up(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        page_size = editor%screen_rows - 2  ! Leave room for status bar
        cursor%line = max(1, cursor%line - page_size)
        cursor%column = cursor%desired_column
    end subroutine extend_selection_page_up

    subroutine extend_selection_page_down(cursor, editor, line_count)
        type(cursor_t), intent(inout) :: cursor
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: line_count
        integer :: page_size

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        page_size = editor%screen_rows - 2  ! Leave room for status bar
        cursor%line = min(line_count, cursor%line + page_size)
        cursor%column = cursor%desired_column
    end subroutine extend_selection_page_down

    subroutine extend_selection_word_left(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_len
        logical :: in_word

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        pos = cursor%column

        ! Handle empty lines
        if (line_len == 0) then
            if (cursor%line > 1) then
                cursor%line = cursor%line - 1
                if (allocated(line)) deallocate(line)
                line = buffer_get_line(buffer, cursor%line)
                cursor%column = len(line) + 1
            else
                cursor%column = 1
            end if
            cursor%desired_column = cursor%column
            if (allocated(line)) deallocate(line)
            return
        end if

        if (pos > 1 .and. line_len > 0) then
            ! Simple algorithm: move left one position at a time until we find a word start
            pos = pos - 1  ! Move left one position

            ! Skip any whitespace
            do while (pos > 1 .and. pos <= line_len)
                if (line(pos:pos) /= ' ') exit
                pos = pos - 1
            end do

            ! If we're on a word character, go to the start of this word
            if (pos >= 1 .and. pos <= line_len) then
                if (is_word_char(line(pos:pos))) then
                    ! Move to the start of the current word
                    do while (pos > 1)
                        if (pos-1 < 1) exit  ! Safety check
                        if (.not. is_word_char(line(pos-1:pos-1))) exit
                        pos = pos - 1
                    end do
                end if
            end if

            ! Clamp to valid range
            if (pos < 1) pos = 1
            if (pos > line_len + 1) pos = line_len + 1

            cursor%column = pos
        else if (cursor%line > 1) then
            ! Move to end of previous line
            cursor%line = cursor%line - 1
            if (allocated(line)) deallocate(line)
            line = buffer_get_line(buffer, cursor%line)
            cursor%column = len(line) + 1
        else
            cursor%column = 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_word_left

    subroutine extend_selection_word_right(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: line
        integer :: pos, line_count, line_len
        logical :: in_word

        ! Initialize selection if not already started
        if (.not. cursor%has_selection) then
            cursor%has_selection = .true.
            cursor%selection_start_line = cursor%line
            cursor%selection_start_col = cursor%column
        end if

        line = buffer_get_line(buffer, cursor%line)
        line_count = buffer_get_line_count(buffer)
        line_len = len(line)
        pos = cursor%column

        ! Clamp position to valid range
        if (pos > line_len + 1) pos = line_len + 1
        if (pos < 1) pos = 1

        if (line_len == 0 .or. pos > line_len) then
            ! At end of line or empty line - move to next line
            if (cursor%line < line_count) then
                cursor%line = cursor%line + 1
                cursor%column = 1
            else
                cursor%column = line_len + 1
            end if
        else if (pos >= 1 .and. pos <= line_len) then
            ! Check what we're currently on (with bounds checking)
            if (is_word_char(line(pos:pos))) then
                ! We're on a word character - skip to end of word
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (.not. is_word_char(line(pos+1:pos+1))) exit
                    end if
                    pos = pos + 1
                end do
                pos = pos + 1  ! Move past the word
            else
                ! We're on whitespace or punctuation - skip to next word
                ! Skip non-word characters
                do while (pos < line_len)
                    if (pos+1 <= line_len) then
                        if (is_word_char(line(pos+1:pos+1))) exit
                    end if
                    pos = pos + 1
                end do

                ! If we found a word, move to its end
                if (pos < line_len .and. pos+1 <= line_len) then
                    pos = pos + 1  ! Move to start of word
                    do while (pos < line_len)
                        if (pos+1 <= line_len) then
                            if (.not. is_word_char(line(pos+1:pos+1))) exit
                        end if
                        pos = pos + 1
                    end do
                    pos = pos + 1  ! Move past the word
                else
                    pos = line_len + 1  ! At end of line
                end if
            end if

            cursor%column = pos
        else if (cursor%line < line_count) then
            ! Move to start of next line
            cursor%line = cursor%line + 1
            cursor%column = 1
        else
            cursor%column = line_len + 1
        end if

        cursor%desired_column = cursor%column
        if (allocated(line)) deallocate(line)
    end subroutine extend_selection_word_right

    ! ========================================================================
    ! Word Deletion Subroutines
    ! ========================================================================

    subroutine delete_word_forward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: start_col, end_col, line_len
        logical :: in_word

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        start_col = cursor%column
        end_col = start_col

        ! If cursor is past end of line, do nothing (can't delete forward from past the line end)
        if (start_col > line_len + 1) then
            if (allocated(line)) deallocate(line)
            return
        end if

        if (end_col <= line_len) then
            ! Skip current word (use nested ifs to avoid bounds issues)
            in_word = is_word_char(line(end_col:end_col))
            do while (end_col < line_len)
                if (is_word_char(line(end_col:end_col)) .eqv. in_word) then
                    end_col = end_col + 1
                else
                    exit
                end if
            end do

            ! Check if we're still on the same word type at end_col
            if (end_col <= line_len) then
                if (is_word_char(line(end_col:end_col)) .eqv. in_word) then
                    end_col = end_col + 1
                end if
            end if

            ! Skip trailing whitespace
            do while (end_col <= line_len)
                if (line(end_col:end_col) == ' ') then
                    end_col = end_col + 1
                else
                    exit
                end if
            end do

            ! Delete from cursor to end position
            if (end_col > start_col) then
                call delete_range(buffer, cursor%line, start_col, cursor%line, end_col - 1)
                buffer%modified = .true.
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_word_forward

    subroutine delete_word_backward(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        integer :: start_col, end_col, line_len
        logical :: in_word

        line = buffer_get_line(buffer, cursor%line)
        line_len = len(line)
        end_col = cursor%column - 1
        start_col = end_col

        ! Skip whitespace to the left (use nested ifs for safety)
        do while (start_col > 0 .and. start_col <= line_len)
            if (line(start_col:start_col) == ' ') then
                start_col = start_col - 1
            else
                exit
            end if
        end do

        ! Delete word to the left
        if (start_col > 0 .and. start_col <= line_len) then
            in_word = is_word_char(line(start_col:start_col))
            do while (start_col > 1)
                if (is_word_char(line(start_col-1:start_col-1)) .eqv. in_word) then
                    start_col = start_col - 1
                else
                    exit
                end if
            end do

            ! Delete from start position to cursor
            if (start_col <= end_col) then
                call delete_range(buffer, cursor%line, start_col, cursor%line, end_col)
                cursor%column = start_col
                cursor%desired_column = cursor%column
                buffer%modified = .true.
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine delete_word_backward

    ! ========================================================================
    ! Character Transpose Subroutine
    ! ========================================================================

    subroutine transpose_characters(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: line
        character :: temp_char
        integer :: pos1, pos2

        line = buffer_get_line(buffer, cursor%line)

        if (cursor%column > 1 .and. cursor%column <= len(line) + 1) then
            if (cursor%column == len(line) + 1) then
                ! At end of line, swap last two characters
                pos1 = cursor%column - 2
                pos2 = cursor%column - 1
            else
                ! In middle of line, swap character before cursor with character at cursor
                pos1 = cursor%column - 1
                pos2 = cursor%column
            end if

            if (pos1 >= 1 .and. pos2 <= len(line)) then
                ! Get the two characters
                temp_char = line(pos1:pos1)

                ! Delete the first character
                call delete_range(buffer, cursor%line, pos1, cursor%line, pos1)

                ! Insert it after the second position
                call insert_char_at(buffer, cursor%line, pos2, temp_char)

                ! Move cursor forward if not at end of line
                if (cursor%column < len(line) + 1) then
                    cursor%column = cursor%column + 1
                    cursor%desired_column = cursor%column
                end if

                buffer%modified = .true.
            end if
        end if

        if (allocated(line)) deallocate(line)
    end subroutine transpose_characters

    subroutine join_lines(cursor, buffer)
        type(cursor_t), intent(inout) :: cursor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: current_line, next_line
        integer :: line_count, current_len, leading_spaces

        line_count = buffer_get_line_count(buffer)

        ! Can't join if we're on the last line
        if (cursor%line >= line_count) return

        ! Get the current line and next line
        current_line = buffer_get_line(buffer, cursor%line)
        next_line = buffer_get_line(buffer, cursor%line + 1)
        current_len = len(current_line)

        ! Count leading whitespace in next line
        leading_spaces = 0
        do while (leading_spaces < len(next_line) .and. &
                 (next_line(leading_spaces + 1:leading_spaces + 1) == ' ' .or. &
                  next_line(leading_spaces + 1:leading_spaces + 1) == char(9)))
            leading_spaces = leading_spaces + 1
        end do

        ! Delete the newline and leading whitespace from next line
        if (leading_spaces > 0) then
            call buffer_delete_range(buffer, cursor%line, current_len + 1, cursor%line + 1, leading_spaces + 1)
        else
            call buffer_delete_range(buffer, cursor%line, current_len + 1, cursor%line + 1, 1)
        end if

        ! If the next line had non-whitespace content, insert a space between the lines
        if (leading_spaces < len(next_line)) then
            ! Insert a space if current line doesn't end with space
            if (current_len > 0) then
                if (current_line(current_len:current_len) /= ' ') then
                    call buffer_insert_text_at(buffer, cursor%line, current_len + 1, ' ')
                end if
            end if
        end if

        if (allocated(current_line)) deallocate(current_line)
        if (allocated(next_line)) deallocate(next_line)
    end subroutine join_lines

    function get_line_start_pos(buffer, line_num) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer :: pos
        integer :: i, current_line

        pos = 1
        current_line = 1

        ! Find the start position of the given line
        do i = 1, buffer%size
            if (current_line == line_num) then
                return
            end if

            if (buffer_get_char_at(buffer, i) == char(10)) then  ! Newline
                current_line = current_line + 1
                pos = i + 1
            end if
        end do

        ! If line_num is beyond the last line
        if (current_line < line_num) then
            pos = buffer%size + 1
        end if
    end function get_line_start_pos

    subroutine delete_range(buffer, start_line, start_col, end_line, end_col)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: start_line, start_col, end_line, end_col
        integer :: pos

        ! For now, handle single-line deletions
        if (start_line == end_line) then
            ! Calculate buffer position
            pos = get_line_start_pos(buffer, start_line) + start_col - 1

            ! Move gap to deletion point
            call buffer_move_gap(buffer, pos)

            ! Extend gap to delete characters
            buffer%gap_end = buffer%gap_end + (end_col - start_col + 1)
        end if
    end subroutine delete_range

    subroutine insert_char_at(buffer, line_num, col, ch)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_num, col
        character, intent(in) :: ch
        integer :: pos

        ! Calculate buffer position
        pos = get_line_start_pos(buffer, line_num) + col - 1

        ! Move gap to insertion point
        call buffer_move_gap(buffer, pos)

        ! Insert character
        buffer%data(buffer%gap_start:buffer%gap_start) = ch
        buffer%gap_start = buffer%gap_start + 1
    end subroutine insert_char_at

    ! Handle input when in fuss mode
    subroutine handle_fuss_input(key_str, editor, buffer)
        character(len=*), intent(in) :: key_str
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: selected_path
        integer :: status, i

        select case(trim(key_str))
        case('j', 'down')
            ! Move down in tree
            call tree_move_down(tree_state)

        case('k', 'up')
            ! Move up in tree
            call tree_move_up(tree_state)

        case('left')
            ! Move up to parent directory
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (associated(tree_state%selectable_files(tree_state%selected_index)%node)) then
                    if (associated(tree_state%selectable_files(tree_state%selected_index)%node%parent)) then
                        ! Find the parent in the selectable list
                        do i = 1, tree_state%n_selectable
                            if (associated(tree_state%selectable_files(i)%node, &
                                         tree_state%selectable_files(tree_state%selected_index)%node%parent)) then
                                tree_state%selected_index = i
                                exit
                            end if
                        end do
                    end if
                end if
            end if

        case('right')
            ! Move into first child of directory (and expand if needed)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (tree_state%selectable_files(tree_state%selected_index)%is_directory .and. &
                    associated(tree_state%selectable_files(tree_state%selected_index)%node)) then
                    ! Expand if collapsed
                    if (.not. tree_state%selectable_files(tree_state%selected_index)%node%expanded) then
                        tree_state%selectable_files(tree_state%selected_index)%node%expanded = .true.
                        ! Rebuild selectable list
                        if (allocated(tree_state%selectable_files)) deallocate(tree_state%selectable_files)
                        call build_selectable_list(tree_state%root, tree_state%selectable_files, tree_state%n_selectable)
                    end if
                    ! Find first child in selectable list (look for item whose parent is current node)
                    do i = tree_state%selected_index + 1, tree_state%n_selectable
                        if (associated(tree_state%selectable_files(i)%node)) then
                            if (associated(tree_state%selectable_files(i)%node%parent, &
                                         tree_state%selectable_files(tree_state%selected_index)%node)) then
                                tree_state%selected_index = i
                                exit
                            end if
                        end if
                    end do
                end if
            end if

        case(' ', 'space')
            ! Toggle directory expand/collapse
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (.not. tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                    ! Not a directory - do nothing
                else if (associated(tree_state%selectable_files(tree_state%selected_index)%node)) then
                    ! Toggle expanded
                    tree_state%selectable_files(tree_state%selected_index)%node%expanded = &
                        .not. tree_state%selectable_files(tree_state%selected_index)%node%expanded
                    ! Rebuild selectable list
                    if (allocated(tree_state%selectable_files)) deallocate(tree_state%selectable_files)
                    call build_selectable_list(tree_state%root, tree_state%selectable_files, tree_state%n_selectable)
                    ! Clamp selection
                    if (tree_state%selected_index > tree_state%n_selectable .and. tree_state%n_selectable > 0) then
                        tree_state%selected_index = tree_state%n_selectable
                    end if
                end if
            end if

        case('a')
            ! Stage file
            if (allocated(editor%workspace_path)) then
                call tree_stage_file(tree_state, editor%workspace_path)
            end if

        case('u')
            ! Unstage file
            if (allocated(editor%workspace_path)) then
                call tree_unstage_file(tree_state, editor%workspace_path)
            end if

        case('m')
            ! Git commit with message
            if (allocated(editor%workspace_path)) then
                call handle_git_commit(editor)
            end if

        case('p')
            ! Git push
            if (allocated(editor%workspace_path)) then
                call handle_git_push(editor)
            end if

        case('f')
            ! Git fetch
            if (allocated(editor%workspace_path)) then
                call handle_git_fetch(editor)
            end if

        case('l')
            ! Git pull
            if (allocated(editor%workspace_path)) then
                call handle_git_pull(editor)
            end if

        case('t')
            ! Git tag
            if (allocated(editor%workspace_path)) then
                call handle_git_tag(editor)
            end if

        case('d')
            ! Git diff
            if (allocated(editor%workspace_path)) then
                call handle_git_diff(editor, buffer)
            end if

        case('enter', 'o')
            ! Open file in editor (only for files, not directories)
            if (tree_state%selected_index >= 1 .and. tree_state%selected_index <= tree_state%n_selectable) then
                if (.not. tree_state%selectable_files(tree_state%selected_index)%is_directory) then
                    selected_path = get_selected_item_path(tree_state)
                    if (len_trim(selected_path) > 0) then
                        call open_file_in_editor(selected_path, editor, buffer)
                    end if
                end if
            end if

        case('esc')
            ! Exit fuss mode
            editor%fuss_mode_active = .false.
            call cleanup_tree_state(tree_state)

        end select
    end subroutine handle_fuss_input

    ! Open a file in the editor
    subroutine open_file_in_editor(file_path, editor, buffer)
        use editor_state_module, only: create_tab
        use text_buffer_module, only: copy_buffer
        character(len=*), intent(in) :: file_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: full_path
        integer :: status

        ! Build full path
        if (allocated(editor%workspace_path)) then
            full_path = trim(editor%workspace_path) // '/' // trim(file_path)
        else
            full_path = trim(file_path)
        end if

        ! Create a new tab for this file
        call create_tab(editor, full_path)

        ! Load file into the new tab's buffer
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            call buffer_load_file(editor%tabs(editor%active_tab_index)%buffer, full_path, status)
            if (status == 0) then
                ! Copy tab's buffer to main buffer so it's displayed
                call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%buffer)

                ! Update editor state with the new tab's info
                if (allocated(editor%filename)) deallocate(editor%filename)
                allocate(character(len=len_trim(full_path)) :: editor%filename)
                editor%filename = full_path

                ! Reset cursor to top of file
                editor%cursors(editor%active_cursor)%line = 1
                editor%cursors(editor%active_cursor)%column = 1
                editor%cursors(editor%active_cursor)%desired_column = 1
                editor%viewport_line = 1
                editor%viewport_column = 1

                ! Also update tab state
                editor%tabs(editor%active_tab_index)%cursors(1)%line = 1
                editor%tabs(editor%active_tab_index)%cursors(1)%column = 1
                editor%tabs(editor%active_tab_index)%cursors(1)%desired_column = 1
                editor%tabs(editor%active_tab_index)%viewport_line = 1
                editor%tabs(editor%active_tab_index)%viewport_column = 1
            end if
        end if
        ! Note: fuss mode stays active - user must press ctrl-b to exit
    end subroutine open_file_in_editor

    ! Toggle fuss mode (file tree)
    subroutine toggle_fuss_mode(editor)
        type(editor_state_t), intent(inout) :: editor

        editor%fuss_mode_active = .not. editor%fuss_mode_active

        if (editor%fuss_mode_active) then
            ! Entering fuss mode - initialize tree state
            if (allocated(editor%workspace_path)) then
                call init_tree_state(tree_state, editor%workspace_path)
            end if
        else
            ! Exiting fuss mode - cleanup tree state
            call cleanup_tree_state(tree_state)
        end if
    end subroutine toggle_fuss_mode

    ! Handle git commit with message prompt
    subroutine handle_git_commit(editor)
        type(editor_state_t), intent(inout) :: editor
        character(len=512) :: commit_message
        logical :: cancelled, success

        ! Show prompt for commit message
        call show_text_prompt('Commit message: ', commit_message, cancelled, editor%screen_rows)

        if (.not. cancelled .and. len_trim(commit_message) > 0) then
            call git_commit(editor%workspace_path, commit_message, success)

            ! Show feedback message
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(repeat(' ', 200))
            call terminal_move_cursor(editor%screen_rows, 1)
            if (success) then
                call terminal_write(char(27) // '[32m✓ Committed successfully!' // char(27) // '[0m')
            else
                call terminal_write(char(27) // '[31m✗ Commit failed (nothing staged?)' // char(27) // '[0m')
            end if

            ! Brief pause
            call execute_command_line('sleep 1')

            ! Refresh tree
            call refresh_tree_state(tree_state, editor%workspace_path)
        end if
    end subroutine handle_git_commit

    ! Handle git push
    subroutine handle_git_push(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Pushing to remote...')

        call git_push(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Pushed successfully!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Push failed (check remote/branch)' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_push

    ! Handle git fetch
    subroutine handle_git_fetch(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Fetching from remote...')

        call git_fetch(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Fetch completed!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Fetch failed!' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_fetch

    ! Handle git pull
    subroutine handle_git_pull(editor)
        type(editor_state_t), intent(inout) :: editor
        logical :: success

        ! Show progress message
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write('Pulling from remote...')

        call git_pull(editor%workspace_path, success)

        ! Show result
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_write(repeat(' ', 200))
        call terminal_move_cursor(editor%screen_rows, 1)
        if (success) then
            call terminal_write(char(27) // '[32m✓ Pull completed!' // char(27) // '[0m')
        else
            call terminal_write(char(27) // '[31m✗ Pull failed!' // char(27) // '[0m')
        end if

        ! Brief pause
        call execute_command_line('sleep 1')

        ! Refresh tree
        call refresh_tree_state(tree_state, editor%workspace_path)
    end subroutine handle_git_pull

    ! Handle git tag
    subroutine handle_git_tag(editor)
        type(editor_state_t), intent(inout) :: editor
        character(len=256) :: tag_name, tag_message
        logical :: cancelled, success

        ! Show prompt for tag name
        call show_text_prompt('Tag name: ', tag_name, cancelled, editor%screen_rows)

        if (.not. cancelled .and. len_trim(tag_name) > 0) then
            ! Show prompt for tag message (optional)
            call show_text_prompt('Tag message (optional): ', tag_message, cancelled, editor%screen_rows)

            if (.not. cancelled) then
                call git_tag(editor%workspace_path, tag_name, tag_message, success)

                ! Show result
                call terminal_move_cursor(editor%screen_rows, 1)
                call terminal_write(repeat(' ', 200))
                call terminal_move_cursor(editor%screen_rows, 1)
                if (success) then
                    call terminal_write(char(27) // '[32m✓ Tag created: ' // trim(tag_name) // char(27) // '[0m')
                else
                    call terminal_write(char(27) // '[31m✗ Failed to create tag' // char(27) // '[0m')
                end if

                ! Brief pause
                call execute_command_line('sleep 1')

                ! Refresh tree
                call refresh_tree_state(tree_state, editor%workspace_path)
            end if
        end if
    end subroutine handle_git_tag

    subroutine handle_git_diff(editor, buffer)
        use editor_state_module, only: create_tab
        use text_buffer_module, only: buffer_insert, copy_buffer
        use file_tree_module, only: get_selected_item_path
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=:), allocatable :: selected_path, diff_content, tab_name
        character(len=256) :: branch_name
        logical :: success

        ! Get selected file from tree
        if (tree_state%selected_index < 1 .or. tree_state%selected_index > tree_state%n_selectable) return
        if (tree_state%selectable_files(tree_state%selected_index)%is_directory) return

        selected_path = get_selected_item_path(tree_state)
        if (len_trim(selected_path) == 0) return

        ! Get diff content
        call git_diff_file(editor%workspace_path, selected_path, diff_content, branch_name, success)

        if (.not. success) then
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(repeat(' ', 200))
            call terminal_move_cursor(editor%screen_rows, 1)
            call terminal_write(char(27) // '[31m✗ Failed to get diff' // char(27) // '[0m')
            call execute_command_line('sleep 1')
            return
        end if

        ! Create tab name: diff:<filename>:<branch>
        if (len_trim(branch_name) > 0) then
            tab_name = 'diff:' // trim(selected_path) // ':' // trim(branch_name)
        else
            tab_name = 'diff:' // trim(selected_path)
        end if

        ! Create new tab
        call create_tab(editor, tab_name)

        ! Load diff content into the tab's buffer
        if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
            ! Insert diff content at the beginning of the buffer
            call buffer_insert(editor%tabs(editor%active_tab_index)%buffer, 1, diff_content)

            ! Copy tab's buffer to main buffer so it's displayed
            call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%buffer)

            ! Update editor state with the new tab's info
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len_trim(tab_name)) :: editor%filename)
            editor%filename = tab_name

            ! Reset cursor to top of file
            editor%cursors(editor%active_cursor)%line = 1
            editor%cursors(editor%active_cursor)%column = 1
            editor%cursors(editor%active_cursor)%desired_column = 1

            ! Exit fuss mode and show diff
            editor%fuss_mode_active = .false.
        end if
    end subroutine handle_git_diff

end module command_handler_module