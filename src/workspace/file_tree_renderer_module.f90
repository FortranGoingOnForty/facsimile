module file_tree_renderer_module
    use iso_fortran_env, only: int32
    use file_tree_module
    use terminal_io_module
    implicit none
    private

    public :: render_file_tree

    ! Simple expansion indicators (no box-drawing)
    character(len=*), parameter :: EXPANDED_DIR = '-'
    character(len=*), parameter :: COLLAPSED_DIR = '+'
    character(len=1), parameter :: ESC = achar(27)

contains

    subroutine render_file_tree(state, start_row, end_row, start_col, width, hints_expanded, git_prefix_active)
        type(tree_state_t), intent(in) :: state
        integer, intent(in) :: start_row, end_row, start_col, width
        logical, intent(in) :: hints_expanded
        logical, intent(in), optional :: git_prefix_active
        logical :: git_mode
        integer :: current_row, item_idx, row
        character(len=512) :: status_line
        character(len=:), allocatable :: padding

        ! Handle optional git_prefix_active parameter
        if (present(git_prefix_active)) then
            git_mode = git_prefix_active
        else
            git_mode = .false.
        end if

        ! First, clear all rows in the tree pane (start at column 1 to
        ! overwrite any characters that wrapped from the editor pane,
        ! and cover through the column just before the separator)
        padding = repeat(' ', width + start_col)
        do row = start_row, end_row
            call terminal_move_cursor(row, 1)
            call terminal_write(padding)
        end do

        current_row = start_row

        ! Display repo:branch info at top if available
        if (len_trim(state%repo_name) > 0 .and. len_trim(state%branch_name) > 0) then
            call terminal_move_cursor(current_row, start_col)
            write(status_line, '(A,A,A,A,A,A,A)') &
                ESC // '[1;36m', trim(state%repo_name), ESC // '[0m', &
                ':', &
                ESC // '[1;33m', trim(state%branch_name), ESC // '[0m'
            call terminal_write(trim(status_line))
            current_row = current_row + 2  ! Skip a line
        end if

        ! Display root
        if (current_row <= end_row) then
            call terminal_move_cursor(current_row, start_col)
            call terminal_write('.')
            current_row = current_row + 1
        end if

        ! Display tree (leave space for legend - 1 or 6 rows)
        if (associated(state%root)) then
            item_idx = 0
            if (hints_expanded) then
                call render_tree_node(state%root, '', .true., &
                                    state, item_idx, current_row, end_row - 6, start_col, width)
            else
                call render_tree_node(state%root, '', .true., &
                                    state, item_idx, current_row, end_row - 2, start_col, width)
            end if
        end if

        ! Display legend at bottom (either minimal or expanded)
        if (hints_expanded) then
            ! Expanded legend (six rows, short lines to fit pane)
            if (end_row >= start_row + 6) then
                call terminal_move_cursor(end_row - 5, start_col)
                call terminal_write(ESC // '[90m' // 'j/k:nav o:open' // ESC // '[0m')

                call terminal_move_cursor(end_row - 4, start_col)
                call terminal_write(ESC // '[90m' // '→:in ←:up .:hide' // ESC // '[0m')

                call terminal_move_cursor(end_row - 3, start_col)
                call terminal_write(ESC // '[90m' // 'spc:toggle' // ESC // '[0m')

                if (git_mode) then
                    call terminal_move_cursor(end_row - 2, start_col)
                    call terminal_write(ESC // '[1;33m' // 'a:stage u:unstage' // ESC // '[0m')

                    call terminal_move_cursor(end_row - 1, start_col)
                    call terminal_write(ESC // '[1;33m' // 'd:diff m:commit' // ESC // '[0m')
                else
                    call terminal_move_cursor(end_row - 2, start_col)
                    call terminal_write(ESC // '[90m' // 'alt-v:vs alt-s:hs' // ESC // '[0m')

                    call terminal_move_cursor(end_row - 1, start_col)
                    call terminal_write(ESC // '[90m' // 'ctrl-g:git  type:search' // ESC // '[0m')
                end if

                call terminal_move_cursor(end_row, start_col)
                call terminal_write(ESC // '[90m' // 'ctrl-/:less esc:close' // ESC // '[0m')
            end if
        else
            ! Minimal legend (two rows)
            if (end_row >= start_row + 2) then
                call terminal_move_cursor(end_row - 1, start_col)
                call terminal_write(ESC // '[90m' // '.:hide ctrl-/:hints' // ESC // '[0m')

                call terminal_move_cursor(end_row, start_col)
                call terminal_write(ESC // '[90m' // 'esc/F3:close' // ESC // '[0m')
            end if
        end if
    end subroutine render_file_tree

    recursive subroutine render_tree_node(node, prefix, is_root, &
                                         state, item_idx, current_row, end_row, start_col, width)
        type(tree_node_t), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_root
        type(tree_state_t), intent(in) :: state
        integer, intent(inout) :: item_idx, current_row
        integer, intent(in) :: end_row, start_col, width

        character(len=:), allocatable :: line, new_prefix
        character(len=:), allocatable :: base_line
        integer :: visible_len
        type(tree_node_t), pointer :: child
        logical :: is_selected, is_last_child

        ! Don't print root node
        if (.not. is_root) then
            ! Skip hidden files when hide_dotfiles is enabled (dotfiles or gitignored)
            if (state%hide_dotfiles .and. node%is_file .and. (node%is_dotfile .or. node%is_gitignored)) then
                return
            end if

            ! Increment item_idx for both files and directories (all selectable items)
            item_idx = item_idx + 1
            is_selected = (item_idx == state%selected_index)

            ! Only render if within viewport and current_row fits
            if (item_idx >= state%viewport_offset .and. current_row <= end_row) then
                ! Build line with simple +/- indicators and indentation
                if (.not. node%is_file) then
                    ! Directory - add expand/collapse indicator and / suffix
                    if (associated(node%first_child)) then
                        if (node%expanded) then
                            base_line = prefix // EXPANDED_DIR // ' ' // trim(node%name) // '/'
                        else
                            base_line = prefix // COLLAPSED_DIR // ' ' // trim(node%name) // '/'
                        end if
                    else
                        base_line = prefix // '  ' // trim(node%name) // '/'
                    end if
                else
                    base_line = prefix // '  ' // trim(node%name)
                end if

                ! Truncate with ellipsis if line overflows tree pane width
                visible_len = len(base_line)
                if (visible_len > width) then
                    line = base_line(1:width - 3) // '...'
                else
                    line = base_line
                end if

                ! Add status indicators for files only (after truncation
                ! so indicators stay visible at the end)
                if (node%is_file) then
                    if (node%is_staged) then
                        line = line // ' ' // ESC // '[32m↑' // ESC // '[0m'
                    end if
                    if (node%is_unstaged) then
                        line = line // ' ' // ESC // '[31m✗' // ESC // '[0m'
                    end if
                    if (node%is_untracked) then
                        line = line // ' ' // ESC // '[90m✗' // ESC // '[0m'
                    end if
                    if (node%has_incoming) then
                        line = line // ' ' // ESC // '[34m↓' // ESC // '[0m'
                    end if
                end if

                ! Render with selection highlight
                call terminal_move_cursor(current_row, start_col)
                if (is_selected) then
                    call terminal_write(ESC // '[7m' // line // ESC // '[0m')
                else
                    if (.not. node%is_file .and. node%all_children_hidden) then
                        call terminal_write(ESC // '[90m' // line // ESC // '[0m')
                    else
                        call terminal_write(line)
                    end if
                end if

                current_row = current_row + 1
            end if
        end if

        ! Render children (only if directory is expanded or root)
        if ((.not. node%is_file .and. node%expanded) .or. is_root) then
            child => node%first_child
            do while (associated(child))
            is_last_child = .not. associated(child%next_sibling)

            if (is_root) then
                new_prefix = ''
            else
                ! 1-space indentation per level for compact tree
                new_prefix = prefix // ' '
            end if

            call render_tree_node(child, new_prefix, .false., &
                                state, item_idx, current_row, end_row, start_col, width)
            child => child%next_sibling
            end do
        end if
    end subroutine render_tree_node

end module file_tree_renderer_module
