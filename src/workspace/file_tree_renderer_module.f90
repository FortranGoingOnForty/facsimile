module file_tree_renderer_module
    use iso_fortran_env, only: int32
    use file_tree_module
    use terminal_io_module
    implicit none
    private

    public :: render_file_tree

    ! UTF-8 box-drawing characters
    character(len=*), parameter :: BRANCH_LAST = '└──'
    character(len=*), parameter :: BRANCH_MID = '├──'
    character(len=*), parameter :: VERTICAL = '│'
    character(len=1), parameter :: ESC = achar(27)

contains

    subroutine render_file_tree(state, start_row, end_row, start_col, width)
        type(tree_state_t), intent(in) :: state
        integer, intent(in) :: start_row, end_row, start_col, width
        integer :: current_row, item_idx, visible_items, row
        character(len=512) :: status_line
        character(len=:), allocatable :: padding

        ! First, clear all rows in the tree pane
        padding = repeat(' ', width)
        do row = start_row, end_row
            call terminal_move_cursor(row, start_col)
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

        ! Display tree
        if (associated(state%root)) then
            item_idx = 0
            call render_tree_node(state%root, '', .true., .true., &
                                state, item_idx, current_row, end_row, start_col, width)
        end if

        ! Display legend at bottom
        if (end_row > current_row + 1) then
            call terminal_move_cursor(end_row, start_col)
            call terminal_write(ESC // '[90m') ! Gray
            call terminal_write('j/k:nav a:stage u:unstage')
            call terminal_write(ESC // '[0m')
        end if
    end subroutine render_file_tree

    recursive subroutine render_tree_node(node, prefix, is_last, is_root, &
                                         state, item_idx, current_row, end_row, start_col, width)
        type(tree_node_t), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root
        type(tree_state_t), intent(in) :: state
        integer, intent(inout) :: item_idx, current_row
        integer, intent(in) :: end_row, start_col, width

        character(len=1024) :: line, new_prefix
        type(tree_node_t), pointer :: child
        integer :: n_children, i
        logical :: is_selected

        ! Count children
        n_children = 0
        child => node%first_child
        do while (associated(child))
            n_children = n_children + 1
            child => child%next_sibling
        end do

        ! Don't print root node
        if (.not. is_root) then
            ! Only increment item_idx for files (for selection tracking)
            if (node%is_file) then
                item_idx = item_idx + 1
                is_selected = (item_idx == state%selected_index)
            else
                is_selected = .false.
            end if

            if (current_row <= end_row) then
                ! Build line with tree structure
                if (is_last) then
                    line = trim(prefix) // BRANCH_LAST // ' ' // trim(node%name)
                else
                    line = trim(prefix) // BRANCH_MID // ' ' // trim(node%name)
                end if

                ! Add status indicators for files only
                if (node%is_file) then
                    if (node%is_staged) then
                        line = trim(line) // ' ' // ESC // '[32m↑' // ESC // '[0m'  ! Green up arrow
                    end if
                    if (node%is_unstaged) then
                        line = trim(line) // ' ' // ESC // '[31m✗' // ESC // '[0m'  ! Red X
                    end if
                    if (node%is_untracked) then
                        line = trim(line) // ' ' // ESC // '[90m✗' // ESC // '[0m'  ! Gray X
                    end if
                end if

                ! Render with selection highlight for files only
                call terminal_move_cursor(current_row, start_col)
                if (is_selected) then
                    ! Selection highlight (reverse video)
                    call terminal_write(ESC // '[7m' // trim(line) // ESC // '[0m')
                else
                    call terminal_write(trim(line))
                end if

                current_row = current_row + 1
            end if
        end if

        ! Render children
        i = 0
        child => node%first_child
        do while (associated(child) .and. current_row <= end_row)
            i = i + 1

            if (is_root) then
                new_prefix = ''
            else
                if (is_last) then
                    new_prefix = trim(prefix) // '    '
                else
                    new_prefix = trim(prefix) // VERTICAL // '   '
                end if
            end if

            call render_tree_node(child, new_prefix, i == n_children, .false., &
                                state, item_idx, current_row, end_row, start_col, width)
            child => child%next_sibling
        end do
    end subroutine render_tree_node

end module file_tree_renderer_module
