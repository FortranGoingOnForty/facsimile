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

        ! DEBUG: Visual marker at column 1
        call terminal_move_cursor(1, 1)
        call terminal_write('[COL1]')

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

        character(len=:), allocatable :: line, new_prefix, branch
        type(tree_node_t), pointer :: child
        logical :: is_selected, is_last_child
        integer :: prefix_len, i

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
                    branch = BRANCH_LAST
                else
                    branch = BRANCH_MID
                end if

                ! DEBUG: Check branch assignment for root's children
                if (len_trim(prefix) == 0 .and. trim(node%name) == 'src') then
                    open(98, file='/tmp/fac_branch_debug.txt', status='replace')
                    write(98, '(A,L,A,I0)') 'is_last=', is_last, ' len(branch)=', len(branch)
                    write(98, '(A,Z2.2,Z2.2,Z2.2)') 'branch bytes: ', &
                        ichar(branch(1:1)), ichar(branch(2:2)), ichar(branch(3:3))
                    write(98, '(A)') 'BRANCH_MID='//BRANCH_MID
                    write(98, '(A)') 'BRANCH_LAST='//BRANCH_LAST
                    close(98)
                end if

                ! Construct the line - prefix can be empty string, that's fine
                line = prefix // branch // ' ' // trim(node%name)

                ! DEBUG: Check what's actually in line for src
                if (trim(node%name) == 'src') then
                    open(96, file='/tmp/fac_line_debug.txt', status='replace')
                    write(96, '(A,I0)') 'len(line)=', len(line)
                    write(96, '(A,I0)') 'len(prefix)=', len(prefix)
                    write(96, '(A,I0)') 'len(branch)=', len(branch)
                    write(96, '(A)') 'line bytes (first 20):'
                    write(96, '(20(Z2.2,1X))') (ichar(line(i:i)), i=1,min(20,len(line)))
                    write(96, '(A)') 'Full line: >'//line//'<'
                    close(96)
                end if

                ! Add status indicators for files only
                if (node%is_file) then
                    if (node%is_staged) then
                        line = line // ' ' // ESC // '[32m↑' // ESC // '[0m'  ! Green up arrow
                    end if
                    if (node%is_unstaged) then
                        line = line // ' ' // ESC // '[31m✗' // ESC // '[0m'  ! Red X
                    end if
                    if (node%is_untracked) then
                        line = line // ' ' // ESC // '[90m✗' // ESC // '[0m'  ! Gray X
                    end if
                end if

                ! Render with selection highlight for files only
                call terminal_move_cursor(current_row, start_col)
                if (is_selected) then
                    ! Selection highlight (reverse video)
                    call terminal_write(ESC // '[7m' // line // ESC // '[0m')
                else
                    call terminal_write(line)
                end if

                current_row = current_row + 1
            end if
        end if

        ! Render children
        child => node%first_child
        do while (associated(child) .and. current_row <= end_row)
            ! Determine if this is the last sibling
            is_last_child = .not. associated(child%next_sibling)

            ! DEBUG: Write sibling info for workspace children
            if (trim(node%name) == 'workspace') then
                open(99, file='/tmp/fac_render_debug.txt', position='append')
                write(99, '(A,A,A,L)') 'Child: ', trim(child%name), ' has_next_sib=', associated(child%next_sibling)
                close(99)
            end if


            ! Build prefix for child based on current node's position
            if (is_root) then
                ! Root's children start with no prefix
                new_prefix = ''
            else
                ! Non-root children inherit prefix and add continuation
                ! IMPORTANT: Don't trim prefix! It contains accumulated indentation
                if (is_last) then
                    ! Current node is last, so children get spaces (no vertical line continues)
                    new_prefix = prefix // '    '
                else
                    ! Current node is not last, so vertical line continues for children
                    new_prefix = prefix // VERTICAL // '   '
                end if
            end if

            ! DEBUG: Log prefix building for src
            if (trim(node%name) == 'src' .or. trim(child%name) == 'workspace') then
                open(97, file='/tmp/fac_prefix_debug.txt', position='append')
                write(97, '(A,A,A,L,A,A,A,A)') &
                    'Node: ', trim(node%name), ' is_root=', is_root, ' is_last=', merge('T', 'F', is_last), &
                    ' Child: ', trim(child%name)
                write(97, '(A,I0,A)') 'new_prefix len=', len(new_prefix), ' content=>'//new_prefix//'<'
                if (.not. is_root .and. .not. is_last) then
                    write(97, '(A)') 'Should have added VERTICAL: '//VERTICAL
                end if
                close(97)
            end if

            call render_tree_node(child, new_prefix, is_last_child, .false., &
                                state, item_idx, current_row, end_row, start_col, width)
            child => child%next_sibling
        end do
    end subroutine render_tree_node

end module file_tree_renderer_module
