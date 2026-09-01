module file_tree_renderer_module
    use iso_fortran_env, only: int32
    use file_tree_module
    use terminal_io_module
    use clickable_region_module, only: region_add, REGION_TREE_ROW
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_ACCENT, THEME_DIRECTORY, THEME_GIT_ADDED, &
        THEME_GIT_MODIFIED, THEME_HINT, THEME_MUTED, THEME_PANEL, &
        THEME_PANEL_HEADER, THEME_PANEL_SELECTION, THEME_WARNING, &
        theme_glyph, theme_paint, theme_reset, theme_sgr
    implicit none
    private

    public :: render_file_tree

    ! Columns of indent per level of nesting.
    character(len=*), parameter :: NEST_INDENT = '   '

contains

    subroutine render_file_tree(state, start_row, end_row, start_col, width, hints_expanded, git_prefix_active)
        type(tree_state_t), intent(in) :: state
        integer, intent(in) :: start_row, end_row, start_col, width
        logical, intent(in) :: hints_expanded
        logical, intent(in), optional :: git_prefix_active
        logical :: git_mode
        integer :: current_row, item_idx, row
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
            call terminal_write(theme_sgr(THEME_PANEL) // padding // theme_reset())
        end do

        current_row = start_row

        ! Display repo:branch info at top if available
        if (len_trim(state%repo_name) > 0 .and. len_trim(state%branch_name) > 0) then
            call terminal_move_cursor(current_row, start_col)
            block
                character(len=256) :: repo_str, branch_str
                integer :: vis_len, max_branch

                repo_str = trim(state%repo_name)
                branch_str = trim(state%branch_name)
                ! visible length: repo + ':' + branch
                vis_len = len_trim(repo_str) + 1 + &
                          len_trim(branch_str)

                if (vis_len > width) then
                    ! Truncate branch to fit
                    max_branch = width - len_trim(repo_str) - 4
                    if (max_branch > 0) then
                        branch_str = branch_str(1:max_branch) &
                                     // '...'
                    else
                        ! Even repo is too long — truncate it
                        repo_str = repo_str(1:max(1,width-4)) &
                                   // '...'
                        branch_str = ''
                    end if
                end if

                call terminal_write(theme_sgr(THEME_PANEL_HEADER) // ' ' // &
                    trim(repo_str) // theme_sgr(THEME_MUTED))
                if (len_trim(branch_str) > 0) call terminal_write(' : ' // &
                    theme_paint(THEME_GIT_MODIFIED, trim(branch_str)))
                call terminal_write(theme_reset())
            end block
            current_row = current_row + 2  ! Skip a line
        end if

        ! Display root
        if (current_row <= end_row) then
            call terminal_move_cursor(current_row, start_col)
            call terminal_write(theme_paint(THEME_ACCENT, ' WORKSPACE'))
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
                call terminal_write(theme_paint(THEME_HINT, 'j/k:nav  o:open'))

                call terminal_move_cursor(end_row - 4, start_col)
                call terminal_write(theme_paint(THEME_HINT, '→:in  ←:up  .:hide'))

                call terminal_move_cursor(end_row - 3, start_col)
                call terminal_write(theme_paint(THEME_HINT, 'space:toggle'))

                if (git_mode) then
                    call terminal_move_cursor(end_row - 2, start_col)
                    call terminal_write(theme_paint(THEME_WARNING, 'a:stage  u:unstage'))

                    call terminal_move_cursor(end_row - 1, start_col)
                    call terminal_write(theme_paint(THEME_WARNING, 'd:diff  m:commit'))
                else
                    call terminal_move_cursor(end_row - 2, start_col)
                    call terminal_write(theme_paint(THEME_HINT, 'alt-v:vs  alt-s:hs'))

                    call terminal_move_cursor(end_row - 1, start_col)
                    call terminal_write(theme_paint(THEME_HINT, 'ctrl-g:git  type:search'))
                end if

                call terminal_move_cursor(end_row, start_col)
                call terminal_write(theme_paint(THEME_HINT, 'ctrl-/:less  esc:close'))
            end if
        else
            ! Minimal legend (two rows)
            if (end_row >= start_row + 2) then
                call terminal_move_cursor(end_row - 1, start_col)
                call terminal_write(theme_paint(THEME_HINT, '.:hide  ctrl-/:hints'))

                call terminal_move_cursor(end_row, start_col)
                call terminal_write(theme_paint(THEME_HINT, 'esc/F3:close'))
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

        character(len=:), allocatable :: line, new_prefix, shown
        character(len=:), allocatable :: base_line
        integer :: used, role
        type(tree_node_t), pointer :: child
        logical :: is_selected, is_last_child

        ! Don't print root node
        if (.not. is_root) then
            ! Skip hidden entries when hide_dotfiles is enabled
            if (state%hide_dotfiles .and. (node%is_dotfile .or. node%is_gitignored) &
                .and. .not. node%force_visible) then
                return
            end if

            ! Increment item_idx for both files and directories (all selectable items)
            item_idx = item_idx + 1
            is_selected = (item_idx == state%selected_index)

            ! Only render if within viewport and current_row fits
            if (item_idx >= state%viewport_offset .and. current_row <= end_row) then
                ! Build line with simple +/- indicators and indentation
                if (.not. node%is_file) then
                    ! Directory - add expand/collapse indicator and / suffix.
                    ! scan_pending dirs (lazy, never scanned) get a toggle
                    ! too; only scanned-and-empty dirs render as leaves.
                    if (node%scan_pending .or. associated(node%first_child)) then
                        if (node%expanded) then
                            base_line = prefix // theme_glyph('directory_open') // ' ' // trim(node%name) // '/'
                        else
                            base_line = prefix // theme_glyph('directory_closed') // ' ' // trim(node%name) // '/'
                        end if
                    else
                        base_line = prefix // '  ' // trim(node%name) // '/'
                    end if
                else
                    base_line = prefix // theme_glyph('file') // ' ' // trim(node%name)
                end if

                ! Truncate with ellipsis if line overflows tree pane width
                line = base_line

                ! Add status indicators for files only (after truncation
                ! so indicators stay visible at the end)
                if (node%is_file) then
                    if (node%is_staged) then
                        line = line // ' +'
                    end if
                    if (node%is_unstaged) then
                        line = line // ' ~'
                    end if
                    if (node%is_untracked) then
                        line = line // ' ?'
                    end if
                    if (node%has_incoming) then
                        line = line // ' ↓'
                    end if
                end if

                ! Render with selection highlight
                call terminal_move_cursor(current_row, start_col)
                if (is_selected) then
                    call clip_to_cells(line, width, shown, used)
                    call terminal_write(theme_sgr(THEME_PANEL_SELECTION) // shown // &
                        repeat(' ', max(0, width - used)) // theme_reset())
                else
                    ! Greyed when git would ignore it, or when a directory has
                    ! nothing in it but hidden things.
                    !
                    ! is_gitignored is checked FIRST because it is known the
                    ! moment the node is created. all_children_hidden is only
                    ! computed when a directory is scanned, so on its own it
                    ! meant an ignored directory looked ordinary until it had
                    ! been expanded once -- the grey arrived as a reward for
                    ! opening the thing the grey was supposed to warn you about.
                    if (node%is_gitignored .or. &
                        (.not. node%is_file .and. node%all_children_hidden)) then
                        role = THEME_MUTED
                    else if (.not. node%is_file) then
                        role = THEME_DIRECTORY
                    else if (node%is_staged) then
                        role = THEME_GIT_ADDED
                    else if (node%is_unstaged) then
                        role = THEME_GIT_MODIFIED
                    else if (node%is_untracked) then
                        role = THEME_WARNING
                    else
                        role = THEME_PANEL
                    end if
                    call clip_to_cells(line, width, shown, used)
                    call terminal_write(theme_paint(role, shown))
                end if

                ! Claim the row for this item. item_idx is the same index
                ! selected_index and selectable_files use, so a click resolves
                ! to a tree entry directly rather than by counting rows again.
                call region_add(REGION_TREE_ROW, current_row, current_row, &
                                start_col, start_col + width - 1, item_idx)

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
                ! Depth has to be readable at a glance, and one space was not
                ! enough to tell a child from its parent, let alone two levels
                ! apart -- the tree read as a flat list with ragged names.
                new_prefix = prefix // NEST_INDENT
            end if

            call render_tree_node(child, new_prefix, .false., &
                                state, item_idx, current_row, end_row, start_col, width)
            child => child%next_sibling
            end do
        end if
    end subroutine render_tree_node

end module file_tree_renderer_module
