module file_tree_module
    use iso_fortran_env, only: int32, error_unit
    implicit none
    private

    public :: tree_node_t, file_entry_t, tree_state_t
    public :: init_tree_state, cleanup_tree_state, refresh_tree_state
    public :: tree_move_up, tree_move_down, get_selected_item_path
    public :: tree_stage_file, tree_unstage_file, tree_toggle_expand
    public :: build_selectable_list
    public :: update_tree_viewport
    public :: build_tree

    ! Tree node using linked list structure (first-child, next-sibling)
    type :: tree_node_t
        character(len=256) :: name = ''
        character(len=512) :: full_path = ''  ! Full path for files (for staging)
        logical :: is_file = .false.
        logical :: is_staged = .false.
        logical :: is_unstaged = .false.
        logical :: is_untracked = .false.
        logical :: has_incoming = .false.
        logical :: expanded = .true.  ! For directories: true=expanded, false=collapsed
        logical :: is_dotfile = .false.  ! Is this a dotfile (starts with .)
        logical :: is_gitignored = .false.  ! Is this file gitignored
        logical :: all_children_hidden = .false.  ! For directories: all children are hidden
        type(tree_node_t), pointer :: parent => null()  ! Parent node for sibling navigation
        type(tree_node_t), pointer :: first_child => null()
        type(tree_node_t), pointer :: next_sibling => null()
    end type tree_node_t

    type :: file_entry_t
        character(len=512) :: path = ''
        character(len=2) :: status = '  '
        logical :: is_staged = .false.
        logical :: is_unstaged = .false.
        logical :: is_untracked = .false.
        logical :: has_incoming = .false.
    end type file_entry_t

    ! Selectable item (files and directories, in tree traversal order)
    type :: selectable_file_t
        character(len=512) :: path = ''
        logical :: is_directory = .false.
        logical :: is_staged = .false.
        logical :: is_unstaged = .false.
        logical :: is_untracked = .false.
        type(tree_node_t), pointer :: node => null()  ! Pointer to actual tree node
    end type selectable_file_t

    ! Tree state for navigation
    type :: tree_state_t
        type(file_entry_t), allocatable :: files(:)
        type(selectable_file_t), allocatable :: selectable_files(:)
        integer :: n_files = 0
        integer :: n_selectable = 0
        integer :: selected_index = 1
        integer :: viewport_offset = 1
        type(tree_node_t), pointer :: root => null()
        character(len=256) :: repo_name = ''
        character(len=256) :: branch_name = ''
        logical :: hide_dotfiles = .false.
    end type tree_state_t

contains

    subroutine init_tree_state(state, workspace_path)
        type(tree_state_t), intent(out) :: state
        character(len=*), intent(in) :: workspace_path

        state%n_files = 0
        state%selected_index = 1
        state%viewport_offset = 1
        state%root => null()

        ! Get repository info
        call get_repo_info(workspace_path, state%repo_name, state%branch_name)

        ! Load files
        call refresh_tree_state(state, workspace_path)
    end subroutine init_tree_state

    subroutine cleanup_tree_state(state)
        type(tree_state_t), intent(inout) :: state

        if (allocated(state%files)) deallocate(state%files)
        if (allocated(state%selectable_files)) deallocate(state%selectable_files)
        if (associated(state%root)) call free_tree(state%root)
        state%n_files = 0
        state%n_selectable = 0
        state%selected_index = 1
    end subroutine cleanup_tree_state

    subroutine refresh_tree_state(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path
        type(file_entry_t), allocatable :: all_files(:), dirty_files(:)
        integer :: n_all_files, n_dirty_files

        ! Free existing tree if present
        if (associated(state%root)) then
            call free_tree(state%root)
            state%root => null()
        end if
        if (allocated(state%selectable_files)) deallocate(state%selectable_files)

        ! First: Get ALL files from filesystem
        call get_all_files(workspace_path, all_files, n_all_files)

        ! Build tree from ALL files (not just dirty ones)
        if (n_all_files > 0) then
            call build_tree(all_files, n_all_files, state%root)

            ! Second: Get dirty files from git status and overlay markers
            call get_dirty_files(workspace_path, dirty_files, n_dirty_files)
            if (n_dirty_files > 0) then
                call overlay_git_status(state%root, dirty_files, n_dirty_files)
                state%files = dirty_files
                state%n_files = n_dirty_files
            else
                allocate(state%files(0))
                state%n_files = 0
            end if

            ! Mark gitignored files (batch mode - single git command)
            call mark_gitignored_files(state%root, workspace_path)

            ! Collapse tree smartly - only expand dirs with dirty files
            call collapse_tree_smart(state%root)

            ! Build selectable files list in tree traversal order
            call build_selectable_list(state%root, state%selectable_files, state%n_selectable)
        else
            state%n_selectable = 0
        end if

        if (allocated(all_files)) deallocate(all_files)

        ! Clamp selected index
        if (state%selected_index > state%n_selectable .and. state%n_selectable > 0) then
            state%selected_index = state%n_selectable
        else if (state%n_selectable == 0) then
            state%selected_index = 1
        end if
    end subroutine refresh_tree_state

    subroutine get_dirty_files(workspace_path, files, n_files)
        character(len=*), intent(in) :: workspace_path
        type(file_entry_t), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code
        character(len=1024) :: line, cmd
        character(len=512) :: file_path
        character(len=2) :: git_status
        integer :: max_files
        type(file_entry_t), allocatable :: temp_files(:)

        max_files = 1000
        allocate(temp_files(max_files))
        n_files = 0

        ! Build command to execute git status in workspace directory
        write(cmd, '(A,A,A)') 'cd "', trim(workspace_path), '" && git status --porcelain > /tmp/fac_git_status.txt 2>&1'
        call execute_command_line(trim(cmd), exitstat=status_code)

        if (status_code /= 0) then
            allocate(files(0))
            return
        end if

        ! Read git status output
        open(newunit=unit_num, file='/tmp/fac_git_status.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            allocate(files(0))
            return
        end if

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            if (len_trim(line) > 3) then
                ! Parse git status line (format: "XY filename")
                git_status = line(1:2)
                file_path = adjustl(line(4:))

                ! Skip if path is empty
                if (len_trim(file_path) == 0) cycle

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_file_array(temp_files, max_files)
                end if

                temp_files(n_files)%status = git_status
                temp_files(n_files)%path = trim(file_path)
                ! Column 1 = staged status, Column 2 = unstaged status
                temp_files(n_files)%is_untracked = (git_status == '??')
                temp_files(n_files)%is_staged = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                temp_files(n_files)%is_unstaged = (git_status(2:2) /= ' ' .and. .not. temp_files(n_files)%is_untracked)
                temp_files(n_files)%has_incoming = .false.
            end if
        end do

        close(unit_num, status='delete')

        ! Copy to output array
        allocate(files(n_files))
        if (n_files > 0) files(1:n_files) = temp_files(1:n_files)
        deallocate(temp_files)
    end subroutine get_dirty_files

    subroutine get_all_files(workspace_path, files, n_files)
        character(len=*), intent(in) :: workspace_path
        type(file_entry_t), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code
        character(len=1024) :: line, cmd
        character(len=1024) :: file_path
        integer :: max_files
        type(file_entry_t), allocatable :: temp_files(:)

        max_files = 5000
        allocate(temp_files(max_files))
        n_files = 0

        ! Use git ls-files for tracked files, then add ALL untracked files (except .git)
        ! This is fast because it uses git's index for most files
        ! We don't use --exclude-standard so gitignored files are included (can be hidden with ".")
        write(cmd, '(A,A,A)') 'cd "', trim(workspace_path), &
            '" && { git ls-files; git ls-files --others --exclude .git; } | sort -u > /tmp/fac_all_files.txt 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status_code)

        if (status_code /= 0) then
            allocate(files(0))
            return
        end if

        ! Read file list
        open(newunit=unit_num, file='/tmp/fac_all_files.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            allocate(files(0))
            return
        end if

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            file_path = adjustl(line)

            ! Skip if path is empty
            if (len_trim(file_path) == 0) cycle

            n_files = n_files + 1
            if (n_files > max_files) then
                max_files = max_files * 2
                call resize_file_array(temp_files, max_files)
            end if

            temp_files(n_files)%path = trim(file_path)
            temp_files(n_files)%status = '  '  ! No git status yet
            temp_files(n_files)%is_staged = .false.
            temp_files(n_files)%is_unstaged = .false.
            temp_files(n_files)%is_untracked = .false.
            temp_files(n_files)%has_incoming = .false.
        end do

        close(unit_num, status='delete')

        ! Copy to output array
        allocate(files(n_files))
        if (n_files > 0) files(1:n_files) = temp_files(1:n_files)
        deallocate(temp_files)
    end subroutine get_all_files

    subroutine overlay_git_status(root, dirty_files, n_dirty_files)
        type(tree_node_t), pointer, intent(inout) :: root
        type(file_entry_t), intent(in) :: dirty_files(:)
        integer, intent(in) :: n_dirty_files
        integer :: i

        ! For each dirty file, find it in the tree and update its status
        do i = 1, n_dirty_files
            call update_node_status(root, dirty_files(i)%path, &
                                  dirty_files(i)%is_staged, &
                                  dirty_files(i)%is_unstaged, &
                                  dirty_files(i)%is_untracked, &
                                  dirty_files(i)%has_incoming)
        end do
    end subroutine overlay_git_status

    recursive subroutine update_node_status(node, path, is_staged, is_unstaged, is_untracked, has_incoming)
        type(tree_node_t), pointer, intent(inout) :: node
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_staged, is_unstaged, is_untracked, has_incoming
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        ! Check if this node matches the path
        if (node%is_file .and. trim(node%full_path) == trim(path)) then
            node%is_staged = is_staged
            node%is_unstaged = is_unstaged
            node%is_untracked = is_untracked
            node%has_incoming = has_incoming
            return
        end if

        ! Recurse to children
        child => node%first_child
        do while (associated(child))
            call update_node_status(child, path, is_staged, is_unstaged, is_untracked, has_incoming)
            child => child%next_sibling
        end do
    end subroutine update_node_status

    subroutine resize_file_array(arr, new_size)
        type(file_entry_t), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: new_size
        type(file_entry_t), allocatable :: temp(:)
        integer :: old_size

        old_size = size(arr)
        allocate(temp(new_size))
        temp(1:old_size) = arr(1:old_size)
        deallocate(arr)
        call move_alloc(temp, arr)
    end subroutine resize_file_array

    subroutine build_tree(files, n_files, root)
        type(file_entry_t), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(tree_node_t), pointer, intent(out) :: root
        integer :: i
        integer :: debug_unit

        ! Create root
        allocate(root)
        root%name = '.'
        root%is_file = .false.
        root%first_child => null()
        root%next_sibling => null()

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, &
                           files(i)%is_unstaged, files(i)%is_untracked, &
                           files(i)%has_incoming)
        end do

        ! Sort tree
        call sort_tree(root)

        ! Mark directories that only contain hidden files
        i = 0  ! Dummy variable
        if (mark_empty_directories(root)) then
            i = 1  ! Dummy assignment to use function result
        end if

        ! DEBUG: Write tree structure to file (unconditional)
        open(newunit=debug_unit, file='/tmp/fac_tree_debug.txt', status='replace', action='write')
        write(debug_unit, '(A)') '=== FINAL TREE STRUCTURE ==='
        call debug_print_tree(root, '', debug_unit)
        close(debug_unit)

        ! Also write to a simpler path
        open(10, file='fac_debug.txt', status='replace', action='write')
        write(10, '(A)') '=== FINAL TREE STRUCTURE ==='
        call debug_print_tree(root, '', 10)
        close(10)
    end subroutine build_tree

    ! Recursively mark directories that only contain hidden files
    recursive function mark_empty_directories(node) result(all_hidden)
        type(tree_node_t), pointer, intent(inout) :: node
        logical :: all_hidden
        type(tree_node_t), pointer :: child
        logical :: child_hidden
        integer :: visible_count

        if (.not. associated(node)) then
            all_hidden = .true.
            return
        end if

        ! Files are hidden if they're dotfiles or gitignored
        if (node%is_file) then
            all_hidden = node%is_dotfile .or. node%is_gitignored
            return
        end if

        ! For directories, check if all children are hidden
        visible_count = 0
        child => node%first_child
        do while (associated(child))
            child_hidden = mark_empty_directories(child)
            if (.not. child_hidden) then
                visible_count = visible_count + 1
            end if
            child => child%next_sibling
        end do

        ! Directory is "all hidden" if it has no visible children
        all_hidden = (visible_count == 0 .and. associated(node%first_child))
        node%all_children_hidden = all_hidden

    end function mark_empty_directories

    ! Mark files that are gitignored
    subroutine mark_gitignored_files(root, workspace_path)
        type(tree_node_t), pointer, intent(inout) :: root
        character(len=*), intent(in) :: workspace_path
        character(len=1024) :: cmd
        integer :: status, iostat, unit_num
        character(len=512) :: line

        ! Run git check-ignore ONCE with all files (MUCH faster than per-file)
        ! Create temp file with all file paths, then batch check
        write(cmd, '(A,A,A)') 'cd "', trim(workspace_path), &
            '" && git ls-files --others --exclude .git | git check-ignore --stdin > /tmp/fac_ignored_files.txt 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)

        if (status /= 0) then
            ! No ignored files or command failed - nothing to mark
            return
        end if

        ! Read the list of ignored files
        open(newunit=unit_num, file='/tmp/fac_ignored_files.txt', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return

        ! Mark each ignored file in the tree
        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) > 0) then
                call mark_file_as_ignored(root, trim(line))
            end if
        end do

        close(unit_num, status='delete')
    end subroutine mark_gitignored_files

    ! Collapse tree intelligently - only expand directories with dirty files
    subroutine collapse_tree_smart(root)
        type(tree_node_t), pointer, intent(inout) :: root
        logical :: dummy

        if (.not. associated(root)) return

        ! Recursively determine which directories should be expanded
        dummy = has_dirty_files(root)
    end subroutine collapse_tree_smart

    ! Recursive function: returns true if node or descendants have dirty files
    ! Side effect: sets node%expanded based on whether it should be shown expanded
    recursive function has_dirty_files(node) result(has_dirty)
        type(tree_node_t), pointer, intent(inout) :: node
        logical :: has_dirty
        type(tree_node_t), pointer :: child
        logical :: child_has_dirty

        if (.not. associated(node)) then
            has_dirty = .false.
            return
        end if

        ! Files are dirty if they have any git status
        if (node%is_file) then
            has_dirty = node%is_staged .or. node%is_unstaged .or. node%is_untracked
            return
        end if

        ! For directories, check all children
        has_dirty = .false.
        child => node%first_child
        do while (associated(child))
            child_has_dirty = has_dirty_files(child)
            if (child_has_dirty) has_dirty = .true.
            child => child%next_sibling
        end do

        ! Collapse this directory if it has no dirty descendants
        ! Keep root always expanded
        if (trim(node%name) == '.') then
            node%expanded = .true.  ! Root always expanded
        else
            node%expanded = has_dirty  ! Only expand if has dirty files
        end if
    end function has_dirty_files

    recursive subroutine mark_file_as_ignored(node, path)
        type(tree_node_t), pointer, intent(inout) :: node
        character(len=*), intent(in) :: path
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        ! Check if this node matches the path
        if (node%is_file .and. trim(node%full_path) == trim(path)) then
            node%is_gitignored = .true.
            return
        end if

        ! Recurse to children
        child => node%first_child
        do while (associated(child))
            call mark_file_as_ignored(child, path)
            child => child%next_sibling
        end do
    end subroutine mark_file_as_ignored

    recursive subroutine debug_print_tree(node, prefix, unit)
        type(tree_node_t), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        integer, intent(in) :: unit
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        write(unit, '(A,A,A,L1,A,L1)') trim(prefix), trim(node%name), &
            ' is_file=', node%is_file, ' has_next_sib=', associated(node%next_sibling)

        child => node%first_child
        do while (associated(child))
            call debug_print_tree(child, prefix // '  ', unit)
            child => child%next_sibling
        end do
    end subroutine debug_print_tree

    subroutine add_to_tree(root, path, is_staged, is_unstaged, is_untracked, has_incoming)
        type(tree_node_t), pointer, intent(inout) :: root
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_staged, is_unstaged, is_untracked, has_incoming
        character(len=512) :: remaining_path, component
        integer :: slash_pos
        type(tree_node_t), pointer :: current, child, new_node

        current => root
        remaining_path = trim(path)

        do while (len_trim(remaining_path) > 0)
            slash_pos = index(remaining_path, '/')

            if (slash_pos > 0) then
                component = remaining_path(1:slash_pos-1)
                remaining_path = remaining_path(slash_pos+1:)
            else
                component = remaining_path
                remaining_path = ''
            end if

            ! Find or create child with this name
            child => current%first_child
            do while (associated(child))
                if (trim(child%name) == trim(component)) exit
                child => child%next_sibling
            end do

            if (.not. associated(child)) then
                ! Create new node
                allocate(new_node)
                new_node%name = trim(component)
                new_node%is_file = (len_trim(remaining_path) == 0)
                new_node%expanded = .true.  ! Explicitly set expanded for directories
                new_node%parent => current  ! Set parent pointer
                new_node%first_child => null()
                new_node%next_sibling => current%first_child
                ! Mark as dotfile if name starts with '.'
                new_node%is_dotfile = (len_trim(component) > 0 .and. component(1:1) == '.')
                current%first_child => new_node
                child => new_node
            end if

            ! If this is the final component, set status and full path
            if (len_trim(remaining_path) == 0) then
                child%is_staged = is_staged
                child%is_unstaged = is_unstaged
                child%is_untracked = is_untracked
                child%has_incoming = has_incoming
                child%full_path = trim(path)
            end if

            current => child
        end do
    end subroutine add_to_tree

    recursive subroutine sort_tree(node)
        type(tree_node_t), pointer, intent(inout) :: node
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        ! Sort children
        call sort_children(node)

        ! Recursively sort descendants
        child => node%first_child
        do while (associated(child))
            call sort_tree(child)
            child => child%next_sibling
        end do
    end subroutine sort_tree

    subroutine sort_children(parent)
        type(tree_node_t), pointer, intent(inout) :: parent
        type(tree_node_t), pointer :: sorted, current, next_node, insert_pos
        logical :: inserted

        if (.not. associated(parent%first_child)) return

        sorted => null()

        current => parent%first_child
        do while (associated(current))
            next_node => current%next_sibling

            ! Insert current into sorted list
            if (.not. associated(sorted)) then
                sorted => current
                current%next_sibling => null()
            else if (compare_nodes(current, sorted) < 0) then
                current%next_sibling => sorted
                sorted => current
            else
                insert_pos => sorted
                inserted = .false.
                do while (associated(insert_pos%next_sibling))
                    if (compare_nodes(current, insert_pos%next_sibling) < 0) then
                        current%next_sibling => insert_pos%next_sibling
                        insert_pos%next_sibling => current
                        inserted = .true.
                        exit
                    end if
                    insert_pos => insert_pos%next_sibling
                end do
                if (.not. inserted) then
                    insert_pos%next_sibling => current
                    current%next_sibling => null()
                end if
            end if

            current => next_node
        end do

        parent%first_child => sorted
    end subroutine sort_children

    function compare_nodes(a, b) result(cmp)
        type(tree_node_t), pointer, intent(in) :: a, b
        integer :: cmp

        ! Directories come before files
        if (.not. a%is_file .and. b%is_file) then
            cmp = -1
        else if (a%is_file .and. .not. b%is_file) then
            cmp = 1
        else
            ! Alphabetical comparison
            if (a%name < b%name) then
                cmp = -1
            else if (a%name > b%name) then
                cmp = 1
            else
                cmp = 0
            end if
        end if
    end function compare_nodes

    ! Build list of selectable files in tree traversal order
    subroutine build_selectable_list(root, selectable, n_selectable)
        type(tree_node_t), pointer, intent(in) :: root
        type(selectable_file_t), allocatable, intent(out) :: selectable(:)
        integer, intent(out) :: n_selectable
        type(selectable_file_t), allocatable :: temp(:)
        integer :: max_size, count

        max_size = 1000
        allocate(temp(max_size))
        count = 0

        ! Traverse tree and collect files
        call collect_files_recursive(root, temp, count, max_size)

        n_selectable = count
        allocate(selectable(n_selectable))
        if (n_selectable > 0) selectable(1:n_selectable) = temp(1:n_selectable)
        deallocate(temp)
    end subroutine build_selectable_list

    recursive subroutine collect_files_recursive(node, list, count, max_size)
        type(tree_node_t), pointer, intent(in) :: node
        type(selectable_file_t), intent(inout) :: list(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_size
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        ! Add both files and directories to selectable list
        ! Skip root node (name = '.')
        if (trim(node%name) /= '.') then
            count = count + 1
            if (count <= max_size) then
                if (node%is_file) then
                    list(count)%path = node%full_path
                    list(count)%is_directory = .false.
                else
                    list(count)%path = node%name  ! For directories, use name
                    list(count)%is_directory = .true.
                end if
                list(count)%is_staged = node%is_staged
                list(count)%is_unstaged = node%is_unstaged
                list(count)%is_untracked = node%is_untracked
                list(count)%node => node
            end if
        end if

        ! Recursively process children if this node is expanded (always recurse for root)
        if (node%expanded .or. trim(node%name) == '.') then
            child => node%first_child
            do while (associated(child))
                call collect_files_recursive(child, list, count, max_size)
                child => child%next_sibling
            end do
        end if
    end subroutine collect_files_recursive

    recursive subroutine free_tree(node)
        type(tree_node_t), pointer, intent(inout) :: node
        type(tree_node_t), pointer :: child, next_child

        if (.not. associated(node)) return

        ! Free children first
        child => node%first_child
        do while (associated(child))
            next_child => child%next_sibling
            call free_tree(child)
            child => next_child
        end do

        ! Free this node
        deallocate(node)
        node => null()
    end subroutine free_tree

    subroutine get_repo_info(workspace_path, repo_name, branch_name)
        character(len=*), intent(in) :: workspace_path
        character(len=*), intent(out) :: repo_name, branch_name
        integer :: status, unit_num
        character(len=1024) :: cmd, buffer

        repo_name = ''
        branch_name = ''

        ! Get branch name
        write(cmd, '(A,A,A)') 'cd "', trim(workspace_path), '" && git branch --show-current > /tmp/fac_branch.txt 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)
        if (status == 0) then
            open(newunit=unit_num, file='/tmp/fac_branch.txt', status='old', action='read', iostat=status)
            if (status == 0) then
                read(unit_num, '(A)', iostat=status) buffer
                close(unit_num, status='delete')
                if (status == 0) branch_name = trim(buffer)
            end if
        end if

        ! Get repo name from workspace path
        ! Extract last component of path as repo name
        call extract_repo_name(workspace_path, repo_name)
    end subroutine get_repo_info

    subroutine extract_repo_name(path, repo_name)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: repo_name
        integer :: last_slash

        last_slash = index(path, '/', back=.true.)
        if (last_slash > 0) then
            repo_name = path(last_slash+1:)
        else
            repo_name = path
        end if
    end subroutine extract_repo_name

    ! Navigation functions (sibling-only)
    subroutine tree_move_up(state)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer :: current_parent, candidate_parent
        integer :: i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return
        if (.not. associated(state%selectable_files(state%selected_index)%node)) return

        ! Get current item's parent
        current_parent => state%selectable_files(state%selected_index)%node%parent

        ! Search backwards for item with same parent
        do i = state%selected_index - 1, 1, -1
            if (associated(state%selectable_files(i)%node)) then
                candidate_parent => state%selectable_files(i)%node%parent
                ! Check if parents match (same address or both null)
                if (associated(current_parent) .and. associated(candidate_parent)) then
                    if (associated(current_parent, candidate_parent)) then
                        state%selected_index = i
                        return
                    end if
                else if (.not. associated(current_parent) .and. .not. associated(candidate_parent)) then
                    state%selected_index = i
                    return
                end if
            end if
        end do
    end subroutine tree_move_up

    subroutine tree_move_down(state)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer :: current_parent, candidate_parent
        integer :: i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return
        if (.not. associated(state%selectable_files(state%selected_index)%node)) return

        ! Get current item's parent
        current_parent => state%selectable_files(state%selected_index)%node%parent

        ! Search forwards for item with same parent
        do i = state%selected_index + 1, state%n_selectable
            if (associated(state%selectable_files(i)%node)) then
                candidate_parent => state%selectable_files(i)%node%parent
                ! Check if parents match (same address or both null)
                if (associated(current_parent) .and. associated(candidate_parent)) then
                    if (associated(current_parent, candidate_parent)) then
                        state%selected_index = i
                        return
                    end if
                else if (.not. associated(current_parent) .and. .not. associated(candidate_parent)) then
                    state%selected_index = i
                    return
                end if
            end if
        end do
    end subroutine tree_move_down

    function get_selected_item_path(state) result(path)
        type(tree_state_t), intent(in) :: state
        character(len=:), allocatable :: path

        if (state%selected_index >= 1 .and. state%selected_index <= state%n_selectable) then
            path = trim(state%selectable_files(state%selected_index)%path)
        else
            path = ''
        end if
    end function get_selected_item_path

    ! Git operations
    subroutine tree_stage_file(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path
        character(len=1024) :: cmd
        character(len=:), allocatable :: selected_path
        integer :: status, i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return

        ! Save the path of the currently selected file
        selected_path = trim(state%selectable_files(state%selected_index)%path)

        ! Stage the file
        write(cmd, '(A,A,A,A,A)') 'cd "', trim(workspace_path), '" && git add "', &
                                  trim(selected_path), '" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)

        ! Refresh tree
        call refresh_tree_state(state, workspace_path)

        ! Restore selection to the same file
        do i = 1, state%n_selectable
            if (trim(state%selectable_files(i)%path) == selected_path) then
                state%selected_index = i
                exit
            end if
        end do
    end subroutine tree_stage_file

    subroutine tree_unstage_file(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path
        character(len=1024) :: cmd
        character(len=:), allocatable :: selected_path
        integer :: status, i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return

        ! Save the path of the currently selected file
        selected_path = trim(state%selectable_files(state%selected_index)%path)

        ! Unstage the file
        write(cmd, '(A,A,A,A,A)') 'cd "', trim(workspace_path), '" && git restore --staged "', &
                                  trim(selected_path), '" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)

        ! Refresh tree
        call refresh_tree_state(state, workspace_path)

        ! Restore selection to the same file
        do i = 1, state%n_selectable
            if (trim(state%selectable_files(i)%path) == selected_path) then
                state%selected_index = i
                exit
            end if
        end do
    end subroutine tree_unstage_file

    subroutine tree_toggle_expand(state)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer :: selected_node
        integer :: i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return

        ! Get the selected node via the pointer
        selected_node => state%selectable_files(state%selected_index)%node

        if (.not. associated(selected_node)) return

        ! Only toggle directories (not files)
        if (.not. selected_node%is_file .and. associated(selected_node%first_child)) then
            selected_node%expanded = .not. selected_node%expanded

            ! Rebuild selectable list to reflect new visibility
            if (allocated(state%selectable_files)) deallocate(state%selectable_files)
            call build_selectable_list(state%root, state%selectable_files, state%n_selectable)

            ! Find the toggled node in the new list to maintain selection
            do i = 1, state%n_selectable
                if (associated(state%selectable_files(i)%node, selected_node)) then
                    state%selected_index = i
                    return
                end if
            end do

            ! If node not found (shouldn't happen), clamp selected index
            if (state%selected_index > state%n_selectable .and. state%n_selectable > 0) then
                state%selected_index = state%n_selectable
            end if
        end if
    end subroutine tree_toggle_expand

    ! Update viewport to keep selected item visible
    subroutine update_tree_viewport(state, visible_height)
        type(tree_state_t), intent(inout) :: state
        integer, intent(in) :: visible_height

        ! Ensure selected index is valid
        if (state%selected_index < 1) state%selected_index = 1
        if (state%selected_index > state%n_selectable .and. state%n_selectable > 0) then
            state%selected_index = state%n_selectable
        end if

        ! Scroll up if selected item is above viewport
        if (state%selected_index < state%viewport_offset) then
            state%viewport_offset = state%selected_index
        end if

        ! Scroll down if selected item is below viewport
        if (state%selected_index >= state%viewport_offset + visible_height) then
            state%viewport_offset = state%selected_index - visible_height + 1
        end if

        ! Clamp viewport_offset to valid range
        if (state%viewport_offset < 1) state%viewport_offset = 1
        if (state%n_selectable > 0 .and. state%viewport_offset > state%n_selectable) then
            state%viewport_offset = max(1, state%n_selectable - visible_height + 1)
        end if
    end subroutine update_tree_viewport

end module file_tree_module
