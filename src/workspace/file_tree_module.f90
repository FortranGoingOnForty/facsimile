module file_tree_module
    use iso_fortran_env, only: int32, error_unit
    implicit none
    private

    public :: tree_node_t, file_entry_t, tree_state_t
    public :: init_tree_state, cleanup_tree_state, refresh_tree_state
    public :: tree_move_up, tree_move_down, get_selected_item_path
    public :: tree_stage_file, tree_unstage_file

    ! Tree node using linked list structure (first-child, next-sibling)
    type :: tree_node_t
        character(len=256) :: name = ''
        logical :: is_file = .false.
        logical :: is_staged = .false.
        logical :: is_unstaged = .false.
        logical :: is_untracked = .false.
        logical :: has_incoming = .false.
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

    ! Tree state for navigation
    type :: tree_state_t
        type(file_entry_t), allocatable :: files(:)
        integer :: n_files = 0
        integer :: selected_index = 1
        integer :: viewport_offset = 1
        type(tree_node_t), pointer :: root => null()
        character(len=256) :: repo_name = ''
        character(len=256) :: branch_name = ''
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
        if (associated(state%root)) call free_tree(state%root)
        state%n_files = 0
        state%selected_index = 1
    end subroutine cleanup_tree_state

    subroutine refresh_tree_state(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path

        ! Free existing tree if present
        if (associated(state%root)) then
            call free_tree(state%root)
            state%root => null()
        end if

        ! Get dirty files from git
        call get_dirty_files(workspace_path, state%files, state%n_files)

        ! Build tree from files
        if (state%n_files > 0) then
            call build_tree(state%files, state%n_files, state%root)
        end if

        ! Clamp selected index
        if (state%selected_index > state%n_files .and. state%n_files > 0) then
            state%selected_index = state%n_files
        else if (state%n_files == 0) then
            state%selected_index = 1
        end if
    end subroutine refresh_tree_state

    subroutine get_dirty_files(workspace_path, files, n_files)
        character(len=*), intent(in) :: workspace_path
        type(file_entry_t), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code, i
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
    end subroutine build_tree

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
                new_node%first_child => null()
                new_node%next_sibling => current%first_child
                current%first_child => new_node
                child => new_node
            end if

            ! If this is the final component, set status
            if (len_trim(remaining_path) == 0) then
                child%is_staged = is_staged
                child%is_unstaged = is_unstaged
                child%is_untracked = is_untracked
                child%has_incoming = has_incoming
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
        type(tree_node_t), pointer :: sorted, current, next_node, insert_pos, prev
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

    ! Navigation functions
    subroutine tree_move_up(state)
        type(tree_state_t), intent(inout) :: state
        if (state%selected_index > 1) then
            state%selected_index = state%selected_index - 1
        end if
    end subroutine tree_move_up

    subroutine tree_move_down(state)
        type(tree_state_t), intent(inout) :: state
        if (state%selected_index < state%n_files) then
            state%selected_index = state%selected_index + 1
        end if
    end subroutine tree_move_down

    function get_selected_item_path(state) result(path)
        type(tree_state_t), intent(in) :: state
        character(len=:), allocatable :: path

        if (state%selected_index >= 1 .and. state%selected_index <= state%n_files) then
            path = trim(state%files(state%selected_index)%path)
        else
            path = ''
        end if
    end function get_selected_item_path

    ! Git operations
    subroutine tree_stage_file(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path
        character(len=1024) :: cmd
        integer :: status

        if (state%selected_index < 1 .or. state%selected_index > state%n_files) return

        ! Stage the file
        write(cmd, '(A,A,A,A,A)') 'cd "', trim(workspace_path), '" && git add "', &
                                  trim(state%files(state%selected_index)%path), '" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)

        ! Refresh tree
        call refresh_tree_state(state, workspace_path)
    end subroutine tree_stage_file

    subroutine tree_unstage_file(state, workspace_path)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: workspace_path
        character(len=1024) :: cmd
        integer :: status

        if (state%selected_index < 1 .or. state%selected_index > state%n_files) return

        ! Unstage the file
        write(cmd, '(A,A,A,A,A)') 'cd "', trim(workspace_path), '" && git restore --staged "', &
                                  trim(state%files(state%selected_index)%path), '" 2>/dev/null'
        call execute_command_line(trim(cmd), exitstat=status)

        ! Refresh tree
        call refresh_tree_state(state, workspace_path)
    end subroutine tree_unstage_file

end module file_tree_module
