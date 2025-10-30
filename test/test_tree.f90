program test_tree
    use file_tree_module
    implicit none

    type(file_entry_t), allocatable :: files(:)
    type(tree_node_t), pointer :: root, child
    integer :: n_files

    ! Create test data matching current git status
    n_files = 3
    allocate(files(n_files))

    files(1)%path = 'README.md'
    files(1)%is_staged = .false.
    files(1)%is_unstaged = .true.
    files(1)%is_untracked = .false.
    files(1)%has_incoming = .false.

    files(2)%path = 'src/workspace/file_tree_module.f90'
    files(2)%is_staged = .false.
    files(2)%is_unstaged = .true.
    files(2)%is_untracked = .false.
    files(2)%has_incoming = .false.

    files(3)%path = 'src/workspace/file_tree_renderer_module.f90'
    files(3)%is_staged = .false.
    files(3)%is_unstaged = .true.
    files(3)%is_untracked = .false.
    files(3)%has_incoming = .false.

    ! Build tree
    call build_tree(files, n_files, root)

    print *, 'Tree structure:'
    call print_tree(root, '')

    ! Check workspace children specifically
    print *, ''
    print *, 'Checking src/workspace children:'
    call check_workspace_children(root)

contains

    recursive subroutine print_tree(node, prefix)
        type(tree_node_t), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        print '(A,A,A,L,A,L)', trim(prefix), trim(node%name), &
            ' is_file=', node%is_file, ' has_next_sib=', associated(node%next_sibling)

        child => node%first_child
        do while (associated(child))
            call print_tree(child, prefix // '  ')
            child => child%next_sibling
        end do
    end subroutine print_tree

    recursive subroutine check_workspace_children(node)
        type(tree_node_t), pointer, intent(in) :: node
        type(tree_node_t), pointer :: child
        integer :: count

        if (.not. associated(node)) return

        ! Found workspace directory
        if (trim(node%name) == 'workspace') then
            print *, 'Found workspace directory!'
            child => node%first_child
            count = 0
            do while (associated(child))
                count = count + 1
                print '(A,I0,A,A,A,L)', '  Child ', count, ': ', trim(child%name), &
                    ' has_next_sib=', associated(child%next_sibling)
                child => child%next_sibling
            end do
            return
        end if

        ! Recurse to children
        child => node%first_child
        do while (associated(child))
            call check_workspace_children(child)
            child => child%next_sibling
        end do
    end subroutine check_workspace_children

end program test_tree
