! Regression tests for build_tree's O(N) prefix-stack construction and the
! O(C log C) sibling merge sort. Verifies structural correctness that the
! performance rewrite must preserve: shared directory prefixes collapse to a
! single node (no duplicates), and every directory's children are ordered
! directories-first then alphabetical.
program test_tree_build
    use file_tree_module
    implicit none

    integer :: failures
    failures = 0

    call test_shared_prefixes()
    call test_prefix_collision_ordering()
    call test_dir_before_file()
    call test_deep_and_flat()

    if (failures == 0) then
        print *, 'test_tree_build: ALL PASSED'
    else
        print *, 'test_tree_build: FAILURES =', failures
        stop 1
    end if

contains

    recursive subroutine walk(node, n_files, n_dirs, ok)
        type(tree_node_t), pointer, intent(in) :: node
        integer, intent(inout) :: n_files, n_dirs
        logical, intent(inout) :: ok
        type(tree_node_t), pointer :: c, prev

        if (.not. associated(node)) return

        if (node%is_file) then
            n_files = n_files + 1
        else if (trim(node%name) /= '.') then
            n_dirs = n_dirs + 1
        end if

        prev => null()
        c => node%first_child
        do while (associated(c))
            if (associated(prev)) then
                if (prev%is_file .and. .not. c%is_file) then
                    print *, '  ORDER FAIL: file before dir under ', trim(node%name)
                    ok = .false.
                end if
                if (prev%is_file .eqv. c%is_file) then
                    if (trim(prev%name) == trim(c%name)) then
                        print *, '  DUPLICATE sibling ', trim(c%name), &
                            ' under ', trim(node%name)
                        ok = .false.
                    else if (prev%name > c%name) then
                        print *, '  ALPHA FAIL ', trim(prev%name), ' > ', &
                            trim(c%name)
                        ok = .false.
                    end if
                end if
            end if
            prev => c
            c => c%next_sibling
        end do

        c => node%first_child
        do while (associated(c))
            call walk(c, n_files, n_dirs, ok)
            c => c%next_sibling
        end do
    end subroutine walk

    subroutine expect(label, cond)
        character(len=*), intent(in) :: label
        logical, intent(in) :: cond
        if (cond) then
            print *, '  ok  ', label
        else
            print *, '  FAIL', label
            failures = failures + 1
        end if
    end subroutine expect

    ! Consecutive sorted paths sharing a/b must reuse the same a and b nodes.
    subroutine test_shared_prefixes()
        type(file_entry_t), allocatable :: files(:)
        type(tree_node_t), pointer :: root
        integer :: nf, nd
        logical :: ok
        allocate(files(4))
        files(1)%path = 'a/b/c.txt'
        files(2)%path = 'a/b/d.txt'
        files(3)%path = 'a/e/f.txt'
        files(4)%path = 'g/h.txt'
        call build_tree(files, 4, root)
        nf = 0; nd = 0; ok = .true.
        call walk(root, nf, nd, ok)
        ! dirs: a, a/b, a/e, g => 4 (no dups)
        call expect('shared_prefixes files=4', nf == 4)
        call expect('shared_prefixes dirs=4 (deduped)', nd == 4)
        call expect('shared_prefixes ordered', ok)
        deallocate(files)
    end subroutine test_shared_prefixes

    ! 'sub-a' vs 'sub' collide on prefix ('-' < '/'): merge sort must still
    ! order the directory names correctly regardless of input first-appearance.
    subroutine test_prefix_collision_ordering()
        type(file_entry_t), allocatable :: files(:)
        type(tree_node_t), pointer :: root, c
        integer :: nf, nd
        logical :: ok
        character(len=64) :: order
        allocate(files(2))
        files(1)%path = 'sub-a/x.txt'
        files(2)%path = 'sub/y.txt'
        call build_tree(files, 2, root)
        nf = 0; nd = 0; ok = .true.
        call walk(root, nf, nd, ok)
        call expect('collision ordered/dedup', ok)
        ! top-level order should be alphabetical: sub, sub-a
        order = ''
        c => root%first_child
        do while (associated(c))
            order = trim(order) // trim(c%name) // ','
            c => c%next_sibling
        end do
        call expect('collision top order = sub,sub-a,', &
            trim(order) == 'sub,sub-a,')
        deallocate(files)
    end subroutine test_prefix_collision_ordering

    ! A file sibling of directories must sort AFTER the directories.
    subroutine test_dir_before_file()
        type(file_entry_t), allocatable :: files(:)
        type(tree_node_t), pointer :: root, c
        logical :: seen_file
        allocate(files(3))
        files(1)%path = 'a/readme.md'
        files(2)%path = 'a/src/x.txt'
        files(3)%path = 'a/zzz/y.txt'
        call build_tree(files, 3, root)
        ! children of 'a' should be: src(dir), zzz(dir), readme.md(file)
        seen_file = .false.
        c => root%first_child      ! 'a'
        c => c%first_child         ! first child of a
        block
            logical :: order_ok
            order_ok = .true.
            do while (associated(c))
                if (c%is_file) then
                    seen_file = .true.
                else if (seen_file) then
                    order_ok = .false.   ! dir appeared after a file
                end if
                c => c%next_sibling
            end do
            call expect('dir_before_file ordering', order_ok)
        end block
        deallocate(files)
    end subroutine test_dir_before_file

    ! Mix of a very wide flat directory and a deep chain; verify counts.
    subroutine test_deep_and_flat()
        type(file_entry_t), allocatable :: files(:)
        type(tree_node_t), pointer :: root
        integer :: nf, nd, i
        logical :: ok
        character(len=16) :: num
        allocate(files(1000))
        do i = 1, 999
            write(num, '(I4.4)') i
            files(i)%path = 'flat/f' // trim(num) // '.txt'
        end do
        files(1000)%path = 'a/b/c/d/e/deep.txt'
        ! must be sorted; 'a/...' < 'flat/...' already, flat entries sorted by num
        call build_tree(files, 1000, root)
        nf = 0; nd = 0; ok = .true.
        call walk(root, nf, nd, ok)
        call expect('deep_and_flat files=1000', nf == 1000)
        ! dirs: flat + a,b,c,d,e => 6
        call expect('deep_and_flat dirs=6', nd == 6)
        call expect('deep_and_flat ordered/dedup', ok)
        deallocate(files)
    end subroutine test_deep_and_flat

end program test_tree_build
