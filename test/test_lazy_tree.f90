! Regression tests for lazy fuss-tree expansion (non-git workspaces).
! Verifies: root-only initial scan, scan-on-first-expand with caching,
! renderer/selectable-list walk consistency, dotfile toggling over lazy
! nodes, empty dirs, nesting beyond the old maxdepth-4 ceiling, and the
! dir_scan_module primitive itself.
program test_lazy_tree
    use file_tree_module
    use dir_scan_module
    use platform_module, only: get_temp_dir
    implicit none

    character(len=:), allocatable :: fixture
    integer :: failures
    failures = 0

    fixture = trim(get_temp_dir()) // '/fac_lazy_tree_fixture'
    call build_fixture(fixture)

    call test_dir_scan_primitive()
    call test_lazy_init()
    call test_scan_on_expand()
    call test_dotfile_toggle()
    call test_empty_dir()
    call test_deep_nesting()

    call execute_command_line('rm -rf "' // fixture // '"')

    if (failures == 0) then
        print *, 'test_lazy_tree: ALL PASSED'
    else
        print *, 'test_lazy_tree: FAILURES =', failures
        stop 1
    end if

contains

    subroutine build_fixture(root)
        character(len=*), intent(in) :: root
        ! Outside the repo so `git rev-parse` fails -> lazy path activates
        call execute_command_line('rm -rf "' // root // '"')
        call execute_command_line('mkdir -p "' // root // '/sub" "' // &
            root // '/d1/d2/d3/d4/d5" "' // root // '/.dotdir" "' // &
            root // '/emptydir"')
        call execute_command_line('touch "' // root // '/sub/a.txt" "' // &
            root // '/d1/d2/d3/d4/d5/deep.txt" "' // &
            root // '/.dotdir/x.txt" "' // root // '/.dotfile"')
    end subroutine build_fixture

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

    ! Replicates the renderer's visit rule (file_tree_renderer_module:
    ! render_tree_node): skip hidden when hide_dotfiles, count every other
    ! node, recurse only into expanded dirs (root always). Must equal
    ! n_selectable or renderer and navigation desync.
    recursive subroutine renderer_walk(node, hide_dotfiles, count, is_root)
        type(tree_node_t), pointer, intent(in) :: node
        logical, intent(in) :: hide_dotfiles, is_root
        integer, intent(inout) :: count
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return
        if (.not. is_root) then
            if (hide_dotfiles .and. (node%is_dotfile .or. node%is_gitignored)) return
            count = count + 1
        end if
        if (node%expanded .or. is_root) then
            child => node%first_child
            do while (associated(child))
                call renderer_walk(child, hide_dotfiles, count, .false.)
                child => child%next_sibling
            end do
        end if
    end subroutine renderer_walk

    subroutine check_consistency(state, label)
        type(tree_state_t), intent(in) :: state
        character(len=*), intent(in) :: label
        integer :: count
        count = 0
        call renderer_walk(state%root, state%hide_dotfiles, count, .true.)
        call expect(label // ' renderer/selectable agree', count == state%n_selectable)
    end subroutine check_consistency

    ! Select the selectable entry whose node name matches, return index (0 if absent)
    function find_selectable(state, name) result(idx)
        type(tree_state_t), intent(in) :: state
        character(len=*), intent(in) :: name
        integer :: idx, i
        idx = 0
        do i = 1, state%n_selectable
            if (associated(state%selectable_files(i)%node)) then
                if (trim(state%selectable_files(i)%node%name) == trim(name)) then
                    idx = i
                    return
                end if
            end if
        end do
    end function find_selectable

    subroutine test_dir_scan_primitive()
        type(dir_entry_t), allocatable :: entries(:)
        integer :: n, i
        logical :: ok, saw_sub, saw_dot, saw_self
        call list_directory(fixture, entries, n, ok)
        saw_sub = .false.; saw_dot = .false.; saw_self = .false.
        do i = 1, n
            if (trim(entries(i)%name) == 'sub') saw_sub = entries(i)%is_dir
            if (trim(entries(i)%name) == '.dotfile') saw_dot = .not. entries(i)%is_dir
            if (trim(entries(i)%name) == '.' .or. trim(entries(i)%name) == '..') saw_self = .true.
        end do
        call expect('primitive: ok and 5 entries', ok .and. n == 5)
        call expect('primitive: sub is dir, .dotfile is file', saw_sub .and. saw_dot)
        call expect('primitive: no . or ..', .not. saw_self)
        ! failure path
        call list_directory(fixture // '/no_such_dir', entries, n, ok)
        call expect('primitive: missing dir -> ok=false, n=0', (.not. ok) .and. n == 0)
    end subroutine test_dir_scan_primitive

    subroutine test_lazy_init()
        type(tree_state_t) :: st
        type(tree_node_t), pointer :: c
        logical :: dirs_ok
        call init_tree_state(st, fixture)
        call expect('init: non-git detected', .not. st%is_git_repo)
        call expect('init: dotfiles hidden by default', st%hide_dotfiles)
        ! every dir child: collapsed, scan_pending, and NO grandchildren
        dirs_ok = .true.
        c => st%root%first_child
        do while (associated(c))
            if (.not. c%is_file) then
                if (c%expanded .or. .not. c%scan_pending) dirs_ok = .false.
                if (associated(c%first_child)) dirs_ok = .false.
            end if
            c => c%next_sibling
        end do
        call expect('init: dirs collapsed+pending, no grandchildren', dirs_ok)
        ! visible: sub, d1, emptydir (dirs) — .dotdir/.dotfile hidden
        call expect('init: n_selectable=3', st%n_selectable == 3)
        call check_consistency(st, 'init:')
        call cleanup_tree_state(st)
    end subroutine test_lazy_init

    subroutine test_scan_on_expand()
        type(tree_state_t) :: st
        type(tree_node_t), pointer :: sub, c
        integer :: idx, n_children
        call init_tree_state(st, fixture)
        idx = find_selectable(st, 'sub')
        call expect('expand: sub in selectable list', idx > 0)
        if (idx == 0) then
            call cleanup_tree_state(st); return
        end if
        sub => st%selectable_files(idx)%node
        st%selected_index = idx
        call tree_toggle_expand(st)
        call expect('expand: scan_pending cleared', .not. sub%scan_pending)
        call expect('expand: a.txt appears', find_selectable(st, 'a.txt') > 0)
        call expect('expand: selection stays on sub', &
            associated(st%selectable_files(st%selected_index)%node, sub))
        call check_consistency(st, 'expand:')
        ! collapse then re-expand: children cached, not duplicated
        call tree_toggle_expand(st)
        call expect('collapse: a.txt hidden again', find_selectable(st, 'a.txt') == 0)
        call tree_toggle_expand(st)
        n_children = 0
        c => sub%first_child
        do while (associated(c))
            n_children = n_children + 1
            c => c%next_sibling
        end do
        call expect('re-expand: exactly 1 child (no dup)', n_children == 1)
        call check_consistency(st, 're-expand:')
        call cleanup_tree_state(st)
    end subroutine test_scan_on_expand

    subroutine test_dotfile_toggle()
        type(tree_state_t) :: st
        integer :: idx
        call init_tree_state(st, fixture)
        call expect('dots: hidden by default', find_selectable(st, '.dotfile') == 0)
        st%hide_dotfiles = .false.
        if (allocated(st%selectable_files)) deallocate(st%selectable_files)
        call build_selectable_list(st%root, st%selectable_files, st%n_selectable, st%hide_dotfiles)
        idx = find_selectable(st, '.dotfile')
        call expect('dots: toggle reveals .dotfile', idx > 0)
        if (idx > 0) then
            call expect('dots: is_dotfile set', st%selectable_files(idx)%node%is_dotfile)
        end if
        call expect('dots: .dotdir revealed too', find_selectable(st, '.dotdir') > 0)
        call check_consistency(st, 'dots:')
        call cleanup_tree_state(st)
    end subroutine test_dotfile_toggle

    subroutine test_empty_dir()
        type(tree_state_t) :: st
        type(tree_node_t), pointer :: ed
        integer :: idx
        call init_tree_state(st, fixture)
        idx = find_selectable(st, 'emptydir')
        call expect('empty: emptydir selectable', idx > 0)
        if (idx == 0) then
            call cleanup_tree_state(st); return
        end if
        ed => st%selectable_files(idx)%node
        st%selected_index = idx
        call tree_toggle_expand(st)
        call expect('empty: scan_pending cleared', .not. ed%scan_pending)
        call expect('empty: no children', .not. associated(ed%first_child))
        ! further toggles are no-ops (guard: no scan_pending, no children)
        call tree_toggle_expand(st)
        call check_consistency(st, 'empty:')
        call cleanup_tree_state(st)
    end subroutine test_empty_dir

    subroutine test_deep_nesting()
        type(tree_state_t) :: st
        integer :: idx, level
        character(len=8) :: dname
        call init_tree_state(st, fixture)
        do level = 1, 5
            write(dname, '(A,I1)') 'd', level
            idx = find_selectable(st, trim(dname))
            if (idx == 0) exit
            st%selected_index = idx
            call tree_toggle_expand(st)
        end do
        idx = find_selectable(st, 'deep.txt')
        call expect('deep: deep.txt reachable (beyond old maxdepth 4)', idx > 0)
        if (idx > 0) then
            call expect('deep: full_path correct', &
                trim(st%selectable_files(idx)%node%full_path) == 'd1/d2/d3/d4/d5/deep.txt')
        end if
        call check_consistency(st, 'deep:')
        call cleanup_tree_state(st)
    end subroutine test_deep_nesting

end program test_lazy_tree
