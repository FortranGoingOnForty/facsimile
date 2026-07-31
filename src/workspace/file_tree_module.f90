module file_tree_module
    use iso_fortran_env, only: int32, error_unit
    use dir_scan_module, only: dir_entry_t, list_directory
    implicit none
    private

    public :: tree_node_t, file_entry_t, tree_state_t
    public :: init_tree_state, cleanup_tree_state, refresh_tree_state
    public :: tree_move_up, tree_move_down, get_selected_item_path
    public :: tree_stage_file, tree_unstage_file, tree_toggle_expand
    public :: tree_expand_node, tree_reveal_path
    public :: build_selectable_list
    public :: update_tree_viewport

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
        logical :: scan_pending = .false.  ! Dir not yet scanned (lazy mode); eager paths never set it
        ! Shown even while hidden entries are being hidden.
        !
        ! Set on the directories leading to a file the user has open. Having a
        ! file open in a directory is a better answer to "should this be shown"
        ! than the name it happens to start with -- and it is narrow: the
        ! folders on that path are revealed, every other dotfile stays hidden.
        logical :: force_visible = .false.
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
        character(len=1024) :: workspace_path = ''  ! Stored so lazy expand can scan
        logical :: is_git_repo = .false.
        logical :: first_refresh = .true.  ! Guards one-time defaults (hide_dotfiles)
        ! Paths git would ignore, collected once per refresh.
        !
        ! The tree used to be built from `git ls-files --exclude-standard`, so
        ! ignored files were filtered by never being listed -- is_gitignored
        ! was declared and read but never once assigned. Reading directories
        ! instead means they ARE listed, so the ignoring has to be done here
        ! rather than fall out of the listing. Directory entries keep their
        ! trailing '/', which is what makes a prefix test enough to cover
        ! everything beneath them.
        character(len=512), allocatable :: ignored(:)
        integer :: n_ignored = 0
    end type tree_state_t

    ! The other temp files in this module use fixed names, which two fac
    ! instances refreshing at once will fight over. Not repeating that for a
    ! list whose job is to decide what NOT to show.
    interface
        function c_getpid() bind(c, name="getpid")
            use iso_c_binding, only: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface

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
        ! Bounded: past this many open directories the tree is not something
        ! anyone is reading anyway, and the rest reopen on demand.
        integer, parameter :: MAX_REMEMBERED = 256
        ! Allocatable rather than automatic: 256 x 512 bytes is past the point
        ! where gfortran quietly moves an array to static storage, which would
        ! make this procedure unsafe to reenter.
        character(len=512), allocatable :: was_open(:)
        integer :: n_was_open, k

        ! Free existing tree if present
        ! Where the user had got to, noted before the tree is thrown away.
        allocate(was_open(MAX_REMEMBERED))
        n_was_open = 0
        if (associated(state%root)) then
            call collect_expanded(state%root, was_open, n_was_open)
            call free_tree(state%root)
            state%root => null()
        end if
        if (allocated(state%selectable_files)) deallocate(state%selectable_files)

        ! Check if this is a git repository
        block
            logical :: is_git_repo
            integer :: git_check
            character(len=1024) :: git_cmd
            write(git_cmd, '(A,A,A)') 'cd "', &
                trim(workspace_path), &
                '" && git rev-parse --git-dir > /dev/null 2>&1'
            call execute_command_line(trim(git_cmd), &
                exitstat=git_check)
            is_git_repo = (git_check == 0)

        ! Remember workspace + repo kind so lazy expansion can scan later
        state%workspace_path = trim(workspace_path)
        state%is_git_repo = is_git_repo

        ! ONE way to discover what is in the tree, git repo or not: read the
        ! directories. The git path used to derive the tree from
        ! `git ls-files`, which meant a directory existed only if git named a
        ! file inside it -- so a gitignored or empty directory had no node at
        ! all, and the '.' toggle could not reveal what was never built. Git is
        ! now asked only about STATUS and about what to ignore, which are the
        ! two things git actually knows better than the filesystem.
        !
        ! Hidden by default in both modes. This used to be set only on the
        ! non-git path, so the same '.' key revealed dotfiles outside a repo
        ! and hid them inside one.
        if (state%first_refresh) then
            state%hide_dotfiles = .true.
            state%first_refresh = .false.
        end if

        n_all_files = 0
        ! Freed before every rebuild. This is a REFRESH, which can now run more
        ! than once against the same state -- it used to be reached only
        ! through init_tree_state, whose intent(out) silently cleared these
        ! first, so allocating them here was safe by accident.
        if (allocated(state%files)) deallocate(state%files)
        if (is_git_repo) then
            call get_ignored_paths(workspace_path, state)
            call get_dirty_files(workspace_path, dirty_files, n_dirty_files)
            if (n_dirty_files > 0) then
                state%files = dirty_files
                state%n_files = n_dirty_files
            else
                allocate(state%files(0))
                state%n_files = 0
            end if
        else
            if (allocated(state%ignored)) deallocate(state%ignored)
            allocate(state%ignored(0))
            state%n_ignored = 0
            allocate(state%files(0))
            state%n_files = 0
        end if

        allocate(state%root)
        state%root%name = '.'
        state%root%is_file = .false.
        state%root%expanded = .true.
        state%root%full_path = ''
        state%root%scan_pending = .true.
        call scan_directory_children(state, state%root)

        ! Put back what was open before, then open the folders holding
        ! changes. Order matters only in that both are cheap and idempotent.
        do k = 1, n_was_open
            call tree_reveal_path(state, trim(was_open(k)))
        end do
        deallocate(was_open)
        if (is_git_repo) call expand_to_dirty_files(state)

        call build_selectable_list(state%root, state%selectable_files, &
                                   state%n_selectable, state%hide_dotfiles)

        end block  ! is_git_repo block

        if (allocated(all_files)) deallocate(all_files)

        ! Clamp selected index
        if (state%selected_index > state%n_selectable .and. state%n_selectable > 0) then
            state%selected_index = state%n_selectable
        else if (state%n_selectable == 0) then
            state%selected_index = 1
        end if
    end subroutine refresh_tree_state

    !> Open the folders your changes are in.
    !>
    !> collapse_tree_smart did this by walking the whole tree asking each node
    !> whether anything beneath it was dirty. A lazy tree has no "beneath it"
    !> until something is scanned, so that question cannot be asked -- but it
    !> does not need to be. Git has already named the dirty paths, so walking
    !> those is the same answer without reading a single directory that has
    !> nothing in it worth showing.
    subroutine expand_to_dirty_files(state)
        type(tree_state_t), intent(inout) :: state
        integer :: i

        if (state%n_files == 0) return
        if (.not. allocated(state%files)) return

        do i = 1, state%n_files
            call tree_reveal_path(state, trim(state%files(i)%path))
        end do
    end subroutine expand_to_dirty_files

    !> Every directory currently open, as workspace-relative paths.
    !>
    !> A refresh throws the tree away and reads the directories again, which is
    !> how it notices files that appeared or vanished. Collecting the open
    !> directories first and reopening them afterwards is what keeps that from
    !> also throwing away where the user had got to.
    recursive subroutine collect_expanded(node, paths, n)
        type(tree_node_t), pointer, intent(in) :: node
        character(len=512), intent(inout) :: paths(:)
        integer, intent(inout) :: n
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return
        child => node%first_child
        do while (associated(child))
            if (.not. child%is_file .and. child%expanded) then
                if (n < size(paths)) then
                    n = n + 1
                    ! Stored with a trailing '/' so tree_reveal_path treats the
                    ! directory itself as something to open rather than as a
                    ! leaf to stop before.
                    paths(n) = trim(child%full_path) // '/'
                end if
                call collect_expanded(child, paths, n)
            end if
            child => child%next_sibling
        end do
    end subroutine collect_expanded

    !> Open every directory along `path`, so whatever sits at the end of it can
    !> be seen.
    !>
    !> Three callers want exactly this and none of them care why: the folders
    !> holding git's changes, the folders holding open files, and the folders
    !> that were open before a refresh. `path` is relative to the workspace
    !> root; a trailing filename is ignored, since only the directories on the
    !> way to it need opening.
    !>
    !> Hidden and ignored directories are opened like any other. That is the
    !> point -- being asked for something inside one is a better answer to
    !> "should this be shown" than the name it happens to start with.
    subroutine tree_reveal_path(state, path, force)
        type(tree_state_t), intent(inout) :: state
        character(len=*), intent(in) :: path
        logical, intent(in), optional :: force
        type(tree_node_t), pointer :: parent, child
        character(len=512) :: rest, component
        integer :: slash
        logical :: do_force

        do_force = .false.
        if (present(force)) do_force = force

        if (.not. associated(state%root)) return
        if (len_trim(path) == 0) return

        rest = trim(path)
        parent => state%root
        do
            slash = index(trim(rest), '/')
            if (slash <= 0) exit          ! the leaf itself; nothing to open
            component = rest(1:slash-1)
            rest = rest(slash+1:)

            if (parent%scan_pending) call scan_directory_children(state, parent)
            child => parent%first_child
            parent => null()
            do while (associated(child))
                if (trim(child%name) == trim(component) .and. &
                    .not. child%is_file) then
                    ! Scanned as it is opened, not on the next turn of the loop:
                    ! the LAST directory in a path never gets another turn, so
                    ! it would be drawn open and empty with the very thing it
                    ! was opened for missing from it.
                    child%expanded = .true.
                    if (do_force) child%force_visible = .true.
                    if (child%scan_pending) &
                        call scan_directory_children(state, child)
                    parent => child
                    exit
                end if
                child => child%next_sibling
            end do
            ! Named something no longer on disk -- a git listing or a stored
            ! path can be a moment stale. Give up rather than guess.
            if (.not. associated(parent)) exit
        end do

        ! The leaf. Without this the folders open but the file that caused all
        ! of it stays hidden inside them.
        if (do_force .and. associated(parent) .and. len_trim(rest) > 0) then
            child => parent%first_child
            do while (associated(child))
                if (trim(child%name) == trim(rest)) then
                    child%force_visible = .true.
                    exit
                end if
                child => child%next_sibling
            end do
        end if
    end subroutine tree_reveal_path

    !> Copy git's view of a file onto the node standing for it.
    subroutine apply_git_status(state, node)
        type(tree_state_t), intent(in) :: state
        type(tree_node_t), pointer, intent(inout) :: node
        integer :: i

        if (state%n_files == 0) return
        if (.not. allocated(state%files)) return

        do i = 1, state%n_files
            if (trim(state%files(i)%path) == trim(node%full_path)) then
                node%is_staged = state%files(i)%is_staged
                node%is_unstaged = state%files(i)%is_unstaged
                node%is_untracked = state%files(i)%is_untracked
                node%has_incoming = state%files(i)%has_incoming
                return
            end if
        end do
    end subroutine apply_git_status

    !> What git would ignore, asked once per refresh.
    !>
    !> `--directory` collapses an ignored directory to a single entry with a
    !> trailing slash instead of listing everything beneath it, which is the
    !> difference between one line for build/ and ten thousand.
    subroutine get_ignored_paths(workspace_path, state)
        character(len=*), intent(in) :: workspace_path
        type(tree_state_t), intent(inout) :: state
        character(len=1024) :: cmd, tmp_file
        character(len=512) :: line
        integer :: unit_num, iostat, status_code, n, pass
        character(len=32) :: pid_str

        if (allocated(state%ignored)) deallocate(state%ignored)
        state%n_ignored = 0
        n = 0

        write(pid_str, '(i0)') c_getpid()
        tmp_file = '/tmp/.fac_ignored_' // trim(pid_str)

        write(cmd, '(A)') 'cd "' // trim(workspace_path) // &
            '" && git ls-files --others --ignored --exclude-standard ' // &
            '--directory 2>/dev/null > "' // trim(tmp_file) // '"'
        call execute_command_line(trim(cmd), exitstat=status_code)
        if (status_code /= 0) then
            allocate(state%ignored(0))
            return
        end if

        ! Counted, then read: the list is unbounded and a fixed cap would
        ! silently stop ignoring things past it, which shows up as a tree full
        ! of build output rather than as an error.
        do pass = 1, 2
            open(newunit=unit_num, file=trim(tmp_file), status='old', &
                 action='read', iostat=iostat)
            if (iostat /= 0) exit
            n = 0
            do
                read(unit_num, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(line) == 0) cycle
                n = n + 1
                if (pass == 2) state%ignored(n) = trim(adjustl(line))
            end do
            close(unit_num)
            if (pass == 1) allocate(state%ignored(max(n, 0)))
        end do
        state%n_ignored = n
        if (.not. allocated(state%ignored)) allocate(state%ignored(0))

        call execute_command_line('rm -f "' // trim(tmp_file) // '"', wait=.true.)
    end subroutine get_ignored_paths

    !> Would git ignore this path?
    !>
    !> An ignored DIRECTORY is listed as 'build/', so a prefix test covers
    !> everything beneath it without git having to enumerate any of it.
    function path_is_ignored(state, path, is_dir) result(res)
        type(tree_state_t), intent(in) :: state
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_dir
        logical :: res
        character(len=:), allocatable :: p, entry
        integer :: i

        res = .false.
        if (state%n_ignored == 0) return
        if (len_trim(path) == 0) return

        p = trim(path)
        if (is_dir) p = p // '/'

        do i = 1, state%n_ignored
            entry = trim(state%ignored(i))
            if (len(entry) == 0) cycle
            if (p == entry) then
                res = .true.
                return
            end if
            ! Under an ignored directory.
            if (entry(len(entry):len(entry)) == '/' .and. len(p) > len(entry)) then
                if (p(1:len(entry)) == entry) then
                    res = .true.
                    return
                end if
            end if
        end do
    end function path_is_ignored

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
        write(cmd, '(A,A,A)') 'cd "', trim(workspace_path), '" && git status --porcelain > /tmp/fac_git_status.txt 2>/dev/null'
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

    ! List all tracked + untracked-unignored files in a git repo (eager
    ! mode). Non-git workspaces use lazy per-directory scanning instead
    ! (scan_directory_children).

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

    ! O(C log C) stable merge sort of a directory's children. The old
    ! insertion sort was O(C^2), which dominated tree build for wide dirs
    ! (e.g. a home dir with 1000+ entries under one folder).
    subroutine sort_children(parent)
        type(tree_node_t), pointer, intent(inout) :: parent

        if (.not. associated(parent%first_child)) return
        parent%first_child => merge_sort_siblings(parent%first_child)
    end subroutine sort_children

    recursive function merge_sort_siblings(head) result(sorted_head)
        type(tree_node_t), pointer, intent(in) :: head
        type(tree_node_t), pointer :: sorted_head
        type(tree_node_t), pointer :: left, right, slow, fast

        if (.not. associated(head)) then
            sorted_head => null()
            return
        end if
        if (.not. associated(head%next_sibling)) then
            sorted_head => head
            return
        end if

        ! Split the sibling list into halves via slow/fast pointers.
        slow => head
        fast => head%next_sibling
        do while (associated(fast))
            fast => fast%next_sibling
            if (associated(fast)) then
                slow => slow%next_sibling
                fast => fast%next_sibling
            end if
        end do
        left => head
        right => slow%next_sibling
        slow%next_sibling => null()

        left => merge_sort_siblings(left)
        right => merge_sort_siblings(right)
        sorted_head => merge_siblings(left, right)
    end function merge_sort_siblings

    ! Stable merge: on ties the node from `a_in` (left half) is kept first.
    function merge_siblings(a_in, b_in) result(merged)
        type(tree_node_t), pointer, intent(in) :: a_in, b_in
        type(tree_node_t), pointer :: merged, tail, a, b, nxt

        a => a_in
        b => b_in
        merged => null()
        tail => null()

        ! append_sibling nulls the node's next pointer, so capture the
        ! successor before each append to keep walking the source list.
        do while (associated(a) .and. associated(b))
            if (compare_nodes(a, b) <= 0) then
                nxt => a%next_sibling
                call append_sibling(merged, tail, a)
                a => nxt
            else
                nxt => b%next_sibling
                call append_sibling(merged, tail, b)
                b => nxt
            end if
        end do

        do while (associated(a))
            nxt => a%next_sibling
            call append_sibling(merged, tail, a)
            a => nxt
        end do
        do while (associated(b))
            nxt => b%next_sibling
            call append_sibling(merged, tail, b)
            b => nxt
        end do
    end function merge_siblings

    ! Append node to the merged list, updating head and tail.
    subroutine append_sibling(head, tail, node)
        type(tree_node_t), pointer, intent(inout) :: head, tail
        type(tree_node_t), pointer, intent(in) :: node

        if (.not. associated(head)) then
            head => node
        else
            tail%next_sibling => node
        end if
        tail => node
        node%next_sibling => null()
    end subroutine append_sibling

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
    subroutine build_selectable_list(root, selectable, n_selectable, hide_dotfiles)
        type(tree_node_t), pointer, intent(in) :: root
        type(selectable_file_t), allocatable, intent(out) :: selectable(:)
        integer, intent(out) :: n_selectable
        logical, intent(in), optional :: hide_dotfiles
        type(selectable_file_t), allocatable :: temp(:)
        integer :: max_size, count
        logical :: do_hide

        do_hide = .false.
        if (present(hide_dotfiles)) do_hide = hide_dotfiles

        max_size = 1000
        allocate(temp(max_size))
        count = 0

        ! Traverse tree and collect files (grows temp as needed)
        call collect_files_recursive(root, temp, count, &
                                     max_size, do_hide)

        n_selectable = count
        allocate(selectable(n_selectable))
        if (n_selectable > 0) &
            selectable(1:n_selectable) = temp(1:n_selectable)
        deallocate(temp)
    end subroutine build_selectable_list

    subroutine resize_selectable_array(arr, new_size)
        type(selectable_file_t), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: new_size
        type(selectable_file_t), allocatable :: tmp(:)
        integer :: old_size

        old_size = size(arr)
        allocate(tmp(new_size))
        tmp(1:old_size) = arr(1:old_size)
        deallocate(arr)
        call move_alloc(tmp, arr)
    end subroutine resize_selectable_array

    recursive subroutine collect_files_recursive(node, list, &
        count, max_size, hide_dotfiles)
        type(tree_node_t), pointer, intent(in) :: node
        type(selectable_file_t), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: count, max_size
        logical, intent(in) :: hide_dotfiles
        type(tree_node_t), pointer :: child

        if (.not. associated(node)) return

        ! Skip hidden entries when hide_dotfiles is enabled (match renderer)
        if (hide_dotfiles .and. (node%is_dotfile .or. node%is_gitignored) &
            .and. .not. node%force_visible) return

        ! Add both files and directories to selectable list
        ! Skip root node (name = '.')
        if (trim(node%name) /= '.') then
            count = count + 1
            if (count > max_size) then
                max_size = max_size * 2
                call resize_selectable_array(list, max_size)
            end if
            ! Both hold a PATH. Directories used to hold only their name,
            ! which is identical to the path at depth 1 and wrong below it --
            ! so 'ch5' worked and 'ch5/printaf' resolved to nothing. Enter on a
            ! nested directory opened no group dialog, and staging one ran
            ! `git add printaf` from the workspace root.
            list(count)%path = node%full_path
            list(count)%is_directory = .not. node%is_file
            list(count)%is_staged = node%is_staged
            list(count)%is_unstaged = node%is_unstaged
            list(count)%is_untracked = node%is_untracked
            list(count)%node => node
        end if

        ! Recursively process children if this node is expanded (always recurse for root)
        if (node%expanded .or. trim(node%name) == '.') then
            child => node%first_child
            do while (associated(child))
                call collect_files_recursive(child, list, count, max_size, hide_dotfiles)
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
    !> Up and down move to the next VISIBLE row, not the next sibling.
    !>
    !> They used to search for the next item with the same parent, and did
    !> nothing at all when there was not one. So the cursor stopped dead on the
    !> last child of any expanded directory -- inside ch4/ it reached top.c and
    !> refused to go further, and the only way on was Left back to the parent
    !> and then Down. That reads as the cursor being blocked, which is exactly
    !> how it was reported.
    !>
    !> The selectable list is already in tree traversal order and already
    !> excludes what is hidden or collapsed, so walking it by one is the whole
    !> of "the next thing you can see". Left and Right still move by structure,
    !> which is where jumping over a subtree belongs.
    subroutine tree_move_up(state)
        type(tree_state_t), intent(inout) :: state

        if (state%n_selectable <= 0) return
        if (state%selected_index > 1) then
            state%selected_index = state%selected_index - 1
        end if
    end subroutine tree_move_up

    subroutine tree_move_down(state)
        type(tree_state_t), intent(inout) :: state

        if (state%n_selectable <= 0) return
        if (state%selected_index < state%n_selectable) then
            state%selected_index = state%selected_index + 1
        end if
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

    ! Scan a directory node's immediate children from the filesystem (lazy
    ! mode). Children are cached on the node; scan_pending is cleared even on
    ! failure so an unreadable dir degrades to an empty leaf instead of
    ! retrying every expand.
    subroutine scan_directory_children(state, node)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer, intent(inout) :: node
        type(dir_entry_t), allocatable :: entries(:)
        type(tree_node_t), pointer :: new_node
        character(len=1024) :: abs_path
        integer :: n_entries, i
        logical :: ok, any_visible

        node%scan_pending = .false.

        if (len_trim(node%full_path) == 0) then
            abs_path = trim(state%workspace_path)
        else
            abs_path = trim(state%workspace_path) // '/' // trim(node%full_path)
        end if

        call list_directory(trim(abs_path), entries, n_entries, ok)
        if (.not. ok) return

        any_visible = .false.
        do i = 1, n_entries
            allocate(new_node)
            new_node%name = entries(i)%name
            new_node%is_file = .not. entries(i)%is_dir
            if (len_trim(node%full_path) == 0) then
                new_node%full_path = trim(entries(i)%name)
            else
                new_node%full_path = trim(node%full_path) // '/' // trim(entries(i)%name)
            end if
            new_node%is_dotfile = (entries(i)%name(1:1) == '.')
            ! Marked as the node is created rather than overlaid afterwards:
            ! the tree is lazy, so a pass over it would only ever reach the
            ! directories that happen to be open.
            new_node%is_gitignored = path_is_ignored(state, &
                trim(new_node%full_path), entries(i)%is_dir)
            if (entries(i)%is_dir) then
                new_node%expanded = .false.
                new_node%scan_pending = .true.
            else
                call apply_git_status(state, new_node)
            end if
            new_node%parent => node
            new_node%next_sibling => node%first_child
            node%first_child => new_node
            if (.not. (new_node%is_dotfile .or. new_node%is_gitignored)) &
                any_visible = .true.
        end do

        call sort_children(node)

        ! Grey-out marker: children exist but all are dotfiles (per-level
        ! stand-in for mark_empty_directories, which only runs in eager mode)
        node%all_children_hidden = (n_entries > 0 .and. .not. any_visible)
    end subroutine scan_directory_children

    ! Expand a directory node, scanning its children first if they were
    ! never loaded. Rebuilds the selectable list and keeps the selection on
    ! the same node. The single expansion path for toggle/right/space.
    subroutine tree_expand_node(state, node)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer, intent(inout) :: node
        integer :: i

        if (.not. associated(node)) return
        if (node%is_file) return

        if (node%scan_pending) call scan_directory_children(state, node)
        node%expanded = .true.

        ! Rebuild selectable list to reflect new visibility
        if (allocated(state%selectable_files)) deallocate(state%selectable_files)
        call build_selectable_list(state%root, state%selectable_files, state%n_selectable, state%hide_dotfiles)

        ! Keep the selection on this node in the new list
        do i = 1, state%n_selectable
            if (associated(state%selectable_files(i)%node, node)) then
                state%selected_index = i
                return
            end if
        end do
        if (state%selected_index > state%n_selectable .and. state%n_selectable > 0) then
            state%selected_index = state%n_selectable
        end if
    end subroutine tree_expand_node

    subroutine tree_toggle_expand(state)
        type(tree_state_t), intent(inout) :: state
        type(tree_node_t), pointer :: selected_node
        integer :: i

        if (state%selected_index < 1 .or. state%selected_index > state%n_selectable) return

        ! Get the selected node via the pointer
        selected_node => state%selectable_files(state%selected_index)%node

        if (.not. associated(selected_node)) return

        ! Only toggle directories that have (or may have) children
        if (.not. selected_node%is_file .and. &
            (selected_node%scan_pending .or. associated(selected_node%first_child))) then

            if (.not. selected_node%expanded) then
                ! Expanding: route through the lazy scan path (also rebuilds
                ! the selectable list and restores selection)
                call tree_expand_node(state, selected_node)
                return
            end if
            selected_node%expanded = .false.

            ! Rebuild selectable list to reflect new visibility
            if (allocated(state%selectable_files)) deallocate(state%selectable_files)
            call build_selectable_list(state%root, state%selectable_files, state%n_selectable, state%hide_dotfiles)

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
