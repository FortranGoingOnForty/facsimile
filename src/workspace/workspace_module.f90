! Workspace management module
! Handles workspace detection, creation, loading, and saving

module workspace_module
    use iso_fortran_env, only: int32
    use iso_c_binding, only: c_int
    use editor_state_module, only: editor_state_t, create_tab, sync_pane_to_editor
    use editor_state_module, only: group_create, group_find, group_add_member, &
                                   prune_empty_groups
    use editor_state_module, only: active_pane_of
    use text_buffer_module, only: buffer_t, init_buffer, buffer_to_string
    use lsp_server_manager_module, only: notify_file_opened
    use recents_module, only: recents_add_or_update
    use terminal_panel_module, only: terminal_panel_get_permille, &
                                     terminal_panel_set_default_permille
    implicit none
    private

    ! Interface to C getpid function
    interface
        function c_getpid() bind(c, name="getpid")
            use iso_c_binding, only: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface

    public :: workspace_exists, workspace_init, workspace_load, workspace_save
    public :: workspace_get_path, workspace_detect_from_file, workspace_is_file_in_workspace
    public :: workspace_save_state, workspace_restore_state
    public :: workspace_switch

    integer, parameter :: MAX_PATH_LEN = 512

contains

    !> Check if a directory has a workspace
    function workspace_exists(dir_path) result(exists)
        character(len=*), intent(in) :: dir_path
        logical :: exists
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        ! Build path to workspace.json
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Try to open the file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        exists = (ios == 0)

        if (exists) close(unit)
    end function workspace_exists

    !> Detect workspace from a file path (search parent directories)
    function workspace_detect_from_file(file_path) result(workspace_path)
        character(len=*), intent(in) :: file_path
        character(len=MAX_PATH_LEN) :: workspace_path
        character(len=MAX_PATH_LEN) :: current_dir, parent_dir
        integer :: last_slash

        workspace_path = ""

        ! Get directory of file
        last_slash = index(file_path, "/", back=.true.)
        if (last_slash > 0) then
            current_dir = file_path(1:last_slash-1)
        else
            current_dir = "."
        end if

        ! Search up the directory tree for a workspace
        do while (len_trim(current_dir) > 0)
            if (workspace_exists(current_dir)) then
                workspace_path = current_dir
                return
            end if

            ! Move to parent directory
            if (current_dir == "/" .or. current_dir == ".") exit

            last_slash = index(current_dir, "/", back=.true.)
            if (last_slash > 0) then
                parent_dir = current_dir(1:last_slash-1)
                if (len_trim(parent_dir) == 0) parent_dir = "/"
                current_dir = parent_dir
            else
                exit
            end if
        end do
    end function workspace_detect_from_file

    !> Get absolute workspace path from potentially relative path
    subroutine workspace_get_path(input_path, absolute_path)
        character(len=*), intent(in) :: input_path
        character(len=MAX_PATH_LEN), intent(out) :: absolute_path
        character(len=MAX_PATH_LEN) :: temp_file, pid_str
        integer :: unit, ios, pid

        ! Get process ID for unique temp file (avoid race conditions)
        pid = c_getpid()
        write(pid_str, '(I0)') pid
        temp_file = '/tmp/.fac_realpath_' // trim(pid_str)

        ! Use realpath via shell command
        call execute_command_line("realpath '" // trim(input_path) // "' > '" // trim(temp_file) // "' 2>/dev/null", &
                                 wait=.true.)

        open(newunit=unit, file=trim(temp_file), status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) absolute_path
            close(unit)
            call execute_command_line("rm -f '" // trim(temp_file) // "'", wait=.true.)
        else
            absolute_path = input_path
        end if
    end subroutine workspace_get_path

    !> Check if a file path is within the workspace directory
    function workspace_is_file_in_workspace(file_path, workspace_path) result(is_in_workspace)
        character(len=*), intent(in) :: file_path, workspace_path
        logical :: is_in_workspace
        character(len=MAX_PATH_LEN) :: abs_file_path, abs_workspace_path
        integer :: ws_len

        is_in_workspace = .false.

        ! Get absolute paths for both
        call workspace_get_path(file_path, abs_file_path)
        call workspace_get_path(workspace_path, abs_workspace_path)

        ! Check if file path starts with workspace path
        ws_len = len_trim(abs_workspace_path)
        if (len_trim(abs_file_path) > ws_len) then
            ! Check if file path starts with workspace path followed by /
            if (abs_file_path(1:ws_len) == abs_workspace_path(1:ws_len)) then
                if (abs_file_path(ws_len+1:ws_len+1) == '/') then
                    is_in_workspace = .true.
                end if
            end if
        end if
    end function workspace_is_file_in_workspace

    !> Initialize a new workspace in the given directory
    subroutine workspace_init(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: fac_dir, workspace_file, backup_dir
        integer :: unit, ios

        success = .false.

        ! Create .fac directory
        fac_dir = trim(dir_path) // "/.fac"
        call execute_command_line("mkdir -p '" // trim(fac_dir) // "' 2>/dev/null", wait=.true.)

        ! Create backups subdirectory
        backup_dir = trim(fac_dir) // "/backups"
        call execute_command_line("mkdir -p '" // trim(backup_dir) // "' 2>/dev/null", wait=.true.)

        ! Create initial workspace.json
        workspace_file = trim(fac_dir) // "/workspace.json"

        open(newunit=unit, file=workspace_file, status='replace', iostat=ios)
        if (ios /= 0) return

        ! Write minimal initial workspace JSON
        write(unit, '(a)') '{'
        write(unit, '(a)') '  "version": "1.0",'
        write(unit, '(a)') '  "workspace_path": "' // trim(dir_path) // '",'
        write(unit, '(a)') '  "last_opened": "",'
        write(unit, '(a)') '  "tabs": [],'
        write(unit, '(a)') '  "orphan_tabs": [],'
        write(unit, '(a)') '  "active_tab": 0,'
        write(unit, '(a)') '  "fuss_mode": {'
        write(unit, '(a)') '    "active": false,'
        write(unit, '(a)') '    "width": 30'
        write(unit, '(a)') '  }'
        write(unit, '(a)') '}'

        close(unit)
        success = .true.

        ! Track in recents (extract basename for label)
        call track_workspace_in_recents(dir_path)
    end subroutine workspace_init

    !> Load workspace state (stub for now - Phase 3 will implement full deserialization)
    subroutine workspace_load(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! For now, just verify the file exists and is readable
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios == 0) then
            close(unit)
            success = .true.
            ! TODO Phase 3: Parse JSON and restore tabs/panes/cursor state

            ! Track in recents
            call track_workspace_in_recents(dir_path)
        end if
    end subroutine workspace_load

    !> Save workspace state (stub for now - Phase 3 will implement full serialization)
    subroutine workspace_save(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! For now, just verify we can write to the file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios == 0) then
            close(unit)
            success = .true.
            ! TODO Phase 3: Serialize current tabs/panes/cursor state to JSON
        end if
    end subroutine workspace_save

    !> Save editor state to workspace JSON file
    subroutine workspace_save_state(editor, dir_path, success)
        type(editor_state_t), intent(in) :: editor
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file, relative_path
        integer :: unit, ios, i, j, ws_len
        integer :: active_out
        character(len=20) :: timestamp
        logical :: is_relative

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Open file for writing
        open(newunit=unit, file=workspace_file, status='replace', iostat=ios)
        if (ios /= 0) return

        ! Map the active tab onto the deduplicated list written below,
        ! otherwise "active_tab" can point past the saved tab count and
        ! restore lands on the wrong tab.
        active_out = 1
        if (allocated(editor%tabs)) then
            block
                integer, allocatable :: out_pos(:)
                integer :: k, n_out
                allocate(out_pos(size(editor%tabs)))
                n_out = 0
                do i = 1, size(editor%tabs)
                    out_pos(i) = 0
                    if (allocated(editor%tabs(i)%filename)) then
                        do k = 1, i - 1
                            if (allocated(editor%tabs(k)%filename)) then
                                if (trim(editor%tabs(k)%filename) == trim(editor%tabs(i)%filename)) then
                                    out_pos(i) = out_pos(k)
                                    exit
                                end if
                            end if
                        end do
                    end if
                    if (out_pos(i) == 0) then
                        n_out = n_out + 1
                        out_pos(i) = n_out
                    end if
                end do
                if (editor%active_tab_index >= 1 .and. &
                    editor%active_tab_index <= size(editor%tabs)) then
                    active_out = max(1, out_pos(editor%active_tab_index))
                end if
            end block
        end if

        ! Get current timestamp (simplified)
        call date_and_time(timestamp)

        ! Write JSON header
        write(unit, '(A)') '{'
        ! 1.1 added tab groups; 1.2 adds the terminal panel's height. Both
        ! purely additive: an older file simply has no such key, the parser
        ! never fires that branch, and the field keeps its default. The version
        ! string is written for humans reading the file -- nothing reads it
        ! back, and nothing should have to.
        write(unit, '(A)') '  "version": "1.2",'
        write(unit, '(A)') '  "workspace_path": "' // trim(dir_path) // '",'
        write(unit, '(A)') '  "last_opened": "' // trim(timestamp) // '",'

        ! Groups come FIRST. The restore parser creates a tab when it reaches
        ! the close of that tab's first pane, so a group referenced by a tab
        ! has to already exist by then.
        write(unit, '(A)') '  "tab_groups": ['
        if (allocated(editor%groups)) then
            do i = 1, size(editor%groups)
                write(unit, '(A)') '    {'
                write(unit, '(A,I0,A)') '      "id": ', editor%groups(i)%id, ','
                write(unit, '(A)', advance='no') '      "label": "'
                if (allocated(editor%groups(i)%label)) &
                    write(unit, '(A)', advance='no') trim(editor%groups(i)%label)
                write(unit, '(A)') '",'
                write(unit, '(A)', advance='no') '      "dir_path": "'
                if (allocated(editor%groups(i)%dir_path)) &
                    write(unit, '(A)', advance='no') trim(editor%groups(i)%dir_path)
                write(unit, '(A)') '",'
                write(unit, '(A)', advance='no') '      "active_member": "'
                if (allocated(editor%groups(i)%last_active_member)) &
                    write(unit, '(A)', advance='no') &
                        trim(editor%groups(i)%last_active_member)
                write(unit, '(A)') '"'
                if (i < size(editor%groups)) then
                    write(unit, '(A)') '    },'
                else
                    write(unit, '(A)') '    }'
                end if
            end do
        end if
        write(unit, '(A)') '  ],'

        write(unit, '(A)') '  "tabs": ['

        ! Write tabs (deduplicate by filename)
        if (allocated(editor%tabs)) then
            ws_len = len_trim(dir_path)
            do i = 1, size(editor%tabs)
                ! Skip duplicate filenames (keep first occurrence)
                block
                    logical :: is_dup
                    integer :: k
                    is_dup = .false.
                    if (allocated(editor%tabs(i)%filename)) then
                        do k = 1, i - 1
                            if (allocated(editor%tabs(k)%filename)) &
                                then
                                if (trim(editor%tabs(k)%filename) &
                                    == trim(editor%tabs(i) &
                                    %filename)) then
                                    is_dup = .true.
                                    exit
                                end if
                            end if
                        end do
                    end if
                    if (is_dup) cycle
                end block

                ! Write tab start
                write(unit, '(A)') '    {'

                ! Determine if we should use relative path
                is_relative = .false.
                if (.not. editor%tabs(i)%is_orphan .and. allocated(editor%tabs(i)%filename)) then
                    ! Check if filename starts with workspace path
                    if (len_trim(editor%tabs(i)%filename) > ws_len) then
                        if (editor%tabs(i)%filename(1:ws_len) == dir_path(1:ws_len)) then
                            if (editor%tabs(i)%filename(ws_len+1:ws_len+1) == '/') then
                                is_relative = .true.
                                relative_path = editor%tabs(i)%filename(ws_len+2:)
                            end if
                        end if
                    end if
                end if

                ! Write filename
                write(unit, '(A)', advance='no') '      "filename": "'
                if (is_relative) then
                    write(unit, '(A)', advance='no') trim(relative_path)
                else if (allocated(editor%tabs(i)%filename)) then
                    write(unit, '(A)', advance='no') trim(editor%tabs(i)%filename)
                else
                    write(unit, '(A)', advance='no') 'untitled'
                end if
                write(unit, '(A)') '", '

                ! Write flags
                if (editor%tabs(i)%is_orphan) then
                    write(unit, '(A)') '      "is_orphan": true, '
                else
                    write(unit, '(A)') '      "is_orphan": false, '
                end if

                if (editor%tabs(i)%modified) then
                    write(unit, '(A)') '      "modified": true, '
                else
                    write(unit, '(A)') '      "modified": false, '
                end if

                ! Membership must be written BEFORE "panes": the parser
                ! creates the tab at the close of its first pane, so anything
                ! after that array arrives too late to apply.
                write(unit, '(A,I0,A)') '      "group": ', &
                    editor%tabs(i)%group_id, ','
                write(unit, '(A,I0,A)') '      "group_ordinal": ', &
                    editor%tabs(i)%group_ordinal, ','

                ! Write panes array
                write(unit, '(A)') '      "panes": ['
                if (allocated(editor%tabs(i)%panes) .and. size(editor%tabs(i)%panes) > 0) then
                    do j = 1, size(editor%tabs(i)%panes)
                        write(unit, '(A)') '        {'

                        ! Write pane coordinates - each on its own line for parseability
                        write(unit, '(A,F6.4,A)') '          "x_start": ', &
                            editor%tabs(i)%panes(j)%x_start, ','
                        write(unit, '(A,F6.4,A)') '          "y_start": ', &
                            editor%tabs(i)%panes(j)%y_start, ','
                        write(unit, '(A,F6.4,A)') '          "x_end": ', &
                            editor%tabs(i)%panes(j)%x_end, ','
                        write(unit, '(A,F6.4,A)') '          "y_end": ', &
                            editor%tabs(i)%panes(j)%y_end, ','

                        ! Write pane filename (may differ from tab filename)
                        if (allocated(editor%tabs(i)%panes(j)%filename)) then
                            ! Check if we should use relative path
                            if (.not. editor%tabs(i)%is_orphan) then
                                if (len_trim(editor%tabs(i)%panes(j)%filename) > ws_len) then
                                    if (editor%tabs(i)%panes(j)%filename(1:ws_len) == dir_path(1:ws_len)) then
                                        if (editor%tabs(i)%panes(j)%filename(ws_len+1:ws_len+1) == '/') then
                                            write(unit, '(A)') '          "filename": "' // &
                                                trim(editor%tabs(i)%panes(j)%filename(ws_len+2:)) // '",'
                                        else
                                            write(unit, '(A)') '          "filename": "' // &
                                                trim(editor%tabs(i)%panes(j)%filename) // '",'
                                        end if
                                    else
                                        write(unit, '(A)') '          "filename": "' // &
                                            trim(editor%tabs(i)%panes(j)%filename) // '",'
                                    end if
                                else
                                    write(unit, '(A)') '          "filename": "' // &
                                        trim(editor%tabs(i)%panes(j)%filename) // '",'
                                end if
                            else
                                ! Orphan tab - use absolute path
                                write(unit, '(A)') '          "filename": "' // &
                                    trim(editor%tabs(i)%panes(j)%filename) // '",'
                            end if
                        else
                            write(unit, '(A)') '          "filename": "",'
                        end if

                        ! Write cursor and viewport - each on own line
                        if (allocated(editor%tabs(i)%panes(j)%cursors) .and. &
                            size(editor%tabs(i)%panes(j)%cursors) > 0) then
                            write(unit, '(A,I0,A)') '          "cursor_line": ', &
                                editor%tabs(i)%panes(j)%cursors(1)%line, ','
                            write(unit, '(A,I0,A)') '          "cursor_column": ', &
                                editor%tabs(i)%panes(j)%cursors(1)%column, ','
                        else
                            write(unit, '(A)') '          "cursor_line": 1,'
                            write(unit, '(A)') '          "cursor_column": 1,'
                        end if

                        write(unit, '(A,I0,A)') '          "viewport_line": ', &
                            editor%tabs(i)%panes(j)%viewport_line, ','
                        write(unit, '(A,I0)') '          "viewport_column": ', &
                            editor%tabs(i)%panes(j)%viewport_column

                        ! Close pane object
                        write(unit, '(A)') '        }'
                        if (j < size(editor%tabs(i)%panes)) then
                            write(unit, '(A)') '        ,'
                        end if
                    end do
                end if
                write(unit, '(A)') '      ],'

                ! Write active pane index
                write(unit, '(A,I0)') '      "active_pane": ', &
                    editor%tabs(i)%active_pane_index

                ! Close tab object
                if (i < size(editor%tabs)) then
                    write(unit, '(A)') '},'
                else
                    write(unit, '(A)') '}'
                end if
            end do
        end if

        ! Write JSON footer
        write(unit, '(A)') '  ],'
        write(unit, '(A,I0,A)') '  "active_tab": ', active_out, ','
        write(unit, '(A)') '  "fuss_mode": {'
        if (editor%fuss_mode_active) then
            write(unit, '(A)') '    "active": true,'
        else
            write(unit, '(A)') '    "active": false,'
        end if
        write(unit, '(A)') '    "width": 30'
        write(unit, '(A)') '  },'
        ! The panel's height as a fraction of the screen, so it comes back the
        ! same shape on a differently-sized terminal. Unlike the fuss_mode
        ! block above -- whose "width" is a literal nothing ever reads -- this
        ! one round-trips; see the parser.
        write(unit, '(A)') '  "terminal": {'
        write(unit, '(A,I0)') '    "height_permille": ', &
            terminal_panel_get_permille(editor%terminal_panel)
        write(unit, '(A)') '  }'
        write(unit, '(A)') '}'

        close(unit)
        success = .true.
    end subroutine workspace_save_state

    !> Restore editor state from workspace JSON file
    !> Note: For Phase 3, this is a simplified version that only restores the first pane
    !> Full multi-pane restoration will be added when needed

    !> Put a restored tab back in its group.
    !>
    !> The file records the id the group had when it was saved; group_create
    !> hands out fresh ids on restore, so the two are matched through the map
    !> built while the groups array was parsed.
    subroutine attach_group(editor, tab_idx, file_gid, from_file, now, n_map)
        use editor_state_module, only: group_add_member
        type(editor_state_t), intent(inout) :: editor
        integer, intent(in) :: tab_idx, n_map
        integer(int32), intent(in) :: file_gid, from_file(:), now(:)
        integer :: k

        if (file_gid <= 0) return
        do k = 1, n_map
            if (from_file(k) == file_gid) then
                call group_add_member(editor, now(k), tab_idx)
                return
            end if
        end do
    end subroutine attach_group

    !> The integer value of a "key": N line.
    integer function json_int(line)
        character(len=*), intent(in) :: line
        integer :: c, e, ios

        json_int = 0
        c = index(line, ':')
        if (c <= 0) return
        e = index(line(c+1:), ',')
        if (e > 0) then
            read(line(c+1:c+e-1), *, iostat=ios) json_int
        else
            read(line(c+1:), *, iostat=ios) json_int
        end if
        if (ios /= 0) json_int = 0
    end function json_int

    !> The string value of a "key": "..." line.
    function json_str(line) result(text)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: text
        integer :: c, q1, q2

        text = ''
        c = index(line, ':')
        if (c <= 0) return
        q1 = index(line(c+1:), '"')
        if (q1 <= 0) return
        q1 = c + q1
        q2 = index(line(q1+1:), '"')
        if (q2 <= 0) return
        text = line(q1+1:q1+q2-1)
    end function json_str

    subroutine workspace_restore_state(editor, dir_path, success)
        use text_buffer_module, only: buffer_load_file
        use terminal_io_module, only: terminal_write
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file, line, tab_filename, pane_filename, full_path
        integer :: unit, ios, colon_pos, quote1, quote2, comma_pos
        integer :: cursor_line, cursor_col, viewport_line, viewport_col
        real :: x_start, y_start, x_end, y_end
        logical :: in_tabs_array, is_orphan, reading_tab, in_panes_array, reading_pane
        logical :: file_exists
        logical :: tab_ok
        logical :: in_groups_array
        integer(int32) :: g_id, new_gid, tab_group_file_id, tab_group_ord
        character(len=256) :: g_label, g_dir, g_member
        integer(int32) :: gid_from_file(128), gid_now(128)
        integer :: n_gid_map
        integer :: load_status, tab_idx, pane_count, file_unit
        character(len=20) :: value_str

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Clear existing tabs so a workspace switch starts clean
        if (allocated(editor%tabs)) deallocate(editor%tabs)
        allocate(editor%tabs(0))
        editor%active_tab_index = 0

        ! Open workspace file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios /= 0) then
            ! Workspace file doesn't exist or can't be read - initialize new workspace
            call workspace_init(dir_path, success)
            return
        end if

        ! Parse JSON line by line (simple parser for our specific format)
        in_tabs_array = .false.
        in_groups_array = .false.
        g_id = 0
        n_gid_map = 0
        tab_group_file_id = 0
        tab_group_ord = 0
        reading_tab = .false.
        in_panes_array = .false.
        reading_pane = .false.
        tab_filename = ""
        pane_filename = ""
        is_orphan = .false.
        pane_count = 0
        editor%active_tab_index = 1  ! Default to first tab


        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = adjustl(line)

            ! The groups array. Written before "tabs", so every group exists
            ! by the time a member tab is created below.
            !
            ! index() on '"tabs":' does not match '"tab_groups":', so the two
            ! branches cannot be confused for one another.
            if (index(line, '"tab_groups":') > 0) then
                in_groups_array = .true.
                cycle
            end if
            if (in_groups_array) then
                if (index(line, ']') > 0) then
                    in_groups_array = .false.
                    cycle
                end if
                if (index(line, '"id":') > 0) then
                    g_id = int(json_int(line), int32)
                    g_label = ''
                    g_dir = ''
                    g_member = ''
                    cycle
                end if
                if (index(line, '"label":') > 0) then
                    g_label = json_str(line)
                    cycle
                end if
                if (index(line, '"dir_path":') > 0) then
                    g_dir = json_str(line)
                    cycle
                end if
                if (index(line, '"active_member":') > 0) then
                    g_member = json_str(line)
                    ! Last field of the object: the group is complete.
                    if (g_id > 0) then
                        ! Absolute, the same way a tab's path is rebuilt a few
                        ! hundred lines below. The file stores dir_path
                        ! RELATIVE to the workspace, and leaving it that way
                        ! left the group's directory meaning "wherever the
                        ! process happens to be" -- while its members were
                        ! absolute. Nothing could then match a member against
                        ! a file listed in that directory, so the edit dialog
                        ! opened with no member ticked.
                        if (len_trim(g_dir) > 0) then
                            if (g_dir(1:1) /= '/') &
                                g_dir = trim(dir_path) // '/' // trim(g_dir)
                        end if
                        call group_create(editor, trim(g_dir), trim(g_label), new_gid)
                        ! Remember the id this group had in the file, so the
                        ! tabs below -- which reference the OLD id -- can be
                        ! matched to the group we just made.
                        if (n_gid_map < size(gid_from_file)) then
                            n_gid_map = n_gid_map + 1
                            gid_from_file(n_gid_map) = g_id
                            gid_now(n_gid_map) = new_gid
                        end if
                        if (len_trim(g_member) > 0) then
                            block
                                integer :: gx
                                gx = group_find(editor, new_gid)
                                if (gx > 0) editor%groups(gx)%last_active_member = &
                                    trim(g_member)
                            end block
                        end if
                    end if
                    g_id = 0
                    cycle
                end if
                cycle
            end if

            ! Check if we're entering the tabs array
            if (index(line, '"tabs":') > 0) then
                in_tabs_array = .true.
                cycle
            end if

            ! The terminal panel's stored height. Written after the tabs array
            ! closes, so in_tabs_array is already false by the time it arrives
            ! -- but guard anyway, since a pane could in principle carry a key
            ! with the same name later.
            if (.not. in_tabs_array .and. &
                index(line, '"height_permille":') > 0) then
                block
                    integer :: permille
                    permille = json_int(line)
                    ! Ignore junk rather than adopting it: a corrupt value here
                    ! would give the user a panel they cannot see and no
                    ! obvious way to guess why.
                    if (permille >= 50 .and. permille <= 950) then
                        call terminal_panel_set_default_permille( &
                            editor%terminal_panel, permille)
                    end if
                end block
                cycle
            end if

            ! Parse active_tab index (outside tabs array)
            if (.not. in_tabs_array .and. index(line, '"active_tab":') > 0) then
                colon_pos = index(line, ':')
                comma_pos = index(line, ',')
                if (colon_pos > 0) then
                    if (comma_pos > colon_pos) then
                        value_str = adjustl(line(colon_pos+1:comma_pos-1))
                    else
                        value_str = adjustl(line(colon_pos+1:))
                    end if
                    read(value_str, *, iostat=ios) editor%active_tab_index
                    ! Ensure it's at least 1 if tabs were restored
                    if (editor%active_tab_index < 1) editor%active_tab_index = 1
                end if
                cycle
            end if

            ! A tab's membership, recorded before its panes array so it is
            ! available when the tab is created below.
            if (in_tabs_array .and. .not. in_panes_array) then
                if (index(line, '"group":') > 0) then
                    tab_group_file_id = int(json_int(line), int32)
                    cycle
                end if
                if (index(line, '"group_ordinal":') > 0) then
                    tab_group_ord = int(json_int(line), int32)
                    cycle
                end if
            end if

            ! Check if we're exiting the tabs array
            if (in_tabs_array .and. index(line, '],') > 0 .and. .not. in_panes_array) then
                in_tabs_array = .false.
                cycle
            end if

            ! Check if we're starting a new tab object
            if (in_tabs_array .and. index(line, '{') > 0 .and. .not. reading_tab .and. .not. in_panes_array) then
                reading_tab = .true.
                tab_filename = ""
                is_orphan = .false.
                pane_count = 0
                cycle
            end if

            ! Check if we're entering panes array
            if (reading_tab .and. index(line, '"panes":') > 0) then
                in_panes_array = .true.
                cycle
            end if

            ! Check if we're exiting panes array
            if (in_panes_array .and. index(line, '],') > 0) then
                in_panes_array = .false.
                cycle
            end if

            ! Check if we're starting a new pane object
            if (in_panes_array .and. index(line, '{') > 0 .and. .not. reading_pane) then
                reading_pane = .true.
                pane_filename = ""
                cursor_line = 1
                cursor_col = 1
                viewport_line = 1
                viewport_col = 1
                x_start = 0.0
                y_start = 0.0
                x_end = 1.0
                y_end = 1.0
                pane_count = pane_count + 1
                cycle
            end if

            ! Check if we're ending a pane object
            if (reading_pane .and. index(line, '}') > 0) then
                ! For Phase 3: Only restore first pane of each tab for simplicity
                ! Full multi-pane restoration can be added later when workspace switching is implemented
                if (pane_count == 1 .and. len_trim(pane_filename) > 0) then
                    ! Build full path
                    if (is_orphan .or. pane_filename(1:1) == '/') then
                        full_path = pane_filename
                    else
                        full_path = trim(dir_path) // '/' // trim(pane_filename)
                    end if

                    ! Check if this is an untitled tab (in-memory only)
                    if (index(pane_filename, '[Untitled') == 1) then
                        ! Untitled tab - create without loading from file
                        call create_tab(editor, trim(pane_filename), tab_ok)
                        if (.not. tab_ok) cycle
                        tab_idx = editor%active_tab_index
                        call attach_group(editor, tab_idx, tab_group_file_id, &
                                          gid_from_file, gid_now, n_gid_map)

                        ! Set orphan flag and initialize empty buffers
                        if (allocated(editor%tabs) .and. tab_idx > 0) then
                            editor%tabs(tab_idx)%is_orphan = .false.

                            ! Initialize empty buffer for tab
                            call init_buffer(editor%tabs(tab_idx)%panes(active_pane_of(editor, tab_idx))%buffer)

                            ! Set cursor and viewport in first pane
                            if (allocated(editor%tabs(tab_idx)%panes) .and. size(editor%tabs(tab_idx)%panes) > 0) then
                                ! Initialize empty buffer for pane
                                call init_buffer(editor%tabs(tab_idx)%panes(1)%buffer)

                                ! Set pane filename
                                if (allocated(editor%tabs(tab_idx)%panes(1)%filename)) then
                                    deallocate(editor%tabs(tab_idx)%panes(1)%filename)
                                end if
                                allocate(character(len=len_trim(pane_filename)) :: editor%tabs(tab_idx)%panes(1)%filename)
                                editor%tabs(tab_idx)%panes(1)%filename = trim(pane_filename)

                                ! Only the first pane is restored, so it must
                                ! fill the tab. The persisted x/y bounds may come
                                ! from a multi-pane split (e.g. 0.0-0.25); using
                                ! them would leave a lone pane occupying a sliver
                                ! that can no longer be split.
                                editor%tabs(tab_idx)%panes(1)%x_start = 0.0
                                editor%tabs(tab_idx)%panes(1)%y_start = 0.0
                                editor%tabs(tab_idx)%panes(1)%x_end = 1.0
                                editor%tabs(tab_idx)%panes(1)%y_end = 1.0

                                if (allocated(editor%tabs(tab_idx)%panes(1)%cursors) .and. &
                                    size(editor%tabs(tab_idx)%panes(1)%cursors) > 0) then
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%line = cursor_line
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%column = cursor_col
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%desired_column = cursor_col
                                end if

                                editor%tabs(tab_idx)%panes(1)%viewport_line = viewport_line
                                editor%tabs(tab_idx)%panes(1)%viewport_column = viewport_col
                            end if
                        end if
                    else
                        ! Regular file tab - check if file exists before creating
                        file_exists = .false.
                        open(newunit=file_unit, file=trim(full_path), status='old', iostat=ios)
                        if (ios == 0) then
                            file_exists = .true.
                            close(file_unit)
                        end if

                        if (.not. file_exists) then
                            ! File doesn't exist - skip this tab silently
                            reading_pane = .false.
                            cycle
                        end if

                        ! Create tab
                        call create_tab(editor, trim(full_path), tab_ok)
                        if (.not. tab_ok) cycle
                        tab_idx = editor%active_tab_index
                        call attach_group(editor, tab_idx, tab_group_file_id, &
                                          gid_from_file, gid_now, n_gid_map)

                        ! Set orphan flag and load file
                        if (allocated(editor%tabs) .and. tab_idx > 0) then
                            editor%tabs(tab_idx)%is_orphan = is_orphan
                            call buffer_load_file(editor%tabs(tab_idx)%panes(active_pane_of(editor, tab_idx))%buffer, &
                                trim(full_path), load_status)

                            ! Send LSP didOpen notification for restored tabs
                            if (load_status == 0 .and. editor%tabs(tab_idx)%num_lsp_servers > 0) then
                                block
                                    integer :: srv_i
                                    do srv_i = 1, editor%tabs(tab_idx)%num_lsp_servers
                                        call notify_file_opened(editor%lsp_manager, &
                                            editor%tabs(tab_idx)%lsp_server_indices(srv_i), &
                                            trim(full_path), &
                                                buffer_to_string(editor%tabs(tab_idx)%panes(active_pane_of(editor, &
                                                tab_idx))%buffer))
                                    end do
                                end block
                            end if

                            ! Set cursor and viewport in first pane
                            if (allocated(editor%tabs(tab_idx)%panes) .and. size(editor%tabs(tab_idx)%panes) > 0) then
                                ! Already read above, into this same pane.

                                ! Only the first pane is restored, so force it to
                                ! fill the tab. Persisted bounds may belong to a
                                ! multi-pane split and would leave a lone pane in
                                ! a sliver that cannot be split (alt-v/alt-s).
                                editor%tabs(tab_idx)%panes(1)%x_start = 0.0
                                editor%tabs(tab_idx)%panes(1)%y_start = 0.0
                                editor%tabs(tab_idx)%panes(1)%x_end = 1.0
                                editor%tabs(tab_idx)%panes(1)%y_end = 1.0

                                if (allocated(editor%tabs(tab_idx)%panes(1)%cursors) .and. &
                                    size(editor%tabs(tab_idx)%panes(1)%cursors) > 0) then
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%line = cursor_line
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%column = cursor_col
                                    editor%tabs(tab_idx)%panes(1)%cursors(1)%desired_column = cursor_col
                                end if

                                editor%tabs(tab_idx)%panes(1)%viewport_line = viewport_line
                                editor%tabs(tab_idx)%panes(1)%viewport_column = viewport_col
                            end if
                        end if
                    end if
                end if

                reading_pane = .false.
                cycle
            end if

            ! Check if we're ending a tab object
            if (reading_tab .and. index(line, '}') > 0 .and. .not. in_panes_array) then
                reading_tab = .false.
                cycle
            end if

            ! Parse tab-level properties
            if (reading_tab .and. .not. in_panes_array) then
                ! Extract tab filename (fallback if no panes)
                if (index(line, '"filename":') > 0) then
                    quote1 = index(line, '"', .true.)
                    if (quote1 > 0) then
                        quote2 = index(line(1:quote1-1), '"', .true.)
                        if (quote2 > 0) then
                            tab_filename = line(quote2+1:quote1-1)
                        end if
                    end if
                end if

                ! Extract is_orphan
                if (index(line, '"is_orphan":') > 0) then
                    is_orphan = index(line, 'true') > 0
                end if
            end if

            ! Parse pane properties
            if (reading_pane) then
                ! Extract pane filename
                if (index(line, '"filename":') > 0) then
                    quote1 = index(line, '"', .true.)
                    if (quote1 > 0) then
                        quote2 = index(line(1:quote1-1), '"', .true.)
                        if (quote2 > 0) then
                            pane_filename = line(quote2+1:quote1-1)
                        end if
                    end if
                end if

                ! Extract coordinates
                if (index(line, '"x_start":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0 .and. comma_pos > colon_pos) then
                        value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        read(value_str, *, iostat=ios) x_start
                    end if
                end if

                if (index(line, '"y_start":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0 .and. comma_pos > colon_pos) then
                        value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        read(value_str, *, iostat=ios) y_start
                    end if
                end if

                if (index(line, '"x_end":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0 .and. comma_pos > colon_pos) then
                        value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        read(value_str, *, iostat=ios) x_end
                    end if
                end if

                if (index(line, '"y_end":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0 .and. comma_pos > colon_pos) then
                        value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        read(value_str, *, iostat=ios) y_end
                    end if
                end if

                ! Extract cursor_line
                if (index(line, '"cursor_line":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0) then
                        if (comma_pos > colon_pos) then
                            value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        else
                            value_str = adjustl(line(colon_pos+1:))
                        end if
                        read(value_str, *, iostat=ios) cursor_line
                    end if
                end if

                ! Extract cursor_column
                if (index(line, '"cursor_column":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0) then
                        if (comma_pos > colon_pos) then
                            value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        else
                            value_str = adjustl(line(colon_pos+1:))
                        end if
                        read(value_str, *, iostat=ios) cursor_col
                    end if
                end if

                ! Extract viewport_line
                if (index(line, '"viewport_line":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0) then
                        if (comma_pos > colon_pos) then
                            value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        else
                            value_str = adjustl(line(colon_pos+1:))
                        end if
                        read(value_str, *, iostat=ios) viewport_line
                    end if
                end if

                ! Extract viewport_column
                if (index(line, '"viewport_column":') > 0) then
                    colon_pos = index(line, ':')
                    comma_pos = index(line, ',')
                    if (colon_pos > 0) then
                        if (comma_pos > colon_pos) then
                            value_str = adjustl(line(colon_pos+1:comma_pos-1))
                        else
                            value_str = adjustl(line(colon_pos+1:))
                        end if
                        read(value_str, *, iostat=ios) viewport_col
                    end if
                end if
            end if
        end do

        close(unit)

        ! Clamp active_tab_index to valid range
        if (allocated(editor%tabs)) then
            if (size(editor%tabs) > 0) then
                if (editor%active_tab_index > size(editor%tabs)) then
                    editor%active_tab_index = size(editor%tabs)
                end if
                if (editor%active_tab_index < 1) then
                    editor%active_tab_index = 1
                end if
            else
                editor%active_tab_index = 0  ! No tabs
            end if
        else
            editor%active_tab_index = 0  ! No tabs
        end if

        ! Restore skips tabs whose file has gone, so a group can come back
        ! with no members at all. Drop those rather than showing "name (0)".
        call prune_empty_groups(editor)

        ! Sync the active pane to editor state so status bar shows correct filename
        if (allocated(editor%tabs) .and. editor%active_tab_index > 0) then
            if (editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                    if (editor%tabs(editor%active_tab_index)%active_pane_index > 0 .and. &
                        editor%tabs(editor%active_tab_index)%active_pane_index <= &
                        size(editor%tabs(editor%active_tab_index)%panes)) then
                        call sync_pane_to_editor(editor, editor%active_tab_index, &
                                                editor%tabs(editor%active_tab_index)%active_pane_index)
                    end if
                end if
            end if
        end if

        success = .true.
    end subroutine workspace_restore_state

    !> Track workspace in recents (helper function)
    subroutine track_workspace_in_recents(dir_path)
        character(len=*), intent(in) :: dir_path
        character(len=MAX_PATH_LEN) :: label
        logical :: recents_success
        integer :: i

        ! Extract basename for label
        label = dir_path
        do i = len_trim(dir_path), 1, -1
            if (dir_path(i:i) == '/') then
                label = dir_path(i+1:)
                exit
            end if
        end do

        ! Add or update in recents
        call recents_add_or_update(dir_path, trim(label), recents_success)
        ! Silently ignore recents failures
    end subroutine track_workspace_in_recents

    !> Switch to a different workspace
    subroutine workspace_switch(editor, new_workspace_path, success)
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: new_workspace_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: old_workspace_path
        logical :: save_success

        success = .false.

        ! Save current workspace state
        if (allocated(editor%workspace_path)) then
            old_workspace_path = editor%workspace_path
            call workspace_save_state(editor, old_workspace_path, save_success)
            ! Continue even if save fails - best effort
        end if

        ! Update workspace path
        editor%workspace_path = trim(new_workspace_path)

        ! Check if new workspace exists
        if (.not. workspace_exists(new_workspace_path)) then
            ! Create new workspace
            call workspace_init(new_workspace_path, success)
            if (.not. success) then
                ! Restore old workspace path on failure
                if (len_trim(old_workspace_path) > 0) then
                    editor%workspace_path = old_workspace_path
                end if
                return
            end if
        end if

        ! Load/restore new workspace state
        call workspace_restore_state(editor, new_workspace_path, success)

        ! Track in recents
        call track_workspace_in_recents(new_workspace_path)
    end subroutine workspace_switch

end module workspace_module
