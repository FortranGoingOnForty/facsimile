! Workspace management module
! Handles workspace detection, creation, loading, and saving

module workspace_module
    use editor_state_module, only: editor_state_t, create_tab
    implicit none
    private

    public :: workspace_exists, workspace_init, workspace_load, workspace_save
    public :: workspace_get_path, workspace_detect_from_file, workspace_is_file_in_workspace
    public :: workspace_save_state, workspace_restore_state

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
        integer :: unit, ios

        ! Use realpath via shell command
        call execute_command_line("realpath '" // trim(input_path) // "' > /tmp/.fac_realpath 2>/dev/null", &
                                 wait=.true.)

        open(newunit=unit, file='/tmp/.fac_realpath', status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) absolute_path
            close(unit)
            call execute_command_line("rm -f /tmp/.fac_realpath", wait=.true.)
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
        character(len=20) :: timestamp
        logical :: is_relative

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Open file for writing
        open(newunit=unit, file=workspace_file, status='replace', iostat=ios)
        if (ios /= 0) return

        ! Get current timestamp (simplified)
        call date_and_time(timestamp)

        ! Write JSON header
        write(unit, '(A)') '{'
        write(unit, '(A)') '  "version": "1.0",'
        write(unit, '(A)') '  "workspace_path": "' // trim(dir_path) // '",'
        write(unit, '(A)') '  "last_opened": "' // trim(timestamp) // '",'
        write(unit, '(A)') '  "tabs": ['

        ! Write tabs
        if (allocated(editor%tabs)) then
            ws_len = len_trim(dir_path)
            do i = 1, size(editor%tabs)
                ! Write tab start
                write(unit, '(A)', advance='no') '    {'

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
                write(unit, '(A)', advance='no') '"filename": "'
                if (is_relative) then
                    write(unit, '(A)', advance='no') trim(relative_path)
                else if (allocated(editor%tabs(i)%filename)) then
                    write(unit, '(A)', advance='no') trim(editor%tabs(i)%filename)
                else
                    write(unit, '(A)', advance='no') 'untitled'
                end if
                write(unit, '(A)', advance='no') '", '

                ! Write flags
                if (editor%tabs(i)%is_orphan) then
                    write(unit, '(A)') '"is_orphan": true, '
                else
                    write(unit, '(A)') '"is_orphan": false, '
                end if

                if (editor%tabs(i)%modified) then
                    write(unit, '(A)') '"modified": true, '
                else
                    write(unit, '(A)') '"modified": false, '
                end if

                ! Write panes array
                write(unit, '(A)') '"panes": ['
                if (allocated(editor%tabs(i)%panes) .and. size(editor%tabs(i)%panes) > 0) then
                    do j = 1, size(editor%tabs(i)%panes)
                        write(unit, '(A)', advance='no') '      {'

                        ! Write pane coordinates
                        write(unit, '(A,F6.4,A)', advance='no') '"x_start": ', &
                            editor%tabs(i)%panes(j)%x_start, ', '
                        write(unit, '(A,F6.4,A)', advance='no') '"y_start": ', &
                            editor%tabs(i)%panes(j)%y_start, ', '
                        write(unit, '(A,F6.4,A)', advance='no') '"x_end": ', &
                            editor%tabs(i)%panes(j)%x_end, ', '
                        write(unit, '(A,F6.4,A)', advance='no') '"y_end": ', &
                            editor%tabs(i)%panes(j)%y_end, ', '

                        ! Write pane filename (may differ from tab filename)
                        if (allocated(editor%tabs(i)%panes(j)%filename)) then
                            ! Check if we should use relative path
                            if (.not. editor%tabs(i)%is_orphan) then
                                if (len_trim(editor%tabs(i)%panes(j)%filename) > ws_len) then
                                    if (editor%tabs(i)%panes(j)%filename(1:ws_len) == dir_path(1:ws_len)) then
                                        if (editor%tabs(i)%panes(j)%filename(ws_len+1:ws_len+1) == '/') then
                                            write(unit, '(A)', advance='no') '"filename": "'
                                            write(unit, '(A)', advance='no') &
                                                trim(editor%tabs(i)%panes(j)%filename(ws_len+2:))
                                            write(unit, '(A)', advance='no') '", '
                                        else
                                            write(unit, '(A)', advance='no') '"filename": "'
                                            write(unit, '(A)', advance='no') &
                                                trim(editor%tabs(i)%panes(j)%filename)
                                            write(unit, '(A)', advance='no') '", '
                                        end if
                                    else
                                        write(unit, '(A)', advance='no') '"filename": "'
                                        write(unit, '(A)', advance='no') &
                                            trim(editor%tabs(i)%panes(j)%filename)
                                        write(unit, '(A)', advance='no') '", '
                                    end if
                                else
                                    write(unit, '(A)', advance='no') '"filename": "'
                                    write(unit, '(A)', advance='no') trim(editor%tabs(i)%panes(j)%filename)
                                    write(unit, '(A)', advance='no') '", '
                                end if
                            else
                                ! Orphan tab - use absolute path
                                write(unit, '(A)', advance='no') '"filename": "'
                                write(unit, '(A)', advance='no') trim(editor%tabs(i)%panes(j)%filename)
                                write(unit, '(A)', advance='no') '", '
                            end if
                        else
                            write(unit, '(A)', advance='no') '"filename": "", '
                        end if

                        ! Write cursor and viewport
                        if (allocated(editor%tabs(i)%panes(j)%cursors) .and. &
                            size(editor%tabs(i)%panes(j)%cursors) > 0) then
                            write(unit, '(A,I0,A)', advance='no') '"cursor_line": ', &
                                editor%tabs(i)%panes(j)%cursors(1)%line, ', '
                            write(unit, '(A,I0,A)', advance='no') '"cursor_column": ', &
                                editor%tabs(i)%panes(j)%cursors(1)%column, ', '
                        else
                            write(unit, '(A)', advance='no') '"cursor_line": 1, "cursor_column": 1, '
                        end if

                        write(unit, '(A,I0,A)', advance='no') '"viewport_line": ', &
                            editor%tabs(i)%panes(j)%viewport_line, ', '
                        write(unit, '(A,I0)', advance='no') '"viewport_column": ', &
                            editor%tabs(i)%panes(j)%viewport_column

                        ! Close pane object
                        if (j < size(editor%tabs(i)%panes)) then
                            write(unit, '(A)') '},'
                        else
                            write(unit, '(A)') '}'
                        end if
                    end do
                end if
                write(unit, '(A)', advance='no') '], '

                ! Write active pane index
                write(unit, '(A,I0)', advance='no') '"active_pane": ', &
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
        write(unit, '(A,I0,A)') '  "active_tab": ', editor%active_tab_index, ','
        write(unit, '(A)') '  "fuss_mode": {'
        if (editor%fuss_mode_active) then
            write(unit, '(A)') '    "active": true,'
        else
            write(unit, '(A)') '    "active": false,'
        end if
        write(unit, '(A)') '    "width": 30'
        write(unit, '(A)') '  }'
        write(unit, '(A)') '}'

        close(unit)
        success = .true.
    end subroutine workspace_save_state

    !> Restore editor state from workspace JSON file
    !> Note: For Phase 3, this is a simplified version that only restores the first pane
    !> Full multi-pane restoration will be added when needed
    subroutine workspace_restore_state(editor, dir_path, success)
        use text_buffer_module, only: buffer_load_file
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file, line, tab_filename, pane_filename, full_path
        integer :: unit, ios, colon_pos, quote1, quote2, comma_pos
        integer :: cursor_line, cursor_col, viewport_line, viewport_col
        real :: x_start, y_start, x_end, y_end
        logical :: in_tabs_array, is_orphan, reading_tab, in_panes_array, reading_pane
        integer :: load_status, tab_idx, pane_count
        character(len=20) :: value_str

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Open workspace file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios /= 0) return

        ! Parse JSON line by line (simple parser for our specific format)
        in_tabs_array = .false.
        reading_tab = .false.
        in_panes_array = .false.
        reading_pane = .false.
        tab_filename = ""
        pane_filename = ""
        is_orphan = .false.
        pane_count = 0

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = adjustl(line)

            ! Check if we're entering the tabs array
            if (index(line, '"tabs":') > 0) then
                in_tabs_array = .true.
                cycle
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

                    ! Create tab
                    call create_tab(editor, trim(full_path))
                    tab_idx = editor%active_tab_index

                    ! Set orphan flag and load file
                    if (allocated(editor%tabs) .and. tab_idx > 0) then
                        editor%tabs(tab_idx)%is_orphan = is_orphan

                        call buffer_load_file(editor%tabs(tab_idx)%buffer, trim(full_path), load_status)

                        ! Set cursor and viewport in first pane
                        if (allocated(editor%tabs(tab_idx)%panes) .and. size(editor%tabs(tab_idx)%panes) > 0) then
                            call buffer_load_file(editor%tabs(tab_idx)%panes(1)%buffer, trim(full_path), load_status)

                            ! Set pane coordinates (even if only 1 pane for now)
                            editor%tabs(tab_idx)%panes(1)%x_start = x_start
                            editor%tabs(tab_idx)%panes(1)%y_start = y_start
                            editor%tabs(tab_idx)%panes(1)%x_end = x_end
                            editor%tabs(tab_idx)%panes(1)%y_end = y_end

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
        success = .true.
    end subroutine workspace_restore_state

end module workspace_module
