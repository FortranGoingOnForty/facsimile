program facsimile
    use iso_fortran_env, only: error_unit, input_unit, output_unit, int64
    use version_module
    use terminal_io_module
    use input_handler_module, only: get_key_input
    use editor_state_module
    use text_buffer_module
    use renderer_module
    use command_handler_module
    use workspace_module
    use backup_module
    use save_prompt_module
    use welcome_menu_module, only: show_welcome_menu
    use fortress_navigator_module, only: open_fortress_navigator
    implicit none

    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    character(len=32) :: key_input
    character(len=512) :: filename, arg, workspace_dir
    logical :: running, should_quit, is_workspace_mode, workspace_success
    logical :: welcome_cancelled, is_browse, nav_cancelled, is_directory
    character(len=:), allocatable :: selected_path
    integer :: status, argc, rows, cols


    ! Get command line arguments
    argc = command_argument_count()
    is_workspace_mode = .false.
    workspace_dir = ""
    filename = ""

    if (argc > 0) then
        call get_command_argument(1, arg)

        ! Handle version flags
        if (trim(arg) == '--version' .or. trim(arg) == '-v') then
            write(output_unit, '(A,A)') 'fac version ', VERSION
            stop
        end if

        ! Handle help flags
        if (trim(arg) == '--help' .or. trim(arg) == '-h') then
            call print_help()
            stop
        end if

        ! Check if argument is a directory (workspace mode)
        ! Use test -d which is POSIX compliant (works on Linux, macOS, BSD)
        call execute_command_line("test -d '" // trim(arg) // &
            "' && echo 'Directory' > /tmp/.fac_filetype || " // &
            "echo 'File' > /tmp/.fac_filetype", wait=.true.)
        call read_file_type(status)
        if (status == 0) then
            ! Directory - workspace mode
            is_workspace_mode = .true.
            call workspace_get_path(trim(arg), workspace_dir)
        else
            ! File - check if parent directory has a workspace
            workspace_dir = workspace_detect_from_file(trim(arg))
            if (len_trim(workspace_dir) > 0) then
                is_workspace_mode = .true.
            end if
            filename = arg
        end if
    else
        ! No arguments - launch Fortress welcome menu (Phase 5)
        call terminal_init()
        call show_welcome_menu(selected_path, welcome_cancelled)

        if (welcome_cancelled) then
            ! Check if user wants to browse filesystem
            is_browse = .false.
            if (allocated(selected_path) .and. selected_path == "BROWSE") then
                is_browse = .true.
            end if

            call terminal_cleanup()

            if (is_browse) then
                ! Launch fortress navigator
                call terminal_init()
                call open_fortress_navigator(selected_path, is_directory, nav_cancelled)
                call terminal_cleanup()

                if (nav_cancelled .or. .not. allocated(selected_path)) then
                    ! User cancelled navigation too
                    stop
                end if
            else
                ! User just cancelled
                stop
            end if
        end if

        ! User selected a workspace from welcome menu or navigator
        ! Treat it as a directory argument
        if (allocated(selected_path)) then
            ! Check if user selected CURRENT DIRECTORY option
            if (selected_path == "CWD") then
                ! Get actual current working directory
                call get_workspace_path(selected_path)
                arg = selected_path
            else
                arg = selected_path
            end if

            ! Check if it's a directory
            call execute_command_line("test -d '" // trim(arg) // &
                "' && echo 'Directory' > /tmp/.fac_filetype || " // &
                "echo 'File' > /tmp/.fac_filetype", wait=.true.)
            call read_file_type(status)
            if (status == 0) then
                ! Directory - workspace mode
                is_workspace_mode = .true.
                call workspace_get_path(trim(arg), workspace_dir)
            else
                ! Invalid selection
                write(error_unit, '(A)') 'Error: Selected path is not a directory'
                stop 1
            end if
        else
            stop
        end if
    end if

    ! Handle workspace mode
    if (is_workspace_mode) then
        ! Check if workspace exists, create if not
        if (.not. workspace_exists(workspace_dir)) then
            call workspace_init(workspace_dir, workspace_success)
            if (.not. workspace_success) then
                write(error_unit, '(A)') 'Error: Failed to create workspace'
                stop 1
            end if
        else
            ! Load existing workspace
            call workspace_load(workspace_dir, workspace_success)
            if (.not. workspace_success) then
                write(error_unit, '(A)') 'Error: Failed to load workspace'
                stop 1
            end if
        end if
    end if

    ! Initialize editor
    call init_editor(editor)
    running = .true.

    ! Set workspace path
    if (is_workspace_mode) then
        ! Use detected/created workspace directory
        allocate(character(len=len_trim(workspace_dir)) :: editor%workspace_path)
        editor%workspace_path = trim(workspace_dir)

        ! Restore workspace state (tabs, cursor positions, etc.)
        call workspace_restore_state(editor, editor%workspace_path, workspace_success)
        ! Silently ignore restore failures for now
    else
        ! Single-file mode - use current directory
        call get_workspace_path(editor%workspace_path)
    end if

    ! Initialize terminal (before backup restore prompts)
    call terminal_init()
    call terminal_clear_screen()

    ! Check for backups and offer restoration (after workspace load, after terminal init)
    if (is_workspace_mode .and. backup_detect(editor%workspace_path)) then
        call handle_backup_restoration(editor, buffer)
    end if

    ! Get terminal size
    call terminal_get_size(rows, cols)
    editor%screen_rows = rows
    editor%screen_cols = cols

    ! Initialize renderer
    call init_renderer(rows, cols)

    ! Initialize command handler (for yank stack)
    call init_command_handler()

    ! Initialize buffer and load file if specified
    if (len_trim(filename) > 0) then
        ! Create a tab for the initial file
        call create_tab(editor, trim(filename))

        ! Load file into tab's buffer and first pane's buffer
        if (editor%active_tab_index > 0) then
            call buffer_load_file(editor%tabs(editor%active_tab_index)%buffer, trim(filename), status)

            ! Also load into first pane's buffer
            if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                call buffer_load_file(editor%tabs(editor%active_tab_index)%panes(1)%buffer, trim(filename), status)
                ! Copy first pane's buffer to main buffer
                call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(1)%buffer)
            else
                ! Copy tab's buffer to main buffer
                call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%buffer)
            end if
        else
            call buffer_load_file(buffer, trim(filename), status)
        end if

        if (status == 0) then
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)
        else
            ! If file doesn't exist, create empty buffer for new file
            call init_buffer(buffer)
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)
        end if
    else
        call init_buffer(buffer)
    end if

    ! Save initial file state for undo (position 0)
    call save_initial_state_for_undo(buffer, editor)

    ! Initial render
    call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)

    ! Main event loop
    do while (running)
        ! Get input
        call get_key_input(key_input, status)

        if (status == 0) then
            ! Sync buffer before and after input when using panes
            ! Before: copy active pane's buffer -> global buffer
            ! After: copy global buffer -> active pane's buffer
            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                    size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                    ! Get active pane index
                    status = editor%tabs(editor%active_tab_index)%active_pane_index
                    if (status > 0 .and. status <= size(editor%tabs(editor%active_tab_index)%panes)) then
                        ! Copy active pane's buffer to main buffer
                        call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(status)%buffer)
                    end if
                end if
            end if

            ! Process input
            call handle_key_command(key_input, editor, buffer, should_quit)

            ! Sync back to active pane and other instances
            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                    size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                    ! Get active pane index
                    status = editor%tabs(editor%active_tab_index)%active_pane_index
                    if (status > 0 .and. status <= size(editor%tabs(editor%active_tab_index)%panes)) then
                        ! Copy main buffer back to active pane's buffer
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(status)%buffer, buffer)

                        ! Sync to all instances of this file
                        if (allocated(editor%tabs(editor%active_tab_index)%panes(status)%filename)) then
                            call sync_buffer_to_all_instances(editor, &
                                editor%tabs(editor%active_tab_index)%panes(status)%filename, buffer)
                        end if
                    end if

                    ! Also update tab buffer for backwards compatibility
                    call copy_buffer(editor%tabs(editor%active_tab_index)%buffer, buffer)

                    ! Sync modified flag from buffer to tab
                    editor%tabs(editor%active_tab_index)%modified = buffer%modified
                end if
            end if

            if (should_quit) then
                running = .false.
            else
                ! Re-render screen after each command
                if (editor%fuss_mode_active) then
                    call render_screen_with_tree(buffer, editor, allocated(search_pattern), match_case_sensitive)
                else
                    call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)
                end if
            end if
        end if
    end do

    ! IMPORTANT: Save workspace state FIRST, before any prompts or cleanup
    ! This ensures we capture the current state before tabs might be closed
    if (should_quit .and. allocated(editor%workspace_path)) then
        call workspace_save_state(editor, editor%workspace_path, workspace_success)
    end if

    ! Handle unsaved files - prompt for save/backup
    if (allocated(editor%tabs) .and. allocated(editor%workspace_path)) then
        ! Workspace mode - handle all modified tabs
        call handle_unsaved_files_on_quit(editor, buffer, should_quit)
        ! If user cancelled (should_quit = .false.), skip cleanup and restart loop
    else if (buffer%modified .and. allocated(editor%workspace_path) .and. allocated(editor%filename)) then
        ! Single-file mode - handle the current buffer if modified
        call handle_single_file_on_quit(buffer, editor, should_quit)
    end if

    ! Only proceed with cleanup if actually quitting
    if (should_quit) then

        ! Cleanup
        call cleanup_renderer()
        call cleanup_command_handler()
        call terminal_cleanup()
        call cleanup_editor(editor)
        call cleanup_buffer(buffer)
    else
        ! User cancelled quit - re-render and continue
        running = .true.
        call terminal_clear_screen()
        if (editor%fuss_mode_active) then
            call render_screen_with_tree(buffer, editor, allocated(search_pattern), match_case_sensitive)
        else
            call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)
        end if
    end if

contains

    subroutine read_file_type(is_directory)
        integer, intent(out) :: is_directory
        character(len=20) :: file_type
        integer :: unit, ios

        is_directory = 1  ! Default to not a directory

        open(newunit=unit, file='/tmp/.fac_filetype', status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(A)', iostat=ios) file_type
            close(unit)
            call execute_command_line('rm -f /tmp/.fac_filetype', wait=.true.)
            if (ios == 0) then
                if (trim(file_type) == 'Directory') then
                    is_directory = 0
                end if
            end if
        end if
    end subroutine read_file_type

    subroutine get_workspace_path(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=1024) :: buffer
        integer :: status

        ! Use execute_command_line to get current directory
        call execute_command_line('pwd > /tmp/fac_pwd.txt', wait=.true., exitstat=status)
        if (status == 0) then
            open(unit=99, file='/tmp/fac_pwd.txt', status='old', action='read', iostat=status)
            if (status == 0) then
                read(99, '(A)', iostat=status) buffer
                close(99, status='delete')
                if (status == 0) then
                    path = trim(buffer)
                    return
                end if
            end if
        end if
        ! Fallback if command fails
        path = '.'
    end subroutine get_workspace_path

    subroutine print_help()
        write(output_unit, '(A)') 'fac - Fortran text editor'
        write(output_unit, '(A,A)') 'Version: ', VERSION
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Usage:'
        write(output_unit, '(A)') '  fac [filename]       Open a file for editing'
        write(output_unit, '(A)') '  fac                  Start with empty buffer'
        write(output_unit, '(A)') '  fac --version, -v    Show version information'
        write(output_unit, '(A)') '  fac --help, -h       Show this help message'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Key Bindings:'
        write(output_unit, '(A)') '  Ctrl-Q               Quit'
        write(output_unit, '(A)') '  Ctrl-S               Save'
        write(output_unit, '(A)') '  Ctrl-F               Find/Replace (unified prompt)'
        write(output_unit, '(A)') '  Ctrl-G               Go to line'
        write(output_unit, '(A)') '  Ctrl-Z               Undo'
        write(output_unit, '(A)') '  Ctrl-Y               Redo'
        write(output_unit, '(A)') '  Ctrl-X               Cut line'
        write(output_unit, '(A)') '  Ctrl-C               Copy line'
        write(output_unit, '(A)') '  Ctrl-V               Paste'
        write(output_unit, '(A)') '  Ctrl-D               Delete line'
        write(output_unit, '(A)') '  Ctrl-L               Toggle line numbers'
        write(output_unit, '(A)') '  Ctrl-P               Cycle yank stack backward'
        write(output_unit, '(A)') '  Ctrl-N               Cycle yank stack forward'
        write(output_unit, '(A)') '  Ctrl-T               New tab'
        write(output_unit, '(A)') '  Ctrl-W               Close tab'
        write(output_unit, '(A)') '  Alt-1 to Alt-9       Switch to tab 1-9'
        write(output_unit, '(A)') '  Ctrl-B               Toggle file tree (fuss mode)'
        write(output_unit, '(A)') '  Ctrl-\               Split pane vertically'
        write(output_unit, '(A)') '  Ctrl-_               Split pane horizontally'
        write(output_unit, '(A)') '  Ctrl-Arrow           Navigate between panes'
        write(output_unit, '(A)') '  Alt-X                Close current pane'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Find/Replace Mode (Ctrl-F):'
        write(output_unit, '(A)') '  Ctrl-F               Find next match'
        write(output_unit, '(A)') '  Ctrl-R               Replace current match'
        write(output_unit, '(A)') '  Ctrl-A               Replace all matches'
        write(output_unit, '(A)') '  Alt-C                Toggle case sensitivity'
        write(output_unit, '(A)') '  Alt-W                Toggle whole word match'
        write(output_unit, '(A)') '  Alt-R                Toggle regex mode'
        write(output_unit, '(A)') '  Tab                  Switch between find/replace fields'
        write(output_unit, '(A)') '  Enter                Jump to match and exit'
        write(output_unit, '(A)') '  ESC                  Exit find/replace mode'
    end subroutine print_help

    !> Prompt for filename and save an untitled file
    subroutine prompt_for_filename_and_save(editor, buffer, tab_index, success)
        use text_prompt_module, only: show_text_prompt
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: tab_index
        logical, intent(out) :: success
        character(len=512) :: new_filename
        logical :: cancelled
        integer :: save_status

        success = .false.

        ! Prompt for filename
        call show_text_prompt('Save as: ', new_filename, cancelled, editor%screen_rows)

        if (cancelled .or. len_trim(new_filename) == 0) then
            ! User cancelled or entered empty filename
            return
        end if

        ! Update the tab filename
        if (allocated(editor%tabs(tab_index)%filename)) then
            deallocate(editor%tabs(tab_index)%filename)
        end if
        allocate(character(len=len_trim(new_filename)) :: editor%tabs(tab_index)%filename)
        editor%tabs(tab_index)%filename = trim(new_filename)

        ! Update pane filename if exists
        if (allocated(editor%tabs(tab_index)%panes)) then
            if (size(editor%tabs(tab_index)%panes) > 0) then
                if (allocated(editor%tabs(tab_index)%panes(1)%filename)) then
                    deallocate(editor%tabs(tab_index)%panes(1)%filename)
                end if
                allocate(character(len=len_trim(new_filename)) :: editor%tabs(tab_index)%panes(1)%filename)
                editor%tabs(tab_index)%panes(1)%filename = trim(new_filename)
            end if
        end if

        ! Update editor filename
        if (allocated(editor%filename)) then
            deallocate(editor%filename)
        end if
        allocate(character(len=len_trim(new_filename)) :: editor%filename)
        editor%filename = trim(new_filename)

        ! Now save the file
        call buffer_save_file(buffer, new_filename, save_status)
        if (save_status == 0) then
            buffer%modified = .false.
            editor%tabs(tab_index)%modified = .false.
            success = .true.
        end if
    end subroutine prompt_for_filename_and_save

    !> Handle unsaved files on quit - prompt for save/backup
    subroutine handle_unsaved_files_on_quit(editor, buffer, should_quit)
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        logical, intent(inout) :: should_quit
        type(save_prompt_result_t) :: prompt_result
        integer :: i, save_status, modified_count, current_modified
        logical :: backup_success, save_all

        ! Count total modified tabs
        modified_count = 0
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%modified) then
                modified_count = modified_count + 1
            end if
        end do

        ! Process each modified tab
        current_modified = 0
        save_all = .false.
        do i = 1, size(editor%tabs)
            if (editor%tabs(i)%modified) then
                current_modified = current_modified + 1

                ! Skip prompt if "save all" was selected
                if (.not. save_all) then
                    ! Prompt user for this file with progress
                    call save_prompt(editor%tabs(i)%filename, prompt_result, current_modified, modified_count)

                    if (prompt_result%action == 'a') then
                        ! Save all - set flag and treat as save for this file
                        save_all = .true.
                        prompt_result%action = 's'
                    end if

                    if (prompt_result%action == 'c') then
                        ! User cancelled - don't quit
                        should_quit = .false.
                        return
                    end if
                else
                    ! Save all is active - auto-save this file
                    prompt_result%action = 's'
                end if

                if (prompt_result%action == 's') then
                    ! User wants to save - switch to this tab and save
                    editor%active_tab_index = i
                    call switch_to_tab_with_buffer(editor, i, buffer)

                    ! Check if this is an [Untitled] file - need to prompt for filename
                    if (index(editor%tabs(i)%filename, '[Untitled') == 1) then
                        call prompt_for_filename_and_save(editor, buffer, i, should_quit)
                        if (.not. should_quit) return  ! User cancelled
                    else
                        ! Save the file (no backup - it's saved!)
                        call buffer_save_file(buffer, editor%tabs(i)%filename, save_status)
                        if (save_status == 0) then
                            buffer%modified = .false.
                            editor%tabs(i)%modified = .false.
                        end if
                    end if

                else if (prompt_result%action == 'd') then
                    ! User wants to discard - create backup for later recovery
                    ! But skip [Untitled] files - they're in-memory only
                    if (index(editor%tabs(i)%filename, '[Untitled') /= 1) then
                        call backup_create(editor%workspace_path, editor%tabs(i)%filename, backup_success)
                    end if
                    ! Continue even if backup fails
                end if
            end if
        end do

        ! All handled - proceed with quit
        should_quit = .true.
    end subroutine handle_unsaved_files_on_quit

    !> Handle single file on quit (for non-workspace mode)
    subroutine handle_single_file_on_quit(buffer, editor, should_quit)
        type(buffer_t), intent(inout) :: buffer
        type(editor_state_t), intent(inout) :: editor
        logical, intent(inout) :: should_quit
        type(save_prompt_result_t) :: prompt_result
        integer :: save_status
        logical :: backup_success

        if (.not. buffer%modified) return
        if (.not. allocated(editor%filename)) return

        ! Prompt user for this file
        call save_prompt(editor%filename, prompt_result)

        if (prompt_result%action == 'y') then
            ! User wants to save
            call buffer_save_file(buffer, editor%filename, save_status)
            if (save_status == 0) then
                buffer%modified = .false.
                editor%modified = .false.
            end if
        else if (prompt_result%action == 'n') then
            ! User wants to skip - create backup
            call backup_create(editor%workspace_path, editor%filename, backup_success)
            ! Continue even if backup fails
        else if (prompt_result%action == 'c') then
            ! User cancelled - don't quit
            should_quit = .false.
            return
        end if

        ! Proceed with quit
        should_quit = .true.
    end subroutine handle_single_file_on_quit

    !> Handle a restored file - open in tab or reload existing tab
    subroutine handle_restored_file(editor, buffer, restored_file)
        use text_buffer_module, only: buffer_load_file
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: restored_file
        integer :: tab_idx, status
        logical :: found

        ! Check if file is already open in a tab
        found = .false.
        if (allocated(editor%tabs)) then
            do tab_idx = 1, size(editor%tabs)
                if (allocated(editor%tabs(tab_idx)%filename)) then
                    if (trim(editor%tabs(tab_idx)%filename) == trim(restored_file)) then
                        ! File is already in a tab - reload it
                        found = .true.
                        call switch_to_tab_with_buffer(editor, tab_idx, buffer)
                        call buffer_load_file(buffer, restored_file, status)
                        if (status == 0) then
                            buffer%modified = .false.
                            editor%tabs(tab_idx)%modified = .false.
                        end if
                        exit
                    end if
                end if
            end do
        end if

        ! If not found, create a new tab with this file
        if (.not. found) then
            call create_tab(editor, restored_file)
            if (allocated(editor%tabs) .and. editor%active_tab_index > 0) then
                ! Load the restored file into the buffer
                call buffer_load_file(buffer, restored_file, status)
                if (status == 0) then
                    buffer%modified = .false.
                    ! Now switch to the tab and sync the buffer
                    call switch_to_tab_with_buffer(editor, editor%active_tab_index, buffer)
                    if (allocated(editor%tabs) .and. editor%active_tab_index <= size(editor%tabs)) then
                        editor%tabs(editor%active_tab_index)%modified = .false.
                    end if
                end if
            end if
        end if
    end subroutine handle_restored_file

    !> Handle backup restoration on workspace load
    subroutine handle_backup_restoration(editor, buffer)
        use backup_module, only: backup_list, backup_info_t, backup_restore, backup_delete
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        type(backup_info_t), allocatable :: backups(:), unique_backups(:)
        integer :: backup_count, unique_count, i, j, status
        character :: choice
        character(len=32) :: key_input
        logical :: restore_success, found
        integer(int64) :: current_timestamp, best_timestamp

        ! Get list of backups
        call backup_list(editor%workspace_path, backups, backup_count)

        ! Deduplicate - keep only the most recent backup for each unique file
        allocate(unique_backups(backup_count))
        unique_count = 0

        do i = 1, backup_count
            if (len_trim(backups(i)%original_file) == 0) cycle

            ! Skip [Untitled] backups - they're in-memory only
            if (index(backups(i)%original_file, '[Untitled') == 1) then
                call backup_delete(backups(i)%backup_file)
                cycle
            end if

            ! Check if we already have a backup for this file
            found = .false.
            do j = 1, unique_count
                if (trim(unique_backups(j)%original_file) == trim(backups(i)%original_file)) then
                    ! Found duplicate - keep the one with newer timestamp
                    found = .true.
                    read(backups(i)%timestamp, *, iostat=status) current_timestamp
                    read(unique_backups(j)%timestamp, *, iostat=status) best_timestamp
                    if (status == 0 .and. current_timestamp > best_timestamp) then
                        ! This backup is newer - replace it and delete old one
                        call backup_delete(unique_backups(j)%backup_file)
                        unique_backups(j) = backups(i)
                    else
                        ! Keep existing, delete this duplicate
                        call backup_delete(backups(i)%backup_file)
                    end if
                    exit
                end if
            end do

            ! If not found, add to unique list
            if (.not. found) then
                unique_count = unique_count + 1
                unique_backups(unique_count) = backups(i)
            end if
        end do

        ! Prompt for each unique backup
        i = 1
        do while (i <= unique_count)
            ! Show restore prompt with progress
            choice = backup_prompt_restore(unique_backups(i)%original_file, i, unique_count, &
                                           unique_backups(i)%timestamp)

            if (choice == 'r') then
                ! Restore the backup
                call backup_restore(unique_backups(i)%backup_file, &
                                   unique_backups(i)%original_file, restore_success)

                if (restore_success) then
                    ! After successful restore, open/reload this file in a tab
                    call handle_restored_file(editor, buffer, unique_backups(i)%original_file)
                end if

                ! Show confirmation
                call terminal_clear_screen()
                call terminal_move_cursor(1, 1)
                if (restore_success) then
                    call terminal_write('Restored: ' // trim(unique_backups(i)%original_file))
                else
                    call terminal_write('Failed to restore: ' // trim(unique_backups(i)%original_file))
                end if
                call terminal_move_cursor(3, 1)
                call terminal_write('Press any key to continue...')
                call get_key_input(key_input, status)

                i = i + 1  ! Move to next

            else if (choice == 'd') then
                ! Delete backup - keep current file
                call backup_delete(unique_backups(i)%backup_file)
                i = i + 1  ! Move to next

            else if (choice == 'c') then
                ! Compare - show diff (then loop back to prompt again)
                call show_backup_diff(unique_backups(i)%backup_file, &
                                     unique_backups(i)%original_file)
                ! Don't increment i - re-prompt for same file
            end if
        end do

        ! Clear screen after all prompts
        call terminal_clear_screen()
    end subroutine handle_backup_restoration

    !> Show diff between backup and current file
    subroutine show_backup_diff(backup_file, original_file)
        character(len=*), intent(in) :: backup_file, original_file
        character(len=512) :: cmd
        character(len=32) :: key_input
        integer :: status

        ! backup_file already contains full path, use it directly
        ! Clear screen and show diff
        call terminal_clear_screen()
        call terminal_move_cursor(1, 1)
        call terminal_write('Diff: ' // trim(original_file) // ' vs backup')
        call terminal_move_cursor(2, 1)
        call terminal_write('=' // repeat('=', 70))

        ! Shell out to diff command
        write(cmd, '(A,A,A,A,A)') "diff -u '", trim(backup_file), "' '", trim(original_file), "'"
        call execute_command_line(trim(cmd), wait=.true.)

        ! Wait for user
        call terminal_move_cursor(24, 1)
        call terminal_write('Press any key to continue...')
        call get_key_input(key_input, status)
    end subroutine show_backup_diff

end program facsimile