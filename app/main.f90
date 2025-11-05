program facsimile
    use iso_fortran_env, only: error_unit, input_unit, output_unit
    use version_module
    use terminal_io_module
    use input_handler_module, only: get_key_input
    use editor_state_module
    use text_buffer_module
    use renderer_module
    use command_handler_module
    use workspace_module
    implicit none

    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    character(len=32) :: key_input
    character(len=256) :: filename, arg, workspace_dir
    logical :: running, should_quit, is_workspace_mode, workspace_success
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
        call execute_command_line("test -d '" // trim(arg) // "'", exitstat=status)
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
        ! No arguments - TODO Phase 5: launch Fortress welcome menu
        ! For now, just exit with usage message
        write(output_unit, '(A)') 'Usage: fac [file|directory]'
        write(output_unit, '(A)') '  fac file.txt     - Edit a single file'
        write(output_unit, '(A)') '  fac .            - Open workspace in current directory'
        write(output_unit, '(A)') '  fac /path/to/dir - Open workspace at path'
        stop
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
    else
        ! Single-file mode - use current directory
        call get_workspace_path(editor%workspace_path)
    end if

    ! Initialize terminal
    call terminal_init()
    call terminal_clear_screen()

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

    ! Don't auto-save on quit - user must explicitly save with Ctrl+S
    ! In the future, we could prompt if there are unsaved changes

    ! Cleanup
    call cleanup_renderer()
    call cleanup_command_handler()
    call terminal_cleanup()
    call cleanup_editor(editor)
    call cleanup_buffer(buffer)

contains

    subroutine get_workspace_path(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=1024) :: buffer
        integer :: status

        ! Use execute_command_line to get current directory
        call execute_command_line('pwd > /tmp/fac_pwd.txt', exitstat=status)
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

end program facsimile