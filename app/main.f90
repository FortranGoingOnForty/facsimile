! LSP callbacks live at module scope: passing an internal procedure of
! the main program as a callback makes gfortran emit a stack trampoline,
! which requires an executable stack (ld warns about it on -O0 builds).
module main_lsp_callbacks
    use editor_state_module, only: editor_state_t
    use editor_state_module, only: active_pane_of
    use command_handler_module, only: g_lsp_ui_changed
    implicit none
    private
    public :: bind_diagnostics_editor, handle_diagnostics

    type(editor_state_t), pointer :: cb_editor => null()

contains

    subroutine bind_diagnostics_editor(ed)
        type(editor_state_t), intent(inout), target :: ed
        cb_editor => ed
    end subroutine bind_diagnostics_editor

    ! Handler for LSP diagnostics notifications (with server attribution)
    subroutine handle_diagnostics(notification, server_index)
        use lsp_protocol_module, only: lsp_message_t
        use diagnostics_module, only: parse_diagnostics_from_params_with_server
        type(lsp_message_t), intent(in) :: notification
        integer, intent(in) :: server_index

        if (.not. associated(cb_editor)) return
        ! Parse and store diagnostics with server attribution so
        ! diagnostics from different servers stay separate (multi-LSP)
        call parse_diagnostics_from_params_with_server(cb_editor%diagnostics, &
            notification%params, server_index)

        ! And say so, or the screen keeps showing the last set. Diagnostics
        ! arrive on their own schedule, with no keystroke behind them, so
        ! nothing else in the loop knows the frame is now wrong. Fix the
        ! error under the caret and the message sat there until something
        ! unrelated forced a redraw -- which read as "the server has not
        ! noticed my edit" when the server had noticed and said so already.
        g_lsp_ui_changed = .true.
    end subroutine handle_diagnostics

end module main_lsp_callbacks

program facsimile
    use iso_fortran_env, only: error_unit, input_unit, output_unit, int64
    use main_lsp_callbacks, only: bind_diagnostics_editor, handle_diagnostics
    use version_module
    use terminal_io_module
    use input_handler_module, only: get_key_input
    use editor_state_module
    use editor_state_module, only: save_tab_pane
    use renderer_module, only: tab_group_preview_visible, set_group_preview_enabled
    use group_picker_module, only: is_group_picker_visible
    use settings_module, only: settings_get_logical, settings_get_integer
    use theme_module, only: theme_init
    use text_buffer_module
    use renderer_module
    use ai_engine_module, only: ai_tick, ai_configure
    use command_handler_module, only: session_requests_tick, tab_drag_tick
    use command_handler_module, only: backup_tick, backup_configure
    use command_handler_module, only: handle_key_command, init_command_handler, cleanup_command_handler, &
                                      save_initial_state_for_undo, search_pattern, match_case_sensitive, &
                                      g_lsp_modified_buffer, g_lsp_ui_changed, g_cursor_only_move, &
                                      tab_jump_tick, g_no_visible_change
    use workspace_module
    use backup_module
    use save_prompt_module
    use command_palette_module, only: register_command
    use terminal_panel_module, only: is_terminal_panel_visible, &
        terminal_panel_poll, terminal_panel_resize, &
        terminal_panel_set_default_permille, terminal_panel_is_alive
    use iso_c_binding, only: c_int
    use welcome_menu_module, only: show_welcome_menu
    use fortress_navigator_module, only: open_fortress_navigator
    use binary_prompt_module, only: binary_file_prompt
    ! Read by caret_move_only: everything that would make a partial repaint wrong
    use ghost_text_module, only: ghost_is_active
    use completion_popup_module, only: is_completion_visible
    use context_menu_module, only: is_context_menu_visible
    use hover_tooltip_module, only: is_hover_visible
    use diagnostics_panel_module, only: is_diagnostics_panel_visible
    use code_actions_panel_module, only: is_code_actions_panel_visible
    use references_panel_module, only: is_references_panel_visible
    use symbols_panel_module, only: is_symbols_panel_visible
    use lsp_server_installer_panel_module, only: is_lsp_server_installer_panel_visible
    use lsp_server_manager_module, only: take_lsp_error, notify_file_opened, notify_file_changed, &
                                         notify_file_closed, process_server_messages, &
                                         set_diagnostics_handler, set_lsp_workspace_root
    use lsp_protocol_module, only: lsp_message_t
    use app_state_module, only: is_first_run, mark_first_run_complete
    use session_ipc_module, only: session_ipc_begin, session_ipc_end, &
                                  session_ipc_send, session_ipc_take
    use lsp_server_installer_panel_module, only: show_lsp_server_installer_panel
    implicit none

    interface
        subroutine c_usleep(usec) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value, intent(in) :: usec
        end subroutine
    end interface

    type(editor_state_t), target :: editor
    type(buffer_t) :: buffer
    character(len=32) :: key_input
    character(len=512) :: filename, arg, workspace_dir, lsp_workspace
    logical :: running, should_quit, quit_confirmed
    logical :: is_workspace_mode, workspace_success
    logical :: welcome_cancelled, is_browse, nav_cancelled, is_directory
    logical :: arg_is_directory, forwarded
    character(len=512) :: session_dir
    integer :: session_len
    logical :: explicit_lsp_workspace
    character(len=:), allocatable :: selected_path
    integer :: status, argc, rows, cols, i
    integer :: prev_active_tab, prev_active_pane
    character(len=4096) :: prev_active_name
    ! Snapshot for the caret-move fast path
    integer :: prev_cursor_line, prev_viewport_line, prev_viewport_col
    logical :: prev_ghost_visible, batch_caret_only, batch_no_change
    integer :: coalesced_keys
    logical :: active_view_changed
    logical :: opened_existing_tab
    logical :: tab_created
    integer :: save_pane, other_pane, other_status


    ! Get command line arguments
    argc = command_argument_count()
    is_workspace_mode = .false.
    arg_is_directory = .false.
    workspace_dir = ""
    filename = ""
    lsp_workspace = ""
    explicit_lsp_workspace = .false.

    ! First pass: look for -w/--workspace flag
    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)
        if (trim(arg) == '-w' .or. trim(arg) == '--workspace') then
            if (i < argc) then
                call get_command_argument(i + 1, lsp_workspace)
                explicit_lsp_workspace = .true.
                i = i + 2
            else
                write(error_unit, '(A)') 'Error: -w/--workspace requires a directory argument'
                stop 1
            end if
        else
            i = i + 1
        end if
    end do

    ! Second pass: handle other arguments
    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)

        ! Skip -w and its argument (already processed)
        if (trim(arg) == '-w' .or. trim(arg) == '--workspace') then
            i = i + 2
            cycle
        end if

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

        ! Expand ~ to $HOME
        if (len_trim(arg) >= 1 .and. arg(1:1) == '~') then
            block
                character(len=512) :: home_dir
                integer :: home_len
                call get_environment_variable('HOME', &
                    home_dir, home_len)
                if (home_len > 0) then
                    if (len_trim(arg) == 1) then
                        arg = home_dir(1:home_len)
                    else
                        arg = home_dir(1:home_len) // &
                            arg(2:len_trim(arg))
                    end if
                end if
            end block
        end if

        ! This must be the file/directory argument
        ! Check if argument is a directory (workspace mode)
        ! Use test -d which is POSIX compliant (works on Linux, macOS, BSD)
        call execute_command_line("test -d '" // trim(arg) // &
            "' && echo 'Directory' > /tmp/.fac_filetype || " // &
            "echo 'File' > /tmp/.fac_filetype", wait=.true.)
        call read_file_type(status)
        if (status == 0) then
            ! Directory - workspace mode
            arg_is_directory = .true.
            is_workspace_mode = .true.
            call workspace_get_path(trim(arg), workspace_dir)
        else
            ! File argument: resolve to an absolute path so the
            ! workspace is deduced from the file's location, not
            ! the shell's current directory.
            block
                character(len=512) :: dir_part, abs_dir, base_part
                integer :: slash
                slash = index(trim(arg), '/', back=.true.)
                if (slash > 0) then
                    dir_part = arg(1:slash-1)
                    if (len_trim(dir_part) == 0) dir_part = '/'
                    base_part = arg(slash+1:len_trim(arg))
                else
                    dir_part = '.'
                    base_part = trim(arg)
                end if
                call workspace_get_path(trim(dir_part), abs_dir)
                if (len_trim(abs_dir) == 0) abs_dir = trim(dir_part)
                filename = trim(abs_dir) // '/' // trim(base_part)
            end block

            ! Detect an enclosing workspace by walking parents
            workspace_dir = workspace_detect_from_file(trim(filename))
            if (len_trim(workspace_dir) > 0) then
                is_workspace_mode = .true.
            end if
        end if
        i = i + 1
    end do

    ! Started from inside another fac's terminal panel? Hand the argument to
    ! that editor and exit, instead of nesting a whole second editor inside a
    ! pane of the first. This is `code foo.c` behaviour.
    !
    ! Read BEFORE we ever advertise a spool of our own, which is what makes it
    ! impossible to mistake ourselves for a client: at this point FAC_SESSION
    ! can only have come from a parent.
    !
    ! An explicit -w is a deliberate request for a separate workspace, so it
    ! opts out. So does a bare `fac`, which has no argument to forward.
    if (.not. explicit_lsp_workspace .and. &
        (len_trim(filename) > 0 .or. arg_is_directory)) then
        call get_environment_variable('FAC_SESSION', session_dir, session_len)
        if (session_len > 0) then
            if (arg_is_directory) then
                call session_ipc_send(session_dir(1:session_len), 'dir', &
                                      trim(workspace_dir), forwarded)
            else
                call session_ipc_send(session_dir(1:session_len), 'file', &
                                      trim(filename), forwarded)
            end if
            if (forwarded) then
                ! Say which, because the editor that opens it may be scrolled
                ! away from the tab bar and the terminal is what you are
                ! looking at.
                if (arg_is_directory) then
                    write(output_unit, '(A,A)') 'Opening group in facsimile: ', &
                        trim(workspace_dir)
                else
                    write(output_unit, '(A,A)') 'Opening in facsimile: ', &
                        trim(filename)
                end if
                stop
            end if
            ! Could not reach it -- fall through and open normally rather
            ! than failing to open the file at all.
        end if
    end if

    ! Now that FAC_SESSION has been read, it is safe to advertise our own.
    block
        character(len=:), allocatable :: my_session
        call session_ipc_begin(my_session)
    end block

    if (argc == 0) then
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

                ! Handle the selection from navigator
                if (is_directory) then
                    ! Directory - open as workspace
                    is_workspace_mode = .true.
                    call workspace_get_path(trim(selected_path), workspace_dir)
                else
                    ! File - open in single-file mode (like `fac filename`)
                    filename = selected_path
                    ! Check if parent directory has a workspace
                    workspace_dir = workspace_detect_from_file(trim(filename))
                    if (len_trim(workspace_dir) > 0) then
                        is_workspace_mode = .true.
                    end if
                end if
            else
                ! User just cancelled
                stop
            end if
        end if

        ! User selected a workspace from welcome menu (not browse)
        ! This handles favorites, recents, and CURRENT DIRECTORY
        if (allocated(selected_path) .and. .not. is_browse) then
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
                ! Invalid selection (favorites/recents should only have directories)
                write(error_unit, '(A)') 'Error: Selected path is not a directory'
                stop 1
            end if
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

    ! Resolve terminal capabilities and the selected theme before any editor
    ! surface is painted. Settings load lazily inside theme_init.
    call theme_init()

    ! Initialize editor
    call init_editor(editor)

    ! Read AI settings once at startup. Opt-in: with ai.enabled false (the
    ! default) this resolves nothing, connects to nothing, and the tick above
    ! returns immediately.
    ! Motion tracking for the tab-group preview is a standing cost once any
    ! group exists -- an event per pixel of pointer travel for the session --
    ! so it is switchable for slow links. Turning it off loses only the
    ! preview; the pinned member row still works.
    call set_group_preview_enabled( &
        settings_get_logical('tabs.group_hover_preview', .true.))
    ! The terminal panel's starting height, as a percentage of the screen. Only
    ! a starting point -- resizing it stores the new ratio per workspace, which
    ! then wins over this.
    call terminal_panel_set_default_permille(editor%terminal_panel, &
        10 * settings_get_integer('terminal.height_percent', 30))
    call ai_configure(editor%ai)

    ! Autosave settings, read once. Opt-in: with backup.autosave.enabled
    ! false (the default) the tick below does one logical test and returns.
    call backup_configure()
    running = .true.

    ! Set LSP workspace root if explicit -w flag was provided
    if (explicit_lsp_workspace) then
        call set_lsp_workspace_root(editor%lsp_manager, trim(lsp_workspace))
    end if

    ! Set up diagnostics handler for LSP
    call bind_diagnostics_editor(editor)
    call set_diagnostics_handler(editor%lsp_manager, handle_diagnostics)

    ! Register all commands for command palette
    call register_all_commands()

    ! Initialize terminal early (needed for workspace restoration warnings)
    call terminal_init()
    call terminal_clear_screen()

    ! Initialize main buffer early (needed for workspace restoration)
    call init_buffer(buffer)

    ! Set workspace path
    if (is_workspace_mode) then
        ! Use detected/created workspace directory
        allocate(character(len=len_trim(workspace_dir)) :: editor%workspace_path)
        editor%workspace_path = trim(workspace_dir)

        ! Restore workspace state (tabs, cursor positions, etc.)
        call workspace_restore_state(editor, editor%workspace_path, workspace_success)

        ! Sync restored active tab's buffer to main buffer
        if (workspace_success .and. allocated(editor%tabs) .and. editor%active_tab_index > 0) then
            if (editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                    size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                    ! Copy active pane's buffer to main buffer (replaces the empty init)
                    call copy_buffer(buffer, editor%tabs(editor%active_tab_index)%panes(1)%buffer)
                end if
            end if
        end if
    else
        ! Single-file mode with no enclosing workspace: root the
        ! file tree on the file's own directory (display only; not
        ! persisted), NOT the shell's current directory.
        if (len_trim(filename) > 0) then
            block
                character(len=512) :: dir_part
                integer :: slash
                slash = index(trim(filename), '/', back=.true.)
                if (slash > 0) then
                    dir_part = filename(1:slash-1)
                    if (len_trim(dir_part) == 0) dir_part = '/'
                else
                    dir_part = '.'
                end if
                editor%workspace_path = trim(dir_part)
            end block
        else
            ! No file argument at all - fall back to CWD
            call get_workspace_path(editor%workspace_path)
        end if
    end if

    ! Migrate legacy per-workspace backups to global registry
    if (allocated(editor%workspace_path)) then
        call backup_migrate_legacy(editor%workspace_path)
    end if

    ! Get terminal size
    call terminal_get_size(rows, cols)
    editor%screen_rows = rows
    editor%screen_cols = cols

    ! Initialize renderer (pass filename for syntax highlighting detection)
    if (len_trim(filename) > 0) then
        call init_renderer(rows, cols, trim(filename))
    else
        call init_renderer(rows, cols)
    end if

    ! Initialize command handler (for yank stack)
    call init_command_handler()

    ! Initialize buffer and load file if specified
    if (len_trim(filename) > 0) then
        ! If workspace restore already opened this file, reuse that tab.
        ! A duplicate tab would send a second didOpen for the same URI
        ! (LSP servers reject it) and add one more copy of the file to
        ! the saved session on every launch.
        opened_existing_tab = .false.
        block
            integer :: ti
            do ti = 1, size(editor%tabs)
                if (allocated(editor%tabs(ti)%filename)) then
                    if (editor%tabs(ti)%filename == trim(filename)) then
                        call switch_to_tab_with_buffer(editor, ti, buffer)
                        opened_existing_tab = .true.
                        exit
                    end if
                end if
            end do
        end block

        if (.not. opened_existing_tab) then
        ! Create a tab for the initial file
        call create_tab(editor, trim(filename), tab_created)

        ! Read the file once, into the pane that owns it. This used to load
        ! from disk twice -- once into the tab's shadow buffer and once into
        ! the pane's -- for every file, on every open path.
        if (editor%active_tab_index > 0 .and. tab_created) then
            block
                integer :: p0
                p0 = active_pane_of(editor, editor%active_tab_index)
                call buffer_load_file( &
                    editor%tabs(editor%active_tab_index)%panes(p0)%buffer, &
                    trim(filename), status)
                call copy_buffer(buffer, &
                    editor%tabs(editor%active_tab_index)%panes(p0)%buffer)
            end block
        else
            call buffer_load_file(buffer, trim(filename), status)
        end if

        if (status == 0) then
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)

            ! Send LSP didOpen notification to ALL active servers
            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                if (editor%tabs(editor%active_tab_index)%num_lsp_servers > 0) then
                    block
                        integer :: srv_i
                        do srv_i = 1, editor%tabs(editor%active_tab_index)%num_lsp_servers
                            call notify_file_opened(editor%lsp_manager, &
                                editor%tabs(editor%active_tab_index)%lsp_server_indices(srv_i), &
                                trim(filename), buffer_to_string(buffer))
                        end do
                    end block
                end if
            end if
        else if (status == -2) then
            ! Binary file detected - prompt user
            if (binary_file_prompt(trim(filename))) then
                ! User wants to view in hex mode
                call buffer_load_file_as_hex(buffer, trim(filename), status)
                if (status == 0) then
                    ! Deallocate if already allocated
                    if (allocated(editor%filename)) deallocate(editor%filename)
                    allocate(character(len=len_trim(filename) + 6) :: editor%filename)
                    editor%filename = trim(filename) // ' [HEX]'

                    ! Copy hex buffer to tab and pane buffers
                    if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                            editor%active_tab_index))%buffer, buffer)
                        if (allocated(editor%tabs(editor%active_tab_index)%panes) .and. &
                            size(editor%tabs(editor%active_tab_index)%panes) > 0) then
                            call copy_buffer(editor%tabs(editor%active_tab_index)%panes(1)%buffer, buffer)
                        end if
                    end if
                    ! TODO: Mark as read-only in future enhancement
                else
                    ! Failed to load hex view
                    call terminal_cleanup()
                    write(error_unit, '(A)') 'Error: Failed to load binary file'
                    stop 1
                end if
            else
                ! User cancelled - cleanup and exit
                call terminal_cleanup()
                stop
            end if
        else
            ! If file doesn't exist, create empty buffer for new file
            call init_buffer(buffer)
            if (allocated(editor%filename)) deallocate(editor%filename)
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)

            ! didOpen for the empty document: without it the server
            ! ignores every didChange, so brand-new files never get
            ! diagnostics or completion.
            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                if (editor%tabs(editor%active_tab_index)%num_lsp_servers > 0) then
                    block
                        integer :: srv_i
                        do srv_i = 1, editor%tabs(editor%active_tab_index)%num_lsp_servers
                            call notify_file_opened(editor%lsp_manager, &
                                editor%tabs(editor%active_tab_index)%lsp_server_indices(srv_i), &
                                trim(filename), buffer_to_string(buffer))
                        end do
                    end block
                end if
            end if
        end if
        end if  ! .not. opened_existing_tab
    else
        ! Only initialize empty buffer if we don't have restored tabs
        if (.not. (allocated(editor%tabs) .and. editor%active_tab_index > 0)) then
            call init_buffer(buffer)
        end if
    end if

    ! Check global registry for backups (after file loading so
    ! handle_restored_file can find existing tabs). Scoping rule:
    !   - opening a specific file -> only that file's backup
    !   - opening a workspace      -> backups under the workspace
    ! The single-file workspace path (e.g. $HOME) is only a
    ! file-tree root and must NOT scope backups, or it would
    ! over-match every backup beneath it.
    block
        if (len_trim(filename) > 0) then
            if (backup_detect('', trim(filename))) then
                call handle_backup_restoration( &
                    editor, buffer, trim(filename))
            end if
        else if (is_workspace_mode .and. &
                 allocated(editor%workspace_path)) then
            if (backup_detect(trim(editor%workspace_path))) then
                call handle_backup_restoration( &
                    editor, buffer)
            end if
        end if
    end block

    ! Save initial file state for undo (position 0)
    call save_initial_state_for_undo()

    ! First-run experience: show LSP server installer panel
    if (is_first_run()) then
        call show_lsp_server_installer_panel(editor%lsp_installer_panel)
        call mark_first_run_complete()
    end if

    ! Initial render
    call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)

    ! Outer loop: allows cancel-quit to resume editing
    quit_confirmed = .false.
    do while (.not. quit_confirmed)

    ! Main event loop
    do while (running)
        ! Detect terminal resize and reflow. The size is otherwise only read
        ! at startup, so resizing would leave stale dimensions — e.g. split
        ! panes cramped into a sub-region with the rest of the screen unused.
        call terminal_get_size(rows, cols)
        if (rows > 0 .and. cols > 0 .and. &
            (rows /= editor%screen_rows .or. cols /= editor%screen_cols)) then
            editor%screen_rows = rows
            editor%screen_cols = cols
            ! Propagate the new size to every consumer that caches it: the
            ! renderer's screen buffer and the integrated terminal's PTY/grid
            ! (the shell must receive SIGWINCH or it keeps wrapping at the
            ! old width).
            call resize_renderer(rows, cols)
            call terminal_panel_resize(editor%terminal_panel, rows, cols)
            call update_viewport(editor)
            if (editor%fuss_mode_active) then
                call render_screen_with_tree(buffer, editor, &
                    allocated(search_pattern), match_case_sensitive)
            else
                call render_screen(buffer, editor, &
                    allocated(search_pattern), match_case_sensitive)
            end if
        end if

        ! Process any LSP messages
        call process_server_messages(editor%lsp_manager)

        ! Anything the LSP layer wanted to say. It cannot say it itself: it
        ! is compiled before the renderer, and printing would put the text on
        ! the terminal the editor is drawing on -- which is how a wedged
        ! server once scribbled over a document.
        block
            character(len=:), allocatable :: lsp_msg
            if (take_lsp_error(lsp_msg)) then
                call set_status_message(lsp_msg)
                g_lsp_ui_changed = .true.
            end if
        end block

        ! Model-backed completion: decide whether to send, and advance
        ! anything already in flight. One state per call, never blocking.
        ! Inert until ai.enabled is turned on.
        call ai_tick(editor%ai, editor, buffer, g_lsp_ui_changed)

        ! Expire a pending multi-digit tab jump. The key read times out every
        ! 50ms, so this runs while nothing is being typed -- which is the only
        ! way the status hint gets cleared when the user simply stops.
        call tab_jump_tick(g_lsp_ui_changed)

        ! A tab resting on a group opens its member row. Here rather than on
        ! motion, because a pointer that has stopped sends nothing.
        call tab_drag_tick(g_lsp_ui_changed)

        ! Crash backups for buffers with unsaved changes. Inert until
        ! backup.autosave.enabled is turned on, and even then it writes only
        ! when a buffer has actually changed since its last backup.
        call backup_tick(editor, g_lsp_ui_changed)

        ! Anything `fac` in the terminal handed us. Gated on the panel being
        ! alive because that shell is the only thing that can produce a
        ! request -- a session that has never opened a terminal never looks.
        if (terminal_panel_is_alive(editor%terminal_panel)) then
            call session_requests_tick(editor, buffer, g_lsp_ui_changed)
        end if

        ! Poll integrated terminal for new output
        if (is_terminal_panel_visible(editor%terminal_panel)) then
            call terminal_panel_poll(editor%terminal_panel)
            ! Re-render immediately if terminal has new output
            if (editor%terminal_panel%has_new_output) then
                if (editor%fuss_mode_active) then
                    call render_screen_with_tree(buffer, editor, &
                        allocated(search_pattern), &
                        match_case_sensitive)
                else
                    call render_screen(buffer, editor, &
                        allocated(search_pattern), &
                        match_case_sensitive)
                end if
            end if
        end if

        ! Sync local buffer from tab after LSP processing (in case LSP modified it)
        block
            logical :: should_render
            should_render = .false.

            ! Check if LSP set the UI changed flag (e.g., code actions panel shown)
            if (g_lsp_ui_changed) then
                should_render = .true.
                g_lsp_ui_changed = .false.
            end if

            if (editor%active_tab_index > 0 .and. editor%active_tab_index <= size(editor%tabs)) then
                ! Check if LSP set the modified flag
                if (g_lsp_modified_buffer) then
                    should_render = .true.
                    g_lsp_modified_buffer = .false.

                    ! Only sync buffers when LSP actually changed them
                    call copy_buffer(buffer, &
                        editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, editor%active_tab_index))%buffer)

                    if (allocated(editor%tabs( &
                        editor%active_tab_index)%panes) .and. &
                        size(editor%tabs( &
                        editor%active_tab_index)%panes) > 0) then
                        status = editor%tabs( &
                            editor%active_tab_index &
                            )%active_pane_index
                        if (status > 0 .and. status <= size( &
                            editor%tabs( &
                            editor%active_tab_index)%panes)) then
                            call copy_buffer(editor%tabs( &
                                editor%active_tab_index)%panes( &
                                status)%buffer, buffer)
                        end if
                    end if
                end if
            end if

            ! Render immediately if LSP modified the buffer (do this OUTSIDE the if block)
            if (should_render) then
                if (editor%fuss_mode_active) then
                    call render_screen_with_tree(buffer, editor, allocated(search_pattern), match_case_sensitive)
                else
                    call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)
                end if
            end if
        end block

        ! Flush any pending document changes to LSP
        call flush_pending_document_changes(editor)

        ! Get input
        call get_key_input(key_input, status)

        if (status == 0) then
            ! Coalescing loop: when keystrokes are already buffered (fast
            ! typing, paste, or a consumer that fell behind), process the
            ! whole burst and render once at the end. One full-screen frame
            ! per keystroke is 3KB at 80x24 but 20KB+ at large terminals;
            ! emitting one per key floods slow terminals, which then display
            ! stale/torn frames with the caret detached from the text.
            coalesced_keys = 0

            ! Snapshot for the caret-move fast path, taken once for the whole
            ! burst. Taking it per key compared only the last keystroke, so a
            ! held arrow key whose earlier repeats scrolled the viewport ended
            ! up repainting two lines over a screen still showing the old
            ! scroll position -- lines appearing to move with the caret while
            ! the file stayed put.
            batch_caret_only = .true.
            ! Per BURST, not per key: a burst mixing a no-op motion with a real
            ! keystroke must still draw. Only a burst where every key changed
            ! nothing can skip the frame.
            batch_no_change = .true.
            prev_cursor_line = 0
            prev_viewport_line = editor%viewport_line
            prev_viewport_col = editor%viewport_column
            prev_ghost_visible = ghost_is_active(editor%ghost)
            if (allocated(editor%cursors)) then
                if (editor%active_cursor >= 1 .and. &
                    editor%active_cursor <= size(editor%cursors)) then
                    prev_cursor_line = editor%cursors(editor%active_cursor)%line
                end if
            end if

            do
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

            ! Remember which view is active so we can detect whether the
            ! command switched panes/tabs. `buffer` holds the pre-command
            ! active pane's content; writing it back after a switch would
            ! clobber the newly focused pane with the old one's text.
            ! The indices alone are not enough: closing a pane renumbers the
            ! array, so closing the first of two leaves active_pane_index at 1
            ! while pane 1 is now a different pane -- and panes in one tab can
            ! hold different files (open-in-split from the tree). The active
            ! pane's filename is what actually identifies the view.
            prev_active_tab = editor%active_tab_index
            prev_active_pane = 0
            prev_active_name = ''
            if (prev_active_tab > 0 .and. prev_active_tab <= size(editor%tabs)) then
                if (allocated(editor%tabs(prev_active_tab)%panes)) then
                    prev_active_pane = editor%tabs(prev_active_tab)%active_pane_index
                    if (prev_active_pane >= 1 .and. &
                        prev_active_pane <= size(editor%tabs(prev_active_tab)%panes)) then
                        if (allocated(editor%tabs(prev_active_tab)% &
                                      panes(prev_active_pane)%filename)) then
                            prev_active_name = editor%tabs(prev_active_tab)% &
                                               panes(prev_active_pane)%filename
                        end if
                    end if
                end if
            end if

            ! Process input
            call handle_key_command(key_input, editor, buffer, should_quit)

            ! The whole burst has to be caret-only, not just its last key.
            ! One edit anywhere in the batch means the text changed and only
            ! a full frame can show it.
            batch_caret_only = batch_caret_only .and. g_cursor_only_move
            batch_no_change = batch_no_change .and. g_no_visible_change

            ! Did the command move focus to a different tab or pane?
            active_view_changed = (editor%active_tab_index /= prev_active_tab)
            if (.not. active_view_changed .and. editor%active_tab_index > 0 .and. &
                editor%active_tab_index <= size(editor%tabs)) then
                if (allocated(editor%tabs(editor%active_tab_index)%panes)) then
                    status = editor%tabs(editor%active_tab_index)%active_pane_index
                    active_view_changed = (status /= prev_active_pane)
                    if (.not. active_view_changed .and. status >= 1 .and. &
                        status <= size(editor%tabs(editor%active_tab_index)%panes)) then
                        if (allocated(editor%tabs(editor%active_tab_index)% &
                                      panes(status)%filename)) then
                            active_view_changed = &
                                (editor%tabs(editor%active_tab_index)%panes(status)%filename &
                                 /= prev_active_name)
                        else
                            active_view_changed = (len_trim(prev_active_name) > 0)
                        end if
                    end if
                end if
            end if

            ! Sync back to active pane and other instances. Skip for cursor-only
            ! moves (buffer unchanged) and for focus switches (a switch does not
            ! edit; the target pane already holds its own content).
            if (.not. g_cursor_only_move .and. .not. active_view_changed) then
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
                        call copy_buffer(editor%tabs(editor%active_tab_index)%panes(active_pane_of(editor, &
                            editor%active_tab_index))%buffer, buffer)

                        ! Sync modified flag from buffer to tab
                        editor%tabs(editor%active_tab_index)%modified = buffer%modified
                    end if
                end if
            end if

            ! Continue the burst only while more input is already waiting.
            ! Capped so an endless input stream still renders periodically.
            if (should_quit) exit
            if (coalesced_keys >= 64) exit
            if (.not. terminal_input_available()) exit
            call get_key_input(key_input, status)
            if (status /= 0) exit
            coalesced_keys = coalesced_keys + 1
            end do

            if (should_quit) then
                running = .false.
            else
                ! Poll terminal for echo before rendering.
                if (is_terminal_panel_visible(editor%terminal_panel)) then
                    if (editor%terminal_panel%focused) then
                        block
                            integer :: poll_pass
                            do poll_pass = 1, 3
                                call c_usleep(1000)
                                call terminal_panel_poll( &
                                    editor%terminal_panel)
                                if (editor%terminal_panel &
                                    %has_new_output) exit
                            end do
                        end block
                    else
                        call terminal_panel_poll( &
                            editor%terminal_panel)
                    end if
                end if

                ! Re-render screen after each command.
                !
                ! A plain caret move repaints only the handful of lines whose
                ! appearance actually changed. A full frame is ~3.3 KB, which
                ! over ssh is what makes a held arrow key feel like wading.
                ! The guard is deliberately strict: anything that could put
                ! something else on screen falls back to the full path, so
                ! the fast path never has to be right about more than the
                ! caret.
                ! Nothing on screen differs, so draw nothing. Pointer motion is
                ! reported per CELL of travel once any-motion tracking is on,
                ! and each of those events was repainting the whole frame --
                ! more than a real caret move costs, for a screen that had not
                ! changed. Crossing the width of a terminal was a hundred full
                ! frames.
                !
                ! Except when the shell has just printed something: the panel is
                ! polled a few lines above, and its output is a change this
                ! burst's keys knew nothing about.
                if (batch_no_change .and. &
                    .not. editor%terminal_panel%has_new_output) then
                    continue
                else if (batch_caret_only .and. &
                    caret_move_only(editor, prev_cursor_line, prev_viewport_line, &
                                    prev_viewport_col, prev_ghost_visible)) then
                    call render_caret_move(buffer, editor, prev_cursor_line, &
                                           allocated(search_pattern), match_case_sensitive)
                else if (editor%fuss_mode_active) then
                    call render_screen_with_tree(buffer, editor, allocated(search_pattern), match_case_sensitive)
                else
                    call render_screen(buffer, editor, allocated(search_pattern), match_case_sensitive)
                end if
            end if
        end if
    end do

    ! Handle unsaved files - unified for workspace and single-file
    if (allocated(editor%tabs) .and. size(editor%tabs) > 0) then
        call handle_unsaved_files_on_quit(editor, buffer, &
            should_quit)
    end if

    ! Save workspace state AFTER unsaved-files prompt — only for
    ! real workspaces, so single-file opens never write a
    ! .fac/workspace.json into the file's directory.
    if (should_quit .and. is_workspace_mode .and. &
        allocated(editor%workspace_path)) then
        call workspace_save_state(editor, editor%workspace_path, workspace_success)
    end if

    if (should_quit) then
        quit_confirmed = .true.
    else
        ! User cancelled quit - resume editing
        running = .true.
        should_quit = .false.
        call terminal_clear_screen()
        if (editor%fuss_mode_active) then
            call render_screen_with_tree(buffer, editor, &
                allocated(search_pattern), match_case_sensitive)
        else
            call render_screen(buffer, editor, &
                allocated(search_pattern), match_case_sensitive)
        end if
    end if

    end do  ! outer quit_confirmed loop

    ! Cleanup
    call cleanup_renderer()
    call cleanup_command_handler()
    call session_ipc_end()
    call terminal_cleanup()
    call cleanup_editor(editor)
    call cleanup_buffer(buffer)

contains

    ! Flush pending document changes for all tabs
    subroutine flush_pending_document_changes(editor)
        use document_sync_module, only: flush_pending_changes
        type(editor_state_t), intent(inout) :: editor
        integer :: i

        ! Check all tabs for pending changes
        if (allocated(editor%tabs)) then
            do i = 1, size(editor%tabs)
                if (editor%tabs(i)%num_lsp_servers > 0) then
                    call flush_pending_changes(editor%tabs(i)%document_sync, &
                                              editor%lsp_manager, .false.)
                end if
            end do
        end if
    end subroutine flush_pending_document_changes

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
        write(output_unit, '(A)') 'fac - a terminal text editor written in Fortran'
        write(output_unit, '(A,A)') 'Version: ', VERSION
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Usage:'
        write(output_unit, '(A)') '  fac                    Welcome menu: recent workspaces and files'
        write(output_unit, '(A)') '  fac <file>             Open a file'
        write(output_unit, '(A)') '  fac <dir>              Open the directory as a workspace'
        write(output_unit, '(A)') '  fac -w <dir> [file]    Open <file> with <dir> as the workspace root'
        write(output_unit, '(A)') '  fac -v, --version      Print the version'
        write(output_unit, '(A)') '  fac -h, --help         This message'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'A few keys to start with:'
        write(output_unit, '(A)') '  ctrl-s  save              ctrl-q  quit'
        write(output_unit, '(A)') '  ctrl-f  find              ctrl-g  go to line'
        write(output_unit, '(A)') '  ctrl-b  file tree         ctrl-o  file browser'
        write(output_unit, '(A)') '  ctrl-p  command palette   f1      every keybinding'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Keys follow VSCode wherever a terminal can express them. F1 lists all of'
        write(output_unit, '(A)') 'them; docs/KEYBINDINGS.md has the same list with the reasoning.'
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
                        ! The working buffer holds the ACTIVE pane's text, so
                        ! write it under that pane's name -- the tab's name may
                        ! belong to a different file when a tab holds two.
                        save_pane = editor%tabs(i)%active_pane_index
                        if (allocated(editor%tabs(i)%panes) .and. &
                            save_pane >= 1 .and. save_pane <= size(editor%tabs(i)%panes)) then
                            if (allocated(editor%tabs(i)%panes(save_pane)%filename)) then
                                call buffer_save_file(buffer, &
                                    editor%tabs(i)%panes(save_pane)%filename, save_status)
                            else
                                call buffer_save_file(buffer, editor%tabs(i)%filename, save_status)
                            end if
                        else
                            call buffer_save_file(buffer, editor%tabs(i)%filename, save_status)
                        end if

                        ! And the other panes, each to its own file. Quitting
                        ! used to write only the active one, so a dirty split
                        ! on a second file was silently discarded.
                        if (allocated(editor%tabs(i)%panes)) then
                            do other_pane = 1, size(editor%tabs(i)%panes)
                                if (other_pane == save_pane) cycle
                                call save_tab_pane(editor, i, other_pane, other_status)
                            end do
                        end if

                        if (save_status == 0) then
                            buffer%modified = .false.
                            editor%tabs(i)%modified = .false.
                        end if
                    end if

                else if (prompt_result%action == 'd') then
                    ! User wants to discard - backup buffer
                    if (index(editor%tabs(i)%filename, &
                        '[Untitled') /= 1) then
                        block
                            use text_buffer_module, only: &
                                buffer_to_string
                            character(len=:), allocatable :: &
                                buf_str
                            integer :: pane_i
                            pane_i = editor%tabs(i) &
                                %active_pane_index
                            if (allocated(editor%tabs(i)%panes) &
                                .and. pane_i > 0 .and. pane_i &
                                <= size(editor%tabs(i)%panes)) &
                                then
                                buf_str = buffer_to_string( &
                                    editor%tabs(i)%panes(pane_i) &
                                    %buffer)
                            else
                                buf_str = buffer_to_string( &
                                    editor%tabs(i)%panes(active_pane_of(editor, i))%buffer)
                            end if
                            call backup_create( &
                                editor%tabs(i)%filename, &
                                buf_str, backup_success)
                            if (allocated(buf_str)) &
                                deallocate(buf_str)
                        end block
                    end if
                    ! Clear modified so workspace state doesn't re-trigger backup on next launch
                    editor%tabs(i)%modified = .false.
                end if
            end if
        end do

        ! All handled - proceed with quit
        should_quit = .true.
    end subroutine handle_unsaved_files_on_quit

    ! handle_single_file_on_quit removed — unified into
    ! handle_unsaved_files_on_quit which iterates all tabs

    !> Handle a restored file - open in tab or reload existing tab
    subroutine handle_restored_file(editor, buffer, restored_file)
        use text_buffer_module, only: buffer_load_file, &
            copy_buffer
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
                            ! Sync to tab and pane buffers
                            call copy_buffer(editor%tabs(tab_idx)%panes(active_pane_of(editor, tab_idx))%buffer, buffer)
                            if (allocated(editor%tabs(tab_idx)%panes) .and. &
                                size(editor%tabs(tab_idx)%panes) > 0) then
                                call copy_buffer(editor%tabs(tab_idx)%panes(1)%buffer, buffer)
                            end if
                            buffer%modified = .false.
                            editor%tabs(tab_idx)%modified = .false.
                            editor%modified = .false.
                        end if
                        exit
                    end if
                end if
            end do
        end if

        ! If not found, create a new tab with this file
        if (.not. found) then
            call create_tab(editor, restored_file, tab_created)
            if (tab_created .and. allocated(editor%tabs) .and. editor%active_tab_index > 0) then
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
    subroutine handle_backup_restoration(editor, buffer, &
        file_path)
        use backup_module, only: backup_list, &
            backup_info_t, backup_restore, backup_delete
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(inout) :: buffer
        character(len=*), intent(in), optional :: file_path
        type(backup_info_t), allocatable :: &
            backups(:), unique_backups(:)
        integer :: backup_count, unique_count, i, j, status
        character :: choice
        character(len=32) :: key_input
        logical :: restore_success, found
        integer(int64) :: current_timestamp, best_timestamp

        ! Scope backups to the specific file when one is given,
        ! otherwise to the workspace prefix. Never list every
        ! backup in the registry.
        block
            if (present(file_path)) then
                call backup_list('', backups, &
                    backup_count, trim(file_path))
            else if (allocated(editor%workspace_path)) then
                call backup_list( &
                    trim(editor%workspace_path), &
                    backups, backup_count)
            else
                allocate(backups(0))
                backup_count = 0
            end if
        end block

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
                call terminal_flush()
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
        call terminal_flush()
        call get_key_input(key_input, status)
    end subroutine show_backup_diff

    ! Register all available commands for the command palette
    !> Is it safe to repaint only the lines a caret move touched?
    !>
    !> Every condition here is something that puts other pixels on screen, or
    !> moves the whole viewport. The cost of being wrong is a stale artifact
    !> the user has to redraw by hand, so this errs heavily toward the full
    !> path: it only says yes for a plain arrow key, in a single pane, with
    !> one cursor, nothing selected, nothing floating, and the viewport
    !> exactly where it was.
    function caret_move_only(editor, prev_line, prev_vp_line, prev_vp_col, &
                             prev_ghost) result(ok)
        type(editor_state_t), intent(in) :: editor
        integer, intent(in) :: prev_line, prev_vp_line, prev_vp_col
        logical, intent(in) :: prev_ghost
        logical :: ok
        integer :: tab_idx

        ok = .false.

        ! Only the four plain arrow cases set this
        if (.not. g_cursor_only_move) return
        if (prev_line < 1) return

        ! A scroll moves every line on screen
        if (editor%viewport_line /= prev_vp_line) return
        if (editor%viewport_column /= prev_vp_col) return

        ! A ghost suggestion that has just been cleared still has to be
        ! erased, and one still showing has to move with the caret
        if (prev_ghost) return
        if (ghost_is_active(editor%ghost)) return

        ! Anything drawn over the document
        if (is_completion_visible(editor%completion_popup)) return
        if (is_context_menu_visible()) return
        ! The preview covers a document row and its click regions are
        ! re-registered per frame. The fast path repaints neither, so it would
        ! punch a hole through the overlay and leave the region table
        ! describing something no longer drawn.
        if (tab_group_preview_visible()) return
        if (is_group_picker_visible()) return
        if (is_hover_visible(editor%hover_tooltip)) return
        if (is_terminal_panel_visible(editor%terminal_panel)) return
        if (is_diagnostics_panel_visible(editor%diagnostics_panel)) return
        if (is_code_actions_panel_visible(editor%code_actions_panel)) return
        if (is_references_panel_visible(editor%references_panel)) return
        if (is_symbols_panel_visible(editor%symbols_panel)) return
        if (is_lsp_server_installer_panel_visible(editor%lsp_installer_panel)) return
        if (editor%fuss_mode_active) return

        ! An edit or an LSP update means the text itself may have changed
        if (g_lsp_modified_buffer) return
        if (g_lsp_ui_changed) return

        ! Several carets paint several blocks; the ones left behind would
        ! need erasing. Selections likewise span arbitrary lines.
        if (.not. allocated(editor%cursors)) return
        if (size(editor%cursors) /= 1) return
        if (editor%cursors(1)%has_selection) return

        ! Split panes: the inactive pane's caret is drawn too
        tab_idx = editor%active_tab_index
        if (tab_idx < 1) return
        if (tab_idx > size(editor%tabs)) return
        if (.not. allocated(editor%tabs(tab_idx)%panes)) return
        if (size(editor%tabs(tab_idx)%panes) /= 1) return

        ok = .true.
    end function caret_move_only

    subroutine register_all_commands()
        ! File operations
        call register_command('Save File', 'save', 'Ctrl+S', 'File')
        call register_command('Save All', 'save-all', 'Ctrl+Shift+S', 'File')
        call register_command('Quit', 'quit', 'Ctrl+Q', 'File')
        call register_command('Open File', 'open', 'Ctrl+O', 'File')
        call register_command('Toggle File Tree', 'toggle-tree', 'F3', 'File')

        ! Edit operations
        call register_command('Copy', 'copy', 'Ctrl+C', 'Edit')
        call register_command('Paste', 'paste', 'Ctrl+V', 'Edit')
        call register_command('Cut', 'cut', 'Ctrl+X', 'Edit')
        call register_command('Undo', 'undo', 'Ctrl+Z', 'Edit')
        call register_command('Redo', 'redo', 'Ctrl+Shift+Z', 'Edit')
        call register_command('Toggle Line Comment', 'toggle-comment', 'Ctrl+/', 'Edit')
        call register_command('Context Menu', 'context-menu', 'Shift+F10 / Alt+Z', 'Edit')

        ! AI
        call register_command('AI: Toggle Inline Completion', 'ai-toggle', 'Alt+I', 'AI')
        call register_command('AI: Status', 'ai-status', '', 'AI')
        call register_command('Group All Tabs', 'group-all', '', 'View')
        call register_command('Leave Tab Group', 'group-leave', '', 'View')
        call register_command('Edit Tab Group...', 'group-edit', '', 'View')
        call register_command('Rename Tab Group...', 'group-rename', '', 'View')
        call register_command('Dissolve Tab Group', 'group-dissolve', '', 'View')
        call register_command('AI: Deep Completion Here', 'ai-deep', 'Alt+\\', 'AI')
        call register_command('Delete Line', 'delete-line', 'Ctrl+Shift+K', 'Edit')

        ! Search operations
        call register_command('Find', 'find', 'Ctrl+F', 'Search')
        call register_command('Replace', 'replace', 'Ctrl+R', 'Search')
        call register_command('Find Next', 'find-next', 'n (in search)', 'Search')
        call register_command('Find Previous', 'find-prev', 'N (in search)', 'Search')

        ! Navigation
        call register_command('Go to Line', 'goto-line', 'Ctrl+G', 'Navigation')
        call register_command('Go to Definition', 'goto-def', 'F12', 'Navigation')
        call register_command('Find References', 'find-refs', 'Shift+F12', 'Navigation')
        call register_command('Jump Back', 'jump-back', 'Alt+,', 'Navigation')
        call register_command('Go to Symbol', 'goto-symbol', 'F4 / Alt+O', 'Navigation')

        ! LSP features
        call register_command('Code Actions', 'code-actions', 'F10 / Alt+.', 'LSP')
        call register_command('Rename Symbol', 'rename', 'F2 / Alt+N', 'LSP')
        call register_command('Show Diagnostics', 'diagnostics', 'F8 / Alt+E', 'LSP')
        call register_command('Show Hover Info', 'hover', 'Ctrl+H', 'LSP')

        ! View
        call register_command('Split Vertical', 'split-v', 'Alt+V', 'View')
        call register_command('Split Horizontal', 'split-h', 'Alt+S', 'View')
        call register_command('Close Tab', 'close-tab', 'Ctrl+W', 'View')
        call register_command('Close Pane', 'close-pane', 'Alt+Q', 'View')
        call register_command('Navigate Pane Left', 'pane-left', 'Alt+H', 'View')
        call register_command('Navigate Pane Right', 'pane-right', 'Alt+L', 'View')
        call register_command('Navigate Pane Up', 'pane-up', 'Alt+K', 'View')
        call register_command('Navigate Pane Down', 'pane-down', 'Alt+J', 'View')
        ! The terminal had no palette entry at all. These are also the only
        ! route to resizing it from the editor: the keyboard bindings are
        ! deliberately live only while the panel itself has focus.
        call register_command('Toggle Terminal', 'terminal', 'F5', 'View')
        call register_command('Terminal Taller', 'terminal-taller', &
                              'Ctrl+Shift+Up', 'View')
        call register_command('Terminal Shorter', 'terminal-shorter', &
                              'Ctrl+Shift+Down', 'View')
        call register_command('Terminal Maximize/Restore', 'terminal-max', &
                              'Ctrl+Shift+M', 'View')
        call register_command('Preferences: Color Theme', 'theme-select', '', 'Preferences')
        call register_command('Preferences: Reload Theme', 'theme-reload', '', 'Preferences')

        ! Help
        call register_command('Show Help', 'help', 'Ctrl+?', 'Help')
        call register_command('Command Palette', 'palette', 'Ctrl+P', 'Help')
    end subroutine register_all_commands

end program facsimile
