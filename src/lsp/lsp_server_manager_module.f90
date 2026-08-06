module lsp_server_manager_module
    ! Manages multiple language servers
    use iso_fortran_env, only: int32, int64, real64, error_unit
    use iso_c_binding
    use json_module
    use lsp_protocol_module
    implicit none
    private

    public :: take_lsp_error
    public :: lsp_server_t
    public :: lsp_manager_t
    public :: init_lsp_manager, cleanup_lsp_manager, set_lsp_workspace_root
    public :: get_or_start_server, stop_server
    public :: send_request, send_notification
    public :: process_server_messages
    public :: register_callback
    public :: get_language_for_file, start_lsp_for_file
    public :: start_all_lsp_servers_for_file  ! NEW: multi-server support
    public :: get_server_with_capability       ! NEW: capability-based routing
    public :: notify_file_opened, notify_file_changed, notify_file_saved, notify_file_closed
    public :: request_completion, request_hover, request_definition, request_references, request_code_actions
    public :: request_document_symbols, request_signature_help, request_formatting, request_rename
    public :: request_workspace_symbols
    public :: set_diagnostics_handler
    public :: filename_to_uri

    ! Capability constants for routing requests to the right server
    integer, parameter, public :: CAP_COMPLETION = 1
    integer, parameter, public :: CAP_DEFINITION = 2
    integer, parameter, public :: CAP_REFERENCES = 3
    integer, parameter, public :: CAP_RENAME = 4
    integer, parameter, public :: CAP_CODE_ACTIONS = 5
    integer, parameter, public :: CAP_FORMATTING = 6
    integer, parameter, public :: CAP_DIAGNOSTICS = 7
    integer, parameter, public :: CAP_HOVER = 8
    integer, parameter, public :: CAP_DOCUMENT_SYMBOLS = 9
    integer, parameter, public :: CAP_WORKSPACE_SYMBOLS = 10
    integer, parameter, public :: NUM_CAPABILITIES = 10

    ! Language server configuration
    type :: server_config_t
        character(len=:), allocatable :: language
        character(len=:), allocatable :: name        ! Server name (e.g., "pyright", "ruff")
        character(len=:), allocatable :: command
        character(len=:), allocatable :: file_patterns
        logical :: capabilities(10) = .false.        ! Which features this server provides
    end type server_config_t

    ! Language server instance
    type :: lsp_server_t
        type(c_ptr) :: handle = c_null_ptr
        character(len=:), allocatable :: language
        character(len=:), allocatable :: name       ! Server name (e.g., "pyright", "ruff")
        character(len=:), allocatable :: command
        character(len=:), allocatable :: root_path
        logical :: initialized = .false.
        logical :: initializing = .false.
        integer :: process_id = -1
        integer :: config_index = 0                 ! Index into configs array
        integer :: init_request_id = 0              ! Request ID of initialize request

        ! Capabilities
        logical :: supports_completion = .false.
        logical :: supports_hover = .false.
        logical :: supports_definition = .false.
        logical :: supports_references = .false.
        logical :: supports_code_actions = .false.
        logical :: supports_document_symbols = .false.
        logical :: supports_workspace_symbols = .false.
        logical :: supports_formatting = .false.
        logical :: supports_rename = .false.

        ! Request tracking
        integer, allocatable :: pending_requests(:)
        integer :: num_pending = 0

        ! Pending didOpen notifications (queued before initialization)
        type(pending_didopen_t), allocatable :: pending_didopens(:)
        integer :: num_pending_didopens = 0

        ! Message buffer
        character(len=:), allocatable :: read_buffer
    end type lsp_server_t

    ! Pending didOpen notification
    type :: pending_didopen_t
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: content
    end type pending_didopen_t

    ! Callback type for responses
    abstract interface
        subroutine response_callback(request_id, response)
            use lsp_protocol_module, only: lsp_message_t
            integer, intent(in) :: request_id
            type(lsp_message_t), intent(in) :: response
        end subroutine response_callback
    end interface

    ! Callback type for diagnostics notifications (includes server_index for multi-LSP)
    abstract interface
        subroutine diagnostics_callback(notification, server_index)
            use lsp_protocol_module, only: lsp_message_t
            type(lsp_message_t), intent(in) :: notification
            integer, intent(in) :: server_index
        end subroutine diagnostics_callback
    end interface

    ! Request callback entry
    type :: callback_entry_t
        integer :: request_id
        procedure(response_callback), pointer, nopass :: callback => null()
    end type callback_entry_t

    ! Main LSP manager
    type :: lsp_manager_t
        type(lsp_server_t), allocatable :: servers(:)
        integer :: num_servers = 0
        type(server_config_t), allocatable :: configs(:)
        integer :: num_configs = 0
        type(callback_entry_t), allocatable :: callbacks(:)
        integer :: num_callbacks = 0
        procedure(diagnostics_callback), pointer, nopass :: diagnostics_handler => null()
        character(len=512) :: workspace_root = '.'  ! LSP workspace root (cwd by default)
    end type lsp_manager_t

    ! C interfaces
    interface
        subroutine lsp_start_server_f(command, command_len, handle) bind(c, name='lsp_start_server_f')
            use iso_c_binding
            character(c_char), intent(in) :: command(*)
            integer(c_int), value :: command_len
            type(c_ptr), intent(out) :: handle
        end subroutine lsp_start_server_f

        subroutine lsp_stop_server_f(handle) bind(c, name='lsp_stop_server_f')
            use iso_c_binding
            type(c_ptr), intent(inout) :: handle
        end subroutine lsp_stop_server_f

        function lsp_send_message_f(handle, message, message_len) bind(c, name='lsp_send_message_f')
            use iso_c_binding
            integer(c_int) :: lsp_send_message_f
            type(c_ptr), intent(in) :: handle
            character(c_char), intent(in) :: message(*)
            integer(c_int), value :: message_len
        end function lsp_send_message_f

        !> Push whatever a slow server would not take earlier. Called from
        !> the pump, which is the only place allowed to spend time on it.
        function lsp_flush_pending_f(handle) bind(c, name='lsp_flush_pending_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: lsp_flush_pending_f
        end function lsp_flush_pending_f

        function lsp_read_message_f(handle, buffer, buffer_len) bind(c, name='lsp_read_message_f')
            use iso_c_binding
            integer(c_int) :: lsp_read_message_f
            type(c_ptr), intent(in) :: handle
            character(c_char), intent(out) :: buffer(*)
            integer(c_int), value :: buffer_len
        end function lsp_read_message_f

        function lsp_is_running_f(handle) bind(c, name='lsp_is_running_f')
            use iso_c_binding
            integer(c_int) :: lsp_is_running_f
            type(c_ptr), intent(in) :: handle
        end function lsp_is_running_f

        function lsp_get_pid_f(handle) bind(c, name='lsp_get_pid_f')
            use iso_c_binding
            integer(c_int) :: lsp_get_pid_f
            type(c_ptr), intent(in) :: handle
        end function lsp_get_pid_f
    end interface

    ! Something worth telling the user about a language server. Held rather
    ! than printed: printing goes to the terminal the editor is drawing on.
    character(len=256) :: g_lsp_error = ''
    logical :: g_lsp_error_pending = .false.

contains

    subroutine init_lsp_manager(manager, workspace_root)
        type(lsp_manager_t), intent(out) :: manager
        character(len=*), intent(in), optional :: workspace_root
        character(len=512) :: cwd
        integer :: status

        allocate(manager%servers(0))
        allocate(manager%configs(0))
        allocate(manager%callbacks(0))

        ! Set workspace root (default to cwd if not provided)
        if (present(workspace_root) .and. len_trim(workspace_root) > 0) then
            manager%workspace_root = trim(workspace_root)
        else
            call getcwd(cwd, status)
            if (status == 0) then
                manager%workspace_root = trim(cwd)
            else
                manager%workspace_root = '.'
            end if
        end if

        ! Load default server configurations
        call load_default_configs(manager)
    end subroutine init_lsp_manager

    ! Set the workspace root for LSP servers (call before opening files)
    subroutine set_lsp_workspace_root(manager, workspace_root)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: workspace_root

        manager%workspace_root = trim(workspace_root)
    end subroutine set_lsp_workspace_root

    subroutine cleanup_lsp_manager(manager)
        type(lsp_manager_t), intent(inout) :: manager
        integer :: i

        ! Stop all servers
        do i = 1, manager%num_servers
            call stop_server(manager%servers(i))
        end do

        if (allocated(manager%servers)) deallocate(manager%servers)
        if (allocated(manager%configs)) deallocate(manager%configs)
        if (allocated(manager%callbacks)) deallocate(manager%callbacks)

        manager%num_servers = 0
        manager%num_configs = 0
        manager%num_callbacks = 0
    end subroutine cleanup_lsp_manager

    subroutine load_default_configs(manager)
        type(lsp_manager_t), intent(inout) :: manager
        logical :: caps(NUM_CAPABILITIES)

        ! Python - Pyright for semantic features (rename, definition, references, completion, hover)
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_WORKSPACE_SYMBOLS) = .true.
        call add_config(manager, "python", "pyright", "pyright-langserver --stdio", "*.py", caps)

        ! Python - Ruff for linting and code actions
        caps = .false.
        caps(CAP_CODE_ACTIONS) = .true.
        caps(CAP_FORMATTING) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        call add_config(manager, "python", "ruff", "ruff server", "*.py", caps)

        ! Rust
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_CODE_ACTIONS) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_FORMATTING) = .true.
        call add_config(manager, "rust", "rust-analyzer", "rust-analyzer", "*.rs", caps)

        ! C/C++
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_CODE_ACTIONS) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_FORMATTING) = .true.
        call add_config(manager, "c", "clangd", "clangd", "*.c,*.h", caps)
        call add_config(manager, "cpp", "clangd", "clangd", "*.cpp,*.cc,*.cxx,*.hpp,*.hxx", caps)

        ! Go
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_CODE_ACTIONS) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_FORMATTING) = .true.
        call add_config(manager, "go", "gopls", "gopls", "*.go", caps)

        ! TypeScript/JavaScript
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_CODE_ACTIONS) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_FORMATTING) = .true.
        call add_config(manager, "typescript", "ts-server", "typescript-language-server --stdio", "*.ts,*.tsx", caps)
        call add_config(manager, "javascript", "ts-server", "typescript-language-server --stdio", "*.js,*.jsx", caps)

        ! Fortran
        caps = .false.
        caps(CAP_COMPLETION) = .true.
        caps(CAP_DEFINITION) = .true.
        caps(CAP_REFERENCES) = .true.
        caps(CAP_RENAME) = .true.
        caps(CAP_HOVER) = .true.
        caps(CAP_DIAGNOSTICS) = .true.
        caps(CAP_DOCUMENT_SYMBOLS) = .true.
        caps(CAP_CODE_ACTIONS) = .true.
        call add_config(manager, "fortran", "fortls", "fortls", "*.f90,*.f95,*.f03,*.f08", caps)

        ! TODO: Load from config file
    end subroutine load_default_configs

    ! Convert filename to absolute file:// URI
    function filename_to_uri(filename) result(uri)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: uri
        character(len=4096) :: cwd
        integer :: status

        if (len_trim(filename) > 0 .and. filename(1:1) == '/') then
            ! Already absolute path
            uri = "file://" // trim(filename)
        else
            ! Relative path - prepend current directory
            call getcwd(cwd, status)
            if (status == 0) then
                uri = "file://" // trim(cwd) // "/" // trim(filename)
            else
                ! Fallback if getcwd fails
                uri = "file://" // trim(filename)
            end if
        end if
    end function filename_to_uri

    subroutine add_config(manager, language, name, command, patterns, capabilities)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: language, name, command, patterns
        logical, intent(in) :: capabilities(NUM_CAPABILITIES)
        type(server_config_t), allocatable :: new_configs(:)

        allocate(new_configs(manager%num_configs + 1))
        if (manager%num_configs > 0) then
            new_configs(1:manager%num_configs) = manager%configs
        end if

        new_configs(manager%num_configs + 1)%language = language
        new_configs(manager%num_configs + 1)%name = name
        new_configs(manager%num_configs + 1)%command = command
        new_configs(manager%num_configs + 1)%file_patterns = patterns
        new_configs(manager%num_configs + 1)%capabilities = capabilities

        deallocate(manager%configs)
        manager%configs = new_configs
        manager%num_configs = manager%num_configs + 1
    end subroutine add_config

    function get_or_start_server(manager, language, root_path) result(server_index)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: language, root_path
        integer :: server_index
        integer :: i

        ! Check if server already exists
        do i = 1, manager%num_servers
            if (manager%servers(i)%language == language .and. &
                manager%servers(i)%root_path == root_path) then
                server_index = i
                return
            end if
        end do

        ! Start new server
        server_index = start_new_server(manager, language, root_path)
    end function get_or_start_server

    function start_new_server(manager, language, root_path) result(server_index)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: language, root_path
        integer :: server_index
        type(lsp_server_t), allocatable :: new_servers(:)
        character(len=:), allocatable :: command
        integer :: i

        server_index = 0

        ! Find command for language
        command = ""
        do i = 1, manager%num_configs
            if (manager%configs(i)%language == language) then
                command = manager%configs(i)%command
                exit
            end if
        end do

        if (command == "") then
            ! Never stderr: the editor is drawing on that terminal.
            call note_lsp_error('No language server configured for ' // trim(language))
            return
        end if

        ! Expand server array
        allocate(new_servers(manager%num_servers + 1))
        if (manager%num_servers > 0) then
            new_servers(1:manager%num_servers) = manager%servers
        end if

        ! Initialize new server
        new_servers(manager%num_servers + 1)%language = language
        new_servers(manager%num_servers + 1)%command = command
        new_servers(manager%num_servers + 1)%root_path = root_path
        new_servers(manager%num_servers + 1)%initialized = .false.
        new_servers(manager%num_servers + 1)%initializing = .false.
        allocate(new_servers(manager%num_servers + 1)%pending_requests(100))
        new_servers(manager%num_servers + 1)%num_pending = 0
        new_servers(manager%num_servers + 1)%read_buffer = ""

        ! Start the server process
        call lsp_start_server_f(command//c_null_char, len(command), &
                               new_servers(manager%num_servers + 1)%handle)

        if (c_associated(new_servers(manager%num_servers + 1)%handle)) then
            new_servers(manager%num_servers + 1)%process_id = &
                lsp_get_pid_f(new_servers(manager%num_servers + 1)%handle)

            deallocate(manager%servers)
            manager%servers = new_servers
            manager%num_servers = manager%num_servers + 1
            server_index = manager%num_servers

            ! Send initialization request
            call initialize_server(manager%servers(server_index))
        else
            call note_lsp_error('Could not start the language server')
        end if
    end function start_new_server

    subroutine initialize_server(server)
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: json_msg

        if (server%initializing .or. server%initialized) return

        server%initializing = .true.

        ! Create initialization request
        msg = create_initialize_request(server%process_id, server%root_path, "fac")

        ! Store the request ID so we can match the response
        server%init_request_id = msg%id

        ! Send it
        json_msg = format_json_rpc(msg)
        call send_raw_message(server, json_msg)

        ! Track the request
        call track_request(server, msg%id)
    end subroutine initialize_server

    subroutine stop_server(server)
        type(lsp_server_t), intent(inout) :: server

        if (c_associated(server%handle)) then
            ! Send shutdown request
            ! TODO: Implement proper shutdown sequence

            call lsp_stop_server_f(server%handle)
            server%handle = c_null_ptr
        end if

        server%initialized = .false.
        server%initializing = .false.
        server%process_id = -1
    end subroutine stop_server

    subroutine send_request(manager, server_index, msg, callback)
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        type(lsp_message_t), intent(in) :: msg
        procedure(response_callback), optional :: callback
        character(len=:), allocatable :: json_msg

        if (server_index < 1 .or. server_index > manager%num_servers) return

        if (.not. manager%servers(server_index)%initialized .and. &
            .not. manager%servers(server_index)%initializing) return

        json_msg = format_json_rpc(msg)
        call send_raw_message(manager%servers(server_index), json_msg)

        ! Track request and register callback
        call track_request(manager%servers(server_index), msg%id)

        if (present(callback)) then
            call register_callback(manager, msg%id, callback)
        end if
    end subroutine send_request

    subroutine send_notification(server, msg)
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg
        character(len=:), allocatable :: json_msg

        if (.not. server%initialized .and. .not. server%initializing) return

        json_msg = format_json_rpc(msg)
        call send_raw_message(server, json_msg)
    end subroutine send_notification

    subroutine send_raw_message(server, message)
        type(lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        integer :: result

        if (.not. c_associated(server%handle)) return

        result = lsp_send_message_f(server%handle, message//c_null_char, len(message))

        ! NOT stderr. Writing there puts the text straight onto the screen,
        ! over the document, because the editor owns the terminal -- which is
        ! exactly what a user saw when a wedged server made this reachable.
        ! The editor picks this up and shows it on the status line.
        if (result < 0) call note_lsp_error('Language server stopped responding')
    end subroutine send_raw_message

    subroutine process_server_messages(manager)
        type(lsp_manager_t), intent(inout) :: manager
        integer :: i, rc

        do i = 1, manager%num_servers
            if (c_associated(manager%servers(i)%handle)) then
                ! Anything a slow server would not take on the keystroke path
                ! goes out here instead, where a stall costs a frame rather
                ! than a keystroke.
                rc = lsp_flush_pending_f(manager%servers(i)%handle)
                if (rc < 0) call note_lsp_error('Language server stopped responding')
                call process_server_output(manager, manager%servers(i))
            end if
        end do
    end subroutine process_server_messages

    !> Remember something worth telling the user, for the editor to collect.
    !> This module is compiled before the renderer, so it cannot set the
    !> status line itself -- and it must never print, see send_raw_message.
    subroutine note_lsp_error(msg)
        character(len=*), intent(in) :: msg

        if (g_lsp_error_pending) return       ! first one wins; no spew
        g_lsp_error = msg
        g_lsp_error_pending = .true.
    end subroutine note_lsp_error

    !> True once, handing over the message. The editor calls this each loop.
    logical function take_lsp_error(msg)
        character(len=:), allocatable, intent(out) :: msg

        take_lsp_error = g_lsp_error_pending
        if (take_lsp_error) then
            msg = trim(g_lsp_error)
            g_lsp_error_pending = .false.
        else
            msg = ''
        end if
    end function take_lsp_error

    subroutine process_server_output(manager, server)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        character(len=65536) :: buffer
        integer :: bytes_read

        ! Read from server
        bytes_read = lsp_read_message_f(server%handle, buffer, len(buffer))

        if (bytes_read > 0) then
            ! Append to buffer
            server%read_buffer = server%read_buffer // buffer(1:bytes_read)

            ! Try to parse complete messages
            call parse_messages(manager, server)
        end if
    end subroutine process_server_output

    subroutine parse_messages(manager, server)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        integer :: content_length, header_end, message_end
        character(len=:), allocatable :: message
        type(lsp_message_t) :: msg

        do
            ! Look for Content-Length header
            content_length = extract_content_length(server%read_buffer)
            if (content_length <= 0) exit

            ! Find where content starts
            header_end = index(server%read_buffer, char(13)//char(10)//char(13)//char(10))
            if (header_end <= 0) exit

            ! Check if we have the complete message
            message_end = header_end + 3 + content_length
            if (len(server%read_buffer) < message_end) exit

            ! Extract and parse message
            message = server%read_buffer(1:message_end)
            msg = parse_lsp_message(message)

            ! Handle the message
            call handle_message(manager, server, msg)

            ! Remove from buffer
            server%read_buffer = server%read_buffer(message_end+1:)
        end do
    end subroutine parse_messages

    function extract_content_length(buffer) result(length)
        character(len=*), intent(in) :: buffer
        integer :: length
        integer :: pos, end_pos
        character(len=32) :: len_str

        length = -1

        pos = index(buffer, "Content-Length:")
        if (pos <= 0) return

        pos = pos + 15  ! Skip "Content-Length:"

        ! Find end of line
        end_pos = index(buffer(pos:), char(13))
        if (end_pos <= 0) return

        len_str = adjustl(buffer(pos:pos+end_pos-2))
        read(len_str, '(i10)', iostat=pos) length
        if (pos /= 0) length = -1
    end function extract_content_length

    subroutine handle_message(manager, server, msg)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg

        if (msg%is_response) then
            call handle_response(manager, server, msg)
        else if (msg%is_notification) then
            call handle_notification(manager, server, msg)
        else if (msg%is_request) then
            call handle_request(manager, server, msg)
        end if
    end subroutine handle_message

    subroutine handle_response(manager, server, msg)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg
        integer :: i

        ! Remove from pending requests
        call untrack_request(server, msg%id)

        ! Special handling for initialization response
        if (server%initializing .and. msg%id == server%init_request_id) then
            call handle_initialize_response(server, msg)
            return
        end if

        ! Find and call registered callback
        do i = 1, manager%num_callbacks
            if (manager%callbacks(i)%request_id == msg%id) then
                if (associated(manager%callbacks(i)%callback)) then
                    call manager%callbacks(i)%callback(msg%id, msg)
                end if
                ! Remove callback
                call remove_callback(manager, i)
                exit
            end if
        end do
    end subroutine handle_response

    subroutine handle_initialize_response(server, msg)
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg
        type(json_value_t) :: capabilities
        type(lsp_message_t) :: initialized_msg

        ! Parse server capabilities
        capabilities = json_get_object(msg%result, "capabilities")

        ! Check what the server supports
        if (json_has_key(capabilities, "completionProvider")) then
            server%supports_completion = .true.
        end if

        if (json_has_key(capabilities, "hoverProvider")) then
            server%supports_hover = .true.
        end if

        if (json_has_key(capabilities, "definitionProvider")) then
            server%supports_definition = .true.
        end if

        if (json_has_key(capabilities, "referencesProvider")) then
            server%supports_references = .true.
        end if

        if (json_has_key(capabilities, "codeActionProvider")) then
            server%supports_code_actions = .true.
        end if

        if (json_has_key(capabilities, "documentSymbolProvider")) then
            server%supports_document_symbols = .true.
        end if

        if (json_has_key(capabilities, "workspaceSymbolProvider")) then
            server%supports_workspace_symbols = .true.
        end if

        if (json_has_key(capabilities, "documentFormattingProvider")) then
            server%supports_formatting = .true.
        end if

        if (json_has_key(capabilities, "renameProvider")) then
            server%supports_rename = .true.
        end if

        if (json_has_key(capabilities, "codeActionProvider")) then
            server%supports_code_actions = .true.
        end if

        ! Send initialized notification
        initialized_msg = create_initialized_notification()
        call send_notification(server, initialized_msg)

        server%initialized = .true.
        server%initializing = .false.

        ! Send all queued didOpen notifications
        if (allocated(server%pending_didopens) .and. server%num_pending_didopens > 0) then
            block
                integer :: i
                type(lsp_message_t) :: didopen_msg
                character(len=:), allocatable :: lang, file_uri

                do i = 1, server%num_pending_didopens
                    if (allocated(server%pending_didopens(i)%filename) .and. &
                        allocated(server%pending_didopens(i)%content)) then

                        lang = get_language_for_file(server%pending_didopens(i)%filename)
                        file_uri = filename_to_uri(server%pending_didopens(i)%filename)
                        didopen_msg = create_did_open_notification( &
                            file_uri, lang, 1, &
                            server%pending_didopens(i)%content)
                        call send_notification(server, didopen_msg)
                    end if
                end do

                ! Clear the queue
                deallocate(server%pending_didopens)
                server%num_pending_didopens = 0
            end block
        end if
    end subroutine handle_initialize_response

    subroutine handle_notification(manager, server, msg)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg
        integer :: srv_idx, i

        ! Find the server index for this server
        srv_idx = 0
        do i = 1, manager%num_servers
            if (allocated(manager%servers(i)%name) .and. allocated(server%name)) then
                if (manager%servers(i)%name == server%name .and. &
                    manager%servers(i)%root_path == server%root_path) then
                    srv_idx = i
                    exit
                end if
            end if
        end do

        select case(msg%method)
        case("textDocument/publishDiagnostics")
            ! Forward to diagnostics handler if set (with server index)
            if (associated(manager%diagnostics_handler)) then
                call manager%diagnostics_handler(msg, srv_idx)
            end if
        case("window/showMessage")
            ! TODO: Show message to user
        case("window/logMessage")
            ! TODO: Log message
        case default
            ! Unknown notification
        end select
    end subroutine handle_notification

    subroutine handle_request(manager, server, msg)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg

        ! Servers can send requests too (like workspace/configuration)
        ! TODO: Handle server requests
        ! Silence unused argument warnings (stub for future implementation)
        if (.false.) print *, manager%num_servers, server%initialized, msg%id
    end subroutine handle_request

    subroutine track_request(server, request_id)
        type(lsp_server_t), intent(inout) :: server
        integer, intent(in) :: request_id

        if (server%num_pending < size(server%pending_requests)) then
            server%num_pending = server%num_pending + 1
            server%pending_requests(server%num_pending) = request_id
        end if
    end subroutine track_request

    subroutine untrack_request(server, request_id)
        type(lsp_server_t), intent(inout) :: server
        integer, intent(in) :: request_id
        integer :: i, j

        do i = 1, server%num_pending
            if (server%pending_requests(i) == request_id) then
                ! Shift remaining requests
                do j = i, server%num_pending - 1
                    server%pending_requests(j) = server%pending_requests(j + 1)
                end do
                server%num_pending = server%num_pending - 1
                exit
            end if
        end do
    end subroutine untrack_request

    subroutine register_callback(manager, request_id, callback)
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: request_id
        procedure(response_callback) :: callback
        type(callback_entry_t), allocatable :: new_callbacks(:)

        allocate(new_callbacks(manager%num_callbacks + 1))

        if (manager%num_callbacks > 0) then
            new_callbacks(1:manager%num_callbacks) = manager%callbacks
        end if

        new_callbacks(manager%num_callbacks + 1)%request_id = request_id
        new_callbacks(manager%num_callbacks + 1)%callback => callback

        if (allocated(manager%callbacks)) deallocate(manager%callbacks)
        allocate(manager%callbacks(size(new_callbacks)))
        manager%callbacks = new_callbacks
        manager%num_callbacks = manager%num_callbacks + 1
    end subroutine register_callback

    subroutine remove_callback(manager, index)
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: index
        integer :: i

        if (index < 1 .or. index > manager%num_callbacks) return

        ! Shift remaining callbacks
        do i = index, manager%num_callbacks - 1
            manager%callbacks(i) = manager%callbacks(i + 1)
        end do

        manager%num_callbacks = manager%num_callbacks - 1
    end subroutine remove_callback

    ! Helper to get language from filename
    function get_language_for_file(filename) result(language)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: language
        integer :: dot_pos

        ! Find last dot in filename
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos == 0) then
            language = ""
            return
        end if

        ! Match extension to language
        select case(filename(dot_pos:))
        case('.py')
            language = "python"
        case('.rs')
            language = "rust"
        case('.c', '.h')
            language = "c"
        case('.cpp', '.cc', '.cxx', '.hpp', '.hxx', '.C', '.H')
            language = "cpp"
        case('.go')
            language = "go"
        case('.ts', '.tsx')
            language = "typescript"
        case('.js', '.jsx')
            language = "javascript"
        case('.f90', '.f95', '.f03', '.f08', '.F90', '.F95', '.F03', '.F08')
            language = "fortran"
        case('.java')
            language = "java"
        case('.rb')
            language = "ruby"
        case('.lua')
            language = "lua"
        case default
            language = ""
        end select
    end function get_language_for_file

    ! Start LSP server for a file if needed
    function start_lsp_for_file(manager, filename) result(server_index)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: filename
        integer :: server_index
        character(len=:), allocatable :: language

        server_index = 0

        ! Get language from file extension
        language = get_language_for_file(filename)
        if (language == "") return

        ! Use manager's workspace_root (set during init, defaults to cwd)
        server_index = get_or_start_server(manager, language, trim(manager%workspace_root))
    end function start_lsp_for_file

    ! Start ALL LSP servers that match a file (multi-server support)
    subroutine start_all_lsp_servers_for_file(manager, filename, server_indices, num_servers)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: server_indices(:)
        integer, intent(out) :: num_servers
        character(len=:), allocatable :: language
        integer :: i, idx
        integer :: temp_indices(20)  ! Max 20 servers per file

        num_servers = 0
        allocate(server_indices(0))

        ! Get language from file extension
        language = get_language_for_file(filename)
        if (language == "") return

        ! Use manager's workspace_root (set during init, defaults to cwd)
        ! Find ALL configs that match this language and start servers
        do i = 1, manager%num_configs
            if (manager%configs(i)%language == language) then
                ! Start or get server for this config
                idx = get_or_start_server_by_config(manager, i, trim(manager%workspace_root))
                if (idx > 0 .and. num_servers < 20) then
                    num_servers = num_servers + 1
                    temp_indices(num_servers) = idx
                end if
            end if
        end do

        ! Copy to output array
        if (num_servers > 0) then
            deallocate(server_indices)
            allocate(server_indices(num_servers))
            server_indices = temp_indices(1:num_servers)
        end if
    end subroutine start_all_lsp_servers_for_file

    ! Get or start server for a specific config index
    function get_or_start_server_by_config(manager, config_index, root_path) result(server_index)
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: config_index
        character(len=*), intent(in) :: root_path
        integer :: server_index
        integer :: i

        server_index = 0
        if (config_index < 1 .or. config_index > manager%num_configs) return

        ! Check if server already exists for this config and root
        do i = 1, manager%num_servers
            if (manager%servers(i)%config_index == config_index .and. &
                manager%servers(i)%root_path == root_path) then
                server_index = i
                return
            end if
        end do

        ! Start new server
        server_index = start_new_server_from_config(manager, config_index, root_path)
    end function get_or_start_server_by_config

    ! Start a new server from a specific config
    function start_new_server_from_config(manager, config_index, root_path) result(server_index)
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: config_index
        character(len=*), intent(in) :: root_path
        integer :: server_index
        type(lsp_server_t), allocatable :: new_servers(:)
        character(len=:), allocatable :: command

        server_index = 0
        if (config_index < 1 .or. config_index > manager%num_configs) return

        command = manager%configs(config_index)%command

        ! Expand server array
        allocate(new_servers(manager%num_servers + 1))
        if (manager%num_servers > 0) then
            new_servers(1:manager%num_servers) = manager%servers
        end if

        ! Initialize new server
        new_servers(manager%num_servers + 1)%language = manager%configs(config_index)%language
        new_servers(manager%num_servers + 1)%name = manager%configs(config_index)%name
        new_servers(manager%num_servers + 1)%command = command
        new_servers(manager%num_servers + 1)%root_path = root_path
        new_servers(manager%num_servers + 1)%config_index = config_index
        new_servers(manager%num_servers + 1)%initialized = .false.
        new_servers(manager%num_servers + 1)%initializing = .false.
        allocate(new_servers(manager%num_servers + 1)%pending_requests(100))
        new_servers(manager%num_servers + 1)%num_pending = 0
        new_servers(manager%num_servers + 1)%read_buffer = ""

        ! Start the server process
        call lsp_start_server_f(command//c_null_char, len(command), &
                               new_servers(manager%num_servers + 1)%handle)

        if (c_associated(new_servers(manager%num_servers + 1)%handle)) then
            new_servers(manager%num_servers + 1)%process_id = &
                lsp_get_pid_f(new_servers(manager%num_servers + 1)%handle)

            deallocate(manager%servers)
            manager%servers = new_servers
            manager%num_servers = manager%num_servers + 1
            server_index = manager%num_servers

            ! Send initialization request
            call initialize_server(manager%servers(server_index))
        else
            call note_lsp_error('Could not start the language server')
        end if
    end function start_new_server_from_config

    ! Get the first server from a list of indices that has a specific capability
    function get_server_with_capability(manager, server_indices, num_servers, capability) result(server_index)
        type(lsp_manager_t), intent(in) :: manager
        integer, intent(in) :: server_indices(:)
        integer, intent(in) :: num_servers
        integer, intent(in) :: capability
        integer :: server_index
        integer :: i, cfg_idx

        server_index = 0

        do i = 1, num_servers
            if (server_indices(i) > 0 .and. server_indices(i) <= manager%num_servers) then
                cfg_idx = manager%servers(server_indices(i))%config_index
                if (cfg_idx > 0 .and. cfg_idx <= manager%num_configs) then
                    if (manager%configs(cfg_idx)%capabilities(capability)) then
                        server_index = server_indices(i)
                        return
                    end if
                end if
            end if
        end do
    end function get_server_with_capability

    ! Send textDocument/didOpen notification
    subroutine notify_file_opened(manager, server_index, filename, content)
        use lsp_protocol_module, only: create_did_open_notification
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: language
        type(pending_didopen_t), allocatable :: temp_pending(:)
        integer :: i

        if (server_index < 1 .or. server_index > manager%num_servers) return

        ! If server not initialized yet, queue the notification
        if (.not. manager%servers(server_index)%initialized) then
            ! Add to pending queue
            if (allocated(manager%servers(server_index)%pending_didopens)) then
                ! Grow array
                allocate(temp_pending(manager%servers(server_index)%num_pending_didopens + 1))
                do i = 1, manager%servers(server_index)%num_pending_didopens
                    temp_pending(i) = manager%servers(server_index)%pending_didopens(i)
                end do
                deallocate(manager%servers(server_index)%pending_didopens)
                allocate(manager%servers(server_index)%pending_didopens(size(temp_pending)))
                manager%servers(server_index)%pending_didopens = temp_pending
                deallocate(temp_pending)
            else
                allocate(manager%servers(server_index)%pending_didopens(1))
            end if

            manager%servers(server_index)%num_pending_didopens = &
                manager%servers(server_index)%num_pending_didopens + 1

            ! Store filename and content
            allocate(character(len=len(filename)) :: &
                manager%servers(server_index)%pending_didopens( &
                    manager%servers(server_index)%num_pending_didopens)%filename)
            manager%servers(server_index)%pending_didopens( &
                manager%servers(server_index)%num_pending_didopens)%filename = filename

            allocate(character(len=len(content)) :: &
                manager%servers(server_index)%pending_didopens( &
                    manager%servers(server_index)%num_pending_didopens)%content)
            manager%servers(server_index)%pending_didopens( &
                manager%servers(server_index)%num_pending_didopens)%content = content

            return
        end if

        ! Server is ready, send immediately
        language = get_language_for_file(filename)
        block
            character(len=:), allocatable :: file_uri
            file_uri = filename_to_uri(filename)
            msg = create_did_open_notification(file_uri, language, 1, content)
        end block

        call send_notification(manager%servers(server_index), msg)
    end subroutine notify_file_opened

    ! Send textDocument/didChange notification
    subroutine notify_file_changed(manager, server_index, filename, content, version)
        use lsp_protocol_module, only: create_did_change_notification
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        integer, intent(in), optional :: version
        type(lsp_message_t) :: msg
        integer :: doc_version
        integer :: debug_unit

        ! Debug logging
        open(newunit=debug_unit, file='/tmp/fac_didchange_debug.log', status='unknown', &
             position='append', action='write')
        write(debug_unit, '(A,I2,A)') 'notify_file_changed called for server ', server_index, ':'
        write(debug_unit, '(A,A)') '  URI: ', trim(filename)
        write(debug_unit, '(A,I8)') '  Content length: ', len(content)
        write(debug_unit, '(A)') '---'
        close(debug_unit)

        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Use provided version or default to 1
        doc_version = 1
        if (present(version)) doc_version = version

        msg = create_did_change_notification(filename, doc_version, content)
        call send_notification(manager%servers(server_index), msg)
    end subroutine notify_file_changed

    ! Send textDocument/didSave notification
    subroutine notify_file_saved(manager, server_index, filename, content)
        use lsp_protocol_module, only: create_did_save_notification
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: content
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: file_uri

        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        file_uri = filename_to_uri(filename)

        ! Create and send the notification
        if (present(content)) then
            msg = create_did_save_notification(file_uri, content)
        else
            msg = create_did_save_notification(file_uri)
        end if

        call send_notification(manager%servers(server_index), msg)
    end subroutine notify_file_saved

    ! Send textDocument/didClose notification
    subroutine notify_file_closed(manager, server_index, filename)
        use lsp_protocol_module, only: create_did_close_notification
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        type(lsp_message_t) :: msg

        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        msg = create_did_close_notification(filename)
        call send_notification(manager%servers(server_index), msg)
    end subroutine notify_file_closed

    ! Request code completion at cursor position
    function request_completion(manager, server_index, filename, line, character, callback) result(request_id)
        use lsp_protocol_module, only: create_completion_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = filename_to_uri(filename)

        msg = create_completion_request(trim(uri), line, character)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_completion

    ! Request hover information at cursor position
    function request_hover(manager, server_index, filename, line, character, callback) result(request_id)
        use lsp_protocol_module, only: create_hover_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = filename_to_uri(filename)

        msg = create_hover_request(trim(uri), line, character)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_hover

    ! Request definition location at cursor position
    function request_definition(manager, server_index, filename, line, character, callback) result(request_id)
        use lsp_protocol_module, only: create_definition_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        uri = filename_to_uri(filename)
        msg = create_definition_request(uri, line, character)
        request_id = msg%id
        call send_request(manager, server_index, msg, callback)
    end function request_definition

    ! Request references at cursor position
    function request_references(manager, server_index, filename, line, character, callback) result(request_id)
        use lsp_protocol_module, only: create_references_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = filename_to_uri(filename)

        ! Include declaration and references
        msg = create_references_request(uri, line, character, .true.)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_references

    ! Request code actions for a range
    function request_code_actions(manager, server_index, filename, start_line, start_char, &
                                 end_line, end_char, callback, diagnostics_json) result(request_id)
        use lsp_protocol_module, only: create_code_action_request
        use json_module, only: json_create_array, json_value_t
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: start_line, start_char, end_line, end_char  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        type(json_value_t), intent(in), optional :: diagnostics_json
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1

        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = filename_to_uri(filename)

        ! Create code action request with diagnostics context
        if (present(diagnostics_json)) then
            msg = create_code_action_request(uri, start_line, start_char, end_line, end_char, diagnostics_json)
        else
            msg = create_code_action_request(uri, start_line, start_char, end_line, end_char)
        end if
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_code_actions

    ! Request document symbols
    function request_document_symbols(manager, server_index, filename, callback) result(request_id)
        use lsp_protocol_module, only: create_document_symbols_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = filename_to_uri(filename)

        ! Create document symbols request
        msg = create_document_symbols_request(uri)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_document_symbols

    ! Request signature help
    function request_signature_help(manager, server_index, filename, line, character, callback) result(request_id)
        use lsp_protocol_module, only: create_signature_help_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = filename_to_uri(filename)

        ! Create signature help request
        msg = create_signature_help_request(uri, line, character)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_signature_help

    ! Request formatting
    function request_formatting(manager, server_index, filename, tab_size, insert_spaces, callback) result(request_id)
        use lsp_protocol_module, only: create_formatting_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: tab_size
        logical, intent(in) :: insert_spaces
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = filename_to_uri(filename)

        ! Create formatting request
        msg = create_formatting_request(uri, tab_size, insert_spaces)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_formatting

    ! Request rename
    function request_rename(manager, server_index, filename, line, character, new_name, callback) result(request_id)
        use lsp_protocol_module, only: create_rename_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line, character  ! 0-based LSP positions
        character(len=*), intent(in) :: new_name
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = filename_to_uri(filename)

        ! Create rename request
        msg = create_rename_request(uri, line, character, new_name)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_rename

    ! Request workspace symbols
    function request_workspace_symbols(manager, server_index, query, callback) result(request_id)
        use lsp_protocol_module, only: create_workspace_symbols_request
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: query
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return
        if (.not. manager%servers(server_index)%supports_workspace_symbols) return

        ! Create workspace symbols request
        msg = create_workspace_symbols_request(query)
        request_id = msg%id

        call send_request(manager, server_index, msg, callback)
    end function request_workspace_symbols

    ! Set the diagnostics notification handler
    subroutine set_diagnostics_handler(manager, handler)
        type(lsp_manager_t), intent(inout) :: manager
        procedure(diagnostics_callback) :: handler

        manager%diagnostics_handler => handler
    end subroutine set_diagnostics_handler

end module lsp_server_manager_module