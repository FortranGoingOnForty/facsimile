module lsp_server_manager_module
    ! Manages multiple language servers
    use iso_fortran_env, only: int32, int64, real64, error_unit
    use iso_c_binding
    use json_module
    use lsp_protocol_module
    implicit none
    private

    public :: lsp_server_t
    public :: lsp_manager_t
    public :: init_lsp_manager, cleanup_lsp_manager
    public :: get_or_start_server, stop_server
    public :: send_request, send_notification
    public :: process_server_messages
    public :: register_callback
    public :: get_language_for_file, start_lsp_for_file
    public :: notify_file_opened, notify_file_changed, notify_file_saved, notify_file_closed
    public :: request_completion, request_hover, request_definition, request_references, request_code_actions
    public :: request_document_symbols
    public :: set_diagnostics_handler

    ! Language server configuration
    type :: server_config_t
        character(len=:), allocatable :: language
        character(len=:), allocatable :: command
        character(len=:), allocatable :: file_patterns
    end type server_config_t

    ! Language server instance
    type :: lsp_server_t
        type(c_ptr) :: handle = c_null_ptr
        character(len=:), allocatable :: language
        character(len=:), allocatable :: command
        character(len=:), allocatable :: root_path
        logical :: initialized = .false.
        logical :: initializing = .false.
        integer :: process_id = -1

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

        ! Message buffer
        character(len=:), allocatable :: read_buffer
    end type lsp_server_t

    ! Callback type for responses
    abstract interface
        subroutine response_callback(request_id, response)
            use lsp_protocol_module, only: lsp_message_t
            integer, intent(in) :: request_id
            type(lsp_message_t), intent(in) :: response
        end subroutine response_callback
    end interface

    ! Callback type for diagnostics notifications
    abstract interface
        subroutine diagnostics_callback(notification)
            use lsp_protocol_module, only: lsp_message_t
            type(lsp_message_t), intent(in) :: notification
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

contains

    subroutine init_lsp_manager(manager)
        type(lsp_manager_t), intent(out) :: manager

        allocate(manager%servers(0))
        allocate(manager%configs(0))
        allocate(manager%callbacks(0))

        ! Load default server configurations
        call load_default_configs(manager)
    end subroutine init_lsp_manager

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

        ! Python
        call add_config(manager, "python", "pylsp", "*.py")

        ! Rust
        call add_config(manager, "rust", "rust-analyzer", "*.rs")

        ! C/C++
        call add_config(manager, "c", "clangd", "*.c,*.h")
        call add_config(manager, "cpp", "clangd", "*.cpp,*.cc,*.cxx,*.hpp,*.hxx")

        ! Go
        call add_config(manager, "go", "gopls", "*.go")

        ! TypeScript/JavaScript
        call add_config(manager, "typescript", "typescript-language-server --stdio", "*.ts,*.tsx")
        call add_config(manager, "javascript", "typescript-language-server --stdio", "*.js,*.jsx")

        ! Fortran
        call add_config(manager, "fortran", "fortls", "*.f90,*.f95,*.f03,*.f08")

        ! TODO: Load from config file
    end subroutine load_default_configs

    subroutine add_config(manager, language, command, patterns)
        type(lsp_manager_t), intent(inout) :: manager
        character(len=*), intent(in) :: language, command, patterns
        type(server_config_t), allocatable :: new_configs(:)

        allocate(new_configs(manager%num_configs + 1))
        if (manager%num_configs > 0) then
            new_configs(1:manager%num_configs) = manager%configs
        end if

        new_configs(manager%num_configs + 1)%language = language
        new_configs(manager%num_configs + 1)%command = command
        new_configs(manager%num_configs + 1)%file_patterns = patterns

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
            write(error_unit, '(a,a)') "No LSP server configured for language: ", language
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
            write(error_unit, '(a,a)') "Failed to start LSP server: ", command
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

    subroutine send_request(server, msg, callback)
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg
        procedure(response_callback), optional :: callback
        character(len=:), allocatable :: json_msg

        if (.not. server%initialized .and. .not. server%initializing) return

        json_msg = format_json_rpc(msg)
        call send_raw_message(server, json_msg)

        ! Track request and register callback
        call track_request(server, msg%id)
        if (present(callback)) then
            ! TODO: Register callback in manager
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

        if (result < 0) then
            write(error_unit, '(a)') "Failed to send message to LSP server"
        end if
    end subroutine send_raw_message

    subroutine process_server_messages(manager)
        type(lsp_manager_t), intent(inout) :: manager
        integer :: i

        do i = 1, manager%num_servers
            if (c_associated(manager%servers(i)%handle)) then
                call process_server_output(manager, manager%servers(i))
            end if
        end do
    end subroutine process_server_messages

    subroutine process_server_output(manager, server)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        character(len=65536) :: buffer
        integer :: bytes_read
        type(lsp_message_t) :: msg

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
        if (server%initializing .and. msg%id == 1) then
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

        write(error_unit, '(a,a)') "LSP server initialized: ", server%language
    end subroutine handle_initialize_response

    subroutine handle_notification(manager, server, msg)
        type(lsp_manager_t), intent(inout) :: manager
        type(lsp_server_t), intent(inout) :: server
        type(lsp_message_t), intent(in) :: msg

        ! Debug: log all notifications
        write(error_unit, '(A,A)') "[LSP DEBUG] Received notification: ", msg%method

        select case(msg%method)
        case("textDocument/publishDiagnostics")
            write(error_unit, '(A)') "[LSP DEBUG] Processing publishDiagnostics"
            ! Forward to diagnostics handler if set
            if (associated(manager%diagnostics_handler)) then
                call manager%diagnostics_handler(msg)
            else
                write(error_unit, '(A)') "[LSP DEBUG] No diagnostics handler set!"
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

        deallocate(manager%callbacks)
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
        character(len=256) :: workspace_path
        integer :: slash_pos

        server_index = 0

        ! Get language from file extension
        language = get_language_for_file(filename)
        if (language == "") return

        ! Extract workspace path from filename (directory containing file)
        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            workspace_path = filename(1:slash_pos-1)
        else
            workspace_path = "."
        end if

        ! Get or start server for this language
        server_index = get_or_start_server(manager, language, trim(workspace_path))
    end function start_lsp_for_file

    ! Send textDocument/didOpen notification
    subroutine notify_file_opened(manager, server_index, filename, content)
        use lsp_protocol_module, only: create_did_open_notification
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: language

        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        language = get_language_for_file(filename)
        msg = create_did_open_notification(filename, language, 1, content)
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
        file_uri = 'file://' // trim(filename)

        ! Create and send the notification
        if (present(content)) then
            msg = create_did_save_notification(file_uri, content)
        else
            msg = create_did_save_notification(file_uri)
        end if

        call send_notification(manager%servers(server_index), msg)

        ! Debug output
        write(error_unit, '(A,A)') "[LSP DEBUG] Sent didSave for: ", trim(filename)
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
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = "file://" // trim(filename)

        msg = create_completion_request(trim(uri), line, character)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
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
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = "file://" // trim(filename)

        msg = create_hover_request(trim(uri), line, character)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
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
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = "file://" // trim(filename)

        msg = create_definition_request(uri, line, character)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
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
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI (simple file:// for now)
        uri = "file://" // trim(filename)

        ! Include declaration and references
        msg = create_references_request(uri, line, character, .true.)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
    end function request_references

    ! Request code actions for a range
    function request_code_actions(manager, server_index, filename, start_line, start_char, &
                                 end_line, end_char, callback) result(request_id)
        use lsp_protocol_module, only: create_code_action_request
        use json_module, only: json_create_array
        type(lsp_manager_t), intent(inout) :: manager
        integer, intent(in) :: server_index
        character(len=*), intent(in) :: filename
        integer, intent(in) :: start_line, start_char, end_line, end_char  ! 0-based LSP positions
        procedure(response_callback), optional :: callback
        integer :: request_id
        type(lsp_message_t) :: msg
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = "file://" // trim(filename)

        ! Create code action request (diagnostics will be added separately if needed)
        msg = create_code_action_request(uri, start_line, start_char, end_line, end_char)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
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
        character(len=256) :: uri

        request_id = -1
        if (server_index < 1 .or. server_index > manager%num_servers) return
        if (.not. manager%servers(server_index)%initialized) return

        ! Convert filename to URI
        uri = "file://" // trim(filename)

        ! Create document symbols request
        msg = create_document_symbols_request(uri)
        request_id = msg%id

        call send_request(manager%servers(server_index), msg, callback)
    end function request_document_symbols

    ! Set the diagnostics notification handler
    subroutine set_diagnostics_handler(manager, handler)
        type(lsp_manager_t), intent(inout) :: manager
        procedure(diagnostics_callback) :: handler

        manager%diagnostics_handler => handler
    end subroutine set_diagnostics_handler

end module lsp_server_manager_module