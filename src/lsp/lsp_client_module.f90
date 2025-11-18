module lsp_client_module
    ! High-level LSP client interface for fac
    use iso_fortran_env, only: int32, int64, output_unit, error_unit
    use json_module
    use lsp_protocol_module
    use lsp_server_manager_module
    implicit none
    private

    public :: lsp_client_t
    public :: init_lsp_client, cleanup_lsp_client
    public :: open_document, close_document, save_document
    public :: document_changed
    public :: get_completions, get_hover_info
    public :: go_to_definition, find_references
    public :: get_document_symbols
    public :: format_document
    public :: rename_symbol
    public :: get_code_actions
    public :: process_lsp_messages

    ! Document tracking
    type :: document_info_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: file_path
        character(len=:), allocatable :: language_id
        integer :: version = 0
        integer :: server_index = 0  ! 0 means no server assigned
    end type document_info_t

    ! Type stubs for return types
    type :: lsp_completion_t
        character(len=:), allocatable :: label
        character(len=:), allocatable :: detail
    end type lsp_completion_t

    type :: lsp_location_t
        character(len=:), allocatable :: uri
        integer :: line
        integer :: column
    end type lsp_location_t

    type :: lsp_document_symbol_t
        character(len=:), allocatable :: name
        integer :: kind
    end type lsp_document_symbol_t

    type :: lsp_code_action_t
        character(len=:), allocatable :: title
        character(len=:), allocatable :: command
    end type lsp_code_action_t

    ! Main LSP client
    type :: lsp_client_t
        type(lsp_manager_t) :: manager
        type(document_info_t), allocatable :: documents(:)
        integer :: num_documents = 0
        character(len=:), allocatable :: workspace_root
    end type lsp_client_t

contains

    subroutine init_lsp_client(client, workspace_root)
        type(lsp_client_t), intent(out) :: client
        character(len=*), intent(in), optional :: workspace_root

        call init_lsp_manager(client%manager)
        allocate(client%documents(0))
        client%num_documents = 0

        if (present(workspace_root)) then
            client%workspace_root = workspace_root
        else
            client%workspace_root = "."
        end if
    end subroutine init_lsp_client

    subroutine cleanup_lsp_client(client)
        type(lsp_client_t), intent(inout) :: client
        integer :: i

        ! Close all documents
        do i = 1, client%num_documents
            call close_document(client, client%documents(i)%file_path)
        end do

        ! Clean up manager
        call cleanup_lsp_manager(client%manager)

        if (allocated(client%documents)) deallocate(client%documents)
        client%num_documents = 0
    end subroutine cleanup_lsp_client

    subroutine open_document(client, file_path, content, language_id)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path, content
        character(len=*), intent(in), optional :: language_id
        type(document_info_t), allocatable :: new_documents(:)
        type(lsp_server_t), pointer :: server
        type(lsp_message_t) :: msg
        character(len=:), allocatable :: lang_id, uri
        integer :: i, server_index

        ! Check if already open
        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                ! Already open, just update
                client%documents(i)%version = client%documents(i)%version + 1
                return
            end if
        end do

        ! Determine language ID
        if (present(language_id)) then
            lang_id = language_id
        else
            lang_id = detect_language_from_extension(file_path)
        end if

        ! Get or start server for this language
        server_index = get_or_start_server(client%manager, lang_id, client%workspace_root)
        if (server_index <= 0) return

        ! Build URI
        uri = file_path_to_uri(file_path)

        ! Add to documents array
        allocate(new_documents(client%num_documents + 1))
        if (client%num_documents > 0) then
            new_documents(1:client%num_documents) = client%documents
        end if

        new_documents(client%num_documents + 1)%uri = uri
        new_documents(client%num_documents + 1)%file_path = file_path
        new_documents(client%num_documents + 1)%language_id = lang_id
        new_documents(client%num_documents + 1)%version = 1
        new_documents(client%num_documents + 1)%server_index = server_index

        deallocate(client%documents)
        client%documents = new_documents
        client%num_documents = client%num_documents + 1

        ! Send didOpen notification
        if (client%manager%servers(server_index)%initialized) then
            msg = create_did_open_notification(uri, lang_id, 1, content)
            call send_notification(client%manager%servers(server_index), msg)
        end if
    end subroutine open_document

    subroutine close_document(client, file_path)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        type(lsp_message_t) :: msg
        integer :: i, j

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                ! Send didClose notification
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized) then
                        msg = create_did_close_notification(client%documents(i)%uri)
                        call send_notification(client%manager%servers(client%documents(i)%server_index), msg)
                    end if
                end if

                ! Remove from array
                do j = i, client%num_documents - 1
                    client%documents(j) = client%documents(j + 1)
                end do
                client%num_documents = client%num_documents - 1
                exit
            end if
        end do
    end subroutine close_document

    subroutine save_document(client, file_path, content)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in), optional :: content
        type(lsp_message_t) :: msg
        integer :: i

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized) then
                        if (present(content)) then
                            msg = create_did_save_notification(client%documents(i)%uri, content)
                        else
                            msg = create_did_save_notification(client%documents(i)%uri)
                        end if
                        call send_notification(client%manager%servers(client%documents(i)%server_index), msg)
                    end if
                end if
                exit
            end if
        end do
    end subroutine save_document

    subroutine document_changed(client, file_path, content)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path, content
        type(lsp_message_t) :: msg
        integer :: i

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                client%documents(i)%version = client%documents(i)%version + 1

                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized) then
                        msg = create_did_change_notification(client%documents(i)%uri, &
                                                            client%documents(i)%version, content)
                        call send_notification(client%manager%servers(client%documents(i)%server_index), msg)
                    end if
                end if
                exit
            end if
        end do
    end subroutine document_changed

    function get_completions(client, file_path, line, column) result(completions)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: line, column
        type(lsp_completion_t), allocatable :: completions(:)
        type(lsp_message_t) :: msg
        integer :: i

        allocate(completions(0))

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_completion) then
                        msg = create_completion_request(client%documents(i)%uri, &
                                                       line - 1, column - 1)  ! LSP is 0-based
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function get_completions

    function get_hover_info(client, file_path, line, column) result(info)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: line, column
        character(len=:), allocatable :: info
        type(lsp_message_t) :: msg
        integer :: i

        info = ""

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_hover) then
                        msg = create_hover_request(client%documents(i)%uri, &
                                                 line - 1, column - 1)  ! LSP is 0-based
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function get_hover_info

    function go_to_definition(client, file_path, line, column) result(location)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: line, column
        type(lsp_location_t) :: location
        type(lsp_message_t) :: msg
        integer :: i

        location%uri = ""

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_definition) then
                        msg = create_definition_request(client%documents(i)%uri, &
                                                       line - 1, column - 1)  ! LSP is 0-based
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function go_to_definition

    function find_references(client, file_path, line, column, include_declaration) result(locations)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: line, column
        logical, intent(in), optional :: include_declaration
        type(lsp_location_t), allocatable :: locations(:)
        type(lsp_message_t) :: msg
        logical :: include_decl
        integer :: i

        allocate(locations(0))
        include_decl = .true.
        if (present(include_declaration)) include_decl = include_declaration

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_references) then
                        msg = create_references_request(client%documents(i)%uri, &
                                                       line - 1, column - 1, include_decl)
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function find_references

    function get_document_symbols(client, file_path) result(symbols)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        type(lsp_document_symbol_t), allocatable :: symbols(:)
        type(lsp_message_t) :: msg
        integer :: i

        allocate(symbols(0))

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_document_symbols) then
                        msg = create_document_symbols_request(client%documents(i)%uri)
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function get_document_symbols

    subroutine format_document(client, file_path, tab_size, use_spaces)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in), optional :: tab_size
        logical, intent(in), optional :: use_spaces
        type(lsp_message_t) :: msg
        integer :: tabs
        logical :: spaces
        integer :: i

        tabs = 4
        if (present(tab_size)) tabs = tab_size
        spaces = .true.
        if (present(use_spaces)) spaces = use_spaces

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_formatting) then
                        msg = create_formatting_request(client%documents(i)%uri, tabs, spaces)
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end subroutine format_document

    subroutine rename_symbol(client, file_path, line, column, new_name)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path, new_name
        integer, intent(in) :: line, column
        type(lsp_message_t) :: msg
        integer :: i

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_rename) then
                        msg = create_rename_request(client%documents(i)%uri, &
                                                  line - 1, column - 1, new_name)
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end subroutine rename_symbol

    function get_code_actions(client, file_path, start_line, start_col, end_line, end_col) &
             result(actions)
        type(lsp_client_t), intent(inout) :: client
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: start_line, start_col, end_line, end_col
        type(lsp_code_action_t), allocatable :: actions(:)
        type(lsp_message_t) :: msg
        integer :: i

        allocate(actions(0))

        do i = 1, client%num_documents
            if (client%documents(i)%file_path == file_path) then
                if (client%documents(i)%server_index > 0) then
                    if (client%manager%servers(client%documents(i)%server_index)%initialized .and. &
                        client%manager%servers(client%documents(i)%server_index)%supports_code_actions) then
                        msg = create_code_action_request(client%documents(i)%uri, &
                                                        start_line - 1, start_col - 1, &
                                                        end_line - 1, end_col - 1)
                        call send_request(client%manager%servers(client%documents(i)%server_index), msg)
                        ! TODO: Wait for and process response
                    end if
                end if
                exit
            end if
        end do
    end function get_code_actions

    subroutine process_lsp_messages(client)
        type(lsp_client_t), intent(inout) :: client

        call process_server_messages(client%manager)
    end subroutine process_lsp_messages

    ! Helper functions

    function detect_language_from_extension(file_path) result(language_id)
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable :: language_id
        integer :: dot_pos
        character(len=:), allocatable :: ext

        dot_pos = index(file_path, '.', back=.true.)
        if (dot_pos > 0) then
            ext = file_path(dot_pos:)

            select case(ext)
            case('.py', '.pyw')
                language_id = "python"
            case('.c', '.h')
                language_id = "c"
            case('.cpp', '.cc', '.cxx', '.hpp', '.hxx')
                language_id = "cpp"
            case('.rs')
                language_id = "rust"
            case('.go')
                language_id = "go"
            case('.js', '.mjs')
                language_id = "javascript"
            case('.jsx')
                language_id = "javascriptreact"
            case('.ts', '.mts')
                language_id = "typescript"
            case('.tsx')
                language_id = "typescriptreact"
            case('.f90', '.f95', '.f03', '.f08', '.f18')
                language_id = "fortran"
            case('.sh', '.bash')
                language_id = "shellscript"
            case('.md', '.markdown')
                language_id = "markdown"
            case default
                language_id = "text"
            end select
        else
            language_id = "text"
        end if
    end function detect_language_from_extension

    function file_path_to_uri(file_path) result(uri)
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable :: uri

        ! Simple conversion - should handle URL encoding properly
        if (file_path(1:1) == '/') then
            uri = "file://" // file_path
        else
            ! Relative path - should get absolute path
            uri = "file://" // file_path  ! TODO: Get absolute path
        end if
    end function file_path_to_uri

end module lsp_client_module