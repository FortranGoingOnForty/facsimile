module lsp_protocol_module
    ! Core LSP protocol implementation
    use iso_fortran_env, only: int32, int64, real64, error_unit
    use json_module
    implicit none
    private

    public :: lsp_message_t
    public :: lsp_request_t, lsp_response_t, lsp_notification_t
    public :: create_initialize_request
    public :: create_initialized_notification
    public :: create_did_open_notification
    public :: create_did_change_notification
    public :: create_did_save_notification
    public :: create_did_close_notification
    public :: create_completion_request
    public :: create_hover_request
    public :: create_definition_request
    public :: create_references_request
    public :: create_code_action_request
    public :: create_document_symbols_request
    public :: create_signature_help_request
    public :: create_formatting_request
    public :: create_rename_request
    public :: create_workspace_symbols_request
    public :: parse_lsp_message
    public :: format_json_rpc

    ! LSP message types
    type :: lsp_message_t
        character(len=:), allocatable :: jsonrpc
        integer :: id = -1  ! -1 for notifications
        character(len=:), allocatable :: method
        type(json_value_t) :: params
        type(json_value_t) :: result
        type(json_value_t) :: error
        logical :: is_request = .false.
        logical :: is_response = .false.
        logical :: is_notification = .false.
    end type lsp_message_t

    type :: lsp_request_t
        integer :: id
        character(len=:), allocatable :: method
        type(json_value_t) :: params
    end type lsp_request_t

    type :: lsp_response_t
        integer :: id
        type(json_value_t) :: result
        type(json_value_t) :: error
    end type lsp_response_t

    type :: lsp_notification_t
        character(len=:), allocatable :: method
        type(json_value_t) :: params
    end type lsp_notification_t

    ! Request ID counter
    integer :: next_request_id = 1

contains

    function get_next_request_id() result(id)
        integer :: id
        id = next_request_id
        next_request_id = next_request_id + 1
    end function get_next_request_id

    function make_absolute_path(path) result(abs_path)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: abs_path
        character(len=4096) :: cwd
        integer :: status

        ! Check if already absolute
        if (len_trim(path) > 0 .and. path(1:1) == '/') then
            abs_path = trim(path)
        else
            ! Get current working directory and prepend
            call getcwd(cwd, status)
            if (status == 0) then
                abs_path = trim(cwd) // "/" // trim(path)
            else
                abs_path = trim(path)
            end if
        end if
    end function make_absolute_path

    function create_initialize_request(process_id, root_path, client_name) result(msg)
        integer, intent(in) :: process_id
        character(len=*), intent(in) :: root_path
        character(len=*), intent(in) :: client_name
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, client_info, capabilities
        type(json_value_t) :: text_document, completion, hover
        type(json_value_t) :: definition, references, doc_symbols
        type(json_value_t) :: workspace, formatting
        character(len=:), allocatable :: abs_root_path

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "initialize"
        msg%is_request = .true.

        ! Convert root_path to absolute path
        abs_root_path = make_absolute_path(root_path)

        params = json_create_object()
        call json_add_number(params, "processId", real(process_id, real64))
        call json_add_string(params, "rootPath", abs_root_path)
        call json_add_string(params, "rootUri", "file://" // abs_root_path)

        ! Client info
        client_info = json_create_object()
        call json_add_string(client_info, "name", client_name)
        call json_add_string(client_info, "version", "0.1.0")
        call json_add_object(params, "clientInfo", client_info)

        ! Client capabilities
        capabilities = json_create_object()

        ! Text document capabilities
        text_document = json_create_object()

        ! Completion
        completion = json_create_object()
        call json_add_bool(completion, "dynamicRegistration", .false.)
        call json_add_bool(completion, "contextSupport", .true.)
        call json_add_object(text_document, "completion", completion)

        ! Hover
        hover = json_create_object()
        call json_add_bool(hover, "dynamicRegistration", .false.)
        call json_add_object(text_document, "hover", hover)

        ! Definition
        definition = json_create_object()
        call json_add_bool(definition, "dynamicRegistration", .false.)
        call json_add_bool(definition, "linkSupport", .true.)
        call json_add_object(text_document, "definition", definition)

        ! References
        references = json_create_object()
        call json_add_bool(references, "dynamicRegistration", .false.)
        call json_add_object(text_document, "references", references)

        ! Document symbols
        doc_symbols = json_create_object()
        call json_add_bool(doc_symbols, "dynamicRegistration", .false.)
        call json_add_object(text_document, "documentSymbol", doc_symbols)

        ! Formatting
        formatting = json_create_object()
        call json_add_bool(formatting, "dynamicRegistration", .false.)
        call json_add_object(text_document, "formatting", formatting)

        call json_add_object(capabilities, "textDocument", text_document)

        ! Workspace capabilities
        workspace = json_create_object()
        call json_add_bool(workspace, "applyEdit", .true.)
        ! workspaceEdit must be an object, not a boolean
        block
            type(json_value_t) :: workspace_edit
            workspace_edit = json_create_object()
            call json_add_bool(workspace_edit, "documentChanges", .true.)
            call json_add_object(workspace, "workspaceEdit", workspace_edit)
        end block
        call json_add_object(capabilities, "workspace", workspace)

        ! General capabilities.
        !
        ! Declare the position encoding instead of relying on the default.
        ! Every column this client sends and receives is converted through
        ! utf8_char_col_to_utf16 / utf16_to_utf8_char_col, so UTF-16 code
        ! units are what our arithmetic actually means. UTF-16 is also the
        ! protocol default, which is why this worked while we said nothing --
        ! but "correct because nobody wrote the field down" is one server
        ! preference away from silently mis-placing every diagnostic past a
        ! multi-byte character. Saying it out loud costs one object and makes
        ! the agreement explicit on the wire.
        !
        ! Offer UTF-16 alone. A server picks the first encoding it supports
        ! from this list, so adding utf-8 or utf-32 as a "fallback" would
        ! invite exactly the encoding this client cannot count in.
        block
            type(json_value_t) :: general, encodings
            general = json_create_object()
            encodings = json_create_array()
            call json_array_add_element(encodings, json_create_string("utf-16"))
            call json_add_array(general, "positionEncodings", encodings)
            call json_add_object(capabilities, "general", general)
        end block

        call json_add_object(params, "capabilities", capabilities)

        msg%params = params
    end function create_initialize_request

    function create_initialized_notification() result(msg)
        type(lsp_message_t) :: msg
        type(json_value_t) :: params

        msg%jsonrpc = "2.0"
        msg%method = "initialized"
        msg%is_notification = .true.

        params = json_create_object()
        msg%params = params
    end function create_initialized_notification

    function create_did_open_notification(uri, language_id, version, text) result(msg)
        character(len=*), intent(in) :: uri, language_id, text
        integer, intent(in) :: version
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document

        msg%jsonrpc = "2.0"
        msg%method = "textDocument/didOpen"
        msg%is_notification = .true.

        params = json_create_object()
        text_document = json_create_object()

        call json_add_string(text_document, "uri", uri)
        call json_add_string(text_document, "languageId", language_id)
        call json_add_number(text_document, "version", real(version, real64))
        call json_add_string(text_document, "text", text)

        call json_add_object(params, "textDocument", text_document)
        msg%params = params
    end function create_did_open_notification

    function create_did_change_notification(uri, version, text) result(msg)
        character(len=*), intent(in) :: uri, text
        integer, intent(in) :: version
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, changes, change

        msg%jsonrpc = "2.0"
        msg%method = "textDocument/didChange"
        msg%is_notification = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_number(text_document, "version", real(version, real64))
        call json_add_object(params, "textDocument", text_document)

        changes = json_create_array()
        change = json_create_object()
        call json_add_string(change, "text", text)
        call json_array_add_element(changes, change)
        call json_add_array(params, "contentChanges", changes)

        msg%params = params
    end function create_did_change_notification

    function create_did_save_notification(uri, text) result(msg)
        character(len=*), intent(in) :: uri
        character(len=*), intent(in), optional :: text
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document

        msg%jsonrpc = "2.0"
        msg%method = "textDocument/didSave"
        msg%is_notification = .true.

        params = json_create_object()
        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        if (present(text)) then
            call json_add_string(params, "text", text)
        end if

        msg%params = params
    end function create_did_save_notification

    function create_did_close_notification(uri) result(msg)
        character(len=*), intent(in) :: uri
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document

        msg%jsonrpc = "2.0"
        msg%method = "textDocument/didClose"
        msg%is_notification = .true.

        params = json_create_object()
        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        msg%params = params
    end function create_did_close_notification

    function create_completion_request(uri, line, character) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/completion"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, real64))
        call json_add_number(position, "character", real(character, real64))
        call json_add_object(params, "position", position)

        msg%params = params
    end function create_completion_request

    function create_hover_request(uri, line, character) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/hover"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, real64))
        call json_add_number(position, "character", real(character, real64))
        call json_add_object(params, "position", position)

        msg%params = params
    end function create_hover_request

    function create_definition_request(uri, line, character) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/definition"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, real64))
        call json_add_number(position, "character", real(character, real64))
        call json_add_object(params, "position", position)

        msg%params = params
    end function create_definition_request

    function create_references_request(uri, line, character, include_declaration) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        logical, intent(in) :: include_declaration
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position, context

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/references"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, real64))
        call json_add_number(position, "character", real(character, real64))
        call json_add_object(params, "position", position)

        context = json_create_object()
        call json_add_bool(context, "includeDeclaration", include_declaration)
        call json_add_object(params, "context", context)

        msg%params = params
    end function create_references_request

    function create_code_action_request(uri, start_line, start_char, end_line, end_char, diagnostics) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: start_line, start_char, end_line, end_char
        type(json_value_t), intent(in), optional :: diagnostics
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, range_obj, start_pos, end_pos, context
        type(json_value_t) :: empty_array

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/codeAction"
        msg%is_request = .true.

        params = json_create_object()

        ! Text document
        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        ! Range
        range_obj = json_create_object()
        start_pos = json_create_object()
        call json_add_number(start_pos, "line", real(start_line, real64))
        call json_add_number(start_pos, "character", real(start_char, real64))
        call json_add_object(range_obj, "start", start_pos)

        end_pos = json_create_object()
        call json_add_number(end_pos, "line", real(end_line, real64))
        call json_add_number(end_pos, "character", real(end_char, real64))
        call json_add_object(range_obj, "end", end_pos)
        call json_add_object(params, "range", range_obj)

        ! Context with diagnostics and code action kinds
        context = json_create_object()
        if (present(diagnostics)) then
            call json_add_array(context, "diagnostics", diagnostics)
        else
            empty_array = json_create_array()
            call json_add_array(context, "diagnostics", empty_array)
        end if

        ! Note: We don't filter by 'only' to get all available actions from Ruff

        call json_add_object(params, "context", context)

        msg%params = params
    end function create_code_action_request

    function create_document_symbols_request(uri) result(msg)
        character(len=*), intent(in) :: uri
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/documentSymbol"
        msg%is_request = .true.

        params = json_create_object()
        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        msg%params = params
    end function create_document_symbols_request

    function create_signature_help_request(uri, line, character) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character  ! 0-based LSP positions
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/signatureHelp"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, 8))
        call json_add_number(position, "character", real(character, 8))
        call json_add_object(params, "position", position)

        msg%params = params
    end function create_signature_help_request

    function create_formatting_request(uri, tab_size, insert_spaces) result(msg)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: tab_size
        logical, intent(in) :: insert_spaces
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, options

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/formatting"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        options = json_create_object()
        call json_add_number(options, "tabSize", real(tab_size, real64))
        call json_add_bool(options, "insertSpaces", insert_spaces)
        call json_add_object(params, "options", options)

        msg%params = params
    end function create_formatting_request

    function create_rename_request(uri, line, character, new_name) result(msg)
        character(len=*), intent(in) :: uri, new_name
        integer, intent(in) :: line, character
        type(lsp_message_t) :: msg
        type(json_value_t) :: params, text_document, position

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "textDocument/rename"
        msg%is_request = .true.

        params = json_create_object()

        text_document = json_create_object()
        call json_add_string(text_document, "uri", uri)
        call json_add_object(params, "textDocument", text_document)

        position = json_create_object()
        call json_add_number(position, "line", real(line, real64))
        call json_add_number(position, "character", real(character, real64))
        call json_add_object(params, "position", position)

        call json_add_string(params, "newName", new_name)

        msg%params = params
    end function create_rename_request

    function create_workspace_symbols_request(query) result(msg)
        character(len=*), intent(in) :: query
        type(lsp_message_t) :: msg
        type(json_value_t) :: params

        msg%jsonrpc = "2.0"
        msg%id = get_next_request_id()
        msg%method = "workspace/symbol"
        msg%is_request = .true.

        params = json_create_object()
        call json_add_string(params, "query", query)

        msg%params = params
    end function create_workspace_symbols_request

    function format_json_rpc(msg) result(formatted)
        type(lsp_message_t), intent(in) :: msg
        character(len=:), allocatable :: formatted
        type(json_value_t) :: json_msg
        character(len=:), allocatable :: json_str
        character(len=20) :: len_str

        json_msg = json_create_object()
        call json_add_string(json_msg, "jsonrpc", msg%jsonrpc)

        if (msg%is_request .or. msg%is_response) then
            call json_add_number(json_msg, "id", real(msg%id, real64))
        end if

        if (msg%is_request .or. msg%is_notification) then
            call json_add_string(json_msg, "method", msg%method)
            call json_add_object(json_msg, "params", msg%params)
        end if

        if (msg%is_response) then
            if (msg%result%value_type /= JSON_NULL) then
                call json_add_object(json_msg, "result", msg%result)
            end if
            if (msg%error%value_type /= JSON_NULL) then
                call json_add_object(json_msg, "error", msg%error)
            end if
        end if

        json_str = json_stringify(json_msg)

        ! Format as LSP message with Content-Length header
        write(len_str, '(i0)') len(json_str)
        formatted = "Content-Length: " // trim(len_str) // char(13)//char(10) // &
                   char(13)//char(10) // json_str
    end function format_json_rpc

    function parse_lsp_message(message) result(msg)
        character(len=*), intent(in) :: message
        type(lsp_message_t) :: msg
        integer :: content_start
        character(len=:), allocatable :: json_content
        type(json_value_t) :: json_obj

        ! Find where JSON content starts (after headers)
        content_start = index(message, char(13)//char(10)//char(13)//char(10))

        if (content_start > 0) then
            json_content = message(content_start + 4:)
        else
            json_content = message
        end if

        ! Parse JSON
        json_obj = json_parse(json_content)

        ! Extract message components
        msg%jsonrpc = json_get_string(json_obj, "jsonrpc", "2.0")

        if (json_has_key(json_obj, "id")) then
            msg%id = int(json_get_number(json_obj, "id", -1.0_real64))
        else
            msg%id = -1
        end if

        if (json_has_key(json_obj, "method")) then
            msg%method = json_get_string(json_obj, "method", "")
            if (msg%id >= 0) then
                msg%is_request = .true.
            else
                msg%is_notification = .true.
            end if
            msg%params = json_get_object(json_obj, "params")
        else if (msg%id >= 0) then
            msg%is_response = .true.
            ! Try to get result as array first (for definition, symbols, etc.), then as object
            msg%result = json_get_array(json_obj, "result")
            if (msg%result%value_type == JSON_NULL) then
                msg%result = json_get_object(json_obj, "result")
            end if
            msg%error = json_get_object(json_obj, "error")
        end if
    end function parse_lsp_message

end module lsp_protocol_module