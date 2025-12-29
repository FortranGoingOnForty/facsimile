module diagnostics_module
    use iso_fortran_env, only: int32
    use json_module, only: json_value_t, json_get_string, &
                           json_get_number, json_get_object, &
                           json_get_array, json_array_size, &
                           json_get_array_element, json_has_key, &
                           json_stringify, json_get_value
    implicit none
    private

    public :: diagnostic_t, diagnostics_store_t, diagnostic_range_t
    public :: init_diagnostics_store, cleanup_diagnostics_store
    public :: parse_diagnostics, parse_diagnostics_from_params, clear_diagnostics
    public :: parse_diagnostics_from_params_with_server  ! NEW: with server attribution
    public :: get_diagnostics_for_line, get_diagnostic_at_cursor
    public :: get_diagnostics_for_line_by_server  ! NEW: filter by server
    public :: has_diagnostics_for_file, get_diagnostics_for_file
    public :: diagnostics_to_json
    public :: SEVERITY_ERROR, SEVERITY_WARNING, SEVERITY_INFO, SEVERITY_HINT

    ! Diagnostic severity levels (LSP standard)
    integer, parameter :: SEVERITY_ERROR = 1
    integer, parameter :: SEVERITY_WARNING = 2
    integer, parameter :: SEVERITY_INFO = 3
    integer, parameter :: SEVERITY_HINT = 4

    type :: diagnostic_range_t
        integer :: start_line = 0      ! 0-based
        integer :: start_col = 0       ! 0-based
        integer :: end_line = 0        ! 0-based
        integer :: end_col = 0         ! 0-based
    end type diagnostic_range_t

    type :: diagnostic_t
        type(diagnostic_range_t) :: range
        integer :: severity = SEVERITY_ERROR
        character(len=:), allocatable :: message
        character(len=:), allocatable :: source  ! e.g., "eslint", "clangd"
        character(len=:), allocatable :: code    ! Error code
        character(len=:), allocatable :: data    ! Raw JSON data field (for Ruff quickfixes)
        integer :: server_index = 0              ! Which LSP server sent this diagnostic
    end type diagnostic_t

    type :: file_diagnostics_t
        character(len=:), allocatable :: uri
        type(diagnostic_t), allocatable :: items(:)
        integer :: count = 0
    end type file_diagnostics_t

    type :: diagnostics_store_t
        type(file_diagnostics_t), allocatable :: files(:)
        integer :: file_count = 0
    end type diagnostics_store_t

contains

    subroutine init_diagnostics_store(store)
        type(diagnostics_store_t), intent(out) :: store
        allocate(store%files(0))
        store%file_count = 0
    end subroutine init_diagnostics_store

    subroutine cleanup_diagnostics_store(store)
        type(diagnostics_store_t), intent(inout) :: store
        integer :: i, j

        if (allocated(store%files)) then
            do i = 1, store%file_count
                if (allocated(store%files(i)%uri)) deallocate(store%files(i)%uri)
                if (allocated(store%files(i)%items)) then
                    do j = 1, store%files(i)%count
                        if (allocated(store%files(i)%items(j)%message)) &
                            deallocate(store%files(i)%items(j)%message)
                        if (allocated(store%files(i)%items(j)%source)) &
                            deallocate(store%files(i)%items(j)%source)
                        if (allocated(store%files(i)%items(j)%code)) &
                            deallocate(store%files(i)%items(j)%code)
                    end do
                    deallocate(store%files(i)%items)
                end if
            end do
            deallocate(store%files)
        end if
        store%file_count = 0
    end subroutine cleanup_diagnostics_store

    ! Parse diagnostics from LSP notification
    subroutine parse_diagnostics(store, notification)
        type(diagnostics_store_t), intent(inout) :: store
        type(json_value_t), intent(in) :: notification
        type(json_value_t) :: params

        ! Get params from notification
        if (.not. json_has_key(notification, "params")) return
        params = json_get_object(notification, "params")

        ! Delegate to params parser
        call parse_diagnostics_from_params(store, params)
    end subroutine parse_diagnostics

    ! Parse diagnostics from just the params object
    subroutine parse_diagnostics_from_params(store, params)
        use terminal_io_module, only: terminal_write
        type(diagnostics_store_t), intent(inout) :: store
        type(json_value_t), intent(in) :: params
        type(json_value_t) :: diagnostics_array, diag_obj, range_obj
        type(json_value_t) :: start_obj, end_obj
        character(len=:), allocatable :: uri
        integer :: i, n_diagnostics, file_idx
        type(diagnostic_t) :: diag

        ! Get URI
        if (.not. json_has_key(params, "uri")) return
        uri = json_get_string(params, "uri")

        ! Find or create file entry
        file_idx = find_or_create_file(store, uri)

        ! Clear existing diagnostics for this file
        if (allocated(store%files(file_idx)%items)) then
            deallocate(store%files(file_idx)%items)
        end if
        store%files(file_idx)%count = 0

        ! Parse diagnostics array
        if (json_has_key(params, "diagnostics")) then
            diagnostics_array = json_get_array(params, "diagnostics")
            n_diagnostics = json_array_size(diagnostics_array)

            if (n_diagnostics > 0) then
                allocate(store%files(file_idx)%items(n_diagnostics))
                store%files(file_idx)%count = n_diagnostics

                do i = 1, n_diagnostics
                    diag_obj = json_get_array_element(diagnostics_array, i-1)

                    ! Parse range
                    if (json_has_key(diag_obj, "range")) then
                        range_obj = json_get_object(diag_obj, "range")

                        if (json_has_key(range_obj, "start")) then
                            start_obj = json_get_object(range_obj, "start")
                            diag%range%start_line = int(json_get_number(start_obj, "line"))
                            diag%range%start_col = int(json_get_number(start_obj, "character"))
                        end if

                        if (json_has_key(range_obj, "end")) then
                            end_obj = json_get_object(range_obj, "end")
                            diag%range%end_line = int(json_get_number(end_obj, "line"))
                            diag%range%end_col = int(json_get_number(end_obj, "character"))
                        end if
                    end if

                    ! Parse severity
                    if (json_has_key(diag_obj, "severity")) then
                        diag%severity = int(json_get_number(diag_obj, "severity"))
                    else
                        diag%severity = SEVERITY_ERROR
                    end if

                    ! Parse message
                    if (json_has_key(diag_obj, "message")) then
                        diag%message = json_get_string(diag_obj, "message")
                    else
                        diag%message = "Unknown error"
                    end if

                    ! Parse source
                    if (json_has_key(diag_obj, "source")) then
                        diag%source = json_get_string(diag_obj, "source")
                    else
                        diag%source = ""
                    end if

                    ! Parse code
                    if (json_has_key(diag_obj, "code")) then
                        ! Code can be string or number
                        diag%code = json_get_string(diag_obj, "code")
                    else
                        diag%code = ""
                    end if

                    store%files(file_idx)%items(i) = diag
                end do
            end if
        end if
    end subroutine parse_diagnostics_from_params

    ! Parse diagnostics with server attribution (for multi-LSP support)
    ! Instead of clearing all diagnostics, this ADDS to existing diagnostics
    ! and tags them with the server_index so they can be filtered later
    subroutine parse_diagnostics_from_params_with_server(store, params, server_index)
        use terminal_io_module, only: terminal_write
        type(diagnostics_store_t), intent(inout) :: store
        type(json_value_t), intent(in) :: params
        integer, intent(in) :: server_index
        type(json_value_t) :: diagnostics_array, diag_obj, range_obj
        type(json_value_t) :: start_obj, end_obj
        character(len=:), allocatable :: uri
        integer :: i, n_diagnostics, file_idx, new_count, j
        integer :: old_count = 0
        type(diagnostic_t) :: diag
        type(diagnostic_t), allocatable :: old_items(:)

        ! Initialize old_items to prevent uninitialized warning
        allocate(old_items(0))

        ! Get URI
        if (.not. json_has_key(params, "uri")) return
        uri = json_get_string(params, "uri")

        ! Find or create file entry
        file_idx = find_or_create_file(store, uri)

        ! First, remove any existing diagnostics from this server
        if (allocated(store%files(file_idx)%items) .and. store%files(file_idx)%count > 0) then
            ! Count diagnostics NOT from this server
            do i = 1, store%files(file_idx)%count
                if (store%files(file_idx)%items(i)%server_index /= server_index) then
                    old_count = old_count + 1
                end if
            end do

            ! Copy diagnostics NOT from this server
            if (old_count > 0) then
                allocate(old_items(old_count))
                j = 0
                do i = 1, store%files(file_idx)%count
                    if (store%files(file_idx)%items(i)%server_index /= server_index) then
                        j = j + 1
                        old_items(j) = store%files(file_idx)%items(i)
                    end if
                end do
            end if
            deallocate(store%files(file_idx)%items)
        end if

        ! Parse new diagnostics array
        n_diagnostics = 0
        if (json_has_key(params, "diagnostics")) then
            diagnostics_array = json_get_array(params, "diagnostics")
            n_diagnostics = json_array_size(diagnostics_array)
        end if

        ! Allocate combined array
        new_count = old_count + n_diagnostics
        if (new_count > 0) then
            if (allocated(store%files(file_idx)%items)) deallocate(store%files(file_idx)%items)
            allocate(store%files(file_idx)%items(new_count))
            store%files(file_idx)%count = new_count

            ! Copy old diagnostics first
            if (old_count > 0 .and. allocated(old_items)) then
                store%files(file_idx)%items(1:old_count) = old_items(1:old_count)
                deallocate(old_items)
            end if

            ! Parse and add new diagnostics
            do i = 1, n_diagnostics
                diag_obj = json_get_array_element(diagnostics_array, i-1)

                ! Parse range
                if (json_has_key(diag_obj, "range")) then
                    range_obj = json_get_object(diag_obj, "range")

                    if (json_has_key(range_obj, "start")) then
                        start_obj = json_get_object(range_obj, "start")
                        diag%range%start_line = int(json_get_number(start_obj, "line"))
                        diag%range%start_col = int(json_get_number(start_obj, "character"))
                    end if

                    if (json_has_key(range_obj, "end")) then
                        end_obj = json_get_object(range_obj, "end")
                        diag%range%end_line = int(json_get_number(end_obj, "line"))
                        diag%range%end_col = int(json_get_number(end_obj, "character"))
                    end if
                end if

                ! Parse severity
                if (json_has_key(diag_obj, "severity")) then
                    diag%severity = int(json_get_number(diag_obj, "severity"))
                else
                    diag%severity = SEVERITY_ERROR
                end if

                ! Parse message
                if (json_has_key(diag_obj, "message")) then
                    diag%message = json_get_string(diag_obj, "message")
                else
                    diag%message = "Unknown error"
                end if

                ! Parse source
                if (json_has_key(diag_obj, "source")) then
                    diag%source = json_get_string(diag_obj, "source")
                else
                    diag%source = ""
                end if

                ! Parse code
                if (json_has_key(diag_obj, "code")) then
                    diag%code = json_get_string(diag_obj, "code")
                else
                    diag%code = ""
                end if

                ! Parse data field (stores raw JSON for quickfixes like Ruff)
                if (json_has_key(diag_obj, "data")) then
                    diag%data = json_stringify(json_get_value(diag_obj, "data"))
                else
                    diag%data = ""
                end if

                ! Set server index
                diag%server_index = server_index

                store%files(file_idx)%items(old_count + i) = diag
            end do
        else
            ! No diagnostics - ensure items is allocated as empty array
            if (.not. allocated(store%files(file_idx)%items)) then
                allocate(store%files(file_idx)%items(0))
            end if
            store%files(file_idx)%count = 0
        end if
    end subroutine parse_diagnostics_from_params_with_server

    function find_or_create_file(store, uri) result(idx)
        type(diagnostics_store_t), intent(inout) :: store
        character(len=*), intent(in) :: uri
        integer :: idx, i
        type(file_diagnostics_t), allocatable :: new_files(:)

        ! Search for existing file
        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                idx = i
                return
            end if
        end do

        ! Create new file entry
        allocate(new_files(store%file_count + 1))
        if (store%file_count > 0) then
            new_files(1:store%file_count) = store%files(1:store%file_count)
        end if

        idx = store%file_count + 1
        new_files(idx)%uri = uri
        new_files(idx)%count = 0
        allocate(new_files(idx)%items(0))

        deallocate(store%files)
        store%files = new_files
        store%file_count = idx
    end function find_or_create_file

    subroutine clear_diagnostics(store, uri)
        type(diagnostics_store_t), intent(inout) :: store
        character(len=*), intent(in) :: uri
        integer :: i

        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                if (allocated(store%files(i)%items)) then
                    deallocate(store%files(i)%items)
                end if
                store%files(i)%count = 0
                allocate(store%files(i)%items(0))
                exit
            end if
        end do
    end subroutine clear_diagnostics

    function get_diagnostics_for_line(store, uri, line) result(diagnostics)
        type(diagnostics_store_t), intent(in) :: store
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line  ! 1-based editor line
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: i, j, count, lsp_line

        lsp_line = line - 1  ! Convert to 0-based

        allocate(diagnostics(0))

        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                count = 0
                ! Count diagnostics on this line
                do j = 1, store%files(i)%count
                    if (store%files(i)%items(j)%range%start_line <= lsp_line .and. &
                        store%files(i)%items(j)%range%end_line >= lsp_line) then
                        count = count + 1
                    end if
                end do

                if (count > 0) then
                    deallocate(diagnostics)
                    allocate(diagnostics(count))
                    count = 0
                    do j = 1, store%files(i)%count
                        if (store%files(i)%items(j)%range%start_line <= lsp_line .and. &
                            store%files(i)%items(j)%range%end_line >= lsp_line) then
                            count = count + 1
                            diagnostics(count) = store%files(i)%items(j)
                        end if
                    end do
                end if
                exit
            end if
        end do
    end function get_diagnostics_for_line

    ! Get diagnostics for a specific line, filtered by server index
    ! Used for code actions to only send diagnostics from the server being queried
    function get_diagnostics_for_line_by_server(store, uri, line, server_index) result(diagnostics)
        type(diagnostics_store_t), intent(in) :: store
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line  ! 1-based editor line
        integer, intent(in) :: server_index
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: i, j, count, lsp_line

        lsp_line = line - 1  ! Convert to 0-based

        allocate(diagnostics(0))

        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                count = 0
                ! Count diagnostics on this line from this server
                do j = 1, store%files(i)%count
                    if (store%files(i)%items(j)%server_index == server_index .and. &
                        store%files(i)%items(j)%range%start_line <= lsp_line .and. &
                        store%files(i)%items(j)%range%end_line >= lsp_line) then
                        count = count + 1
                    end if
                end do

                if (count > 0) then
                    deallocate(diagnostics)
                    allocate(diagnostics(count))
                    count = 0
                    do j = 1, store%files(i)%count
                        if (store%files(i)%items(j)%server_index == server_index .and. &
                            store%files(i)%items(j)%range%start_line <= lsp_line .and. &
                            store%files(i)%items(j)%range%end_line >= lsp_line) then
                            count = count + 1
                            diagnostics(count) = store%files(i)%items(j)
                        end if
                    end do
                end if
                exit
            end if
        end do
    end function get_diagnostics_for_line_by_server

    function get_diagnostic_at_cursor(store, uri, line, col) result(diagnostic)
        type(diagnostics_store_t), intent(in) :: store
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, col  ! 1-based editor position
        type(diagnostic_t) :: diagnostic
        integer :: i, j, lsp_line, lsp_col
        logical :: found

        lsp_line = line - 1  ! Convert to 0-based
        lsp_col = col - 1

        found = .false.
        diagnostic%severity = SEVERITY_INFO
        diagnostic%message = ""

        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                do j = 1, store%files(i)%count
                    if (lsp_line >= store%files(i)%items(j)%range%start_line .and. &
                        lsp_line <= store%files(i)%items(j)%range%end_line) then

                        ! Check column range if on same line
                        if ((lsp_line == store%files(i)%items(j)%range%start_line .and. &
                             lsp_col >= store%files(i)%items(j)%range%start_col) .or. &
                            (lsp_line == store%files(i)%items(j)%range%end_line .and. &
                             lsp_col <= store%files(i)%items(j)%range%end_col) .or. &
                            (lsp_line > store%files(i)%items(j)%range%start_line .and. &
                             lsp_line < store%files(i)%items(j)%range%end_line)) then

                            diagnostic = store%files(i)%items(j)
                            found = .true.
                            exit
                        end if
                    end if
                end do
                exit
            end if
        end do
    end function get_diagnostic_at_cursor

    function has_diagnostics_for_file(store, uri) result(has_diags)
        type(diagnostics_store_t), intent(in) :: store
        character(len=*), intent(in) :: uri
        logical :: has_diags
        integer :: i

        has_diags = .false.
        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                has_diags = store%files(i)%count > 0
                exit
            end if
        end do
    end function has_diagnostics_for_file

    function get_diagnostics_for_file(store, uri) result(diagnostics)
        type(diagnostics_store_t), intent(in) :: store
        character(len=*), intent(in) :: uri
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: i

        ! Find file and return all its diagnostics
        do i = 1, store%file_count
            if (store%files(i)%uri == uri) then
                if (store%files(i)%count > 0) then
                    allocate(diagnostics(store%files(i)%count))
                    diagnostics = store%files(i)%items(1:store%files(i)%count)
                else
                    allocate(diagnostics(0))
                end if
                return
            end if
        end do

        ! File not found, return empty array
        allocate(diagnostics(0))
    end function get_diagnostics_for_file

    ! Convert diagnostics array to JSON array for LSP codeAction context
    function diagnostics_to_json(diagnostics) result(json_array)
        use json_module, only: json_value_t, json_create_array, json_create_object, &
                               json_add_string, json_add_number, json_add_object, &
                               json_array_add_element, json_parse
        use iso_fortran_env, only: real64
        type(diagnostic_t), intent(in) :: diagnostics(:)
        type(json_value_t) :: json_array
        type(json_value_t) :: diag_obj, range_obj, start_pos, end_pos, data_obj
        integer :: i

        json_array = json_create_array()

        do i = 1, size(diagnostics)
            diag_obj = json_create_object()

            ! Build range object
            range_obj = json_create_object()
            start_pos = json_create_object()
            call json_add_number(start_pos, "line", real(diagnostics(i)%range%start_line, real64))
            call json_add_number(start_pos, "character", real(diagnostics(i)%range%start_col, real64))
            call json_add_object(range_obj, "start", start_pos)

            end_pos = json_create_object()
            call json_add_number(end_pos, "line", real(diagnostics(i)%range%end_line, real64))
            call json_add_number(end_pos, "character", real(diagnostics(i)%range%end_col, real64))
            call json_add_object(range_obj, "end", end_pos)

            call json_add_object(diag_obj, "range", range_obj)

            ! Add severity
            call json_add_number(diag_obj, "severity", real(diagnostics(i)%severity, real64))

            ! Add message
            if (allocated(diagnostics(i)%message)) then
                call json_add_string(diag_obj, "message", diagnostics(i)%message)
            else
                call json_add_string(diag_obj, "message", "")
            end if

            ! Add source if present (use nested if to ensure short-circuit evaluation)
            if (allocated(diagnostics(i)%source)) then
                if (len_trim(diagnostics(i)%source) > 0) then
                    call json_add_string(diag_obj, "source", diagnostics(i)%source)
                end if
            end if

            ! Add code if present (use nested if to ensure short-circuit evaluation)
            if (allocated(diagnostics(i)%code)) then
                if (len_trim(diagnostics(i)%code) > 0) then
                    call json_add_string(diag_obj, "code", diagnostics(i)%code)
                end if
            end if

            ! Add data field if present (required for Ruff quickfixes, LSP-agnostic)
            if (allocated(diagnostics(i)%data)) then
                if (len_trim(diagnostics(i)%data) > 0) then
                    data_obj = json_parse(diagnostics(i)%data)
                    call json_add_object(diag_obj, "data", data_obj)
                end if
            end if

            call json_array_add_element(json_array, diag_obj)
        end do
    end function diagnostics_to_json

end module diagnostics_module