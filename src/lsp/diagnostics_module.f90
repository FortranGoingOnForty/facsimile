module diagnostics_module
    use iso_fortran_env, only: int32
    use json_module, only: json_value_t, json_get_string, &
                           json_get_number, json_get_object, &
                           json_get_array, json_array_size, &
                           json_get_array_element, json_has_key
    implicit none
    private

    public :: diagnostic_t, diagnostics_store_t
    public :: init_diagnostics_store, cleanup_diagnostics_store
    public :: parse_diagnostics, clear_diagnostics
    public :: get_diagnostics_for_line, get_diagnostic_at_cursor
    public :: has_diagnostics_for_file
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
        type(json_value_t) :: params, diagnostics_array, diag_obj, range_obj
        type(json_value_t) :: start_obj, end_obj
        character(len=:), allocatable :: uri
        integer :: i, n_diagnostics, file_idx
        type(diagnostic_t) :: diag

        ! Get params from notification
        if (.not. json_has_key(notification, "params")) return
        params = json_get_object(notification, "params")

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
    end subroutine parse_diagnostics

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

end module diagnostics_module