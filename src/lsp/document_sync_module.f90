module document_sync_module
    use iso_fortran_env, only: int32, int64, real64
    use lsp_server_manager_module, only: notify_file_changed
    implicit none
    private

    public :: document_sync_t
    public :: init_document_sync, cleanup_document_sync
    public :: notify_document_change, flush_pending_changes
    public :: set_sync_delay

    ! Document synchronization manager with debouncing
    type :: document_sync_t
        integer :: version = 0                    ! Current document version
        character(len=:), allocatable :: uri      ! File URI
        character(len=:), allocatable :: pending_content  ! Pending content to send
        real(real64) :: last_change_time = 0.0    ! Time of last change (for debouncing)
        real(real64) :: sync_delay = 0.5          ! Delay before sending (in seconds)
        logical :: has_pending_changes = .false.  ! Flag for pending changes
        integer :: server_index = 0               ! LSP server handling this document
    end type document_sync_t

contains

    subroutine init_document_sync(sync, uri, server_index)
        type(document_sync_t), intent(out) :: sync
        character(len=*), intent(in) :: uri
        integer, intent(in) :: server_index

        sync%version = 0
        sync%uri = uri
        sync%server_index = server_index
        sync%sync_delay = 0.5  ! 500ms default
        sync%has_pending_changes = .false.
        sync%last_change_time = 0.0
    end subroutine init_document_sync

    subroutine cleanup_document_sync(sync)
        type(document_sync_t), intent(inout) :: sync

        if (allocated(sync%uri)) deallocate(sync%uri)
        if (allocated(sync%pending_content)) deallocate(sync%pending_content)
        sync%has_pending_changes = .false.
    end subroutine cleanup_document_sync

    subroutine set_sync_delay(sync, delay_seconds)
        type(document_sync_t), intent(inout) :: sync
        real(real64), intent(in) :: delay_seconds

        sync%sync_delay = max(0.1_real64, min(2.0_real64, delay_seconds))  ! Clamp between 100ms and 2s
    end subroutine set_sync_delay

    ! Notify that the document has changed (with debouncing)
    subroutine notify_document_change(sync, content)
        type(document_sync_t), intent(inout) :: sync
        character(len=*), intent(in) :: content

        ! Update pending content
        if (allocated(sync%pending_content)) deallocate(sync%pending_content)
        sync%pending_content = content

        ! Mark that we have pending changes
        sync%has_pending_changes = .true.

        ! Update timestamp
        sync%last_change_time = get_current_time()
    end subroutine notify_document_change

    ! Check if enough time has passed and flush pending changes
    subroutine flush_pending_changes(sync, manager, force)
        use lsp_server_manager_module, only: lsp_manager_t, notify_file_changed
        type(document_sync_t), intent(inout) :: sync
        type(lsp_manager_t), intent(inout) :: manager
        logical, intent(in), optional :: force
        real(real64) :: current_time, elapsed
        logical :: should_flush

        if (.not. sync%has_pending_changes) return

        ! `force` says "do not wait for the debounce", NOT "do not flush".
        ! It used to select between the two, so the main loop's periodic call
        ! -- which passes .false. -- disabled the elapsed check instead of
        ! asking for it, and the debounce never fired at all. Changes then
        ! reached the server only when something else forced a flush, which
        ! in practice meant a completion request: edit, do not type another
        ! word character, and the server was still holding the old text.
        ! That is a stale diagnostic, a stale hover and a stale definition,
        ! and it is why the completion path had to force a flush of its own
        ! before it could trust a position.
        should_flush = .false.
        if (present(force)) should_flush = force
        if (.not. should_flush) then
            current_time = get_current_time()
            elapsed = current_time - sync%last_change_time
            should_flush = (elapsed >= sync%sync_delay)
        end if

        if (should_flush .and. allocated(sync%pending_content)) then
            ! Increment version
            sync%version = sync%version + 1

            ! Send the notification
            call notify_file_changed(manager, sync%server_index, &
                                   sync%uri, sync%pending_content)

            ! Clear pending changes
            sync%has_pending_changes = .false.
            deallocate(sync%pending_content)
        end if
    end subroutine flush_pending_changes

    ! Helper function to get current time in seconds
    function get_current_time() result(time)
        real(real64) :: time
        integer(int64) :: count, count_rate

        call system_clock(count, count_rate)
        time = real(count, real64) / real(count_rate, real64)
    end function get_current_time

end module document_sync_module