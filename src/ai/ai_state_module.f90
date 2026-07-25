! State for model-backed completion.
!
! Deliberately free of any dependency on editor_state_module, so this type can
! live ON editor_state_t without a circular use. All the logic that needs the
! editor is in ai_engine_module, which sits above both.
module ai_state_module
    use iso_fortran_env, only: int64
    use ai_http_module, only: ai_http_t, ai_http_addr_t
    implicit none
    private

    public :: ai_state_t
    public :: AI_HEALTH_UNKNOWN, AI_HEALTH_OK, AI_HEALTH_DOWN, AI_HEALTH_NO_FIM

    integer, parameter :: AI_HEALTH_UNKNOWN = 0
    integer, parameter :: AI_HEALTH_OK      = 1
    integer, parameter :: AI_HEALTH_DOWN    = 2
    integer, parameter :: AI_HEALTH_NO_FIM  = 3

    type :: ai_state_t
        ! ---- configuration ----
        logical :: enabled = .false.          ! opt-in; nothing happens until true
        logical :: configured = .false.       ! settings have been read at least once
        character(len=:), allocatable :: host
        integer :: port = 11434
        character(len=:), allocatable :: model
        integer :: debounce_ms = 150
        integer :: num_predict = 24
        integer :: temperature_x100 = 15
        integer :: prefix_bytes = 6000
        integer :: suffix_bytes = 2000
        ! 1 disables block suggestions entirely
        integer :: max_block_lines = 4

        ! ---- backend ----
        type(ai_http_addr_t) :: addr
        integer :: health = AI_HEALTH_UNKNOWN
        character(len=:), allocatable :: last_error
        integer :: consecutive_failures = 0

        ! ---- pending trigger, recorded by the keystroke path ----
        logical :: trigger_pending = .false.
        integer(int64) :: trigger_ms = 0
        integer :: trig_line = 0
        integer :: trig_col = 0
        integer(int64) :: trig_doc_revision = -1
        character(len=:), allocatable :: trig_prefix
        character(len=:), allocatable :: trig_line_after
        logical :: trig_at_eol = .false.

        ! ---- request in flight ----
        type(ai_http_t) :: req
        logical :: in_flight = .false.
        integer :: generation = 0
        integer :: flight_generation = 0
        integer :: flight_line = 0
        integer :: flight_col = 0
        integer(int64) :: flight_doc_revision = -1
        character(len=:), allocatable :: flight_prefix
        character(len=:), allocatable :: flight_line_after
        logical :: flight_at_eol = .false.

        ! ---- rate limiting ----
        integer(int64) :: bucket_window_ms = 0
        integer :: bucket_count = 0

        ! ---- observability ----
        integer :: requests_sent = 0
        integer :: accepted_count = 0
        integer :: rejected_count = 0
        integer :: last_latency_ms = 0
        integer(int64) :: request_started_ms = 0
        character(len=:), allocatable :: last_reject_reason
    end type ai_state_t

end module ai_state_module
