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
    public :: ai_remote_badge, ai_host_is_loopback

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
        logical :: include_header = .true.
        logical :: include_symbols = .true.
        character(len=:), allocatable :: filename

        ! ---- remote tier, a SEPARATE opt-in ----
        ! Turning on completion enables loopback only. Reaching another
        ! machine is a second, deliberate decision, because source code
        ! leaving the box deserves its own switch.
        logical :: remote_enabled = .false.
        character(len=:), allocatable :: remote_host
        integer :: remote_port = 11434
        character(len=:), allocatable :: remote_model
        integer :: remote_num_predict = 256
        type(ai_http_addr_t) :: remote_addr
        logical :: remote_is_loopback = .true.
        character(len=:), allocatable :: gate_url

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

contains

    pure function ai_host_is_loopback(host) result(res)
        character(len=*), intent(in) :: host
        logical :: res
        character(len=:), allocatable :: h

        h = trim(host)
        res = h == '127.0.0.1' .or. h == 'localhost' .or. h == '::1' .or. &
              h == '' .or. index(h, '127.') == 1
    end function ai_host_is_loopback

    ! A persistent marker shown whenever code can leave this machine. Lives
    ! here rather than in the engine so the renderer can read it without
    ! depending on the engine, which sits above editor_state_t.
    function ai_remote_badge(ai) result(text)
        type(ai_state_t), intent(in) :: ai
        character(len=:), allocatable :: text

        text = ''
        if (.not. ai%enabled) return
        if (.not. ai%remote_enabled) return
        if (ai%remote_is_loopback) return
        if (.not. allocated(ai%remote_host)) return
        if (len_trim(ai%remote_host) == 0) return
        text = '[AI->' // trim(ai%remote_host) // ']'
    end function ai_remote_badge

end module ai_state_module
