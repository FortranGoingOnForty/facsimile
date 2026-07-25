program test_ai_http
    ! Non-blocking HTTP transport for the model backend.
    !
    ! The property that matters most is not correctness of parsing but that
    ! nothing here can take the editor down or stall it. fac is single
    ! threaded and blocks ~50ms on stdin; a synchronous network call would
    ! freeze typing, and an unhandled SIGPIPE from a peer that closed
    ! mid-send would terminate the process and lose every unsaved buffer.
    !
    ! Driven against a fixture server started by the caller (see
    ! test/run_ai_http_test.sh) on 127.0.0.1. Ports are passed as arguments
    ! so the harness can vary them.
    use ai_http_module
    implicit none

    integer :: nfail
    integer :: port_ok, port_slow, port_close, port_dead
    character(len=32) :: arg

    nfail = 0

    if (command_argument_count() < 4) then
        print '(a)', 'SKIP: needs 4 ports (ok slow close dead); run via run_ai_http_test.sh'
        stop 0
    end if
    call get_command_argument(1, arg); read(arg, *) port_ok
    call get_command_argument(2, arg); read(arg, *) port_slow
    call get_command_argument(3, arg); read(arg, *) port_close
    call get_command_argument(4, arg); read(arg, *) port_dead

    call ai_http_init()
    call check(ai_http_available(), 'transport is available on this platform', '')

    call test_resolve()
    call test_plain_get()
    call test_slow_drip()
    call test_peer_close_midway()
    call test_connection_refused()
    call test_abort_midflight()
    call test_pump_is_cheap()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All ai_http tests passed'

contains

    subroutine test_resolve()
        type(ai_http_addr_t) :: addr
        logical :: ok

        ok = ai_http_resolve('127.0.0.1', port_ok, addr)
        call check(ok .and. addr%resolved, 'loopback resolves', '')

        ok = ai_http_resolve('no-such-host.invalid', 80, addr)
        call check(.not. ok .and. .not. addr%resolved, &
                   'an unresolvable host fails cleanly', '')
    end subroutine test_resolve

    ! Drive a request to completion, bounded so a hang fails the test rather
    ! than wedging it.
    subroutine run_to_end(req, addr, path, pumps, total_ms)
        type(ai_http_t), intent(inout) :: req
        type(ai_http_addr_t), intent(in) :: addr
        character(len=*), intent(in) :: path
        integer, intent(out) :: pumps
        integer, intent(in) :: total_ms
        character(len=:), allocatable :: raw
        integer :: i
        integer(8) :: t0, t1, rate

        raw = ai_http_build_request('GET', path, '127.0.0.1', '')
        call ai_http_begin(req, addr, raw, 500, total_ms)

        ! Bounded by wall clock, not by a spin count: a fixture that drips
        ! over 100ms would otherwise race a fast spin loop and look hung.
        call system_clock(t0, rate)
        pumps = 0
        do
            call ai_http_pump(req)
            pumps = pumps + 1
            if (req%state == AI_HTTP_DONE .or. req%state == AI_HTTP_ERROR) exit
            call system_clock(t1)
            if (real(t1 - t0) / real(rate) * 1000.0 > real(total_ms) + 1000.0) exit
        end do
        i = pumps
    end subroutine run_to_end

    subroutine test_plain_get()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        character(len=:), allocatable :: body
        integer :: pumps

        if (.not. ai_http_resolve('127.0.0.1', port_ok, addr)) then
            call check(.false., 'resolve for plain GET', '')
            return
        end if

        call run_to_end(req, addr, '/hello', pumps, 5000)
        call check(req%state == AI_HTTP_DONE, 'content-length response completes', &
                   state_str(req%state))
        call check(req%status == 200, 'status line parsed', int_str(req%status))
        body = ai_http_take(req)
        call check(body == 'hello world', 'body matches exactly', body)
        call ai_http_abort(req)
        call check(req%state == AI_HTTP_IDLE, 'abort returns to idle', '')
    end subroutine test_plain_get

    ! The body arrives in pieces across many pumps: the state machine has to
    ! carry partial reads rather than assume one recv gets everything.
    subroutine test_slow_drip()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        character(len=:), allocatable :: body
        integer :: pumps

        if (.not. ai_http_resolve('127.0.0.1', port_slow, addr)) return

        call run_to_end(req, addr, '/drip', pumps, 10000)
        call check(req%state == AI_HTTP_DONE, 'dripped response completes', &
                   state_str(req%state))
        body = ai_http_take(req)
        call check(len(body) == 40, 'every dripped byte arrives', int_str(len(body)))
        call check(pumps > 1, 'it genuinely took several pumps', int_str(pumps))
        call ai_http_abort(req)
    end subroutine test_slow_drip

    ! THE important one: the peer closes without a complete response. An
    ! unignored SIGPIPE here kills the process, so reaching the next line at
    ! all is the assertion.
    subroutine test_peer_close_midway()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        integer :: pumps

        if (.not. ai_http_resolve('127.0.0.1', port_close, addr)) return

        call run_to_end(req, addr, '/cut', pumps, 5000)
        call check(req%state == AI_HTTP_DONE .or. req%state == AI_HTTP_ERROR, &
                   'a peer closing mid-response is survived, not fatal', &
                   state_str(req%state))
        call ai_http_abort(req)
        call check(.true., 'process still alive after the peer vanished', '')
    end subroutine test_peer_close_midway

    subroutine test_connection_refused()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        integer :: pumps

        if (.not. ai_http_resolve('127.0.0.1', port_dead, addr)) return

        call run_to_end(req, addr, '/nope', pumps, 2000)
        call check(req%state == AI_HTTP_ERROR, &
                   'a refused connection ends in error, not a hang', &
                   state_str(req%state))
        call check(len(ai_http_take(req)) == 0, 'no body from a failed request', '')
        call ai_http_abort(req)
    end subroutine test_connection_refused

    ! Supersession aborts in flight; closing the socket also cancels
    ! generation server-side, which is why we abort rather than ignore.
    subroutine test_abort_midflight()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        character(len=:), allocatable :: raw

        if (.not. ai_http_resolve('127.0.0.1', port_slow, addr)) return

        raw = ai_http_build_request('GET', '/drip', '127.0.0.1', '')
        call ai_http_begin(req, addr, raw, 500, 10000)
        call ai_http_pump(req)
        call ai_http_abort(req)
        call check(req%state == AI_HTTP_IDLE, 'abort mid-flight is clean', &
                   state_str(req%state))

        ! and the struct is reusable straight afterwards
        call ai_http_begin(req, addr, raw, 500, 10000)
        call check(req%state /= AI_HTTP_ERROR, 'reusable after abort', &
                   state_str(req%state))
        call ai_http_abort(req)
    end subroutine test_abort_midflight

    ! A pump with nothing in flight must not syscall at all, and a pump with
    ! a request in flight must return promptly -- the main loop calls this
    ! every iteration.
    subroutine test_pump_is_cheap()
        type(ai_http_t) :: req
        type(ai_http_addr_t) :: addr
        integer(8) :: t0, t1, rate
        integer :: i
        real :: ms

        call ai_http_pump(req)
        call check(req%state == AI_HTTP_IDLE, 'pumping an idle request is a no-op', '')

        if (.not. ai_http_resolve('127.0.0.1', port_slow, addr)) return
        call ai_http_begin(req, addr, &
                           ai_http_build_request('GET', '/drip', '127.0.0.1', ''), &
                           500, 10000)

        call system_clock(t0, rate)
        do i = 1, 1000
            call ai_http_pump(req)
            if (req%state == AI_HTTP_DONE .or. req%state == AI_HTTP_ERROR) exit
        end do
        call system_clock(t1)
        ms = real(t1 - t0) / real(rate) * 1000.0 / real(max(1, i))
        call check(ms < 2.0, 'a pump averages well under 2ms', ms_str(ms))
        call ai_http_abort(req)
    end subroutine test_pump_is_cheap

    function state_str(s) result(t)
        integer, intent(in) :: s
        character(len=24) :: t
        select case(s)
        case(AI_HTTP_IDLE);       t = 'IDLE'
        case(AI_HTTP_CONNECTING); t = 'CONNECTING'
        case(AI_HTTP_SENDING);    t = 'SENDING'
        case(AI_HTTP_RECV_HEAD);  t = 'RECV_HEAD'
        case(AI_HTTP_RECV_BODY);  t = 'RECV_BODY'
        case(AI_HTTP_DONE);       t = 'DONE'
        case(AI_HTTP_ERROR);      t = 'ERROR'
        case default;             t = 'UNKNOWN'
        end select
    end function state_str

    function int_str(v) result(s)
        integer, intent(in) :: v
        character(len=16) :: s
        write(s, '(i0)') v
    end function int_str

    function ms_str(v) result(s)
        real, intent(in) :: v
        character(len=24) :: s
        write(s, '(f8.4,a)') v, ' ms/pump'
    end function ms_str

    subroutine check(ok, name, got)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name, got

        if (ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: ' // trim(got) // ')'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ai_http
