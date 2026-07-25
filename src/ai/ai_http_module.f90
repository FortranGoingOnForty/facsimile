! Fortran face of the non-blocking HTTP client in ai_http.c.
!
! Usage is deliberately request-at-a-time: resolve a host once when a backend
! is enabled, then begin/pump/take/abort per request. Nothing here blocks --
! ai_http_pump advances one state and returns, so it can be called from the
! main loop beside process_server_messages without touching input latency.
module ai_http_module
    use iso_c_binding, only: c_ptr, c_int, c_char, c_null_ptr, c_null_char, &
                             c_associated, c_signed_char
    implicit none
    private

    public :: ai_http_t, ai_http_addr_t
    public :: ai_http_init, ai_http_available, ai_http_resolve
    public :: ai_http_begin, ai_http_pump, ai_http_take, ai_http_abort
    public :: ai_http_build_request
    public :: AI_HTTP_IDLE, AI_HTTP_CONNECTING, AI_HTTP_SENDING
    public :: AI_HTTP_RECV_HEAD, AI_HTTP_RECV_BODY, AI_HTTP_DONE, AI_HTTP_ERROR

    ! Must match the #defines in ai_http.c
    integer, parameter :: AI_HTTP_IDLE       = 0
    integer, parameter :: AI_HTTP_CONNECTING = 1
    integer, parameter :: AI_HTTP_SENDING    = 2
    integer, parameter :: AI_HTTP_RECV_HEAD  = 3
    integer, parameter :: AI_HTTP_RECV_BODY  = 4
    integer, parameter :: AI_HTTP_DONE       = 5
    integer, parameter :: AI_HTTP_ERROR      = 6

    ! Opaque sockaddr_in. 16 bytes on every POSIX platform we build for; the
    ! C side memcpy's into it and never reads it back through Fortran.
    type :: ai_http_addr_t
        integer(c_signed_char) :: bytes(16) = 0
        logical :: resolved = .false.
    end type ai_http_addr_t

    type :: ai_http_t
        type(c_ptr) :: handle = c_null_ptr
        integer :: state = AI_HTTP_IDLE
        integer :: status = 0
        integer :: body_bytes = 0
    end type ai_http_t

    interface
        subroutine c_ai_http_init() bind(C, name='ai_http_init_f')
        end subroutine

        function c_ai_http_available() result(res) &
                bind(C, name='ai_http_available_f')
            import :: c_int
            integer(c_int) :: res
        end function

        function c_ai_http_resolve(host, host_len, port, addr) result(res) &
                bind(C, name='ai_http_resolve_f')
            import :: c_int, c_char, c_signed_char
            character(kind=c_char), intent(in) :: host(*)
            integer(c_int), value :: host_len, port
            integer(c_signed_char), intent(inout) :: addr(*)
            integer(c_int) :: res
        end function

        subroutine c_ai_http_begin(handle, addr, body, body_len, &
                                   connect_ms, total_ms) &
                bind(C, name='ai_http_begin_f')
            import :: c_ptr, c_int, c_char, c_signed_char
            type(c_ptr), intent(inout) :: handle
            integer(c_signed_char), intent(in) :: addr(*)
            character(kind=c_char), intent(in) :: body(*)
            integer(c_int), value :: body_len, connect_ms, total_ms
        end subroutine

        subroutine c_ai_http_pump(handle, state, status, nbytes) &
                bind(C, name='ai_http_pump_f')
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int), intent(out) :: state, status, nbytes
        end subroutine

        function c_ai_http_take(handle, out, out_cap) result(res) &
                bind(C, name='ai_http_take_f')
            import :: c_ptr, c_int, c_char
            type(c_ptr), intent(inout) :: handle
            character(kind=c_char), intent(inout) :: out(*)
            integer(c_int), value :: out_cap
            integer(c_int) :: res
        end function

        subroutine c_ai_http_abort(handle) bind(C, name='ai_http_abort_f')
            import :: c_ptr
            type(c_ptr), intent(inout) :: handle
        end subroutine
    end interface

contains

    ! Ignore SIGPIPE. Call once at startup, before any request. Without it a
    ! peer closing mid-send terminates the process and loses unsaved buffers.
    subroutine ai_http_init()
        call c_ai_http_init()
    end subroutine ai_http_init

    ! False on platforms where the transport is compiled out (Windows).
    function ai_http_available() result(res)
        logical :: res
        res = c_ai_http_available() /= 0
    end function ai_http_available

    ! Blocking name resolution. Call when a backend is enabled, never on the
    ! completion path.
    function ai_http_resolve(host, port, addr) result(ok)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        type(ai_http_addr_t), intent(out) :: addr
        logical :: ok

        ok = c_ai_http_resolve(host // c_null_char, int(len_trim(host), c_int), &
                               int(port, c_int), addr%bytes) /= 0
        addr%resolved = ok
    end function ai_http_resolve

    ! Compose a complete HTTP/1.1 request. Connection: close because a fresh
    ! connection per request is cheap next to inference and removes the whole
    ! keep-alive failure surface.
    function ai_http_build_request(method, path, host_header, body) result(req)
        character(len=*), intent(in) :: method, path, host_header, body
        character(len=:), allocatable :: req
        character(len=32) :: len_str

        write(len_str, '(i0)') len(body)
        req = trim(method) // ' ' // trim(path) // ' HTTP/1.1' // achar(13) // achar(10) // &
              'Host: ' // trim(host_header) // achar(13) // achar(10) // &
              'Content-Type: application/json' // achar(13) // achar(10) // &
              'Content-Length: ' // trim(len_str) // achar(13) // achar(10) // &
              'Connection: close' // achar(13) // achar(10) // &
              achar(13) // achar(10) // &
              body
    end function ai_http_build_request

    subroutine ai_http_begin(req, addr, raw_request, connect_ms, total_ms)
        type(ai_http_t), intent(inout) :: req
        type(ai_http_addr_t), intent(in) :: addr
        character(len=*), intent(in) :: raw_request
        integer, intent(in) :: connect_ms, total_ms

        call ai_http_abort(req)
        if (.not. addr%resolved) then
            req%state = AI_HTTP_ERROR
            return
        end if

        call c_ai_http_begin(req%handle, addr%bytes, raw_request, &
                             int(len(raw_request), c_int), &
                             int(connect_ms, c_int), int(total_ms, c_int))
        req%state = AI_HTTP_CONNECTING
        req%status = 0
        req%body_bytes = 0
    end subroutine ai_http_begin

    ! One step of progress. Never blocks.
    subroutine ai_http_pump(req)
        type(ai_http_t), intent(inout) :: req
        integer(c_int) :: st, code, n

        if (.not. c_associated(req%handle)) return
        call c_ai_http_pump(req%handle, st, code, n)
        req%state = int(st)
        req%status = int(code)
        req%body_bytes = int(n)
    end subroutine ai_http_pump

    ! Response body, once state is AI_HTTP_DONE. Empty otherwise.
    function ai_http_take(req) result(body)
        type(ai_http_t), intent(inout) :: req
        character(len=:), allocatable :: body
        character(len=:), allocatable :: buf
        integer(c_int) :: n

        body = ''
        if (.not. c_associated(req%handle)) return
        if (req%state /= AI_HTTP_DONE) return
        if (req%body_bytes <= 0) return

        allocate(character(len=req%body_bytes) :: buf)
        n = c_ai_http_take(req%handle, buf, int(req%body_bytes, c_int))
        if (n > 0) body = buf(1:n)
    end function ai_http_take

    ! Close and free. Closing the socket also cancels generation server-side,
    ! which is why supersession aborts rather than just dropping the reply.
    subroutine ai_http_abort(req)
        type(ai_http_t), intent(inout) :: req

        if (c_associated(req%handle)) call c_ai_http_abort(req%handle)
        req%handle = c_null_ptr
        req%state = AI_HTTP_IDLE
        req%status = 0
        req%body_bytes = 0
    end subroutine ai_http_abort

end module ai_http_module
