program test_ai_live
    ! End-to-end: transport -> JSON -> sanitizer, against a real ollama.
    !
    ! Skips cleanly when ollama is not running, so it is safe in CI. When it
    ! does run it is the only test that proves the three Sprint 1-2 modules
    ! compose, and the only one that exercises real model output rather than
    ! fixtures written by hand.
    use ai_http_module
    use ai_json_module
    use completion_sanitize_module
    implicit none

    integer :: nfail
    type(ai_http_addr_t) :: addr
    logical :: have_server

    nfail = 0
    call ai_http_init()

    if (.not. ai_http_resolve('127.0.0.1', 11434, addr)) then
        print '(a)', 'SKIP: cannot resolve loopback'
        stop 0
    end if

    have_server = probe_tags()
    if (.not. have_server) then
        print '(a)', 'SKIP: no ollama on 127.0.0.1:11434'
        stop 0
    end if

    call test_fim_c()
    call test_fim_python()
    call test_comment_driven()
    call test_chat_model_is_caught()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All live AI pipeline tests passed'

contains

    ! Run one request to completion and return the raw body.
    function fetch(method, path, body, ok) result(resp)
        character(len=*), intent(in) :: method, path, body
        logical, intent(out) :: ok
        character(len=:), allocatable :: resp
        type(ai_http_t) :: req
        integer(8) :: t0, t1, rate

        resp = ''
        ok = .false.
        call ai_http_begin(req, addr, &
            ai_http_build_request(method, path, '127.0.0.1:11434', body), &
            1000, 60000)

        call system_clock(t0, rate)
        do
            call ai_http_pump(req)
            if (req%state == AI_HTTP_DONE .or. req%state == AI_HTTP_ERROR) exit
            call system_clock(t1)
            if (real(t1 - t0) / real(rate) > 70.0) exit
        end do

        if (req%state == AI_HTTP_DONE .and. req%status == 200) then
            resp = ai_http_take(req)
            ok = len(resp) > 0
        end if
        call ai_http_abort(req)
    end function fetch

    function probe_tags() result(res)
        logical :: res
        character(len=:), allocatable :: body
        body = fetch('GET', '/api/tags', '', res)
    end function probe_tags

    ! Build a FIM request. suffix is ALWAYS sent: without it ollama applies
    ! the model's chat template and returns prose.
    function generate(model, prefix, suffix, npredict) result(body)
        character(len=*), intent(in) :: model, prefix, suffix
        integer, intent(in) :: npredict
        character(len=:), allocatable :: body
        character(len=16) :: np

        write(np, '(i0)') npredict
        body = '{"model":"' // model // '",' // &
               '"prompt":"' // ai_json_escape(prefix) // '",' // &
               '"suffix":"' // ai_json_escape(suffix) // '",' // &
               '"stream":false,"keep_alive":"5m",' // &
               '"options":{"num_predict":' // trim(np) // ',"temperature":0.1}}'
    end function generate

    ! Ask a model, decode, sanitize. Reports the sanitizer verdict.
    subroutine complete(model, prefix, suffix, line_after, max_lines, out, code, ok)
        character(len=*), intent(in) :: model, prefix, suffix, line_after
        integer, intent(in) :: max_lines
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: code
        logical, intent(out) :: ok
        character(len=:), allocatable :: resp, raw

        out = ''
        code = SAN_EMPTY
        resp = fetch('POST', '/api/generate', generate(model, prefix, suffix, 48), ok)
        if (.not. ok) return
        call ai_json_get_string(resp, 'response', raw, ok)
        if (.not. ok) return
        call sanitize_completion(raw, line_after, max_lines, out, code)
    end subroutine complete

    subroutine test_fim_c()
        character(len=:), allocatable :: out
        integer :: code
        logical :: ok

        call complete('qwen2.5-coder:1.5b-base', &
            '#include <stdio.h>' // achar(10) // achar(10) // &
            'int str_length(const char *s) {' // achar(10) // '    int n = 0;' // achar(10), &
            '    return n;' // achar(10) // '}' // achar(10), &
            '', 4, out, code, ok)

        if (.not. ok) then
            call check(.false., 'C FIM request succeeded', 'request failed')
            return
        end if
        call check(code == SAN_OK, 'a real C completion passes the sanitizer', &
                   sanitize_reason(code))
        call check(len(out) > 0, 'and is non-empty', out)
        print '(a)', '      C  -> ' // shorten(out)
    end subroutine test_fim_c

    subroutine test_fim_python()
        character(len=:), allocatable :: out
        integer :: code
        logical :: ok

        call complete('qwen2.5-coder:1.5b-base', &
            'def total(values):' // achar(10) // '    s = 0' // achar(10) // &
            '    for v in values:' // achar(10), &
            '    return s' // achar(10), '', 3, out, code, ok)

        if (.not. ok) return
        call check(code == SAN_OK, 'a real Python completion passes', &
                   sanitize_reason(code))
        print '(a)', '      py -> ' // shorten(out)
    end subroutine test_fim_python

    ! The capability the whole feature is for: the model reads the comment and
    ! writes what it describes.
    subroutine test_comment_driven()
        character(len=:), allocatable :: out
        integer :: code
        logical :: ok

        call complete('qwen2.5-coder:1.5b-base', &
            '/* Return the larger of a and b. */' // achar(10) // &
            'int max_of(int a, int b) {' // achar(10), &
            '}' // achar(10), '', 3, out, code, ok)

        if (.not. ok) return
        call check(code == SAN_OK, 'a comment-driven completion passes', &
                   sanitize_reason(code))
        ! It should mention both parameters -- weak, but it distinguishes a
        ! real completion from filler.
        call check(index(out, 'a') > 0 .and. index(out, 'b') > 0, &
                   'and uses the parameters the comment describes', out)
        print '(a)', '      cm -> ' // shorten(out)
    end subroutine test_comment_driven

    ! qwen3.5:9b is a vision+thinking chat model with no FIM template. It is
    ! the only model that was on this machine before the feature existed, so
    ! it is exactly what a user would hit first. Its output must not survive.
    subroutine test_chat_model_is_caught()
        character(len=:), allocatable :: resp, raw, out
        logical :: ok
        integer :: code

        resp = fetch('POST', '/api/show', '{"model":"qwen3.5:9b"}', ok)
        if (.not. ok) then
            print '(a)', 'SKIP: qwen3.5:9b not installed'
            return
        end if

        call check(.not. ai_json_array_has_string(resp, 'capabilities', 'insert'), &
                   'a chat model is detected as lacking FIM', '')

        ! And if it were used anyway, prompt-only output must be rejected
        resp = fetch('POST', '/api/generate', &
            '{"model":"qwen3.5:9b","prompt":"int add(int a, int b) {\n    ",' // &
            '"stream":false,"keep_alive":"1m","options":{"num_predict":24}}', ok)
        if (.not. ok) return
        call ai_json_get_string(resp, 'response', raw, ok)
        if (.not. ok) return

        call sanitize_completion(raw, '', 1, out, code)
        call check(code /= SAN_OK, &
                   'chat-template output does not reach the buffer', &
                   sanitize_reason(code) // ' | raw: ' // shorten(raw))
        print '(a)', '      chat rejected as: ' // sanitize_reason(code)
    end subroutine test_chat_model_is_caught

    function shorten(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i
        t = ''
        do i = 1, min(len(s), 60)
            if (iachar(s(i:i)) == 10) then
                t = t // '\n'
            else if (iachar(s(i:i)) < 32) then
                t = t // '?'
            else
                t = t // s(i:i)
            end if
        end do
    end function shorten

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (' // got // ')'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ai_live
