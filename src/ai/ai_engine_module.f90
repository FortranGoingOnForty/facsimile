! Drives model-backed inline completion.
!
! The keystroke path only RECORDS intent; ai_tick decides whether to send and
! pumps whatever is in flight. That inversion matters twice over: it keeps
! network work off the typing path, and it means a burst of keystrokes costs
! one request instead of one per character.
!
! No callback plumbing. The LSP trampoline workaround exists because LSP
! callbacks fire deep inside process_server_messages with no access to the
! editor; ai_tick is called from the main loop where editor and buffer are
! both in scope.
module ai_engine_module
    use iso_fortran_env, only: int64
    use editor_state_module, only: editor_state_t
    use text_buffer_module, only: buffer_t
    use ai_state_module
    use ai_http_module
    use ai_json_module, only: ai_json_get_string
    use ollama_client_module
    use completion_context_module
    use completion_prompt_module
    use completion_sanitize_module
    use ghost_text_module, only: ghost_is_active, ghost_apply_text, ghost_may_replace, &
                                 ghost_clear, ghost_apply_block, &
                                 GHOST_SRC_LLM, GHOST_SRC_NONE
    use settings_module
    implicit none
    private

    public :: ai_configure, ai_note_trigger, ai_tick, ai_cancel
    public :: ai_is_enabled, ai_set_enabled, ai_status_line

    integer, parameter :: BUCKET_LIMIT_REQUESTS = 4
    integer, parameter :: BUCKET_WINDOW_MS = 2000
    integer, parameter :: CONNECT_TIMEOUT_MS = 500
    integer, parameter :: TOTAL_TIMEOUT_MS = 4000

contains

    ! Read settings and, only if enabled, resolve the backend. Resolution is
    ! blocking (getaddrinfo has no async form), which is why it happens here at
    ! enable time and never on the completion path.
    subroutine ai_configure(ai)
        type(ai_state_t), intent(inout) :: ai
        logical :: ok

        ai%enabled = settings_get_logical('ai.enabled', .false.)
        ai%host = settings_get_string('ai.host', '127.0.0.1')
        ai%port = settings_get_integer('ai.port', 11434)
        ai%model = settings_get_string('ai.model', 'qwen2.5-coder:1.5b-base')
        ai%debounce_ms = settings_get_integer('ai.debounce_ms', 150)
        ai%num_predict = settings_get_integer('ai.num_predict', 24)
        ai%temperature_x100 = settings_get_integer('ai.temperature_x100', 15)
        ai%prefix_bytes = settings_get_integer('ai.prefix_bytes', DEFAULT_PREFIX_BYTES)
        ai%suffix_bytes = settings_get_integer('ai.suffix_bytes', DEFAULT_SUFFIX_BYTES)
        ai%max_block_lines = min(8, max(1, settings_get_integer('ai.max_block_lines', 4)))
        ai%include_header = settings_get_logical('ai.context.file_header', .true.)
        ai%include_symbols = settings_get_logical('ai.context.symbols', .true.)
        ai%configured = .true.

        if (.not. ai%enabled) then
            ai%health = AI_HEALTH_UNKNOWN
            return
        end if

        call ai_http_init()
        if (.not. ai_http_available()) then
            ai%health = AI_HEALTH_DOWN
            ai%last_error = 'transport unavailable on this platform'
            return
        end if

        ok = ai_http_resolve(ai%host, ai%port, ai%addr)
        if (.not. ok) then
            ai%health = AI_HEALTH_DOWN
            ai%last_error = 'cannot resolve ' // ai%host
            return
        end if
        ai%health = AI_HEALTH_UNKNOWN     ! proven by the first successful reply
    end subroutine ai_configure

    function ai_is_enabled(ai) result(res)
        type(ai_state_t), intent(in) :: ai
        logical :: res
        res = ai%enabled
    end function ai_is_enabled

    subroutine ai_set_enabled(ai, on)
        type(ai_state_t), intent(inout) :: ai
        logical, intent(in) :: on
        logical :: saved

        call settings_set_logical('ai.enabled', on)
        call settings_save(saved)
        call ai_cancel(ai)
        call ai_configure(ai)
    end subroutine ai_set_enabled

    ! Record that a completion might be wanted here. Cheap: no allocation
    ! beyond two short strings, no syscall, no network.
    subroutine ai_note_trigger(ai, line, col, prefix, line_after, doc_revision, filename)
        type(ai_state_t), intent(inout) :: ai
        integer, intent(in) :: line, col
        character(len=*), intent(in) :: prefix, line_after
        integer(int64), intent(in) :: doc_revision
        character(len=*), intent(in), optional :: filename

        if (.not. ai%enabled) return
        if (ai%health == AI_HEALTH_DOWN .or. ai%health == AI_HEALTH_NO_FIM) return

        ai%trigger_pending = .true.
        ai%trigger_ms = now_ms()
        ai%trig_line = line
        ai%trig_col = col
        ai%trig_prefix = prefix
        ai%trig_line_after = line_after
        ai%trig_doc_revision = doc_revision

        ! A block is only ever offered with the caret past the last character
        ! of its line. Mid-line there is no coherent place for the rest of the
        ! line to go, and it keeps the renderer's "open the line up" trick and
        ! the block row loop from ever interacting.
        ai%trig_at_eol = len_trim(line_after) == 0

        ! Language detection drives the stop sequences and the comment token
        ! used for the header and symbol digest, so the filename has to come
        ! along with the trigger.
        if (present(filename)) then
            ai%filename = filename
        else
            ai%filename = ''
        end if
    end subroutine ai_note_trigger

    subroutine ai_cancel(ai)
        type(ai_state_t), intent(inout) :: ai

        if (ai%in_flight) call ai_http_abort(ai%req)
        ai%in_flight = .false.
        ai%trigger_pending = .false.
    end subroutine ai_cancel

    ! Called once per main-loop iteration. Never blocks.
    subroutine ai_tick(ai, editor, buffer, ui_changed)
        type(ai_state_t), intent(inout) :: ai
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        logical, intent(inout) :: ui_changed

        if (.not. ai%enabled) return

        if (ai%in_flight) then
            call pump_in_flight(ai, editor, ui_changed)
            return
        end if

        if (ai%trigger_pending) call maybe_send(ai, editor, buffer)
    end subroutine ai_tick

    subroutine maybe_send(ai, editor, buffer)
        use terminal_io_module, only: terminal_input_available
        type(ai_state_t), intent(inout) :: ai
        type(editor_state_t), intent(inout) :: editor
        type(buffer_t), intent(in) :: buffer
        character(len=:), allocatable :: prefix, suffix, body
        type(prompt_options_t) :: popts
        integer :: npredict
        integer(int64) :: t

        t = now_ms()
        if (t - ai%trigger_ms < int(ai%debounce_ms, int64)) return

        ! A keystroke is already waiting: whatever we would ask about is
        ! stale before the request leaves.
        if (terminal_input_available()) return

        ! The caret must still be where the trigger was recorded
        if (size(editor%cursors) /= 1) then
            ai%trigger_pending = .false.
            return
        end if
        if (editor%cursors(editor%active_cursor)%line /= ai%trig_line .or. &
            editor%cursors(editor%active_cursor)%column /= ai%trig_col) then
            ai%trigger_pending = .false.
            return
        end if

        if (.not. take_token(ai, t)) then
            ai%trigger_pending = .false.
            return
        end if

        popts%prefix_bytes = ai%prefix_bytes
        popts%suffix_bytes = ai%suffix_bytes
        popts%include_header = ai%include_header
        popts%include_symbols = ai%include_symbols
        call build_completion_prompt(buffer, ai%filename, ai%trig_line, ai%trig_col, &
                                     popts, '', prefix, suffix)

        ! Ask for more tokens when a block is possible; latency is dominated
        ! by num_predict, so a single-line request stays deliberately tight.
        if (ai%trig_at_eol .and. ai%max_block_lines > 1) then
            npredict = ai%num_predict * 4
        else
            npredict = ai%num_predict
        end if

        body = ollama_generate_body(ai%model, prefix, suffix, npredict, &
                                    ai%temperature_x100, '10m', &
                                    completion_stop_json(ai%filename))

        call ai_http_begin(ai%req, ai%addr, &
            ai_http_build_request('POST', '/api/generate', &
                                  ai%host // ':' // int_str(ai%port), body), &
            CONNECT_TIMEOUT_MS, TOTAL_TIMEOUT_MS)

        ai%in_flight = .true.
        ai%trigger_pending = .false.
        ai%generation = ai%generation + 1
        ai%flight_generation = ai%generation
        ai%flight_line = ai%trig_line
        ai%flight_col = ai%trig_col
        ai%flight_prefix = ai%trig_prefix
        ai%flight_line_after = ai%trig_line_after
        ai%flight_doc_revision = ai%trig_doc_revision
        ai%flight_at_eol = ai%trig_at_eol
        ai%requests_sent = ai%requests_sent + 1
        ai%request_started_ms = t
    end subroutine maybe_send

    subroutine pump_in_flight(ai, editor, ui_changed)
        type(ai_state_t), intent(inout) :: ai
        type(editor_state_t), intent(inout) :: editor
        logical, intent(inout) :: ui_changed
        character(len=:), allocatable :: body, raw, reason, text
        logical :: ok
        integer :: code

        call ai_http_pump(ai%req)

        if (ai%req%state /= AI_HTTP_DONE .and. ai%req%state /= AI_HTTP_ERROR) return

        ai%last_latency_ms = int(now_ms() - ai%request_started_ms)

        if (ai%req%state == AI_HTTP_ERROR .or. ai%req%status /= 200) then
            call note_failure(ai)
            call ai_http_abort(ai%req)
            ai%in_flight = .false.
            return
        end if

        body = ai_http_take(ai%req)
        call ai_http_abort(ai%req)
        ai%in_flight = .false.

        ai%consecutive_failures = 0
        ai%health = AI_HEALTH_OK

        call ollama_read_completion(body, raw, reason, ok)
        if (.not. ok) then
            ai%last_reject_reason = 'no response field'
            ai%rejected_count = ai%rejected_count + 1
            return
        end if

        if (ai%flight_at_eol .and. ai%max_block_lines > 1) then
            call sanitize_completion(raw, ai%flight_line_after, ai%max_block_lines, text, code)
        else
            call sanitize_completion(raw, ai%flight_line_after, 1, text, code)
        end if
        if (code /= SAN_OK) then
            ai%last_reject_reason = sanitize_reason(code)
            ai%rejected_count = ai%rejected_count + 1
            return
        end if

        call apply_to_ghost(ai, editor, text, ui_changed)
    end subroutine pump_in_flight

    ! Everything here re-checks live state. A reply is ~300ms old by the time
    ! it lands; the user may have typed, moved, undone, switched tabs, or
    ! opened a panel since.
    subroutine apply_to_ghost(ai, editor, text, ui_changed)
        type(ai_state_t), intent(inout) :: ai
        type(editor_state_t), intent(inout) :: editor
        character(len=*), intent(in) :: text
        logical, intent(inout) :: ui_changed

        if (ai%flight_generation /= ai%generation) return
        if (size(editor%cursors) /= 1) return
        if (editor%cursors(editor%active_cursor)%has_selection) return
        if (editor%cursors(editor%active_cursor)%line /= ai%flight_line) return
        if (editor%cursors(editor%active_cursor)%column /= ai%flight_col) return
        if (current_doc_rev(editor) /= ai%flight_doc_revision) return

        if (.not. ghost_may_replace(editor%ghost%source, GHOST_SRC_LLM, text)) return

        if (index(text, achar(10)) > 0) then
            call ghost_apply_block(editor%ghost, text, ai%flight_prefix, &
                                   ai%flight_line, ai%flight_col, GHOST_SRC_LLM)
        else
            call ghost_apply_text(editor%ghost, text, ai%flight_prefix, &
                                  ai%flight_line, ai%flight_col, GHOST_SRC_LLM)
        end if
        ai%accepted_count = ai%accepted_count + 1
        ui_changed = .true.
    end subroutine apply_to_ghost

    function current_doc_rev(editor) result(rev)
        type(editor_state_t), intent(in) :: editor
        integer(int64) :: rev

        rev = -1
        if (editor%active_tab_index < 1 .or. &
            editor%active_tab_index > size(editor%tabs)) return
        rev = editor%tabs(editor%active_tab_index)%doc_revision
    end function current_doc_rev

    subroutine note_failure(ai)
        type(ai_state_t), intent(inout) :: ai

        ai%consecutive_failures = ai%consecutive_failures + 1
        if (ai%consecutive_failures >= 3) then
            ai%health = AI_HEALTH_DOWN
            ai%last_error = 'backend unreachable'
        end if
    end subroutine note_failure

    ! A hard cap independent of the debounce, so a bug in the trigger logic
    ! cannot turn into a flood against a shared host.
    function take_token(ai, t) result(ok)
        type(ai_state_t), intent(inout) :: ai
        integer(int64), intent(in) :: t
        logical :: ok

        if (t - ai%bucket_window_ms > int(BUCKET_WINDOW_MS, int64)) then
            ai%bucket_window_ms = t
            ai%bucket_count = 0
        end if
        ok = ai%bucket_count < BUCKET_LIMIT_REQUESTS
        if (ok) ai%bucket_count = ai%bucket_count + 1
    end function take_token

    function ai_status_line(ai) result(text)
        type(ai_state_t), intent(in) :: ai
        character(len=:), allocatable :: text

        if (.not. ai%enabled) then
            text = 'AI completion: off'
            return
        end if

        text = 'AI: ' // ai%model // ' @ ' // ai%host // ':' // int_str(ai%port)
        select case(ai%health)
        case(AI_HEALTH_OK)
            text = text // ' | ok, ' // int_str(ai%last_latency_ms) // 'ms'
        case(AI_HEALTH_DOWN)
            text = text // ' | DOWN'
            if (allocated(ai%last_error)) text = text // ': ' // ai%last_error
        case(AI_HEALTH_NO_FIM)
            text = text // ' | model has no FIM support'
        case default
            text = text // ' | not yet contacted'
        end select

        text = text // ' | sent ' // int_str(ai%requests_sent) // &
               ', shown ' // int_str(ai%accepted_count) // &
               ', rejected ' // int_str(ai%rejected_count)
        if (allocated(ai%last_reject_reason)) then
            if (len(ai%last_reject_reason) > 0) &
                text = text // ' (' // ai%last_reject_reason // ')'
        end if
    end function ai_status_line

    function int_str(v) result(t)
        integer, intent(in) :: v
        character(len=:), allocatable :: t
        character(len=16) :: b
        write(b, '(i0)') v
        t = trim(b)
    end function int_str

    function now_ms() result(ms)
        integer(int64) :: ms, c, r
        call system_clock(c, r)
        if (r <= 0) then
            ms = 0
        else
            ms = c * 1000_int64 / r
        end if
    end function now_ms

end module ai_engine_module
