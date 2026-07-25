program test_ai_ghost
    ! Model-backed ghost text: the opt-in gate, prefix extension, and
    ! arbitration between the three suggestion sources.
    !
    ! The single most important assertion here is that with ai.enabled false
    ! -- the default -- nothing is resolved, nothing is connected to, and no
    ! request is ever recorded. Opt-in has to be real, not cosmetic.
    use editor_state_module
    use text_buffer_module
    use ai_state_module
    use ai_engine_module
    use ghost_text_module
    use settings_module
    implicit none

    integer :: nfail
    type(editor_state_t) :: editor
    type(buffer_t) :: buf

    nfail = 0
    call use_temp_home()

    call test_disabled_by_default()
    call test_disabled_records_nothing()
    call test_enable_configures()
    call test_prefix_extension()
    call test_arbitration()
    call test_status_line()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All AI ghost tests passed'

contains

    subroutine use_temp_home()
        integer :: ios
        call execute_command_line( &
            'rm -rf /tmp/fac_ai_ghost && mkdir -p /tmp/fac_ai_ghost', &
            wait=.true., exitstat=ios)
        call set_env('HOME', '/tmp/fac_ai_ghost')
        call set_env('XDG_CONFIG_HOME', '/tmp/fac_ai_ghost/.config')
        call settings_reset()
    end subroutine use_temp_home

    subroutine set_env(name, val)
        use iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: name, val
        interface
            function c_setenv(n, v, o) result(r) bind(C, name='setenv')
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: n(*), v(*)
                integer(c_int), value :: o
                integer(c_int) :: r
            end function
        end interface
        integer :: ignored
        ignored = int(c_setenv(name // c_null_char, val // c_null_char, 1_c_int))
    end subroutine set_env

    ! --- opt-in must be real ---
    subroutine test_disabled_by_default()
        type(ai_state_t) :: ai

        call settings_reset()
        call ai_configure(ai)
        call check(.not. ai_is_enabled(ai), &
                   'AI completion is OFF unless explicitly enabled', '')
        call check(.not. ai%addr%resolved, &
                   'and no address is resolved while it is off', '')
        call check(ai%health == AI_HEALTH_UNKNOWN, &
                   'and the backend is never contacted', '')
    end subroutine test_disabled_by_default

    ! Even a trigger from the keystroke path must be a complete no-op.
    subroutine test_disabled_records_nothing()
        type(ai_state_t) :: ai
        logical :: ui

        call settings_reset()
        call ai_configure(ai)
        call ai_note_trigger(ai, 1, 1, 'pre', '', 0_8)
        call check(.not. ai%trigger_pending, &
                   'a trigger while disabled records nothing', '')

        ! And ticking does not reach the network
        call setup('x')
        ui = .false.
        call ai_tick(ai, editor, buf, ui)
        call check(.not. ai%in_flight, 'ticking while disabled sends nothing', '')
        call check(ai%requests_sent == 0, 'no request is counted', '')
        call check(.not. ui, 'and no repaint is requested', '')
    end subroutine test_disabled_records_nothing

    subroutine test_enable_configures()
        type(ai_state_t) :: ai

        call settings_reset()
        call settings_set_logical('ai.enabled', .true.)
        call settings_set_string('ai.host', '127.0.0.1')
        call settings_set_integer('ai.debounce_ms', 200)
        call ai_configure(ai)

        call check(ai_is_enabled(ai), 'enabling turns it on', '')
        call check(ai%debounce_ms == 200, 'debounce comes from settings', '')
        call check(ai%addr%resolved, &
                   'the address is resolved once, at enable time', '')

        ! A trigger is now recorded, but still not sent from here
        call ai_note_trigger(ai, 3, 5, 'pre', ');', 7_8)
        call check(ai%trigger_pending, 'a trigger is recorded when enabled', '')
        call check(ai%trig_line == 3 .and. ai%trig_col == 5, 'anchor captured', '')
        call check(ai%trig_doc_revision == 7, 'document revision captured', '')
        call check(.not. ai%in_flight, &
                   'but nothing is sent from the keystroke path', '')

        call settings_reset()
    end subroutine test_enable_configures

    ! --- the thing that makes it feel instant ---
    subroutine test_prefix_extension()
        type(ghost_text_t) :: g
        logical :: ok

        call ghost_apply_text(g, 'ength(s)', 'str_l', 1, 6, GHOST_SRC_LLM)
        call check(ghost_is_active(g), 'suggestion installed', '')
        call check(ghost_suffix(g) == 'ength(s)', 'suffix is the insertion', ghost_suffix(g))

        ! typing 'e' -- the predicted character -- advances in place
        ok = ghost_extend_prefix(g, 'e')
        call check(ok, 'typing the predicted character extends the ghost', '')
        call check(ghost_suffix(g) == 'ngth(s)', 'the suffix shrinks by one', ghost_suffix(g))
        call check(g%anchor_col == 7, 'the anchor follows the caret', '')
        call check(g%suggestion == 'str_length(s)', &
                   'the full suggestion is unchanged', g%suggestion)

        ! typing something else does not
        ok = ghost_extend_prefix(g, 'z')
        call check(.not. ok, 'typing a different character does not extend', '')

        ! consuming the whole suffix deactivates cleanly
        call ghost_apply_text(g, 'x', 'ab', 1, 3, GHOST_SRC_LLM)
        ok = ghost_extend_prefix(g, 'x')
        call check(.not. ok .and. .not. ghost_is_active(g), &
                   'consuming the last character clears the ghost', '')
    end subroutine test_prefix_extension

    ! --- three sources must not fight ---
    subroutine test_arbitration()
        ! nothing showing: anything may take over
        call check(ghost_may_replace(GHOST_SRC_NONE, GHOST_SRC_WORDS, 'abc'), &
                   'word scan fills an empty slot', '')
        call check(ghost_may_replace(GHOST_SRC_NONE, GHOST_SRC_LLM, 'abc'), &
                   'the model fills an empty slot', '')

        ! higher rank wins
        call check(ghost_may_replace(GHOST_SRC_WORDS, GHOST_SRC_LSP, 'abc'), &
                   'LSP outranks the word scan', '')
        call check(ghost_may_replace(GHOST_SRC_WORDS, GHOST_SRC_LLM, 'abc'), &
                   'the model outranks the word scan', '')

        ! lower rank never displaces higher
        call check(.not. ghost_may_replace(GHOST_SRC_LSP, GHOST_SRC_WORDS, 'abc'), &
                   'the word scan never displaces LSP', '')
        call check(.not. ghost_may_replace(GHOST_SRC_LLM, GHOST_SRC_WORDS, 'abc'), &
                   'the word scan never displaces the model', '')

        ! THE interesting rule: a model guess at a NAME does not displace an
        ! LSP name, which is type-correct and cannot be hallucinated
        call check(.not. ghost_may_replace(GHOST_SRC_LSP, GHOST_SRC_LLM, 'my_symbol'), &
                   'a bare identifier from the model does not displace LSP', '')
        call check(.not. ghost_may_replace(GHOST_SRC_LSP, GHOST_SRC_LLM, 'foo_bar99'), &
                   'nor does one with digits and underscores', '')

        ! ...but real code prediction does, because LSP cannot offer that
        call check(ghost_may_replace(GHOST_SRC_LSP, GHOST_SRC_LLM, 'foo(a, b);'), &
                   'a code fragment from the model does displace LSP', '')
        call check(ghost_may_replace(GHOST_SRC_LSP, GHOST_SRC_LLM, 'a + b'), &
                   'so does an expression with spaces', '')

        call check(.not. ghost_may_replace(GHOST_SRC_NONE, GHOST_SRC_LLM, ''), &
                   'empty text never replaces anything', '')
    end subroutine test_arbitration

    subroutine test_status_line()
        type(ai_state_t) :: ai
        character(len=:), allocatable :: line

        call settings_reset()
        call ai_configure(ai)
        line = ai_status_line(ai)
        call check(index(line, 'off') > 0, 'status says off when disabled', line)

        call settings_set_logical('ai.enabled', .true.)
        call ai_configure(ai)
        line = ai_status_line(ai)
        call check(index(line, 'qwen') > 0, 'status names the model', line)
        call check(index(line, '127.0.0.1') > 0, 'status names the host', line)
        call settings_reset()
    end subroutine test_status_line

    subroutine setup(text)
        character(len=*), intent(in) :: text
        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_buffer(buf)
        if (len(text) > 0) call buffer_insert(buf, 1, text)
        call init_editor(editor)
        call create_tab(editor, 'ai_test.c')
    end subroutine setup

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // trim(got) // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ai_ghost
