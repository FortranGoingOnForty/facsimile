program test_ghost_block
    ! Multi-line block suggestions and word-at-a-time acceptance.
    !
    ! The block path had to be added without disturbing the two existing ghost
    ! sources, so block state sits alongside the single-line fields rather than
    ! replacing them: block_lines == 0 means "not a block", which is what every
    ! pre-existing check already assumes.
    use editor_state_module
    use text_buffer_module
    use ghost_text_module
    use command_handler_module, only: handle_key_command, init_command_handler, &
                                      save_initial_state_for_undo
    implicit none

    character, parameter :: NL = achar(10)
    integer :: nfail
    type(editor_state_t) :: editor
    type(buffer_t) :: buf
    logical :: quit

    nfail = 0
    call init_command_handler()

    call test_block_state()
    call test_block_lines()
    call test_single_line_unaffected()
    call test_block_accept()
    call test_block_undo_is_one_step()
    call test_word_accept()
    call test_line_accept()

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All ghost-block tests passed'

contains

    subroutine test_block_state()
        type(ghost_text_t) :: g

        call ghost_apply_block(g, 'a();' // NL // 'b();' // NL // 'c();', '', 1, 1, &
                               GHOST_SRC_LLM)
        call check(ghost_is_active(g), 'a block is an active suggestion', '')
        call check(ghost_is_block(g), 'and reports itself as a block', '')
        call check(g%block_lines == 3, 'with the right line count', int_str(g%block_lines))
        call check(ghost_insert_text(g) == 'a();' // NL // 'b();' // NL // 'c();', &
                   'the insertion is the whole block', ghost_insert_text(g))

        ! row 1 still lives in suggestion/prefix, so the renderer's existing
        ! first-row path and ghost_suffix keep working untouched
        call check(ghost_suffix(g) == 'a();', &
                   'the first row is still reachable as a plain suffix', ghost_suffix(g))

        call ghost_clear(g)
        call check(.not. ghost_is_block(g), 'clearing drops the block', '')
        call check(g%block_lines == 0, 'and resets the line count', '')
    end subroutine test_block_state

    subroutine test_block_lines()
        type(ghost_text_t) :: g

        call ghost_apply_block(g, 'one' // NL // 'two' // NL // 'three', '', 1, 1, &
                               GHOST_SRC_LLM)
        call check(ghost_block_line(g, 1) == 'one', 'line 1', ghost_block_line(g, 1))
        call check(ghost_block_line(g, 2) == 'two', 'line 2', ghost_block_line(g, 2))
        call check(ghost_block_line(g, 3) == 'three', 'line 3 (no trailing newline)', &
                   ghost_block_line(g, 3))
        call check(ghost_block_line(g, 4) == '', 'past the end yields nothing', '')
        call check(ghost_block_line(g, 0) == '', 'before the start yields nothing', '')
    end subroutine test_block_lines

    ! The two pre-existing sources must behave exactly as before.
    subroutine test_single_line_unaffected()
        type(ghost_text_t) :: g

        call ghost_apply_text(g, 'ounter', 'c', 1, 2, GHOST_SRC_WORDS)
        call check(.not. ghost_is_block(g), 'a single-line suggestion is not a block', '')
        call check(g%block_lines == 0, 'block_lines stays zero', '')
        call check(ghost_insert_text(g) == 'ounter', &
                   'insert text is the plain suffix', ghost_insert_text(g))
        call check(ghost_suffix(g) == 'ounter', 'ghost_suffix unchanged', ghost_suffix(g))
    end subroutine test_single_line_unaffected

    subroutine test_block_accept()
        call setup('int f(void) {' // NL // '' // NL // '}')
        call cursor_at(2, 1)
        call ghost_apply_block(editor%ghost, '    int n = 0;' // NL // '    return n;', &
                               '', 2, 1, GHOST_SRC_LLM)
        call key('tab')
        call check_text('int f(void) {' // NL // '    int n = 0;' // NL // &
                        '    return n;' // NL // '}', 'the whole block is inserted')
        call check(editor%cursors(1)%line == 3, 'the caret lands on the last block line', &
                   int_str(editor%cursors(1)%line))
        call check(editor%cursors(1)%column == 14, 'at the end of it', &
                   int_str(editor%cursors(1)%column))
        call check(.not. ghost_is_active(editor%ghost), 'the ghost is consumed', '')
    end subroutine test_block_accept

    ! A block accepted straight after typing used to coalesce with that typing
    ! run, so Ctrl-Z could not remove it on its own.
    subroutine test_block_undo_is_one_step()
        call setup('start')
        call cursor_at(1, 6)
        call save_initial_state_for_undo(buf, editor)

        call key('X')                       ! an edit, so last_action_was_edit is set
        call ghost_apply_block(editor%ghost, 'a' // NL // 'b', '', 1, 7, GHOST_SRC_LLM)
        call key('tab')
        call check_text('startXa' // NL // 'b', 'block inserted after typing')

        call key('ctrl-z')
        call check_text('startX', 'one undo removes the whole block, not part of it')
    end subroutine test_block_undo_is_one_step

    subroutine test_word_accept()
        type(ghost_text_t) :: g
        character(len=:), allocatable :: w

        ! a plain identifier hands over the whole word
        call ghost_apply_text(g, 'ength(s)', 'str_l', 1, 6, GHOST_SRC_LLM)
        w = ghost_take_word(g)
        call check(w == 'ength', 'one word is taken', w)
        call check(ghost_suffix(g) == '(s)', 'the rest stays ghosted', ghost_suffix(g))
        call check(g%anchor_col == 11, 'the anchor moves with it', int_str(g%anchor_col))

        ! leading punctuation is its own step, so the user is not forced past
        ! a paren they may not want
        w = ghost_take_word(g)
        call check(w == '(', 'punctuation is handed over separately', w)
        call check(ghost_suffix(g) == 's)', 'and the rest still stays', ghost_suffix(g))

        ! a block does not accept by word
        call ghost_apply_block(g, 'a' // NL // 'b', '', 1, 1, GHOST_SRC_LLM)
        w = ghost_take_word(g)
        call check(len(w) == 0, 'a block is not accepted word by word', w)

        ! and the editor path inserts it
        call setup('str_l')
        call cursor_at(1, 6)
        call ghost_apply_text(editor%ghost, 'ength(s)', 'str_l', 1, 6, GHOST_SRC_LLM)
        call key('ctrl-right')
        call check_text('str_length', 'ctrl-right inserts one word')
        call check(ghost_is_active(editor%ghost), 'and the remainder is still offered', '')
    end subroutine test_word_accept

    ! Accept a block one line at a time, keeping the rest offered.
    subroutine test_line_accept()
        type(ghost_text_t) :: g
        character(len=:), allocatable :: t
        logical :: ok

        call ghost_apply_block(g, 'aa();' // NL // 'bb();' // NL // 'cc();', '', 1, 1, &
                               GHOST_SRC_LLM)
        call ghost_take_line(g, t, ok)
        call check(ok .and. t == 'aa();' // NL, &
                   'the first line comes back with its newline', t)
        call check(g%block_lines == 2, 'two lines remain', int_str(g%block_lines))
        call check(ghost_is_block(g), 'and it is still a block', '')
        call check(g%prefix == '', &
                   'the prefix resets: the caret is on a fresh line', g%prefix)

        call ghost_take_line(g, t, ok)
        call check(ok .and. t == 'bb();' // NL, 'the second line follows', t)
        call check(.not. ghost_is_block(g), &
                   'one line left stops being a block', '')
        call check(ghost_suffix(g) == 'cc();', &
                   'and becomes an ordinary single-line suggestion', ghost_suffix(g))

        ! a single-line suggestion has no line to take
        call ghost_take_line(g, t, ok)
        call check(.not. ok, 'a single-line suggestion has no line to take', '')

        ! ...and through the editor, with the caret re-anchored each time
        call setup('int f(void) {' // NL // '' // NL // '}')
        call cursor_at(2, 1)
        call ghost_apply_block(editor%ghost, '    int n = 0;' // NL // '    return n;', &
                               '', 2, 1, GHOST_SRC_LLM)
        call key('alt-right')
        call check_text('int f(void) {' // NL // '    int n = 0;' // NL // '' // NL // '}', &
                        'alt-right inserts just the first line')
        call check(ghost_is_active(editor%ghost), &
                   'the remainder is still offered', '')
        call check(editor%ghost%anchor_line == 3, &
                   'and is re-anchored on the new line', &
                   int_str(editor%ghost%anchor_line))

        call key('tab')
        call check_text('int f(void) {' // NL // '    int n = 0;' // NL // &
                        '    return n;' // NL // '}', &
                        'Tab then takes the rest')
    end subroutine test_line_accept

    ! ------------------------------------------------------------------

    subroutine setup(text)
        character(len=*), intent(in) :: text
        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_command_handler()
        call init_buffer(buf)
        if (len(text) > 0) call buffer_insert(buf, 1, text)
        buf%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'block_test.c')
    end subroutine setup

    subroutine cursor_at(l, c)
        integer, intent(in) :: l, c
        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(1))
        editor%cursors(1)%line = l
        editor%cursors(1)%column = c
        editor%active_cursor = 1
    end subroutine cursor_at

    subroutine key(k)
        character(len=*), intent(in) :: k
        quit = .false.
        call handle_key_command(k, editor, buf, quit)
    end subroutine key

    subroutine check_text(expected, name)
        character(len=*), intent(in) :: expected, name
        character(len=:), allocatable :: got
        got = buffer_to_string(buf)
        call check(got == expected, name, got)
    end subroutine check_text

    function int_str(v) result(t)
        integer, intent(in) :: v
        character(len=16) :: t
        write(t, '(i0)') v
    end function int_str

    subroutine check(cond, name, got)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name, got
        character(len=:), allocatable :: shown
        integer :: i

        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            shown = ''
            do i = 1, min(len(got), 80)
                if (iachar(got(i:i)) == 10) then
                    shown = shown // '\n'
                else
                    shown = shown // got(i:i)
                end if
            end do
            print '(a)', 'FAIL: ' // name // ' (got: "' // shown // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ghost_block
