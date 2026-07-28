program test_modifier_prefix
    ! How a terminal's modifier parameter becomes a key name.
    !
    ! The parameter is 1 plus a bitmask -- shift 1, alt 2, ctrl 4, super 8,
    ! and above that hyper, meta and the lock states. It used to be a case
    ! table over 2..9, which could not express anything above 9 and, worse,
    ! fell through to an EMPTY prefix. The caller then appended the terminator
    ! and produced a bare arrow, so super+ctrl+left silently moved the caret
    ! instead of doing nothing.
    !
    ! The 2..7 strings are pinned by literal on purpose: every existing
    ! binding depends on them, and nothing in this codebase spells a chord
    ! 'ctrl-alt-'. A refactor that renamed them would break bindings quietly.
    use input_handler_module, only: modifier_prefix
    implicit none

    integer :: nfail

    nfail = 0

    call test_the_existing_chords_are_unchanged()
    call test_the_combinations_that_used_to_vanish()
    call test_super()
    call test_lock_states_do_not_break_a_chord()
    call test_degenerate_input()

    if (nfail == 0) then
        print '(a)', 'test_modifier_prefix: all passed'
    else
        print '(a,i0,a)', 'test_modifier_prefix: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine expect(modifier, want, label)
        integer, intent(in) :: modifier
        character(len=*), intent(in) :: want, label
        character(len=:), allocatable :: got

        got = modifier_prefix(modifier)
        if (got /= want) then
            print '(a)', '  FAIL: ' // label
            print '(a,i0,a)', '        modifier ', modifier, &
                              ' -> "' // got // '" (wanted "' // want // '")'
            nfail = nfail + 1
        end if
    end subroutine expect

    ! Byte-for-byte what the old table produced for 2..7. Every binding in the
    ! editor is written against these exact strings.
    subroutine test_the_existing_chords_are_unchanged()
        call expect(2, 'shift-',      'shift')
        call expect(3, 'alt-',        'alt')
        call expect(4, 'alt-shift-',  'alt+shift')
        call expect(5, 'ctrl-',       'ctrl')
        call expect(6, 'ctrl-shift-', 'ctrl+shift')
        call expect(7, 'alt-ctrl-',   'alt+ctrl -- NOT ctrl-alt-')
    end subroutine test_the_existing_chords_are_unchanged

    subroutine test_the_combinations_that_used_to_vanish()
        ! 8 is ctrl+alt+shift. The old table called it 'alt-shift-', which
        ! collided with 4, so ctrl+alt+shift+left silently did word-selection.
        call expect(8, 'alt-ctrl-shift-', 'ctrl+alt+shift no longer collides with alt+shift')

        ! 10 through 16 all produced an empty prefix, and therefore a bare key.
        call expect(10, 'super-shift-',          'super+shift')
        call expect(11, 'super-alt-',            'super+alt')
        call expect(12, 'super-alt-shift-',      'super+alt+shift')
        call expect(14, 'super-ctrl-shift-',     'super+ctrl+shift')
        call expect(15, 'super-alt-ctrl-',       'super+alt+ctrl')
        call expect(16, 'super-alt-ctrl-shift-', 'every modifier at once')
    end subroutine test_the_combinations_that_used_to_vanish

    ! The chord the tab-group navigation is bound to. Before this it produced
    ! a bare arrow, so pressing it moved the caret.
    subroutine test_super()
        call expect(9,  'super-',      'super alone')
        call expect(13, 'super-ctrl-', 'super+ctrl -- the group navigation chord')
    end subroutine test_super

    ! Some terminals fold the lock states into the same field. Num lock adds
    ! 128, which a table lookup cannot possibly cover; ignoring the high bits
    ! means a chord still works with num lock on.
    subroutine test_lock_states_do_not_break_a_chord()
        call expect(13 + 64,  'super-ctrl-', 'super+ctrl with caps lock on')
        call expect(13 + 128, 'super-ctrl-', 'super+ctrl with num lock on')
        call expect(5 + 128,  'ctrl-',       'plain ctrl with num lock on')
        call expect(2 + 64 + 128, 'shift-',  'shift with both locks on')
    end subroutine test_lock_states_do_not_break_a_chord

    subroutine test_degenerate_input()
        call expect(1, '', 'no modifiers at all')
        call expect(0, '', 'zero, which no terminal should send')
        call expect(-5, '', 'a negative value is clamped, not indexed')
    end subroutine test_degenerate_input

end program test_modifier_prefix
