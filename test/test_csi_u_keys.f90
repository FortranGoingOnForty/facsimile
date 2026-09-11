program test_csi_u_keys
    ! The kitty keyboard protocol (negotiated in terminal_init with CSI > 1 u)
    ! reports keys as CSI <codepoint> ; <modifiers> u. It exists here for one
    ! reason: Ctrl+/ and Ctrl+Shift+/ both collapse onto byte 0x1F in the
    ! legacy encoding, so ctrl-/ (toggle comment) and ctrl-? (help) cannot
    ! coexist without it.
    !
    ! The risk is that EVERY ctrl/alt chord now arrives this way in terminals
    ! that accept the negotiation, so these tests pin the whole translation
    ! back into the key names the command layer already speaks -- including
    ! the chords whose control bytes used to be indistinguishable from Tab,
    ! Enter and Escape, which must keep their old meanings.
    use input_handler_module, only: decode_csi_u
    implicit none

    integer :: nfail

    nfail = 0

    ! --- The reason this protocol is on at all ---
    call expect('47;5', 'ctrl-/',        'ctrl+/ is its own key')
    call expect('47;6', 'ctrl-shift-/',  'ctrl+shift+/ is distinguishable')

    ! --- Bare keys ---
    call expect('27', 'esc',        'escape')
    call expect('13', 'enter',      'enter')
    call expect('9',  'tab',        'tab')
    call expect('127', 'backspace', 'backspace')
    call expect('32', ' ',          'space arrives as the character itself')
    call expect('97', 'a',          'printable arrives as the character itself')
    call expect('27;1', 'esc',      'modifier field of 1 means no modifiers')

    ! --- Ctrl chords keep the names the control bytes produced ---
    call expect('97;5',  'ctrl-a',     'ctrl+a')
    call expect('99;5',  'ctrl-c',     'ctrl+c')
    call expect('104;5', 'ctrl-h',     'ctrl+h')
    call expect('93;5',  'ctrl-]',     'ctrl+] (redo)')
    call expect('32;5',  'ctrl-space', 'ctrl+space (completion)')
    call expect('127;7', 'alt-ctrl-backspace', &
                'ctrl+alt+backspace keeps both modifiers')
    call expect('8;7',   'alt-ctrl-backspace', &
                'ctrl+alt+ctrl-h backspace spelling')

    ! --- Chords whose control bytes WERE Tab/Enter/Escape keep that meaning,
    !     so nothing a user had in muscle memory changes under the protocol ---
    call expect('105;5', 'tab',   'ctrl+i still means tab')
    call expect('109;5', 'enter', 'ctrl+m still means enter')
    call expect('106;5', 'enter', 'ctrl+j still means enter')
    call expect('91;5',  'esc',   'ctrl+[ still means escape')

    ! --- Shift and alt ---
    call expect('9;2',   'shift-tab',   'shift+tab')
    call expect('97;2',  'A',           'shift+letter is the uppercase letter')
    call expect('122;6', 'ctrl-shift-z', 'ctrl+shift+z (redo)')
    call expect('120;3', 'alt-x',       'alt+x')
    call expect('120;4', 'alt-shift-x', 'alt+shift+letter stays lowercase')
    call expect('91;3',  'alt-[',       'alt+[ (matching bracket)')
    call expect('46;3',  'alt-.',       'alt+. (code actions)')
    call expect("39;3",  "alt-'",       'alt+apostrophe (cycle quotes)')
    call expect('39;4',  'alt-shift-apostrophe', &
                'alt+shift+apostrophe keeps its spelled-out legacy name')

    ! --- Modifier field details ---
    call expect('97;69', 'ctrl-a', 'caps-lock bit is ignored')
    call expect('47:63;6', 'ctrl-shift-/', 'alternate-key sub-parameters ignored')
    call expect('97;5;97', 'ctrl-a', 'associated-text field ignored')
    call expect('97;5:1', 'ctrl-a', 'press event')
    call expect('97;5:2', 'ctrl-a', 'key repeat is treated as a press')

    ! --- Kitty reports functional keys in the Unicode private-use area.
    !     F2 is rename; if these are unmapped the key silently does nothing
    !     and, worse, used to leave the legacy parser eating the bytes of
    !     whatever was typed next. ---
    call expect('57364', 'f1',  'F1 as a functional key code')
    call expect('57365', 'f2',  'F2 as a functional key code (rename)')
    call expect('57375', 'f12', 'F12 as a functional key code')
    call expect('57365;2', 'shift-f2', 'shift+F2')
    call expect('57375;2', 'shift-f12', 'shift+F12 (find references)')

    ! --- Keypad keys resolve to their ordinary equivalents ---
    call expect('57399', '0',        'keypad 0')
    call expect('57408', '9',        'keypad 9')
    call expect('57413', '+',        'keypad plus')
    call expect('57414', 'enter',    'keypad enter')
    call expect('57417', 'left',     'keypad left')
    call expect('57421', 'pageup',   'keypad page up')
    call expect('57424', 'end',      'keypad end')
    call expect('57426', 'delete',   'keypad delete')

    ! --- Events and keys with no name here produce nothing at all ---
    call expect_none('97;5:3', 'key release is dropped')
    call expect_none('57376',  'F13 has no binding here')
    call expect_none('57430',  'media keys have no binding')
    call expect_none('57441',  'a lone modifier keypress has no binding')
    call expect_none('',       'empty parameters')
    call expect_none('x',      'garbage parameters')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All CSI-u key decoding tests passed'

contains

    subroutine expect(params, want, name)
        character(len=*), intent(in) :: params, want, name
        character(len=32) :: got
        logical :: ok

        call decode_csi_u(params, got, ok)
        if (ok .and. got == want) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // trim(got) // '")'
            nfail = nfail + 1
        end if
    end subroutine expect

    subroutine expect_none(params, name)
        character(len=*), intent(in) :: params, name
        character(len=32) :: got
        logical :: ok

        call decode_csi_u(params, got, ok)
        if (.not. ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // trim(got) // '")'
            nfail = nfail + 1
        end if
    end subroutine expect_none

end program test_csi_u_keys
