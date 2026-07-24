program test_auto_indent
    ! Regression tests for auto-indent on Enter.
    !
    ! The bug: the new line's indentation was PREPENDED. Whenever the caret
    ! sat left of a line's own indentation, that whitespace was already part
    ! of the text pushed onto the new line, so re-inserting the indent doubled
    ! it -- and every subsequent Enter doubled the doubled value (4, 8, 12,
    ! 16...). On a line with text it also shifted the text right, turning
    ! '    int x;' into '        int x;'.
    !
    ! The fix replaces the new line's leading whitespace with the computed
    ! indent instead of prepending, which makes repeated Enter idempotent.
    ! An extra indent level is added in exactly one situation: the caret
    ! directly after an opening brace.
    use editor_state_module
    use text_buffer_module
    use command_handler_module, only: handle_key_command, init_command_handler
    implicit none

    character(len=1), parameter :: NL = char(10)
    character(len=1), parameter :: TAB = char(9)
    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    logical :: quit
    integer :: nfail

    nfail = 0
    call init_command_handler()

    ! --- The reported bug: Enter on a blank indented line inside braces ---
    call setup('int main() {' // NL // '    ' // NL // '}')
    call cursor_at(2, 1)
    call key('enter')
    call check_text('int main() {' // NL // '' // NL // '    ' // NL // '}', &
                    'blank indented line: one enter does not double the indent')
    call key('enter')
    call key('enter')
    call check_text('int main() {' // NL // '' // NL // '    ' // NL // &
                    '    ' // NL // '    ' // NL // '}', &
                    'blank indented line: repeated enter stays at one indent')

    ! --- Same escalation, with no braces anywhere ---
    call setup('int main;' // NL // '    ')
    call cursor_at(2, 1)
    call key('enter')
    call key('enter')
    call check_text('int main;' // NL // '' // NL // '    ' // NL // '    ', &
                    'escalation is not brace-specific')

    ! --- Caret left of a statement's indent must not move the statement ---
    call setup('void f() {' // NL // '    int x;' // NL // '}')
    call cursor_at(2, 1)
    call key('enter')
    call check_text('void f() {' // NL // '' // NL // '    int x;' // NL // '}', &
                    'text keeps its own indent when pushed down')
    ! The caret is now at the indent column, so the next enter splits after
    ! the indent -- the statement still keeps exactly its own four spaces
    call key('enter')
    call check_text('void f() {' // NL // '' // NL // '    ' // NL // &
                    '    int x;' // NL // '}', &
                    'and still does on the next enter')

    ! --- Caret inside the indent run reindents to the line's level ---
    call setup('    int x;')
    call cursor_at(1, 3)
    call key('enter')
    call check_text('  ' // NL // '    int x;', &
                    'caret inside the indent reindents the moved text')

    ! --- Enter at end of an indented line still indents (the normal case) ---
    call setup('    int x;')
    call cursor_at(1, 11)
    call key('enter')
    call check_text('    int x;' // NL // '    ', 'end-of-line enter indents')
    call check_cursor(1, 2, 5, 'caret sits after the new indent')

    ! --- Tab-indented files keep their tabs ---
    call setup('if (a) {' // NL // TAB // 'int x;' // NL // '}')
    call cursor_at(2, 1)
    call key('enter')
    call check_text('if (a) {' // NL // '' // NL // TAB // 'int x;' // NL // '}', &
                    'tab indentation is left alone')

    ! --- Caret directly after '{' adds one indent level ---
    call setup('int main() {' // NL // '}')
    call cursor_at(1, 13)
    call key('enter')
    call check_text('int main() {' // NL // '    ' // NL // '}', &
                    'enter after an open brace indents one level')
    call check_cursor(1, 2, 5, 'caret lands inside the new indent')
    ! ...and only once: the blank line it created does not keep growing
    call key('enter')
    call key('enter')
    call check_text('int main() {' // NL // '    ' // NL // '    ' // NL // &
                    '    ' // NL // '}', &
                    'the extra level is added once, not on every enter')

    ! --- '{|}' expands into three lines ---
    call setup('int main() {}')
    call cursor_at(1, 13)
    call key('enter')
    call check_text('int main() {' // NL // '    ' // NL // '}', &
                    'braces expand with the closer on its own line')
    call check_cursor(1, 2, 5, 'caret between the braces')

    ! --- ...at whatever indent the opening line sits at ---
    call setup('    if (a) {}')
    call cursor_at(1, 13)
    call key('enter')
    call check_text('    if (a) {' // NL // '        ' // NL // '    }', &
                    'nested braces expand relative to the opening line')

    ! --- No extra level when the caret is not after a brace ---
    call setup('int f() { a }')
    call cursor_at(1, 12)
    call key('enter')
    call check_text('int f() { a' // NL // '}', &
                    'a brace earlier on the line does not trigger expansion')

    ! --- Enter on a plain unindented line is unchanged ---
    call setup('abc')
    call cursor_at(1, 2)
    call key('enter')
    call check_text('a' // NL // 'bc', 'plain split is unaffected')
    call check_cursor(1, 2, 1, 'caret at the start of the new line')

    ! --- Multi-cursor keeps the fix (expansion is off there by design) ---
    call setup('    aaa' // NL // '    bbb')
    call cursors_at(1, 1, 2, 1)
    call key('enter')
    call check_text('' // NL // '    aaa' // NL // '' // NL // '    bbb', &
                    'multi-cursor enter does not double indents either')

    ! --- UTF-8: the split point is a character column, not a byte ---
    call setup('    caf' // char(195) // char(169) // 'x')
    call cursor_at(1, 9)
    call key('enter')
    call check_text('    caf' // char(195) // char(169) // NL // '    x', &
                    'multibyte line splits at the right character')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All auto-indent tests passed'

contains

    subroutine setup(text)
        character(len=*), intent(in) :: text

        if (allocated(editor%tabs)) call cleanup_editor(editor)
        call init_command_handler()
        call init_buffer(buffer)
        if (len(text) > 0) call buffer_insert(buffer, 1, text)
        buffer%modified = .false.
        call init_editor(editor)
        call create_tab(editor, 'indent_test.c')
    end subroutine setup

    subroutine cursor_at(l, c)
        integer, intent(in) :: l, c

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(1))
        editor%cursors(1)%line = l
        editor%cursors(1)%column = c
        editor%active_cursor = 1
    end subroutine cursor_at

    subroutine cursors_at(l1, c1, l2, c2)
        integer, intent(in) :: l1, c1, l2, c2

        if (allocated(editor%cursors)) deallocate(editor%cursors)
        allocate(editor%cursors(2))
        editor%cursors(1)%line = l1
        editor%cursors(1)%column = c1
        editor%cursors(2)%line = l2
        editor%cursors(2)%column = c2
        editor%active_cursor = 1
    end subroutine cursors_at

    subroutine key(k)
        character(len=*), intent(in) :: k

        quit = .false.
        call handle_key_command(k, editor, buffer, quit)
    end subroutine key

    subroutine check_text(expected, name)
        character(len=*), intent(in) :: expected, name
        character(len=:), allocatable :: got

        got = buffer_to_string(buffer)
        call check(got == expected, name, '"' // got // '"')
    end subroutine check_text

    subroutine check_cursor(idx, l, c, name)
        integer, intent(in) :: idx, l, c
        character(len=*), intent(in) :: name
        character(len=64) :: got

        if (idx > size(editor%cursors)) then
            call check(.false., name, 'cursor index out of range')
            return
        end if
        write(got, '(a,i0,a,i0,a)') '(', editor%cursors(idx)%line, ',', &
            editor%cursors(idx)%column, ')'
        call check(editor%cursors(idx)%line == l .and. &
                   editor%cursors(idx)%column == c, name, trim(got))
    end subroutine check_cursor

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

end program test_auto_indent
