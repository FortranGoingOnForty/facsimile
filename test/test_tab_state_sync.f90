program test_tab_state_sync
    ! Regression tests: stale cursor/viewport state must not leak
    ! between tabs. A restored workspace session left editor%cursors
    ! pointing at another tab's position; create_tab kept it, the next
    ! sync_editor_to_pane stamped it into the new pane, and the
    ! viewport then scrolled a 1-line file completely off screen.
    use editor_state_module
    use text_buffer_module
    implicit none

    type(editor_state_t) :: editor
    integer :: nfail

    nfail = 0

    call init_editor(editor)

    ! Tab 1 with a 100-line buffer, cursor parked deep in the file
    call create_tab(editor, 'first.txt')
    ! The pane owns the text; the tab no longer keeps a shadow copy.
    call fill_lines(editor%tabs(1)%panes(1)%buffer, 100)
    editor%cursors(1)%line = 85
    editor%cursors(1)%column = 8
    editor%viewport_line = 82
    editor%viewport_column = 1
    call sync_editor_to_pane(editor)

    ! Opening a new tab must load the new pane's state into the editor
    ! globals, not carry tab 1's cursor/viewport along
    call create_tab(editor, 'second.txt')
    call check(editor%active_tab_index == 2, 'new tab is active')
    call check(editor%cursors(1)%line == 1, 'cursor line reset to 1')
    call check(editor%cursors(1)%column == 1, 'cursor column reset to 1')
    call check(editor%viewport_line == 1, 'viewport line reset to 1')
    call check(editor%viewport_column == 1, 'viewport column reset to 1')
    call check(editor%filename == 'second.txt', 'filename follows new tab')
    call check(editor%tabs(1)%panes(1)%cursors(1)%line == 85, &
               'tab 1 pane keeps its own cursor')

    ! Switching to a tab whose persisted cursor points past EOF must
    ! clamp (e.g. the file shrank on disk between sessions)
    editor%tabs(1)%panes(1)%cursors(1)%line = 999
    editor%tabs(1)%panes(1)%cursors(1)%column = 500
    call switch_to_tab(editor, 1)
    call check(editor%active_tab_index == 1, 'switched back to tab 1')
    call check(editor%cursors(1)%line == 100, 'cursor line clamped to last line')
    call check(editor%cursors(1)%column <= len('line 100') + 1, &
               'cursor column clamped to line length')

    ! Round trip: the freshly clamped state must survive a switch away
    ! and back without drifting
    call switch_to_tab(editor, 2)
    call check(editor%cursors(1)%line == 1, 'tab 2 state intact after round trip')
    call switch_to_tab(editor, 1)
    call check(editor%cursors(1)%line == 100, 'tab 1 clamped state stable')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All tab state sync tests passed'

contains

    subroutine fill_lines(buf, n)
        type(buffer_t), intent(inout) :: buf
        integer, intent(in) :: n
        integer :: i
        character(len=16) :: line
        character(len=:), allocatable :: text
        text = ''
        do i = 1, n
            write(line, '(a,i0)') 'line ', i
            if (i > 1) text = text // char(10)
            text = text // trim(line)
        end do
        call buffer_insert(buf, 1, text)
    end subroutine fill_lines

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name
        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name
            nfail = nfail + 1
        end if
    end subroutine check

end program test_tab_state_sync
