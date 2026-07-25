program test_context_menu
    ! Geometry, hit-testing and keyboard navigation for the right-click menu.
    ! No editor state is involved, so the edge cases worth pinning down --
    ! clamping at the screen edges, separators, disabled rows, wrapping --
    ! belong here rather than in a pty test.
    use context_menu_module
    implicit none

    integer :: nfail
    integer :: row0, col0, width, height
    logical :: ok

    nfail = 0

    ! --- Width follows the widest row ---
    call context_menu_begin(1)
    call context_menu_add_item('Cut', 'Ctrl+X', 10)
    call context_menu_add_item('A Very Long Menu Label Here', 'Shift+F12', 11)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call check(ok, "a menu with room shows")
    call context_menu_geometry(row0, col0, width, height)
    ! ' ' + 27 label + 2 gap + 9 accel + ' ' + 2 borders
    call check(width == 42, "width derives from the widest row")
    call check(height == 4, "height is rows plus two borders")
    call check(row0 == 5 .and. col0 == 5, "the anchor is the top-left corner")

    ! --- A short menu still gets a minimum width ---
    call context_menu_begin(1)
    call context_menu_add_item('Ok', '', 1)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call context_menu_geometry(row0, col0, width, height)
    call check(width == 18, "a narrow menu is padded to the minimum width")

    ! --- Clamping at the right edge: slide left, never overflow ---
    call context_menu_begin(1)
    call context_menu_add_item('Copy', 'Ctrl+C', 2)
    ok = context_menu_show(5, 95, 2, 29, 1, 100)
    call context_menu_geometry(row0, col0, width, height)
    call check(col0 + width - 1 == 100, "a menu at the right edge slides left")
    call check(col0 == 83, "and only as far as it must")

    ! --- Clamping at the bottom edge: slide up ---
    call context_menu_begin(1)
    call context_menu_add_item('One', '', 1)
    call context_menu_add_item('Two', '', 2)
    call context_menu_add_item('Three', '', 3)
    ok = context_menu_show(28, 5, 2, 29, 1, 100)
    call context_menu_geometry(row0, col0, width, height)
    call check(row0 + height - 1 == 29, "a menu at the bottom edge slides up")
    call check(row0 == 25, "and only as far as it must")

    ! --- Too little room: refuse rather than draw a broken box ---
    call context_menu_begin(1)
    call context_menu_add_item('One', '', 1)
    ok = context_menu_show(5, 5, 2, 3, 1, 100)
    call check(.not. ok, "two rows of space is refused")
    call check(.not. is_context_menu_visible(), "and nothing is shown")

    call context_menu_begin(1)
    call context_menu_add_item('One', '', 1)
    ok = context_menu_show(5, 5, 2, 29, 1, 10)
    call check(.not. ok, "a too-narrow screen is refused")

    ! --- An empty menu never shows ---
    call context_menu_begin(1)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call check(.not. ok, "an empty menu is refused")

    ! --- Separators: not selectable, not first, and they count as rows ---
    call context_menu_begin(2)
    call context_menu_add_separator()
    call check(context_menu_row_count() == 0, "a leading separator is dropped")
    call context_menu_add_item('Alpha', '', 1)
    call context_menu_add_separator()
    call context_menu_add_item('Beta', '', 2)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call check(context_menu_row_count() == 3, "separators occupy a row")
    call check(context_menu_selected() == 1, "the first item starts selected")
    call check(.not. context_menu_row_enabled(2), "a separator is never enabled")
    call check(context_menu_kind() == 2, "the kind round-trips")

    ! --- Navigation skips separators and wraps ---
    call nav('down')
    call check(context_menu_selected() == 3, "down skips the separator")
    call nav('down')
    call check(context_menu_selected() == 1, "down wraps to the top")
    call nav('up')
    call check(context_menu_selected() == 3, "up wraps to the bottom")
    call nav('home')
    call check(context_menu_selected() == 1, "home selects the first item")
    call nav('end')
    call check(context_menu_selected() == 3, "end selects the last item")

    ! --- Navigation skips disabled rows ---
    call context_menu_begin(1)
    call context_menu_add_item('Enabled A', '', 1)
    call context_menu_add_item('Disabled', '', 2, enabled=.false.)
    call context_menu_add_item('Enabled B', '', 3)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call check(context_menu_selected() == 1, "selection starts on an enabled row")
    call nav('down')
    call check(context_menu_selected() == 3, "down skips the disabled row")
    call nav('up')
    call check(context_menu_selected() == 1, "up skips it too")
    call check(.not. context_menu_row_enabled(2), "the disabled row reports so")
    call check(context_menu_row_action(2) == 2, "but still carries its action")

    ! --- A menu with nothing selectable leaves the selection empty ---
    call context_menu_begin(1)
    call context_menu_add_item('Nope', '', 1, enabled=.false.)
    call context_menu_add_item('Also nope', '', 2, enabled=.false.)
    ok = context_menu_show(5, 5, 2, 29, 1, 100)
    call check(context_menu_selected() == 0, "nothing selectable means no selection")
    call nav('down')
    call check(context_menu_selected() == 0, "and navigation cannot invent one")

    ! --- Hit testing ---
    call context_menu_begin(1)
    call context_menu_add_item('Alpha', 'Ctrl+A', 11)
    call context_menu_add_separator()
    call context_menu_add_item('Beta', 'Ctrl+B', 22)
    ok = context_menu_show(10, 20, 2, 29, 1, 100)
    call context_menu_geometry(row0, col0, width, height)
    call check(context_menu_row_at(row0, col0) == 0, "the top border is not a row")
    call check(context_menu_row_at(row0 + 1, col0 + 2) == 1, "the first row hits")
    call check(context_menu_row_at(row0 + 2, col0 + 2) == 0, "the separator does not")
    call check(context_menu_row_at(row0 + 3, col0 + 2) == 3, "the last row hits")
    call check(context_menu_row_at(row0 + 4, col0 + 2) == 0, "the bottom border does not")
    call check(context_menu_row_at(row0 + 1, col0 - 1) == 0, "one column left misses")
    call check(context_menu_row_at(row0 + 1, col0 + width) == 0, "one column right misses")
    call check(context_menu_row_at(row0 + 1, col0) == 1, "the left border column hits its row")
    call check(context_menu_row_action(3) == 22, "actions round-trip")

    ! --- Hiding clears everything ---
    call context_menu_hide()
    call check(.not. is_context_menu_visible(), "hide clears visibility")
    call check(context_menu_row_at(row0 + 1, col0 + 2) == 0, "and stops hit-testing")
    call check(context_menu_selected() == 0, "and clears the selection")

    ! --- Height clamps to the space, dropping rows that do not fit ---
    call context_menu_begin(1)
    call context_menu_add_item('R1', '', 1)
    call context_menu_add_item('R2', '', 2)
    call context_menu_add_item('R3', '', 3)
    call context_menu_add_item('R4', '', 4)
    ok = context_menu_show(2, 5, 2, 6, 1, 100)
    call context_menu_geometry(row0, col0, width, height)
    call check(height == 5, "height clamps to the available rows")
    call check(context_menu_row_at(row0 + 3, col0 + 1) == 3, "rows that fit still hit")
    call check(context_menu_row_at(row0 + 4, col0 + 1) == 0, "rows past the box do not")

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All context-menu tests passed'

contains

    subroutine nav(key)
        character(len=*), intent(in) :: key
        logical :: handled

        handled = context_menu_handle_key(key)
        if (.not. handled) then
            print '(a)', 'FAIL: navigation key not handled: ' // key
            nfail = nfail + 1
        end if
    end subroutine nav

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

end program test_context_menu
