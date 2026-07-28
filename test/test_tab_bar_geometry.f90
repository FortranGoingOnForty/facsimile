program test_tab_bar_geometry
    ! The tab bar's height, and the document's, must be the same fact.
    !
    ! They were two facts: a literal `screen_rows - 2` in the height math and a
    ! literal `start_row = 2` in twenty-odd coordinate calculations. The
    ! coordinates handled "no tabs" and the height did not, so with zero tabs
    ! the document drew one more row than the height believed and the last line
    ! could not be paged to.
    !
    ! That mattered little at one row. It matters a great deal at two, which is
    ! what a tab group's member row will need -- so the relationship is pinned
    ! here rather than left to agree by coincidence.
    use iso_fortran_env, only: int32
    use editor_state_module
    use renderer_module, only: tab_bar_height, first_content_row, text_area_height
    implicit none

    integer :: nfail

    nfail = 0

    call test_no_tabs()
    call test_with_tabs()
    call test_the_invariant_holds_at_every_size()
    call test_tiny_terminals()

    if (nfail == 0) then
        print '(a)', 'test_tab_bar_geometry: all passed'
    else
        print '(a,i0,a)', 'test_tab_bar_geometry: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine check(cond, label)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label

        if (.not. cond) then
            print '(a)', '  FAIL: ' // label
            nfail = nfail + 1
        end if
    end subroutine check

    ! A fresh editor genuinely has zero tabs -- init_editor allocates tabs(0)
    ! and closing the last one returns to it, so this is a reachable state.
    subroutine test_no_tabs()
        type(editor_state_t) :: editor

        call init_editor(editor)
        editor%screen_rows = 24
        editor%screen_cols = 80

        call check(size(editor%tabs) == 0, 'a fresh editor has no tabs')
        call check(tab_bar_height(editor) == 0, 'no tabs, no tab bar')
        call check(first_content_row(editor) == 1, 'the document starts at row 1')
        call check(text_area_height(editor) == 23, &
                   'and gets every row but the status bar')
    end subroutine test_no_tabs

    subroutine test_with_tabs()
        type(editor_state_t) :: editor

        call init_editor(editor)
        editor%screen_rows = 24
        editor%screen_cols = 80
        call create_tab(editor, 'a.txt')

        call check(tab_bar_height(editor) == 1, 'one tab bar row')
        call check(first_content_row(editor) == 2, 'the document starts at row 2')
        call check(text_area_height(editor) == 22, &
                   'and gets the rest less the status bar')

        call create_tab(editor, 'b.txt')
        call check(tab_bar_height(editor) == 1, &
                   'a second tab does not make a second bar row')
    end subroutine test_with_tabs

    ! The relationship, asserted directly rather than inferred from the three
    ! cases above. If a future change makes the bar two rows, this is what
    ! catches any consumer that did not get the message.
    subroutine test_the_invariant_holds_at_every_size()
        type(editor_state_t) :: editor
        integer :: rows

        call init_editor(editor)
        editor%screen_cols = 80

        do rows = 5, 60
            editor%screen_rows = rows

            ! with no tabs
            call check(first_content_row(editor) == 1 + tab_bar_height(editor), &
                       'first_content_row is one past the bar (no tabs)')
            call check(text_area_height(editor) == &
                       max(1, rows - 1 - tab_bar_height(editor)), &
                       'height is the screen less status bar and tab bar (no tabs)')
        end do

        call create_tab(editor, 'a.txt')
        do rows = 5, 60
            editor%screen_rows = rows
            call check(first_content_row(editor) == 1 + tab_bar_height(editor), &
                       'first_content_row is one past the bar (with a tab)')
            call check(text_area_height(editor) == &
                       max(1, rows - 1 - tab_bar_height(editor)), &
                       'height is the screen less status bar and tab bar (with a tab)')

            ! The rows the document may draw on must fit on the screen: the
            ! last one is first_content_row + height - 1, and the status bar
            ! owns the row below it.
            call check(first_content_row(editor) + text_area_height(editor) - 1 &
                       <= rows - 1, 'the document never reaches the status bar')
        end do
    end subroutine test_the_invariant_holds_at_every_size

    ! A one-row terminal is absurd but reachable while dragging a split, and
    ! the clamp exists so nothing computes a negative height.
    subroutine test_tiny_terminals()
        type(editor_state_t) :: editor
        integer :: rows

        call init_editor(editor)
        editor%screen_cols = 80
        call create_tab(editor, 'a.txt')

        do rows = 1, 4
            editor%screen_rows = rows
            call check(text_area_height(editor) >= 1, &
                       'the document always gets at least one row')
            call check(first_content_row(editor) >= 1, &
                       'and starts on a real row')
        end do
    end subroutine test_tiny_terminals

end program test_tab_bar_geometry
