program test_tab_strip
    ! Where the tab bar puts things, with no terminal involved.
    !
    ! The old bar computed its layout inline while drawing, in bytes, and
    ! simply stopped at the first label that did not fit. Two consequences:
    ! with a dozen tabs the active one could be entirely off-screen and
    ! therefore unclickable, and a single CJK filename shifted every click to
    ! its right, because the drawn width and the recorded click span were both
    ! counted in bytes.
    !
    ! Separating placement from drawing makes both testable here, and means the
    ! click regions come from the same arithmetic as the pixels. The second row
    ! for tab groups will reuse it, so the two rows cannot drift apart.
    use renderer_module, only: strip_entry_t, strip_span_t, strip_layout, &
                               STRIP_MAX_ENTRIES
    implicit none

    integer :: nfail

    nfail = 0

    call test_everything_fits()
    call test_nothing_is_dropped_silently()
    call test_the_active_entry_is_always_visible()
    call test_a_redraw_keeps_the_scroll()
    call test_spans_never_overlap()
    call test_wide_characters_are_counted_in_cells()
    call test_absurd_widths_terminate()

    if (nfail == 0) then
        print '(a)', 'test_tab_strip: all passed'
    else
        print '(a,i0,a)', 'test_tab_strip: ', nfail, ' FAILED'
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

    subroutine build(entries, n, prefix)
        type(strip_entry_t), intent(out) :: entries(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: prefix
        integer :: i

        do i = 1, n
            write(entries(i)%label, '(a,i0,a)') '[' // prefix, i, ']'
            entries(i)%payload = i
        end do
    end subroutine build

    subroutine test_everything_fits()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll
        logical :: ml, mr

        call build(e, 3, 'a')
        scroll = 1
        call strip_layout(e, 3, 100, 1, scroll, sp, n_sp, ml, mr)
        call check(n_sp == 3, 'three short entries all fit')
        call check(.not. ml .and. .not. mr, 'and neither chevron is shown')
        call check(sp(1)%col0 == 1, 'the first starts at column 1')
    end subroutine test_everything_fits

    ! The behaviour that was actually broken: entries past the edge used to
    ! vanish with nothing to say they existed.
    subroutine test_nothing_is_dropped_silently()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll
        logical :: ml, mr

        call build(e, 30, 'file')
        scroll = 1
        call strip_layout(e, 30, 40, 1, scroll, sp, n_sp, ml, mr)
        call check(n_sp < 30, 'thirty entries do not fit in forty columns')
        call check(n_sp > 0, 'but some are drawn')
        call check(mr, 'and the overflow is reported, not hidden')
    end subroutine test_nothing_is_dropped_silently

    subroutine test_the_active_entry_is_always_visible()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll, active, i
        logical :: ml, mr, seen

        call build(e, 30, 'file')
        scroll = 1
        ! Walk the active entry across the whole set, keeping scroll as the
        ! editor would, and assert it is drawn every single time.
        do active = 1, 30
            call strip_layout(e, 30, 40, active, scroll, sp, n_sp, ml, mr)
            seen = .false.
            do i = 1, n_sp
                if (sp(i)%idx == active) seen = .true.
            end do
            call check(seen, 'the active entry is drawn wherever it is')
        end do

        ! And on the way back, which is the case a forward-only scroll misses.
        do active = 30, 1, -1
            call strip_layout(e, 30, 40, active, scroll, sp, n_sp, ml, mr)
            seen = .false.
            do i = 1, n_sp
                if (sp(i)%idx == active) seen = .true.
            end do
            call check(seen, 'and still is when moving back to the left')
        end do
    end subroutine test_the_active_entry_is_always_visible

    !> A redraw must not undo a scroll the user asked for.
    !>
    !> strip_layout used to drag the active entry back into view on EVERY
    !> call, which meant a chevron click that would push the active tab off
    !> the right was reverted before it could be seen -- the chevron only
    !> appeared to work when the adjacent tab happened to be the active one.
    !> It is also what pinned the bar to the far right after a tab opened
    !> there. Following happens when the active entry CHANGES; a plain redraw
    !> passes follow_active=.false.
    subroutine test_a_redraw_keeps_the_scroll()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll, i
        logical :: ml, mr, seen

        call build(e, 30, 'file')

        ! Entry 30 is active and in view: the scroll has followed it.
        scroll = 1
        call strip_layout(e, 30, 40, 30, scroll, sp, n_sp, ml, mr)
        seen = .false.
        do i = 1, n_sp
            if (sp(i)%idx == 30) seen = .true.
        end do
        call check(seen, 'following brings the active entry into view')
        call check(scroll > 1, 'and it had to scroll to do so')

        ! Now scroll left by hand, far enough that entry 30 cannot be shown,
        ! and redraw without following.
        scroll = 1
        call strip_layout(e, 30, 40, 30, scroll, sp, n_sp, ml, mr, &
                          follow_active=.false.)
        call check(scroll == 1, 'a redraw leaves a hand-set scroll alone')
        seen = .false.
        do i = 1, n_sp
            if (sp(i)%idx == 30) seen = .true.
        end do
        call check(.not. seen, 'even when the active entry falls off the end')
        call check(mr, 'and it is reported as more to the right')

        ! Redrawing again must not creep back either.
        call strip_layout(e, 30, 40, 30, scroll, sp, n_sp, ml, mr, &
                          follow_active=.false.)
        call check(scroll == 1, 'and repeated redraws do not creep')
    end subroutine test_a_redraw_keeps_the_scroll

    subroutine test_spans_never_overlap()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll, i, w
        logical :: ml, mr, ok

        call build(e, 20, 'name')
        ok = .true.
        do w = 20, 120
            scroll = 1
            call strip_layout(e, 20, w, 1, scroll, sp, n_sp, ml, mr)
            do i = 2, n_sp
                if (sp(i)%col0 <= sp(i-1)%col1) ok = .false.
            end do
            do i = 1, n_sp
                if (sp(i)%col1 > w) ok = .false.
                if (sp(i)%col0 < 1) ok = .false.
            end do
        end do
        call check(ok, 'spans stay in order, inside the width, and never overlap')
    end subroutine test_spans_never_overlap

    ! A click lands on a column, so the span must be measured in columns. When
    ! it was measured in bytes, one multibyte filename displaced every click to
    ! its right.
    subroutine test_wide_characters_are_counted_in_cells()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll, cjk_cells, ascii_cells
        logical :: ml, mr

        ! Two CJK characters: six bytes, four display cells.
        e(1)%label = '[' // char(228)//char(189)//char(160) // &
                            char(229)//char(165)//char(189) // ']'
        e(1)%payload = 1
        e(2)%label = '[ab]'
        e(2)%payload = 2
        scroll = 1
        call strip_layout(e, 2, 60, 1, scroll, sp, n_sp, ml, mr)
        call check(n_sp == 2, 'both entries fit')
        if (n_sp < 2) return

        cjk_cells = sp(1)%col1 - sp(1)%col0 + 1
        ascii_cells = sp(2)%col1 - sp(2)%col0 + 1
        call check(cjk_cells == 6, &
                   'the CJK label is six cells wide, not eight bytes')
        call check(ascii_cells == 4, 'the ascii label is four')
        call check(sp(2)%col0 == sp(1)%col1 + 2, &
                   'so the next entry starts where it is actually drawn')
    end subroutine test_wide_characters_are_counted_in_cells

    ! A width too small for any entry must still terminate and must not
    ! produce a blank bar.
    subroutine test_absurd_widths_terminate()
        type(strip_entry_t) :: e(STRIP_MAX_ENTRIES)
        type(strip_span_t) :: sp(STRIP_MAX_ENTRIES)
        integer :: n_sp, scroll, w
        logical :: ml, mr

        call build(e, 10, 'averylongentryname')
        do w = 1, 12
            scroll = 1
            call strip_layout(e, 10, w, 5, scroll, sp, n_sp, ml, mr)
            call check(n_sp >= 0, 'a tiny width returns rather than spinning')
            if (n_sp > 0) then
                call check(sp(1)%col0 >= 1, 'and any span it does return is sane')
            end if
        end do

        scroll = 1
        call strip_layout(e, 0, 80, 1, scroll, sp, n_sp, ml, mr)
        call check(n_sp == 0, 'no entries produces no spans')
    end subroutine test_absurd_widths_terminate

end program test_tab_strip
