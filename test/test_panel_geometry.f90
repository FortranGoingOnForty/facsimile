program test_panel_geometry
    ! How many rows the terminal panel gets, and why.
    !
    ! The height used to be recomputed from a fixed 30% in two separate places,
    ! so there was nothing a user could set: a chosen height was discarded the
    ! next time the window changed size -- which included merely opening the
    ! panel, because toggle_terminal_panel calls resize itself.
    !
    ! Now the ratio is the stored intent and the row count is derived from it.
    ! That single change is what makes the panel hold its proportion across a
    ! window resize, so these assertions are mostly about the arithmetic
    ! staying honest at the edges where the two clamps meet.
    use terminal_panel_module, only: height_for, permille_for
    implicit none

    integer :: nfail

    nfail = 0

    call test_a_ratio_becomes_rows()
    call test_the_proportion_survives_a_resize()
    call test_the_panel_keeps_a_usable_minimum()
    call test_the_editor_is_never_starved()
    call test_a_tiny_terminal_resolves_the_clash()
    call test_the_round_trip_is_stable()
    call test_degenerate_input()

    if (nfail == 0) then
        print '(a)', 'test_panel_geometry: all passed'
    else
        print '(a,i0,a)', 'test_panel_geometry: ', nfail, ' FAILED'
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

    subroutine expect(got, want, label)
        integer, intent(in) :: got, want
        character(len=*), intent(in) :: label

        if (got /= want) then
            print '(a)', '  FAIL: ' // label
            print '(a,i0,a,i0)', '        got ', got, ', wanted ', want
            nfail = nfail + 1
        end if
    end subroutine expect

    subroutine test_a_ratio_becomes_rows()
        call expect(height_for(300, 60), 18, 'a third of 60 rows')
        call expect(height_for(500, 40), 20, 'half of 40 rows')
        call expect(height_for(250, 40), 10, 'a quarter of 40 rows')
    end subroutine test_a_ratio_becomes_rows

    ! THE behaviour the feature exists for, and the one that was impossible
    ! while the height was recomputed from a constant.
    subroutine test_the_proportion_survives_a_resize()
        integer :: permille

        ! A user drags the panel to 20 rows on a 60-row screen ...
        permille = permille_for(20, 60)

        ! ... and the window is resized. The panel keeps its share.
        call expect(height_for(permille, 30), 10, &
                    'a third of the screen stays a third when halved')
        call expect(height_for(permille, 90), 30, &
                    'and stays a third when the window grows')
        call expect(height_for(permille, 60), 20, &
                    'and comes back to exactly where it was')
    end subroutine test_the_proportion_survives_a_resize

    subroutine test_the_panel_keeps_a_usable_minimum()
        ! One row of the panel is the separator bar, so a small ratio must
        ! still leave something to look at.
        call check(height_for(1, 60) >= 5, &
                   'a tiny ratio still gives the panel its minimum')
        call check(height_for(10, 100) >= 5, &
                   'and does so on a large screen too')
    end subroutine test_the_panel_keeps_a_usable_minimum

    ! The clamp that used to be spelled "80% of screen" -- true, but it never
    ! said what it was protecting.
    subroutine test_the_editor_is_never_starved()
        integer :: rows, h

        do rows = 12, 120
            h = height_for(1000, rows)      ! ask for the whole screen
            call check(rows - 1 - h >= 4, &
                       'asking for everything still leaves the editor rows')
        end do
    end subroutine test_the_editor_is_never_starved

    ! Below about 10 rows the two clamps cannot both be satisfied. The editor
    ! wins: a short panel is awkward, a document with no lines is useless.
    subroutine test_a_tiny_terminal_resolves_the_clash()
        integer :: rows, h

        do rows = 8, 11
            h = height_for(500, rows)
            call check(h >= 2, 'the panel always keeps the separator plus a line')
            call check(h <= rows - 2, 'but never takes the whole screen')
        end do
    end subroutine test_a_tiny_terminal_resolves_the_clash

    ! Rows -> permille -> rows must not drift, or a drag would creep by a row
    ! every time the window was touched.
    subroutine test_the_round_trip_is_stable()
        integer :: rows, screen, once, twice

        do screen = 30, 80, 7
            do rows = 6, screen - 6, 3
                once  = height_for(permille_for(rows, screen), screen)
                twice = height_for(permille_for(once, screen), screen)
                call expect(twice, once, 'the round trip settles immediately')
            end do
        end do
    end subroutine test_the_round_trip_is_stable

    subroutine test_degenerate_input()
        call check(height_for(0, 60) >= 2, 'a zero ratio is still drawable')
        call check(height_for(5000, 60) <= 59, 'an absurd ratio is clamped')
        call check(permille_for(10, 0) > 0, 'a zero-row screen does not divide by it')
        call check(permille_for(-5, 60) > 0, 'a negative row count is clamped')
    end subroutine test_degenerate_input

end program test_panel_geometry
