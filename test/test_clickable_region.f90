program test_clickable_region
    ! The clickable-region table the renderer fills while drawing and the
    ! mouse router consults on a click. Geometry only, no editor state, so
    ! the edge cases (overlap order, frame reset, capacity) are worth
    ! pinning down here rather than through a pty.
    !
    ! Note the local `hit`: Fortran has no `region_at(...)%kind`, a component
    ! cannot be taken of a function result.
    use clickable_region_module
    implicit none

    type(clickable_region_t) :: hit
    integer :: nfail, i

    nfail = 0

    ! --- A fresh frame has nothing in it ---
    call regions_begin_frame()
    call check(region_count() == 0, "a new frame starts empty")
    hit = region_at(5, 5)
    call check(hit%kind == REGION_NONE, "a miss on an empty frame")

    ! --- Basic hit testing, bounds inclusive ---
    call regions_begin_frame()
    call region_add(REGION_TAB, 1, 1, 10, 20, 3)
    call check(region_count() == 1, "one region recorded")
    hit = region_at(1, 10)
    call check(hit%kind == REGION_TAB, "left edge is inside")
    hit = region_at(1, 20)
    call check(hit%kind == REGION_TAB, "right edge is inside")
    hit = region_at(1, 9)
    call check(hit%kind == REGION_NONE, "one column left is outside")
    hit = region_at(1, 21)
    call check(hit%kind == REGION_NONE, "one column right is outside")
    hit = region_at(2, 15)
    call check(hit%kind == REGION_NONE, "the row below is outside")
    hit = region_at(1, 15)
    call check(hit%payload == 3, "payload comes back")

    ! --- Later regions win: they were drawn on top ---
    call regions_begin_frame()
    call region_add(REGION_TAB, 1, 1, 1, 40, 7)
    call region_add(REGION_BLOCK, 1, 1, 10, 20)
    hit = region_at(1, 15)
    call check(hit%kind == REGION_BLOCK, "the later region is on top")
    hit = region_at(1, 5)
    call check(hit%kind == REGION_TAB, "outside it the earlier one still hits")

    ! --- A frame reset drops the previous frame's regions ---
    call regions_begin_frame()
    call region_add(REGION_TAB, 1, 1, 10, 20, 1)
    call regions_begin_frame()
    call check(region_count() == 0, "begin_frame clears")
    hit = region_at(1, 15)
    call check(hit%kind == REGION_NONE, "and its regions stop matching")

    ! --- Degenerate rectangles are ignored, so callers need no guards ---
    call regions_begin_frame()
    call region_add(REGION_TAB, 5, 4, 1, 10, 1)      ! inverted rows
    call region_add(REGION_TAB, 1, 1, 10, 9, 2)      ! inverted columns
    call check(region_count() == 0, "inverted rectangles are dropped")

    ! --- A single cell is a legal region ---
    call regions_begin_frame()
    call region_add(REGION_TAB, 1, 1, 12, 12, 4)
    call check(region_count() == 1, "a single-cell region is kept")
    hit = region_at(1, 12)
    call check(hit%kind == REGION_TAB, "and it hits")

    ! --- Overflow degrades to unclickable, never corrupts ---
    !
    ! The cap is not asserted by value: it has already been raised once, when
    ! a second tab-bar row and the group dialog joined the tree in competing
    ! for slots. What must hold is the BEHAVIOUR -- overflow drops the extra
    ! regions and leaves the earlier ones intact, rather than corrupting the
    ! table or wrapping around.
    call regions_begin_frame()
    do i = 1, 4000
        call region_add(REGION_TAB, 1, 1, i, i, i)
    end do
    call check(region_count() > 0, "some regions are kept")
    call check(region_count() < 4000, "capacity is capped")
    hit = region_at(1, 1)
    call check(hit%payload == 1, "the earliest region survives")
    hit = region_at(1, 3999)
    call check(hit%kind == REGION_NONE, "the dropped ones simply miss")

    ! --- Payload defaults to 0 when the caller omits it ---
    call regions_begin_frame()
    call region_add(REGION_FUSS_TOGGLE, 30, 30, 90, 92)
    hit = region_at(30, 91)
    call check(hit%payload == 0, "payload defaults to zero")
    call check(hit%kind == REGION_FUSS_TOGGLE, "kind round-trips")

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All clickable-region tests passed'

contains

    subroutine check(ok, name)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name

        if (ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name
            nfail = nfail + 1
        end if
    end subroutine check

end program test_clickable_region
