module clickable_region_module
    ! Where a click can land, recorded by the renderer as it draws.
    !
    ! Before this existed every mouse handler recomputed screen geometry
    ! inline, and the tab bar's layout was thrown away the moment it was
    ! drawn -- so a click on a tab could not be resolved to a tab at all
    ! without redoing the whole layout at click time, including the varying
    ! label widths and the tree offset. Panels had the same problem from the
    ! other side: several drew over the document but let clicks through to it.
    !
    ! The renderer knows all of this exactly once, while drawing. So it says
    ! so: clear the table at the start of a frame, add a rectangle for each
    ! thing a click means something to, and let one router in the command
    ! handler look up a click instead of deriving it.
    !
    ! Regions are rebuilt per frame. Only a full redraw rebuilds them; the
    ! cursor-only fast path leaves them alone, which is correct because
    ! nothing structural moved.
    implicit none
    private

    public :: clickable_region_t
    public :: regions_begin_frame, region_add, region_at, region_count
    public :: REGION_NONE, REGION_TAB, REGION_FUSS_TOGGLE, REGION_TREE_ROW
    public :: REGION_BLOCK, REGION_CTX_ROW, REGION_TAB_SCROLL

    ! What a region means. REGION_BLOCK is deliberately inert: it marks a
    ! panel that owns its rectangle, so a click there is swallowed rather
    ! than falling through to the document underneath.
    integer, parameter :: REGION_NONE = 0
    integer, parameter :: REGION_TAB = 1          ! payload: tab index
    integer, parameter :: REGION_FUSS_TOGGLE = 2  ! payload: unused
    integer, parameter :: REGION_TREE_ROW = 3     ! payload: tree item index
    integer, parameter :: REGION_BLOCK = 4        ! payload: unused
    integer, parameter :: REGION_CTX_ROW = 5      ! payload: context-menu row
    ! payload -1 scroll left, +1 scroll right
    integer, parameter :: REGION_TAB_SCROLL = 6

    ! Fixed capacity, so a frame costs no allocation. Overflow drops the
    ! extra regions: they simply stay unclickable, which degrades to the
    ! behaviour that existed before this module.
    integer, parameter :: MAX_REGIONS = 256

    type :: clickable_region_t
        integer :: kind = REGION_NONE
        integer :: row0 = 0, row1 = 0    ! inclusive, 1-based screen rows
        integer :: col0 = 0, col1 = 0    ! inclusive, 1-based screen columns
        integer :: payload = 0
    end type clickable_region_t

    type(clickable_region_t) :: regions(MAX_REGIONS)
    integer :: n_regions = 0

contains

    !> Discard the previous frame's regions. Call once per full redraw.
    subroutine regions_begin_frame()
        n_regions = 0
    end subroutine regions_begin_frame

    !> Record a rectangle. Bounds are inclusive; an empty or inverted
    !> rectangle is ignored so callers need not special-case a zero-width
    !> label or a panel that is currently collapsed.
    subroutine region_add(kind, row0, row1, col0, col1, payload)
        integer, intent(in) :: kind, row0, row1, col0, col1
        integer, intent(in), optional :: payload

        if (n_regions >= MAX_REGIONS) return
        if (row1 < row0 .or. col1 < col0) return

        n_regions = n_regions + 1
        regions(n_regions)%kind = kind
        regions(n_regions)%row0 = row0
        regions(n_regions)%row1 = row1
        regions(n_regions)%col0 = col0
        regions(n_regions)%col1 = col1
        if (present(payload)) then
            regions(n_regions)%payload = payload
        else
            regions(n_regions)%payload = 0
        end if
    end subroutine region_add

    !> The region under a screen cell, or kind == REGION_NONE if there is
    !> none. Searches backwards so the most recently added region wins:
    !> later draws paint over earlier ones, so later regions are on top.
    function region_at(row, col) result(hit)
        integer, intent(in) :: row, col
        type(clickable_region_t) :: hit
        integer :: i

        do i = n_regions, 1, -1
            if (row >= regions(i)%row0 .and. row <= regions(i)%row1 .and. &
                col >= regions(i)%col0 .and. col <= regions(i)%col1) then
                hit = regions(i)
                return
            end if
        end do

        hit%kind = REGION_NONE
        hit%row0 = 0
        hit%row1 = 0
        hit%col0 = 0
        hit%col1 = 0
        hit%payload = 0
    end function region_at

    !> How many regions the current frame registered. For tests.
    function region_count() result(n)
        integer :: n
        n = n_regions
    end function region_count

end module clickable_region_module
