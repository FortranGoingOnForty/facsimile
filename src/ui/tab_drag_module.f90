!> What the pointer is holding, while it is holding it.
!>
!> Its own module because BOTH ends need it and neither can own it: the
!> renderer draws the drag, the command handler drives it, and the renderer is
!> compiled first so it cannot ask the handler. A holder with accessors, no
!> logic beyond bookkeeping.
!>
!> **Nothing in editor%tabs changes until the drop.** The drag carries a
!> target; the renderer draws the bar as if the held entry were already there;
!> release applies it once. Swapping live would look identical and be far
!> worse underneath -- snapping back would mean undoing an arbitrary number of
!> moves, a drag that wandered off the bar would leave the array half
!> shuffled, and dropping a tab into the document (the next feature) must not
!> have quietly reordered the bar on the way.
!>
!> One consequence worth naming: because the array is frozen for the drag's
!> lifetime, a tab INDEX is stable for that lifetime, which is why the held
!> entry can be matched by payload while it is being drawn. The commit still
!> resolves by path, and a change in tab count cancels the drag outright.
module tab_drag_module
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: DRAG_NONE, DRAG_TAB, DRAG_GROUP
    public :: drag_begin, drag_cancel, drag_arm_only
    public :: drag_is_armed, drag_is_showing, drag_kind
    public :: drag_payload, drag_path, drag_gid, drag_from_row
    public :: drag_label, drag_pointer_row, drag_pointer_col
    public :: drag_set_pointer, drag_press_row, drag_press_col
    public :: drag_to_row, drag_to_slot, drag_to_gid, drag_has_target
    public :: drag_set_target, drag_clear_target, drag_tab_count
    public :: SPLIT_NONE, SPLIT_LEFT, SPLIT_RIGHT, SPLIT_BELOW
    public :: drag_set_split, drag_split_side, drag_split_rect

    integer, parameter :: DRAG_NONE = 0, DRAG_TAB = 1, DRAG_GROUP = 2

    ! Where a drop into the document would put the held tab. A split target
    ! and a strip target are mutually exclusive: the pointer is either on the
    ! tab bar or in the document, never both.
    integer, parameter :: SPLIT_NONE = 0, SPLIT_LEFT = 1
    integer, parameter :: SPLIT_RIGHT = 2, SPLIT_BELOW = 3

    integer :: g_kind = DRAG_NONE
    logical :: g_armed = .false.      ! pressed, threshold not yet crossed
    logical :: g_showing = .false.    ! the pointer has moved: really dragging

    character(len=:), allocatable :: g_path      ! identity, for the commit
    character(len=:), allocatable :: g_label     ! what the ghost reads
    integer(int32) :: g_gid = 0
    integer :: g_payload = 0          ! +tab index, or -(group id)
    integer :: g_from_row = 0
    integer :: g_tab_count = 0        ! cancel if this changes mid-drag

    integer :: g_press_row = 0, g_press_col = 0
    integer :: g_row = 0, g_col = 0

    integer :: g_split_side = SPLIT_NONE
    integer :: g_split_r0 = 0, g_split_c0 = 0, g_split_r1 = 0, g_split_c1 = 0

    logical :: g_has_target = .false.
    integer :: g_to_row = 0
    integer :: g_to_slot = 0
    integer(int32) :: g_to_gid = 0

contains

    !> Record a press on a tab-bar entry. Not yet a drag: a click that never
    !> moves has to stay a click, so the drag only starts once the pointer
    !> leaves the pressed cell.
    subroutine drag_arm_only(kind, payload, path, label, gid, from_row, &
                             press_row, press_col, tab_count)
        integer, intent(in) :: kind, payload, from_row, press_row, press_col
        integer, intent(in) :: tab_count
        character(len=*), intent(in) :: path, label
        integer(int32), intent(in) :: gid

        g_kind = kind
        g_payload = payload
        g_path = path
        g_label = label
        g_gid = gid
        g_from_row = from_row
        g_press_row = press_row
        g_press_col = press_col
        g_row = press_row
        g_col = press_col
        g_tab_count = tab_count
        g_armed = .true.
        g_showing = .false.
        call drag_clear_target()
    end subroutine drag_arm_only

    !> Promote an armed press to a live drag.
    subroutine drag_begin()
        if (.not. g_armed) return
        g_showing = .true.
    end subroutine drag_begin

    subroutine drag_cancel()
        g_kind = DRAG_NONE
        g_armed = .false.
        g_showing = .false.
        g_payload = 0
        g_gid = 0
        g_from_row = 0
        g_tab_count = 0
        if (allocated(g_path)) deallocate(g_path)
        if (allocated(g_label)) deallocate(g_label)
        call drag_clear_target()
    end subroutine drag_cancel

    logical function drag_is_armed()
        drag_is_armed = g_armed
    end function drag_is_armed

    !> True once the drag is real, which is also when anything gets drawn.
    logical function drag_is_showing()
        drag_is_showing = g_armed .and. g_showing
    end function drag_is_showing

    integer function drag_kind()
        drag_kind = g_kind
    end function drag_kind

    integer function drag_payload()
        drag_payload = g_payload
    end function drag_payload

    function drag_path() result(p)
        character(len=:), allocatable :: p
        if (allocated(g_path)) then
            p = g_path
        else
            p = ''
        end if
    end function drag_path

    function drag_label() result(t)
        character(len=:), allocatable :: t
        if (allocated(g_label)) then
            t = g_label
        else
            t = ''
        end if
    end function drag_label

    integer(int32) function drag_gid()
        drag_gid = g_gid
    end function drag_gid

    integer function drag_from_row()
        drag_from_row = g_from_row
    end function drag_from_row

    integer function drag_tab_count()
        drag_tab_count = g_tab_count
    end function drag_tab_count

    integer function drag_press_row()
        drag_press_row = g_press_row
    end function drag_press_row

    integer function drag_press_col()
        drag_press_col = g_press_col
    end function drag_press_col

    subroutine drag_set_pointer(row, col)
        integer, intent(in) :: row, col
        g_row = row
        g_col = col
    end subroutine drag_set_pointer

    integer function drag_pointer_row()
        drag_pointer_row = g_row
    end function drag_pointer_row

    integer function drag_pointer_col()
        drag_pointer_col = g_col
    end function drag_pointer_col

    !> Where the drop would land: `to_row` 1 or 2, `slot` an entry position on
    !> that strip, `gid` the group when the target is a member row.
    subroutine drag_set_target(to_row, slot, gid)
        integer, intent(in) :: to_row, slot
        integer(int32), intent(in) :: gid

        g_has_target = .true.
        g_to_row = to_row
        g_to_slot = slot
        g_to_gid = gid
    end subroutine drag_set_target

    subroutine drag_clear_target()
        g_has_target = .false.
        g_to_row = 0
        g_to_slot = 0
        g_to_gid = 0
        g_split_side = SPLIT_NONE
    end subroutine drag_clear_target

    !> A drop here would split, along `side`, filling the given rectangle.
    !> The rectangle is worked out by the caller, which knows the pane
    !> geometry; the renderer only paints it.
    subroutine drag_set_split(side, r0, c0, r1, c1)
        integer, intent(in) :: side, r0, c0, r1, c1

        g_has_target = .false.        ! not a strip drop
        g_to_row = 0
        g_to_slot = 0
        g_to_gid = 0
        g_split_side = side
        g_split_r0 = r0
        g_split_c0 = c0
        g_split_r1 = r1
        g_split_c1 = c1
    end subroutine drag_set_split

    integer function drag_split_side()
        drag_split_side = g_split_side
    end function drag_split_side

    subroutine drag_split_rect(r0, c0, r1, c1)
        integer, intent(out) :: r0, c0, r1, c1
        r0 = g_split_r0
        c0 = g_split_c0
        r1 = g_split_r1
        c1 = g_split_c1
    end subroutine drag_split_rect



    logical function drag_has_target()
        drag_has_target = g_has_target
    end function drag_has_target

    integer function drag_to_row()
        drag_to_row = g_to_row
    end function drag_to_row

    integer function drag_to_slot()
        drag_to_slot = g_to_slot
    end function drag_to_slot

    integer(int32) function drag_to_gid()
        drag_to_gid = g_to_gid
    end function drag_to_gid

end module tab_drag_module
