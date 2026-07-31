program test_tab_reorder
    ! The index arithmetic behind dragging a tab. Every one of these is a
    ! shuffle of an array whose positions other state refers to, which is
    ! exactly where an off-by-one hides and exactly what is tedious to see
    ! through a terminal -- so it is checked here rather than only on screen.
    !
    ! Three operations, three different representations of "order":
    !   reorder_tab        -- position in editor%tabs, which IS row 1's order
    !   reorder_group_block-- the same, for a whole group at once
    !   set_group_ordinal  -- group_ordinal, which is row 2's order, and which
    !                         must move NO tabs in the array
    use iso_fortran_env, only: int32
    use editor_state_module
    implicit none

    type(editor_state_t) :: editor
    integer :: nfail

    nfail = 0

    ! ---- moving one tab ------------------------------------------------
    call setup(5)
    call reorder_tab(editor, 2, 4)
    call check_order('a c d b e', 'right: the passed tabs keep their order')

    call setup(5)
    call reorder_tab(editor, 4, 2)
    call check_order('a d b c e', 'left: likewise')

    call setup(5)
    call reorder_tab(editor, 1, 5)
    call check_order('b c d e a', 'first to last')

    call setup(5)
    call reorder_tab(editor, 5, 1)
    call check_order('e a b c d', 'last to first')

    call setup(5)
    call reorder_tab(editor, 3, 3)
    call check_order('a b c d e', 'onto itself is a no-op')

    ! Out of range must not corrupt the array -- a drag can resolve a target
    ! outside it if the bar scrolled underneath.
    call setup(5)
    call reorder_tab(editor, 2, 99)
    call check_order('a b c d e', 'a target past the end is refused')
    call setup(5)
    call reorder_tab(editor, 0, 3)
    call check_order('a b c d e', 'and so is a source before the start')

    ! ---- the active tab follows -----------------------------------------
    call setup(5)
    editor%active_tab_index = 2
    call reorder_tab(editor, 2, 4)
    call check_active(4, 'the moved tab is still the active one')

    call setup(5)
    editor%active_tab_index = 3
    call reorder_tab(editor, 1, 5)
    call check_active(2, 'a tab passed from the left shifts down')

    call setup(5)
    editor%active_tab_index = 2
    call reorder_tab(editor, 5, 1)
    call check_active(3, 'a tab passed from the right shifts up')

    call setup(5)
    editor%active_tab_index = 1
    call reorder_tab(editor, 3, 4)
    call check_active(1, 'a move entirely to its right leaves it alone')

    ! ---- a group moves as a block ---------------------------------------
    ! b and d are one group, deliberately NOT adjacent, because that is the
    ! state the bar can reach: a group's entry sits at its first member.
    call setup(5)
    call join(2, 1, 1)
    call join(4, 1, 2)
    call reorder_group_block(editor, 1_int32, 4)
    call check_order('a c e b d', 'the block lands together, in ordinal order')

    call setup(5)
    call join(4, 1, 1)
    call join(5, 1, 2)
    call reorder_group_block(editor, 1_int32, 1)
    call check_order('d e a b c', 'and can move to the front')

    ! ---- row 2 order is ordinals, and moves nothing ----------------------
    call setup(4)
    call join(1, 1, 1)
    call join(2, 1, 2)
    call join(3, 1, 3)
    call set_group_ordinal(editor, 3, 1)
    call check_order('a b c d', 'reordering within a group moves no tabs')
    call check_ordinals([2, 3, 1, 0], 'the third member became the first')

    call setup(4)
    call join(1, 1, 1)
    call join(2, 1, 2)
    call join(3, 1, 3)
    call set_group_ordinal(editor, 1, 3)
    call check_ordinals([3, 1, 2, 0], 'and the first became the last')

    call setup(4)
    call join(1, 1, 1)
    call join(2, 1, 2)
    call set_group_ordinal(editor, 1, 99)
    call check_ordinals([2, 1, 0, 0], 'a slot past the end clamps to the end')

    if (nfail == 0) then
        print *, 'test_tab_reorder: all passed'
    else
        print *, 'test_tab_reorder: FAILED', nfail
        stop 1
    end if

contains

    !> n tabs named a, b, c, ... with no groups.
    subroutine setup(n)
        integer, intent(in) :: n
        integer :: i

        call cleanup_editor(editor)
        call init_editor(editor)
        if (allocated(editor%tabs)) deallocate(editor%tabs)
        allocate(editor%tabs(n))
        do i = 1, n
            editor%tabs(i)%filename = achar(iachar('a') + i - 1) // '.c'
            editor%tabs(i)%tab_id = int(i, int32)
            editor%tabs(i)%group_id = 0
            editor%tabs(i)%group_ordinal = 0
        end do
        editor%active_tab_index = 1
    end subroutine setup

    subroutine join(tab_idx, gid, ordinal)
        integer, intent(in) :: tab_idx, gid, ordinal

        editor%tabs(tab_idx)%group_id = int(gid, int32)
        editor%tabs(tab_idx)%group_ordinal = int(ordinal, int32)
    end subroutine join

    !> The tab names in array order, space separated: 'a c d b e'.
    function order_text() result(t)
        character(len=:), allocatable :: t
        integer :: i

        t = ''
        do i = 1, size(editor%tabs)
            if (i > 1) t = t // ' '
            t = t // editor%tabs(i)%filename(1:1)
        end do
    end function order_text

    subroutine check_order(want, name)
        character(len=*), intent(in) :: want, name

        if (order_text() == want) then
            print *, 'ok   ', name
        else
            print *, 'FAIL ', name, ' want [', want, '] got [', order_text(), ']'
            nfail = nfail + 1
        end if
    end subroutine check_order

    subroutine check_active(want, name)
        integer, intent(in) :: want
        character(len=*), intent(in) :: name

        if (editor%active_tab_index == want) then
            print *, 'ok   ', name
        else
            print *, 'FAIL ', name, ' want', want, ' got', editor%active_tab_index
            nfail = nfail + 1
        end if
    end subroutine check_active

    subroutine check_ordinals(want, name)
        integer, intent(in) :: want(:)
        character(len=*), intent(in) :: name
        integer :: i
        logical :: ok

        ok = .true.
        do i = 1, min(size(want), size(editor%tabs))
            if (editor%tabs(i)%group_ordinal /= want(i)) ok = .false.
        end do
        if (ok) then
            print *, 'ok   ', name
        else
            print *, 'FAIL ', name, ' got', &
                (editor%tabs(i)%group_ordinal, i = 1, size(editor%tabs))
            nfail = nfail + 1
        end if
    end subroutine check_ordinals

end program test_tab_reorder
