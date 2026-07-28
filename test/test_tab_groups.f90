program test_tab_groups
    ! Tab groups: sub-workspaces over the flat tab list.
    !
    ! The invariants that matter are all about not storing what can be derived.
    ! A group keeps no member list, because closing a tab renumbers the array
    ! and any stored list would be stale one close later. The member count is
    ! computed, so the "src/ (4)" label cannot claim four when three are open.
    ! And the active group is derived from the active tab, because
    ! active_tab_index is assigned raw in the main loop and the workspace
    ! restore, where a cached group would silently fall out of step.
    use iso_fortran_env, only: int32
    use editor_state_module
    implicit none

    integer :: nfail

    nfail = 0

    call test_create_and_find()
    call test_membership_and_count()
    call test_the_label_cannot_lie()
    call test_ordinals_compact_when_a_member_leaves()
    call test_a_group_dissolves_when_emptied()
    call test_a_tab_belongs_to_one_group()
    call test_active_group_follows_the_active_tab()
    call test_closing_a_member_updates_the_group()
    call test_prune_empty_groups()

    if (nfail == 0) then
        print '(a)', 'test_tab_groups: all passed'
    else
        print '(a,i0,a)', 'test_tab_groups: ', nfail, ' FAILED'
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

    !> Three tabs, and a group holding the first two.
    subroutine setup(editor, gid)
        type(editor_state_t), intent(out) :: editor
        integer(int32), intent(out) :: gid

        call init_editor(editor)
        call create_tab(editor, 'src/a.c')
        call create_tab(editor, 'src/b.c')
        call create_tab(editor, 'docs/read.md')
        call group_create(editor, 'src', '', gid)
        call group_add_member(editor, gid, 1)
        call group_add_member(editor, gid, 2)
    end subroutine setup

    subroutine test_create_and_find()
        type(editor_state_t) :: editor
        integer(int32) :: gid

        call init_editor(editor)
        call check(size(editor%groups) == 0, 'a fresh editor has no groups')

        call group_create(editor, 'src', '', gid)
        call check(gid > 0, 'creating a group yields an id')
        call check(size(editor%groups) == 1, 'and adds it to the array')
        call check(group_find(editor, gid) == 1, 'and it can be found by id')
        call check(group_find(editor, 9999_int32) == 0, 'an unknown id is not found')
        call check(group_find(editor, 0_int32) == 0, 'nor is a null id')
    end subroutine test_create_and_find

    subroutine test_membership_and_count()
        type(editor_state_t) :: editor
        integer(int32) :: gid
        integer, allocatable :: members(:)

        call setup(editor, gid)
        call check(group_member_count(editor, gid) == 2, 'two members')
        call check(editor%tabs(3)%group_id == 0, 'the third tab is ungrouped')

        call group_members(editor, gid, members)
        call check(size(members) == 2, 'the member list has two entries')
        call check(members(1) == 1 .and. members(2) == 2, &
                   'and is in ordinal order')
    end subroutine test_membership_and_count

    ! The count is derived, so it tracks the tabs rather than a stored number.
    subroutine test_the_label_cannot_lie()
        type(editor_state_t) :: editor
        integer(int32) :: gid

        call setup(editor, gid)
        call check(group_label(editor, gid) == 'src/ (2)', &
                   'the label carries the live count')

        call group_add_member(editor, gid, 3)
        call check(group_label(editor, gid) == 'src/ (3)', &
                   'and follows a member joining')

        call group_remove_member(editor, 3)
        call check(group_label(editor, gid) == 'src/ (2)', &
                   'and a member leaving')
    end subroutine test_the_label_cannot_lie

    subroutine test_ordinals_compact_when_a_member_leaves()
        type(editor_state_t) :: editor
        integer(int32) :: gid

        call init_editor(editor)
        call create_tab(editor, 'a.c')
        call create_tab(editor, 'b.c')
        call create_tab(editor, 'c.c')
        call group_create(editor, 'src', '', gid)
        call group_add_member(editor, gid, 1)
        call group_add_member(editor, gid, 2)
        call group_add_member(editor, gid, 3)
        call check(editor%tabs(1)%group_ordinal == 1 .and. &
                   editor%tabs(2)%group_ordinal == 2 .and. &
                   editor%tabs(3)%group_ordinal == 3, 'ordinals are 1,2,3')

        call group_remove_member(editor, 2)      ! the middle one
        call check(editor%tabs(1)%group_ordinal == 1, 'the first keeps its place')
        call check(editor%tabs(3)%group_ordinal == 2, &
                   'and the third moves up to fill the gap')
        call check(editor%tabs(2)%group_ordinal == 0, &
                   'the departed tab has no ordinal')
    end subroutine test_ordinals_compact_when_a_member_leaves

    subroutine test_a_group_dissolves_when_emptied()
        type(editor_state_t) :: editor
        integer(int32) :: gid

        call setup(editor, gid)
        call group_remove_member(editor, 1)
        call check(group_find(editor, gid) > 0, 'one member left, group survives')

        call group_remove_member(editor, 2)
        call check(group_find(editor, gid) == 0, &
                   'the last member leaving dissolves the group')
        call check(size(editor%groups) == 0, 'and removes it from the array')
        call check(editor%tabs(1)%group_id == 0 .and. &
                   editor%tabs(2)%group_id == 0, 'its tabs are ungrouped, not orphaned')
    end subroutine test_a_group_dissolves_when_emptied

    ! Flat membership means one group per tab. Adding to a second must move it,
    ! not leave it counted in both.
    subroutine test_a_tab_belongs_to_one_group()
        type(editor_state_t) :: editor
        integer(int32) :: g1, g2

        call init_editor(editor)
        call create_tab(editor, 'a.c')
        call create_tab(editor, 'b.c')
        call group_create(editor, 'src', '', g1)
        call group_create(editor, 'docs', '', g2)
        call group_add_member(editor, g1, 1)
        call group_add_member(editor, g1, 2)
        call check(group_member_count(editor, g1) == 2, 'both in the first group')

        call group_add_member(editor, g2, 2)
        call check(editor%tabs(2)%group_id == g2, 'the tab moved to the second')
        call check(group_member_count(editor, g1) == 1, &
                   'and is no longer counted in the first')
        call check(group_member_count(editor, g2) == 1, 'nor twice in the second')
        call check(editor%tabs(1)%group_ordinal == 1, &
                   'the remaining member keeps a valid ordinal')
    end subroutine test_a_tab_belongs_to_one_group

    ! active_tab_index is assigned raw in several places, so this must be
    ! derived rather than cached.
    subroutine test_active_group_follows_the_active_tab()
        type(editor_state_t) :: editor
        integer(int32) :: gid

        call setup(editor, gid)

        editor%active_tab_index = 1
        call check(active_group_id(editor) == gid, 'inside the group')

        editor%active_tab_index = 3
        call check(active_group_id(editor) == 0, 'on an ungrouped tab')

        editor%active_tab_index = 2
        call check(active_group_id(editor) == gid, 'back inside')

        editor%active_tab_index = 0
        call check(active_group_id(editor) == 0, 'with no active tab at all')
    end subroutine test_active_group_follows_the_active_tab

    ! Closing goes through close_tab, which renumbers the array -- the case a
    ! stored member list would get wrong.
    subroutine test_closing_a_member_updates_the_group()
        type(editor_state_t) :: editor
        integer(int32) :: gid
        integer, allocatable :: members(:)

        call setup(editor, gid)
        call check(group_member_count(editor, gid) == 2, 'two members before')

        call close_tab(editor, 1)
        call check(group_member_count(editor, gid) == 1, &
                   'closing a member leaves one')
        call group_members(editor, gid, members)
        call check(size(members) == 1, 'and the member list agrees')
        if (size(members) == 1) then
            call check(editor%tabs(members(1))%filename == 'src/b.c', &
                       'the surviving member is the right file')
        end if

        call close_tab(editor, members(1))
        call check(group_find(editor, gid) == 0, &
                   'closing the last member dissolves the group')
    end subroutine test_closing_a_member_updates_the_group

    subroutine test_prune_empty_groups()
        type(editor_state_t) :: editor
        integer(int32) :: g1, g2

        call init_editor(editor)
        call create_tab(editor, 'a.c')
        call group_create(editor, 'src', '', g1)
        call group_create(editor, 'docs', '', g2)
        call group_add_member(editor, g1, 1)
        call check(size(editor%groups) == 2, 'two groups, one empty')

        call prune_empty_groups(editor)
        call check(size(editor%groups) == 1, 'the empty one is dropped')
        call check(group_find(editor, g1) > 0, 'and the populated one survives')
        call check(group_find(editor, g2) == 0, 'by id, not by position')
    end subroutine test_prune_empty_groups

end program test_tab_groups
