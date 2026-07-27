program test_tab_identity
    ! Tabs must keep their identity when the array is rebuilt.
    !
    ! Closing a tab compacts `tabs`, so every index above the removed one
    ! shifts down. Code that remembers "the active tab" as an index therefore
    ! finds a *different* tab there afterwards -- the same shape as the bug
    ! that let a closed pane's text be written over its sibling's file. The
    ! fix is an id that survives the rebuild.
    !
    ! The second half covers move_tab. Growth and compaction now transfer
    ! ownership with move_alloc instead of deep-copying every open document,
    ! which is what makes opening a directory linear rather than quadratic.
    ! The hazard there is aliasing, not speed: two tabs must never end up
    ! sharing one buffer.
    use iso_fortran_env, only: int32
    use editor_state_module
    use text_buffer_module
    implicit none

    integer :: nfail

    nfail = 0

    call test_ids_are_unique_and_monotonic()
    call test_closing_the_active_tab_lands_on_a_real_neighbour()
    call test_closing_a_later_tab_leaves_the_active_one_alone()
    call test_find_by_id_reports_a_closed_tab_as_gone()
    call test_moved_tabs_do_not_alias()

    if (nfail == 0) then
        print '(a)', 'test_tab_identity: all passed'
    else
        print '(a,i0,a)', 'test_tab_identity: ', nfail, ' FAILED'
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

    subroutine three_tabs(editor)
        type(editor_state_t), intent(out) :: editor

        call init_editor(editor)
        call create_tab(editor, 'alpha.txt')
        call create_tab(editor, 'beta.txt')
        call create_tab(editor, 'gamma.txt')
    end subroutine three_tabs

    subroutine test_ids_are_unique_and_monotonic()
        type(editor_state_t) :: editor

        call three_tabs(editor)
        call check(size(editor%tabs) == 3, 'three tabs exist')
        call check(editor%tabs(1)%tab_id /= editor%tabs(2)%tab_id .and. &
                   editor%tabs(2)%tab_id /= editor%tabs(3)%tab_id .and. &
                   editor%tabs(1)%tab_id /= editor%tabs(3)%tab_id, &
                   'every tab has a distinct id')
        call check(editor%tabs(1)%tab_id < editor%tabs(2)%tab_id .and. &
                   editor%tabs(2)%tab_id < editor%tabs(3)%tab_id, &
                   'ids are handed out in order')
        call check(all(editor%tabs(:)%tab_id > 0), 'no tab has a null id')
    end subroutine test_ids_are_unique_and_monotonic

    ! The reported shape: close the ACTIVE first tab of three. The index stays
    ! 1, so anything comparing indices sees no change -- but tab 1 is now a
    ! different file, and its buffer must be the one that belongs to it.
    subroutine test_closing_the_active_tab_lands_on_a_real_neighbour()
        type(editor_state_t) :: editor
        integer(int32) :: beta_id

        call three_tabs(editor)
        editor%active_tab_index = 1
        beta_id = editor%tabs(2)%tab_id

        call close_tab(editor, 1)

        call check(size(editor%tabs) == 2, 'one tab was removed')
        call check(editor%active_tab_index >= 1 .and. &
                   editor%active_tab_index <= size(editor%tabs), &
                   'the active index is in range')
        call check(editor%tabs(editor%active_tab_index)%tab_id == beta_id, &
                   'the active tab is the one that was to its right')
        call check(editor%tabs(editor%active_tab_index)%filename == 'beta.txt', &
                   'and it carries its own filename')
    end subroutine test_closing_the_active_tab_lands_on_a_real_neighbour

    subroutine test_closing_a_later_tab_leaves_the_active_one_alone()
        type(editor_state_t) :: editor
        integer(int32) :: alpha_id

        call three_tabs(editor)
        editor%active_tab_index = 1
        alpha_id = editor%tabs(1)%tab_id

        call close_tab(editor, 3)      ! close a tab that is not active

        call check(editor%tabs(editor%active_tab_index)%tab_id == alpha_id, &
                   'closing another tab does not move the active one')
        call check(editor%tabs(editor%active_tab_index)%filename == 'alpha.txt', &
                   'and it still names its own file')
    end subroutine test_closing_a_later_tab_leaves_the_active_one_alone

    ! An id is never reused, so a stale one must resolve to nothing rather
    ! than to whatever later took that slot.
    subroutine test_find_by_id_reports_a_closed_tab_as_gone()
        type(editor_state_t) :: editor
        integer(int32) :: gone_id

        call three_tabs(editor)
        gone_id = editor%tabs(2)%tab_id
        call check(find_tab_by_id(editor, gone_id) == 2, 'an open tab is found')

        call close_tab(editor, 2)
        call check(find_tab_by_id(editor, gone_id) == 0, &
                   'a closed tab is reported as gone, not as its replacement')
        call check(find_tab_by_id(editor, 99999) == 0, 'an unknown id is gone')
        call check(find_tab_by_id(editor, 0) == 0, 'a null id is gone')

        call create_tab(editor, 'delta.txt')
        call check(find_tab_by_id(editor, gone_id) == 0, &
                   'and a new tab does not inherit the dead id')
    end subroutine test_find_by_id_reports_a_closed_tab_as_gone

    ! move_tab hands over pointers. If it ever left two tabs pointing at one
    ! buffer, editing through one would silently change the other -- and
    ! saving would write it to the wrong file.
    subroutine test_moved_tabs_do_not_alias()
        type(editor_state_t) :: editor
        integer :: i
        character(len=16) :: name
        logical :: distinct

        call init_editor(editor)
        do i = 1, 40
            write(name, '(a,i0,a)') 'f', i, '.txt'
            call create_tab(editor, trim(name))
            ! Give each tab text of its own so a shared buffer is detectable.
            call buffer_insert(editor%tabs(i)%buffer, 1, 'body of ' // trim(name))
        end do

        call check(size(editor%tabs) == 40, 'forty tabs were created')

        distinct = .true.
        do i = 1, 40
            write(name, '(a,i0,a)') 'f', i, '.txt'
            if (editor%tabs(i)%filename /= trim(name)) distinct = .false.
        end do
        call check(distinct, 'every tab kept its own filename through the growth')

        ! Mutating one tab must not disturb its neighbour.
        call buffer_insert(editor%tabs(1)%buffer, 1, 'XX')
        call check(index(buffer_get_line(editor%tabs(2)%buffer, 1), 'XX') == 0, &
                   'editing one tab does not change another')

        ! And the same after a compaction.
        call close_tab(editor, 20)
        call check(size(editor%tabs) == 39, 'a tab was removed')
        call buffer_insert(editor%tabs(1)%buffer, 1, 'YY')
        call check(index(buffer_get_line(editor%tabs(2)%buffer, 1), 'YY') == 0, &
                   'and still does not after the array is compacted')
    end subroutine test_moved_tabs_do_not_alias

end program test_tab_identity
