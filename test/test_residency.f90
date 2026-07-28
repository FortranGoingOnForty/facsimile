program test_residency
    ! Deferred tabs: a group member whose file has not been read yet.
    !
    ! The whole feature turns on one question -- can a buffer that was never
    ! read reach a save path? If it can, the answer is an empty file where a
    ! real one used to be. So residency is derived from the ALLOCATION rather
    ! than tracked in a flag: a flag can drift, and the allocation is what
    ! buffer_save_file ultimately asks about anyway.
    !
    ! `modified` is deliberately not a guard here. sync_buffer_to_all_instances
    ! sets it on any tab whose filename matches the one being edited, with no
    ! regard for whether that tab holds text -- so a deferred tab could be
    ! marked dirty and then "saved" over a real file.
    use iso_fortran_env, only: int32
    use editor_state_module
    use text_buffer_module
    implicit none

    integer :: nfail
    character(len=*), parameter :: DIR  = '/tmp/fac_resident_test'
    character(len=*), parameter :: BODY = 'REAL CONTENT ON DISK'

    nfail = 0
    call make_files()

    call test_a_deferred_tab_holds_nothing()
    call test_it_cannot_be_saved()
    call test_it_cannot_be_saved_even_when_marked_modified()
    call test_editing_another_copy_does_not_touch_it()
    call test_hydrating_reads_the_file()
    call test_switching_hydrates()

    call remove_files()

    if (nfail == 0) then
        print '(a)', 'test_residency: all passed'
    else
        print '(a,i0,a)', 'test_residency: ', nfail, ' FAILED'
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

    subroutine make_files()
        integer :: u, i
        character(len=8) :: n

        call execute_command_line('rm -rf ' // DIR // ' && mkdir -p ' // DIR, wait=.true.)
        do i = 1, 3
            write(n, '(a,i0,a)') 'f', i, '.txt'
            open(newunit=u, file=DIR // '/' // trim(n), status='replace', action='write')
            write(u, '(a)') BODY
            close(u)
        end do
    end subroutine make_files

    subroutine remove_files()
        call execute_command_line('rm -rf ' // DIR, wait=.true.)
    end subroutine remove_files

    function on_disk(name) result(text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: text
        character(len=256) :: line
        integer :: u, ios

        text = ''
        open(newunit=u, file=DIR // '/' // name, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(u, '(a)', iostat=ios) line
        if (ios == 0) text = trim(line)
        close(u)
    end function on_disk

    !> Three tabs, all deferred but the first.
    subroutine setup(editor)
        type(editor_state_t), intent(out) :: editor
        integer :: i, st

        call init_editor(editor)
        call create_tab(editor, DIR // '/f1.txt')
        call create_tab(editor, DIR // '/f2.txt')
        call create_tab(editor, DIR // '/f3.txt')
        do i = 2, 3
            call defer_tab(editor, i)
        end do
    end subroutine setup

    subroutine test_a_deferred_tab_holds_nothing()
        type(editor_state_t) :: editor

        call setup(editor)
        call check(.not. tab_is_resident(editor, 2), 'a deferred tab is not resident')
        call check(.not. allocated(editor%tabs(2)%panes(1)%buffer%data), &
                   'and its buffer is genuinely unallocated, not merely empty')
        call check(.not. editor%tabs(2)%modified, 'a tab read from nowhere is not modified')
        call check(allocated(editor%tabs(2)%filename), &
                   'but it still knows which file it stands for')
    end subroutine test_a_deferred_tab_holds_nothing

    ! The failure this feature could cause, asserted directly against the disk.
    subroutine test_it_cannot_be_saved()
        type(editor_state_t) :: editor
        integer :: status

        call setup(editor)
        call check(on_disk('f2.txt') == BODY, 'the file starts with its contents')

        call save_tab_pane(editor, 2, 1, status)
        call check(status /= 0, 'saving a deferred tab reports failure')
        call check(on_disk('f2.txt') == BODY, &
                   'and the file on disk is untouched')
    end subroutine test_it_cannot_be_saved

    ! `modified` is set by the sync path without regard to residency, so it
    ! must not be what protects the file.
    subroutine test_it_cannot_be_saved_even_when_marked_modified()
        type(editor_state_t) :: editor
        integer :: status

        call setup(editor)
        editor%tabs(2)%modified = .true.        ! exactly what the sync path does

        call save_tab_pane(editor, 2, 1, status)
        call check(status /= 0, 'still refuses when the tab claims to be dirty')
        call check(on_disk('f2.txt') == BODY, &
                   'and the file is still untouched')
    end subroutine test_it_cannot_be_saved_even_when_marked_modified

    !> Two tabs on one file, one of them deferred: editing the live one must
    !> not push text into the deferred one, nor mark it dirty.
    subroutine test_editing_another_copy_does_not_touch_it()
        type(editor_state_t) :: editor
        type(buffer_t) :: work

        call init_editor(editor)
        call create_tab(editor, DIR // '/f1.txt')
        call create_tab(editor, DIR // '/f1.txt')   ! same file, second tab
        call defer_tab(editor, 2)

        call init_buffer(work, 'EDITED')
        call sync_buffer_to_all_instances(editor, DIR // '/f1.txt', work)

        call check(.not. tab_is_resident(editor, 2), &
                   'the deferred copy stays deferred')
        call check(.not. editor%tabs(2)%modified, &
                   'and is not marked modified, which would invite a save')
        call cleanup_buffer(work)
    end subroutine test_editing_another_copy_does_not_touch_it

    subroutine test_hydrating_reads_the_file()
        type(editor_state_t) :: editor
        integer :: status
        character(len=:), allocatable :: first

        call setup(editor)
        call hydrate_tab(editor, 2, status)
        call check(status == 0, 'hydrating succeeds')
        call check(tab_is_resident(editor, 2), 'the tab is resident afterwards')

        first = buffer_get_line(editor%tabs(2)%panes(1)%buffer, 1)
        call check(index(first, 'REAL CONTENT') > 0, &
                   'and holds the text that was on disk')

        ! And now it can be saved, because there is something to save.
        call save_tab_pane(editor, 2, 1, status)
        call check(status == 0, 'a hydrated tab saves normally')
        call check(on_disk('f2.txt') == BODY, 'writing back what it read')
    end subroutine test_hydrating_reads_the_file

    subroutine test_switching_hydrates()
        type(editor_state_t) :: editor
        type(buffer_t) :: work

        call setup(editor)
        call init_buffer(work)
        call check(.not. tab_is_resident(editor, 3), 'tab 3 starts deferred')

        call switch_to_tab_with_buffer(editor, 3, work)
        call check(tab_is_resident(editor, 3), &
                   'switching to a deferred tab reads it in')
        call check(index(buffer_get_line(work, 1), 'REAL CONTENT') > 0, &
                   'and the working buffer holds its text')
        call cleanup_buffer(work)
    end subroutine test_switching_hydrates

end program test_residency
