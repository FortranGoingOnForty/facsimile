program test_group_picker
    ! The "new tab group" dialog, with no editor and no terminal.
    !
    ! The invariant this suite exists for: **ticks survive walking to another
    ! directory**. The selection is a set of paths rather than a flag on each
    ! listed row, because walking re-lists different rows -- per-row flags
    ! would drop every tick the moment you moved, which would make the ../ row
    ! pointless, since assembling a group that spans directories is the only
    ! reason it is there.
    use group_picker_module
    implicit none

    integer :: nfail
    character(len=*), parameter :: ROOT = '/tmp/fac_gp_test'

    nfail = 0

    call make_tree()

    call test_it_lists_a_directory()
    call test_space_ticks_a_file()
    call test_ticks_survive_walking_away_and_back()
    call test_a_group_can_span_directories()
    call test_directories_are_walked_not_ticked()
    call test_confirm_refuses_an_empty_selection()
    call test_escape_cancels()
    call test_the_hit_test_agrees_with_the_layout()

    call remove_tree()

    if (nfail == 0) then
        print '(a)', 'test_group_picker: all passed'
    else
        print '(a,i0,a)', 'test_group_picker: ', nfail, ' FAILED'
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

    subroutine make_tree()
        call execute_command_line('rm -rf ' // ROOT // &
            ' && mkdir -p ' // ROOT // '/sub' // &
            ' && touch ' // ROOT // '/alpha.c ' // ROOT // '/beta.c ' // &
                            ROOT // '/sub/gamma.c', wait=.true.)
    end subroutine make_tree

    subroutine remove_tree()
        call execute_command_line('rm -rf ' // ROOT, wait=.true.)
    end subroutine remove_tree

    subroutine key(k)
        character(len=*), intent(in) :: k
        logical :: handled
        handled = group_picker_handle_key(k)
    end subroutine key

    subroutine space()
        logical :: handled
        handled = group_picker_handle_key(' ')
    end subroutine space

    !> Tick every file in the current directory, leaving directories alone.
    !>
    !> Pressing space on a directory walks into it, which is deliberate but
    !> would make a blind "press space on every row" helper wander off. So ask
    !> which rows are files.
    subroutine tick_all_files()
        integer :: i, n

        call key('down')                      ! name -> list, or move within it
        call key('home')                      ! either way we are in the list now
        n = group_picker_item_count()
        do i = 1, n
            if (group_picker_item_is_dir(i)) cycle
            call goto_row(i)
            call space()
        end do
    end subroutine tick_all_files

    !> Put the list selection on row `target`.
    subroutine goto_row(target)
        integer, intent(in) :: target
        integer :: guard

        call key('home')
        guard = 0
        do while (group_picker_selected() < target .and. guard < 600)
            call key('down')
            guard = guard + 1
        end do
    end subroutine goto_row

    subroutine test_it_lists_a_directory()
        logical :: shown

        shown = group_picker_show(ROOT, 40, 120)
        call check(shown, 'the dialog opens on a real directory')
        call check(is_group_picker_visible(), 'and reports itself visible')
        call check(group_picker_result() == GP_PENDING, 'with no result yet')
        call check(len(group_picker_name()) > 0, 'and a name pre-filled from the directory')
        call check(index(group_picker_name(), 'fac_gp_test') > 0, &
                   'which is the directory basename')
        call group_picker_hide()
    end subroutine test_it_lists_a_directory

    subroutine test_space_ticks_a_file()
        logical :: shown

        shown = group_picker_show(ROOT, 40, 120)
        call check(group_picker_count() == 0, 'nothing ticked to begin with')
        call tick_all_files()
        call check(group_picker_count() >= 2, &
                   'space ticks the files in the directory')
        call group_picker_hide()
    end subroutine test_space_ticks_a_file

    ! THE invariant. Tick here, walk away, walk back: the ticks are still there.
    subroutine test_ticks_survive_walking_away_and_back()
        logical :: shown
        integer :: before

        shown = group_picker_show(ROOT, 40, 120)
        call tick_all_files()
        before = group_picker_count()
        call check(before >= 2, 'some files are ticked')

        ! Walk up to the parent. tick_all_files left the focus on the list.
        call key('home')
        call key('left')                      ! to the parent
        call check(group_picker_count() == before, &
                   'walking to the parent keeps every tick')

        call check(group_picker_dir() /= ROOT, 'we really did move')
        call group_picker_hide()
    end subroutine test_ticks_survive_walking_away_and_back

    subroutine test_a_group_can_span_directories()
        logical :: shown
        integer :: after_root

        shown = group_picker_show(ROOT, 40, 120)
        call tick_all_files()
        after_root = group_picker_count()

        ! Into the subdirectory, and tick what is there too.
        call key('home')
        call key('down')                      ! past ../ onto sub/
        call key('enter')                     ! walk in
        call tick_all_files()
        call check(group_picker_count() > after_root, &
                   'files from a second directory join the same selection')
        call group_picker_hide()
    end subroutine test_a_group_can_span_directories

    subroutine test_directories_are_walked_not_ticked()
        logical :: shown
        integer :: before

        shown = group_picker_show(ROOT, 40, 120)
        call key('down')
        call key('home')                      ! the ../ row
        before = group_picker_count()
        call space()
        call check(group_picker_count() == before, &
                   'space on ../ does not tick anything')
        call group_picker_hide()
    end subroutine test_directories_are_walked_not_ticked

    subroutine test_confirm_refuses_an_empty_selection()
        logical :: shown

        shown = group_picker_show(ROOT, 40, 120)
        call key('enter')                     ! from the name field
        call check(group_picker_result() == GP_PENDING, &
                   'enter with nothing ticked does not create a group')
        call check(is_group_picker_visible(), 'and leaves the dialog open')

        call tick_all_files()
        call key('enter')
        call check(group_picker_result() == GP_CONFIRMED, &
                   'enter with files ticked confirms')
        call check(.not. is_group_picker_visible(), 'and closes the dialog')
        call check(group_picker_count() >= 2, 'handing back the ticked paths')
        call check(len(group_picker_path(1)) > 0, 'each of which is a real path')
    end subroutine test_confirm_refuses_an_empty_selection

    subroutine test_escape_cancels()
        logical :: shown

        shown = group_picker_show(ROOT, 40, 120)
        call tick_all_files()
        call key('esc')
        call check(group_picker_result() == GP_CANCELLED, 'escape cancels')
        call check(.not. is_group_picker_visible(), 'and closes the dialog')
    end subroutine test_escape_cancels

    ! The hit test repeats the renderer's row arithmetic, so it can drift.
    subroutine test_the_hit_test_agrees_with_the_layout()
        logical :: shown
        integer :: r, idx, found

        shown = group_picker_show(ROOT, 40, 120)
        found = 0
        do r = 1, 40
            idx = group_picker_row_at(r, 60)
            if (idx > 0) found = found + 1
        end do
        call check(found > 0, 'some screen rows map to items')
        call check(group_picker_row_at(1, 60) == 0, &
                   'a row above the dialog maps to nothing')
        call check(group_picker_row_at(20, 1) == 0, &
                   'a column outside the dialog maps to nothing')
        call group_picker_hide()
        call check(group_picker_row_at(10, 60) == 0, &
                   'and nothing at all once hidden')
    end subroutine test_the_hit_test_agrees_with_the_layout

end program test_group_picker
