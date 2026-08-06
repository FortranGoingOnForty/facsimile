! UI display for Fortress navigator (simplified for fac)

module fortress_display_module
    use iso_fortran_env, only: output_unit
    use fortress_fs_module, only: MAX_PATH, MAX_FILES
    use utf8_module, only: clip_to_cells
    implicit none
    private

    public :: draw_fortress_interface

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: UNDERLINE = ESC // "[4m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"

contains

    subroutine draw_fortress_interface(r, c, current_dir, current_files, current_is_dir, current_is_exec, &
                                       current_count, parent_files, parent_is_dir, parent_count, &
                                       selected, parent_selected, scroll_offset, parent_scroll_offset, &
                                       first_draw, row0, col0, chrome)
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir
        character(len=*), dimension(*), intent(in) :: current_files, parent_files
        logical, dimension(*), intent(in) :: current_is_dir, parent_is_dir
        logical, dimension(*), intent(in) :: current_is_exec
        logical, intent(in), optional :: first_draw
        !> Top-left of the area to draw in. Absent means the whole screen.
        integer, intent(in), optional :: row0, col0
        !> Draw the header and footer. False when a frame already provides
        !> them, so a window does not carry two titles and two footers.
        logical, intent(in), optional :: chrome
        integer :: left_w, i, j, parent_idx, current_idx, vis_h
        character(len=256) :: parent_name, current_name
        logical :: do_clear
        integer :: r0, c0, name_w, used, list_top
        logical :: want_chrome
        character(len=:), allocatable :: shown

        ! Origin. Defaults to the whole screen, which is what the startup
        ! browser wants; a modal passes the inside of its box instead. Every
        ! row is POSITIONED rather than written sequentially, so nothing here
        ! depends on where the cursor happened to be left.
        r0 = 1
        c0 = 1
        if (present(row0)) r0 = row0
        if (present(col0)) c0 = col0

        want_chrome = .true.
        if (present(chrome)) want_chrome = chrome

        ! Calculate layout. Without the chrome the panes have the header,
        ! blank and footer rows back.
        left_w = c * 3 / 10
        if (want_chrome) then
            vis_h = r - 3
            list_top = r0 + 1
        else
            vis_h = r
            list_top = r0 - 1
        end if
        name_w = max(0, c - left_w - 3)      ! after the " | " separator

        ! Clearing the SCREEN is only ever right for the full-screen driver.
        do_clear = .false.
        if (present(first_draw)) do_clear = first_draw
        if (r0 /= 1 .or. c0 /= 1) do_clear = .false.

        write(output_unit, '(a)', advance='no') ESC // "[?25l"
        if (do_clear) write(output_unit, '(a)', advance='no') ESC // "[2J"

        if (want_chrome) then
            call move_to(r0, c0)
            call put_cells(BOLD // "FORTRESS" // RESET // " - " // trim(current_dir), &
                           "FORTRESS - " // trim(current_dir), c)
            call move_to(r0 + 1, c0)
            call put_cells("", "", c)
        end if

        do i = 1, vis_h
            parent_idx = i + parent_scroll_offset
            current_idx = i + scroll_offset

            call move_to(list_top + i, c0)

            ! === Parent pane (left 30%) ===
            parent_name = ''
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                parent_name = trim(adjustl(parent_files(parent_idx)))
                j = scan(parent_name, char(10)//char(13))
                if (j > 0) parent_name = parent_name(1:j-1)
                if (parent_is_dir(parent_idx)) parent_name = trim(parent_name) // "/"
            end if
            call clip_to_cells(trim(parent_name), left_w, shown, used)
            ! Active directory stands out (bold + underlined); other siblings
            ! are dimmed but still visible -- dirs in blue, files in default.
            ! (The old DIM+GREY rendered as near-invisible dark-on-dark.)
            if (used > 0 .and. parent_idx == parent_selected) then
                write(output_unit, '(a)', advance='no') BOLD // UNDERLINE // BLUE // shown // RESET
            else if (used > 0 .and. parent_is_dir(parent_idx)) then
                write(output_unit, '(a)', advance='no') DIM // BLUE // shown // RESET
            else if (used > 0) then
                write(output_unit, '(a)', advance='no') DIM // shown // RESET
            end if
            if (left_w - used > 0) &
                write(output_unit, '(a)', advance='no') repeat(' ', left_w - used)

            write(output_unit, '(a)', advance='no') " " // char(226)//char(148)//char(130) // " "

            ! === Current pane (right) ===
            current_name = ''
            if (current_idx >= 1 .and. current_idx <= current_count) then
                current_name = trim(adjustl(current_files(current_idx)))
                j = scan(current_name, char(10)//char(13))
                if (j > 0) current_name = current_name(1:j-1)
                if (current_is_dir(current_idx)) current_name = trim(current_name) // "/"
            end if
            call clip_to_cells(trim(current_name), name_w, shown, used)
            if (used > 0 .and. current_idx == selected) then
                write(output_unit, '(a)', advance='no') BOLD // UNDERLINE // WHITE // shown // RESET
            else if (used > 0 .and. current_is_dir(current_idx)) then
                write(output_unit, '(a)', advance='no') BLUE // shown // RESET
            else if (used > 0 .and. current_is_exec(current_idx)) then
                write(output_unit, '(a)', advance='no') GREEN // shown // RESET
            else if (used > 0) then
                write(output_unit, '(a)', advance='no') shown // RESET
            end if
            ! Pad rather than ESC[K. Clear-to-end-of-line clears to the end of
            ! the TERMINAL line, which for a modal is the document beside the
            ! box -- the same defect that had ghost text erasing the pane next
            ! to it.
            if (name_w - used > 0) &
                write(output_unit, '(a)', advance='no') repeat(' ', name_w - used)
        end do

        ! Footer
        if (.not. want_chrome) then
            write(output_unit, '(a)', advance='no') ESC // "[?25h"
            flush(output_unit)
            return
        end if
        call move_to(r0 + r - 1, c0)
        call put_cells(DIM // "arrows:nav  enter:open  S-enter/^g:tab group  " // &
                       "^f:favorite  esc:quit" // RESET, &
                       "arrows:nav  enter:open  S-enter/^g:tab group  " // &
                       "^f:favorite  esc:quit", c)

        write(output_unit, '(a)', advance='no') ESC // "[?25h"
        flush(output_unit)

    contains

        subroutine move_to(row, col)
            integer, intent(in) :: row, col
            character(len=32) :: seq
            write(seq, '(a,i0,a,i0,a)') ESC // "[", row, ";", col, "H"
            write(output_unit, '(a)', advance='no') trim(seq)
        end subroutine move_to

        !> Write styled text and pad to `cells`. `plain` is the same text with
        !> no escapes, because the styled form's length is not its width.
        subroutine put_cells(styled, plain, cells)
            character(len=*), intent(in) :: styled, plain
            integer, intent(in) :: cells
            character(len=:), allocatable :: cut
            integer :: n

            call clip_to_cells(plain, cells, cut, n)
            if (n >= len(plain)) then
                write(output_unit, '(a)', advance='no') styled
            else
                write(output_unit, '(a)', advance='no') cut
            end if
            if (cells - n > 0) write(output_unit, '(a)', advance='no') repeat(' ', cells - n)
        end subroutine put_cells

    end subroutine draw_fortress_interface

end module fortress_display_module
