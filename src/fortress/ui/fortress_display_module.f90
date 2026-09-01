! UI display for Fortress navigator (simplified for fac)

module fortress_display_module
    ! Everything here goes through the shared write buffer, not straight to
    ! the unit. The rest of the editor batches a frame into one write; a
    ! module that flushes on its own puts half a frame on the terminal
    ! ahead of the other half, which is what made this window flicker as it
    ! scrolled.
    use terminal_io_module, only: terminal_write, terminal_flush, terminal_move_cursor
    use fortress_fs_module, only: MAX_PATH, MAX_FILES
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_ACCENT, THEME_DIRECTORY, THEME_EXECUTABLE, &
        THEME_HINT, THEME_MUTED, THEME_PANEL, THEME_PANEL_HEADER, &
        theme_foreground_sgr, theme_reset, theme_sgr
    implicit none
    private

    public :: draw_fortress_interface

    ! ANSI escape codes
    character(len=*), parameter :: ESC = char(27)
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
        integer :: parent_role, current_role
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

        ! Hiding and showing the caret is the FRAME's job when this is a
        ! window inside the editor -- doing it here as well means showing it
        ! again halfway through a frame the renderer has not finished, which
        ! is a flicker all of its own. The full-screen driver has no frame
        ! around it, so it still owns the caret.
        if (want_chrome) call terminal_write(ESC // "[?25l")
        if (do_clear) call terminal_write(ESC // "[2J")

        if (want_chrome) then
            call move_to(r0, c0)
            call put_cells(theme_sgr(THEME_PANEL_HEADER) // " FORTRESS " // &
                           theme_sgr(THEME_PANEL) // trim(current_dir) // theme_reset(), &
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
            parent_role = THEME_MUTED
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                if (parent_is_dir(parent_idx)) parent_role = THEME_DIRECTORY
            end if
            call put_entry(shown, used, left_w, parent_role, &
                           used > 0 .and. parent_idx == parent_selected)

            call terminal_write(theme_sgr(THEME_PANEL) // &
                theme_foreground_sgr(THEME_MUTED) // &
                " " // char(226)//char(148)//char(130) // " ")

            ! === Current pane (right) ===
            current_name = ''
            if (current_idx >= 1 .and. current_idx <= current_count) then
                current_name = trim(adjustl(current_files(current_idx)))
                j = scan(current_name, char(10)//char(13))
                if (j > 0) current_name = current_name(1:j-1)
                if (current_is_dir(current_idx)) current_name = trim(current_name) // "/"
            end if
            call clip_to_cells(trim(current_name), name_w, shown, used)
            current_role = THEME_PANEL
            if (current_idx >= 1 .and. current_idx <= current_count) then
                if (current_is_dir(current_idx)) then
                    current_role = THEME_DIRECTORY
                else if (current_is_exec(current_idx)) then
                    current_role = THEME_EXECUTABLE
                end if
            end if
            call put_entry(shown, used, name_w, current_role, &
                           used > 0 .and. current_idx == selected)
            call terminal_write(theme_reset())
        end do

        ! Footer. As a window there is none, and nothing is pushed out here
        ! either: the frame flushes once at the end, which is the whole point
        ! -- a mid-frame flush puts the panes on the terminal before the box
        ! around them, and scrolling shows the two arriving separately.
        if (.not. want_chrome) return
        call move_to(r0 + r - 1, c0)
        call put_cells(theme_sgr(THEME_HINT) // " arrows:nav  enter:open  S-enter/^g:tab group  " // &
                       "^f:favorite  esc:quit" // theme_reset(), &
                       "arrows:nav  enter:open  S-enter/^g:tab group  " // &
                       "^f:favorite  esc:quit", max(0, c - 1))

        ! The full-screen driver has no frame to flush for it: it draws and
        ! then blocks on a key, so anything still in the buffer would not
        ! reach the terminal until the next keystroke.
        call terminal_write(ESC // "[?25h")
        call terminal_flush()

    contains

        subroutine move_to(row, col)
            integer, intent(in) :: row, col
            call terminal_move_cursor(row, col)
        end subroutine move_to

        !> Paint one name and all of its padding as a single panel surface.
        !> Foreground-only overlays keep directory/muted colors from replacing
        !> the modal background. The active row is intentionally typographic,
        !> not reverse video: reverse made the two-pane cursor look like
        !> broken rectangular fragments around the separator.
        subroutine put_entry(text, used_cells, cells, role, active)
            character(len=*), intent(in) :: text
            integer, intent(in) :: used_cells, cells, role
            logical, intent(in) :: active

            call terminal_write(theme_sgr(THEME_PANEL))
            if (used_cells > 0) then
                if (active) then
                    call terminal_write(theme_foreground_sgr(THEME_ACCENT) // &
                                        ESC // '[1;4m' // text)
                else
                    call terminal_write(theme_foreground_sgr(role) // text)
                end if
            end if
            if (cells - used_cells > 0) then
                call terminal_write(theme_sgr(THEME_PANEL) // &
                                    repeat(' ', cells - used_cells))
            end if
        end subroutine put_entry

        !> Write styled text and pad to `cells`. `plain` is the same text with
        !> no escapes, because the styled form's length is not its width.
        subroutine put_cells(styled, plain, cells)
            character(len=*), intent(in) :: styled, plain
            integer, intent(in) :: cells
            character(len=:), allocatable :: cut
            integer :: n

            call clip_to_cells(plain, cells, cut, n)
            if (n >= len(plain)) then
                call terminal_write(styled)
            else
                call terminal_write(cut)
            end if
            if (cells - n > 0) call terminal_write(repeat(' ', cells - n))
        end subroutine put_cells

    end subroutine draw_fortress_interface

end module fortress_display_module
