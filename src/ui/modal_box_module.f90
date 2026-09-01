!> Shared chrome for floating terminal surfaces: an opaque body, border,
!> title, and a clipped drop shadow.
!>
!> Extracted so a modal can be *placed* without also owning the arithmetic of
!> drawing a box. The helpers take explicit geometry so dialogs, menus, and
!> cursor-anchored popups can use the same surface rules without sharing
!> state.
!>
!> Nothing here clears to end of line. ESC[K clears to the end of the
!> TERMINAL line rather than the box, which for a dialog floating over a
!> document means erasing the document beside it.
module modal_box_module
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_BORDER, THEME_PANEL, THEME_SHADOW, &
        theme_reset, theme_sgr, theme_shadows_enabled
    implicit none
    private

    public :: box_fill, box_frame, box_inner_rect, box_row, box_rule, box_shadow

contains

    !> Where a caller may draw, given the box. One cell of border on every
    !> side; the caller never needs to know the border is one cell.
    subroutine box_inner_rect(row0, col0, height, width, &
                              inner_row, inner_col, inner_h, inner_w)
        integer, intent(in) :: row0, col0, height, width
        integer, intent(out) :: inner_row, inner_col, inner_h, inner_w

        inner_row = row0 + 1
        inner_col = col0 + 1
        inner_h = max(0, height - 2)
        inner_w = max(0, width - 2)
    end subroutine box_inner_rect

    !> Draw an opaque body, border, and shadow. Callers overwrite the body
    !> with their own semantic rows; pre-filling guarantees that short lists
    !> and blank sections never reveal document text underneath.
    subroutine box_frame(row0, col0, height, width, title, footer, max_row, max_col)
        integer, intent(in) :: row0, col0, height, width
        character(len=*), intent(in) :: title, footer
        integer, intent(in), optional :: max_row, max_col
        integer :: r

        if (height < 2 .or. width < 2) return

        call box_shadow(row0, col0, height, width, max_row, max_col)
        call box_fill(row0, col0, height, width)
        call put_edge(row0, col0, width, '╭', '╮', title)
        do r = row0 + 1, row0 + height - 2
            call terminal_move_cursor(r, col0)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())
            call terminal_move_cursor(r, col0 + width - 1)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())
        end do
        call put_edge(row0 + height - 1, col0, width, '╰', '╯', footer)
    end subroutine box_frame

    !> Fill only the box interior. Borders are left for box_frame or a caller
    !> with custom rules, and every write is bounded to the box width.
    subroutine box_fill(row0, col0, height, width, role)
        integer, intent(in) :: row0, col0, height, width
        integer, intent(in), optional :: role
        integer :: body_role, r

        if (height < 3 .or. width < 3) return
        body_role = THEME_PANEL
        if (present(role)) body_role = role
        do r = row0 + 1, row0 + height - 2
            call terminal_move_cursor(r, col0 + 1)
            call terminal_write(theme_sgr(body_role) // &
                                repeat(' ', width - 2) // theme_reset())
        end do
    end subroutine box_fill

    !> One complete framed row with a clipped, padded body.
    subroutine box_row(row, col0, width, text, role)
        integer, intent(in) :: row, col0, width, role
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: shown
        integer :: inner, used

        if (width < 2) return
        inner = max(0, width - 2)
        call clip_to_cells(text, inner, shown, used)
        call terminal_move_cursor(row, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                            theme_sgr(role) // shown // &
                            repeat(' ', max(0, inner - used)) // &
                            theme_sgr(THEME_BORDER) // '│' // theme_reset())
    end subroutine box_row

    !> A full-width rule inside an existing box.
    subroutine box_rule(row, col0, width)
        integer, intent(in) :: row, col0, width

        if (width < 2) return
        call terminal_move_cursor(row, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '├' // &
                            repeat('─', max(0, width - 2)) // '┤' // theme_reset())
    end subroutine box_rule

    !> A horizontal rule with the given corners, and a label set into it.
    subroutine put_edge(row, col0, width, left, right, label)
        integer, intent(in) :: row, col0, width
        character(len=*), intent(in) :: left, right, label
        character(len=:), allocatable :: shown
        integer :: inner, used, pad

        inner = max(0, width - 2)
        call clip_to_cells(trim(label), max(0, inner - 4), shown, used)

        call terminal_move_cursor(row, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // left)
        if (used > 0) then
            call terminal_write('─ ' // shown // ' ')
            pad = inner - used - 3
        else
            pad = inner
        end if
        if (pad > 0) call terminal_write(repeat('─', pad))
        call terminal_write(right // theme_reset())
    end subroutine put_edge

    !> Two columns right and one row down, so the box reads as floating above
    !> the document rather than pasted into it. The right strip and full
    !> bottom strip meet at the lower-right corner.
    subroutine box_shadow(row0, col0, height, width, max_row, max_col)
        integer, intent(in) :: row0, col0, height, width
        integer, intent(in), optional :: max_row, max_col
        integer :: bottom_limit, right_limit, r, shadow_col, shadow_width

        if (.not. theme_shadows_enabled()) return

        bottom_limit = huge(1)
        right_limit = huge(1)
        if (present(max_row)) bottom_limit = max_row
        if (present(max_col)) right_limit = max_col

        shadow_col = col0 + width
        shadow_width = min(2, right_limit - shadow_col + 1)
        do r = row0 + 1, min(row0 + height - 1, bottom_limit)
            if (shadow_width < 1) exit
            call terminal_move_cursor(r, shadow_col)
            call terminal_write(theme_sgr(THEME_SHADOW) // &
                                repeat(' ', shadow_width) // theme_reset())
        end do

        r = row0 + height
        shadow_col = col0 + 2
        shadow_width = min(width, right_limit - shadow_col + 1)
        if (r <= bottom_limit .and. shadow_width > 0) then
            call terminal_move_cursor(r, shadow_col)
            call terminal_write(theme_sgr(THEME_SHADOW) // &
                                repeat(' ', shadow_width) // theme_reset())
        end if
    end subroutine box_shadow

end module modal_box_module
