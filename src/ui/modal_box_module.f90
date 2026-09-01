!> The frame around a floating dialog: border, title, and a drop shadow.
!>
!> Extracted so a modal can be *placed* without also owning the arithmetic of
!> drawing a box. group_picker has equivalents of these as private routines
!> reading its own module state; this version takes an explicit origin so
!> anything can use it. Converting the picker to it is deliberately a
!> separate change -- it is a working, tested dialog and the conversion
!> carries its own risk.
!>
!> Nothing here clears to end of line. ESC[K clears to the end of the
!> TERMINAL line rather than the box, which for a dialog floating over a
!> document means erasing the document beside it.
module modal_box_module
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_BORDER, THEME_SHADOW, theme_reset, theme_sgr, &
        theme_shadows_enabled
    implicit none
    private

    public :: box_frame, box_inner_rect

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

    !> Draw the border and shadow. The inside is left alone: the caller fills
    !> it, and filling it here first would only be overdrawn.
    subroutine box_frame(row0, col0, height, width, title, footer)
        integer, intent(in) :: row0, col0, height, width
        character(len=*), intent(in) :: title, footer
        integer :: r

        if (height < 2 .or. width < 2) return

        call put_shadow(row0, col0, height, width)
        call put_edge(row0, col0, width, '╭', '╮', title)
        do r = row0 + 1, row0 + height - 2
            call terminal_move_cursor(r, col0)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())
            call terminal_move_cursor(r, col0 + width - 1)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // theme_reset())
        end do
        call put_edge(row0 + height - 1, col0, width, '╰', '╯', footer)
    end subroutine box_frame

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
    !> the document rather than pasted into it.
    subroutine put_shadow(row0, col0, height, width)
        integer, intent(in) :: row0, col0, height, width
        integer :: r

        if (.not. theme_shadows_enabled()) return
        do r = row0 + 1, row0 + height
            call terminal_move_cursor(r, col0 + width)
            call terminal_write(theme_sgr(THEME_SHADOW) // '  ' // theme_reset())
        end do
    end subroutine put_shadow

end module modal_box_module
