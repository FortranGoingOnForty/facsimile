! UI display for Fortress navigator (simplified for fac)

module fortress_display_module
    use iso_fortran_env, only: output_unit
    use fortress_fs_module, only: MAX_PATH, MAX_FILES
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
                                       selected, parent_selected, scroll_offset, parent_scroll_offset, first_draw)
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir
        character(len=*), dimension(*), intent(in) :: current_files, parent_files
        logical, dimension(*), intent(in) :: current_is_dir, parent_is_dir
        logical, dimension(*), intent(in) :: current_is_exec
        logical, intent(in), optional :: first_draw
        integer :: left_w, i, j, parent_idx, current_idx, vis_h
        character(len=256) :: parent_name, current_name
        character(len=256) :: line
        logical :: do_clear

        ! Calculate layout
        left_w = c * 3 / 10
        vis_h = r - 3

        ! Check if we should do full clear (only on first draw)
        do_clear = .false.
        if (present(first_draw)) do_clear = first_draw

        ! Hide cursor and optionally clear screen
        if (do_clear) then
            write(output_unit, '(a)', advance='no') ESC // "[?25l" // ESC // "[2J" // ESC // "[H"
        else
            write(output_unit, '(a)', advance='no') ESC // "[?25l" // ESC // "[H"
        end if

        ! Header - clear line then write
        write(output_unit, '(a)', advance='no') ESC // "[K"
        write(output_unit, '(a)') BOLD // "FORTRESS" // RESET // " - " // trim(current_dir)
        write(output_unit, '(a)', advance='no') ESC // "[K"
        write(output_unit, '(a)') ""

        ! Render each line - write complete lines at once
        do i = 1, vis_h
            parent_idx = i + parent_scroll_offset
            current_idx = i + scroll_offset

            ! Move to absolute row position (row 3 + i) and clear line
            write(line, '(a,i0,a)') ESC // "[", 2 + i, ";1H" // ESC // "[K"
            write(output_unit, '(a)', advance='no') trim(line)

            ! === Parent pane (left 30%) ===
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                ! Clean the filename - remove any control characters
                parent_name = trim(adjustl(parent_files(parent_idx)))

                ! Remove any newlines or carriage returns
                j = scan(parent_name, char(10)//char(13))
                if (j > 0) then
                    parent_name = parent_name(1:j-1)
                end if

                if (parent_is_dir(parent_idx)) parent_name = trim(parent_name) // "/"

                ! Truncate if too long
                if (len_trim(parent_name) > left_w) then
                    parent_name = parent_name(1:left_w)
                end if

                ! Write parent item with color
                if (parent_idx == parent_selected) then
                    write(output_unit, '(a)', advance='no') DIM // BOLD // BLUE // trim(parent_name) // RESET
                else
                    write(output_unit, '(a)', advance='no') DIM // GREY // trim(parent_name) // RESET
                end if
            end if

            ! Use explicit cursor positioning for separator - build the position string
            write(line, '(a,i0,a)') ESC // "[", left_w + 1, "G"
            write(output_unit, '(a)', advance='no') trim(line) // " │ "

            ! === Current pane (right) ===
            if (current_idx >= 1 .and. current_idx <= current_count) then
                ! Clean the filename - remove any control characters
                current_name = trim(adjustl(current_files(current_idx)))

                ! Remove any newlines or carriage returns by finding first occurrence
                j = scan(current_name, char(10)//char(13))
                if (j > 0) then
                    current_name = current_name(1:j-1)
                end if

                if (current_is_dir(current_idx)) current_name = trim(current_name) // "/"

                if (current_idx == selected) then
                    write(output_unit, '(a)', advance='no') BOLD // UNDERLINE // WHITE // trim(current_name) // RESET
                else if (current_is_dir(current_idx)) then
                    write(output_unit, '(a)', advance='no') BLUE // trim(current_name) // RESET
                else if (current_is_exec(current_idx)) then
                    write(output_unit, '(a)', advance='no') GREEN // trim(current_name) // RESET
                else
                    write(output_unit, '(a)', advance='no') trim(current_name) // RESET
                end if
            end if
            ! No else needed - line is already cleared
        end do

        ! Footer - position at last row and clear line
        write(line, '(a,i0,a)') ESC // "[", r, ";1H" // ESC // "[K"
        write(output_unit, '(a)', advance='no') trim(line)
        write(output_unit, '(a)', advance='no') DIM // "arrows:nav enter:open f:favorite esc:quit" // RESET

        ! Show cursor again
        write(output_unit, '(a)', advance='no') ESC // "[?25h"
        flush(output_unit)
    end subroutine draw_fortress_interface

end module fortress_display_module
