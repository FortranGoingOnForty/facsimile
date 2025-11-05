! UI display for Fortress navigator (adapted for fac - Phase 1 simplified)
! Original source: fortress/src/ui/display.f90

module fortress_display_module
    use iso_fortran_env, only: output_unit
    use fortress_fs_module, only: MAX_PATH, MAX_FILES
    implicit none
    private

    public :: draw_fortress_interface, get_file_color

    ! ANSI escape codes (matching fortress terminal_control)
    character(len=*), parameter :: ESC = char(27)
    character(len=*), parameter :: BOLD = ESC // "[1m"
    character(len=*), parameter :: DIM = ESC // "[2m"
    character(len=*), parameter :: UNDERLINE = ESC // "[4m"
    character(len=*), parameter :: RESET = ESC // "[0m"
    character(len=*), parameter :: BLUE = ESC // "[34m"
    character(len=*), parameter :: GREEN = ESC // "[32m"
    character(len=*), parameter :: RED = ESC // "[31m"
    character(len=*), parameter :: GREY = ESC // "[90m"
    character(len=*), parameter :: WHITE = ESC // "[37m"
    character(len=*), parameter :: YELLOW = ESC // "[33m"

contains

    subroutine draw_fortress_interface(r, c, current_dir, current_files, current_is_dir, current_is_exec, &
                                       current_count, parent_files, parent_is_dir, parent_is_exec, parent_count, &
                                       selected, parent_selected, scroll_offset, parent_scroll_offset)
        integer, intent(in) :: r, c, current_count, parent_count, selected, parent_selected
        integer, intent(in) :: scroll_offset, parent_scroll_offset
        character(len=*), intent(in) :: current_dir
        character(len=*), dimension(*), intent(in) :: current_files, parent_files
        logical, dimension(*), intent(in) :: current_is_dir, parent_is_dir
        logical, dimension(*), intent(in) :: current_is_exec, parent_is_exec
        integer :: left_w, i, parent_idx, current_idx, vis_h, display_len, inner_w
        character(len=256) :: fname
        character(len=20) :: color_code

        left_w = c * 3 / 10
        vis_h = r - 4  ! Visible height (top border + header + footer + bottom border)
        inner_w = c - 4  ! Inner width (minus border chars and padding)

        ! Clear screen and move to top
        write(output_unit, '(a)', advance='no') ESC // "[H" // ESC // "[J"
        flush(output_unit)

        ! Top border with rounded corners
        write(output_unit, '(a)') DIM // "╭─" // repeat("─", inner_w) // "─╮" // RESET

        ! Header - path with border
        write(output_unit, '(a)', advance='no') DIM // "│ " // RESET
        write(output_unit, '(a)', advance='no') BOLD // trim(current_dir) // RESET
        write(output_unit, '(a)', advance='no') repeat(" ", max(0, inner_w - len_trim(current_dir)))
        write(output_unit, '(a)') DIM // " │" // RESET

        ! Files (render based on scroll offsets) with border
        do i = 1, vis_h
            parent_idx = i + parent_scroll_offset
            current_idx = i + scroll_offset

            ! Left border
            write(output_unit, '(a)', advance='no') DIM // "│ " // RESET

            ! Parent pane (left 30%)
            if (parent_idx >= 1 .and. parent_idx <= parent_count) then
                fname = parent_files(parent_idx)

                ! Add trailing slash for directories
                if (parent_is_dir(parent_idx) .and. parent_files(parent_idx) /= "." .and. parent_files(parent_idx) /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for parent file
                color_code = get_file_color(parent_files(parent_idx), parent_is_dir(parent_idx), parent_is_exec(parent_idx))

                ! Calculate visual width
                display_len = min(len_trim(fname), left_w)

                ! Highlight if selected in parent pane
                if (parent_idx == parent_selected) then
                    write(output_unit, '(a)', advance='no') DIM // BOLD // trim(color_code) // &
                        fname(1:min(len_trim(fname), left_w)) // RESET
                else
                    write(output_unit, '(a)', advance='no') DIM // trim(color_code) // &
                        fname(1:min(len_trim(fname), left_w)) // RESET
                end if
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, left_w - display_len))
            else
                write(output_unit, '(a)', advance='no') repeat(" ", left_w)
            end if

            ! RESET before separator
            write(output_unit, '(a)', advance='no') RESET

            ! Separator
            write(output_unit, '(a)', advance='no') " │ "

            ! Current pane (right 70%)
            if (current_idx >= 1 .and. current_idx <= current_count) then
                fname = current_files(current_idx)

                ! Add trailing slash for directories
                if (current_is_dir(current_idx) .and. current_files(current_idx) /= "." .and. current_files(current_idx) /= "..") then
                    fname = trim(fname) // "/"
                end if

                ! Get color for current file
                color_code = get_file_color(current_files(current_idx), current_is_dir(current_idx), current_is_exec(current_idx))

                ! Highlight selected item
                if (current_idx == selected) then
                    write(output_unit, '(a)', advance='no') BOLD // UNDERLINE // trim(color_code) // trim(fname) // RESET
                else
                    write(output_unit, '(a)', advance='no') trim(color_code) // trim(fname) // RESET
                end if

                ! Pad to right border
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, inner_w - left_w - 3 - len_trim(fname)))
            else
                write(output_unit, '(a)', advance='no') repeat(" ", max(0, inner_w - left_w - 3))
            end if

            ! Right border
            write(output_unit, '(a)') DIM // " │" // RESET
        end do

        ! Bottom border with ESC hint
        write(output_unit, '(a)', advance='no') DIM // "╰─" // RESET
        write(output_unit, '(a)', advance='no') DIM // " ESC to exit " // RESET
        write(output_unit, '(a)') DIM // repeat("─", max(0, inner_w - 13)) // "─╯" // RESET

        flush(output_unit)
    end subroutine draw_fortress_interface

    function get_file_color(filename, is_dir, is_exec) result(color)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_dir, is_exec
        character(len=20) :: color

        ! Directories: Blue and bold
        if (is_dir) then
            color = BOLD // BLUE
        ! Dotfiles: Grey
        else if (len_trim(filename) > 0) then
            if (filename(1:1) == '.') then
                color = GREY
            ! Executable files: Green
            else if (is_exec) then
                color = GREEN
            ! All other files: White
            else
                color = WHITE
            end if
        else
            color = WHITE
        end if
    end function get_file_color

end module fortress_display_module
