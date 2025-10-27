module help_display_module
    use iso_fortran_env, only: input_unit
    use terminal_io_module
    use editor_state_module
    implicit none
    private

    public :: show_help

contains

    subroutine show_help(editor)
        type(editor_state_t), intent(in) :: editor
        integer :: row, col, max_rows
        character(len=100) :: line
        character(len=1) :: ch
        integer :: ios

        max_rows = editor%screen_rows

        ! Clear screen and hide cursor
        call terminal_clear_screen()
        call terminal_hide_cursor()

        ! Title
        row = 1
        call terminal_move_cursor(row, 1)
        call terminal_write("FACSIMILE HELP - Press any key to close")
        row = row + 1
        call terminal_move_cursor(row, 1)
        call terminal_write(repeat("=", 60))
        row = row + 2

        ! Navigation
        call display_section(row, max_rows, "NAVIGATION", &
            ["arrows          move cursor                     ", &
             "ctrl-a/home     start of line                   ", &
             "ctrl-e/end      end of line                     ", &
             "alt-left/right  word jump                       ", &
             "pageup/down     page scroll                     ", &
             "click           position cursor                 ", &
             "alt-click       add/remove cursor               "])

        ! Selection
        call display_section(row, max_rows, "SELECTION", &
            ["shift-arrows        character selection         ", &
             "shift-alt-l/r       word selection              ", &
             "shift-ctrl-a/e      select to line start/end    ", &
             "shift-home/end      select to line boundaries   ", &
             "shift-pageup/down   page selection              "])

        ! Editing
        call display_section(row, max_rows, "EDITING", &
            ["backspace/ctrl-h    delete backward             ", &
             "delete              delete forward              ", &
             "tab                 insert 4 spaces             ", &
             "ctrl-k              kill line forward           ", &
             "ctrl-u              kill line backward          ", &
             "ctrl-y              yank from stack             ", &
             "ctrl-w/alt-bksp     delete word backward        ", &
             "alt-d               delete word forward         ", &
             "ctrl-t              transpose characters        "])

        ! Clipboard
        call display_section(row, max_rows, "CLIPBOARD", &
            ["ctrl-x          cut line/selection              ", &
             "ctrl-c          copy line/selection             ", &
             "ctrl-v          paste                           "])

        ! Lines
        call display_section(row, max_rows, "LINES", &
            ["alt-up/down         move line                   ", &
             "alt-shift-up/down   duplicate line              "])

        ! Multiple Cursors
        call display_section(row, max_rows, "MULTIPLE CURSORS", &
            ["ctrl-d              select next match           ", &
             "alt-click           add/remove cursor           ", &
             "opt-meta-up/down    cursor above/below          "])

        ! Special
        call display_section(row, max_rows, "SPECIAL", &
            ["ctrl-'              cycle quotes                ", &
             "ctrl-opt-backspace  remove brackets             ", &
             "ctrl-z              undo                        ", &
             "ctrl-shift-z        redo                        "])

        ! File
        call display_section(row, max_rows, "FILE", &
            ["ctrl-s          save                            ", &
             "ctrl-q          quit                            ", &
             "ctrl-?          show this help                  "])

        ! Footer
        if (row < max_rows - 1) then
            row = max_rows - 1
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat("=", 60))
        end if

        ! Show cursor at bottom
        call terminal_move_cursor(max_rows, 1)
        call terminal_show_cursor()

        ! Wait for any key press
        read(input_unit, '(a1)', advance='no', iostat=ios) ch

        ! Redraw will happen after returning
    end subroutine show_help

    subroutine display_section(row, max_rows, title, items)
        integer, intent(inout) :: row
        integer, intent(in) :: max_rows
        character(len=*), intent(in) :: title
        character(len=*), dimension(:), intent(in) :: items
        integer :: i

        if (row >= max_rows - 2) return

        ! Section title
        call terminal_move_cursor(row, 1)
        call terminal_write(title)
        row = row + 1

        ! Section items
        do i = 1, size(items)
            if (row >= max_rows - 2) return
            call terminal_move_cursor(row, 3)
            call terminal_write(items(i))
            row = row + 1
        end do

        row = row + 1  ! Extra space between sections
    end subroutine display_section

end module help_display_module