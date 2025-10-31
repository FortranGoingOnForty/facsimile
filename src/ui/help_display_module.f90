module help_display_module
    use iso_fortran_env, only: input_unit
    use terminal_io_module, only: terminal_clear_screen, terminal_hide_cursor, terminal_show_cursor, &
                                   terminal_move_cursor, terminal_write
    use input_handler_module, only: get_key_input
    use editor_state_module
    implicit none
    private

    public :: show_help

contains

    subroutine show_help(editor)
        type(editor_state_t), intent(in) :: editor
        integer :: row, col, max_rows
        character(len=100) :: line
        character(len=32) :: key_input
        integer :: status

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
             "ctrl-a/home     smart home (toggle)             ", &
             "ctrl-e/end      end of line                     ", &
             "ctrl-home/end   file start/end                  ", &
             "alt-left/right  word jump                       ", &
             "alt-[/alt-]     jump to matching bracket        ", &
             "pageup/down     page scroll                     ", &
             "ctrl-g          go to line:column               ", &
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
             "tab                 insert 4 spaces/indent      ", &
             "shift-tab           dedent selection/line       ", &
             "ctrl-k              kill line forward           ", &
             "ctrl-u              kill line backward          ", &
             "ctrl-y              yank from stack             ", &
             "alt-bksp            delete word backward        ", &
             "alt-d               delete word forward         ", &
             "ctrl-j              join lines                  ", &
             "auto-close          brackets/quotes             "])

        ! Clipboard
        call display_section(row, max_rows, "CLIPBOARD", &
            ["ctrl-x          cut line/selection              ", &
             "ctrl-c          copy line/selection             ", &
             "ctrl-v          paste                           "])

        ! Lines
        call display_section(row, max_rows, "LINES", &
            ["alt-up/down         move line                   ", &
             "alt-shift-up/down   duplicate line              "])

        ! Search & Replace
        call display_section(row, max_rows, "SEARCH & REPLACE", &
            ["ctrl-f              search forward              ", &
             "ctrl-r              find and replace            ", &
             "n                   next match                  ", &
             "N                   previous match              ", &
             "ctrl-d              select next match           ", &
             "alt-c (in search)   toggle case sensitive       ", &
             "alt-w (in search)   toggle whole word match     "])

        ! Multiple Cursors
        call display_section(row, max_rows, "MULTIPLE CURSORS", &
            ["alt-click           add/remove cursor           ", &
             "opt-meta-up/down    cursor above/below          "])

        ! Special
        call display_section(row, max_rows, "SPECIAL", &
            ["ctrl-'              cycle quotes                ", &
             "ctrl-opt-backspace  remove brackets             ", &
             "ctrl-z              undo                        ", &
             "ctrl-shift-z        redo                        ", &
             "ctrl-l              clear/redraw screen         "])

        ! Tabs
        call display_section(row, max_rows, "TABS", &
            ["ctrl-t              new empty tab               ", &
             "ctrl-w              close current tab           ", &
             "alt-1 to alt-9      jump to tab 1-9             ", &
             "ctrl-alt-left       previous tab                ", &
             "ctrl-alt-right      next tab                    ", &
             "ctrl-b              toggle file tree (fuss)     "])

        ! Git (in fuss mode)
        call display_section(row, max_rows, "GIT (in fuss mode)", &
            ["a                   stage file/add                           ", &
             "u                   unstage file                             ", &
             "m                   commit with message                      ", &
             "p                   push to remote                           ", &
             "f                   fetch from remote                        ", &
             "l                   pull from remote                         ", &
             "t                   create tag                               ", &
             "Markers: staged, modified, untracked, incoming               "])

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

        ! Wait for any key press (use get_key_input to properly consume escape sequences)
        ! Keep reading until we get a valid key (not timeout)
        do
            call get_key_input(key_input, status)
            if (status == 0) exit  ! Got a valid key, exit loop
        end do

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