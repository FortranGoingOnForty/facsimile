module help_display_module
    use iso_fortran_env, only: input_unit
    use terminal_io_module, only: terminal_clear_screen, terminal_hide_cursor, terminal_show_cursor, &
                                   terminal_move_cursor, terminal_write
    use input_handler_module, only: get_key_input
    use editor_state_module
    implicit none
    private

    public :: show_help, show_tags_modal, display_tags_header, show_fuss_hints

contains

    subroutine show_help(editor)
        type(editor_state_t), intent(in) :: editor
        character(len=100), allocatable :: help_lines(:)
        integer :: n_lines, viewport_start, viewport_size
        integer :: row, max_rows
        character(len=32) :: key_input
        integer :: status
        logical :: done

        max_rows = editor%screen_rows

        ! Build help content array
        call build_help_content(help_lines, n_lines)

        ! Initialize viewport
        viewport_start = 1
        viewport_size = max_rows - 4  ! Leave room for header and footer

        call terminal_hide_cursor()
        done = .false.
        do while (.not. done)
            ! Clear screen (buffered, no flush — arrives with content atomically)
            call terminal_write(achar(27) // '[2J' // achar(27) // '[H')

            ! Header
            call terminal_move_cursor(1, 1)
            call terminal_write("FACSIMILE HELP - Navigate: ↑↓/jk/PgUp/PgDn, Quit: q/ESC")
            call terminal_move_cursor(2, 1)
            call terminal_write(repeat("=", 70))

            ! Display visible portion of help
            call display_help_viewport(help_lines, n_lines, viewport_start, viewport_size, 3)

            ! Footer with scroll indicator
            row = max_rows - 1
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat("=", 70))
            row = max_rows
            call terminal_move_cursor(row, 1)
            if (n_lines > viewport_size) then
                write(key_input, '(a,i0,a,i0,a,i0,a,i0,a)') "Lines ", viewport_start, "-", &
                      min(viewport_start + viewport_size - 1, n_lines), " of ", n_lines, &
                      " (", int(real(viewport_start) * 100.0 / real(max(1, n_lines - viewport_size + 1))), "%)"
                call terminal_write(trim(key_input))
            else
                call terminal_write("All content visible")
            end if

            ! Show cursor
            call terminal_move_cursor(max_rows, 1)
            call terminal_show_cursor()

            ! Handle navigation
            call get_key_input(key_input, status)
            if (status == 0) then
                select case(trim(key_input))
                case('q', 'Q', 'esc', 'ctrl-shift-/', 'ctrl-?', 'f1')
                    done = .true.
                case('up', 'k')
                    if (viewport_start > 1) viewport_start = viewport_start - 1
                case('down', 'j')
                    if (viewport_start + viewport_size - 1 < n_lines) viewport_start = viewport_start + 1
                case('pageup')
                    viewport_start = max(1, viewport_start - viewport_size)
                case('pagedown')
                    if (viewport_start + viewport_size - 1 < n_lines) then
                        viewport_start = min(n_lines - viewport_size + 1, viewport_start + viewport_size)
                    end if
                case('home')
                    viewport_start = 1
                case('end')
                    if (n_lines > viewport_size) then
                        viewport_start = n_lines - viewport_size + 1
                    end if
                end select
            end if
        end do

        ! Cleanup
        if (allocated(help_lines)) deallocate(help_lines)

        ! Redraw will happen after returning
    end subroutine show_help

    subroutine build_help_content(lines, n_lines)
        character(len=100), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: n_lines
        integer :: i

        ! Count total lines needed (sections + items + spacing)
        n_lines = 0
        n_lines = n_lines + 9 + 2   ! NAVIGATION (mouse moved to its own section)
        n_lines = n_lines + 11 + 2  ! MOUSE
        n_lines = n_lines + 6 + 2   ! SELECTION
        n_lines = n_lines + 13 + 2  ! EDITING
        n_lines = n_lines + 4 + 2   ! CLIPBOARD
        n_lines = n_lines + 3 + 2   ! LINES
        n_lines = n_lines + 9 + 2   ! SEARCH & REPLACE
        n_lines = n_lines + 5 + 2   ! MULTIPLE CURSORS (increased from 4)
        n_lines = n_lines + 9 + 2   ! SPECIAL (added the two terminal resize lines)
        n_lines = n_lines + 7 + 2   ! TABS
        n_lines = n_lines + 7 + 2   ! PANES
        n_lines = n_lines + 10 + 2  ! GIT
        n_lines = n_lines + 5 + 2   ! FILE
        n_lines = n_lines + 7 + 2   ! LSP (added code actions)

        allocate(lines(n_lines))
        i = 1

        ! NAVIGATION
        lines(i) = "NAVIGATION"; i = i + 1
        lines(i) = "  arrows              move cursor"; i = i + 1
        lines(i) = "  ctrl-a/home         smart home (toggle)"; i = i + 1
        lines(i) = "  ctrl-e/end          end of line"; i = i + 1
        lines(i) = "  ctrl-home/end       file start/end"; i = i + 1
        lines(i) = "  alt-left/right      word jump"; i = i + 1
        lines(i) = "  alt-[/alt-]         jump to matching bracket"; i = i + 1
        lines(i) = "  pageup/down         page scroll"; i = i + 1
        lines(i) = "  ctrl-g              go to line:column"; i = i + 1
        lines(i) = ""; i = i + 1

        ! MOUSE
        lines(i) = "MOUSE"; i = i + 1
        lines(i) = "  click               position cursor (closes the tree)"; i = i + 1
        lines(i) = "  drag                select text"; i = i + 1
        lines(i) = "  alt-click           add/remove cursor"; i = i + 1
        lines(i) = "  right-click         context menu at the pointer"; i = i + 1
        lines(i) = "  ctrl-click          same menu (one-button pointer)"; i = i + 1
        lines(i) = "  shift-f10 / alt-z   context menu at the caret"; i = i + 1
        lines(i) = "  wheel               scroll whatever is under it"; i = i + 1
        lines(i) = "  click a tab         switch to it"; i = i + 1
        lines(i) = "  click a tree row    open a file / expand a folder"; i = i + 1
        lines(i) = "  right-click a row   splits and git actions"; i = i + 1
        lines(i) = "  click the chevron   toggle the file tree (bottom left)"; i = i + 1
        lines(i) = ""; i = i + 1

        ! SELECTION
        lines(i) = "SELECTION"; i = i + 1
        lines(i) = "  shift-arrows        character selection"; i = i + 1
        lines(i) = "  shift-alt-l/r       word selection"; i = i + 1
        lines(i) = "  shift-ctrl-a/e      select to line start/end"; i = i + 1
        lines(i) = "  shift-home/end      select to line boundaries"; i = i + 1
        lines(i) = "  shift-pageup/down   page selection"; i = i + 1
        lines(i) = "  esc                 clear selection"; i = i + 1
        lines(i) = ""; i = i + 1

        ! EDITING
        lines(i) = "EDITING"; i = i + 1
        lines(i) = "  backspace/ctrl-h    delete backward"; i = i + 1
        lines(i) = "  delete              delete forward"; i = i + 1
        lines(i) = "  tab                 insert 4 spaces/indent"; i = i + 1
        lines(i) = "  shift-tab           dedent selection/line"; i = i + 1
        lines(i) = "  ctrl-/              toggle line comment"; i = i + 1
        lines(i) = "  ctrl-k              kill line forward"; i = i + 1
        lines(i) = "  ctrl-shift-k        delete line (no clipboard/yank)"; i = i + 1
        lines(i) = "  ctrl-u              kill line backward"; i = i + 1
        lines(i) = "  ctrl-y              yank from stack"; i = i + 1
        lines(i) = "  alt-bksp            delete word backward (eats blank lines)"; i = i + 1
        lines(i) = "  alt-d               delete word forward"; i = i + 1
        lines(i) = "  ctrl-j              join lines"; i = i + 1
        lines(i) = "  ctrl-t              transpose characters"; i = i + 1
        lines(i) = ""; i = i + 1

        ! CLIPBOARD
        lines(i) = "CLIPBOARD (uses system clipboard)"; i = i + 1
        lines(i) = "  ctrl-x              cut line/selection"; i = i + 1
        lines(i) = "  ctrl-c              copy line/selection"; i = i + 1
        lines(i) = "  ctrl-v              paste"; i = i + 1
        lines(i) = ""; i = i + 1

        ! LINES
        lines(i) = "LINES"; i = i + 1
        lines(i) = "  alt-up/down         move line"; i = i + 1
        lines(i) = "  alt-shift-up/down   duplicate line"; i = i + 1
        lines(i) = ""; i = i + 1

        ! SEARCH & REPLACE
        lines(i) = "SEARCH & REPLACE"; i = i + 1
        lines(i) = "  ctrl-f              search forward"; i = i + 1
        lines(i) = "  ctrl-r              find and replace"; i = i + 1
        lines(i) = "  n                   next match (after ctrl-f)"; i = i + 1
        lines(i) = "  N                   previous match (after ctrl-f)"; i = i + 1
        lines(i) = "  ctrl-d              select word & find next match"; i = i + 1
        lines(i) = "  esc                 exit match mode / clear selections"; i = i + 1
        lines(i) = "  alt-c (in search)   toggle case sensitive"; i = i + 1
        lines(i) = "  alt-w (in search)   toggle whole word match"; i = i + 1
        lines(i) = ""; i = i + 1

        ! MULTIPLE CURSORS
        lines(i) = "MULTIPLE CURSORS"; i = i + 1
        lines(i) = "  ctrl-d              select word & add cursor at next match"; i = i + 1
        lines(i) = "  alt-click           add/remove cursor at position"; i = i + 1
        lines(i) = "  ctrl-alt-up/down    add cursor on line above/below"; i = i + 1
        lines(i) = "  esc                 reduce to single cursor"; i = i + 1
        lines(i) = ""; i = i + 1

        ! SPECIAL
        lines(i) = "SPECIAL"; i = i + 1
        lines(i) = "  alt-'               cycle quotes"; i = i + 1
        lines(i) = "  alt-shift-'         remove brackets/quotes"; i = i + 1
        lines(i) = "  ctrl-z              undo"; i = i + 1
        lines(i) = "  ctrl-]/ctrl-shift-z redo"; i = i + 1
        lines(i) = "  ctrl-l              clear/redraw screen"; i = i + 1
        lines(i) = "  F5 / alt-t          terminal panel (esc closes it at a bare prompt)"; i = i + 1
        lines(i) = "  ctrl-shift-up/down  resize the terminal (while it has focus)"; i = i + 1
        lines(i) = "  ctrl-shift-m        maximize/restore the terminal"; i = i + 1
        lines(i) = ""; i = i + 1

        ! TABS
        lines(i) = "TABS"; i = i + 1
        lines(i) = "  ctrl-t              new empty tab"; i = i + 1
        lines(i) = "  ctrl-w              close current tab"; i = i + 1
        lines(i) = "  ctrl-1 to ctrl-9    jump to tab 1-9 (or alt-1 to alt-9)"; i = i + 1
        lines(i) = "  ctrl-0              jump to tab 10 (or alt-0)"; i = i + 1
        lines(i) = "  ctrl-pageup         previous tab (or ctrl-alt-left)"; i = i + 1
        lines(i) = "  ctrl-pagedown       next tab (or ctrl-alt-right)"; i = i + 1
        lines(i) = ""; i = i + 1

        ! PANES
        lines(i) = "PANES"; i = i + 1
        lines(i) = "  alt-v               split pane vertically"; i = i + 1
        lines(i) = "  alt-s               split pane horizontally"; i = i + 1
        lines(i) = "  alt-q               close current pane only"; i = i + 1
        lines(i) = "  ctrl-w              close pane (then tab if last)"; i = i + 1
        lines(i) = "  alt-h/j/k/l         navigate left/down/up/right (Vim style)"; i = i + 1
        lines(i) = "  ctrl-shift-arrows   navigate between panes (alternative)"; i = i + 1
        lines(i) = ""; i = i + 1

        ! GIT (in fuss mode)
        lines(i) = "GIT (in fuss mode - ctrl-b)"; i = i + 1
        lines(i) = "  a                   stage file/add"; i = i + 1
        lines(i) = "  u                   unstage file"; i = i + 1
        lines(i) = "  m                   commit with message"; i = i + 1
        lines(i) = "  p                   push to remote"; i = i + 1
        lines(i) = "  f                   fetch from remote"; i = i + 1
        lines(i) = "  l                   pull from remote"; i = i + 1
        lines(i) = "  t                   create tag"; i = i + 1
        lines(i) = "  d                   diff file in new tab"; i = i + 1
        lines(i) = "  enter               open file in editor"; i = i + 1
        lines(i) = ""; i = i + 1

        ! FILE
        lines(i) = "FILE"; i = i + 1
        lines(i) = "  ctrl-b              toggle file browser (fuss mode)"; i = i + 1
        lines(i) = "  ctrl-s              save"; i = i + 1
        lines(i) = "  ctrl-q              quit"; i = i + 1
        lines(i) = "  ctrl-? or F1        show this help"; i = i + 1
        lines(i) = ""; i = i + 1

        ! LSP (Language Server Protocol)
        lines(i) = "LSP (Language Server Protocol)"; i = i + 1
        lines(i) = "  ctrl-space          code completion"; i = i + 1
        lines(i) = "  F12/alt-g           go to definition"; i = i + 1
        lines(i) = "  shift-F12/alt-r     find all references"; i = i + 1
        lines(i) = "  alt-, (alt-comma)   jump back (navigation history)"; i = i + 1
        lines(i) = "  F2 / alt-n          rename symbol"; i = i + 1
        lines(i) = "  F10/alt-.           code actions (quick fixes)"; i = i + 1
        lines(i) = "  F4/alt-o            document symbols (outline)"; i = i + 1
        lines(i) = "  F6/alt-p            workspace symbols (search project)"; i = i + 1
        lines(i) = "  F8/alt-e            toggle diagnostics panel (errors)"; i = i + 1
        lines(i) = "  ctrl-p              command palette"; i = i + 1
        lines(i) = ""; i = i + 1

        n_lines = i - 1
    end subroutine build_help_content

    subroutine display_help_viewport(lines, n_lines, start_line, viewport_size, start_row)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: n_lines, start_line, viewport_size, start_row
        integer :: i, row, end_line

        row = start_row
        end_line = min(start_line + viewport_size - 1, n_lines)

        do i = start_line, end_line
            call terminal_move_cursor(row, 1)
            call terminal_write(lines(i))
            row = row + 1
        end do
    end subroutine display_help_viewport

    subroutine show_tags_modal(editor, tags, n_tags)
        type(editor_state_t), intent(in) :: editor
        character(len=256), intent(in) :: tags(:)
        integer, intent(in) :: n_tags
        integer :: row, i, max_display
        character(len=32) :: key_input
        integer :: status

        ! Clear screen (buffered) and hide cursor
        call terminal_write(achar(27) // '[2J' // achar(27) // '[H')

        ! Title
        row = 1
        call terminal_move_cursor(row, 1)
        call terminal_write("EXISTING GIT TAGS - Press any key to continue")
        row = row + 1
        call terminal_move_cursor(row, 1)
        call terminal_write(repeat("=", 60))
        row = row + 2

        ! Display tags
        if (n_tags == 0) then
            call terminal_move_cursor(row, 1)
            call terminal_write("  (no tags found)")
            row = row + 2
        else
            ! Show up to screen_rows - 6 tags (leave room for header/footer)
            max_display = min(n_tags, editor%screen_rows - 6)
            do i = 1, max_display
                call terminal_move_cursor(row, 3)
                call terminal_write(trim(tags(i)))
                row = row + 1
            end do

            if (n_tags > max_display) then
                row = row + 1
                call terminal_move_cursor(row, 3)
                call terminal_write("... and " // trim(int_to_str(n_tags - max_display)) // " more")
                row = row + 1
            end if
            row = row + 1
        end if

        ! Footer
        if (row < editor%screen_rows - 1) then
            row = editor%screen_rows - 1
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat("=", 60))
        end if

        ! Show cursor at bottom
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_show_cursor()

        ! Wait for any key press
        do
            call get_key_input(key_input, status)
            if (status == 0) exit  ! Got a valid key, exit loop
        end do
    end subroutine show_tags_modal

    ! Helper function to convert integer to string
    function int_to_str(val) result(str)
        integer, intent(in) :: val
        character(len=20) :: str
        write(str, '(I0)') val
    end function int_to_str

    ! Display fuss mode hints (compact help for ctrl-b mode)
    subroutine show_fuss_hints(editor)
        type(editor_state_t), intent(in) :: editor
        integer :: row
        character(len=32) :: key_input
        integer :: status

        ! Clear screen (buffered) and hide cursor
        call terminal_write(achar(27) // '[2J' // achar(27) // '[H')

        ! Title
        row = 1
        call terminal_move_cursor(row, 1)
        call terminal_write("FUSS MODE HINTS - Press any key to close")
        row = row + 1
        call terminal_move_cursor(row, 1)
        call terminal_write(repeat("=", 60))
        row = row + 2

        ! Navigation
        call terminal_move_cursor(row, 1)
        call terminal_write("NAVIGATION")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("j/k                 move to previous/next sibling")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("→/←                 expand/collapse or enter/exit directory")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("o/enter             open file in editor")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("space               toggle directory expand/collapse")
        row = row + 2

        ! Git Operations
        call terminal_move_cursor(row, 1)
        call terminal_write("GIT OPERATIONS")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("a                   stage file")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("u                   unstage file")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("d                   diff file in new tab")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("m                   commit with message")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("p                   push to remote")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("f                   fetch from remote")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("l                   pull from remote")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("t                   create and push tag")
        row = row + 2

        ! Exit
        call terminal_move_cursor(row, 1)
        call terminal_write("EXIT")
        row = row + 1
        call terminal_move_cursor(row, 3)
        call terminal_write("esc/ctrl-b          close fuss mode")
        row = row + 1

        ! Footer
        if (row < editor%screen_rows - 1) then
            row = editor%screen_rows - 1
            call terminal_move_cursor(row, 1)
            call terminal_write(repeat("=", 60))
        end if

        ! Show cursor at bottom
        call terminal_move_cursor(editor%screen_rows, 1)
        call terminal_show_cursor()

        ! Wait for any key press
        do
            call get_key_input(key_input, status)
            if (status == 0) exit  ! Got a valid key, exit loop
        end do
    end subroutine show_fuss_hints

    ! Display tags header without waiting for input (for split view with prompt)
    subroutine display_tags_header(editor, tags, n_tags)
        type(editor_state_t), intent(in) :: editor
        character(len=256), intent(in) :: tags(:)
        integer, intent(in) :: n_tags
        integer :: row, i, max_display

        ! Clear screen (buffered)
        call terminal_write(achar(27) // '[2J' // achar(27) // '[H')

        ! Title
        row = 1
        call terminal_move_cursor(row, 1)
        call terminal_write("EXISTING GIT TAGS")
        row = row + 1
        call terminal_move_cursor(row, 1)
        call terminal_write(repeat("=", 50))
        row = row + 1

        ! Display tags (reserve bottom 2 lines for prompt)
        if (n_tags == 0) then
            call terminal_move_cursor(row, 1)
            call terminal_write("  (no tags found)")
        else
            ! Show up to screen_rows - 4 tags (leave room for header + prompt area)
            max_display = min(n_tags, editor%screen_rows - 4)
            do i = 1, max_display
                call terminal_move_cursor(row, 3)
                call terminal_write(trim(tags(i)))
                row = row + 1
            end do

            if (n_tags > max_display) then
                call terminal_move_cursor(row, 3)
                call terminal_write("... and " // trim(int_to_str(n_tags - max_display)) // " more")
            end if
        end if

        ! Separator line before prompt area
        call terminal_move_cursor(editor%screen_rows - 1, 1)
        call terminal_write(repeat("-", 50))
    end subroutine display_tags_header

end module help_display_module