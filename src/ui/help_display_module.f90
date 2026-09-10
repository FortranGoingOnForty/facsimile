module help_display_module
    use terminal_io_module, only: terminal_begin_sync, terminal_end_sync, &
                                   terminal_hide_cursor, terminal_show_cursor, &
                                   terminal_move_cursor, terminal_write, terminal_flush
    use input_handler_module, only: get_key_input
    use editor_state_module
    use modal_box_module, only: box_frame, box_inner_rect
    use utf8_module, only: clip_to_cells
    use theme_module, only: THEME_ACCENT, THEME_BORDER, THEME_HINT, THEME_PANEL, &
        theme_paint, theme_foreground_sgr, theme_reset, theme_sgr
    implicit none
    private

    public :: show_help, show_tags_modal, display_tags_header, show_fuss_hints

    integer, parameter :: HELP_BLANK = 0
    integer, parameter :: HELP_SECTION = 1
    integer, parameter :: HELP_BINDING = 2
    integer, parameter :: HELP_NOTE = 3

    type :: help_line_t
        integer :: kind = HELP_BLANK
        character(len=32) :: key = ''
        character(len=100) :: text = ''
    end type help_line_t

contains

    subroutine show_help(editor)
        type(editor_state_t), intent(in) :: editor
        type(help_line_t), allocatable :: help_lines(:)
        integer :: n_lines, viewport_start, viewport_size
        integer :: row0, col0, box_width, box_height
        integer :: inner_row, inner_col, inner_h, inner_w, max_start
        character(len=32) :: key_input
        integer :: status
        logical :: done

        call build_help_content(help_lines, n_lines)
        box_width = min(88, max(2, editor%screen_cols - 4))
        box_height = min(26, max(4, editor%screen_rows - 4))
        row0 = max(1, (editor%screen_rows - box_height) / 2 + 1)
        col0 = max(1, (editor%screen_cols - box_width) / 2 + 1)
        call box_inner_rect(row0, col0, box_height, box_width, &
                            inner_row, inner_col, inner_h, inner_w)

        viewport_start = 1
        viewport_size = max(1, inner_h)
        max_start = max(1, n_lines - viewport_size + 1)

        call terminal_hide_cursor()
        done = .false.
        do while (.not. done)
            call terminal_begin_sync()
            call render_help_modal(help_lines, n_lines, viewport_start, &
                                   row0, col0, box_height, box_width)
            call terminal_end_sync()
            call terminal_flush()

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
                    viewport_start = max(1, viewport_start - max(1, viewport_size - 1))
                case('pagedown')
                    viewport_start = min(max_start, &
                        viewport_start + max(1, viewport_size - 1))
                case('home')
                    viewport_start = 1
                case('end')
                    viewport_start = max_start
                end select
            end if
        end do

        if (allocated(help_lines)) deallocate(help_lines)
    end subroutine show_help

    subroutine build_help_content(lines, n_lines)
        type(help_line_t), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: n_lines
        allocate(lines(160))
        n_lines = 0

        call add_section(lines, n_lines, 'NAVIGATION')
        call add_binding(lines, n_lines, 'Arrows', 'Move the caret')
        call add_binding(lines, n_lines, 'Home / Ctrl+A', 'Smart line start; press again for column one')
        call add_binding(lines, n_lines, 'End / Ctrl+E', 'Move to line end')
        call add_binding(lines, n_lines, 'Ctrl+Home / End', 'Move to file start / end')
        call add_binding(lines, n_lines, 'Alt+Left / Right', 'Move by word')
        call add_binding(lines, n_lines, 'PageUp / PageDown', 'Move by one editor page')
        call add_binding(lines, n_lines, 'Ctrl+G', 'Go to line or line:column')
        call add_binding(lines, n_lines, 'Alt+[ / Alt+]', 'Jump to the matching bracket')
        call add_binding(lines, n_lines, 'Alt+,', 'Return to the previous jump location')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'EDITING')
        call add_binding(lines, n_lines, 'Ctrl+Z / Ctrl+]', 'Undo / redo (Ctrl+Shift+Z also redoes)')
        call add_binding(lines, n_lines, 'Tab / Shift+Tab', 'Indent / dedent using the file policy')
        call add_binding(lines, n_lines, 'Ctrl+/', 'Toggle line comments')
        call add_binding(lines, n_lines, 'Ctrl+K / Ctrl+U', 'Kill to line end / start')
        call add_binding(lines, n_lines, 'Ctrl+Y', 'Yank the latest killed text')
        call add_binding(lines, n_lines, 'Alt+Backspace / Alt+D', 'Delete the previous / next word')
        call add_binding(lines, n_lines, 'Alt+Up / Down', 'Move the current line')
        call add_binding(lines, n_lines, 'Alt+Shift+Up / Down', 'Duplicate the current line')
        call add_binding(lines, n_lines, 'Alt+Shift+J', 'Join the next line onto this one')
        call add_binding(lines, n_lines, 'Ctrl+Shift+K', 'Delete lines without using the clipboard')
        call add_binding(lines, n_lines, "Alt+' / Alt+Shift+'", 'Cycle quotes / remove surrounding delimiters')
        call add_binding(lines, n_lines, 'Ctrl+X / C / V', 'Cut / copy / paste line or selection')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'SELECTION & MULTIPLE CURSORS')
        call add_binding(lines, n_lines, 'Shift+motion', 'Extend the selection by character, word, line, or page')
        call add_binding(lines, n_lines, 'Alt+A', 'Select the entire file')
        call add_binding(lines, n_lines, 'Ctrl+D', 'Select the word and add its next match')
        call add_binding(lines, n_lines, 'Alt+Click', 'Add or remove a cursor at the pointer')
        call add_binding(lines, n_lines, 'Ctrl+Alt+Up / Down', 'Add a cursor above / below')
        call add_note(lines, n_lines, 'Super+Up/Down and Ctrl+Shift+Alt+Up/Down are alternate cursor chords.')
        call add_binding(lines, n_lines, 'Esc', 'Clear selections and return to one cursor')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'FILES, TABS & PANES')
        call add_binding(lines, n_lines, 'Ctrl+S / Ctrl+Shift+S', 'Save this file / save every modified tab')
        call add_binding(lines, n_lines, 'Ctrl+O', 'Open the Fortress file navigator')
        call add_binding(lines, n_lines, 'Ctrl+T / Ctrl+W', 'New tab / close pane or tab')
        call add_binding(lines, n_lines, 'Alt+1 ... Alt+0', 'Jump to a numbered tab, then a numbered group member')
        call add_binding(lines, n_lines, 'Ctrl+1 ... Ctrl+0', 'Jump to a tab group in left-to-right order')
        call add_binding(lines, n_lines, 'Ctrl+PgUp / PgDown', 'Previous / next tab or group member')
        call add_binding(lines, n_lines, 'Alt+V / Alt+S', 'Split vertically / horizontally')
        call add_binding(lines, n_lines, 'Alt+H/J/K/L', 'Move between panes')
        call add_binding(lines, n_lines, 'Alt+Q', 'Close only the current pane')
        call add_binding(lines, n_lines, 'Ctrl+Q', 'Close the top surface, then quit')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'SEARCH & REPLACE')
        call add_binding(lines, n_lines, 'Ctrl+F', 'Open or close the live find bar')
        call add_binding(lines, n_lines, 'Ctrl+R', 'Find and replace')
        call add_binding(lines, n_lines, 'Ctrl+D', 'Select the word and find its next match')
        call add_binding(lines, n_lines, 'n / N', 'Next / previous match after the bar closes')
        call add_note(lines, n_lines, 'In the find bar: arrows/page keys navigate; Alt+C/W/R/S change matching.')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'FUSS FILE TREE')
        call add_binding(lines, n_lines, 'Ctrl+B / F3', 'Open or close Fuss')
        call add_binding(lines, n_lines, 'Arrows / Enter / Space', 'Navigate, open, and expand the tree')
        call add_binding(lines, n_lines, 'Type', 'Fuzzy-filter visible paths')
        call add_binding(lines, n_lines, '. / Ctrl+/', 'Toggle hidden files / expand hints')
        call add_binding(lines, n_lines, 'Ctrl+G then A/U/D', 'Stage / unstage / open diff')
        call add_binding(lines, n_lines, 'Ctrl+G then M/P/F/L', 'Commit / push / fetch / pull')
        call add_binding(lines, n_lines, 'Ctrl+G then T', 'Create and push a tag')
        call add_note(lines, n_lines, 'Bare letters belong to fuzzy search; Git actions always use the Ctrl+G prefix.')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'COMMANDS & TERMINAL')
        call add_binding(lines, n_lines, 'Ctrl+P', 'Open the command palette')
        call add_binding(lines, n_lines, 'F5 / Alt+T', 'Open or focus the integrated terminal')
        call add_binding(lines, n_lines, 'Ctrl+Shift+Up / Down', 'Resize the focused terminal')
        call add_binding(lines, n_lines, 'Ctrl+Shift+M', 'Maximize / restore the terminal')
        call add_binding(lines, n_lines, 'Ctrl+L', 'Clear and redraw the editor')
        call add_binding(lines, n_lines, 'Ctrl+? / F1', 'Open this help modal')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'INLINE COMPLETION')
        call add_binding(lines, n_lines, 'Alt+I', 'Toggle inline AI completion')
        call add_binding(lines, n_lines, 'Tab', 'Accept the complete suggestion')
        call add_binding(lines, n_lines, 'Ctrl+Right / Alt+Right', 'Accept one word / one line')
        call add_binding(lines, n_lines, 'Alt+\', 'Request a deeper completion')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'LANGUAGE INTELLIGENCE')
        call add_binding(lines, n_lines, 'Ctrl+Space / Ctrl+H', 'Completion / hover information')
        call add_binding(lines, n_lines, 'F12 / Alt+G / Ctrl+\', 'Go to definition')
        call add_binding(lines, n_lines, 'Shift+F12 / Alt+R', 'Find references')
        call add_binding(lines, n_lines, 'F2 / Alt+N', 'Rename symbol')
        call add_binding(lines, n_lines, 'F10 / Alt+.', 'Code actions and quick fixes')
        call add_binding(lines, n_lines, 'F4 / Alt+O', 'Document symbols')
        call add_binding(lines, n_lines, 'F6 / Alt+P', 'Workspace symbols')
        call add_binding(lines, n_lines, 'F8 / Alt+E', 'Diagnostics panel')
        call add_binding(lines, n_lines, 'Shift+Alt+F / Alt+M', 'Format document / manage language servers')
        call add_note(lines, n_lines, 'Panels use arrows or j/k, Enter to choose, and Esc to close.')
        call add_blank(lines, n_lines)

        call add_section(lines, n_lines, 'MOUSE')
        call add_binding(lines, n_lines, 'Click / Drag', 'Place the caret / select text')
        call add_binding(lines, n_lines, 'Right-click', 'Open the context menu at the pointer')
        call add_binding(lines, n_lines, 'Shift+F10 / Alt+Z', 'Open the context menu at the caret')
        call add_binding(lines, n_lines, 'Wheel', 'Scroll the pane or terminal under the pointer')
        call add_binding(lines, n_lines, 'Drag terminal border', 'Resize the integrated terminal')
        call add_binding(lines, n_lines, 'Click tabs or tree rows', 'Switch files, open paths, or expand directories')
    end subroutine build_help_content

    subroutine add_section(lines, n, text)
        type(help_line_t), intent(inout) :: lines(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: text
        n = n + 1
        lines(n)%kind = HELP_SECTION
        lines(n)%text = text
    end subroutine add_section

    subroutine add_binding(lines, n, key, text)
        type(help_line_t), intent(inout) :: lines(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: key, text
        n = n + 1
        lines(n)%kind = HELP_BINDING
        lines(n)%key = key
        lines(n)%text = text
    end subroutine add_binding

    subroutine add_note(lines, n, text)
        type(help_line_t), intent(inout) :: lines(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: text
        n = n + 1
        lines(n)%kind = HELP_NOTE
        lines(n)%text = text
    end subroutine add_note

    subroutine add_blank(lines, n)
        type(help_line_t), intent(inout) :: lines(:)
        integer, intent(inout) :: n
        n = n + 1
        lines(n)%kind = HELP_BLANK
    end subroutine add_blank

    subroutine render_help_modal(lines, n_lines, start_line, row0, col0, height, width)
        type(help_line_t), intent(in) :: lines(:)
        integer, intent(in) :: n_lines, start_line, row0, col0, height, width
        integer :: inner_row, inner_col, inner_h, inner_w
        integer :: i, row, key_col, key_width, separator_col
        integer :: description_col, description_width, used, description_used
        integer :: left_padding, description_gap
        character(len=80) :: footer
        character(len=:), allocatable :: shown, description, styled

        call box_inner_rect(row0, col0, height, width, &
                            inner_row, inner_col, inner_h, inner_w)
        write(footer, '(a,i0,a,i0,a,i0)') &
            '↑↓ scroll  PgUp/PgDn  Esc close  ', start_line, '-', &
            min(n_lines, start_line + inner_h - 1), '/', n_lines
        call box_frame(row0, col0, height, width, 'FACSIMILE HELP', trim(footer))

        key_col = inner_col + min(2, max(0, inner_w - 1))
        key_width = min(24, max(10, inner_w / 3))
        separator_col = min(inner_col + inner_w - 1, key_col + key_width)
        key_width = max(0, separator_col - key_col)
        description_col = min(inner_col + inner_w, separator_col + 2)
        description_width = max(0, inner_col + inner_w - description_col)
        left_padding = max(0, key_col - inner_col)
        description_gap = max(0, description_col - separator_col - 1)

        do row = inner_row, inner_row + inner_h - 1
            i = start_line + row - inner_row
            styled = theme_sgr(THEME_PANEL)
            if (i <= n_lines) then
                select case (lines(i)%kind)
                case (HELP_SECTION)
                    call clip_to_cells(trim(lines(i)%text), &
                        max(0, inner_w - left_padding), shown, used)
                    styled = styled // repeat(' ', left_padding) // &
                        theme_foreground_sgr(THEME_ACCENT) // shown // &
                        theme_sgr(THEME_PANEL) // &
                        repeat(' ', max(0, inner_w - left_padding - used))
                case (HELP_BINDING)
                    call clip_to_cells(trim(lines(i)%key), &
                        max(0, key_width - 1), shown, used)
                    call clip_to_cells(trim(lines(i)%text), description_width, &
                        description, description_used)
                    styled = styled // repeat(' ', left_padding) // &
                        theme_foreground_sgr(THEME_ACCENT) // shown // &
                        theme_sgr(THEME_PANEL) // &
                        repeat(' ', max(0, key_width - used)) // &
                        theme_foreground_sgr(THEME_BORDER) // '│' // &
                        theme_sgr(THEME_PANEL) // repeat(' ', description_gap) // &
                        description // repeat(' ', max(0, description_width - description_used))
                case (HELP_NOTE)
                    call clip_to_cells(trim(lines(i)%text), &
                        max(0, inner_w - left_padding), shown, used)
                    styled = styled // repeat(' ', left_padding) // &
                        theme_foreground_sgr(THEME_HINT) // shown // &
                        theme_sgr(THEME_PANEL) // &
                        repeat(' ', max(0, inner_w - left_padding - used))
                case default
                    styled = styled // repeat(' ', inner_w)
                end select
            else
                styled = styled // repeat(' ', inner_w)
            end if
            call terminal_move_cursor(row, inner_col)
            call terminal_write(styled // theme_reset())
        end do
    end subroutine render_help_modal

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
        character(len=18), parameter :: keys(15) = [character(len=18) :: &
            'j / k', 'arrows', 'o / Enter', 'Space', '.', '/', 'Ctrl+/', &
            'Ctrl+G then A', 'Ctrl+G then U', 'Ctrl+G then D', &
            'Ctrl+G then M', 'Ctrl+G then P', 'Ctrl+G then F', &
            'Ctrl+G then L', 'Esc / F3']
        character(len=42), parameter :: descriptions(15) = [character(len=42) :: &
            'Move selection', 'Enter or leave a directory', 'Open file', &
            'Expand or collapse directory', 'Toggle hidden files', &
            'Search the tree', 'Toggle compact hints', 'Stage file', &
            'Unstage file', 'Open diff in a tab', 'Commit changes', &
            'Push', 'Fetch', 'Pull', 'Close Fuss']
        integer :: row, row0, col0, width, height, shown_count, i, pad, used
        character(len=32) :: key_input
        character(len=:), allocatable :: shown
        integer :: status

        call terminal_hide_cursor()
        width = min(70, max(28, editor%screen_cols - 4))
        height = min(editor%screen_rows - 2, 17)
        shown_count = min(15, max(1, height - 2))
        row0 = max(1, (editor%screen_rows - height) / 2 + 1)
        col0 = max(1, (editor%screen_cols - width) / 2 + 1)

        call terminal_begin_sync()
        call box_frame(row0, col0, height, width, 'FUSS COMMANDS', &
                       'Any key closes', editor%screen_rows, editor%screen_cols)
        do i = 1, shown_count
            row = row0 + i
            call terminal_move_cursor(row, col0)
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                                theme_sgr(THEME_PANEL) // ' ')
            call terminal_write(theme_paint(THEME_ACCENT, keys(i)))
            pad = max(1, 20 - len_trim(keys(i)))
            call clip_to_cells(trim(descriptions(i)), max(1, width - 23), shown, used)
            call terminal_write(theme_sgr(THEME_PANEL) // repeat(' ', pad) // shown // &
                repeat(' ', max(0, width - 3 - 20 - used)) // &
                theme_sgr(THEME_BORDER) // '│' // theme_reset())
        end do
        call terminal_end_sync()
        call terminal_flush()

        do
            call get_key_input(key_input, status)
            if (status == 0) exit
        end do
        call terminal_show_cursor()
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
