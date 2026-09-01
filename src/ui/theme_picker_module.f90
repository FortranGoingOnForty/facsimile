module theme_picker_module
    use input_handler_module, only: get_key_input
    use terminal_io_module, only: terminal_begin_sync, terminal_end_sync, &
        terminal_flush, terminal_move_cursor, terminal_write
    use modal_box_module, only: box_fill, box_row, box_shadow
    use utf8_module, only: utf8_display_width
    use theme_module, only: THEME_ACCENT, THEME_BORDER, THEME_HINT, THEME_MUTED, &
        THEME_PANEL, THEME_PANEL_FOOTER, THEME_PANEL_HEADER, THEME_PANEL_SELECTION, &
        THEME_SYNTAX_COMMENT, THEME_SYNTAX_KEYWORD, &
        THEME_SYNTAX_NUMBER, THEME_SYNTAX_STRING, theme_current_id, theme_list, &
        theme_foreground_sgr, theme_reset, theme_select, theme_sgr
    implicit none
    private

    public :: show_theme_picker_interactive

contains

    subroutine show_theme_picker_interactive(screen_rows, screen_cols, changed, message)
        integer, intent(in) :: screen_rows
        integer, intent(in) :: screen_cols
        logical, intent(out) :: changed
        character(len=:), allocatable, intent(out) :: message
        character(len=64), allocatable :: names(:)
        character(len=:), allocatable :: original
        character(len=32) :: key
        integer :: count
        integer :: selected
        integer :: status
        integer :: i
        logical :: ok

        call theme_list(names, count)
        original = theme_current_id()
        selected = 1
        do i = 1, count
            if (trim(names(i)) == original) selected = i
        end do
        call preview_selected(names, selected, ok, message)
        call render_picker(names, count, selected, screen_rows, screen_cols)
        call terminal_flush()

        changed = .false.
        do
            call get_key_input(key, status)
            if (status /= 0) cycle
            select case (trim(key))
            case ('up', 'k')
                selected = selected - 1
                if (selected < 1) selected = count
                call preview_selected(names, selected, ok, message)
            case ('down', 'j')
                selected = selected + 1
                if (selected > count) selected = 1
                call preview_selected(names, selected, ok, message)
            case ('enter')
                call theme_select(trim(names(selected)), .true., ok, message)
                changed = ok
                return
            case ('esc')
                call theme_select(original, .false., ok, message)
                changed = .false.
                message = 'Theme unchanged'
                return
            case default
                cycle
            end select
            call render_picker(names, count, selected, screen_rows, screen_cols)
            call terminal_flush()
        end do
    end subroutine show_theme_picker_interactive

    subroutine preview_selected(names, selected, ok, message)
        character(len=64), intent(in) :: names(:)
        integer, intent(in) :: selected
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: message
        call theme_select(trim(names(selected)), .false., ok, message)
    end subroutine preview_selected

    subroutine render_picker(names, count, selected, rows, cols)
        character(len=64), intent(in) :: names(:)
        integer, intent(in) :: count
        integer, intent(in) :: selected
        integer, intent(in) :: rows
        integer, intent(in) :: cols
        integer :: width
        integer :: height
        integer :: row0
        integer :: col0
        integer :: i
        integer :: item
        integer :: first
        integer :: list_rows
        integer :: shown_items
        integer :: pad
        character(len=:), allocatable :: line

        width = min(58, max(24, cols - 4))
        height = min(rows - 2, count + 7)
        row0 = max(1, (rows - height) / 2 + 1)
        col0 = max(1, (cols - width) / 2 + 1)

        call terminal_begin_sync()
        call box_shadow(row0, col0, height, width, rows, cols)
        call box_fill(row0, col0, height, width)
        call terminal_move_cursor(row0, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '┌' // repeat('─', width - 2) // '┐')
        call terminal_move_cursor(row0 + 1, col0)
        line = ' Color Theme'
        pad = max(0, width - 2 - len(line))
        call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
            theme_sgr(THEME_PANEL_HEADER) // line // repeat(' ', pad) // &
            theme_sgr(THEME_BORDER) // '│' // theme_reset())
        call terminal_move_cursor(row0 + 2, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '├' // repeat('─', width - 2) // '┤')

        list_rows = max(1, height - 6)
        first = max(1, min(selected, count - list_rows + 1))
        shown_items = min(count, list_rows)
        do i = 1, shown_items
            item = first + i - 1
            if (item > count) exit
            if (item == selected) then
                call box_row(row0 + 2 + i, col0, width, &
                             '  ' // trim(names(item)), THEME_PANEL_SELECTION)
            else
                call box_row(row0 + 2 + i, col0, width, &
                             '  ' // trim(names(item)), THEME_PANEL)
            end if
        end do
        do i = shown_items + 1, list_rows
            call box_row(row0 + 2 + i, col0, width, '', THEME_PANEL)
        end do

        call terminal_move_cursor(row0 + height - 3, col0)
        if (width >= 48) then
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                theme_sgr(THEME_PANEL) // '  ' // &
                theme_foreground_sgr(THEME_SYNTAX_KEYWORD) // 'function' // ' ' // &
                theme_foreground_sgr(THEME_ACCENT) // 'facsimile' // &
                theme_foreground_sgr(THEME_MUTED) // '(' // &
                theme_foreground_sgr(THEME_SYNTAX_STRING) // '"theme"' // &
                theme_foreground_sgr(THEME_MUTED) // ', ' // &
                theme_foreground_sgr(THEME_SYNTAX_NUMBER) // '33' // &
                theme_foreground_sgr(THEME_MUTED) // ')' // &
                theme_foreground_sgr(THEME_SYNTAX_COMMENT) // '  ! preview' // &
                theme_sgr(THEME_PANEL) // repeat(' ', max(0, width - 46)) // &
                theme_sgr(THEME_BORDER) // '│' // theme_reset())
        else
            call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
                theme_sgr(THEME_PANEL) // '  ' // &
                theme_foreground_sgr(THEME_ACCENT) // 'Aa' // ' ' // &
                theme_foreground_sgr(THEME_SYNTAX_NUMBER) // '123' // &
                theme_foreground_sgr(THEME_SYNTAX_COMMENT) // ' ! preview' // &
                theme_sgr(THEME_PANEL) // repeat(' ', max(0, width - 20)) // &
                theme_sgr(THEME_BORDER) // '│' // theme_reset())
        end if
        call terminal_move_cursor(row0 + height - 2, col0)
        line = ' ↑↓ preview   Enter apply   Esc cancel'
        pad = max(0, width - 2 - utf8_display_width(line))
        call terminal_write(theme_sgr(THEME_BORDER) // '│' // &
            theme_sgr(THEME_PANEL_FOOTER) // line // repeat(' ', pad) // &
            theme_sgr(THEME_BORDER) // '│' // theme_reset())
        call terminal_move_cursor(row0 + height - 1, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '└' // repeat('─', width - 2) // '┘' // theme_reset())
        call terminal_end_sync()
    end subroutine render_picker

end module theme_picker_module
