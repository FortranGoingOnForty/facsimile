module theme_picker_module
    use input_handler_module, only: get_key_input
    use terminal_io_module, only: terminal_flush, terminal_move_cursor, terminal_write
    use utf8_module, only: clip_to_cells, utf8_display_width
    use theme_module, only: THEME_ACCENT, THEME_BORDER, THEME_HINT, THEME_MUTED, &
        THEME_PANEL, THEME_PANEL_FOOTER, THEME_PANEL_HEADER, THEME_PANEL_SELECTION, &
        THEME_SHADOW, THEME_SYNTAX_COMMENT, THEME_SYNTAX_KEYWORD, &
        THEME_SYNTAX_NUMBER, THEME_SYNTAX_STRING, theme_current_id, theme_list, &
        theme_paint, theme_reset, theme_select, theme_sgr, theme_shadows_enabled
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
        integer :: pad
        integer :: used
        character(len=:), allocatable :: line

        width = min(58, max(24, cols - 4))
        height = min(rows - 2, count + 7)
        row0 = max(1, (rows - height) / 2 + 1)
        col0 = max(1, (cols - width) / 2 + 1)

        call terminal_write(achar(27) // '[2J' // achar(27) // '[H')
        call terminal_move_cursor(row0, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '┌' // repeat('─', width - 2) // '┐')
        call terminal_move_cursor(row0 + 1, col0)
        line = ' Color Theme'
        pad = max(0, width - 2 - len(line))
        call terminal_write(theme_sgr(THEME_PANEL_HEADER) // '│' // line // repeat(' ', pad) // &
            theme_sgr(THEME_BORDER) // '│')
        call terminal_move_cursor(row0 + 2, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '├' // repeat('─', width - 2) // '┤')

        list_rows = max(1, height - 6)
        first = max(1, min(selected, count - list_rows + 1))
        do i = 1, min(count, list_rows)
            item = first + i - 1
            if (item > count) exit
            call terminal_move_cursor(row0 + 2 + i, col0)
            call clip_to_cells('  ' // trim(names(item)), width - 2, line, used)
            pad = max(0, width - 2 - used)
            if (item == selected) then
                call terminal_write(theme_sgr(THEME_PANEL_SELECTION) // '│' // trim(line) // &
                    repeat(' ', pad) // theme_sgr(THEME_BORDER) // '│')
            else
                call terminal_write(theme_sgr(THEME_PANEL) // '│' // trim(line) // &
                    repeat(' ', pad) // theme_sgr(THEME_BORDER) // '│')
            end if
        end do

        call terminal_move_cursor(row0 + height - 3, col0)
        if (width >= 48) then
            call terminal_write(theme_sgr(THEME_PANEL) // '│  ' // &
                theme_paint(THEME_SYNTAX_KEYWORD, 'function') // ' ' // &
                theme_paint(THEME_ACCENT, 'facsimile') // theme_paint(THEME_MUTED, '(') // &
                theme_paint(THEME_SYNTAX_STRING, '"theme"') // theme_paint(THEME_MUTED, ', ') // &
                theme_paint(THEME_SYNTAX_NUMBER, '33') // theme_paint(THEME_MUTED, ')') // &
                theme_paint(THEME_SYNTAX_COMMENT, '  ! preview') // &
                theme_sgr(THEME_PANEL) // repeat(' ', max(0, width - 46)) // &
                theme_sgr(THEME_BORDER) // '│')
        else
            call terminal_write(theme_sgr(THEME_PANEL) // '│  ' // &
                theme_paint(THEME_ACCENT, 'Aa') // ' ' // &
                theme_paint(THEME_SYNTAX_NUMBER, '123') // &
                theme_paint(THEME_SYNTAX_COMMENT, ' ! preview') // &
                theme_sgr(THEME_PANEL) // repeat(' ', max(0, width - 20)) // &
                theme_sgr(THEME_BORDER) // '│')
        end if
        call terminal_move_cursor(row0 + height - 2, col0)
        line = ' ↑↓ preview   Enter apply   Esc cancel'
        pad = max(0, width - 2 - utf8_display_width(line))
        call terminal_write(theme_sgr(THEME_PANEL_FOOTER) // '│' // line // repeat(' ', pad) // &
            theme_sgr(THEME_BORDER) // '│')
        call terminal_move_cursor(row0 + height - 1, col0)
        call terminal_write(theme_sgr(THEME_BORDER) // '└' // repeat('─', width - 2) // '┘' // theme_reset())

        if (theme_shadows_enabled() .and. col0 + width <= cols) then
            do i = 1, height - 1
                call terminal_move_cursor(row0 + i, col0 + width)
                call terminal_write(theme_sgr(THEME_SHADOW) // ' ' // theme_reset())
            end do
            if (row0 + height <= rows) then
                call terminal_move_cursor(row0 + height, col0 + 2)
                call terminal_write(theme_sgr(THEME_SHADOW) // repeat(' ', width - 1) // theme_reset())
            end if
        end if
    end subroutine render_picker

end module theme_picker_module
