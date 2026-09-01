module references_panel_module
    use iso_fortran_env, only: int32
    use terminal_io_module, only: terminal_move_cursor, terminal_write
    use clickable_region_module, only: region_add, REGION_BLOCK
    use theme_module, only: THEME_ACCENT, THEME_MUTED, THEME_PANEL, &
        THEME_PANEL_FOOTER, THEME_PANEL_HEADER, THEME_PANEL_SELECTION, &
        theme_foreground_sgr, theme_glyph, theme_reset, theme_sgr
    use utf8_module, only: clip_to_cells, utf8_display_width
    implicit none
    private

    public :: references_panel_t, reference_location_t
    public :: init_references_panel, cleanup_references_panel
    public :: show_references_panel, hide_references_panel, toggle_references_panel
    public :: is_references_panel_visible, references_panel_handle_key
    public :: set_references, clear_references
    public :: get_selected_reference_location
    public :: render_references_panel, render_references_panel_at

    ! Reference location
    type :: reference_location_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: filename  ! Extracted from URI
        integer(int32) :: line
        integer(int32) :: column
        integer(int32) :: end_line
        integer(int32) :: end_column
        character(len=:), allocatable :: preview_text  ! Line content for preview
    end type reference_location_t

    ! References panel
    type :: references_panel_t
        logical :: visible = .false.
        integer :: width = 50  ! Panel width in columns
        integer :: selected_index = 1
        integer :: scroll_offset = 0

        ! References data
        type(reference_location_t), allocatable :: references(:)
        integer :: num_references = 0
        character(len=:), allocatable :: symbol_name

        ! Screen position
        integer :: screen_width = 80
        integer :: screen_height = 24
    end type references_panel_t

contains

    subroutine init_references_panel(panel)
        type(references_panel_t), intent(out) :: panel

        panel%visible = .false.
        panel%width = 50
        panel%selected_index = 1
        panel%scroll_offset = 0
        panel%num_references = 0
        panel%screen_width = 80
        panel%screen_height = 24
    end subroutine init_references_panel

    subroutine cleanup_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        integer :: i

        if (allocated(panel%references)) then
            do i = 1, panel%num_references
                if (allocated(panel%references(i)%uri)) deallocate(panel%references(i)%uri)
                if (allocated(panel%references(i)%filename)) deallocate(panel%references(i)%filename)
                if (allocated(panel%references(i)%preview_text)) deallocate(panel%references(i)%preview_text)
            end do
            deallocate(panel%references)
        end if

        if (allocated(panel%symbol_name)) deallocate(panel%symbol_name)

        panel%num_references = 0
        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine cleanup_references_panel

    subroutine set_references(panel, references, num_refs, symbol_name)
        type(references_panel_t), intent(inout) :: panel
        type(reference_location_t), intent(in) :: references(:)
        integer, intent(in) :: num_refs
        character(len=*), intent(in), optional :: symbol_name
        integer :: i, entry_len, max_entry_len
        character(len=100) :: loc_tmp

        ! Clear existing references
        call cleanup_references_panel(panel)

        max_entry_len = 0

        ! Allocate and copy new references
        if (num_refs > 0) then
            allocate(panel%references(num_refs))
            panel%num_references = num_refs

            do i = 1, num_refs
                if (allocated(references(i)%uri)) then
                    allocate(character(len=len(references(i)%uri)) :: panel%references(i)%uri)
                    panel%references(i)%uri = references(i)%uri
                end if

                if (allocated(references(i)%filename)) then
                    allocate(character(len=len(references(i)%filename)) :: panel%references(i)%filename)
                    panel%references(i)%filename = references(i)%filename
                end if

                if (allocated(references(i)%preview_text)) then
                    allocate(character(len=len(references(i)%preview_text)) :: panel%references(i)%preview_text)
                    panel%references(i)%preview_text = references(i)%preview_text
                end if

                panel%references(i)%line = references(i)%line
                panel%references(i)%column = references(i)%column
                panel%references(i)%end_line = references(i)%end_line
                panel%references(i)%end_column = references(i)%end_column

                ! Measure display width: " basename:line:col "
                if (allocated(references(i)%filename)) then
                    write(loc_tmp, '(A,A,I0,A,I0)') &
                        trim(get_basename(references(i)%filename)), &
                        ':', references(i)%line, ':', references(i)%column
                else
                    write(loc_tmp, '(I0,A,I0)') &
                        references(i)%line, ':', references(i)%column
                end if
                entry_len = len_trim(loc_tmp) + 3  ! +3 for leading/trailing spaces and margin
                if (entry_len > max_entry_len) max_entry_len = entry_len
            end do
        end if

        ! Set symbol name
        if (present(symbol_name)) then
            if (allocated(panel%symbol_name)) deallocate(panel%symbol_name)
            allocate(character(len=len_trim(symbol_name)) :: panel%symbol_name)
            panel%symbol_name = trim(symbol_name)
        end if

        ! Dynamic width: fit longest entry, min 30, max screen width
        panel%width = max(30, min(max_entry_len + 4, panel%screen_width))

        panel%selected_index = 1
        panel%scroll_offset = 0
    end subroutine set_references

    subroutine clear_references(panel)
        type(references_panel_t), intent(inout) :: panel
        call cleanup_references_panel(panel)
    end subroutine clear_references

    subroutine show_references_panel(panel, screen_width, screen_height)
        type(references_panel_t), intent(inout) :: panel
        integer, intent(in) :: screen_width, screen_height
        integer :: i, entry_len, max_entry_len
        character(len=100) :: loc_tmp

        panel%screen_width = screen_width
        panel%screen_height = screen_height
        panel%visible = .true.

        ! Recalculate width now that screen_width is known
        max_entry_len = 0
        do i = 1, panel%num_references
            if (allocated(panel%references(i)%filename)) then
                write(loc_tmp, '(A,A,I0,A,I0)') &
                    trim(get_basename(panel%references(i)%filename)), &
                    ':', panel%references(i)%line, &
                    ':', panel%references(i)%column
            else
                write(loc_tmp, '(I0,A,I0)') &
                    panel%references(i)%line, ':', panel%references(i)%column
            end if
            entry_len = len_trim(loc_tmp) + 3
            if (entry_len > max_entry_len) max_entry_len = entry_len
        end do
        panel%width = max(30, min(max_entry_len + 4, screen_width))
    end subroutine show_references_panel

    subroutine hide_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        panel%visible = .false.
    end subroutine hide_references_panel

    subroutine toggle_references_panel(panel)
        type(references_panel_t), intent(inout) :: panel
        panel%visible = .not. panel%visible
    end subroutine toggle_references_panel

    function is_references_panel_visible(panel) result(visible)
        type(references_panel_t), intent(in) :: panel
        logical :: visible
        visible = panel%visible
    end function is_references_panel_visible

    subroutine render_references_panel(panel, start_row)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: start_row
        integer :: start_col

        if (.not. panel%visible) return

        start_col = panel%screen_width - panel%width + 1
        call render_references_panel_at(panel, start_col, panel%width, &
                                        start_row, panel%screen_height - 1)
    end subroutine render_references_panel

    ! Shared by the compact docked view and the wider off-canvas LSP view.
    subroutine render_references_panel_at(panel, start_col, width, start_row, end_row)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: start_col, width, start_row, end_row
        integer :: body_start, body_end, i, max_visible, row, visible_index

        if (.not. panel%visible .or. width <= 0 .or. end_row < start_row) return

        call render_reference_header(panel, start_row, start_col, width)
        if (start_row + 1 <= end_row) then
            call render_reference_section(panel, start_row + 1, start_col, width)
        end if

        body_start = start_row + 3
        body_end = end_row - 1
        max_visible = max(0, body_end - body_start + 1)

        if (start_row + 2 < end_row) then
            call render_reference_filled_row(start_row + 2, start_col, width, '', THEME_PANEL)
        end if
        do row = body_start, body_end
            call render_reference_filled_row(row, start_col, width, '', THEME_PANEL)
        end do

        if (max_visible > 0) then
            if (panel%num_references == 0) then
                call render_reference_message(body_start, start_col, width, &
                                              '  No references found')
            else
                do i = 1, min(max_visible, &
                              max(0, panel%num_references - panel%scroll_offset))
                    visible_index = panel%scroll_offset + i
                    if (visible_index > panel%num_references) exit
                    call render_reference_entry(panel, visible_index, &
                                                body_start + i - 1, start_col, width)
                end do
            end if
        end if

        if (end_row > start_row) then
            call render_reference_footer(panel, end_row, start_col, width)
        end if

        call region_add(REGION_BLOCK, start_row, end_row, start_col, &
                        start_col + width - 1)
    end subroutine render_references_panel_at

    subroutine render_reference_header(panel, row, start_col, width)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: row, start_col, width
        character(len=64) :: count_buffer
        character(len=:), allocatable :: count_text

        if (panel%num_references == 1) then
            write(count_buffer, '(I0,A)') panel%num_references, ' result'
        else
            write(count_buffer, '(I0,A)') panel%num_references, ' results'
        end if
        count_text = trim(count_buffer) // '  '
        call render_reference_split_row(row, start_col, width, '  References', &
                                        count_text, THEME_PANEL_HEADER, THEME_MUTED)
    end subroutine render_reference_header

    subroutine render_reference_section(panel, row, start_col, width)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: row, start_col, width
        character(len=:), allocatable :: label, suffix, shown
        integer :: label_cells, suffix_cells, used

        label = '  LOCATIONS'
        suffix = ''
        if (allocated(panel%symbol_name)) then
            if (len_trim(panel%symbol_name) > 0) suffix = '  ' // trim(panel%symbol_name)
        end if

        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_PANEL))
        call clip_to_cells(label, width, shown, used)
        call terminal_write(theme_foreground_sgr(THEME_ACCENT) // shown)
        label_cells = used

        if (len(suffix) > 0 .and. label_cells < width) then
            call clip_to_cells(suffix, width - label_cells, shown, suffix_cells)
            call terminal_write(theme_foreground_sgr(THEME_MUTED) // shown)
        else
            suffix_cells = 0
        end if
        call terminal_write(repeat(' ', max(0, width - label_cells - suffix_cells)) // &
                            theme_reset())
    end subroutine render_reference_section

    subroutine render_reference_entry(panel, index, row, start_col, width)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: index, row, start_col, width
        character(len=64) :: location_buffer
        character(len=:), allocatable :: filename, location, marker, shown
        integer :: base_role, filename_budget, filename_cells, location_cells
        integer :: marker_cells, padding

        if (allocated(panel%references(index)%filename)) then
            filename = get_basename(panel%references(index)%filename)
        else
            filename = '(current file)'
        end if
        write(location_buffer, '(I0,A,I0)') panel%references(index)%line, ':', &
                                            panel%references(index)%column
        location = '  ' // trim(location_buffer) // '  '

        if (index == panel%selected_index) then
            base_role = THEME_PANEL_SELECTION
            marker = ' ' // theme_glyph('chevron_right') // ' '
        else
            base_role = THEME_PANEL
            marker = '   '
        end if

        marker_cells = utf8_display_width(marker)
        location_cells = utf8_display_width(location)
        filename_budget = width - marker_cells - location_cells

        if (filename_budget < 1) then
            call render_reference_filled_row(row, start_col, width, &
                marker // trim(filename) // ':' // trim(location_buffer), base_role)
            return
        end if

        call clip_to_cells(filename, filename_budget, shown, filename_cells)
        padding = max(0, filename_budget - filename_cells)

        call terminal_move_cursor(row, start_col)
        if (index == panel%selected_index) then
            ! Keep reverse video active across the whole selected row. A
            ! foreground-only SGR intentionally clears reverse, which would
            ! expose the role's stored pre-inversion background halfway over.
            call terminal_write(theme_sgr(base_role) // marker // shown // &
                                repeat(' ', padding) // location // theme_reset())
        else
            call terminal_write(theme_sgr(base_role) // &
                                theme_foreground_sgr(THEME_MUTED) // marker // &
                                theme_foreground_sgr(base_role) // shown // &
                                repeat(' ', padding) // &
                                theme_foreground_sgr(THEME_MUTED) // location // &
                                theme_reset())
        end if
    end subroutine render_reference_entry

    subroutine render_reference_footer(panel, row, start_col, width)
        type(references_panel_t), intent(in) :: panel
        integer, intent(in) :: row, start_col, width
        character(len=64) :: count_buffer
        character(len=:), allocatable :: hints, count_text

        if (width >= 42) then
            hints = '  ↑↓ Navigate  Enter Open  Esc Close'
        else
            hints = '  j/k  Enter  Esc'
        end if

        if (panel%num_references > 0) then
            write(count_buffer, '(I0,A,I0)') panel%selected_index, '/', panel%num_references
        else
            count_buffer = '0/0'
        end if
        count_text = trim(count_buffer) // '  '

        call render_reference_split_row(row, start_col, width, hints, count_text, &
                                        THEME_PANEL_FOOTER, THEME_ACCENT)
    end subroutine render_reference_footer

    subroutine render_reference_message(row, start_col, width, text)
        integer, intent(in) :: row, start_col, width
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: shown
        integer :: used

        call clip_to_cells(text, width, shown, used)
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(THEME_PANEL) // &
                            theme_foreground_sgr(THEME_MUTED) // shown // &
                            repeat(' ', max(0, width - used)) // theme_reset())
    end subroutine render_reference_message

    subroutine render_reference_filled_row(row, start_col, width, text, role)
        integer, intent(in) :: row, start_col, width, role
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: shown
        integer :: used

        call clip_to_cells(text, width, shown, used)
        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(role) // shown // &
                            repeat(' ', max(0, width - used)) // theme_reset())
    end subroutine render_reference_filled_row

    subroutine render_reference_split_row(row, start_col, width, left, right, &
                                          surface_role, right_role)
        integer, intent(in) :: row, start_col, width, surface_role, right_role
        character(len=*), intent(in) :: left, right
        character(len=:), allocatable :: shown_left, shown_right
        integer :: left_cells, right_cells, gap

        call clip_to_cells(left, width, shown_left, left_cells)
        call clip_to_cells(right, width, shown_right, right_cells)

        call terminal_move_cursor(row, start_col)
        call terminal_write(theme_sgr(surface_role) // shown_left)
        if (left_cells + right_cells <= width) then
            gap = width - left_cells - right_cells
            call terminal_write(repeat(' ', gap) // &
                                theme_foreground_sgr(right_role) // shown_right)
        else
            call terminal_write(repeat(' ', max(0, width - left_cells)))
        end if
        call terminal_write(theme_reset())
    end subroutine render_reference_split_row

    function references_panel_handle_key(panel, key) result(handled)
        type(references_panel_t), intent(inout) :: panel
        character(len=*), intent(in) :: key
        logical :: handled
        integer :: max_visible

        handled = .false.
        if (.not. panel%visible) return

        max_visible = max(1, panel%screen_height - 5)

        select case(trim(key))
        case('j', 'down')
            ! Move selection down
            if (panel%selected_index < panel%num_references) then
                panel%selected_index = panel%selected_index + 1

                ! Adjust scroll if needed
                if (panel%selected_index > panel%scroll_offset + max_visible) then
                    panel%scroll_offset = panel%selected_index - max_visible
                end if
                handled = .true.
            end if

        case('k', 'up')
            ! Move selection up
            if (panel%selected_index > 1) then
                panel%selected_index = panel%selected_index - 1

                ! Adjust scroll if needed
                if (panel%selected_index <= panel%scroll_offset) then
                    panel%scroll_offset = max(0, panel%selected_index - 1)
                end if
                handled = .true.
            end if

        case('pagedown')
            ! Page down
            panel%selected_index = min(panel%num_references, &
                panel%selected_index + max_visible)
            panel%scroll_offset = min(max(0, panel%num_references - max_visible), &
                panel%scroll_offset + max_visible)
            handled = .true.

        case('pageup')
            ! Page up
            panel%selected_index = max(1, panel%selected_index - max_visible)
            panel%scroll_offset = max(0, panel%scroll_offset - max_visible)
            handled = .true.

        case('home')
            ! Jump to first
            panel%selected_index = 1
            panel%scroll_offset = 0
            handled = .true.

        case('end')
            ! Jump to last
            panel%selected_index = panel%num_references
            panel%scroll_offset = max(0, panel%num_references - max_visible)
            handled = .true.

        case('enter')
            ! User wants to jump to this reference
            handled = .true.

        case('escape', 'shift-f12')
            ! Hide panel
            panel%visible = .false.
            handled = .true.
        end select
    end function references_panel_handle_key

    function get_selected_reference_location(panel, uri, line, col) result(has_location)
        type(references_panel_t), intent(in) :: panel
        character(len=:), allocatable, intent(out) :: uri
        integer(int32), intent(out) :: line, col
        logical :: has_location

        has_location = .false.

        if (panel%selected_index > 0 .and. panel%selected_index <= panel%num_references) then
            if (allocated(panel%references(panel%selected_index)%uri)) then
                allocate(character(len=len(panel%references(panel%selected_index)%uri)) :: uri)
                uri = panel%references(panel%selected_index)%uri
                line = panel%references(panel%selected_index)%line
                col = panel%references(panel%selected_index)%column
                has_location = .true.
            end if
        end if
    end function get_selected_reference_location

    ! Helper function to extract basename from path
    function get_basename(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: basename
        integer :: i, last_slash

        last_slash = 0
        do i = len_trim(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                last_slash = i
                exit
            end if
        end do
        if (last_slash > 0) then
            basename = path(last_slash+1:)
        else
            basename = path
        end if
    end function get_basename

end module references_panel_module
