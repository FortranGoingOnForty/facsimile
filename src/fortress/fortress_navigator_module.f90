! Fortress Navigator Integration for fac
! Main API for opening file/directory navigator (Ctrl-O)

module fortress_navigator_module
    use iso_fortran_env, only: output_unit, input_unit
    use fortress_fs_module
    use fortress_display_module
    use terminal_io_module, only: terminal_read_char, terminal_write, terminal_move_cursor, terminal_flush, &
                                  terminal_consume_csi, ESC_STANDALONE, ESC_OTHER
    use favorites_module, only: favorites_add
    implicit none
    private

    public :: open_fortress_navigator
    ! The same browser, driven by the editor's loop instead of its own. Same
    ! shape as group_picker_module, deliberately: show/hide/visible, a key
    ! handler that claims or declines, a render, and a result the caller acts
    ! on. See is_group_picker_visible and friends.
    public :: fortress_show, fortress_hide, is_fortress_visible
    public :: fortress_handle_key, render_fortress, fortress_click
    public :: fortress_result, fortress_path, fortress_is_dir, fortress_as_group
    public :: FT_PENDING, FT_CONFIRMED, FT_CANCELLED

    ! classify_escape result for a navigation key, distinct from the ESC_*
    ! values terminal_io returns for sequences it swallowed
    integer, parameter :: NAV_ARROW = 3
    ! Shift+Enter, which only exists as a kitty CSI-u report. Terminals that
    ! do not speak that protocol send a plain Enter for it and cannot tell
    ! the two apart, which is why Ctrl-G does the same thing.
    integer, parameter :: NAV_GROUP = 4

    integer, parameter :: FT_PENDING = 0, FT_CONFIRMED = 1, FT_CANCELLED = 2

    ! Modal state. The navigation state below is shared with the blocking
    ! driver -- the two are never live at once, and fortress_show resets it
    ! so nothing is inherited from a previous visit.
    logical :: g_ft_visible = .false.
    integer :: g_ft_result = FT_PENDING
    character(len=MAX_PATH) :: g_ft_path = ''
    logical :: g_ft_is_dir = .false.
    logical :: g_ft_as_group = .false.
    ! Where the box was last drawn, so a click can be mapped back to a row.
    integer :: g_ft_row0 = 0, g_ft_col0 = 0, g_ft_h = 0, g_ft_w = 0
    integer :: g_ft_inner_h = 0

    ! Navigation state
    character(len=MAX_PATH), dimension(MAX_FILES) :: current_files, parent_files
    logical, dimension(MAX_FILES) :: current_is_dir, parent_is_dir
    logical, dimension(MAX_FILES) :: current_is_exec, parent_is_exec
    integer :: current_count, parent_count
    integer :: selected, parent_selected
    integer :: scroll_offset, parent_scroll_offset
    ! Where we are, and what the listings were last read for. Module state
    ! because BOTH drivers need it -- the blocking loop kept these as locals,
    ! which is why the modal could not see them.
    character(len=MAX_PATH) :: current_dir = '', last_dir = '', last_parent = ''
    ! Set when we ascend: the directory we came OUT of. The selection cannot
    ! be resolved at the time the key is pressed, because the listing that
    ! contains that directory has not been read yet -- so record it and let
    ! the refresh put the selection on it.
    character(len=MAX_PATH) :: pending_reveal = ''
    ! How many rows the list pane last had. A page key has to move by what
    ! the user can see, and the two drivers size their pane differently, so
    ! whichever drew last records it.
    integer :: vis_h_last = 10

    ! Fuzzy search state
    character(len=32) :: search_buffer = ''
    integer :: search_len = 0
    integer(8) :: last_search_tick = 0

contains

    !> Main entry point: open fortress navigator and return selection
    !! @param selected_path - Output: selected file/directory path (empty if cancelled)
    !! @param is_directory - Output: true if selected item is directory
    !! @param cancelled - Output: true if user pressed ESC/q
    !! @param initial_path - Input (optional): starting directory
    subroutine open_fortress_navigator(selected_path, is_directory, cancelled, &
                                       initial_path, as_group)
        character(len=:), allocatable, intent(out) :: selected_path
        logical, intent(out) :: is_directory, cancelled
        !> Set when the directory was chosen with Shift+Enter (or Ctrl-G):
        !> the caller should offer to make a TAB GROUP of it rather than
        !> switch the workspace. Plain Enter leaves this false and keeps its
        !> old meaning. Reporting rather than deciding, because the startup
        !> navigator has no workspace for a group to live in and maps both
        !> onto opening one.
        logical, intent(out), optional :: as_group
        character(len=*), intent(in), optional :: initial_path
        character(len=MAX_PATH) :: parent_dir, temp_dir
        character(len=1) :: key
        integer :: esc_kind
        integer :: rows, cols, ios, last_selected, last_scroll
        logical :: running, dir_changed, first_draw, need_redraw

        ! Initialize state
        if (present(as_group)) as_group = .false.
        selected = 1
        parent_selected = -1
        scroll_offset = 0
        parent_scroll_offset = 0
        running = .true.
        cancelled = .false.
        last_dir = ""
        last_parent = ""
        pending_reveal = ''
        first_draw = .true.
        need_redraw = .true.
        last_selected = -1
        last_scroll = -1

        ! Set initial directory
        if (present(initial_path)) then
            current_dir = initial_path
        else
            current_dir = get_pwd()
        end if

        ! Get terminal size (re-polled each iteration below so a resize
        ! during navigation reflows the panes)
        call get_term_size(rows, cols)

        ! Main navigation loop
        do while (running)
            ! Detect terminal resize and force a full redraw
            block
                integer :: nrows, ncols
                call get_term_size(nrows, ncols)
                if (nrows /= rows .or. ncols /= cols) then
                    rows = nrows
                    cols = ncols
                    first_draw = .true.  ! full clear at the new size
                end if
            end block
            ! Only refresh directory listings if directory changed
            dir_changed = (current_dir /= last_dir)
            if (dir_changed) then
                parent_dir = get_parent_path(current_dir)

                ! Only refresh parent if it changed too
                if (parent_dir /= last_parent) then
                    call get_file_list(parent_dir, parent_files, parent_is_dir, parent_is_exec, parent_count)
                    last_parent = parent_dir
                end if

                call get_file_list(current_dir, current_files, current_is_dir, current_is_exec, current_count)
                last_dir = current_dir
            else
                ! Just update parent_dir for consistency
                parent_dir = get_parent_path(current_dir)
            end if

            ! Now that the listing exists, put the selection back where an
            ! ascent asked for it.
            call resolve_pending_reveal()

            ! Find current directory in parent listing
            parent_selected = find_in_parent(current_dir, parent_files, parent_count)

            ! Bounds check
            if (selected < 1) selected = 1
            if (selected > current_count) selected = current_count
            if (current_count == 0) selected = 1

            ! Adjust scroll offsets. The current pane keeps a margin so
            ! the cursor stays off the edges. The parent pane is anchored
            ! to the top: all siblings list from row 1 down, scrolling
            ! only if the current directory would fall below the fold
            ! (no centering margin) — matches reference fortress.
            vis_h_last = max(1, rows - 4)
            call adjust_scroll(selected, scroll_offset, rows - 4)
            call adjust_parent_scroll(parent_selected, &
                parent_scroll_offset, parent_count, rows - 4)

            ! Check if we need to redraw (directory changed, selection changed, or scroll changed)
            need_redraw = dir_changed .or. first_draw .or. &
                         selected /= last_selected .or. scroll_offset /= last_scroll

            ! Render interface only if something changed
            if (need_redraw) then
                call draw_fortress_interface(rows, cols, current_dir, &
                                             current_files, current_is_dir, current_is_exec, current_count, &
                                             parent_files, parent_is_dir, parent_count, &
                                             selected, parent_selected, scroll_offset, parent_scroll_offset, first_draw)

                call terminal_flush()

                ! Update tracking variables
                last_selected = selected
                last_scroll = scroll_offset
                if (first_draw) first_draw = .false.
            end if

            ! Read key using raw terminal input
            ! Note: terminal_read_char is non-blocking, returns -1 if no input
            ios = terminal_read_char()
            if (ios < 0) cycle
            key = achar(ios)

            ! Fuzzy search: printable chars are type-to-jump
            ! (processed before control keys so letters aren't
            ! consumed by single-key bindings)
            if ((ios >= ichar('a') .and. ios <= ichar('z')) .or. &
                (ios >= ichar('A') .and. ios <= ichar('Z')) .or. &
                (ios >= ichar('0') .and. ios <= ichar('9')) .or. &
                key == '-' .or. key == '_' .or. key == '.') then
                call fortress_fuzzy_search(key)
                cycle
            end if

            ! Backspace removes last search character
            if (ios == 127 .or. ios == 8) then
                if (search_len > 0) then
                    search_len = search_len - 1
                    if (search_len > 0) then
                        call fortress_fuzzy_jump( &
                            search_buffer(1:search_len))
                    end if
                end if
                cycle
            end if

            ! Handle control/special keys
            select case (key)
                case (char(27))  ! ESC — arrow key, swallowed sequence, or quit
                    search_len = 0; search_buffer = ''
                    esc_kind = classify_escape(key)
                    if (esc_kind == NAV_GROUP) then
                        if (current_count > 0) then
                            if (current_is_dir(selected)) then
                                selected_path = join_path(current_dir, &
                                    trim(current_files(selected)))
                                is_directory = .true.
                                if (present(as_group)) as_group = .true.
                                running = .false.
                            end if
                        end if
                    else if (esc_kind == NAV_ARROW) then
                        call handle_arrow_key(key, selected, &
                            current_dir, temp_dir, &
                            current_files, &
                            current_is_dir, current_count)
                    else if (esc_kind == ESC_STANDALONE) then
                        ! Standalone ESC — always quit
                        cancelled = .true.
                        running = .false.
                    end if
                    ! A mouse report or other CSI was consumed: stay put

                case (char(7))  ! Ctrl-G — same as Shift+Enter, for terminals
                                ! that cannot report it
                    if (current_count > 0) then
                        if (current_is_dir(selected)) then
                            selected_path = join_path(current_dir, &
                                trim(current_files(selected)))
                            is_directory = .true.
                            if (present(as_group)) as_group = .true.
                            running = .false.
                        end if
                    end if

                case (char(17))  ! Ctrl-Q — quit
                    cancelled = .true.
                    running = .false.

                case (char(10), char(13))  ! Enter
                    if (current_count > 0) then
                        if (current_is_dir(selected)) then
                            selected_path = join_path(current_dir, &
                                trim(current_files(selected)))
                            is_directory = .true.
                            running = .false.
                        else
                            selected_path = join_path(current_dir, &
                                trim(current_files(selected)))
                            is_directory = .false.
                            running = .false.
                        end if
                    end if

                case ('~')  ! Jump to home
                    call get_environment_variable("HOME", &
                        current_dir)
                    selected = 1
                    scroll_offset = 0
                    search_len = 0; search_buffer = ''

                case ('/')  ! Jump to root
                    current_dir = "/"
                    selected = 1
                    scroll_offset = 0
                    search_len = 0; search_buffer = ''

                case (char(6))  ! Ctrl-F — add to favorites
                    call add_to_favorites(current_dir, rows)

            end select
        end do

        ! Set outputs based on result. The loop already sets
        ! selected_path and is_directory correctly for both
        ! file and directory selections — don't override them.
        if (.not. cancelled) then
            if (.not. allocated(selected_path)) then
                ! No explicit selection — treat current directory
                ! as the selection (e.g. user navigated into it
                ! via arrows but didn't press Enter on an item)
                selected_path = trim(current_dir)
                is_directory = .true.
            end if
        else
            selected_path = ""
            is_directory = .false.
        end if

    end subroutine open_fortress_navigator

    !> Adjust scroll offset to keep selection visible with margin
    subroutine adjust_scroll(sel, offset, visible_height)
        integer, intent(in) :: sel, visible_height
        integer, intent(inout) :: offset
        integer :: margin

        ! Add a margin to avoid selection being at the very edge
        margin = 3
        if (margin > visible_height / 4) margin = visible_height / 4

        ! If selection is above the visible window (with margin)
        if (sel < offset + 1 + margin) then
            offset = sel - margin - 1
            if (offset < 0) offset = 0
        ! If selection is below the visible window (with margin)
        else if (sel > offset + visible_height - margin) then
            offset = sel - visible_height + margin
        end if

        ! Ensure offset is not negative
        if (offset < 0) offset = 0
    end subroutine adjust_scroll

    !> Top-anchored scroll for the parent pane: keep the list pinned to
    !! the top so all siblings render from row 1 down, scrolling only
    !! when the selection would otherwise fall outside the visible area.
    subroutine adjust_parent_scroll(sel, offset, total, visible_height)
        integer, intent(in) :: sel, total, visible_height
        integer, intent(inout) :: offset

        if (sel <= 0) then
            offset = 0
            return
        end if

        ! Scroll up only if the selection is above the window
        if (sel < offset + 1) offset = sel - 1
        ! Scroll down only if the selection is below the window
        if (sel > offset + visible_height) offset = sel - visible_height

        ! Clamp so we never scroll past the end or before the top
        offset = max(0, min(offset, max(0, total - visible_height)))
    end subroutine adjust_parent_scroll

    !> Classify an ESC byte: a navigation key, a sequence to swallow, or a
    !> real ESC keypress. Returns NAV_ARROW with `key` set to the final byte,
    !> or one of terminal_io's ESC_* values.
    !>
    !> The swallow case matters: a click arrives as ESC [ < b ; c ; r M, and
    !> the old two-way version returned '<' as an "arrow", after which the
    !> digits and the trailing M came back round the loop and were fed to
    !> type-to-jump one at a time, moving the selection on every click.
    function classify_escape(key) result(kind)
        character(len=1), intent(inout) :: key
        integer :: kind
        integer :: char_code

        ! Deliberately narrow: every path except a mouse report behaves
        ! exactly as the previous two-way version did, because the navigator
        ! is hard to exercise in a pty and a broader change could not be
        ! verified. Only the '<' (SGR) and 'M' (legacy X10) introducers are
        ! new, and both were previously mishandled.
        kind = ESC_STANDALONE
        if (key /= char(27)) return

        char_code = terminal_read_char()
        if (char_code < 0) return          ! nothing followed: a real ESC
        if (achar(char_code) /= '[') return

        char_code = terminal_read_char()
        if (char_code < 0) return

        if (char_code == iachar('<') .or. char_code == iachar('M')) then
            ! A mouse report. Swallow it whole: the old code returned '<' as
            ! an unrecognised "arrow" and the digits then reached
            ! type-to-jump one at a time.
            kind = terminal_consume_csi(char_code)
            return
        end if

        ! A CSI-u key report, e.g. ESC [ 13 ; 2 u for Shift+Enter. Parsed
        ! rather than passed through: the old code returned the FIRST digit
        ! as an "arrow" and let the rest reach type-to-jump one character at
        ! a time, so any such key silently corrupted the search buffer.
        if (char_code >= iachar('0') .and. char_code <= iachar('9')) then
            block
                integer :: cp, mods, val, c
                cp = 0
                mods = 0
                val = char_code - iachar('0')
                c = -1
                do
                    c = terminal_read_char()
                    if (c < 0) exit
                    if (c >= iachar('0') .and. c <= iachar('9')) then
                        val = val * 10 + (c - iachar('0'))
                    else if (c == iachar(';')) then
                        if (cp == 0) cp = val
                        val = 0
                    else
                        exit                    ! final byte
                    end if
                end do
                if (cp == 0) cp = val
                mods = val
                ! 13 is Enter; modifier 2 is Shift (the encoding is 1 + bits).
                if (c == iachar('u') .and. cp == 13 .and. mods == 2) then
                    kind = NAV_GROUP
                else if (c == iachar('~')) then
                    ! The tilde-terminated keys. Reported this way they used
                    ! to be swallowed whole, so a long directory could only
                    ! be walked one row at a time.
                    select case (cp)
                    case (1, 7)
                        key = 'H'; kind = NAV_ARROW
                    case (4, 8)
                        key = 'F'; kind = NAV_ARROW
                    case (5)
                        key = 'P'; kind = NAV_ARROW
                    case (6)
                        key = 'N'; kind = NAV_ARROW
                    case default
                        kind = ESC_OTHER
                    end select
                else
                    kind = ESC_OTHER
                end if
            end block
            return
        end if

        key = achar(char_code)
        kind = NAV_ARROW
    end function classify_escape

    ! ---- modal driver -------------------------------------------------
    !
    ! The blocking driver above owns its own input loop; this one owns
    ! nothing and is stepped by the editor. Both reach the SAME navigation
    ! helpers -- handle_arrow_key, the fuzzy search, the listing refresh --
    ! so a key cannot come to mean two different things.

    subroutine fortress_show(start_dir)
        character(len=*), intent(in), optional :: start_dir

        ! Reset explicitly. Two drivers share this state and a `selected`
        ! carried over from a previous visit would point into a listing that
        ! is about to be replaced.
        selected = 1
        parent_selected = -1
        scroll_offset = 0
        parent_scroll_offset = 0
        search_len = 0
        search_buffer = ''
        last_dir = ''
        last_parent = ''
        pending_reveal = ''

        if (present(start_dir)) then
            if (len_trim(start_dir) > 0) then
                current_dir = start_dir
            else
                current_dir = get_pwd()
            end if
        else
            current_dir = get_pwd()
        end if

        g_ft_result = FT_PENDING
        g_ft_path = ''
        g_ft_is_dir = .false.
        g_ft_as_group = .false.
        g_ft_visible = .true.
    end subroutine fortress_show

    subroutine fortress_hide()
        g_ft_visible = .false.
    end subroutine fortress_hide

    logical function is_fortress_visible()
        is_fortress_visible = g_ft_visible
    end function is_fortress_visible

    integer function fortress_result()
        fortress_result = g_ft_result
    end function fortress_result

    function fortress_path() result(p)
        character(len=:), allocatable :: p
        p = trim(g_ft_path)
    end function fortress_path

    logical function fortress_is_dir()
        fortress_is_dir = g_ft_is_dir
    end function fortress_is_dir

    logical function fortress_as_group()
        fortress_as_group = g_ft_as_group
    end function fortress_as_group

    !> Put the selection back on the directory we ascended out of, now that
    !> the listing that contains it has actually been read. Must run before
    !> the scroll is adjusted, or the view follows the old selection.
    subroutine resolve_pending_reveal()
        if (len_trim(pending_reveal) == 0) return
        if (current_count > 0) &
            selected = find_in_parent(pending_reveal, current_files, &
                                      current_count)
        pending_reveal = ''
    end subroutine resolve_pending_reveal

    !> Bring the listings and the selection into agreement with current_dir.
    !> The blocking loop does this at the top of every iteration; the modal
    !> does it before drawing.
    subroutine fortress_sync(vis_h)
        integer, intent(in) :: vis_h
        character(len=MAX_PATH) :: parent_dir

        if (current_dir /= last_dir) then
            parent_dir = get_parent_path(current_dir)
            if (parent_dir /= last_parent) then
                call get_file_list(parent_dir, parent_files, parent_is_dir, &
                                   parent_is_exec, parent_count)
                last_parent = parent_dir
            end if
            call get_file_list(current_dir, current_files, current_is_dir, &
                               current_is_exec, current_count)
            last_dir = current_dir
        end if

        vis_h_last = max(1, vis_h)
        call resolve_pending_reveal()
        parent_selected = find_in_parent(current_dir, parent_files, parent_count)
        if (selected < 1) selected = 1
        if (selected > current_count) selected = current_count
        if (current_count == 0) selected = 1
        call adjust_scroll(selected, scroll_offset, vis_h)
        call adjust_parent_scroll(parent_selected, parent_scroll_offset, &
                                  parent_count, vis_h)
    end subroutine fortress_sync

    !> True when the key was ours. The caller stops looking; anything we
    !> decline goes on to the editor.
    logical function fortress_handle_key(key_str) result(claimed)
        character(len=*), intent(in) :: key_str
        character(len=MAX_PATH) :: temp_dir
        character(len=1) :: ch

        claimed = .false.
        if (.not. g_ft_visible) return
        claimed = .true.

        select case (trim(key_str))
        case ('up')
            call handle_arrow_key('A', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('down')
            call handle_arrow_key('B', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('right')
            call handle_arrow_key('C', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('left')
            call handle_arrow_key('D', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('home')
            call handle_arrow_key('H', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('end')
            call handle_arrow_key('F', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('pageup')
            call handle_arrow_key('P', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('pagedown')
            call handle_arrow_key('N', selected, current_dir, temp_dir, &
                                  current_files, current_is_dir, current_count)
        case ('esc')
            g_ft_result = FT_CANCELLED
        case ('enter')
            call fortress_choose(.false.)
        case ('shift-enter', 'ctrl-g')
            call fortress_choose(.true.)
        case ('backspace')
            if (search_len > 0) then
                search_len = search_len - 1
                if (search_len > 0) call fortress_fuzzy_jump(search_buffer(1:search_len))
            end if
        case default
            ! Type-to-jump. Single printable characters only, so editor
            ! chords fall through to be declined rather than typed.
            if (len_trim(key_str) == 1) then
                ch = key_str(1:1)
                if ((ch >= 'a' .and. ch <= 'z') .or. (ch >= 'A' .and. ch <= 'Z') .or. &
                    (ch >= '0' .and. ch <= '9') .or. ch == '-' .or. ch == '_' .or. &
                    ch == '.') then
                    call fortress_fuzzy_search(ch)
                else
                    claimed = .false.
                end if
            else
                claimed = .false.
            end if
        end select
    end function fortress_handle_key

    !> Record what was picked. A file is always just a file; a directory is
    !> either a workspace or a tab group, which is the caller's business.
    subroutine fortress_choose(as_group)
        logical, intent(in) :: as_group

        if (current_count <= 0) return
        if (selected < 1 .or. selected > current_count) return
        if (as_group .and. .not. current_is_dir(selected)) return

        g_ft_path = join_path(current_dir, trim(current_files(selected)))
        g_ft_is_dir = current_is_dir(selected)
        g_ft_as_group = as_group
        g_ft_result = FT_CONFIRMED
    end subroutine fortress_choose

    !> A click inside the listing selects that row; a click anywhere else in
    !> the box is swallowed so it cannot reach the document underneath.
    logical function fortress_click(row, col) result(claimed)
        integer, intent(in) :: row, col
        integer :: idx, vis, left_w

        claimed = .false.
        if (.not. g_ft_visible) return
        if (row < g_ft_row0 .or. row > g_ft_row0 + g_ft_h - 1) return
        if (col < g_ft_col0 .or. col > g_ft_col0 + g_ft_w - 1) return
        claimed = .true.

        ! Given the inner rect and told to draw no chrome, the display puts
        ! visible row i at inner_row - 1 + i -- which is the box's own row0
        ! plus i. The old arithmetic here subtracted two header rows that a
        ! window does not have, so it would have picked the wrong entry; it
        ! was never noticed because no click ever reached this function.
        vis = row - g_ft_row0
        if (vis < 1 .or. vis > g_ft_inner_h) return

        ! Only the current pane. The left thirty per cent lists the parent's
        ! siblings, and moving the selection in response to a click there
        ! would point at an unrelated entry. Swallowed, not acted on.
        left_w = ((g_ft_w - 2) * 3) / 10
        if (col < g_ft_col0 + 1 + left_w + 3) return

        idx = vis + scroll_offset
        if (idx >= 1 .and. idx <= current_count) selected = idx
    end function fortress_click

    !> Draw the window. Centred and about seven tenths of the area given,
    !> clamped so it stays usable on a small terminal.
    subroutine render_fortress(top_row, bottom_row, left_col, right_col)
        use modal_box_module, only: box_frame, box_inner_rect
        use fortress_display_module, only: draw_fortress_interface
        use clickable_region_module, only: region_add, REGION_BLOCK
        integer, intent(in) :: top_row, bottom_row, left_col, right_col
        integer :: avail_h, avail_w, h, w, r0, c0
        integer :: ir, ic, ih, iw

        if (.not. g_ft_visible) return

        avail_h = max(0, bottom_row - top_row + 1)
        avail_w = max(0, right_col - left_col + 1)
        h = min(avail_h, max(8, (avail_h * 7) / 10))
        w = min(avail_w, max(30, (avail_w * 7) / 10))
        if (h < 6 .or. w < 24) return          ! no room to be a window

        r0 = top_row + (avail_h - h) / 2
        c0 = left_col + (avail_w - w) / 2

        g_ft_row0 = r0
        g_ft_col0 = c0
        g_ft_h = h
        g_ft_w = w

        call box_inner_rect(r0, c0, h, w, ir, ic, ih, iw)
        g_ft_inner_h = ih

        ! Claim the rectangle, as the group dialog does. Without this a click
        ! inside the window found no region, fell through to the document
        ! underneath and moved the caret there -- and no click could ever
        ! reach fortress_click, which was written for exactly this and was
        ! never wired up.
        call region_add(REGION_BLOCK, r0, r0 + h - 1, c0, c0 + w - 1)

        ! The display reserves three rows of its own (header, blank, footer).
        ! The frame carries the title and the key hints, so the display is
        ! asked for the panes only -- otherwise the window shows two titles
        ! and two footers.
        call fortress_sync(ih)
        call box_frame(r0, c0, h, w, 'FORTRESS  ' // trim(current_dir), &
                       'arrows:nav  enter:open  S-enter/^g:group  esc:close')
        call draw_fortress_interface(ih, iw, current_dir, current_files, &
                                     current_is_dir, current_is_exec, current_count, &
                                     parent_files, parent_is_dir, parent_count, &
                                     selected, parent_selected, scroll_offset, &
                                     parent_scroll_offset, row0=ir, col0=ic, &
                                     chrome=.false.)
    end subroutine render_fortress

    !> Handle arrow key navigation
    subroutine handle_arrow_key(key, sel, curr_dir, temp_dir, files, is_dir, file_count)
        character(len=1), intent(in) :: key
        integer, intent(inout) :: sel
        character(len=MAX_PATH), intent(inout) :: curr_dir, temp_dir
        character(len=*), dimension(*), intent(in) :: files
        logical, dimension(*), intent(in) :: is_dir
        integer, intent(in) :: file_count

        select case (key)
            case ('A')  ! Up arrow
                if (sel > 1) sel = sel - 1

            case ('B')  ! Down arrow
                if (sel < file_count) sel = sel + 1

            ! Home/End/PageUp/PageDown. A directory with a few hundred
            ! entries was only reachable one row at a time.
            case ('H')  ! Home
                sel = 1

            case ('F')  ! End
                sel = max(1, file_count)

            case ('P')  ! Page up
                sel = max(1, sel - max(1, vis_h_last - 1))

            case ('N')  ! Page down
                sel = min(max(1, file_count), sel + max(1, vis_h_last - 1))

            case ('C')  ! Right arrow - enter directory
                if (file_count > 0 .and. is_dir(sel)) then
                    temp_dir = curr_dir
                    curr_dir = join_path(curr_dir, trim(files(sel)))
                    sel = 1
                    search_len = 0; search_buffer = ''
                end if

            case ('D')  ! Left arrow - go to parent
                temp_dir = curr_dir
                curr_dir = get_parent_path(curr_dir)
                ! Deliberately NOT resolved here. `files` still describes the
                ! directory being left; the parent is not read until the
                ! listings refresh. Searching the child's own listing for the
                ! child's name never matches, and the fallback put the
                ! selection on row 1 -- so backing out always lost your place.
                if (curr_dir /= temp_dir) pending_reveal = temp_dir
                search_len = 0; search_buffer = ''
        end select
    end subroutine handle_arrow_key

    !> Get terminal size using fac's terminal module
    subroutine get_term_size(rows, cols)
        use terminal_io_module, only: terminal_get_size
        integer, intent(out) :: rows, cols

        call terminal_get_size(rows, cols)

        ! Sanity check
        if (rows <= 0) rows = 24
        if (cols <= 0) cols = 80
    end subroutine get_term_size

    !> Add current directory to favorites
    subroutine add_to_favorites(dir_path, rows)
        character(len=*), intent(in) :: dir_path
        integer, intent(in) :: rows
        character(len=256) :: label
        logical :: success
        integer :: i

        ! Extract basename for label
        label = dir_path
        do i = len_trim(dir_path), 1, -1
            if (dir_path(i:i) == '/') then
                label = dir_path(i+1:)
                exit
            end if
        end do

        ! Add to favorites
        call favorites_add(dir_path, trim(label), success)

        ! Show feedback message
        call terminal_move_cursor(rows, 1)
        if (success) then
            call terminal_write('Added to favorites: ' // trim(label))
        else
            call terminal_write('Already in favorites or error')
        end if

        ! Pause briefly so user can see message
        call sleep_ms(800)
    end subroutine add_to_favorites

    !> Accumulate a character into the search buffer and jump
    subroutine fortress_fuzzy_search(ch)
        character(len=1), intent(in) :: ch
        integer(8) :: tick, rate

        call system_clock(tick, rate)
        if (rate <= 0) rate = 1000

        ! Reset buffer after 500ms timeout
        if (search_len > 0 .and. last_search_tick > 0) then
            if (tick - last_search_tick > rate / 2) then
                search_len = 0
                search_buffer = ''
            end if
        end if

        ! Append character
        if (search_len < 32) then
            search_len = search_len + 1
            search_buffer(search_len:search_len) = ch
            last_search_tick = tick
            call fortress_fuzzy_jump(search_buffer(1:search_len))
        end if
    end subroutine fortress_fuzzy_search

    !> Jump to the best prefix match in the current file list.
    !! Stays on the current selection if it still matches (no
    !! bouncing between items with the same prefix).
    subroutine fortress_fuzzy_jump(pattern)
        character(len=*), intent(in) :: pattern
        integer :: i, plen
        character(len=256) :: name_lower, pat_lower

        plen = len_trim(pattern)
        if (plen == 0) return
        if (current_count == 0) return

        pat_lower = to_lower_str(pattern(1:plen))

        ! Check current selection first (sticky)
        if (selected >= 1 .and. selected <= current_count) then
            name_lower = to_lower_str( &
                trim(current_files(selected)))
            if (len_trim(name_lower) >= plen) then
                if (name_lower(1:plen) == pat_lower(1:plen)) return
            end if
        end if

        ! Scan from current+1 with wrap
        do i = 1, current_count
            name_lower = to_lower_str( &
                trim(current_files(i)))
            if (len_trim(name_lower) >= plen) then
                if (name_lower(1:plen) == pat_lower(1:plen)) then
                    selected = i
                    return
                end if
            end if
        end do
    end subroutine fortress_fuzzy_jump

    !> Simple lowercase conversion for short strings
    function to_lower_str(s) result(low)
        character(len=*), intent(in) :: s
        character(len=256) :: low
        integer :: i, ic
        low = s
        do i = 1, len_trim(s)
            ic = ichar(s(i:i))
            if (ic >= ichar('A') .and. ic <= ichar('Z')) then
                low(i:i) = achar(ic + 32)
            end if
        end do
    end function to_lower_str

    !> Sleep for specified milliseconds
    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        integer :: i, j, dummy

        ! Simple busy-wait (not ideal but portable)
        dummy = 0
        do i = 1, milliseconds * 1000
            do j = 1, 100
                dummy = dummy + 1
            end do
        end do
    end subroutine sleep_ms

end module fortress_navigator_module
