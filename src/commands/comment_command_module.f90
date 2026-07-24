! Toggle line comments over the lines touched by every cursor (ctrl-/).
!
! Behaviour matches VSCode's "Toggle Line Comment":
!   * No selection  -> the cursor's line.
!   * Selection     -> every line the selection touches. A selection that ends
!                      in column 1 does not drag that last line in, so
!                      shift-down over three lines comments three lines, not
!                      four. Partial lines are always commented whole -- the
!                      selection's columns only pick the line range.
!   * Commenting is indent-aware: the token goes at the *shallowest* indent in
!                      the range, not at column 1 and not at each line's own
!                      indent, so a block keeps its relative shape and stays
!                      aligned under its own leading whitespace.
!   * Uncommenting fires only when every non-blank line in the range is
!                      already commented; otherwise the whole range is
!                      commented (so a half-commented block becomes fully
!                      commented, exactly like VSCode).
!   * Blank lines are skipped when commenting, but a range that is entirely
!                      blank still gets a token so an empty line can be
!                      commented on the spot.
!
! Languages with no line-comment token (html, css) fall back to wrapping the
! range in block delimiters.
module comment_command_module
    use text_buffer_module, only: buffer_t, buffer_get_line, buffer_get_line_count, &
                                  buffer_get_char, buffer_insert, buffer_delete
    use editor_state_module, only: cursor_t
    use utf8_module, only: utf8_char_to_byte_index, utf8_char_count
    use comment_syntax_module, only: comment_syntax_t, get_comment_syntax, has_comment_syntax
    implicit none
    private

    public :: toggle_comment_lines, comment_syntax_available

    ! Per-line record of what the toggle did, used to drag every cursor and
    ! selection anchor along with the text it was sitting on.
    type :: line_edit_t
        integer :: col = 0     ! character column the edit happened at
        integer :: delta = 0   ! characters added (>0) or removed (<0)
    end type line_edit_t

contains

    ! True when this file's language has a comment form we can toggle.
    function comment_syntax_available(filename) result(res)
        character(len=*), intent(in) :: filename
        logical :: res
        res = has_comment_syntax(get_comment_syntax(filename))
    end function comment_syntax_available

    ! Toggle comments across the union of all cursors' line ranges.
    ! changed reports whether the buffer was actually modified.
    subroutine toggle_comment_lines(buffer, cursors, filename, changed)
        type(buffer_t), intent(inout) :: buffer
        type(cursor_t), intent(inout) :: cursors(:)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: changed

        type(comment_syntax_t) :: syn
        type(line_edit_t), allocatable :: edits(:)
        integer, allocatable :: range_lo(:), range_hi(:)
        integer :: n_ranges, total_lines, lo, hi, i
        logical :: uncomment

        changed = .false.
        if (size(cursors) == 0) return

        syn = get_comment_syntax(filename)
        if (.not. has_comment_syntax(syn)) return

        total_lines = buffer_get_line_count(buffer)
        call collect_ranges(cursors, total_lines, range_lo, range_hi, n_ranges)
        if (n_ranges == 0) return

        lo = range_lo(1)
        hi = range_hi(n_ranges)
        allocate(edits(lo:hi))

        if (len_trim(syn%line) > 0) then
            ! One global decision across every range: uncomment only when
            ! there is nothing left to comment anywhere.
            uncomment = .true.
            do i = 1, n_ranges
                if (.not. range_fully_commented(buffer, range_lo(i), range_hi(i), &
                                                trim(syn%line))) then
                    uncomment = .false.
                    exit
                end if
            end do

            ! Back to front so each range's line numbers are still valid when
            ! it is reached.
            do i = n_ranges, 1, -1
                if (uncomment) then
                    call uncomment_range(buffer, range_lo(i), range_hi(i), &
                                         trim(syn%line), edits, lo, hi, changed)
                else
                    call comment_range(buffer, range_lo(i), range_hi(i), &
                                       trim(syn%line), edits, lo, hi, changed)
                end if
            end do
        else
            ! Block-only language: wrap or unwrap each range.
            do i = n_ranges, 1, -1
                call toggle_block_range(buffer, range_lo(i), range_hi(i), &
                                        trim(syn%block_start), trim(syn%block_end), &
                                        edits, lo, hi, changed)
            end do
        end if

        if (changed) then
            do i = 1, size(cursors)
                call apply_edits_to_cursor(cursors(i), edits, lo, hi)
            end do
        end if

        deallocate(edits)
    end subroutine toggle_comment_lines

    ! ------------------------------------------------------------------
    ! Range collection
    ! ------------------------------------------------------------------

    ! Build the sorted, merged set of line ranges the cursors cover.
    subroutine collect_ranges(cursors, total_lines, lo, hi, n)
        type(cursor_t), intent(in) :: cursors(:)
        integer, intent(in) :: total_lines
        integer, allocatable, intent(out) :: lo(:), hi(:)
        integer, intent(out) :: n
        integer, allocatable :: raw_lo(:), raw_hi(:)
        integer :: i, j, a, b, tmp

        allocate(raw_lo(size(cursors)), raw_hi(size(cursors)))
        n = 0

        do i = 1, size(cursors)
            call cursor_line_range(cursors(i), a, b)
            a = max(1, min(a, total_lines))
            b = max(1, min(b, total_lines))
            if (b < a) cycle
            n = n + 1
            raw_lo(n) = a
            raw_hi(n) = b
        end do

        if (n == 0) then
            allocate(lo(0), hi(0))
            deallocate(raw_lo, raw_hi)
            return
        end if

        ! Insertion sort by start line; cursor counts are small.
        do i = 2, n
            do j = i, 2, -1
                if (raw_lo(j) < raw_lo(j-1)) then
                    tmp = raw_lo(j); raw_lo(j) = raw_lo(j-1); raw_lo(j-1) = tmp
                    tmp = raw_hi(j); raw_hi(j) = raw_hi(j-1); raw_hi(j-1) = tmp
                else
                    exit
                end if
            end do
        end do

        allocate(lo(n), hi(n))
        lo(1) = raw_lo(1)
        hi(1) = raw_hi(1)
        j = 1
        do i = 2, n
            ! Merge overlapping *and* adjacent ranges: cursors on consecutive
            ! lines are one block, and one block gets one indent baseline.
            if (raw_lo(i) <= hi(j) + 1) then
                hi(j) = max(hi(j), raw_hi(i))
            else
                j = j + 1
                lo(j) = raw_lo(i)
                hi(j) = raw_hi(i)
            end if
        end do
        n = j

        deallocate(raw_lo, raw_hi)
    end subroutine collect_ranges

    ! Line span of one cursor. A selection ending at column 1 stops on the
    ! previous line -- otherwise shift-down N times would comment N+1 lines.
    subroutine cursor_line_range(cursor, first, last)
        type(cursor_t), intent(in) :: cursor
        integer, intent(out) :: first, last
        integer :: end_col

        if (.not. cursor%has_selection) then
            first = cursor%line
            last = cursor%line
            return
        end if

        if (cursor%selection_start_line < cursor%line .or. &
            (cursor%selection_start_line == cursor%line .and. &
             cursor%selection_start_col <= cursor%column)) then
            first = cursor%selection_start_line
            last = cursor%line
            end_col = cursor%column
        else
            first = cursor%line
            last = cursor%selection_start_line
            end_col = cursor%selection_start_col
        end if

        if (last > first .and. end_col <= 1) last = last - 1
    end subroutine cursor_line_range

    ! ------------------------------------------------------------------
    ! Line comments
    ! ------------------------------------------------------------------

    ! True when every non-blank line already carries the token. An all-blank
    ! range is not "fully commented" -- toggling it should add tokens.
    function range_fully_commented(buffer, first, last, token) result(res)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: token
        logical :: res
        character(len=:), allocatable :: line
        integer :: i, indent
        logical :: saw_content

        res = .false.
        saw_content = .false.

        do i = first, last
            line = buffer_get_line(buffer, i)
            indent = leading_ws_bytes(line)
            if (indent >= len(line)) cycle   ! blank / whitespace-only
            saw_content = .true.
            if (len(line) - indent < len(token)) return
            if (line(indent+1:indent+len(token)) /= token) return
        end do

        res = saw_content
    end function range_fully_commented

    ! Insert token + a space at the shallowest indent in the range.
    subroutine comment_range(buffer, first, last, token, edits, lo, hi, changed)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: first, last, lo, hi
        character(len=*), intent(in) :: token
        type(line_edit_t), intent(inout) :: edits(lo:hi)
        logical, intent(inout) :: changed
        character(len=:), allocatable :: line, insert_text
        integer :: i, indent_col, min_col, pos
        logical :: any_content

        ! Shallowest indent across the non-blank lines, in character columns.
        min_col = huge(1)
        any_content = .false.
        do i = first, last
            line = buffer_get_line(buffer, i)
            if (leading_ws_bytes(line) >= len(line)) cycle
            any_content = .true.
            indent_col = leading_ws_bytes(line) + 1   ! indentation is ASCII
            if (indent_col < min_col) min_col = indent_col
        end do

        ! An entirely blank range still gets commented, at column 1.
        if (.not. any_content) min_col = 1

        insert_text = token // ' '

        do i = first, last
            line = buffer_get_line(buffer, i)
            ! Skip blank lines when there is real content elsewhere in the
            ! range; commenting whitespace only leaves trailing junk behind.
            if (any_content .and. leading_ws_bytes(line) >= len(line)) cycle
            pos = line_char_pos(buffer, i, min_col)
            call buffer_insert(buffer, pos, insert_text)
            edits(i)%col = min_col
            edits(i)%delta = len(insert_text)
            changed = .true.
        end do
    end subroutine comment_range

    ! Strip the token (and one following space) from each commented line.
    subroutine uncomment_range(buffer, first, last, token, edits, lo, hi, changed)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: first, last, lo, hi
        character(len=*), intent(in) :: token
        type(line_edit_t), intent(inout) :: edits(lo:hi)
        logical, intent(inout) :: changed
        character(len=:), allocatable :: line
        integer :: i, indent, remove_bytes, col, pos

        do i = first, last
            line = buffer_get_line(buffer, i)
            indent = leading_ws_bytes(line)
            if (indent >= len(line)) cycle
            if (len(line) - indent < len(token)) cycle
            if (line(indent+1:indent+len(token)) /= token) cycle

            remove_bytes = len(token)
            ! Eat the single separating space we (or the author) inserted, but
            ! leave deeper indentation inside the comment intact.
            if (len(line) >= indent + remove_bytes + 1) then
                if (line(indent+remove_bytes+1:indent+remove_bytes+1) == ' ') then
                    remove_bytes = remove_bytes + 1
                end if
            end if

            ! Leading whitespace is ASCII, so bytes and columns agree here.
            col = indent + 1
            pos = line_char_pos(buffer, i, col)
            call buffer_delete(buffer, pos, remove_bytes)
            edits(i)%col = col
            edits(i)%delta = -remove_bytes
            changed = .true.
        end do
    end subroutine uncomment_range

    ! ------------------------------------------------------------------
    ! Block comments (languages with no line-comment form)
    ! ------------------------------------------------------------------

    subroutine toggle_block_range(buffer, first, last, open_tok, close_tok, &
                                  edits, lo, hi, changed)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: first, last, lo, hi
        character(len=*), intent(in) :: open_tok, close_tok
        type(line_edit_t), intent(inout) :: edits(lo:hi)
        logical, intent(inout) :: changed
        character(len=:), allocatable :: first_line, last_line
        integer :: indent, tail, pos, open_cols, close_col

        first_line = buffer_get_line(buffer, first)
        last_line = buffer_get_line(buffer, last)
        indent = leading_ws_bytes(first_line)
        tail = trailing_ws_start(last_line)

        if (len(first_line) - indent >= len(open_tok) .and. tail - 1 >= len(close_tok)) then
            if (first_line(indent+1:indent+len(open_tok)) == open_tok .and. &
                last_line(tail-len(close_tok):tail-1) == close_tok) then
                ! Already wrapped: unwrap, closer first so the opener's
                ! position is still valid on a single-line range.
                close_col = utf8_char_count(last_line(1:tail-len(close_tok)-1)) + 1
                if (close_col > 1) then
                    ! Also take the space we inserted before the closer.
                    if (last_line(tail-len(close_tok)-1:tail-len(close_tok)-1) == ' ') then
                        close_col = close_col - 1
                        call remove_at(buffer, last, close_col, len(close_tok) + 1, &
                                       edits, lo, hi, changed)
                    else
                        call remove_at(buffer, last, close_col, len(close_tok), &
                                       edits, lo, hi, changed)
                    end if
                else
                    call remove_at(buffer, last, close_col, len(close_tok), &
                                   edits, lo, hi, changed)
                end if

                open_cols = len(open_tok)
                if (len(first_line) > indent + len(open_tok)) then
                    if (first_line(indent+len(open_tok)+1:indent+len(open_tok)+1) == ' ') then
                        open_cols = open_cols + 1
                    end if
                end if
                call remove_at(buffer, first, indent + 1, open_cols, edits, lo, hi, changed)
                return
            end if
        end if

        ! Wrap. Closer first, again so `first`'s columns stay valid when the
        ! range is a single line.
        pos = line_char_pos(buffer, last, utf8_char_count(last_line) + 1)
        call buffer_insert(buffer, pos, ' ' // close_tok)
        if (last /= first) then
            edits(last)%col = utf8_char_count(last_line) + 1
            edits(last)%delta = len(close_tok) + 1
        end if

        pos = line_char_pos(buffer, first, indent + 1)
        call buffer_insert(buffer, pos, open_tok // ' ')
        edits(first)%col = indent + 1
        edits(first)%delta = edits(first)%delta + len(open_tok) + 1
        changed = .true.
    end subroutine toggle_block_range

    subroutine remove_at(buffer, line_num, col, ncols, edits, lo, hi, changed)
        type(buffer_t), intent(inout) :: buffer
        integer, intent(in) :: line_num, col, ncols, lo, hi
        type(line_edit_t), intent(inout) :: edits(lo:hi)
        logical, intent(inout) :: changed
        integer :: pos

        pos = line_char_pos(buffer, line_num, col)
        call buffer_delete(buffer, pos, ncols)
        edits(line_num)%col = col
        edits(line_num)%delta = edits(line_num)%delta - ncols
        changed = .true.
    end subroutine remove_at

    ! ------------------------------------------------------------------
    ! Cursor fixup
    ! ------------------------------------------------------------------

    subroutine apply_edits_to_cursor(cursor, edits, lo, hi)
        type(cursor_t), intent(inout) :: cursor
        integer, intent(in) :: lo, hi
        type(line_edit_t), intent(in) :: edits(lo:hi)

        call shift_col(cursor%line, cursor%column, edits, lo, hi)
        cursor%desired_column = cursor%column
        if (cursor%has_selection) then
            call shift_col(cursor%selection_start_line, cursor%selection_start_col, &
                           edits, lo, hi)
        end if
    end subroutine apply_edits_to_cursor

    subroutine shift_col(line_num, col, edits, lo, hi)
        integer, intent(in) :: line_num, lo, hi
        integer, intent(inout) :: col
        type(line_edit_t), intent(in) :: edits(lo:hi)

        if (line_num < lo .or. line_num > hi) return
        if (edits(line_num)%delta == 0) return

        if (edits(line_num)%delta > 0) then
            ! Text at or after the insertion point moves right with it.
            if (col >= edits(line_num)%col) col = col + edits(line_num)%delta
        else
            ! A cursor inside the removed token lands where the token was.
            if (col > edits(line_num)%col) then
                col = max(edits(line_num)%col, col + edits(line_num)%delta)
            end if
        end if
    end subroutine shift_col

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    ! Byte count of the leading run of spaces/tabs. Equals len(line) for a
    ! blank or whitespace-only line.
    pure function leading_ws_bytes(line) result(n)
        character(len=*), intent(in) :: line
        integer :: n

        n = 0
        do while (n < len(line))
            if (line(n+1:n+1) /= ' ' .and. line(n+1:n+1) /= char(9)) exit
            n = n + 1
        end do
    end function leading_ws_bytes

    ! 1-based byte index just past the last non-whitespace character.
    pure function trailing_ws_start(line) result(n)
        character(len=*), intent(in) :: line
        integer :: n

        n = len(line) + 1
        do while (n > 1)
            if (line(n-1:n-1) /= ' ' .and. line(n-1:n-1) /= char(9)) exit
            n = n - 1
        end do
    end function trailing_ws_start

    ! Absolute buffer byte position of a character column on a line.
    function line_char_pos(buffer, line_num, char_col) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, char_col
        integer :: pos
        character(len=:), allocatable :: line
        integer :: byte_in_line

        pos = line_start_pos(buffer, line_num)
        line = buffer_get_line(buffer, line_num)
        byte_in_line = utf8_char_to_byte_index(line, char_col)
        if (byte_in_line <= 0) byte_in_line = len(line) + 1
        pos = pos + byte_in_line - 1
    end function line_char_pos

    ! Absolute buffer byte position of the first byte of a line.
    function line_start_pos(buffer, line_num) result(pos)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num
        integer :: pos
        integer :: current_line, i, content_size

        pos = 1
        if (line_num <= 1) return

        current_line = 1
        content_size = buffer%size - (buffer%gap_end - buffer%gap_start)
        do i = 1, content_size
            if (buffer_get_char(buffer, i) == char(10)) then
                current_line = current_line + 1
                if (current_line == line_num) then
                    pos = i + 1
                    return
                end if
            end if
        end do

        pos = content_size + 1
    end function line_start_pos

end module comment_command_module
