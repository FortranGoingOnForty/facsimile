module ghost_text_module
    use iso_fortran_env, only: int64
    use text_buffer_module, only: buffer_t, buffer_get_line, buffer_get_line_count
    use utf8_module, only: utf8_char_to_byte_index
    use json_module, only: json_value_t, json_has_key, json_get_array, json_get_string, &
                           json_array_size, json_get_array_element, JSON_ARRAY, JSON_OBJECT
    implicit none
    private

    public :: ghost_text_t
    public :: GHOST_SRC_NONE, GHOST_SRC_WORDS, GHOST_SRC_LSP, GHOST_SRC_LLM
    public :: ghost_extend_prefix, ghost_may_replace, ghost_apply_text
    public :: ghost_apply_block, ghost_insert_text, ghost_is_block
    public :: ghost_block_line, ghost_take_word
    public :: ghost_take_line, ghost_set_anchor
    public :: ghost_clear, ghost_clear_pending
    public :: ghost_get_prefix_at_cursor
    public :: ghost_get_include_prefix
    public :: ghost_update_from_buffer
    public :: ghost_apply_lsp_result
    public :: ghost_suffix
    public :: ghost_is_active
    public :: ghost_word_char

    integer, parameter :: GHOST_SRC_NONE = 0
    integer, parameter :: GHOST_SRC_WORDS = 1
    integer, parameter :: GHOST_SRC_LSP = 2
    integer, parameter :: GHOST_SRC_LLM = 3

    ! Inline "shadow text" suggestion state. One global instance lives on
    ! editor_state_t. The suggestion is always a full identifier that starts
    ! with the prefix the user has typed; only the remaining suffix is drawn
    ! (dim) at the cursor and inserted on accept.
    type :: ghost_text_t
        logical :: enabled = .true.
        logical :: visible = .false.
        integer :: source = GHOST_SRC_NONE
        character(len=:), allocatable :: suggestion   ! full word (prefix + suffix)
        character(len=:), allocatable :: prefix       ! typed prefix that generated it
        integer :: anchor_line = 0                    ! 1-based buffer line
        integer :: anchor_col = 0                     ! 1-based UTF-8 char column
        integer :: pending_request_id = 0             ! outstanding LSP request (0 = none)
        character(len=:), allocatable :: pending_prefix
        logical :: pending_include = .false.          ! request made in #include context
        ! Document revision when the request went out. Cursor position and
        ! typed prefix can both be restored by an undo while the rest of the
        ! document changes, so they are not enough on their own to tell that
        ! a reply still applies.
        integer(int64) :: pending_doc_revision = 0

        ! Multi-line suggestion. Added alongside the single-line fields rather
        ! than replacing them, so the word-scan and LSP sources keep exactly
        ! their existing behaviour. block_lines == 0 means "not a block", which
        ! is what every pre-existing check already assumes.
        character(len=:), allocatable :: block_text
        integer :: block_lines = 0
    end type ghost_text_t

contains

    ! The user typed the character the suggestion already predicted. Advance
    ! in place rather than clearing and re-querying: prefix grows, anchor_col
    ! moves right, and suggestion is untouched -- ghost_suffix is defined as
    ! suggestion(len(prefix)+1:), so the visible remainder shrinks for free.
    !
    ! This is what makes the feature feel instant. A round trip is ~300ms; by
    ! not making one at all while the user types into a correct suggestion,
    ! most keystrokes cost nothing.
    function ghost_extend_prefix(ghost, ch) result(extended)
        type(ghost_text_t), intent(inout) :: ghost
        character(len=*), intent(in) :: ch
        logical :: extended
        character(len=:), allocatable :: suffix

        extended = .false.
        if (.not. ghost_is_active(ghost)) return
        if (len(ch) /= 1) return

        suffix = ghost_suffix(ghost)
        if (len(suffix) == 0) return
        if (suffix(1:1) /= ch) return

        ghost%prefix = ghost%prefix // ch
        ghost%anchor_col = ghost%anchor_col + 1
        extended = ghost_is_active(ghost)
        if (.not. extended) call ghost_clear(ghost)
    end function ghost_extend_prefix

    ! Install a multi-line suggestion. Only ever offered with the caret at end
    ! of line: that keeps the mid-line "open the line up" trick and the block
    ! row loop from ever meeting, and a block mid-line is incoherent anyway --
    ! there is no sensible place for the rest of the line to go.
    subroutine ghost_apply_block(ghost, text, prefix, cur_line, cur_col, source)
        type(ghost_text_t), intent(inout) :: ghost
        character(len=*), intent(in) :: text, prefix
        integer, intent(in) :: cur_line, cur_col, source
        integer :: i, n

        if (len(text) == 0) return

        n = 1
        do i = 1, len(text)
            if (text(i:i) == achar(10)) n = n + 1
        end do

        ! The first line still lives in suggestion/prefix so the renderer's
        ! row-1 path and ghost_suffix keep working unchanged.
        ghost%suggestion = prefix // first_line_of(text)
        ghost%prefix = prefix
        ghost%anchor_line = cur_line
        ghost%anchor_col = cur_col
        ghost%source = source
        ghost%visible = .true.
        ghost%block_text = text
        ghost%block_lines = n
    end subroutine ghost_apply_block

    function ghost_is_block(ghost) result(res)
        type(ghost_text_t), intent(in) :: ghost
        logical :: res
        res = ghost%block_lines > 1 .and. allocated(ghost%block_text)
    end function ghost_is_block

    ! What gets inserted on accept: the whole block when there is one, the
    ! single-line suffix otherwise. Every insert path goes through this.
    function ghost_insert_text(ghost) result(text)
        type(ghost_text_t), intent(in) :: ghost
        character(len=:), allocatable :: text

        if (ghost_is_block(ghost)) then
            text = ghost%block_text
        else
            text = ghost_suffix(ghost)
        end if
    end function ghost_insert_text

    ! 1-based line of the block, for the renderer's row loop.
    function ghost_block_line(ghost, n) result(text)
        type(ghost_text_t), intent(in) :: ghost
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        integer :: i, seen, start

        text = ''
        if (.not. ghost_is_block(ghost)) return
        if (n < 1 .or. n > ghost%block_lines) return

        seen = 1
        start = 1
        do i = 1, len(ghost%block_text)
            if (ghost%block_text(i:i) == achar(10)) then
                if (seen == n) then
                    text = ghost%block_text(start:i-1)
                    return
                end if
                seen = seen + 1
                start = i + 1
            end if
        end do
        if (seen == n) text = ghost%block_text(start:)
    end function ghost_block_line

    pure function first_line_of(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i

        out = s
        do i = 1, len(s)
            if (s(i:i) == achar(10)) then
                out = s(1:i-1)
                return
            end if
        end do
    end function first_line_of

    ! Accept one word of the suggestion and keep the rest, re-anchored. Useful
    ! exactly when the model is mostly right: it turns a suggestion that would
    ! otherwise be rejected wholesale into a partial win.
    !
    ! Returns the text to insert; the caller advances the buffer and cursor,
    ! then the ghost's prefix/anchor have already been moved to match.
    function ghost_take_word(ghost) result(word)
        type(ghost_text_t), intent(inout) :: ghost
        character(len=:), allocatable :: word
        character(len=:), allocatable :: suffix
        integer :: i, n

        word = ''
        if (.not. ghost_is_active(ghost)) return
        if (ghost_is_block(ghost)) return       ! blocks accept by line, not word

        suffix = ghost_suffix(ghost)
        if (len(suffix) == 0) return

        ! Leading non-word run (punctuation, spaces) counts as one step, then
        ! a word run. That way '(x' hands over '(' first rather than jumping
        ! past the paren the user may not want.
        n = 0
        if (.not. ghost_word_char(suffix(1:1))) then
            do i = 1, len(suffix)
                if (ghost_word_char(suffix(i:i))) exit
                n = i
            end do
        else
            do i = 1, len(suffix)
                if (.not. ghost_word_char(suffix(i:i))) exit
                n = i
            end do
        end if
        if (n == 0) return

        word = suffix(1:n)
        ghost%prefix = ghost%prefix // word
        ghost%anchor_col = ghost%anchor_col + n
        if (.not. ghost_is_active(ghost)) call ghost_clear(ghost)
    end function ghost_take_word

    ! Accept one line of a block, keeping the remaining lines offered.
    !
    ! Returns the text to insert -- the first line plus its newline -- and
    ! leaves the ghost holding lines 2..N. The caller inserts, then calls
    ! ghost_set_anchor with wherever the caret ended up: only the caller knows
    ! that, because auto-indent and the buffer decide it.
    !
    ! When one line remains the ghost stops being a block, so the renderer's
    ! row loop and the block accept path both fall back to the simpler
    ! single-line handling automatically.
    subroutine ghost_take_line(ghost, text, ok)
        type(ghost_text_t), intent(inout) :: ghost
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: ok
        character(len=:), allocatable :: rest
        integer :: nl

        text = ''
        ok = .false.
        if (.not. ghost_is_block(ghost)) return

        nl = index(ghost%block_text, achar(10))
        if (nl <= 0) return

        text = ghost%block_text(1:nl)          ! includes the newline
        rest = ghost%block_text(nl+1:)
        if (len(rest) == 0) then
            call ghost_clear(ghost)
            ok = .true.
            return
        end if

        ! The caret lands on a fresh line, so nothing is typed there yet
        ghost%prefix = ''
        ghost%block_lines = ghost%block_lines - 1

        if (ghost%block_lines <= 1) then
            ! One line left: an ordinary single-line suggestion
            ghost%block_lines = 0
            if (allocated(ghost%block_text)) deallocate(ghost%block_text)
            ghost%suggestion = rest
        else
            ghost%block_text = rest
            ghost%suggestion = first_line_of(rest)
        end if

        ghost%visible = .true.
        ok = .true.
    end subroutine ghost_take_line

    ! Move the anchor after the caller has inserted something. Used by the
    ! partial-accept paths, which cannot know the new caret position until the
    ! buffer has been changed.
    subroutine ghost_set_anchor(ghost, line_num, col)
        type(ghost_text_t), intent(inout) :: ghost
        integer, intent(in) :: line_num, col

        ghost%anchor_line = line_num
        ghost%anchor_col = col
    end subroutine ghost_set_anchor

    ! Whether a newly arrived suggestion may take over the display.
    !
    ! Three sources now produce ghosts at very different latencies: the buffer
    ! word scan is instant, LSP is ~50ms, a local model ~300ms. Last-writer-wins
    ! would flicker, and worse, would change what Tab does between the user
    ! deciding to press it and pressing it.
    !
    ! Rank alone is not enough. A model completion that is a bare identifier is
    ! a *guess at a name*, and LSP's names are type-correct and cannot be
    ! hallucinated -- so the model only outranks LSP when it is predicting
    ! actual code (punctuation or spaces present). That single rule removes the
    ! most irritating flicker: one identifier replaced by a different one.
    function ghost_may_replace(current_source, new_source, new_text) result(ok)
        integer, intent(in) :: current_source, new_source
        character(len=*), intent(in) :: new_text
        logical :: ok

        ok = .false.
        if (len(new_text) == 0) return

        if (current_source == GHOST_SRC_NONE) then
            ok = .true.
            return
        end if

        if (new_source == GHOST_SRC_LLM .and. current_source == GHOST_SRC_LSP) then
            ok = .not. is_bare_identifier(new_text)
            return
        end if

        ok = new_source > current_source
    end function ghost_may_replace

    pure function is_bare_identifier(s) result(res)
        character(len=*), intent(in) :: s
        logical :: res
        integer :: i

        res = .true.
        do i = 1, len(s)
            if (.not. ghost_word_char(s(i:i))) then
                res = .false.
                return
            end if
        end do
    end function is_bare_identifier

    ! Install a suggestion that did not come from the word scan or LSP. text is
    ! the insertion -- what the user gets on Tab -- so suggestion is prefix
    ! plus text, keeping the prefix-anchored contract the renderer and the
    ! accept path both rely on.
    subroutine ghost_apply_text(ghost, text, prefix, cur_line, cur_col, source)
        type(ghost_text_t), intent(inout) :: ghost
        character(len=*), intent(in) :: text, prefix
        integer, intent(in) :: cur_line, cur_col, source

        if (len(text) == 0) return
        ghost%suggestion = prefix // text
        ghost%prefix = prefix
        ghost%anchor_line = cur_line
        ghost%anchor_col = cur_col
        ghost%source = source
        ghost%visible = .true.
    end subroutine ghost_apply_text

    ! Hide the suggestion. Does NOT cancel a pending LSP request: a response
    ! that is already in flight is revalidated against the cursor on arrival.
    subroutine ghost_clear(ghost)
        type(ghost_text_t), intent(inout) :: ghost

        ghost%visible = .false.
        ghost%source = GHOST_SRC_NONE
        ghost%anchor_line = 0
        ghost%anchor_col = 0
        if (allocated(ghost%suggestion)) deallocate(ghost%suggestion)
        if (allocated(ghost%prefix)) deallocate(ghost%prefix)
        if (allocated(ghost%block_text)) deallocate(ghost%block_text)
        ghost%block_lines = 0
    end subroutine ghost_clear

    subroutine ghost_clear_pending(ghost)
        type(ghost_text_t), intent(inout) :: ghost

        ghost%pending_request_id = 0
        ghost%pending_doc_revision = 0
        ghost%pending_include = .false.
        if (allocated(ghost%pending_prefix)) deallocate(ghost%pending_prefix)
    end subroutine ghost_clear_pending

    ! Extract the partial word immediately left of the cursor.
    ! col is the cursor's 1-based UTF-8 char column; returns '' when the
    ! char before the cursor is not a word char (or cursor is at col 1).
    subroutine ghost_get_prefix_at_cursor(buffer, line_num, col, prefix)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        character(len=:), allocatable, intent(out) :: prefix
        character(len=:), allocatable :: line
        integer :: byte_end, i

        prefix = ''
        if (col <= 1) return
        line = buffer_get_line(buffer, line_num)
        if (len(line) == 0) return

        ! Byte position the cursor sits at (len+1 when at end of line)
        byte_end = utf8_char_to_byte_index(line, col)
        if (byte_end < 2) return

        ! Word chars are ASCII, so a backward byte walk is UTF-8 safe:
        ! continuation bytes never match [A-Za-z0-9_]
        i = byte_end - 1
        do while (i >= 1)
            if (.not. ghost_word_char(line(i:i))) exit
            i = i - 1
        end do
        if (i < byte_end - 1) prefix = line(i+1:byte_end-1)
    end subroutine ghost_get_prefix_at_cursor

    ! Detect an include-directive context: '#include <partial' or
    ! '#include "partial' with the cursor inside the unclosed header name.
    ! The prefix is the current path segment - the text after the last of
    ! '<', '"' or '/' - because that is how clangd anchors header
    ! completions (insertText for '<sys/epo' is 'epoll.h>', not the full
    ! path). prefix may come back empty with in_include true, e.g. right
    ! after typing the '<' or a '/'.
    subroutine ghost_get_include_prefix(buffer, line_num, col, prefix, in_include)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: in_include
        character(len=:), allocatable :: line
        integer :: cursor_byte, i, n, word_start, seg_start
        character :: closer

        prefix = ''
        in_include = .false.
        line = buffer_get_line(buffer, line_num)
        n = len(line)
        if (n == 0) return
        cursor_byte = utf8_char_to_byte_index(line, col)

        ! '#', optional blanks, then an include-like directive word
        i = 1
        do while (i <= n)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) exit
            i = i + 1
        end do
        if (i > n) return
        if (line(i:i) /= '#') return
        i = i + 1
        do while (i <= n)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) exit
            i = i + 1
        end do
        word_start = i
        do while (i <= n)
            if (.not. ghost_word_char(line(i:i))) exit
            i = i + 1
        end do
        if (i <= word_start) return
        select case (line(word_start:i-1))
        case ('include', 'include_next', 'import')
        case default
            return
        end select

        ! Opening delimiter before the cursor, not yet closed
        do while (i <= n)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) exit
            i = i + 1
        end do
        if (i > n .or. i >= cursor_byte) return
        if (line(i:i) == '<') then
            closer = '>'
        else if (line(i:i) == '"') then
            closer = '"'
        else
            return
        end if

        seg_start = i + 1
        do i = seg_start, cursor_byte - 1
            if (line(i:i) == closer) return
            if (line(i:i) == '/') then
                seg_start = i + 1
            else if (.not. header_char(line(i:i))) then
                return
            end if
        end do

        in_include = .true.
        if (seg_start <= cursor_byte - 1) prefix = line(seg_start:cursor_byte-1)
    end subroutine ghost_get_include_prefix

    ! Scan the whole buffer for [A-Za-z0-9_]+ tokens that extend the prefix
    ! and keep the one nearest the cursor line (tie: earliest occurrence).
    ! Case-sensitive matches win over case-insensitive ones. The token being
    ! typed (ending right before the cursor) is excluded.
    subroutine ghost_update_from_buffer(ghost, buffer, prefix, cur_line, cur_col)
        type(ghost_text_t), intent(inout) :: ghost
        type(buffer_t), intent(in) :: buffer
        character(len=*), intent(in) :: prefix
        integer, intent(in) :: cur_line, cur_col
        character(len=:), allocatable :: line, token, best_cs, best_ci, lower_prefix
        integer :: line_count, l, i, tok_start, tok_end, plen, dist
        integer :: best_cs_dist, best_ci_dist, typed_end_byte

        call ghost_clear(ghost)
        plen = len(prefix)
        if (plen == 0) return

        lower_prefix = to_lower(prefix)
        best_cs_dist = huge(1)
        best_ci_dist = huge(1)

        ! Byte position of the last char of the token being typed
        line = buffer_get_line(buffer, cur_line)
        typed_end_byte = utf8_char_to_byte_index(line, cur_col) - 1

        line_count = buffer_get_line_count(buffer)
        do l = 1, line_count
            line = buffer_get_line(buffer, l)
            i = 1
            do while (i <= len(line))
                if (ghost_word_char(line(i:i))) then
                    tok_start = i
                    do while (i <= len(line))
                        if (.not. ghost_word_char(line(i:i))) exit
                        i = i + 1
                    end do
                    tok_end = i - 1
                    if (.not. (l == cur_line .and. tok_end == typed_end_byte) .and. &
                        tok_end - tok_start + 1 > plen) then
                        token = line(tok_start:tok_end)
                        dist = abs(l - cur_line)
                        if (token(1:plen) == prefix) then
                            if (dist < best_cs_dist) then
                                best_cs = token
                                best_cs_dist = dist
                            end if
                        else if (to_lower(token(1:plen)) == lower_prefix) then
                            if (dist < best_ci_dist) then
                                best_ci = token
                                best_ci_dist = dist
                            end if
                        end if
                    end if
                else
                    i = i + 1
                end if
            end do
        end do

        if (best_cs_dist /= huge(1)) then
            ghost%suggestion = best_cs
        else if (best_ci_dist /= huge(1)) then
            ! Keep what the user typed; append the candidate's suffix as-is
            ghost%suggestion = prefix // best_ci(plen+1:)
        else
            return
        end if
        ghost%prefix = prefix
        ghost%anchor_line = cur_line
        ghost%anchor_col = cur_col
        ghost%visible = .true.
        ghost%source = GHOST_SRC_WORDS
    end subroutine ghost_update_from_buffer

    ! Replace the current suggestion with the first LSP completion item that
    ! extends the prefix and passes the shape filter: a plain identifier
    ! normally, or a header path (float.h>, sys/stat.h>) in include context.
    ! Rejects snippets with placeholders like ${1:...} and multibyte text.
    ! Accepts both the CompletionList {items:[...]} shape and a bare item
    ! array. If no item survives, any word-scan suggestion is left in place.
    ! In include context an empty prefix is legal (cursor right after '<'
    ! or '/'), so the first header item shows whole.
    subroutine ghost_apply_lsp_result(ghost, result_json, prefix, cur_line, cur_col, header_ctx)
        type(ghost_text_t), intent(inout) :: ghost
        type(json_value_t), intent(in) :: result_json
        character(len=*), intent(in) :: prefix
        integer, intent(in) :: cur_line, cur_col
        logical, intent(in) :: header_ctx
        type(json_value_t) :: items, item
        character(len=:), allocatable :: text
        logical :: shape_ok
        integer :: i, n, plen

        plen = len(prefix)
        if (plen == 0 .and. .not. header_ctx) return

        if (result_json%value_type == JSON_OBJECT) then
            if (.not. json_has_key(result_json, "items")) return
            items = json_get_array(result_json, "items")
        else if (result_json%value_type == JSON_ARRAY) then
            items = result_json
        else
            return
        end if

        n = json_array_size(items)
        do i = 0, n - 1
            item = json_get_array_element(items, i)
            if (json_has_key(item, "insertText")) then
                text = json_get_string(item, "insertText")
            else
                text = json_get_string(item, "label")
            end if
            if (len(text) > plen) then
                if (header_ctx) then
                    shape_ok = is_header_completion(text)
                else
                    shape_ok = is_identifier(text)
                end if
                if (shape_ok) then
                    if (plen == 0) then
                        shape_ok = .true.
                    else
                        shape_ok = text(1:plen) == prefix
                    end if
                end if
                if (shape_ok) then
                    ghost%suggestion = text
                    ghost%prefix = prefix
                    ghost%anchor_line = cur_line
                    ghost%anchor_col = cur_col
                    ghost%visible = .true.
                    ghost%source = GHOST_SRC_LSP
                    return
                end if
            end if
        end do
    end subroutine ghost_apply_lsp_result

    ! The part of the suggestion not yet typed (what gets drawn/inserted)
    function ghost_suffix(ghost) result(s)
        type(ghost_text_t), intent(in) :: ghost
        character(len=:), allocatable :: s

        if (ghost_is_active(ghost)) then
            s = ghost%suggestion(len(ghost%prefix)+1:)
        else
            s = ''
        end if
    end function ghost_suffix

    function ghost_is_active(ghost) result(active)
        type(ghost_text_t), intent(in) :: ghost
        logical :: active

        active = ghost%enabled .and. ghost%visible .and. &
                 allocated(ghost%suggestion) .and. allocated(ghost%prefix)
        if (active) active = len(ghost%suggestion) > len(ghost%prefix)
    end function ghost_is_active

    pure function ghost_word_char(ch) result(is_word)
        character, intent(in) :: ch
        logical :: is_word

        is_word = (ch >= 'a' .and. ch <= 'z') .or. &
                  (ch >= 'A' .and. ch <= 'Z') .or. &
                  (ch >= '0' .and. ch <= '9') .or. &
                  ch == '_'
    end function ghost_word_char

    ! Chars legal inside one segment of a header name (no '/')
    pure function header_char(ch) result(ok)
        character, intent(in) :: ch
        logical :: ok

        ok = ghost_word_char(ch) .or. ch == '.' .or. ch == '+' .or. ch == '-'
    end function header_char

    ! Header completion item: path chars, optionally closed by '>' or '"'
    pure function is_header_completion(text) result(yes)
        character(len=*), intent(in) :: text
        logical :: yes
        integer :: i, last

        last = len(text)
        yes = last > 0
        if (.not. yes) return
        if (text(last:last) == '>' .or. text(last:last) == '"') last = last - 1
        if (last < 1) then
            yes = .false.
            return
        end if
        do i = 1, last
            if (.not. (header_char(text(i:i)) .or. text(i:i) == '/')) then
                yes = .false.
                return
            end if
        end do
    end function is_header_completion

    pure function is_identifier(text) result(yes)
        character(len=*), intent(in) :: text
        logical :: yes
        integer :: i

        yes = len(text) > 0
        do i = 1, len(text)
            if (.not. ghost_word_char(text(i:i))) then
                yes = .false.
                return
            end if
        end do
    end function is_identifier

    pure function to_lower(str) result(lower)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower
        integer :: i

        lower = str
        do i = 1, len(str)
            if (lower(i:i) >= 'A' .and. lower(i:i) <= 'Z') then
                lower(i:i) = achar(iachar(lower(i:i)) + 32)
            end if
        end do
    end function to_lower

end module ghost_text_module
