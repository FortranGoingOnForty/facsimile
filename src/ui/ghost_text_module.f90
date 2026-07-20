module ghost_text_module
    use text_buffer_module, only: buffer_t, buffer_get_line, buffer_get_line_count
    use utf8_module, only: utf8_char_to_byte_index
    use json_module, only: json_value_t, json_has_key, json_get_array, json_get_string, &
                           json_array_size, json_get_array_element, JSON_ARRAY, JSON_OBJECT
    implicit none
    private

    public :: ghost_text_t
    public :: GHOST_SRC_NONE, GHOST_SRC_WORDS, GHOST_SRC_LSP
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
    end type ghost_text_t

contains

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
    end subroutine ghost_clear

    subroutine ghost_clear_pending(ghost)
        type(ghost_text_t), intent(inout) :: ghost

        ghost%pending_request_id = 0
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
