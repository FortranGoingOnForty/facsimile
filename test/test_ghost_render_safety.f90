program test_ghost_render_safety
    ! Ghost text is written straight to the terminal and clipped to the pane
    ! width. Both operations were byte-based, which is only correct because
    ! today's two ghost sources filter their suggestions down to ASCII
    ! identifiers. Anything producing arbitrary UTF-8 -- a model, a paste-driven
    ! source, a future backend -- would have been clipped mid-codepoint,
    ! emitting a partial sequence to the terminal and misplacing the shifted
    ! line tail by the difference between bytes and cells.
    !
    ! These pin the two guards that make the renderer safe for arbitrary text.
    use renderer_module, only: clip_to_cells, is_terminal_safe, format_status_line
    use utf8_module, only: utf8_display_width
    implicit none

    ! 2-byte chars (1 cell each)
    character(len=*), parameter :: EACUTE = char(195) // char(169)      ! é
    ! 3-byte char, 2 cells wide (CJK)
    character(len=*), parameter :: CJK = char(228) // char(184) // char(173)  ! 中
    ! 4-byte char, 2 cells wide (emoji)
    character(len=*), parameter :: EMOJI = char(240) // char(159) // char(152) // char(128)
    character(len=*), parameter :: ELLIPSIS = char(226) // char(128) // char(166)
    character(len=*), parameter :: EMDASH = char(226) // char(128) // char(148)

    integer :: nfail
    character(len=:), allocatable :: out
    integer :: used

    nfail = 0

    ! --- ASCII behaves exactly as before ---
    call clip_to_cells('hello', 10, out, used)
    call check(out == 'hello' .and. used == 5, 'ascii under budget is untouched', out)

    call clip_to_cells('hello', 3, out, used)
    call check(out == 'hel' .and. used == 3, 'ascii is cut at the budget', out)

    call clip_to_cells('hello', 0, out, used)
    call check(len(out) == 0 .and. used == 0, 'zero budget yields nothing', out)

    ! --- The actual bug: bytes are not cells ---
    ! 'éééé' is 8 bytes but only 4 cells. Byte clipping at 3 would have cut
    ! the second é in half and emitted a lone continuation byte.
    call clip_to_cells(EACUTE // EACUTE // EACUTE // EACUTE, 3, out, used)
    call check(out == EACUTE // EACUTE // EACUTE .and. used == 3, &
               'multibyte clips on a character boundary, by cells', out)
    call check(len(out) == 6, 'three 2-byte chars really are six bytes', int_str(len(out)))

    ! Budget larger than the byte count must not over-read
    call clip_to_cells(EACUTE // EACUTE, 99, out, used)
    call check(out == EACUTE // EACUTE .and. used == 2, &
               'budget beyond the text returns all of it', out)

    ! --- Wide characters occupy two cells each ---
    call clip_to_cells(CJK // CJK, 4, out, used)
    call check(out == CJK // CJK .and. used == 4, 'two wide chars fit in four cells', out)

    call clip_to_cells(CJK // CJK, 3, out, used)
    call check(out == CJK .and. used == 2, &
               'a wide char that would straddle the limit is dropped whole', out)

    call clip_to_cells(CJK, 1, out, used)
    call check(len(out) == 0 .and. used == 0, &
               'a wide char never half-drawn into a one-cell gap', out)

    ! --- Emoji: 4 bytes, 2 cells ---
    call clip_to_cells(EMOJI // 'x', 2, out, used)
    call check(out == EMOJI .and. used == 2, 'emoji measured as two cells', out)

    call clip_to_cells(EMOJI // 'x', 3, out, used)
    call check(out == EMOJI // 'x' .and. used == 3, 'emoji plus ascii', out)

    ! --- Mixed run ---
    call clip_to_cells('a' // CJK // 'b', 3, out, used)
    call check(out == 'a' // CJK .and. used == 3, 'mixed narrow and wide', out)

    ! Wolf's multiline diagnostic has only ASCII in the visible prefix, then
    ! several multibyte punctuation characters later in the message. The old
    ! status formatter counted those later bytes toward the prefix's budget,
    ! emitted 101 cells into a 93-cell terminal, and every caret repaint then
    ! dragged document rows through the wrapped status text.
    call format_status_line(93, &
        '» | this string never closes' // char(10) // &
        'it opens here, and an interpolation `{` inside it is still open' // char(10) // &
        'when the line ends; a "' // ELLIPSIS // '" string ' // EMDASH // &
        ' and every interpolation ' // EMDASH // ' must close', out)
    call check(utf8_display_width(out) == 93, &
               'a multiline UTF-8 diagnostic occupies exactly one status row', out)
    call check(len(out) >= 3 .and. out(len(out)-2:len(out)) == '...', &
               'an overlong diagnostic ends with the clipping ellipsis', out)
    call check(index(out, char(10)) == 0, &
               'diagnostic line breaks become harmless spaces', out)

    ! --- Terminal safety: the guard that stops model text corrupting the screen ---
    call check(is_terminal_safe('normal text'), 'plain text is safe', '')
    call check(is_terminal_safe('tabs' // char(9) // 'ok'), 'tab is allowed', '')
    call check(is_terminal_safe(EACUTE // CJK // EMOJI), 'utf-8 is safe', '')

    call check(.not. is_terminal_safe(char(27) // '[2J'), &
               'an escape sequence is rejected', '')
    call check(.not. is_terminal_safe('x' // char(27) // '[31m'), &
               'a colour code mid-string is rejected', '')
    call check(.not. is_terminal_safe('a' // char(0) // 'b'), 'NUL is rejected', '')
    call check(.not. is_terminal_safe('a' // char(13) // 'b'), &
               'carriage return is rejected', '')
    call check(.not. is_terminal_safe('a' // char(7)), 'bell is rejected', '')
    call check(.not. is_terminal_safe('a' // char(14)), &
               'shift-out is rejected (it garbles the charset)', '')
    call check(.not. is_terminal_safe('a' // char(127)), 'DEL is rejected', '')
    call check(is_terminal_safe(''), 'empty text is trivially safe', '')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All ghost render-safety tests passed'

contains

    function int_str(v) result(s)
        integer, intent(in) :: v
        character(len=16) :: s
        write(s, '(i0)') v
    end function int_str

    subroutine check(ok, name, got)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name, got

        if (ok) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name // ' (got: "' // trim(got) // '")'
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ghost_render_safety
