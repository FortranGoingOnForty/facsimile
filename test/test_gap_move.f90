program test_gap_move
    ! Moving the gap left over a distance larger than the gap itself.
    !
    ! A gap buffer moves its gap by shifting the text between the caret and
    ! the gap across to the far side. Moving LEFT shifts that text RIGHT by
    ! exactly the gap size, so source and destination OVERLAP as soon as the
    ! gap is smaller than the distance moved -- which is the ordinary state of
    ! a buffer after a few thousand keystrokes, not an edge case.
    !
    ! The copy ran forward, so it read bytes it had already written and
    ! smeared a ragged block of earlier text over later text. In practice:
    ! type for a while near the end of a document, scroll back up, edit
    ! something there, and a chunk from the top of the file appears in the
    ! middle of a section further down, cutting across line boundaries. It
    ! survives a save, because the buffer really is that shape.
    !
    ! Nothing about it is visible until the gap has been eaten into, which is
    ! why it read as "the editor corrupts documents after a long session".
    use text_buffer_module, only: buffer_t, init_buffer, buffer_insert, &
                                  buffer_get_line, buffer_delete
    implicit none

    integer :: nfail

    nfail = 0
    call left_move_over_a_small_gap(nfail)
    call right_move_still_works(nfail)
    call a_delete_then_a_far_earlier_insert(nfail)

    if (nfail == 0) then
        print *, 'test_gap_move: all passed'
    else
        print *, 'test_gap_move: FAILED', nfail
        stop 1
    end if

contains

    !> Build a document whose every line is one repeated letter, so any byte
    !> landing where it does not belong is obvious by inspection.
    subroutine striped(buf, nlines, width, total)
        type(buffer_t), intent(out) :: buf
        integer, intent(in) :: nlines, width
        integer, intent(out) :: total
        character(len=:), allocatable :: text
        character(len=1) :: c
        integer :: i

        text = ''
        do i = 1, nlines
            c = achar(iachar('A') + mod(i, 26))
            text = text // repeat(c, width) // char(10)
        end do
        total = len(text)
        call init_buffer(buf, text)
    end subroutine striped

    integer function stripe_damage(buf, nlines) result(bad)
        type(buffer_t), intent(inout) :: buf
        integer, intent(in) :: nlines
        character(len=:), allocatable :: line
        character(len=1) :: want
        integer :: i

        bad = 0
        do i = 1, nlines
            line = buffer_get_line(buf, i)
            want = achar(iachar('A') + mod(i, 26))
            if (len_trim(line) == 0) cycle
            ! Every line is one letter repeated, so ten of the right letter in
            ! a row is enough to say the line is intact.
            if (index(trim(line), repeat(want, 10)) == 0) bad = bad + 1
        end do
    end function stripe_damage

    subroutine left_move_over_a_small_gap(nfail)
        integer, intent(inout) :: nfail
        type(buffer_t) :: buf
        integer :: total, i, gap, jump, bad

        call striped(buf, 60, 40, total)

        ! Type at the end until the gap is smaller than the document. Without
        ! this the gap is wider than any jump and the overlap never happens --
        ! which is exactly why a fresh buffer looks fine.
        do i = 1, 5000
            call buffer_insert(buf, total + i, 'z')
        end do

        gap = buf%gap_end - buf%gap_start
        jump = buf%gap_start - 50
        if (gap >= jump) then
            print *, '  left_move: gap ', gap, ' is not smaller than the jump ', jump, &
                     ' -- the test no longer exercises the overlap'
            nfail = nfail + 1
            return
        end if

        ! The ordinary act of scrolling back up and fixing a typo.
        call buffer_insert(buf, 50, 'Q')

        bad = stripe_damage(buf, 60)
        if (bad /= 0) then
            print *, '  left_move: ', bad, ' of 60 lines corrupted by moving the gap left'
            nfail = nfail + 1
        end if
    end subroutine left_move_over_a_small_gap

    subroutine right_move_still_works(nfail)
        integer, intent(inout) :: nfail
        type(buffer_t) :: buf
        integer :: total, i, bad

        call striped(buf, 60, 40, total)
        ! Park the gap early, eat into it, then edit late: the mirror image.
        ! Moving right copies leftward, which never overlaps destructively --
        ! asserted so a fix to the left move cannot quietly break it.
        do i = 1, 5000
            call buffer_insert(buf, 50 + i - 1, 'q')
        end do
        call buffer_insert(buf, total + 4000, 'Z')

        bad = stripe_damage(buf, 60)
        if (bad /= 0) then
            print *, '  right_move: ', bad, ' of 60 lines corrupted by moving the gap right'
            nfail = nfail + 1
        end if
    end subroutine right_move_still_works

    subroutine a_delete_then_a_far_earlier_insert(nfail)
        integer, intent(inout) :: nfail
        type(buffer_t) :: buf
        integer :: total, i, bad

        ! Deleting grows the gap, so a session of edits leaves it at sizes the
        ! two simpler cases do not cover. Walk it around a few times.
        !
        ! Every edit here is confined to line 3 or to the run of 'z' past the
        ! last newline, so no edit can add or remove a line: lines 10..60 must
        ! come through untouched. (An earlier version deleted at a fixed byte
        ! offset inside line 1, which after eleven passes ate its newline,
        ! merged two lines and shifted every line number by one -- the test
        ! reporting its own edits as corruption.)
        call striped(buf, 60, 40, total)
        do i = 1, 4000
            call buffer_insert(buf, total + i, 'z')
        end do
        do i = 1, 12
            call buffer_insert(buf, 100, 'c')            ! inside line 3
            call buffer_insert(buf, total + 1000, 'b')   ! inside the z run
            call buffer_delete(buf, total + 500, 1)      ! inside the z run
            call buffer_insert(buf, 105, 'd')            ! inside line 3
        end do

        bad = 0
        do i = 10, 60
            block
                character(len=:), allocatable :: line
                character(len=1) :: want
                line = buffer_get_line(buf, i)
                want = achar(iachar('A') + mod(i, 26))
                if (len_trim(line) > 0) then
                    if (trim(line) /= repeat(want, 40)) bad = bad + 1
                end if
            end block
        end do
        if (bad /= 0) then
            print *, '  walked gap: ', bad, ' of lines 10..60 corrupted'
            nfail = nfail + 1
        end if
    end subroutine a_delete_then_a_far_earlier_insert

end program test_gap_move
