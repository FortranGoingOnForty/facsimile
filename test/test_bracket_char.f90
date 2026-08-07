program test_bracket_char
    ! Asking "is there a bracket under the caret" when there is no character
    ! under the caret at all.
    !
    ! utf8_char_at returns a ZERO-LENGTH string for a column past the end of
    ! the line -- the caret sitting at end of line, or anywhere on an empty
    ! one, both perfectly ordinary places to be. The renderer used to hand
    ! that straight to is_opening_bracket, whose dummy is character(len=1).
    ! At -O2 that reads the byte after the allocation and usually gets away
    ! with it; any checked build aborts. Pressing End on the first line was
    ! enough to kill a `make dev` binary:
    !
    !   Fortran runtime error: Actual string length is shorter than the
    !   declared one for dummy argument 'ch' (0/1)
    !
    ! is_bracket_char takes character(len=*) and answers false for anything
    ! that is not exactly one bracket, so there is nothing left to get wrong
    ! at the call site.
    use bracket_matching_module, only: is_bracket_char
    implicit none

    integer :: nfail

    nfail = 0

    call expect('(', .true., 'open paren')
    call expect('[', .true., 'open square')
    call expect('{', .true., 'open brace')
    call expect(')', .true., 'close paren')
    call expect(']', .true., 'close square')
    call expect('}', .true., 'close brace')

    call expect('a', .false., 'a letter')
    call expect(' ', .false., 'a space')
    call expect('<', .false., 'an angle bracket is not one of ours')

    ! The whole point: no character there at all.
    call expect('', .false., 'nothing under the caret')

    ! Longer than one character, which is what a multibyte character looks
    ! like coming back from utf8_char_at. Not a bracket, and not a crash.
    call expect('()', .false., 'two brackets are not one bracket')
    call expect(char(226) // char(148) // char(130), .false., &
                'a multibyte character')

    if (nfail == 0) then
        print '(a)', 'test_bracket_char: all passed'
    else
        print '(a,i0,a)', 'test_bracket_char: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine expect(s, want, label)
        character(len=*), intent(in) :: s
        logical, intent(in) :: want
        character(len=*), intent(in) :: label
        logical :: got

        got = is_bracket_char(s)
        if (got .neqv. want) then
            nfail = nfail + 1
            print '(a,a,a,l1,a,l1)', 'FAIL ', label, ': want ', want, ' got ', got
        else
            print '(a,a)', 'ok   ', label
        end if
    end subroutine expect

end program test_bracket_char
