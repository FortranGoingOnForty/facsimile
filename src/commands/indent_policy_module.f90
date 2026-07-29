! What a Tab key actually inserts, per file.
!
! Two separate things were wrong before this existed, and they are worth
! keeping distinct:
!
!   1. Tab inserted a fixed four spaces wherever the caret was, so it did not
!      line anything up. Lining a trailing comment up against the line above
!      needs the caret to land on a tab STOP -- a multiple of the tab width --
!      not four columns further right than wherever it started.
!
!   2. A makefile recipe line must begin with a literal TAB. Spaces there are
!      not a style preference, they are a syntax error: make reports
!      "missing separator" and refuses to build. fac was silently writing
!      files that could not be built, which is why nvim flags them.
!
! So the policy has two parts -- the character, and the distance -- and only
! the first is language-specific. Keyed on the filename because that is all
! that is reliably known: the highlighter disables itself for unknown
! extensions and there is no LSP request for "how do you indent".
module indent_policy_module
    implicit none
    private

    public :: indent_uses_hard_tabs, indent_text_for, dedent_width_at
    public :: indent_string_for
    public :: INDENT_WIDTH

    ! Display columns per indent level. Matches renderer_module's TAB_WIDTH;
    ! they must agree or the caret lands somewhere the text is not.
    integer, parameter :: INDENT_WIDTH = 4

contains

    !> True when this file's indentation must be literal tab characters.
    !>
    !> Deliberately narrow. Go and some other languages conventionally use
    !> tabs, but there the choice is cosmetic and a formatter settles it;
    !> here a space is a build error, which is a different kind of fact and
    !> the only one that justifies overriding what the user typed.
    function indent_uses_hard_tabs(filename) result(res)
        character(len=*), intent(in) :: filename
        logical :: res
        character(len=:), allocatable :: base, lower_base, ext
        integer :: slash_pos, dot_pos

        res = .false.
        if (len_trim(filename) == 0) return

        slash_pos = index(trim(filename), '/', back=.true.)
        if (slash_pos > 0) then
            base = trim(filename(slash_pos+1:))
        else
            base = trim(filename)
        end if
        if (len(base) == 0) return

        lower_base = to_lower(base)

        select case(lower_base)
        case('makefile', 'gnumakefile', 'makefile.am', 'makefile.in')
            res = .true.
            return
        end select

        dot_pos = index(base, '.', back=.true.)
        if (dot_pos > 0) then
            ext = to_lower(base(dot_pos:))
            select case(ext)
            case('.mk', '.mak', '.make')
                res = .true.
            end select
        end if

        ! Makefile.local, Makefile.dev and friends: an unknown suffix on a
        ! makefile is still a makefile.
        if (.not. res .and. len(lower_base) >= 9) then
            if (lower_base(1:9) == 'makefile.') res = .true.
        end if
    end function indent_uses_hard_tabs

    !> The text one Tab press should insert.
    !>
    !> display_col is the caret's position in display CELLS from the start of
    !> the line, zero-based -- not a character index, because a line that
    !> already contains tabs or wide characters has no fixed relationship
    !> between the two. Callers must convert first.
    function indent_text_for(filename, display_col) result(text)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: display_col
        character(len=:), allocatable :: text
        integer :: n

        if (indent_uses_hard_tabs(filename)) then
            text = achar(9)
            return
        end if

        ! Advance to the next multiple of INDENT_WIDTH. At column 0 that is a
        ! full width; at column 6 with width 4 it is 2, landing on 8. A caret
        ! already on a stop moves a whole width rather than standing still.
        n = INDENT_WIDTH - modulo(display_col, INDENT_WIDTH)
        if (n <= 0) n = INDENT_WIDTH
        text = repeat(' ', n)
    end function indent_text_for

    !> Whitespace representing `width` display columns of indentation.
    !>
    !> For writing a whole indent at once -- auto-indent on Enter, or Tab
    !> jumping a blank line to where it belongs -- as opposed to indent_text_for
    !> above, which answers "one more press from here".
    !>
    !> The character matters for the same reason it does there: auto-indent was
    !> writing spaces into makefile recipes, which is not a style question but a
    !> "missing separator" build failure. Tab was fixed for that in 0.22 and
    !> Enter was not, because the choice lived at the Tab call site instead of
    !> here.
    function indent_string_for(filename, width) result(text)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width
        character(len=:), allocatable :: text
        integer :: n

        text = ''
        if (width <= 0) return

        if (indent_uses_hard_tabs(filename)) then
            ! Round up: a recipe line needs at least one tab, and half a tab
            ! does not exist. Anything under a full width still gets one.
            n = (width + INDENT_WIDTH - 1) / INDENT_WIDTH
            if (n < 1) n = 1
            text = repeat(achar(9), n)
        else
            text = repeat(' ', width)
        end if
    end function indent_string_for

    !> How many leading characters one Shift-Tab should remove, so that
    !> dedent undoes exactly what indent did.
    function dedent_width_at(filename, line) result(n)
        character(len=*), intent(in) :: filename, line
        integer :: n
        integer :: i

        n = 0
        if (len(line) == 0) return

        if (indent_uses_hard_tabs(filename)) then
            ! One tab, or a run of spaces someone left behind.
            if (line(1:1) == achar(9)) then
                n = 1
                return
            end if
        end if

        do i = 1, min(INDENT_WIDTH, len(line))
            if (line(i:i) /= ' ') exit
            n = n + 1
        end do

        ! Nothing but a stray tab to remove.
        if (n == 0 .and. line(1:1) == achar(9)) n = 1
    end function dedent_width_at

    function to_lower(str) result(res)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: res
        integer :: i, code

        do i = 1, len(str)
            code = iachar(str(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                res(i:i) = achar(code + 32)
            else
                res(i:i) = str(i:i)
            end if
        end do
    end function to_lower

end module indent_policy_module
