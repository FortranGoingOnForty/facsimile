program test_indent_policy
    ! What Tab inserts, and where it lands.
    !
    ! Two behaviours are pinned here. The first is that Tab advances to a tab
    ! STOP rather than adding a fixed number of spaces -- the difference is
    ! invisible from column 0 and is the whole bug everywhere else, which is
    ! why the table below walks every column across a stop boundary.
    !
    ! The second is that a makefile gets a literal tab. That one is not a
    ! preference: make treats a space-indented recipe as a syntax error, so
    ! getting it wrong writes a file that cannot be built.
    use indent_policy_module
    implicit none

    integer :: nfail

    nfail = 0

    call test_hard_tab_detection()
    call test_tab_stops()
    call test_makefile_inserts_a_real_tab()
    call test_dedent_is_the_inverse()

    if (nfail == 0) then
        print '(a)', 'test_indent_policy: all passed'
    else
        print '(a,i0,a)', 'test_indent_policy: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine check(cond, label)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label

        if (.not. cond) then
            print '(a)', '  FAIL: ' // label
            nfail = nfail + 1
        end if
    end subroutine check

    subroutine test_hard_tab_detection()
        call check(indent_uses_hard_tabs('Makefile'), 'Makefile')
        call check(indent_uses_hard_tabs('makefile'), 'lowercase makefile')
        call check(indent_uses_hard_tabs('GNUmakefile'), 'GNUmakefile')
        call check(indent_uses_hard_tabs('/home/u/proj/Makefile'), &
                   'Makefile with a path in front of it')
        call check(indent_uses_hard_tabs('Makefile.am'), 'automake input')
        call check(indent_uses_hard_tabs('rules.mk'), '.mk')
        call check(indent_uses_hard_tabs('build.mak'), '.mak')
        call check(indent_uses_hard_tabs('Makefile.local'), &
                   'an unknown suffix on a makefile is still a makefile')

        ! The damage from a false positive is worse than from a false
        ! negative: a stray tab in a C or Python file is a style problem in
        ! one direction and a syntax error in the other.
        call check(.not. indent_uses_hard_tabs('main.c'), 'C is not tab-indented')
        call check(.not. indent_uses_hard_tabs('app.py'), 'Python is not')
        call check(.not. indent_uses_hard_tabs('mod.f90'), 'Fortran is not')
        call check(.not. indent_uses_hard_tabs('makefile_helper.py'), &
                   'a python file merely named after make is not a makefile')
        call check(.not. indent_uses_hard_tabs('README.md'), 'markdown is not')
        call check(.not. indent_uses_hard_tabs(''), 'no filename is not')
    end subroutine test_hard_tab_detection

    ! The reported symptom: lining up trailing comments, Tab on the second
    ! line lands one column past where the first line ended up, because a
    ! fixed four spaces takes you four columns on from wherever you were
    ! rather than to the next stop.
    subroutine test_tab_stops()
        call check(len(indent_text_for('a.c', 0)) == 4, 'column 0 -> a full width')
        call check(len(indent_text_for('a.c', 1)) == 3, 'column 1 -> lands on 4')
        call check(len(indent_text_for('a.c', 2)) == 2, 'column 2 -> lands on 4')
        call check(len(indent_text_for('a.c', 3)) == 1, 'column 3 -> lands on 4')
        call check(len(indent_text_for('a.c', 4)) == 4, 'already on a stop -> a full width')
        call check(len(indent_text_for('a.c', 5)) == 3, 'column 5 -> lands on 8')
        call check(len(indent_text_for('a.c', 14)) == 2, 'column 14 -> lands on 16')

        ! Every landing point is a multiple of the width. This is the property
        ! that makes two adjacent lines line up, so assert it directly rather
        ! than trusting the lengths above.
        block
            integer :: c, landed
            do c = 0, 40
                landed = c + len(indent_text_for('a.c', c))
                call check(modulo(landed, INDENT_WIDTH) == 0, &
                           'every tab lands on a stop')
                call check(landed > c, 'a tab always moves the caret')
            end do
        end block

        call check(indent_text_for('a.c', 0) == '    ', 'spaces, not tabs, in C')
    end subroutine test_tab_stops

    subroutine test_makefile_inserts_a_real_tab()
        call check(indent_text_for('Makefile', 0) == achar(9), &
                   'a makefile gets one literal tab')
        call check(len(indent_text_for('Makefile', 0)) == 1, 'exactly one character')

        ! A tab is a tab wherever the caret is: make cares about the byte at
        ! the start of the line, not about columns.
        call check(indent_text_for('Makefile', 7) == achar(9), &
                   'still a tab mid-line')
        call check(index(indent_text_for('Makefile', 3), ' ') == 0, &
                   'never a space in a makefile')
    end subroutine test_makefile_inserts_a_real_tab

    ! Shift-Tab must remove exactly what Tab added, or repeated indent and
    ! dedent walks the line sideways.
    subroutine test_dedent_is_the_inverse()
        call check(dedent_width_at('a.c', '    code') == 4, 'four spaces come off')
        call check(dedent_width_at('a.c', '  code') == 2, 'a partial indent comes off')
        call check(dedent_width_at('a.c', 'code') == 0, 'nothing to remove')
        call check(dedent_width_at('a.c', '') == 0, 'empty line is safe')

        call check(dedent_width_at('Makefile', achar(9) // 'gcc') == 1, &
                   'one tab comes off a makefile line')
        call check(dedent_width_at('Makefile', '    gcc') == 4, &
                   'spaces someone else left in a makefile still come off')

        ! A tab in a space-indented file should still be removable rather
        ! than leaving Shift-Tab inert on a line it cannot fix.
        call check(dedent_width_at('a.c', achar(9) // 'x') == 1, &
                   'a stray tab comes off a C line too')
    end subroutine test_dedent_is_the_inverse

end program test_indent_policy
