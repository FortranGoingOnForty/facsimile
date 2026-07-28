program test_path_canonical
    ! One spelling per file path.
    !
    ! Two open copies of the same file are matched by comparing their stored
    ! filenames as plain strings, on every non-cursor keystroke. Four spellings
    ! of one path therefore look like four files, and the copies diverge until
    ! whichever saves last wins. This pins the normalisation that stops it.
    use platform_module, only: canonical_path
    implicit none

    integer :: nfail

    nfail = 0

    call test_the_spellings_that_actually_collide()
    call test_dot_dot_is_resolved()
    call test_absolute_paths_survive()
    call test_degenerate_input()
    call test_it_is_idempotent()

    if (nfail == 0) then
        print '(a)', 'test_path_canonical: all passed'
    else
        print '(a,i0,a)', 'test_path_canonical: ', nfail, ' FAILED'
        stop 1
    end if

contains

    subroutine same(a, b, label)
        character(len=*), intent(in) :: a, b, label

        if (canonical_path(a) /= canonical_path(b)) then
            print '(a)', '  FAIL: ' // label
            print '(a)', '        ' // a // ' -> ' // canonical_path(a)
            print '(a)', '        ' // b // ' -> ' // canonical_path(b)
            nfail = nfail + 1
        end if
    end subroutine same

    subroutine is(a, expect, label)
        character(len=*), intent(in) :: a, expect, label

        if (canonical_path(a) /= expect) then
            print '(a)', '  FAIL: ' // label
            print '(a)', '        ' // a // ' -> ' // canonical_path(a) // &
                         '  (wanted ' // expect // ')'
            nfail = nfail + 1
        end if
    end subroutine is

    ! The editor generates all of these itself: the tree joins with '/', the
    ! workspace prefixes a root, and argv arrives however the shell spelled it.
    subroutine test_the_spellings_that_actually_collide()
        call same('a.c', './a.c', 'a leading ./ is not a different file')
        call same('dir/a.c', 'dir//a.c', 'a doubled slash is not a different file')
        call same('dir/a.c', 'dir/./a.c', 'an interior /./ is not a different file')
        call same('dir/a.c', './dir//./a.c', 'nor all three at once')
        call is('./a.c', 'a.c', 'the leading ./ is removed')
        call is('dir//a.c', 'dir/a.c', 'the doubled slash collapses')
    end subroutine test_the_spellings_that_actually_collide

    subroutine test_dot_dot_is_resolved()
        call is('dir/sub/../a.c', 'dir/a.c', 'one .. cancels one segment')
        call is('dir/sub/deep/../../a.c', 'dir/a.c', 'two cancel two')
        call same('dir/a.c', 'dir/sub/../a.c', 'and the result compares equal')

        ! Nothing to cancel against: keep them, or a relative path silently
        ! changes meaning.
        call is('../a.c', '../a.c', 'a leading .. on a relative path is kept')
        call is('../../a.c', '../../a.c', 'and so are several')

        ! Above an absolute root there is nowhere to go.
        call is('/../a.c', '/a.c', '.. cannot escape an absolute root')
        call is('/a/../../b.c', '/b.c', 'even repeatedly')
    end subroutine test_dot_dot_is_resolved

    subroutine test_absolute_paths_survive()
        call is('/home/u/a.c', '/home/u/a.c', 'a clean absolute path is untouched')
        call is('/home//u/./a.c', '/home/u/a.c', 'a messy one is cleaned')
        call is('/a.c', '/a.c', 'a file at the root')

        ! Relative and absolute deliberately stay distinct: resolving them
        ! needs the working directory, and the workspace file stores paths
        ! relative to its root on purpose.
        if (canonical_path('a.c') == canonical_path('/a.c')) then
            print '(a)', '  FAIL: relative and absolute must not be conflated'
            nfail = nfail + 1
        end if
    end subroutine test_absolute_paths_survive

    subroutine test_degenerate_input()
        call is('', '', 'empty stays empty')
        call is('.', '.', 'a bare dot is the current directory')
        call is('./', '.', 'and so is ./')
        call is('/', '/', 'the root survives')
        call is('a.c', 'a.c', 'a bare filename is already canonical')
        call is('  a.c  ', 'a.c', 'surrounding blanks are dropped')
        call is('dir/', 'dir', 'a trailing slash is dropped')
    end subroutine test_degenerate_input

    ! Filenames are normalised where they are stored, so the function will be
    ! applied to already-normalised input constantly. It must be a no-op then.
    subroutine test_it_is_idempotent()
        call check_idem('./dir//sub/../a.c')
        call check_idem('/home//u/./x.f90')
        call check_idem('../../up.c')
        call check_idem('/')
        call check_idem('.')
    end subroutine test_it_is_idempotent

    subroutine check_idem(p)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: once, twice

        once = canonical_path(p)
        twice = canonical_path(once)
        if (once /= twice) then
            print '(a)', '  FAIL: not idempotent for ' // p
            print '(a)', '        ' // once // ' -> ' // twice
            nfail = nfail + 1
        end if
    end subroutine check_idem

end program test_path_canonical
