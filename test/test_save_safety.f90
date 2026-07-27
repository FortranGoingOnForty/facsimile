program test_save_safety
    ! Saving a buffer that was never loaded must not touch the file.
    !
    ! This is a precondition for anything lazy. `buffer_save_file` opens with
    ! status='replace' and then walks the gap buffer; with no allocated data
    ! both write loops iterate zero times, `ios` is never set, and the routine
    ! reports success -- having just truncated a real file to nothing.
    !
    ! Nothing today deliberately saves an unloaded buffer, so this is a guard
    ! rail rather than a regression test. It has to exist before tab groups
    ! open files lazily, because at that point most open tabs hold exactly such
    ! a buffer and every save path can reach one.
    use text_buffer_module
    implicit none

    integer :: nfail
    character(len=*), parameter :: PATH = '/tmp/fac_test_save_safety.txt'
    character(len=*), parameter :: CONTENT = 'IMPORTANT DATA'

    nfail = 0

    call test_unallocated_buffer_is_refused()
    call test_a_real_buffer_still_saves()
    call test_an_emptied_buffer_still_saves()

    call unlink(PATH)

    if (nfail == 0) then
        print '(a)', 'test_save_safety: all passed'
    else
        print '(a,i0,a)', 'test_save_safety: ', nfail, ' FAILED'
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

    subroutine write_seed()
        integer :: u
        open(newunit=u, file=PATH, status='replace', action='write', &
             form='unformatted', access='stream')
        write(u) CONTENT
        close(u)
    end subroutine write_seed

    function read_back() result(text)
        character(len=:), allocatable :: text
        integer :: u, sz, ios
        character(len=:), allocatable :: buf

        text = ''
        open(newunit=u, file=PATH, status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) return
        inquire(unit=u, size=sz)
        if (sz > 0) then
            allocate(character(len=sz) :: buf)
            read(u, iostat=ios) buf
            if (ios == 0) text = buf
        end if
        close(u)
    end function read_back

    subroutine unlink(p)
        character(len=*), intent(in) :: p
        integer :: u, ios
        open(newunit=u, file=p, status='old', iostat=ios)
        if (ios == 0) close(u, status='delete')
    end subroutine unlink

    ! The property that matters: the bytes on disk are untouched. A nonzero
    ! status alone would not be enough -- the file is opened before the write
    ! loops run, so a late refusal would already have truncated it.
    subroutine test_unallocated_buffer_is_refused()
        type(buffer_t) :: b
        integer :: status

        call write_seed()
        call cleanup_buffer(b)          ! guarantees data is not allocated

        call buffer_save_file(b, PATH, status)
        call check(status /= 0, 'saving an unloaded buffer reports failure')
        call check(read_back() == CONTENT, &
                   'and leaves the file byte-identical')
    end subroutine test_unallocated_buffer_is_refused

    subroutine test_a_real_buffer_still_saves()
        type(buffer_t) :: b
        integer :: status

        call write_seed()
        call init_buffer(b, 'NEW TEXT')
        call buffer_save_file(b, PATH, status)
        call check(status == 0, 'a loaded buffer still saves')
        call check(read_back() == 'NEW TEXT', 'and writes its contents')
        call cleanup_buffer(b)
    end subroutine test_a_real_buffer_still_saves

    ! Deliberately empty is not the same as never loaded: truncating a file to
    ! zero on purpose must keep working.
    subroutine test_an_emptied_buffer_still_saves()
        type(buffer_t) :: b
        integer :: status

        call write_seed()
        call init_buffer(b)             ! allocated, but holds no text
        call buffer_save_file(b, PATH, status)
        call check(status == 0, 'an intentionally empty buffer still saves')
        call check(len(read_back()) == 0, 'and truncates the file')
        call cleanup_buffer(b)
    end subroutine test_an_emptied_buffer_still_saves

end program test_save_safety
