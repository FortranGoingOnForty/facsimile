!> The channel from a shell inside the terminal panel back to the editor.
!>
!> `fac foo.c` typed in the integrated terminal should open foo.c in the
!> session that owns the terminal, the way `code` does, rather than starting a
!> second editor nested inside the first one. This is how the two halves talk.
!>
!> A spool DIRECTORY, not a pipe or a socket. The host creates one per session
!> and advertises it to child shells as FAC_SESSION; a client drops one small
!> file in it per request and exits; the host lists the directory on its normal
!> tick and consumes what it finds.
!>
!> The directory matters. A FIFO would need a non-blocking open and a reader
!> that survives having no writer, and a single shared file would need the
!> host's read and a client's append to interleave safely. With one file per
!> request there is nothing to interleave: each request is written to a
!> temporary name and RENAMED into place, and rename within a directory is
!> atomic, so the host either sees a whole request or does not see it yet.
!> Several clients at once cost nothing extra.
module session_ipc_module
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: session_ipc_begin, session_ipc_end
    public :: session_ipc_send, session_ipc_take
    public :: session_ipc_dir, session_ipc_active

    !> Where this session's spool lives. Empty when we are not hosting one.
    character(len=:), allocatable :: g_dir

    !> Bumped per request so a client sending twice cannot collide with itself.
    integer :: g_seq = 0

contains

    !> The spool directory for this session, or '' if there is none.
    function session_ipc_dir() result(d)
        character(len=:), allocatable :: d

        if (allocated(g_dir)) then
            d = g_dir
        else
            d = ''
        end if
    end function session_ipc_dir

    !> Whether this process is hosting a spool.
    function session_ipc_active() result(yes)
        logical :: yes

        yes = .false.
        if (allocated(g_dir)) yes = len_trim(g_dir) > 0
    end function session_ipc_active

    !> Create this session's spool directory. Idempotent.
    !>
    !> Named for the pid so two editors running at once never share one, and
    !> placed under TMPDIR so it is cleaned up even if we die badly.
    subroutine session_ipc_begin(dir)
        character(len=:), allocatable, intent(out) :: dir
        character(len=512) :: tmp_root
        integer :: tlen, pid
        character(len=32) :: pid_text

        pid = get_pid()
        call get_environment_variable('TMPDIR', tmp_root, tlen)
        if (tlen <= 0) then
            tmp_root = '/tmp'
            tlen = 4
        end if
        ! A trailing slash in TMPDIR would double up in every path we build.
        do while (tlen > 1 .and. tmp_root(tlen:tlen) == '/')
            tlen = tlen - 1
        end do

        write(pid_text, '(i0)') pid
        g_dir = tmp_root(1:tlen) // '/fac-session-' // trim(pid_text)
        dir = g_dir

        call execute_command_line("mkdir -p '" // g_dir // "' 2>/dev/null", &
                                  wait=.true.)
    end subroutine session_ipc_begin

    !> Remove the spool directory. Safe to call when there is none.
    subroutine session_ipc_end()
        if (.not. session_ipc_active()) return
        ! Guarded rather than trusting the variable: an empty or short path
        ! here would be an rm -rf on something that is not ours.
        if (index(g_dir, '/fac-session-') > 0) then
            call execute_command_line("rm -rf '" // g_dir // "' 2>/dev/null", &
                                      wait=.true.)
        end if
        deallocate(g_dir)
    end subroutine session_ipc_end

    !> Client half: hand `path` to the editor whose spool is `dir`.
    !>
    !> `kind` is 'file' or 'dir'. Written to a dot-prefixed temporary and
    !> renamed into place, so the host never lists a half-written request --
    !> and never lists the temporary either, since it only takes req.* names.
    subroutine session_ipc_send(dir, kind, path, ok)
        character(len=*), intent(in) :: dir, kind, path
        logical, intent(out) :: ok
        character(len=:), allocatable :: stem, tmp_file, final_file
        character(len=32) :: pid_text, seq_text
        integer :: unit, ios, pid
        logical :: there

        ok = .false.
        if (len_trim(dir) == 0 .or. len_trim(path) == 0) return

        pid = get_pid()
        g_seq = g_seq + 1
        write(pid_text, '(i0)') pid
        write(seq_text, '(i0)') g_seq
        stem = trim(pid_text) // '-' // trim(seq_text)
        tmp_file = trim(dir) // '/.tmp.' // stem
        final_file = trim(dir) // '/req.' // stem

        open(newunit=unit, file=tmp_file, status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) return
        write(unit, '(a)', iostat=ios) trim(kind)
        if (ios == 0) write(unit, '(a)', iostat=ios) trim(path)
        close(unit)
        if (ios /= 0) then
            call execute_command_line("rm -f '" // tmp_file // "' 2>/dev/null", &
                                      wait=.true.)
            return
        end if

        call execute_command_line("mv -f '" // tmp_file // "' '" // &
                                  final_file // "' 2>/dev/null", wait=.true.)
        inquire(file=final_file, exist=there)
        ok = there
    end subroutine session_ipc_send

    !> Host half: consume one pending request, if there is one.
    !>
    !> Removes the request before reporting it, so a path that somehow cannot
    !> be opened is not retried on every tick for the rest of the session.
    subroutine session_ipc_take(kind, path, found)
        use dir_scan_module, only: dir_entry_t, list_directory
        character(len=:), allocatable, intent(out) :: kind, path
        logical, intent(out) :: found
        character(len=:), allocatable :: req
        character(len=512) :: line
        type(dir_entry_t), allocatable :: entries(:)
        integer :: unit, ios, n, i
        logical :: ok

        kind = ''
        path = ''
        found = .false.
        if (.not. session_ipc_active()) return

        ! readdir, not a shelled-out ls. This runs on the editor's normal tick
        ! for as long as a terminal panel is alive, and forking a process
        ! several times a second to discover that nothing happened is not a
        ! reasonable price for a feature used a few times an hour.
        call list_directory(g_dir, entries, n, ok)
        if (.not. ok .or. n <= 0) return

        ! One request per tick. Deliberate: opening a file re-renders, and
        ! draining a burst in one pass would do that work several times over
        ! for frames nobody sees. The rest keep until the next tick.
        req = ''
        do i = 1, n
            if (len_trim(entries(i)%name) < 4) cycle
            if (entries(i)%name(1:4) /= 'req.') cycle
            req = g_dir // '/' // trim(entries(i)%name)
            exit
        end do
        if (len_trim(req) == 0) return

        open(newunit=unit, file=req, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, '(a)', iostat=ios) line
        if (ios == 0) kind = trim(adjustl(line))
        if (ios == 0) read(unit, '(a)', iostat=ios) line
        if (ios == 0) path = trim(adjustl(line))
        close(unit)

        call execute_command_line("rm -f '" // req // "' 2>/dev/null", &
                                  wait=.true.)

        found = len_trim(path) > 0
        if (.not. found) then
            kind = ''
            path = ''
        end if
    end subroutine session_ipc_take

    !> This process's pid.
    function get_pid() result(pid)
        integer :: pid
        pid = getpid()
    end function get_pid

end module session_ipc_module
