module clipboard_module
    use platform_module, only: is_windows, get_temp_dir, &
        platform_copy_to_clipboard, platform_paste_from_clipboard, have_command
    implicit none
    private

    public :: copy_to_clipboard, paste_from_clipboard, cut_to_clipboard

    ! Internal clipboard for when system clipboard is unavailable
    character(len=:), allocatable :: internal_clipboard

contains

    subroutine copy_to_clipboard(text)
        character(len=*), intent(in) :: text
        integer :: unit, ios, cmdstat
        character(len=:), allocatable :: command, temp_dir, temp_file, quoted_file

        ! Guard against empty or invalid text
        if (len_trim(text) == 0) return

        ! Always store in internal clipboard as fallback
        if (allocated(internal_clipboard)) deallocate(internal_clipboard)
        allocate(character(len=len_trim(text)) :: internal_clipboard)
        internal_clipboard = trim(text)

        ! On Windows, use native clipboard API
        if (is_windows()) then
            if (platform_copy_to_clipboard(trim(text))) return
            ! Fall through to internal clipboard only
            return
        end if

        ! Unix: Try to also copy to system clipboard
        ! Write text to temp file first (avoids shell escaping issues)
        temp_dir = get_temp_dir()
        temp_file = temp_dir // 'facsimile_clipboard.tmp'

        open(newunit=unit, file=temp_file, &
             status='replace', action='write', access='stream', iostat=ios)

        if (ios /= 0) return

        write(unit, iostat=ios) trim(text)
        close(unit)

        if (ios /= 0) return

        ! Feed an installed clipboard utility directly from the file. The old
        ! `cat file | tool` chain tried missing Linux utilities before pbcopy
        ! on macOS. Each missing reader closed its pipe and GNU cat printed a
        ! broken-pipe diagnostic into the editor's live terminal surface.
        ! Probing first and redirecting the whole utility keeps every fallback
        ! silent, including installed tools that fail because no display is
        ! available.
        quoted_file = shell_quote(temp_file)
        ios = 1
        if (have_command('xsel')) then
            command = 'xsel -b -i < ' // quoted_file // ' >/dev/null 2>&1'
            call execute_command_line(command, wait=.true., exitstat=ios, cmdstat=cmdstat)
        end if
        if (ios /= 0) then
            if (have_command('xclip')) then
                command = 'xclip -sel c < ' // quoted_file // ' >/dev/null 2>&1'
                call execute_command_line(command, wait=.true., exitstat=ios, cmdstat=cmdstat)
            end if
        end if
        if (ios /= 0) then
            if (have_command('pbcopy')) then
                command = 'pbcopy < ' // quoted_file // ' >/dev/null 2>&1'
                call execute_command_line(command, wait=.true., exitstat=ios, cmdstat=cmdstat)
            end if
        end if
        if (ios /= 0) then
            if (have_command('wl-copy')) then
                command = 'wl-copy < ' // quoted_file // ' >/dev/null 2>&1'
                call execute_command_line(command, wait=.true., exitstat=ios, cmdstat=cmdstat)
            end if
        end if

        ! Clean up temp file
        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete', iostat=ios)
    end subroutine copy_to_clipboard

    function paste_from_clipboard() result(text)
        character(len=:), allocatable :: text
        integer :: unit, ios, file_size
        character(len=512) :: command
        character(len=:), allocatable :: buffer, temp_dir, temp_file

        text = ''

        ! On Windows, use native clipboard API
        if (is_windows()) then
            text = platform_paste_from_clipboard()
            if (len_trim(text) > 0) return
            ! Fall through to internal clipboard
            goto 100
        end if

        ! Unix: Try system clipboard first
        temp_dir = get_temp_dir()
        temp_file = temp_dir // 'facsimile_clipboard.tmp'

        command = "sh -c 'xsel -b -o > " // temp_file // " 2>/dev/null || " // &
                  "xclip -sel c -o > " // temp_file // " 2>/dev/null || " // &
                  "pbpaste > " // temp_file // " 2>/dev/null || " // &
                  "wl-paste > " // temp_file // " 2>/dev/null || true'"
        call execute_command_line(trim(command), wait=.true., exitstat=ios)

        if (ios == 0) then
            ! Try to read the clipboard content
            open(newunit=unit, file=temp_file, &
                 status='old', action='read', access='stream', iostat=ios)

            if (ios == 0) then
                inquire(unit=unit, size=file_size)
                if (file_size > 0 .and. file_size < 1000000) then
                    allocate(character(len=file_size) :: buffer)
                    read(unit, iostat=ios) buffer
                    if (ios == 0) then
                        if (allocated(text)) deallocate(text)
                        allocate(character(len=file_size) :: text)
                        text = buffer
                    end if
                    if (allocated(buffer)) deallocate(buffer)
                end if
                close(unit)
            end if

            ! Clean up temp file
            command = 'rm -f ' // temp_file // ' 2>/dev/null'
            call execute_command_line(trim(command), wait=.true.)
        end if

100     continue
        ! Fall back to internal clipboard if system clipboard failed or was empty
        if (len_trim(text) == 0 .and. allocated(internal_clipboard)) then
            if (len(internal_clipboard) > 0) then
                if (allocated(text)) deallocate(text)
                allocate(character(len=len(internal_clipboard)) :: text)
                text = internal_clipboard
            end if
        end if
    end function paste_from_clipboard

    subroutine cut_to_clipboard(text)
        character(len=*), intent(in) :: text
        ! Cut is just copy (caller handles deletion)
        call copy_to_clipboard(text)
    end subroutine cut_to_clipboard

    !> Single-quote a path for a POSIX shell command.
    pure function shell_quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        integer :: i, pos, n, quote_count

        n = len_trim(text)
        quote_count = 0
        do i = 1, n
            if (text(i:i) == "'") quote_count = quote_count + 1
        end do

        allocate(character(len=n + 2 + 3 * quote_count) :: quoted)
        quoted(1:1) = "'"
        pos = 2
        do i = 1, n
            if (text(i:i) == "'") then
                quoted(pos:pos+3) = "'\''"
                pos = pos + 4
            else
                quoted(pos:pos) = text(i:i)
                pos = pos + 1
            end if
        end do
        quoted(pos:pos) = "'"
    end function shell_quote

end module clipboard_module
