module clipboard_module
    use iso_fortran_env, only: int32, error_unit
    use platform_module, only: is_windows, get_temp_dir, &
        platform_copy_to_clipboard, platform_paste_from_clipboard
    implicit none
    private

    public :: copy_to_clipboard, paste_from_clipboard, cut_to_clipboard

    ! Internal clipboard for when system clipboard is unavailable
    character(len=:), allocatable :: internal_clipboard

contains

    subroutine copy_to_clipboard(text)
        character(len=*), intent(in) :: text
        integer :: unit, ios
        character(len=512) :: command
        character(len=:), allocatable :: temp_dir, temp_file

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

        ! Send to system clipboard using temp file
        ! Try multiple clipboard tools via sh -c, suppress all output
        command = "sh -c 'cat " // temp_file // " | xsel -b -i 2>/dev/null || " // &
                  "cat " // temp_file // " | xclip -sel c 2>/dev/null || " // &
                  "cat " // temp_file // " | pbcopy 2>/dev/null || " // &
                  "cat " // temp_file // " | wl-copy 2>/dev/null || true'"
        call execute_command_line(trim(command), wait=.true., exitstat=ios)

        ! Clean up temp file
        command = 'rm -f ' // temp_file // ' 2>/dev/null'
        call execute_command_line(trim(command), wait=.true.)
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

end module clipboard_module