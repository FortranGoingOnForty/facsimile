module clipboard_module
    use iso_fortran_env, only: int32, error_unit
    implicit none
    private

    public :: copy_to_clipboard, paste_from_clipboard, cut_to_clipboard

contains

    subroutine copy_to_clipboard(text)
        character(len=*), intent(in) :: text
        integer :: unit, ios
        character(len=256) :: command

        ! Use pbcopy on macOS, xclip on Linux
        ! For now, implementing macOS version
        open(newunit=unit, file='/tmp/facsimile_clipboard.tmp', &
             status='replace', action='write', access='stream', iostat=ios)

        if (ios == 0) then
            write(unit, iostat=ios) text
            close(unit)

            ! Send to system clipboard
            command = 'cat /tmp/facsimile_clipboard.tmp | pbcopy 2>/dev/null'
            call execute_command_line(command, exitstat=ios)

            if (ios /= 0) then
                ! Try Linux xclip as fallback
                command = 'cat /tmp/facsimile_clipboard.tmp | xclip -selection clipboard 2>/dev/null'
                call execute_command_line(command, exitstat=ios)
            end if
        end if
    end subroutine copy_to_clipboard

    function paste_from_clipboard() result(text)
        character(len=:), allocatable :: text
        integer :: unit, ios, file_size
        character(len=256) :: command
        character(len=1000000) :: buffer  ! 1MB buffer for clipboard content

        ! Get clipboard content
        command = 'pbpaste > /tmp/facsimile_clipboard.tmp 2>/dev/null'
        call execute_command_line(command, exitstat=ios)

        if (ios /= 0) then
            ! Try Linux xclip as fallback
            command = 'xclip -selection clipboard -o > /tmp/facsimile_clipboard.tmp 2>/dev/null'
            call execute_command_line(command, exitstat=ios)
        end if

        if (ios == 0) then
            ! Read the clipboard content
            open(newunit=unit, file='/tmp/facsimile_clipboard.tmp', &
                 status='old', action='read', access='stream', iostat=ios)

            if (ios == 0) then
                ! Get file size
                inquire(unit=unit, size=file_size)
                if (file_size > 0 .and. file_size < 1000000) then
                    read(unit, iostat=ios) buffer(1:file_size)
                    if (ios == 0) then
                        allocate(character(len=file_size) :: text)
                        text = buffer(1:file_size)
                    else
                        text = ''
                    end if
                else
                    text = ''
                end if
                close(unit)
            else
                text = ''
            end if
        else
            text = ''
        end if

        ! Clean up temp file
        command = 'rm -f /tmp/facsimile_clipboard.tmp 2>/dev/null'
        call execute_command_line(command)
    end function paste_from_clipboard

    subroutine cut_to_clipboard(text)
        character(len=*), intent(in) :: text
        ! Cut is just copy (caller handles deletion)
        call copy_to_clipboard(text)
    end subroutine cut_to_clipboard

end module clipboard_module