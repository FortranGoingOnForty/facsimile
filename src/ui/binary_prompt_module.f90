! Binary file prompt module
! Prompts user when attempting to open binary files

module binary_prompt_module
    implicit none
    private

    public :: binary_file_prompt

contains

    !> Prompt user about opening a binary file
    !> Returns .true. to open in hex view, .false. to cancel
    function binary_file_prompt(filename) result(open_anyway)
        use terminal_io_module, only: terminal_write, terminal_move_cursor, terminal_clear_screen
        use input_handler_module, only: get_key_input
        character(len=*), intent(in) :: filename
        logical :: open_anyway
        character(len=512) :: prompt_text, basename
        character(len=32) :: key_input
        integer :: status, i, slash_pos

        open_anyway = .false.

        ! Extract basename from filename
        basename = filename
        slash_pos = 0
        do i = len_trim(filename), 1, -1
            if (filename(i:i) == '/') then
                slash_pos = i
                exit
            end if
        end do
        if (slash_pos > 0 .and. slash_pos < len_trim(filename)) then
            basename = filename(slash_pos+1:)
        end if

        ! Clear screen and show warning
        call terminal_clear_screen()
        call terminal_move_cursor(3, 1)

        write(prompt_text, '(A)') '══════════════════════════════════════════════'
        call terminal_write(trim(prompt_text))

        call terminal_move_cursor(4, 1)
        call terminal_write('  WARNING: Binary File Detected')

        call terminal_move_cursor(5, 1)
        write(prompt_text, '(A)') '══════════════════════════════════════════════'
        call terminal_write(trim(prompt_text))

        call terminal_move_cursor(7, 1)
        write(prompt_text, '(A,A)') 'File: ', trim(basename)
        call terminal_write(trim(prompt_text))

        call terminal_move_cursor(9, 1)
        call terminal_write('This file appears to be a binary file (contains null bytes).')

        call terminal_move_cursor(10, 1)
        call terminal_write('Binary files like .mod, .o, .a, executables, and images')

        call terminal_move_cursor(11, 1)
        call terminal_write('cannot be edited as text.')

        call terminal_move_cursor(13, 1)
        call terminal_write('Options:')

        call terminal_move_cursor(15, 1)
        call terminal_write('[v]iew  - Open in hex viewer mode (read-only)')

        call terminal_move_cursor(16, 1)
        call terminal_write('[c]ancel - Return to editor')

        call terminal_move_cursor(18, 1)
        call terminal_write('Choice: ')

        ! Get user input
        do
            call get_key_input(key_input, status)
            if (status == 0) then
                if (key_input == 'v' .or. key_input == 'V') then
                    open_anyway = .true.
                    exit
                else if (key_input == 'c' .or. key_input == 'C' .or. key_input == 'esc') then
                    open_anyway = .false.
                    exit
                end if
            end if
        end do
    end function binary_file_prompt

end module binary_prompt_module
