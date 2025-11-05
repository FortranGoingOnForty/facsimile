! Save prompt module
! Prompts user to save/backup files on quit

module save_prompt_module
    implicit none
    private

    public :: save_prompt, save_prompt_result_t

    type :: save_prompt_result_t
        character :: action = 'c'  ! 'y' = yes (save), 'n' = no (backup), 'c' = cancel
    end type save_prompt_result_t

contains

    !> Prompt user to save a file
    !> Returns 'y' to save, 'n' to skip (backup), 'c' to cancel quit
    subroutine save_prompt(filename, result)
        use terminal_io_module, only: terminal_write, terminal_move_cursor, terminal_clear_screen
        use input_handler_module, only: get_key_input
        character(len=*), intent(in) :: filename
        type(save_prompt_result_t), intent(out) :: result
        character(len=512) :: prompt_text, basename
        character(len=32) :: key_input
        integer :: status, i, slash_pos

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

        ! Clear screen and show prompt
        call terminal_clear_screen()
        call terminal_move_cursor(3, 1)

        write(prompt_text, '(A,A,A)') 'Save ', trim(basename), '? '
        call terminal_write(trim(prompt_text))

        call terminal_move_cursor(4, 1)
        call terminal_write('[y]es - save file')

        call terminal_move_cursor(5, 1)
        call terminal_write('[n]o  - skip (will be backed up)')

        call terminal_move_cursor(6, 1)
        call terminal_write('[c]ancel - don''t quit')

        call terminal_move_cursor(8, 1)
        call terminal_write('Choice: ')

        ! Get user input
        result%action = 'c'  ! Default to cancel
        do
            call get_key_input(key_input, status)
            if (status == 0) then
                if (key_input == 'y' .or. key_input == 'Y') then
                    result%action = 'y'
                    exit
                else if (key_input == 'n' .or. key_input == 'N') then
                    result%action = 'n'
                    exit
                else if (key_input == 'c' .or. key_input == 'C') then
                    result%action = 'c'
                    exit
                else if (key_input == 'ESCAPE') then
                    result%action = 'c'  ! ESC = cancel
                    exit
                end if
            end if
        end do
    end subroutine save_prompt

end module save_prompt_module
