! Save prompt module
! Prompts user to save/backup files on quit

module save_prompt_module
    implicit none
    private

    public :: save_prompt, save_prompt_result_t

    type :: save_prompt_result_t
        character :: action = 'c'  ! 's' = save, 'd' = discard (backup), 'a' = all, 'c' = cancel
    end type save_prompt_result_t

contains

    !> Prompt user to save a file
    !> Returns 's' to save, 'd' to discard (backup), 'a' to save all, 'c' to cancel quit
    subroutine save_prompt(filename, result, current_index, total_count)
        use terminal_io_module, only: terminal_write, terminal_move_cursor, terminal_clear_screen, terminal_flush
        use input_handler_module, only: get_key_input
        character(len=*), intent(in) :: filename
        type(save_prompt_result_t), intent(out) :: result
        integer, intent(in), optional :: current_index, total_count
        character(len=512) :: prompt_text, basename
        character(len=32) :: key_input
        integer :: status, i, slash_pos
        logical :: show_all_option

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

        ! Determine if we should show the [a]ll option
        show_all_option = .false.
        if (present(current_index) .and. present(total_count)) then
            if (current_index < total_count) then
                show_all_option = .true.
            end if
        end if

        ! Clear screen and show prompt
        call terminal_clear_screen()
        call terminal_move_cursor(3, 1)

        ! Build prompt with progress if available
        if (present(current_index) .and. present(total_count)) then
            write(prompt_text, '(A,I0,A,I0,A,A)') 'Unsaved changes [', current_index, &
                ' of ', total_count, ']: ', trim(basename)
        else
            write(prompt_text, '(A,A)') 'Unsaved changes: ', trim(basename)
        end if
        call terminal_write(trim(prompt_text))

        call terminal_move_cursor(5, 1)
        call terminal_write('[s]ave    - Save changes')

        call terminal_move_cursor(6, 1)
        call terminal_write('[d]iscard - Don''t save (backup will be created for recovery)')

        if (show_all_option) then
            call terminal_move_cursor(7, 1)
            call terminal_write('[a]ll     - Save all remaining unsaved files')

            call terminal_move_cursor(8, 1)
            call terminal_write('[c]ancel  - Cancel quit and return to editing')

            call terminal_move_cursor(10, 1)
        else
            call terminal_move_cursor(7, 1)
            call terminal_write('[c]ancel  - Cancel quit and return to editing')

            call terminal_move_cursor(9, 1)
        end if
        call terminal_write('Choice: ')
        call terminal_flush()

        ! Get user input
        result%action = 'c'  ! Default to cancel
        do
            call get_key_input(key_input, status)
            if (status == 0) then
                if (key_input == 's' .or. key_input == 'S') then
                    result%action = 's'
                    exit
                else if (key_input == 'd' .or. key_input == 'D') then
                    result%action = 'd'
                    exit
                else if (show_all_option .and. (key_input == 'a' .or. key_input == 'A')) then
                    result%action = 'a'
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
