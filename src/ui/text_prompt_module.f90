module text_prompt_module
    use iso_fortran_env, only: int32
    use terminal_io_module
    implicit none
    private

    public :: show_text_prompt

contains

    subroutine show_text_prompt(prompt_text, input_text, cancelled, screen_rows)
        character(len=*), intent(in) :: prompt_text
        character(len=*), intent(out) :: input_text
        logical, intent(out) :: cancelled
        integer(int32), intent(in) :: screen_rows
        character(len=512) :: input_buffer
        integer :: input_pos, ch

        ! Initialize
        input_buffer = ''
        input_pos = 0
        cancelled = .false.

        ! Display prompt at bottom of screen
        call terminal_move_cursor(screen_rows, 1)
        call terminal_write(repeat(' ', 200))  ! Clear line
        call terminal_move_cursor(screen_rows, 1)
        call terminal_write(trim(prompt_text))
        call terminal_show_cursor()

        ! Input loop
        do
            ch = terminal_read_char()

            if (ch == -1) then
                ! No input, continue
                cycle
            else if (ch == 27) then  ! ESC
                ! Cancel
                cancelled = .true.
                input_text = ''
                exit
            else if (ch == 10 .or. ch == 13) then  ! Enter
                ! Accept input
                input_text = input_buffer(1:input_pos)
                exit
            else if (ch == 127 .or. ch == 8) then  ! Backspace/Delete
                if (input_pos > 0) then
                    input_pos = input_pos - 1
                    ! Redraw line
                    call terminal_move_cursor(screen_rows, 1)
                    call terminal_write(repeat(' ', 200))
                    call terminal_move_cursor(screen_rows, 1)
                    call terminal_write(trim(prompt_text) // input_buffer(1:input_pos))
                end if
            else if (ch >= 32 .and. ch <= 126) then  ! Printable characters
                if (input_pos < len(input_buffer)) then
                    input_pos = input_pos + 1
                    input_buffer(input_pos:input_pos) = achar(ch)
                    ! Write character
                    call terminal_write(achar(ch))
                end if
            end if
        end do

        call terminal_hide_cursor()
    end subroutine show_text_prompt

end module text_prompt_module
