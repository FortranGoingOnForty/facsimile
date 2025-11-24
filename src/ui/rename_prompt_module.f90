module rename_prompt_module
    use iso_fortran_env, only: int32
    use text_prompt_module, only: show_text_prompt
    implicit none
    private

    public :: show_rename_prompt

contains

    subroutine show_rename_prompt(screen_rows, old_name, new_name, cancelled)
        integer(int32), intent(in) :: screen_rows
        character(len=*), intent(in) :: old_name
        character(len=:), allocatable, intent(out) :: new_name
        logical, intent(out) :: cancelled

        character(len=256) :: prompt_text
        character(len=512) :: input_text

        ! Create prompt message with old name
        write(prompt_text, '(A)') "Rename '" // trim(old_name) // "' to: "

        ! Show the text prompt
        call show_text_prompt(trim(prompt_text), input_text, cancelled, screen_rows)

        ! Debug logging
        block
            integer :: debug_unit
            open(newunit=debug_unit, file='/tmp/fac_keys.log', position='append', action='write')
            write(debug_unit, '(A)') '>>> RENAME PROMPT RETURNED <<<'
            write(debug_unit, '(A,L1)') 'Cancelled: ', cancelled
            write(debug_unit, '(A)') 'Input text: "' // trim(input_text) // '"'
            write(debug_unit, '(A)') 'Old name: "' // trim(old_name) // '"'
            write(debug_unit, '(A,I0)') 'Input length: ', len_trim(input_text)
            close(debug_unit)
        end block

        if (.not. cancelled) then
            if (len_trim(input_text) > 0 .and. trim(input_text) /= trim(old_name)) then
                allocate(character(len=len_trim(input_text)) :: new_name)
                new_name = trim(input_text)
            else
                cancelled = .true.
            end if
        end if

        ! Debug logging after check
        block
            integer :: debug_unit
            open(newunit=debug_unit, file='/tmp/fac_keys.log', position='append', action='write')
            write(debug_unit, '(A,L1)') 'Final cancelled: ', cancelled
            if (allocated(new_name)) then
                write(debug_unit, '(A)') 'New name allocated: "' // trim(new_name) // '"'
            else
                write(debug_unit, '(A)') 'New name NOT allocated'
            end if
            close(debug_unit)
        end block
    end subroutine show_rename_prompt

end module rename_prompt_module
