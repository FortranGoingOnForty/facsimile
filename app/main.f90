program facsimile
    use iso_fortran_env, only: error_unit, input_unit, output_unit
    use terminal_io_module
    use input_handler_module, only: get_key_input
    use editor_state_module
    use text_buffer_module
    use renderer_module
    use command_handler_module
    implicit none

    type(editor_state_t) :: editor
    type(buffer_t) :: buffer
    character(len=32) :: key_input
    character(len=256) :: filename
    logical :: running, should_quit
    integer :: status, argc, rows, cols

    ! Get command line arguments
    argc = command_argument_count()
    if (argc > 0) then
        call get_command_argument(1, filename)
    else
        filename = ''
    end if

    ! Initialize editor
    call init_editor(editor)
    running = .true.

    ! Initialize terminal
    call terminal_init()
    call terminal_clear_screen()

    ! Get terminal size
    call terminal_get_size(rows, cols)
    editor%screen_rows = rows
    editor%screen_cols = cols

    ! Initialize renderer
    call init_renderer(rows, cols)

    ! Initialize command handler (for yank stack)
    call init_command_handler()

    ! Initialize buffer and load file if specified
    if (len_trim(filename) > 0) then
        call buffer_load_file(buffer, trim(filename), status)
        if (status == 0) then
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)
        else
            ! If file doesn't exist, create empty buffer for new file
            call init_buffer(buffer)
            allocate(character(len=len_trim(filename)) :: editor%filename)
            editor%filename = trim(filename)
        end if
    else
        call init_buffer(buffer)
    end if

    ! Initial render
    call render_screen(buffer, editor)

    ! Main event loop
    do while (running)
        ! Get input
        call get_key_input(key_input, status)

        if (status == 0) then
            ! Process input
            call handle_key_command(key_input, editor, buffer, should_quit)

            if (should_quit) then
                running = .false.
            else
                ! Re-render screen after each command
                call render_screen(buffer, editor)
            end if
        end if
    end do

    ! Save file if modified (optional prompt in future)
    if (buffer%modified .and. allocated(editor%filename)) then
        call buffer_save_file(buffer, editor%filename, status)
    end if

    ! Cleanup
    call cleanup_renderer()
    call cleanup_command_handler()
    call terminal_cleanup()
    call cleanup_editor(editor)
    call cleanup_buffer(buffer)

end program facsimile