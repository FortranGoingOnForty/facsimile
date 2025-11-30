module app_state_module
    use config_module, only: get_config_dir, ensure_config_dir
    implicit none
    private

    public :: app_state_t
    public :: app_state_load, app_state_save
    public :: is_first_run, mark_first_run_complete

    type :: app_state_t
        logical :: first_run_completed = .false.
        logical :: lsp_installer_seen = .false.
        character(len=16) :: version = '1.0'
    end type app_state_t

contains

    subroutine app_state_load(state)
        type(app_state_t), intent(out) :: state
        character(len=:), allocatable :: config_dir
        character(len=512) :: state_file
        character(len=1024) :: line
        integer :: unit_num, ios
        logical :: file_exists

        ! Initialize defaults
        state%first_run_completed = .false.
        state%lsp_installer_seen = .false.
        state%version = '1.0'

        ! Get config directory
        call get_config_dir(config_dir)
        if (.not. allocated(config_dir)) return
        state_file = trim(config_dir) // '/state.json'

        ! Check if file exists
        inquire(file=trim(state_file), exist=file_exists)
        if (.not. file_exists) return

        ! Read and parse JSON
        open(newunit=unit_num, file=trim(state_file), status='old', &
             action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            ! Parse simple JSON fields
            if (index(line, '"first_run_completed"') > 0) then
                if (index(line, 'true') > 0) then
                    state%first_run_completed = .true.
                else
                    state%first_run_completed = .false.
                end if
            else if (index(line, '"lsp_installer_seen"') > 0) then
                if (index(line, 'true') > 0) then
                    state%lsp_installer_seen = .true.
                else
                    state%lsp_installer_seen = .false.
                end if
            else if (index(line, '"version"') > 0) then
                call extract_json_string(line, 'version', state%version)
            end if
        end do

        close(unit_num)
    end subroutine app_state_load

    subroutine app_state_save(state)
        type(app_state_t), intent(in) :: state
        character(len=:), allocatable :: config_dir
        character(len=512) :: state_file
        integer :: unit_num, ios
        logical :: dir_success

        ! Ensure config directory exists
        call ensure_config_dir(dir_success)
        if (.not. dir_success) return

        ! Get config directory
        call get_config_dir(config_dir)
        if (.not. allocated(config_dir)) return
        state_file = trim(config_dir) // '/state.json'

        ! Write JSON
        open(newunit=unit_num, file=trim(state_file), status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) return

        write(unit_num, '(A)') '{'
        if (state%first_run_completed) then
            write(unit_num, '(A)') '  "first_run_completed": true,'
        else
            write(unit_num, '(A)') '  "first_run_completed": false,'
        end if
        if (state%lsp_installer_seen) then
            write(unit_num, '(A)') '  "lsp_installer_seen": true,'
        else
            write(unit_num, '(A)') '  "lsp_installer_seen": false,'
        end if
        write(unit_num, '(A)') '  "version": "' // trim(state%version) // '"'
        write(unit_num, '(A)') '}'

        close(unit_num)
    end subroutine app_state_save

    function is_first_run() result(first_run)
        logical :: first_run
        type(app_state_t) :: state

        call app_state_load(state)
        first_run = .not. state%first_run_completed
    end function is_first_run

    subroutine mark_first_run_complete()
        type(app_state_t) :: state

        call app_state_load(state)
        state%first_run_completed = .true.
        call app_state_save(state)
    end subroutine mark_first_run_complete

    subroutine extract_json_string(line, key, value)
        character(len=*), intent(in) :: line, key
        character(len=*), intent(out) :: value
        integer :: key_pos, colon_pos, quote1, quote2

        value = ''
        key_pos = index(line, '"' // trim(key) // '"')
        if (key_pos == 0) return

        colon_pos = index(line(key_pos:), ':')
        if (colon_pos == 0) return
        colon_pos = key_pos + colon_pos

        quote1 = index(line(colon_pos:), '"')
        if (quote1 == 0) return
        quote1 = colon_pos + quote1

        quote2 = index(line(quote1:), '"')
        if (quote2 == 0) return
        quote2 = quote1 + quote2 - 2

        if (quote2 >= quote1) then
            value = line(quote1:quote2)
        end if
    end subroutine extract_json_string

end module app_state_module
