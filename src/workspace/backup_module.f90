! Backup system module
! Handles auto-backup of modified buffers and restoration
! Backups stored relative to the file's parent directory.
! Global registry at ~/.config/fac/backup-registry.json.

module backup_module
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: backup_create, backup_detect, backup_list
    public :: backup_restore, backup_delete
    public :: backup_prompt_restore, backup_info_t
    public :: backup_migrate_legacy

    integer, parameter :: MAX_PATH_LEN = 512
    integer, parameter :: MAX_REGISTRY = 200

    type :: backup_info_t
        character(len=MAX_PATH_LEN) :: original_file = ""
        character(len=MAX_PATH_LEN) :: backup_file = ""
        character(len=MAX_PATH_LEN) :: timestamp = ""
        integer(int64) :: file_size = 0
    end type backup_info_t

contains

    ! ================================================================
    ! Global registry helpers
    ! ================================================================

    !> Get the global registry file path
    subroutine get_registry_path(path)
        use config_module, only: get_config_dir, ensure_config_dir
        character(len=MAX_PATH_LEN), intent(out) :: path
        character(len=:), allocatable :: config_dir
        logical :: ok

        call ensure_config_dir(ok)
        call get_config_dir(config_dir)
        if (allocated(config_dir)) then
            path = trim(config_dir) // '/backup-registry.json'
            deallocate(config_dir)
        else
            path = '/tmp/fac-backup-registry.json'
        end if
    end subroutine get_registry_path

    !> Load the global registry (validates entries exist on disk)
    subroutine registry_load(entries, count)
        type(backup_info_t), intent(out) :: entries(MAX_REGISTRY)
        integer, intent(out) :: count
        character(len=MAX_PATH_LEN) :: reg_path, line
        integer :: unit, ios, probe_unit

        count = 0
        call get_registry_path(reg_path)

        open(newunit=unit, file=trim(reg_path), &
             status='old', iostat=ios)
        if (ios /= 0) return

        ! Parse line-by-line: look for original_file, then
        ! backup_file and timestamp on subsequent lines
        do while (count < MAX_REGISTRY)
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, '"original_file":') > 0) then
                count = count + 1
                call extract_json_value(line, &
                    entries(count)%original_file)

                ! Read backup_file line
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                call extract_json_value(line, &
                    entries(count)%backup_file)

                ! Read timestamp line
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                call extract_json_value(line, &
                    entries(count)%timestamp)

                ! Validate: backup file must exist on disk
                open(newunit=probe_unit, &
                     file=trim(entries(count)%backup_file), &
                     status='old', iostat=ios)
                if (ios /= 0) then
                    ! Backup file gone — skip this entry
                    count = count - 1
                else
                    close(probe_unit)
                end if
            end if
        end do

        close(unit)
    end subroutine registry_load

    !> Save the global registry atomically (rewrite, not append)
    subroutine registry_save(entries, count)
        type(backup_info_t), intent(in) :: entries(MAX_REGISTRY)
        integer, intent(in) :: count
        character(len=MAX_PATH_LEN) :: reg_path, tmp_path
        integer :: unit, ios, i

        call get_registry_path(reg_path)
        tmp_path = trim(reg_path) // '.tmp'

        open(newunit=unit, file=trim(tmp_path), &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) return

        write(unit, '(A)') '{'
        write(unit, '(A)') '  "backups": ['

        do i = 1, count
            write(unit, '(A)') '    {'
            write(unit, '(A)') '      "original_file": "' // &
                trim(entries(i)%original_file) // '",'
            write(unit, '(A)') '      "backup_file": "' // &
                trim(entries(i)%backup_file) // '",'
            write(unit, '(A)') '      "timestamp": "' // &
                trim(entries(i)%timestamp) // '"'
            if (i < count) then
                write(unit, '(A)') '    },'
            else
                write(unit, '(A)') '    }'
            end if
        end do

        write(unit, '(A)') '  ]'
        write(unit, '(A)') '}'
        close(unit)

        ! Atomic rename
        call execute_command_line( &
            "mv '" // trim(tmp_path) // "' '" // &
            trim(reg_path) // "'", wait=.true.)
    end subroutine registry_save

    !> Add an entry to the global registry (replaces existing
    !> entry for same original_file, deleting old backup)
    subroutine registry_add(original_file, backup_file, timestamp)
        character(len=*), intent(in) :: original_file
        character(len=*), intent(in) :: backup_file, timestamp
        type(backup_info_t) :: entries(MAX_REGISTRY)
        integer :: count, i
        logical :: replaced

        call registry_load(entries, count)

        ! Replace existing entry for same file
        replaced = .false.
        do i = 1, count
            if (trim(entries(i)%original_file) == &
                trim(original_file)) then
                ! Delete old backup file from disk
                call delete_file(entries(i)%backup_file)
                entries(i)%backup_file = backup_file
                entries(i)%timestamp = timestamp
                replaced = .true.
                exit
            end if
        end do

        ! Add new entry if not replaced
        if (.not. replaced .and. count < MAX_REGISTRY) then
            count = count + 1
            entries(count)%original_file = original_file
            entries(count)%backup_file = backup_file
            entries(count)%timestamp = timestamp
        end if

        call registry_save(entries, count)
    end subroutine registry_add

    !> Remove an entry from the global registry by backup_file
    subroutine registry_remove(backup_file)
        character(len=*), intent(in) :: backup_file
        type(backup_info_t) :: entries(MAX_REGISTRY)
        integer :: count, i, j

        call registry_load(entries, count)

        ! Find and remove
        do i = 1, count
            if (trim(entries(i)%backup_file) == &
                trim(backup_file)) then
                ! Shift remaining entries down
                do j = i, count - 1
                    entries(j) = entries(j + 1)
                end do
                count = count - 1
                exit
            end if
        end do

        call registry_save(entries, count)
    end subroutine registry_remove

    ! ================================================================
    ! Backup file location
    ! ================================================================

    !> Get backup directory for a file (sibling .fac/backups/)
    subroutine backup_get_dir_for_file(file_path, backup_dir)
        character(len=*), intent(in) :: file_path
        character(len=MAX_PATH_LEN), intent(out) :: backup_dir
        integer :: last_slash

        last_slash = index(file_path, '/', back=.true.)
        if (last_slash > 1) then
            backup_dir = file_path(1:last_slash-1) // &
                '/.fac/backups'
        else if (last_slash == 1) then
            backup_dir = '/.fac/backups'
        else
            backup_dir = '.fac/backups'
        end if
    end subroutine backup_get_dir_for_file

    ! ================================================================
    ! Public API
    ! ================================================================

    !> Create backup of in-memory buffer content
    subroutine backup_create(file_path, buffer_content, success)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: buffer_content
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: backup_dir, backup_file
        character(len=MAX_PATH_LEN) :: basename, timestamp_str
        integer :: unit_dst, ios, i

        success = .false.

        ! Determine backup directory from file location
        call backup_get_dir_for_file(file_path, backup_dir)
        call execute_command_line( &
            "mkdir -p '" // trim(backup_dir) // "'", &
            wait=.true.)

        ! Extract basename
        basename = file_path
        do i = len_trim(file_path), 1, -1
            if (file_path(i:i) == '/') then
                basename = file_path(i+1:)
                exit
            end if
        end do

        ! Generate backup filename
        call get_timestamp(timestamp_str)
        write(backup_file, '(A,A,A,A,A)') &
            trim(backup_dir), '/', trim(basename), '.', &
            trim(timestamp_str)

        ! Write buffer content to backup file
        open(newunit=unit_dst, file=trim(backup_file), &
             status='replace', action='write', &
             access='stream', form='unformatted', &
             iostat=ios)
        if (ios /= 0) return
        if (len(buffer_content) > 0) then
            write(unit_dst, iostat=ios) buffer_content
        end if
        close(unit_dst)

        ! Register in global registry
        call registry_add(file_path, trim(backup_file), &
            trim(timestamp_str))

        success = .true.
    end subroutine backup_create

    !> Detect if any backups exist (optionally filtered by
    !> workspace prefix)
    function backup_detect(workspace_path) result(has_backups)
        character(len=*), intent(in) :: workspace_path
        logical :: has_backups
        type(backup_info_t) :: entries(MAX_REGISTRY)
        integer :: count, i

        has_backups = .false.
        call registry_load(entries, count)

        if (len_trim(workspace_path) == 0) then
            has_backups = (count > 0)
            return
        end if

        ! Check if any backup's original_file is within workspace
        do i = 1, count
            if (index(trim(entries(i)%original_file), &
                trim(workspace_path)) == 1) then
                has_backups = .true.
                return
            end if
        end do

        ! Also check for any backups at all (for single-file mode)
        has_backups = (count > 0)
    end function backup_detect

    !> List backups (optionally filtered by workspace prefix)
    subroutine backup_list(workspace_path, backups, count)
        character(len=*), intent(in) :: workspace_path
        type(backup_info_t), allocatable, intent(out) :: backups(:)
        integer, intent(out) :: count
        type(backup_info_t) :: entries(MAX_REGISTRY)
        integer :: total, i

        call registry_load(entries, total)

        allocate(backups(total))
        count = 0

        do i = 1, total
            ! Include all if no workspace filter, or if file
            ! path starts with workspace path
            if (len_trim(workspace_path) == 0 .or. &
                index(trim(entries(i)%original_file), &
                    trim(workspace_path)) == 1) then
                count = count + 1
                backups(count) = entries(i)
            end if
        end do
    end subroutine backup_list

    !> Restore a backup file to its original location
    subroutine backup_restore(backup_file, original_file, success)
        character(len=*), intent(in) :: backup_file, original_file
        logical, intent(out) :: success
        integer :: unit_src, unit_dst, ios
        character(len=1024) :: line

        success = .false.

        open(newunit=unit_src, file=trim(backup_file), &
             status='old', action='read', iostat=ios)
        if (ios /= 0) return

        open(newunit=unit_dst, file=trim(original_file), &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            close(unit_src)
            return
        end if

        do
            read(unit_src, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(unit_dst, '(A)') trim(line)
        end do

        close(unit_src)
        close(unit_dst)

        call backup_delete(backup_file)
        success = .true.
    end subroutine backup_restore

    !> Delete a backup file and remove from registry
    subroutine backup_delete(backup_file)
        character(len=*), intent(in) :: backup_file

        ! Delete file from disk
        call delete_file(backup_file)

        ! Remove from global registry
        call registry_remove(backup_file)
    end subroutine backup_delete

    !> Prompt user to restore backup
    function backup_prompt_restore(original_file, &
        current_backup, total_backups, backup_timestamp) &
        result(choice)
        use terminal_io_module, only: terminal_write, &
            terminal_move_cursor, terminal_clear_screen, &
            terminal_flush
        use input_handler_module, only: get_key_input
        character(len=*), intent(in) :: original_file
        integer, intent(in), optional :: current_backup
        integer, intent(in), optional :: total_backups
        character(len=*), intent(in), optional :: &
            backup_timestamp
        character :: choice
        character(len=32) :: key_input
        character(len=512) :: prompt
        integer :: status

        call terminal_clear_screen()
        call terminal_move_cursor(2, 1)

        if (present(current_backup) .and. &
            present(total_backups)) then
            if (present(backup_timestamp)) then
                write(prompt, '(A,I0,A,I0,A,A,A,A,A)') &
                    'Backup found [', current_backup, &
                    ' of ', total_backups, ']: ', &
                    trim(original_file), ' (from ', &
                    trim(backup_timestamp), ')'
            else
                write(prompt, '(A,I0,A,I0,A,A)') &
                    'Backup found [', current_backup, &
                    ' of ', total_backups, ']: ', &
                    trim(original_file)
            end if
        else
            write(prompt, '(A,A)') 'Backup found for: ', &
                trim(original_file)
        end if
        call terminal_write(trim(prompt))

        call terminal_move_cursor(4, 1)
        call terminal_write( &
            '[r]estore - Use backup (replaces current file)')
        call terminal_move_cursor(5, 1)
        call terminal_write( &
            '[d]elete  - Delete backup, keep current file')
        call terminal_move_cursor(6, 1)
        call terminal_write( &
            '[c]ompare - Show differences')

        call terminal_move_cursor(8, 1)
        call terminal_write('Choice [d]: ')
        call terminal_flush()

        do
            call get_key_input(key_input, status)
            if (status == 0) then
                if (key_input == 'r' .or. key_input == 'R') then
                    choice = 'r'; exit
                else if (key_input == 'd' .or. &
                         key_input == 'D') then
                    choice = 'd'; exit
                else if (key_input == 'c' .or. &
                         key_input == 'C') then
                    choice = 'c'; exit
                else if (key_input == 'enter') then
                    choice = 'd'; exit
                else if (key_input == 'esc') then
                    choice = 'd'; exit
                end if
            end if
        end do
    end function backup_prompt_restore

    !> Migrate legacy per-workspace backups to global registry
    subroutine backup_migrate_legacy(workspace_path)
        character(len=*), intent(in) :: workspace_path
        character(len=MAX_PATH_LEN) :: metadata_file, line
        integer :: unit, ios, count
        type(backup_info_t) :: entry

        metadata_file = trim(workspace_path) // &
            '/.fac/backups/.backup-metadata.json'

        open(newunit=unit, file=trim(metadata_file), &
             status='old', iostat=ios)
        if (ios /= 0) return  ! No legacy metadata

        count = 0
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, '"original_file":') > 0) then
                call extract_json_value(line, &
                    entry%original_file)
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                call extract_json_value(line, &
                    entry%backup_file)
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                call extract_json_value(line, &
                    entry%timestamp)

                ! Register in global registry
                call registry_add( &
                    trim(entry%original_file), &
                    trim(entry%backup_file), &
                    trim(entry%timestamp))
                count = count + 1
            end if
        end do

        close(unit)

        ! Delete legacy metadata file
        if (count > 0) then
            open(newunit=unit, file=trim(metadata_file), &
                 status='old', iostat=ios)
            if (ios == 0) close(unit, status='delete')
        end if
    end subroutine backup_migrate_legacy

    ! ================================================================
    ! Internal helpers
    ! ================================================================

    !> Get current timestamp string (Unix epoch)
    subroutine get_timestamp(timestamp_str)
        character(len=*), intent(out) :: timestamp_str
        integer :: values(8)
        integer(int64) :: epoch

        call date_and_time(values=values)
        epoch = int(values(1) - 1970, int64) * &
                365_int64 * 86400_int64 + &
                int(values(2), int64) * &
                30_int64 * 86400_int64 + &
                int(values(3), int64) * 86400_int64 + &
                int(values(5), int64) * 3600_int64 + &
                int(values(6), int64) * 60_int64 + &
                int(values(7), int64)
        write(timestamp_str, '(I0)') epoch
    end subroutine get_timestamp

    !> Extract value from a JSON line like: "key": "value"
    subroutine extract_json_value(line, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: value
        integer :: colon_pos, q1, q2

        value = ''
        colon_pos = index(line, ':')
        if (colon_pos == 0) return

        ! Find opening quote after colon
        q1 = index(line(colon_pos+1:), '"')
        if (q1 == 0) return
        q1 = colon_pos + q1  ! absolute position

        ! Find closing quote
        q2 = index(line(q1+1:), '"')
        if (q2 == 0) return
        q2 = q1 + q2  ! absolute position

        if (q2 > q1 + 1) then
            value = line(q1+1:q2-1)
        end if
    end subroutine extract_json_value

    !> Delete a file from disk
    subroutine delete_file(file_path)
        character(len=*), intent(in) :: file_path
        integer :: unit, ios

        open(newunit=unit, file=trim(file_path), &
             status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete')
    end subroutine delete_file

end module backup_module
