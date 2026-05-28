! Backup system module
! Handles auto-backup of modified buffers and restoration

module backup_module
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: backup_create, backup_detect, backup_list, backup_restore
    public :: backup_delete, backup_prompt_restore, backup_info_t

    integer, parameter :: MAX_PATH_LEN = 512
    integer, parameter :: MAX_BACKUPS = 100

    type :: backup_info_t
        character(len=MAX_PATH_LEN) :: original_file = ""
        character(len=MAX_PATH_LEN) :: backup_file = ""
        character(len=MAX_PATH_LEN) :: timestamp = ""
        integer(int64) :: file_size = 0
    end type backup_info_t

contains

    !> Create backup of a file
    subroutine backup_create(workspace_path, file_path, success)
        character(len=*), intent(in) :: workspace_path, file_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: backup_dir, backup_file, basename
        character(len=MAX_PATH_LEN) :: src_file, timestamp_str
        integer :: unit_src, unit_dst, ios, i
        character(len=1024) :: line

        success = .false.

        ! Create backup directory if needed
        backup_dir = trim(workspace_path) // "/.fac/backups"
        call execute_command_line("mkdir -p '" // trim(backup_dir) // "'", wait=.true.)

        ! Extract basename from file_path
        basename = file_path
        do i = len_trim(file_path), 1, -1
            if (file_path(i:i) == '/') then
                basename = file_path(i+1:)
                exit
            end if
        end do

        ! Generate timestamp-based backup filename
        call get_timestamp(timestamp_str)
        write(backup_file, '(A,A,A,A,A)') trim(backup_dir), '/', trim(basename), '.', trim(timestamp_str)

        ! Copy file to backup
        src_file = file_path
        open(newunit=unit_src, file=src_file, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        open(newunit=unit_dst, file=backup_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            close(unit_src)
            return
        end if

        ! Copy line by line
        do
            read(unit_src, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(unit_dst, '(A)') trim(line)
        end do

        close(unit_src)
        close(unit_dst)

        ! Update metadata
        call backup_update_metadata(workspace_path, file_path, backup_file, timestamp_str)

        success = .true.
    end subroutine backup_create

    !> Detect if backups exist for workspace
    function backup_detect(workspace_path) result(has_backups)
        character(len=*), intent(in) :: workspace_path
        logical :: has_backups
        character(len=MAX_PATH_LEN) :: metadata_file
        integer :: unit, ios

        has_backups = .false.
        metadata_file = trim(workspace_path) // "/.fac/backups/.backup-metadata.json"

        open(newunit=unit, file=metadata_file, status='old', iostat=ios)
        if (ios == 0) then
            close(unit)
            has_backups = .true.
        end if
    end function backup_detect

    !> List all backups for workspace
    subroutine backup_list(workspace_path, backups, count)
        character(len=*), intent(in) :: workspace_path
        type(backup_info_t), allocatable, intent(out) :: backups(:)
        integer, intent(out) :: count
        character(len=MAX_PATH_LEN) :: metadata_file, line
        integer :: unit, ios

        count = 0
        allocate(backups(MAX_BACKUPS))

        metadata_file = trim(workspace_path) // "/.fac/backups/.backup-metadata.json"

        open(newunit=unit, file=metadata_file, status='old', iostat=ios)
        if (ios /= 0) return

        ! Simple JSON parsing (assumes one backup per "original_file" line)
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(line, '"original_file":') > 0) then
                count = count + 1
                if (count > MAX_BACKUPS) exit
                call parse_backup_entry(line, backups(count))
            end if
        end do

        close(unit)
    end subroutine backup_list

    !> Restore a backup file
    subroutine backup_restore(backup_file, original_file, success)
        character(len=*), intent(in) :: backup_file, original_file
        logical, intent(out) :: success
        integer :: unit_src, unit_dst, ios
        character(len=1024) :: line

        success = .false.

        ! Copy backup to original location using native Fortran I/O
        ! (avoids execute_command_line issues with flang)
        open(newunit=unit_src, file=trim(backup_file), status='old', action='read', iostat=ios)
        if (ios /= 0) return

        open(newunit=unit_dst, file=trim(original_file), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            close(unit_src)
            return
        end if

        ! Copy line by line
        do
            read(unit_src, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(unit_dst, '(A)') trim(line)
        end do

        close(unit_src)
        close(unit_dst)

        ! Delete the backup after successful restore
        call backup_delete(backup_file)
        success = .true.
    end subroutine backup_restore

    !> Delete a backup file
    subroutine backup_delete(backup_file)
        character(len=*), intent(in) :: backup_file
        integer :: unit, ios

        ! Delete using Fortran file operations (avoids execute_command_line issues)
        open(newunit=unit, file=trim(backup_file), status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine backup_delete

    !> Prompt user to restore backup (returns 'r', 'd' (delete), or 'c' (compare))
    function backup_prompt_restore(original_file, current_backup, total_backups, backup_timestamp) result(choice)
        use terminal_io_module, only: terminal_write, terminal_move_cursor, terminal_clear_screen, terminal_flush
        use input_handler_module, only: get_key_input
        character(len=*), intent(in) :: original_file
        integer, intent(in), optional :: current_backup, total_backups
        character(len=*), intent(in), optional :: backup_timestamp
        character :: choice
        character(len=32) :: key_input
        character(len=512) :: prompt
        integer :: status

        ! Clear screen and show prompt
        call terminal_clear_screen()
        call terminal_move_cursor(2, 1)

        ! Build prompt with progress and timestamp if available
        if (present(current_backup) .and. present(total_backups)) then
            if (present(backup_timestamp)) then
                write(prompt, '(A,I0,A,I0,A,A,A,A,A)') 'Backup found [', current_backup, &
                    ' of ', total_backups, ']: ', trim(original_file), ' (from ', trim(backup_timestamp), ')'
            else
                write(prompt, '(A,I0,A,I0,A,A)') 'Backup found [', current_backup, &
                    ' of ', total_backups, ']: ', trim(original_file)
            end if
        else
            if (present(backup_timestamp)) then
                write(prompt, '(A,A,A,A,A)') 'Backup found for: ', trim(original_file), &
                    ' (from ', trim(backup_timestamp), ')'
            else
                write(prompt, '(A,A)') 'Backup found for: ', trim(original_file)
            end if
        end if
        call terminal_write(trim(prompt))

        call terminal_move_cursor(4, 1)
        call terminal_write('[r]estore - Use backup (current file will be replaced)')

        call terminal_move_cursor(5, 1)
        call terminal_write('[d]elete  - Delete backup and keep current file')

        call terminal_move_cursor(6, 1)
        call terminal_write('[c]ompare - Show differences between files')

        call terminal_move_cursor(8, 1)
        call terminal_write('Choice [d]: ')
        call terminal_flush()

        ! Get user input (Enter defaults to delete)
        do
            call get_key_input(key_input, status)
            if (status == 0) then
                if (key_input == 'r' .or. key_input == 'R') then
                    choice = 'r'
                    exit
                else if (key_input == 'd' .or. key_input == 'D') then
                    choice = 'd'
                    exit
                else if (key_input == 'c' .or. key_input == 'C') then
                    choice = 'c'
                    exit
                else if (key_input == 'enter') then
                    choice = 'd'  ! Enter = delete (default)
                    exit
                else if (key_input == 'esc' .or. key_input == 'ESCAPE') then
                    choice = 'd'
                    exit
                end if
            end if
        end do
    end function backup_prompt_restore

    !> Get current timestamp string (Unix epoch)
    subroutine get_timestamp(timestamp_str)
        character(len=*), intent(out) :: timestamp_str
        integer :: values(8)
        integer(int64) :: epoch

        ! Get current time
        call date_and_time(values=values)

        ! Simple epoch calculation (approximate)
        epoch = int(values(1) - 1970, int64) * 365_int64 * 86400_int64 + &
                int(values(2), int64) * 30_int64 * 86400_int64 + &
                int(values(3), int64) * 86400_int64 + &
                int(values(5), int64) * 3600_int64 + &
                int(values(6), int64) * 60_int64 + &
                int(values(7), int64)

        write(timestamp_str, '(I0)') epoch
    end subroutine get_timestamp

    !> Update backup metadata JSON
    subroutine backup_update_metadata(workspace_path, original_file, backup_file, timestamp)
        character(len=*), intent(in) :: workspace_path, original_file, backup_file, timestamp
        character(len=MAX_PATH_LEN) :: metadata_file
        integer :: unit, ios

        metadata_file = trim(workspace_path) // "/.fac/backups/.backup-metadata.json"

        ! Simple append for now (not parsing existing JSON)
        open(newunit=unit, file=metadata_file, status='old', position='append', iostat=ios)
        if (ios /= 0) then
            ! Create new file
            open(newunit=unit, file=metadata_file, status='replace')
            write(unit, '(A)') '{'
            write(unit, '(A)') '  "backups": ['
        end if

        ! Write backup entry (simplified JSON)
        write(unit, '(A)') '    {'
        write(unit, '(A)') '      "original_file": "' // trim(original_file) // '",'
        write(unit, '(A)') '      "backup_file": "' // trim(backup_file) // '",'
        write(unit, '(A)') '      "timestamp": "' // trim(timestamp) // '"'
        write(unit, '(A)') '    }'

        close(unit)
    end subroutine backup_update_metadata

    !> Parse a backup entry from JSON line (simplified)
    subroutine parse_backup_entry(line, backup)
        character(len=*), intent(in) :: line
        type(backup_info_t), intent(out) :: backup
        integer :: quote1, quote2

        ! Extract original_file (very simple parsing)
        quote1 = index(line, '"', .true.)
        if (quote1 > 0) then
            quote2 = index(line(1:quote1-1), '"', .true.)
            if (quote2 > 0) then
                backup%original_file = line(quote2+1:quote1-1)
            end if
        end if
    end subroutine parse_backup_entry

end module backup_module
