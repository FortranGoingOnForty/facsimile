! Recents management module
! Handles recents.json read/write and recent workspace tracking

module recents_module
    use iso_fortran_env, only: int32
    use config_module, only: get_config_dir, ensure_config_dir
    implicit none
    private

    public :: recent_t, recents_load, recents_save, recents_add_or_update
    public :: recents_exists, recents_get_path, recents_remove

    integer, parameter :: MAX_PATH_LEN = 512
    integer, parameter :: MAX_LABEL_LEN = 128
    integer, parameter :: DEFAULT_MAX_RECENTS = 20

    type :: recent_t
        character(len=MAX_PATH_LEN) :: path = ""
        character(len=MAX_LABEL_LEN) :: label = ""
        character(len=32) :: last_opened = ""  ! ISO 8601 timestamp
        integer :: open_count = 0
    end type recent_t

contains

    !> Get path to recents.json
    subroutine recents_get_path(recents_file)
        character(len=:), allocatable, intent(out) :: recents_file
        character(len=:), allocatable :: config_dir

        call get_config_dir(config_dir)
        recents_file = trim(config_dir) // '/recents.json'
    end subroutine recents_get_path

    !> Check if recents.json exists
    function recents_exists() result(exists)
        logical :: exists
        character(len=:), allocatable :: recents_file
        integer :: unit, ios

        call recents_get_path(recents_file)
        open(newunit=unit, file=recents_file, status='old', iostat=ios)
        exists = (ios == 0)
        if (exists) close(unit)
    end function recents_exists

    !> Load recents from recents.json
    subroutine recents_load(recents, count, max_recents, success)
        type(recent_t), allocatable, intent(out) :: recents(:)
        integer, intent(out) :: count, max_recents
        logical, intent(out) :: success
        character(len=:), allocatable :: recents_file
        character(len=1024) :: line
        integer :: unit, ios, i
        logical :: in_recents, reading_entry

        success = .false.
        count = 0
        max_recents = DEFAULT_MAX_RECENTS
        allocate(recents(max_recents))

        ! Ensure config directory exists
        call ensure_config_dir(success)
        if (.not. success) return

        call recents_get_path(recents_file)

        ! Open file
        open(newunit=unit, file=recents_file, status='old', iostat=ios)
        if (ios /= 0) then
            ! File doesn't exist - return empty list
            success = .true.
            count = 0
            return
        end if

        ! Simple JSON parsing
        in_recents = .false.
        reading_entry = .false.
        i = 0

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = adjustl(line)

            ! Check for max_recents setting
            if (index(line, '"max_recents"') > 0) then
                call extract_json_int(line, max_recents)
                cycle
            end if

            ! Check if we're in the recents array
            if (index(line, '"recents"') > 0) then
                in_recents = .true.
                cycle
            end if

            if (.not. in_recents) cycle

            ! Look for start of entry
            if (index(line, '{') > 0 .and. .not. reading_entry) then
                reading_entry = .true.
                i = i + 1
                if (i > max_recents) exit
                ! Initialize entry
                recents(i)%path = ""
                recents(i)%label = ""
                recents(i)%last_opened = ""
                recents(i)%open_count = 0
                cycle
            end if

            if (reading_entry) then
                ! Parse fields
                if (index(line, '"path"') > 0) then
                    call extract_json_string(line, recents(i)%path)
                else if (index(line, '"label"') > 0) then
                    call extract_json_string(line, recents(i)%label)
                else if (index(line, '"last_opened"') > 0) then
                    call extract_json_string(line, recents(i)%last_opened)
                else if (index(line, '"open_count"') > 0) then
                    call extract_json_int(line, recents(i)%open_count)
                end if

                ! Check for end of entry
                if (index(line, '}') > 0) then
                    reading_entry = .false.
                end if
            end if
        end do

        close(unit)
        count = i
        success = .true.
    end subroutine recents_load

    !> Save recents to recents.json
    subroutine recents_save(recents, count, max_recents, success)
        type(recent_t), intent(in) :: recents(:)
        integer, intent(in) :: count, max_recents
        logical, intent(out) :: success
        character(len=:), allocatable :: recents_file
        integer :: unit, ios, i

        success = .false.

        ! Ensure config directory exists
        call ensure_config_dir(success)
        if (.not. success) return

        call recents_get_path(recents_file)

        ! Write JSON file
        open(newunit=unit, file=recents_file, status='replace', iostat=ios)
        if (ios /= 0) return

        write(unit, '(A)') '{'
        write(unit, '(A,I0,A)') '  "version": "1.0",'
        write(unit, '(A,I0,A)') '  "max_recents": ', max_recents, ','
        write(unit, '(A)') '  "recents": ['

        do i = 1, count
            write(unit, '(A)') '    {'
            write(unit, '(A)') '      "path": "' // trim(recents(i)%path) // '",'
            write(unit, '(A)') '      "label": "' // trim(recents(i)%label) // '",'
            write(unit, '(A)') '      "last_opened": "' // trim(recents(i)%last_opened) // '",'
            write(unit, '(A,I0)') '      "open_count": ', recents(i)%open_count

            if (i < count) then
                write(unit, '(A)') '    },'
            else
                write(unit, '(A)') '    }'
            end if
        end do

        write(unit, '(A)') '  ]'
        write(unit, '(A)') '}'

        close(unit)
        success = .true.
    end subroutine recents_save

    !> Add or update a recent workspace
    subroutine recents_add_or_update(path, label, success)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        logical, intent(out) :: success
        type(recent_t), allocatable :: recents(:), sorted(:)
        integer :: count, max_recents, i, found_index
        character(len=32) :: timestamp

        success = .false.

        ! Load existing recents
        call recents_load(recents, count, max_recents, success)
        if (.not. success) return

        call get_current_timestamp(timestamp)

        ! Check if path already exists
        found_index = 0
        do i = 1, count
            if (trim(recents(i)%path) == trim(path)) then
                found_index = i
                exit
            end if
        end do

        if (found_index > 0) then
            ! Update existing entry
            recents(found_index)%last_opened = trim(timestamp)
            recents(found_index)%open_count = recents(found_index)%open_count + 1
        else
            ! Add new entry
            if (count >= max_recents) then
                ! Remove oldest entry (last in sorted list)
                count = count - 1
            end if

            count = count + 1
            recents(count)%path = trim(path)
            recents(count)%label = trim(label)
            recents(count)%last_opened = trim(timestamp)
            recents(count)%open_count = 1
        end if

        ! Sort by last_opened (most recent first)
        allocate(sorted(max_recents))
        call sort_recents_by_time(recents, count, sorted)

        ! Save
        call recents_save(sorted, count, max_recents, success)
        deallocate(sorted)
    end subroutine recents_add_or_update

    !> Sort recents by last_opened (most recent first)
    subroutine sort_recents_by_time(recents, count, sorted)
        type(recent_t), intent(in) :: recents(:)
        integer, intent(in) :: count
        type(recent_t), intent(out) :: sorted(:)
        integer :: i, j, max_idx
        character(len=32) :: max_time
        logical :: used(count)

        used = .false.

        ! Simple selection sort
        do i = 1, count
            max_time = ""
            max_idx = 0

            ! Find most recent unused entry
            do j = 1, count
                if (.not. used(j)) then
                    if (len_trim(max_time) == 0 .or. recents(j)%last_opened > max_time) then
                        max_time = recents(j)%last_opened
                        max_idx = j
                    end if
                end if
            end do

            if (max_idx > 0) then
                sorted(i) = recents(max_idx)
                used(max_idx) = .true.
            end if
        end do
    end subroutine sort_recents_by_time

    !> Extract string value from JSON line
    subroutine extract_json_string(line, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: value
        integer :: start_quote, end_quote, colon_pos

        value = ""

        colon_pos = index(line, ':')
        if (colon_pos == 0) return

        start_quote = index(line(colon_pos:), '"')
        if (start_quote == 0) return
        start_quote = start_quote + colon_pos

        end_quote = index(line(start_quote+1:), '"')
        if (end_quote == 0) return
        end_quote = end_quote + start_quote

        if (end_quote > start_quote) then
            value = line(start_quote+1:end_quote-1)
        end if
    end subroutine extract_json_string

    !> Extract integer value from JSON line
    subroutine extract_json_int(line, value)
        character(len=*), intent(in) :: line
        integer, intent(out) :: value
        character(len=32) :: num_str
        integer :: colon_pos, comma_pos, ios

        value = 0

        colon_pos = index(line, ':')
        if (colon_pos == 0) return

        comma_pos = index(line(colon_pos:), ',')
        if (comma_pos == 0) comma_pos = len_trim(line) - colon_pos + 1

        num_str = adjustl(line(colon_pos+1:colon_pos+comma_pos-1))
        read(num_str, *, iostat=ios) value
        if (ios /= 0) value = 0
    end subroutine extract_json_int

    !> Get current timestamp in ISO 8601 format
    subroutine get_current_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)

        write(timestamp, '(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7), 'Z'
    end subroutine get_current_timestamp

    !> Remove a recent workspace by index (Phase 7: handle deleted workspaces)
    subroutine recents_remove(index, success)
        integer, intent(in) :: index
        logical, intent(out) :: success
        type(recent_t), allocatable :: recents(:)
        integer :: count, max_recents, i

        success = .false.

        ! Load existing recents
        call recents_load(recents, count, max_recents, success)
        if (.not. success) return

        ! Check bounds
        if (index < 1 .or. index > count) then
            success = .false.
            return
        end if

        ! Shift entries after the removed one
        do i = index, count - 1
            recents(i) = recents(i + 1)
        end do
        count = count - 1

        ! Save updated list
        call recents_save(recents, count, max_recents, success)
    end subroutine recents_remove

end module recents_module
