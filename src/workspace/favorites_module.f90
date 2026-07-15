! Favorites management module
! Handles favorites.json read/write and favorite workspace management

module favorites_module
    use iso_fortran_env, only: int32
    use config_module, only: get_config_dir, ensure_config_dir
    implicit none
    private

    public :: favorite_t, favorites_load, favorites_save, favorites_add, favorites_remove
    public :: favorites_exists, favorites_get_path

    integer, parameter :: MAX_PATH_LEN = 512
    integer, parameter :: MAX_LABEL_LEN = 128
    integer, parameter :: MAX_FAVORITES = 50

    type :: favorite_t
        character(len=MAX_PATH_LEN) :: path = ""
        character(len=MAX_LABEL_LEN) :: label = ""
        character(len=32) :: added = ""  ! ISO 8601 timestamp
        logical :: pinned = .false.
    end type favorite_t

contains

    !> Get path to favorites.json
    subroutine favorites_get_path(favorites_file)
        character(len=:), allocatable, intent(out) :: favorites_file
        character(len=:), allocatable :: config_dir

        call get_config_dir(config_dir)
        favorites_file = trim(config_dir) // '/favorites.json'
    end subroutine favorites_get_path

    !> Check if favorites.json exists
    function favorites_exists() result(exists)
        logical :: exists
        character(len=:), allocatable :: favorites_file
        integer :: unit, ios

        call favorites_get_path(favorites_file)
        open(newunit=unit, file=favorites_file, status='old', iostat=ios)
        exists = (ios == 0)
        if (exists) close(unit)
    end function favorites_exists

    !> Load favorites from favorites.json
    subroutine favorites_load(favorites, count, success)
        type(favorite_t), allocatable, intent(out) :: favorites(:)
        integer, intent(out) :: count
        logical, intent(out) :: success
        character(len=:), allocatable :: favorites_file
        character(len=1024) :: line
        integer :: unit, ios, i
        logical :: in_favorites, reading_entry

        success = .false.
        count = 0
        allocate(favorites(MAX_FAVORITES))

        ! Ensure config directory exists
        call ensure_config_dir(success)
        if (.not. success) return

        call favorites_get_path(favorites_file)

        ! Open file
        open(newunit=unit, file=favorites_file, status='old', iostat=ios)
        if (ios /= 0) then
            ! File doesn't exist - return empty list
            success = .true.
            count = 0
            return
        end if

        ! Simple JSON parsing (look for "path", "label", "added", "pinned" fields)
        in_favorites = .false.
        reading_entry = .false.
        i = 0

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            ! Trim whitespace
            line = adjustl(line)

            ! Check if we're in the favorites array
            if (index(line, '"favorites"') > 0) then
                in_favorites = .true.
                cycle
            end if

            if (.not. in_favorites) cycle

            ! Look for start of entry
            if (index(line, '{') > 0 .and. .not. reading_entry) then
                reading_entry = .true.
                i = i + 1
                if (i > MAX_FAVORITES) exit
                ! Initialize entry
                favorites(i)%path = ""
                favorites(i)%label = ""
                favorites(i)%added = ""
                favorites(i)%pinned = .false.
                cycle
            end if

            if (reading_entry) then
                ! Parse fields
                if (index(line, '"path"') > 0) then
                    call extract_json_string(line, favorites(i)%path)
                else if (index(line, '"label"') > 0) then
                    call extract_json_string(line, favorites(i)%label)
                else if (index(line, '"added"') > 0) then
                    call extract_json_string(line, favorites(i)%added)
                else if (index(line, '"pinned"') > 0) then
                    favorites(i)%pinned = (index(line, 'true') > 0)
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
    end subroutine favorites_load

    !> Save favorites to favorites.json
    subroutine favorites_save(favorites, count, success)
        type(favorite_t), intent(in) :: favorites(:)
        integer, intent(in) :: count
        logical, intent(out) :: success
        character(len=:), allocatable :: favorites_file
        integer :: unit, ios, i

        success = .false.

        ! Ensure config directory exists
        call ensure_config_dir(success)
        if (.not. success) return

        call favorites_get_path(favorites_file)

        ! Write JSON file
        open(newunit=unit, file=favorites_file, status='replace', iostat=ios)
        if (ios /= 0) return

        write(unit, '(A)') '{'
        write(unit, '(A)') '  "version": "1.0",'
        write(unit, '(A)') '  "favorites": ['

        do i = 1, count
            write(unit, '(A)') '    {'
            write(unit, '(A)') '      "path": "' // trim(favorites(i)%path) // '",'
            write(unit, '(A)') '      "label": "' // trim(favorites(i)%label) // '",'
            write(unit, '(A)') '      "added": "' // trim(favorites(i)%added) // '",'
            if (favorites(i)%pinned) then
                write(unit, '(A)') '      "pinned": true'
            else
                write(unit, '(A)') '      "pinned": false'
            end if

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
    end subroutine favorites_save

    !> Add a favorite workspace
    subroutine favorites_add(path, label, success)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        logical, intent(out) :: success
        type(favorite_t), allocatable :: favorites(:)
        integer :: count, i
        character(len=32) :: timestamp

        success = .false.

        ! Load existing favorites
        call favorites_load(favorites, count, success)
        if (.not. success) return

        ! Check if path already exists
        do i = 1, count
            if (trim(favorites(i)%path) == trim(path)) then
                ! Already exists - don't add duplicate
                success = .true.
                return
            end if
        end do

        ! Check if we've reached max favorites
        if (count >= MAX_FAVORITES) then
            success = .false.
            return
        end if

        ! Add new favorite
        count = count + 1
        favorites(count)%path = trim(path)
        favorites(count)%label = trim(label)
        call get_current_timestamp(timestamp)
        favorites(count)%added = trim(timestamp)
        favorites(count)%pinned = .false.

        ! Save
        call favorites_save(favorites, count, success)
    end subroutine favorites_add

    !> Remove a favorite by index
    subroutine favorites_remove(index, success)
        integer, intent(in) :: index
        logical, intent(out) :: success
        type(favorite_t), allocatable :: favorites(:), temp(:)
        integer :: count, i, j

        success = .false.

        ! Load existing favorites
        call favorites_load(favorites, count, success)
        if (.not. success) return

        ! Check valid index
        if (index < 1 .or. index > count) then
            success = .false.
            return
        end if

        ! Create new array without the removed entry
        allocate(temp(MAX_FAVORITES))
        j = 0
        do i = 1, count
            if (i /= index) then
                j = j + 1
                temp(j) = favorites(i)
            end if
        end do

        ! Save
        call favorites_save(temp, j, success)
        deallocate(temp)
    end subroutine favorites_remove

    !> Extract string value from JSON line (simple parser)
    subroutine extract_json_string(line, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: value
        integer :: start_quote, end_quote, colon_pos

        value = ""

        ! Find colon
        colon_pos = index(line, ':')
        if (colon_pos == 0) return

        ! Find first quote after colon (index into the substring is
        ! 1-based, so subtract 1 to get the absolute position)
        start_quote = index(line(colon_pos:), '"')
        if (start_quote == 0) return
        start_quote = start_quote + colon_pos - 1

        ! Find second quote
        end_quote = index(line(start_quote+1:), '"')
        if (end_quote == 0) return
        end_quote = end_quote + start_quote

        ! Extract string
        if (end_quote > start_quote) then
            value = line(start_quote+1:end_quote-1)
        end if
    end subroutine extract_json_string

    !> Get current timestamp in ISO 8601 format
    subroutine get_current_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)

        ! Format: YYYY-MM-DDTHH:MM:SSZ
        write(timestamp, '(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7), 'Z'
    end subroutine get_current_timestamp

end module favorites_module
