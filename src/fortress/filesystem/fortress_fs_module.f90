! Filesystem operations for Fortress navigator (adapted for fac)
! Original source: fortress/src/filesystem/fs_ops.f90

module fortress_fs_module
    implicit none
    private

    public :: get_file_list, get_pwd, get_parent_path, join_path
    public :: find_in_parent, find_file_in_list
    public :: MAX_PATH, MAX_FILES

    integer, parameter :: MAX_PATH = 512
    integer, parameter :: MAX_FILES = 500

contains

    subroutine get_file_list(dir, files, is_dir, is_exec, count)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(out) :: files
        logical, dimension(*), intent(out) :: is_dir, is_exec
        integer, intent(out) :: count
        integer :: unit, ios, i, stat_code
        character(len=MAX_PATH) :: temp_file, stat_file
        character(len=MAX_PATH) :: line, filename, file_type

        ! Get list of files
        call get_environment_variable("HOME", temp_file)
        temp_file = trim(temp_file) // "/.fac_fortress_ls"
        stat_file = trim(temp_file) // "_stat"

        call execute_command_line("ls -1a '" // trim(dir) // "' > " // trim(temp_file) // " 2>/dev/null", wait=.true.)

        open(newunit=unit, file=temp_file, status='old', iostat=ios)
        if (ios /= 0) then
            count = 0
            call execute_command_line("rm -f " // trim(temp_file) // " 2>/dev/null")
            return
        end if

        ! Read all filenames first
        count = 0
        do
            count = count + 1
            if (count > MAX_FILES) exit
            read(unit, '(a)', iostat=ios) files(count)
            if (ios /= 0) then
                count = count - 1
                exit
            end if
        end do
        close(unit)

        ! Now check file attributes - use simpler approach with stat via ls
        ! Generate a script that checks each file and outputs "filename:type"
        call execute_command_line("cd '" // trim(dir) // "' && " // &
            "for f in $(ls -1a 2>/dev/null); do " // &
            "  if [ -d ""$f"" ]; then echo ""$f:d""; " // &
            "  elif [ -x ""$f"" ] && [ ! -d ""$f"" ]; then echo ""$f:x""; " // &
            "  else echo ""$f:f""; fi; " // &
            "done > " // trim(stat_file) // " 2>/dev/null", wait=.true.)

        ! Initialize all as regular non-executable files
        do i = 1, count
            is_dir(i) = .false.
            is_exec(i) = .false.
        end do

        ! Read the stat results and update file types
        open(newunit=unit, file=stat_file, status='old', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(a)', iostat=ios) line
                if (ios /= 0) exit

                ! Parse "filename:type" format
                stat_code = index(line, ':', back=.true.)
                if (stat_code > 0) then
                    filename = line(1:stat_code-1)
                    file_type = line(stat_code+1:stat_code+1)

                    ! Find this file in our list and update its type
                    do i = 1, count
                        if (trim(files(i)) == trim(filename)) then
                            if (file_type == 'd') then
                                is_dir(i) = .true.
                                is_exec(i) = .false.
                            else if (file_type == 'x') then
                                is_dir(i) = .false.
                                is_exec(i) = .true.
                            else
                                is_dir(i) = .false.
                                is_exec(i) = .false.
                            end if
                            exit
                        end if
                    end do
                end if
            end do
            close(unit)
        end if

        ! Cleanup temp files
        call execute_command_line("rm -f " // trim(temp_file) // " " // trim(stat_file) // " 2>/dev/null")
    end subroutine get_file_list

    function get_pwd() result(path)
        character(len=MAX_PATH) :: path
        integer :: unit, ios

        call execute_command_line("pwd > .fac_pwd 2>/dev/null", wait=.true.)
        open(newunit=unit, file=".fac_pwd", status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)') path
            close(unit)
        else
            path = "."
        end if
        call execute_command_line("rm -f .fac_pwd 2>/dev/null")
    end function get_pwd

    function get_parent_path(path) result(parent)
        character(len=*), intent(in) :: path
        character(len=MAX_PATH) :: parent
        integer :: pos

        pos = index(path, "/", back=.true.)
        if (pos > 1) then
            parent = path(1:pos-1)
        else if (pos == 1) then
            parent = "/"
        else
            parent = "."
        end if
    end function get_parent_path

    function join_path(base, name) result(full)
        character(len=*), intent(in) :: base, name
        character(len=MAX_PATH) :: full

        if (base == "/") then
            full = "/" // trim(name)
        else
            full = trim(base) // "/" // trim(name)
        end if
    end function join_path

    function find_in_parent(dir, files, count) result(idx)
        character(len=*), intent(in) :: dir
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=256) :: basename

        pos = index(dir, "/", back=.true.)
        if (pos > 0) then
            basename = dir(pos+1:)
        else
            basename = dir
        end if

        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do
        idx = 1
    end function find_in_parent

    function find_file_in_list(target_path, files, count) result(idx)
        character(len=*), intent(in) :: target_path
        character(len=*), dimension(*), intent(in) :: files
        integer, intent(in) :: count
        integer :: idx, pos
        character(len=MAX_PATH) :: basename

        ! Extract basename from target_path
        pos = index(target_path, "/", back=.true.)
        if (pos > 0) then
            basename = target_path(pos+1:)
        else
            basename = target_path
        end if

        ! Search for the file in the list
        do idx = 1, count
            if (trim(files(idx)) == trim(basename)) return
        end do

        ! Default to first item if not found
        idx = 1
    end function find_file_in_list

end module fortress_fs_module
