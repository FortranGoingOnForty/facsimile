! Workspace management module
! Handles workspace detection, creation, loading, and saving

module workspace_module
    implicit none
    private

    public :: workspace_exists, workspace_init, workspace_load, workspace_save
    public :: workspace_get_path, workspace_detect_from_file

    integer, parameter :: MAX_PATH_LEN = 512

contains

    !> Check if a directory has a workspace
    function workspace_exists(dir_path) result(exists)
        character(len=*), intent(in) :: dir_path
        logical :: exists
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        ! Build path to workspace.json
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! Try to open the file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        exists = (ios == 0)

        if (exists) close(unit)
    end function workspace_exists

    !> Detect workspace from a file path (search parent directories)
    function workspace_detect_from_file(file_path) result(workspace_path)
        character(len=*), intent(in) :: file_path
        character(len=MAX_PATH_LEN) :: workspace_path
        character(len=MAX_PATH_LEN) :: current_dir, parent_dir
        integer :: last_slash

        workspace_path = ""

        ! Get directory of file
        last_slash = index(file_path, "/", back=.true.)
        if (last_slash > 0) then
            current_dir = file_path(1:last_slash-1)
        else
            current_dir = "."
        end if

        ! Search up the directory tree for a workspace
        do while (len_trim(current_dir) > 0)
            if (workspace_exists(current_dir)) then
                workspace_path = current_dir
                return
            end if

            ! Move to parent directory
            if (current_dir == "/" .or. current_dir == ".") exit

            last_slash = index(current_dir, "/", back=.true.)
            if (last_slash > 0) then
                parent_dir = current_dir(1:last_slash-1)
                if (len_trim(parent_dir) == 0) parent_dir = "/"
                current_dir = parent_dir
            else
                exit
            end if
        end do
    end function workspace_detect_from_file

    !> Get absolute workspace path from potentially relative path
    subroutine workspace_get_path(input_path, absolute_path)
        character(len=*), intent(in) :: input_path
        character(len=MAX_PATH_LEN), intent(out) :: absolute_path
        integer :: unit, ios

        ! Use realpath via shell command
        call execute_command_line("realpath '" // trim(input_path) // "' > /tmp/.fac_realpath 2>/dev/null", &
                                 wait=.true.)

        open(newunit=unit, file='/tmp/.fac_realpath', status='old', iostat=ios)
        if (ios == 0) then
            read(unit, '(a)', iostat=ios) absolute_path
            close(unit)
            call execute_command_line("rm -f /tmp/.fac_realpath", wait=.true.)
        else
            absolute_path = input_path
        end if
    end subroutine workspace_get_path

    !> Initialize a new workspace in the given directory
    subroutine workspace_init(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: fac_dir, workspace_file, backup_dir
        integer :: unit, ios

        success = .false.

        ! Create .fac directory
        fac_dir = trim(dir_path) // "/.fac"
        call execute_command_line("mkdir -p '" // trim(fac_dir) // "' 2>/dev/null", wait=.true.)

        ! Create backups subdirectory
        backup_dir = trim(fac_dir) // "/backups"
        call execute_command_line("mkdir -p '" // trim(backup_dir) // "' 2>/dev/null", wait=.true.)

        ! Create initial workspace.json
        workspace_file = trim(fac_dir) // "/workspace.json"

        open(newunit=unit, file=workspace_file, status='replace', iostat=ios)
        if (ios /= 0) return

        ! Write minimal initial workspace JSON
        write(unit, '(a)') '{'
        write(unit, '(a)') '  "version": "1.0",'
        write(unit, '(a)') '  "workspace_path": "' // trim(dir_path) // '",'
        write(unit, '(a)') '  "last_opened": "",'
        write(unit, '(a)') '  "tabs": [],'
        write(unit, '(a)') '  "orphan_tabs": [],'
        write(unit, '(a)') '  "active_tab": 0,'
        write(unit, '(a)') '  "fuss_mode": {'
        write(unit, '(a)') '    "active": false,'
        write(unit, '(a)') '    "width": 30'
        write(unit, '(a)') '  }'
        write(unit, '(a)') '}'

        close(unit)
        success = .true.
    end subroutine workspace_init

    !> Load workspace state (stub for now - Phase 3 will implement full deserialization)
    subroutine workspace_load(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! For now, just verify the file exists and is readable
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios == 0) then
            close(unit)
            success = .true.
            ! TODO Phase 3: Parse JSON and restore tabs/panes/cursor state
        end if
    end subroutine workspace_load

    !> Save workspace state (stub for now - Phase 3 will implement full serialization)
    subroutine workspace_save(dir_path, success)
        character(len=*), intent(in) :: dir_path
        logical, intent(out) :: success
        character(len=MAX_PATH_LEN) :: workspace_file
        integer :: unit, ios

        success = .false.
        workspace_file = trim(dir_path) // "/.fac/workspace.json"

        ! For now, just verify we can write to the file
        open(newunit=unit, file=workspace_file, status='old', iostat=ios)
        if (ios == 0) then
            close(unit)
            success = .true.
            ! TODO Phase 3: Serialize current tabs/panes/cursor state to JSON
        end if
    end subroutine workspace_save

end module workspace_module
