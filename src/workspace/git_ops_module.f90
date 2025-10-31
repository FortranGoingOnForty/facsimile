module git_ops_module
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: git_commit, git_push, git_fetch, git_pull, git_tag
    public :: git_check_upstream, git_diff_file, git_list_tags, git_push_tag

contains

    subroutine git_commit(workspace_path, message, success)
        character(len=*), intent(in) :: workspace_path
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        character(len=2048) :: command
        integer :: status

        success = .false.

        if (len_trim(message) == 0) then
            write(error_unit, '(A)') 'Error: Empty commit message'
            return
        end if

        ! Build git commit command with message
        write(command, '(A,A,A,A,A)') 'cd "', trim(workspace_path), '" && git commit -m "', trim(message), '" 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        success = (status == 0)
    end subroutine git_commit

    subroutine git_push(workspace_path, success)
        character(len=*), intent(in) :: workspace_path
        logical, intent(out) :: success
        character(len=1024) :: command
        integer :: status
        logical :: has_upstream

        success = .false.

        ! Check if upstream is configured
        call git_check_upstream(workspace_path, has_upstream)

        if (.not. has_upstream) then
            ! Try to set upstream to origin/current-branch
            write(command, '(A,A,A)') 'cd "', trim(workspace_path), &
                '" && git push --set-upstream origin $(git rev-parse --abbrev-ref HEAD) 2>&1'
        else
            ! Normal push
            write(command, '(A,A,A)') 'cd "', trim(workspace_path), '" && git push 2>&1'
        end if

        call execute_command_line(trim(command), exitstat=status)
        success = (status == 0)
    end subroutine git_push

    subroutine git_fetch(workspace_path, success)
        character(len=*), intent(in) :: workspace_path
        logical, intent(out) :: success
        character(len=1024) :: command
        integer :: status

        success = .false.

        write(command, '(A,A,A)') 'cd "', trim(workspace_path), '" && git fetch 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        success = (status == 0)
    end subroutine git_fetch

    subroutine git_pull(workspace_path, success)
        character(len=*), intent(in) :: workspace_path
        logical, intent(out) :: success
        character(len=1024) :: command
        integer :: status

        success = .false.

        write(command, '(A,A,A)') 'cd "', trim(workspace_path), '" && git pull 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        success = (status == 0)
    end subroutine git_pull

    subroutine git_tag(workspace_path, tag_name, tag_message, success)
        character(len=*), intent(in) :: workspace_path
        character(len=*), intent(in) :: tag_name
        character(len=*), intent(in) :: tag_message
        logical, intent(out) :: success
        character(len=2048) :: command
        integer :: status

        success = .false.

        if (len_trim(tag_name) == 0) then
            write(error_unit, '(A)') 'Error: Empty tag name'
            return
        end if

        if (len_trim(tag_message) > 0) then
            ! Create annotated tag with message
            write(command, '(A,A,A,A,A,A,A)') 'cd "', trim(workspace_path), &
                '" && git tag -a "', trim(tag_name), '" -m "', trim(tag_message), '" 2>&1'
        else
            ! Create lightweight tag (no message)
            write(command, '(A,A,A,A,A)') 'cd "', trim(workspace_path), &
                '" && git tag "', trim(tag_name), '" 2>&1'
        end if

        call execute_command_line(trim(command), exitstat=status)

        if (status == 0) then
            ! Also fetch after tagging to sync with remote
            write(command, '(A,A,A)') 'cd "', trim(workspace_path), '" && git fetch --tags 2>&1'
            call execute_command_line(trim(command), exitstat=status)
            success = .true.
        end if
    end subroutine git_tag

    subroutine git_push_tag(workspace_path, tag_name, success)
        character(len=*), intent(in) :: workspace_path
        character(len=*), intent(in) :: tag_name
        logical, intent(out) :: success
        character(len=1024) :: command
        integer :: status

        success = .false.

        if (len_trim(tag_name) == 0) then
            write(error_unit, '(A)') 'Error: Empty tag name'
            return
        end if

        ! Push the specific tag to origin
        write(command, '(A,A,A,A,A)') 'cd "', trim(workspace_path), &
            '" && git push origin "', trim(tag_name), '" 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        success = (status == 0)
    end subroutine git_push_tag

    subroutine git_check_upstream(workspace_path, has_upstream)
        character(len=*), intent(in) :: workspace_path
        logical, intent(out) :: has_upstream
        character(len=1024) :: command
        integer :: status

        write(command, '(A,A,A)') 'cd "', trim(workspace_path), &
            '" && git rev-parse --abbrev-ref @{upstream} > /dev/null 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        has_upstream = (status == 0)
    end subroutine git_check_upstream

    subroutine git_diff_file(workspace_path, file_path, diff_content, branch_name, success)
        character(len=*), intent(in) :: workspace_path
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable, intent(out) :: diff_content
        character(len=*), intent(out) :: branch_name
        logical, intent(out) :: success
        character(len=2048) :: command
        character(len=512) :: temp_file, line
        integer :: status, unit_num, ios
        integer :: total_size, current_pos

        success = .false.
        branch_name = ''

        ! Get current branch name
        temp_file = '/tmp/fac_branch.tmp'
        write(command, '(A,A,A,A,A)') 'cd "', trim(workspace_path), &
            '" && git rev-parse --abbrev-ref HEAD > "', trim(temp_file), '" 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        if (status == 0) then
            open(newunit=unit_num, file=trim(temp_file), status='old', action='read', iostat=ios)
            if (ios == 0) then
                read(unit_num, '(A)', iostat=ios) branch_name
                close(unit_num)
            end if
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
        end if

        ! Get diff output
        temp_file = '/tmp/fac_diff.tmp'
        write(command, '(A,A,A,A,A,A,A)') 'cd "', trim(workspace_path), &
            '" && git diff HEAD -- "', trim(file_path), '" > "', trim(temp_file), '" 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        if (status /= 0) then
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
            diff_content = ''
            return
        end if

        ! Read diff content from temp file
        open(newunit=unit_num, file=trim(temp_file), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
            diff_content = ''
            return
        end if

        ! First pass: calculate total size needed
        total_size = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            total_size = total_size + len_trim(line) + 1  ! +1 for newline
        end do

        if (total_size == 0) then
            ! No changes
            close(unit_num)
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
            diff_content = '(no changes)'
            success = .true.
            return
        end if

        ! Allocate string with exact size needed
        allocate(character(len=total_size) :: diff_content)
        current_pos = 1

        ! Second pass: read content into allocated string
        rewind(unit_num)
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            ! Append line and newline
            if (len_trim(line) > 0) then
                ! Non-empty line
                if (current_pos + len_trim(line) - 1 <= total_size) then
                    diff_content(current_pos:current_pos+len_trim(line)-1) = trim(line)
                    current_pos = current_pos + len_trim(line)
                end if
            end if

            ! Add newline after each line (including empty lines)
            if (current_pos <= total_size) then
                diff_content(current_pos:current_pos) = new_line('a')
                current_pos = current_pos + 1
            end if
        end do

        close(unit_num)
        call execute_command_line('rm -f "' // trim(temp_file) // '"')
        success = .true.
    end subroutine git_diff_file

    subroutine git_list_tags(workspace_path, tags, n_tags)
        character(len=*), intent(in) :: workspace_path
        character(len=256), allocatable, intent(out) :: tags(:)
        integer, intent(out) :: n_tags
        character(len=1024) :: command, temp_file
        character(len=256) :: line
        integer :: status, unit_num, ios, i, max_tags
        character(len=256), allocatable :: temp_tags(:)

        n_tags = 0
        max_tags = 100  ! Initial allocation

        ! Get tags sorted by creation date (newest first)
        temp_file = '/tmp/fac_tags.tmp'
        write(command, '(A,A,A,A,A)') 'cd "', trim(workspace_path), &
            '" && git tag --sort=-creatordate > "', trim(temp_file), '" 2>&1'
        call execute_command_line(trim(command), exitstat=status)

        if (status /= 0) then
            ! No tags or error
            allocate(tags(0))
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
            return
        end if

        ! Count tags
        open(newunit=unit_num, file=trim(temp_file), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            allocate(tags(0))
            call execute_command_line('rm -f "' // trim(temp_file) // '"')
            return
        end if

        ! Read tags into temporary array
        allocate(temp_tags(max_tags))
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0) then
                n_tags = n_tags + 1
                if (n_tags > max_tags) then
                    ! Reallocate if needed
                    max_tags = max_tags * 2
                    deallocate(temp_tags)
                    allocate(temp_tags(max_tags))
                end if
                temp_tags(n_tags) = trim(line)
            end if
        end do
        close(unit_num)

        ! Copy to output array
        if (n_tags > 0) then
            allocate(tags(n_tags))
            do i = 1, n_tags
                tags(i) = temp_tags(i)
            end do
        else
            allocate(tags(0))
        end if

        deallocate(temp_tags)
        call execute_command_line('rm -f "' // trim(temp_file) // '"')
    end subroutine git_list_tags

end module git_ops_module
