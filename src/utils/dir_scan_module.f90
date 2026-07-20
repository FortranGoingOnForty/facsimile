! Fortran interface to dir_scan_wrapper.c - native single-directory listing
! for lazy file-tree expansion. Replaces shell-outs (ls/find) with
! opendir/readdir; no entry-count cap.
module dir_scan_module
    use iso_c_binding
    implicit none
    private

    public :: dir_entry_t, list_directory

    type :: dir_entry_t
        character(len=256) :: name = ''
        logical :: is_dir = .false.
    end type dir_entry_t

    interface
        function dir_open_c(path, path_len) bind(C, name='dir_open_f') result(handle)
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: path_len
            type(c_ptr) :: handle
        end function

        function dir_next_c(handle, name_buf, name_cap, name_len, is_dir) &
                bind(C, name='dir_next_f') result(status)
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: handle
            character(kind=c_char), intent(out) :: name_buf(*)
            integer(c_int), value :: name_cap
            integer(c_int), intent(out) :: name_len
            integer(c_int), intent(out) :: is_dir
            integer(c_int) :: status
        end function

        subroutine dir_close_c(handle) bind(C, name='dir_close_f')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine
    end interface

contains

    ! List one directory's entries ('.' and '..' excluded, dotfiles
    ! included). ok = .false. when the directory cannot be opened
    ! (missing, permission denied); entries is then allocated empty.
    subroutine list_directory(path, entries, n_entries, ok)
        character(len=*), intent(in) :: path
        type(dir_entry_t), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: n_entries
        logical, intent(out) :: ok

        type(c_ptr) :: handle
        character(kind=c_char) :: name_buf(256)
        integer(c_int) :: name_len, is_dir, status
        integer :: cap, i
        type(dir_entry_t), allocatable :: tmp(:)

        n_entries = 0
        ok = .false.

        handle = dir_open_c(trim(path), int(len_trim(path), c_int))
        if (.not. c_associated(handle)) then
            allocate(entries(0))
            return
        end if

        cap = 64
        allocate(entries(cap))

        do
            status = dir_next_c(handle, name_buf, 256_c_int, name_len, is_dir)
            if (status /= 1) exit

            n_entries = n_entries + 1
            if (n_entries > cap) then
                allocate(tmp(cap * 2))
                tmp(1:cap) = entries(1:cap)
                call move_alloc(tmp, entries)
                cap = cap * 2
            end if

            entries(n_entries)%name = ''
            do i = 1, int(name_len)
                entries(n_entries)%name(i:i) = name_buf(i)
            end do
            entries(n_entries)%is_dir = (is_dir == 1)
        end do

        call dir_close_c(handle)
        ok = .true.
    end subroutine list_directory

end module dir_scan_module
