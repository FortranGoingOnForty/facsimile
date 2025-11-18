program test_lsp_init
    use lsp_server_manager_module
    use iso_fortran_env, only: output_unit, error_unit
    implicit none

    type(lsp_manager_t) :: manager
    integer :: server_index
    character(len=256) :: test_file
    integer :: i

    print *, "Testing LSP initialization with fixed JSON parser..."

    ! Create test file
    test_file = "/tmp/test.c"
    open(10, file=test_file, status='replace')
    write(10, '(a)') "#include <stdio.h>"
    write(10, '(a)') "int main() { return 0; }"
    close(10)

    ! Initialize manager
    call init_lsp_manager(manager)

    ! Get or start server for C language
    server_index = get_or_start_server(manager, "c", "/tmp")
    if (server_index > 0) then
        print *, "✓ Server started successfully"
        print '(a,i0)', "   Server index: ", server_index
    else
        print *, "✗ Failed to start server"
        stop 1
    end if

    ! Wait a bit for initialization
    print *, "Waiting for initialization..."
    call sleep(2)

    ! Process any pending messages
    call process_server_messages(manager)

    ! Check if server is initialized
    do i = 1, manager%num_servers
        if (manager%servers(i)%initialized) then
            print *, "✓ Server initialized successfully!"
        else
            print *, "✗ Server not yet initialized"
        end if

        ! Show server info
        print '(a,a)', "   Command: ", trim(manager%servers(i)%command)
        print '(a,i0)', "   Process ID: ", manager%servers(i)%process_id
        print '(a,l1)', "   Initialized: ", manager%servers(i)%initialized
        print '(a,l1)', "   Supports completion: ", manager%servers(i)%supports_completion
    end do

    ! Cleanup
    call cleanup_lsp_manager(manager)
    print *, "Test complete."

end program test_lsp_init