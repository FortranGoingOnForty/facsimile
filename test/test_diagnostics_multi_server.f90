program test_diagnostics_multi_server
    use diagnostics_module, only: diagnostic_t, diagnostics_store_t, &
                                  init_diagnostics_store, cleanup_diagnostics_store, &
                                  parse_diagnostics_from_params_with_server, &
                                  get_diagnostics_for_file
    use json_module, only: json_value_t, json_parse
    implicit none

    character(len=*), parameter :: uri = 'file:///tmp/grouped.lu'
    type(diagnostics_store_t) :: store
    type(json_value_t) :: params
    type(diagnostic_t), allocatable :: items(:)
    integer :: nfail

    nfail = 0
    call init_diagnostics_store(store)

    params = diagnostic_params('wolf first')
    call parse_diagnostics_from_params_with_server(store, params, 1)
    call expect_servers([1], ['wolf first'], 'first server publishes')

    params = diagnostic_params('second server')
    call parse_diagnostics_from_params_with_server(store, params, 2)
    call expect_servers([1, 2], ['wolf first   ', 'second server'], &
                        'a second server keeps the first result')

    params = diagnostic_params('wolf refreshed')
    call parse_diagnostics_from_params_with_server(store, params, 1)
    call expect_servers([2, 1], ['second server ', 'wolf refreshed'], &
                        'refresh replaces only its server results')

    params = empty_params()
    call parse_diagnostics_from_params_with_server(store, params, 2)
    call expect_servers([1], ['wolf refreshed'], &
                        'empty publication clears only its server results')

    params = empty_params()
    call parse_diagnostics_from_params_with_server(store, params, 1)
    call expect_servers([integer ::], [character(len=1) ::], &
                        'last server can clear the file')

    call cleanup_diagnostics_store(store)

    if (nfail > 0) then
        print '(a,i0,a)', 'test_diagnostics_multi_server: ', nfail, ' FAILED'
        stop 1
    end if
    print '(a)', 'test_diagnostics_multi_server: all passed'

contains

    function diagnostic_params(message) result(value)
        character(len=*), intent(in) :: message
        type(json_value_t) :: value

        value = json_parse('{"uri":"' // uri // '","diagnostics":[' // &
                           '{"range":{"start":{"line":0,"character":0},' // &
                           '"end":{"line":0,"character":3}},"severity":1,' // &
                           '"message":"' // message // '","source":"test"}]}')
    end function diagnostic_params

    function empty_params() result(value)
        type(json_value_t) :: value

        value = json_parse('{"uri":"' // uri // '","diagnostics":[]}')
    end function empty_params

    subroutine expect_servers(want_servers, want_messages, label)
        integer, intent(in) :: want_servers(:)
        character(len=*), intent(in) :: want_messages(:)
        character(len=*), intent(in) :: label
        integer :: i

        items = get_diagnostics_for_file(store, uri)
        if (size(items) /= size(want_servers)) then
            call fail(label // ': wrong result count')
            return
        end if

        do i = 1, size(items)
            if (items(i)%server_index /= want_servers(i)) then
                call fail(label // ': wrong server order')
                return
            end if
            if (items(i)%message /= trim(want_messages(i))) then
                call fail(label // ': wrong diagnostic message')
                return
            end if
        end do
    end subroutine expect_servers

    subroutine fail(message)
        character(len=*), intent(in) :: message

        nfail = nfail + 1
        print '(a)', 'FAIL: ' // message
    end subroutine fail

end program test_diagnostics_multi_server
