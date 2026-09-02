program test_lsp_response_shapes
    ! The two response shapes LSP allows and this client read only half of.
    !
    ! Both were found by putting a real `wolf lsp` behind a PATH shim and
    ! reading the wire: Ctrl+Space sent textDocument/completion, the server
    ! answered fifty items, and no popup appeared -- because the answer was a
    ! bare CompletionItem[] and only the CompletionList form was parsed. The
    ! definition half is the mirror image: this client declares
    ! textDocument.definition.linkSupport, so a server that honours the
    ! declaration answers LocationLink[], which was not parsed either.
    use json_module, only: json_value_t, json_parse
    use lsp_protocol_module, only: definition_target
    use completion_popup_module, only: completion_popup_t, &
        init_completion_popup, cleanup_completion_popup, handle_completion_response
    implicit none

    type(completion_popup_t) :: popup
    type(json_value_t) :: result
    character(len=:), allocatable :: uri
    integer :: line, col, nfail
    logical :: ok

    nfail = 0

    ! --- textDocument/completion: CompletionItem[] | CompletionList | null

    call init_completion_popup(popup)

    ! The CompletionList form, which always worked
    result = json_parse('{"isIncomplete":false,"items":[' // &
        '{"label":"alpha","kind":6},{"label":"beta","kind":3}]}')
    call handle_completion_response(popup, result)
    call check(popup%item_count == 2, 'completion: CompletionList yields its items')
    call check(popup%items(1)%label == 'alpha', 'completion: first CompletionList label')
    call check(popup%items(2)%kind == 'Function', 'completion: kind 3 is Function')

    ! The bare-array form -- what wolf 0.2.1 answers
    result = json_parse('[{"label":"name","kind":6},' // &
        '{"detail":"fn main()","kind":3,"label":"main"},' // &
        '{"label":"match","kind":14}]')
    call handle_completion_response(popup, result)
    call check(popup%item_count == 3, 'completion: a bare CompletionItem[] yields its items')
    call check(popup%items(1)%label == 'name', 'completion: first bare-array label')
    call check(popup%items(2)%detail == 'fn main()', 'completion: bare-array detail survives')
    call check(popup%items(3)%kind == 'Keyword', 'completion: kind 14 is Keyword')
    call check(popup%items(1)%insert_text == 'name', &
               'completion: insert text falls back to the label')

    ! An empty answer of either shape leaves nothing behind
    result = json_parse('[]')
    call handle_completion_response(popup, result)
    call check(popup%item_count == 0, 'completion: an empty array is no items')

    result = json_parse('{"isIncomplete":false,"items":[]}')
    call handle_completion_response(popup, result)
    call check(popup%item_count == 0, 'completion: an empty CompletionList is no items')

    ! Neither shape at all, and nothing is invented
    result = json_parse('{"unexpected":true}')
    call handle_completion_response(popup, result)
    call check(popup%item_count == 0, 'completion: an unrecognised shape is no items')

    call cleanup_completion_popup(popup)

    ! --- textDocument/definition: Location | LocationLink

    ! The Location form, which always worked
    result = json_parse('{"uri":"file:///tmp/a.lu",' // &
        '"range":{"start":{"line":4,"character":8},"end":{"line":4,"character":13}}}')
    call definition_target(result, uri, line, col, ok)
    call check(ok, 'definition: a Location is understood')
    call check(uri == 'file:///tmp/a.lu', 'definition: Location uri')
    call check(line == 4 .and. col == 8, 'definition: Location start, still 0-based')

    ! The LocationLink form, which the linkSupport declaration invites
    result = json_parse('{"targetUri":"file:///tmp/b.lu",' // &
        '"targetRange":{"start":{"line":9,"character":0},"end":{"line":12,"character":1}},' // &
        '"targetSelectionRange":{"start":{"line":10,"character":3},' // &
        '"end":{"line":10,"character":7}}}')
    call definition_target(result, uri, line, col, ok)
    call check(ok, 'definition: a LocationLink is understood')
    call check(uri == 'file:///tmp/b.lu', 'definition: LocationLink targetUri')
    call check(line == 10 .and. col == 3, &
               'definition: the cursor follows targetSelectionRange, not targetRange')

    ! targetSelectionRange is required by the protocol, but a server that
    ! omits it should still land the cursor somewhere sensible
    result = json_parse('{"targetUri":"file:///tmp/c.lu",' // &
        '"targetRange":{"start":{"line":2,"character":4},"end":{"line":6,"character":1}}}')
    call definition_target(result, uri, line, col, ok)
    call check(ok, 'definition: a LocationLink without targetSelectionRange still works')
    call check(line == 2 .and. col == 4, 'definition: it falls back to targetRange')

    ! Neither shape, and nothing is claimed
    result = json_parse('{"something":"else"}')
    call definition_target(result, uri, line, col, ok)
    call check(.not. ok, 'definition: an unrecognised shape is not a target')

    result = json_parse('{"uri":"","range":{"start":{"line":1,"character":1}}}')
    call definition_target(result, uri, line, col, ok)
    call check(.not. ok, 'definition: an empty uri is not a target')

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All LSP response shape tests passed'

contains

    subroutine check(cond, name)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: name
        if (cond) then
            print '(a)', 'PASS: ' // name
        else
            print '(a)', 'FAIL: ' // name
            nfail = nfail + 1
        end if
    end subroutine check

end program test_lsp_response_shapes
