program test_popup_blocks_clicks
    ! The completion popup must claim its cells, so a click on it is swallowed
    ! instead of moving the caret in the document behind it -- which used to
    ! leave the popup open over a caret that had moved out from under it, so
    ! the next Enter inserted the completion in the wrong place.
    !
    ! Driven through the real path (parse an LSP response, show, render)
    ! rather than a pty: summoning the popup in a terminal needs a live
    ! language server to answer a ctrl-space within the test's patience, which
    ! proved too flaky to assert on.
    use json_module, only: json_value_t, json_parse
    use completion_popup_module, only: completion_popup_t, init_completion_popup, &
                                       handle_completion_response, &
                                       show_completion_popup, render_completion_popup, &
                                       is_completion_visible
    use clickable_region_module, only: clickable_region_t, regions_begin_frame, &
                                       region_at, region_count, REGION_BLOCK
    implicit none

    type(completion_popup_t) :: popup
    type(json_value_t) :: response
    type(clickable_region_t) :: hit
    character(len=:), allocatable :: payload
    integer :: nfail

    nfail = 0

    ! A minimal textDocument/completion result
    payload = '{"items":[' // &
              '{"label":"zebra_alpha","kind":3},' // &
              '{"label":"zebra_bravo","kind":3},' // &
              '{"label":"zebra_charlie","kind":3}]}'

    call init_completion_popup(popup)
    response = json_parse(payload)
    call handle_completion_response(popup, response)
    call check(popup%item_count == 3, "the response yields three items")

    ! show_completion_popup renders immediately, so start a frame first
    call regions_begin_frame()
    call show_completion_popup(popup, 8, 20, 30, 100)
    call check(is_completion_visible(popup), "the popup is visible once shown")
    call check(region_count() > 0, "showing it registers a region")

    ! Re-render the way the frame loop does, then probe the claimed cells
    call regions_begin_frame()
    call render_completion_popup(popup)

    hit = region_at(popup%row + 1, popup%col + 2)
    call check(hit%kind == REGION_BLOCK, "a cell inside the box is claimed")

    hit = region_at(popup%row, popup%col)
    call check(hit%kind == REGION_BLOCK, "the top-left corner is claimed")

    hit = region_at(popup%row + 1, popup%col + popup%width - 1)
    call check(hit%kind == REGION_BLOCK, "the right border column is claimed")

    hit = region_at(popup%row + 1, popup%col - 1)
    call check(hit%kind /= REGION_BLOCK, "one column left of the box is not")

    hit = region_at(popup%row - 1, popup%col + 2)
    call check(hit%kind /= REGION_BLOCK, "the row above the box is not")

    if (nfail > 0) then
        print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
        stop 1
    end if
    print '(a)', 'All completion-popup click-blocking tests passed'

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

end program test_popup_blocks_clicks
