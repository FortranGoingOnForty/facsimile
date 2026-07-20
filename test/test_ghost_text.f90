program test_ghost_text
    ! Unit tests for ghost_text_module (inline shadow-text suggestions)
    use text_buffer_module
    use json_module, only: json_value_t, json_parse
    use ghost_text_module
    implicit none

    type(buffer_t) :: buffer
    type(ghost_text_t) :: ghost
    type(json_value_t) :: result_json
    character(len=:), allocatable :: prefix
    integer :: nfail

    nfail = 0

    ! Buffer under test:
    !   line 1: program facsimile
    !   line 2:     integer :: counter_total
    !   line 3:     counter_max = 10
    !   line 4:     cou              <- "typing" here, cursor at EOL (col 8)
    !   line 5:     print *, counter_total
    !   line 6:     Fortran_Rocks
    call init_buffer(buffer)
    call buffer_insert(buffer, 1, &
        "program facsimile" // char(10) // &
        "    integer :: counter_total" // char(10) // &
        "    counter_max = 10" // char(10) // &
        "    cou" // char(10) // &
        "    print *, counter_total" // char(10) // &
        "    Fortran_Rocks")

    ! --- Prefix extraction ---
    call ghost_get_prefix_at_cursor(buffer, 4, 8, prefix)
    call check(prefix == "cou", "prefix at EOL after word", prefix)

    call ghost_get_prefix_at_cursor(buffer, 4, 1, prefix)
    call check(prefix == "", "prefix at column 1 is empty", prefix)

    ! line 3 col 16 is just after "counter_max " (space before cursor)
    call ghost_get_prefix_at_cursor(buffer, 3, 17, prefix)
    call check(prefix == "", "prefix after space is empty", prefix)

    ! --- Word scan: nearest line wins, tie -> earlier line ---
    ! Candidates for "cou": counter_total (l2, dist 2), counter_max (l3,
    ! dist 1), counter_total (l5, dist 1). l3 found first at dist 1.
    call ghost_update_from_buffer(ghost, buffer, "cou", 4, 8)
    call check(ghost_is_active(ghost), "word scan finds a suggestion", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "counter_max", &
                   "nearest line wins (counter_max)", ghost%suggestion)
        call check(ghost_suffix(ghost) == "nter_max", &
                   "suffix strips typed prefix", ghost_suffix(ghost))
        call check(ghost%source == GHOST_SRC_WORDS, "source is word scan", "wrong source")
    end if

    ! --- Case-insensitive fallback preserves what was typed ---
    call ghost_update_from_buffer(ghost, buffer, "fort", 4, 8)
    call check(ghost_is_active(ghost), "case-insensitive fallback fires", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "fortran_Rocks", &
                   "typed case kept, candidate suffix appended", ghost%suggestion)
    end if

    ! --- No match -> inactive ---
    call ghost_update_from_buffer(ghost, buffer, "zzz", 4, 8)
    call check(.not. ghost_is_active(ghost), "no match leaves ghost inactive", "active")

    ! --- The word being typed never suggests itself ---
    ! "facsimile" appears only on line 1; typing it there (cursor at its end,
    ! col 18) must not self-suggest
    call ghost_update_from_buffer(ghost, buffer, "facsimile", 1, 18)
    call check(.not. ghost_is_active(ghost), "typed token excluded from candidates", "active")

    ! --- UTF-8: prefix after multibyte text ---
    call cleanup_buffer(buffer)
    call init_buffer(buffer)
    call buffer_insert(buffer, 1, "αβγ myv" // char(10) // "myvalue = 1")
    ! line 1 has 7 chars (3 multibyte + space + "myv"); cursor at EOL col 8
    call ghost_get_prefix_at_cursor(buffer, 1, 8, prefix)
    call check(prefix == "myv", "prefix after multibyte text", prefix)
    call ghost_update_from_buffer(ghost, buffer, "myv", 1, 8)
    call check(ghost_is_active(ghost), "scan works with multibyte lines", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "myvalue", "suggestion from next line", ghost%suggestion)
    end if

    ! --- LSP results: CompletionList shape, snippet/space filtering ---
    call ghost_clear(ghost)
    result_json = json_parse('{"items":[{"label":"co$1"},{"label":"count me"},' // &
                             '{"label":"counter_total"}]}')
    call ghost_apply_lsp_result(ghost, result_json, "cou", 1, 8, .false.)
    call check(ghost_is_active(ghost), "LSP CompletionList applied", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "counter_total", &
                   "snippet and non-identifier items filtered", ghost%suggestion)
        call check(ghost%source == GHOST_SRC_LSP, "source is LSP", "wrong source")
    end if

    ! --- LSP results: bare-array shape, insertText preferred ---
    call ghost_clear(ghost)
    result_json = json_parse('[{"label":"shown","insertText":"couple"}]')
    call ghost_apply_lsp_result(ghost, result_json, "cou", 1, 8, .false.)
    call check(ghost_is_active(ghost), "LSP bare array applied", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "couple", "insertText preferred over label", &
                   ghost%suggestion)
    end if

    ! --- LSP result with no surviving item keeps prior suggestion ---
    call ghost_update_from_buffer(ghost, buffer, "myv", 1, 8)
    result_json = json_parse('{"items":[{"label":"$0 snippet"}]}')
    call ghost_apply_lsp_result(ghost, result_json, "myv", 1, 8, .false.)
    call check(ghost_is_active(ghost) .and. ghost%suggestion == "myvalue", &
               "word suggestion kept when LSP items all filtered", "lost")

    ! --- Include-directive context detection ---
    call cleanup_buffer(buffer)
    call init_buffer(buffer)
    call buffer_insert(buffer, 1, &
        '#include <floa' // char(10) // &
        '#include <sys/epo' // char(10) // &
        '#include "loc' // char(10) // &
        '#include <stdio.h>' // char(10) // &
        '  #  include <x' // char(10) // &
        'int x; // #include <floa' // char(10) // &
        '#define MAX <y')

    block
        logical :: inc

        call ghost_get_include_prefix(buffer, 1, 15, prefix, inc)
        call check(inc .and. prefix == "floa", "include <floa detected", prefix)

        ! Prefix is the segment after the last '/', matching clangd anchors
        call ghost_get_include_prefix(buffer, 2, 18, prefix, inc)
        call check(inc .and. prefix == "epo", "include <sys/epo segment", prefix)

        call ghost_get_include_prefix(buffer, 3, 14, prefix, inc)
        call check(inc .and. prefix == "loc", 'include "loc detected', prefix)

        ! Right after '<': in context with empty prefix
        call ghost_get_include_prefix(buffer, 1, 11, prefix, inc)
        call check(inc .and. prefix == "", "empty prefix right after <", prefix)

        ! Cursor after the closing '>': not in context
        call ghost_get_include_prefix(buffer, 4, 20, prefix, inc)
        call check(.not. inc, "closed <stdio.h> not in context", "in ctx")

        ! Blanks around '#' and the directive word are legal
        call ghost_get_include_prefix(buffer, 5, 16, prefix, inc)
        call check(inc .and. prefix == "x", "blanks after # accepted", prefix)

        ! '#' not opening the line: no context
        call ghost_get_include_prefix(buffer, 6, 26, prefix, inc)
        call check(.not. inc, "# inside comment not a directive", "in ctx")

        ! Non-include directive: no context
        call ghost_get_include_prefix(buffer, 7, 15, prefix, inc)
        call check(.not. inc, "#define is not include context", "in ctx")
    end block

    ! --- Header items pass the LSP filter only in include context ---
    call ghost_clear(ghost)
    result_json = json_parse('{"items":[{"label":" float.h>","insertText":"float.h>"}]}')
    call ghost_apply_lsp_result(ghost, result_json, "floa", 1, 15, .true.)
    call check(ghost_is_active(ghost), "header item accepted in include ctx", "inactive")
    if (ghost_is_active(ghost)) then
        call check(ghost%suggestion == "float.h>", "header insertText used", ghost%suggestion)
        call check(ghost_suffix(ghost) == "t.h>", "header suffix", ghost_suffix(ghost))
    end if

    ! Empty prefix in include ctx shows the whole first header item
    call ghost_clear(ghost)
    call ghost_apply_lsp_result(ghost, result_json, "", 1, 11, .true.)
    call check(ghost_is_active(ghost) .and. ghost%suggestion == "float.h>", &
               "empty include prefix shows whole item", "inactive")

    ! Outside include ctx the same item is rejected ('.' and '>')
    call ghost_clear(ghost)
    call ghost_apply_lsp_result(ghost, result_json, "floa", 1, 15, .false.)
    call check(.not. ghost_is_active(ghost), &
               "header item rejected outside include ctx", "active")

    ! Subdirectory paths are legal header items
    call ghost_clear(ghost)
    result_json = json_parse('{"items":[{"insertText":"sys/stat.h>","label":"x"}]}')
    call ghost_apply_lsp_result(ghost, result_json, "sy", 1, 13, .true.)
    call check(ghost_is_active(ghost) .and. ghost%suggestion == "sys/stat.h>", &
               "subdir header item accepted", "inactive")

    ! --- Clearing ---
    call ghost_clear(ghost)
    call check(.not. ghost_is_active(ghost), "ghost_clear deactivates", "active")

    call cleanup_buffer(buffer)

    print *, ""
    if (nfail == 0) then
        print *, "All ghost text tests passed!"
    else
        print *, nfail, "ghost text test(s) FAILED"
        stop 1
    end if

contains

    subroutine check(ok, name, got)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name, got

        if (ok) then
            print *, "PASS ", name
        else
            print *, "FAIL ", name, " (got: '", got, "')"
            nfail = nfail + 1
        end if
    end subroutine check

end program test_ghost_text
