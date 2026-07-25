! Build the prefix and suffix windows a fill-in-the-middle request needs.
!
! FIM is what separates this from prefix-only completion: the model is shown
! the code on BOTH sides of the caret, so it can close a brace it can see,
! respect a return type below, and stop where the existing code resumes.
!
! Windows are snapped to line boundaries. A prefix cut mid-statement gives
! base models a fragment they try to finish literally, which produces
! confident nonsense; a whole-line cut gives them a clean edge.
!
! Budgets are generous on purpose. prompt_eval measured 7-32 ms on GPU
! against 112+ ms of generation, so context length is not the latency driver
! -- num_predict is. Trimming context to save time would cost quality for
! nothing.
module completion_context_module
    use text_buffer_module, only: buffer_t, buffer_get_line, buffer_get_line_count
    use utf8_module, only: utf8_char_to_byte_index, utf8_char_count
    implicit none
    private

    public :: build_fim_context, context_line_after_cursor

    integer, parameter, public :: DEFAULT_PREFIX_BYTES = 6000
    integer, parameter, public :: DEFAULT_SUFFIX_BYTES = 2000

    integer, parameter :: MAX_SCAN_LINES = 400

contains

    ! Text from the caret to the end of its line. The sanitizer needs this to
    ! spot a completion that merely restates what is already there.
    function context_line_after_cursor(buffer, line_num, col) result(after)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col
        character(len=:), allocatable :: after
        character(len=:), allocatable :: line
        integer :: b

        after = ''
        line = buffer_get_line(buffer, line_num)
        if (len(line) == 0) return
        b = utf8_char_to_byte_index(line, col)
        if (b <= 0 .or. b > len(line)) return
        after = line(b:)
    end function context_line_after_cursor

    ! prefix : up to prefix_budget bytes of the lines before the caret, plus
    !          the caret's own line up to the caret
    ! suffix : the rest of the caret's line, then following lines, up to
    !          suffix_budget bytes
    subroutine build_fim_context(buffer, line_num, col, prefix_budget, &
                                 suffix_budget, prefix, suffix)
        type(buffer_t), intent(in) :: buffer
        integer, intent(in) :: line_num, col, prefix_budget, suffix_budget
        character(len=:), allocatable, intent(out) :: prefix, suffix
        character(len=:), allocatable :: line
        integer :: total_lines, i, b, used, first_line_kept, last_line_kept

        prefix = ''
        suffix = ''
        total_lines = buffer_get_line_count(buffer)
        if (line_num < 1 .or. line_num > total_lines) return

        ! ---- prefix: walk back from the caret line, whole lines only ----
        line = buffer_get_line(buffer, line_num)
        b = utf8_char_to_byte_index(line, col)
        if (b <= 0) b = len(line) + 1
        if (b > len(line) + 1) b = len(line) + 1

        prefix = line(1:b-1)
        used = len(prefix)
        first_line_kept = line_num

        do i = line_num - 1, max(1, line_num - MAX_SCAN_LINES), -1
            line = buffer_get_line(buffer, i)
            if (used + len(line) + 1 > prefix_budget) exit
            prefix = line // achar(10) // prefix
            used = used + len(line) + 1
            first_line_kept = i
        end do

        ! ---- suffix: rest of the caret line, then forward ----
        line = buffer_get_line(buffer, line_num)
        if (b <= len(line)) then
            suffix = line(b:)
        else
            suffix = ''
        end if
        used = len(suffix)
        last_line_kept = line_num

        do i = line_num + 1, min(total_lines, line_num + MAX_SCAN_LINES)
            line = buffer_get_line(buffer, i)
            if (used + len(line) + 1 > suffix_budget) exit
            suffix = suffix // achar(10) // line
            used = used + len(line) + 1
            last_line_kept = i
        end do

        ! A trailing newline on the suffix tells the model the file continues
        ! past what it can see, rather than that it must close everything.
        if (last_line_kept < total_lines) suffix = suffix // achar(10)
    end subroutine build_fim_context

end module completion_context_module
