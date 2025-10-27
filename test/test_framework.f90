module test_framework
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    private

    public :: test_suite, test_case, assert_equals, assert_true, assert_false
    public :: run_tests, test_summary

    type :: test_result
        logical :: passed = .true.
        character(len=256) :: message = ""
    end type test_result

    type :: test_case
        character(len=128) :: name = ""
        procedure(test_proc), pointer, nopass :: test_procedure => null()
        type(test_result) :: result
    end type test_case

    type :: test_suite
        character(len=128) :: name = ""
        type(test_case), allocatable :: tests(:)
        integer :: num_tests = 0
        integer :: num_passed = 0
        integer :: num_failed = 0
    end type test_suite

    abstract interface
        subroutine test_proc()
        end subroutine test_proc
    end interface

    ! Global test context for assertions - using static allocation
    type(test_result), target :: global_test_result
    logical :: test_active = .false.

contains

    subroutine run_tests(suite)
        type(test_suite), intent(inout) :: suite
        integer :: i

        write(output_unit, '(A)') repeat("=", 60)
        write(output_unit, '(A,A)') "Running test suite: ", trim(suite%name)
        write(output_unit, '(A)') repeat("=", 60)

        suite%num_tests = size(suite%tests)
        suite%num_passed = 0
        suite%num_failed = 0

        do i = 1, suite%num_tests
            call run_single_test(suite%tests(i))

            if (suite%tests(i)%result%passed) then
                suite%num_passed = suite%num_passed + 1
                write(output_unit, '(A,A,A)') "✓ ", trim(suite%tests(i)%name), " PASSED"
            else
                suite%num_failed = suite%num_failed + 1
                write(error_unit, '(A,A,A)') "✗ ", trim(suite%tests(i)%name), " FAILED"
                if (len_trim(suite%tests(i)%result%message) > 0) then
                    write(error_unit, '(A,A)') "  Reason: ", trim(suite%tests(i)%result%message)
                end if
            end if
        end do

        call test_summary(suite)
    end subroutine run_tests

    subroutine run_single_test(test)
        type(test_case), intent(inout) :: test

        ! Copy test result to global for assertions
        global_test_result%passed = .true.
        global_test_result%message = ""
        test_active = .true.

        ! Run the test
        if (associated(test%test_procedure)) then
            call test%test_procedure()
            ! Copy result back
            test%result = global_test_result
        else
            test%result%passed = .false.
            test%result%message = "No test procedure defined"
        end if

        ! Clean up
        test_active = .false.
    end subroutine run_single_test

    subroutine test_summary(suite)
        type(test_suite), intent(in) :: suite

        write(output_unit, '(A)') repeat("-", 60)
        write(output_unit, '(A,I0,A,I0,A,I0,A)') &
            "Tests run: ", suite%num_tests, &
            ", Passed: ", suite%num_passed, &
            ", Failed: ", suite%num_failed

        if (suite%num_failed == 0) then
            write(output_unit, '(A)') "All tests passed! ✓"
        else
            write(error_unit, '(A,I0,A)') "FAILURE: ", suite%num_failed, " tests failed"
        end if
        write(output_unit, '(A)') repeat("=", 60)
    end subroutine test_summary

    ! Assertion utilities
    subroutine assert_equals(expected, actual, message)
        integer, intent(in) :: expected, actual
        character(len=*), intent(in), optional :: message
        character(len=256) :: fail_msg

        if (.not. test_active) return

        if (expected /= actual) then
            global_test_result%passed = .false.
            if (present(message)) then
                write(fail_msg, '(A,A,I0,A,I0)') trim(message), &
                    ": Expected ", expected, " but got ", actual
            else
                write(fail_msg, '(A,I0,A,I0)') "Expected ", expected, " but got ", actual
            end if
            global_test_result%message = trim(fail_msg)
        end if
    end subroutine assert_equals

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message

        if (.not. test_active) return

        if (.not. condition) then
            global_test_result%passed = .false.
            if (present(message)) then
                global_test_result%message = trim(message)
            else
                global_test_result%message = "Assertion failed: expected true"
            end if
        end if
    end subroutine assert_true

    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message

        if (.not. test_active) return

        if (condition) then
            global_test_result%passed = .false.
            if (present(message)) then
                global_test_result%message = trim(message)
            else
                global_test_result%message = "Assertion failed: expected false"
            end if
        end if
    end subroutine assert_false

end module test_framework