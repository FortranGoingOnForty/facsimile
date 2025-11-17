program test_syntax
    ! Test program for syntax highlighting
    use iso_fortran_env, only: int32, real64
    implicit none

    integer :: i, n = 10
    real(real64) :: pi = 3.14159265359
    character(len=20) :: greeting = "Hello, Fortran!"

    ! Print greeting
    print *, greeting

    ! Calculate factorial
    print *, "Factorial of", n, "is", factorial(n)

    ! Loop example
    do i = 1, 5
        if (mod(i, 2) == 0) then
            print *, i, "is even"
        else
            print *, i, "is odd"
        end if
    end do

contains

    recursive function factorial(n) result(fact)
        integer, intent(in) :: n
        integer :: fact

        if (n <= 1) then
            fact = 1
        else
            fact = n * factorial(n - 1)
        end if
    end function factorial

end program test_syntax