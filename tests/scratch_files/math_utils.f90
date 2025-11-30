module math_utils
    ! Simple math utilities for testing LSP features
    !
    ! Test:
    ! - Syntax highlighting (keywords, comments, strings)
    ! - Go to Definition (F12 on subroutine calls)
    ! - Find References (Shift+F12 on subroutine names)
    ! - Document Symbols (Ctrl+Shift+O to see module structure)
    ! - Formatting (Shift+Alt+F)

    implicit none
    private

    public :: vector_add, vector_dot, vector_magnitude
    public :: matrix_multiply

contains

    subroutine vector_add(a, b, result, n)
        ! Add two vectors element-wise
        integer, intent(in) :: n
        real, intent(in) :: a(n), b(n)
        real, intent(out) :: result(n)
        integer :: i

        do i = 1, n
            result(i) = a(i) + b(i)
        end do
    end subroutine vector_add

    function vector_dot(a, b, n) result(dot_product)
        ! Calculate dot product of two vectors
        integer, intent(in) :: n
        real, intent(in) :: a(n), b(n)
        real :: dot_product
        integer :: i

        dot_product = 0.0
        do i = 1, n
            dot_product = dot_product + a(i) * b(i)
        end do
    end function vector_dot

    function vector_magnitude(vec, n) result(magnitude)
        ! Calculate magnitude of a vector
        integer, intent(in) :: n
        real, intent(in) :: vec(n)
        real :: magnitude
        integer :: i
        real :: sum_squares

        sum_squares = 0.0
        do i = 1, n
            sum_squares = sum_squares + vec(i)**2
        end do
        magnitude = sqrt(sum_squares)
    end function vector_magnitude

    subroutine matrix_multiply(A, B, C, m, n, p)
        ! Multiply two matrices: C = A * B
        ! A is m x n, B is n x p, C is m x p
        integer, intent(in) :: m, n, p
        real, intent(in) :: A(m, n), B(n, p)
        real, intent(out) :: C(m, p)
        integer :: i, j, k

        ! Intentional error: missing initialization
        do i = 1, m
            do j = 1, p
                ! ERROR: C(i,j) not initialized before use
                do k = 1, n
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end subroutine matrix_multiply

    ! Intentional error function for diagnostics
    subroutine broken_routine(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: temp

        temp = x * 2.0
        ! ERROR: using undefined variable
        y = temp + undefined_var
    end subroutine broken_routine

end module math_utils
