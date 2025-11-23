program test_program
    ! Test program using math_utils module
    !
    ! Test cross-file navigation:
    ! - F12 on 'vector_add' should jump to math_utils.f90
    ! - Shift+F12 on 'vector_dot' should show all usages
    ! - Ctrl+Shift+T and search "vector" to find all vector functions

    use math_utils
    implicit none

    integer, parameter :: n = 3
    real :: a(n), b(n), result(n)
    real :: dot_prod, mag
    integer :: i

    ! Initialize vectors
    a = [1.0, 2.0, 3.0]
    b = [4.0, 5.0, 6.0]

    ! Test vector addition
    ! F12 on vector_add should jump to math_utils.f90
    call vector_add(a, b, result, n)

    print *, 'Vector A:', a
    print *, 'Vector B:', b
    print *, 'A + B   :', result

    ! Test dot product
    ! F12 on vector_dot should jump to definition
    dot_prod = vector_dot(a, b, n)
    print *, 'A · B   :', dot_prod

    ! Test magnitude
    mag = vector_magnitude(a, n)
    print *, '|A|     :', mag

    ! Intentional error - missing variable declaration
    ! ERROR: 'undefined_result' is not declared
    undefined_result = dot_prod * 2.0
    print *, 'Result  :', undefined_result

end program test_program
