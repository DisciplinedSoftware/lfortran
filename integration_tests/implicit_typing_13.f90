! Regression test: with --implicit-typing, a DIMENSION (or DATA)
! statement naming an entity before its type declaration, inside a
! scope that has IMPLICIT NONE, declared the entity by the implicit
! rules that IMPLICIT NONE had switched off, and the type declaration
! that followed was then rejected as a redeclaration.
subroutine fill(h, n)
    implicit none
    integer :: n
    integer :: i
    dimension h(n)
    double precision h
    dimension w(2)
    data w/3, 4/
    integer w
    do i = 1, n
        h(i) = 2.5d0 * i + w(1) - w(2)
    end do
end subroutine

program implicit_typing_13
    implicit none
    double precision :: a(4)
    call fill(a, 4)
    if (abs(a(1) - 1.5d0) > 1d-12) error stop
    if (abs(a(4) - 9.0d0) > 1d-12) error stop
end program
