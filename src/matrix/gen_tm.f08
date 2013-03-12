SUBROUTINE gen_tm(a, b, k, tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: a(:,:), b(:,:), k
        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
        INTEGER :: i, j
        REAL :: r
        ALLOCATE(tm(SIZE(b,2),SIZE(a,2)))
        !$OMP PARALLEL SHARED(a, b, k, tm) PRIVATE(i,j,r)
        !$OMP DO
        DO i = 1, SIZE(a,2)
                DO j = 1, SIZE(b,2)
                        r = NORM2(b(1:3,j) - a(1:3,i))
                        tm(j,i) = a(4,i) * EXP(CMPLX(0,-k*r)) / r
                END DO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
END SUBROUTINE gen_tm
