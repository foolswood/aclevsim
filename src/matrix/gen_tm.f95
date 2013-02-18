!Transfer matrix generator
PROGRAM tm_test
        IMPLICIT NONE

        INTERFACE
                SUBROUTINE gen_tm(a,b,k,tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE gen_tm
        END INTERFACE
        
        REAL, DIMENSION(4,2) :: a = 1, b = 3
        COMPLEX, ALLOCATABLE :: tm(:,:)
        
        PRINT *, size(a,1)
        CALL gen_tm(a,b,3.0,tm)
        PRINT *, size(tm,1), size(tm,2)
        DEALLOCATE(tm)
END PROGRAM tm_test


SUBROUTINE gen_tm(a, b, k, tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: k
        REAL, INTENT(IN) :: a(:,:), b(:,:)
        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
        INTEGER :: i, j
        REAL :: r
        ALLOCATE(tm(SIZE(a,2),SIZE(b,2)))
        DO j = 1, SIZE(a,2)
                DO i = 1, SIZE(b,2)
                        r = SQRT(SUM((a(1:3,i) - b(1:3, j))*(a(1:3,i) - b(1:3, j)))) !USE NORM2?
                        tm(i,j) = a(4,i) * EXP(CMPLX(0,-k*r)) / r
                END DO
        END DO
END SUBROUTINE gen_tm
