!Transfer matrix generator
PROGRAM tm_test
        IMPLICIT NONE

        INTERFACE
                FUNCTION gen_tm(a,b,k) RESULT(tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, ALLOCATABLE :: tm(:,:)
                END FUNCTION gen_tm
        END INTERFACE
        
        REAL, DIMENSION(4,2) :: a = 1, b = 3
        COMPLEX, ALLOCATABLE :: tm(:,:)
        
        PRINT *, size(a,1)
        tm = gen_tm(a,b,3.0)

        !DEALLOCATE(tm)

END PROGRAM tm_test


FUNCTION gen_tm(a, b, k) RESULT(tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: k
        REAL, INTENT(IN) :: a(:,:), b(:,:)
        COMPLEX, ALLOCATABLE :: tm(:,:)
        INTEGER :: i, j
        !REAL :: r
        print *, ALLOCATED(tm)
        ALLOCATE(tm(SIZE(a,2),SIZE(b,2)))
        PRINT *, "tm size is:"
        PRINT *, SIZE(tm,1), SIZE(tm,2)
        PRINT *, "I", "J"
        print *, ALLOCATED(tm)
        DO j = 1, SIZE(a,2)
                DO i = 1, SIZE(b,2)
                        PRINT *, i, j
                        tm(i,j) = 1.0
                        !gen_tm(i,j) = a(4,i) * EXP(CMPLX(0,-k*r)) / r
                END DO
        END DO
        PRINT *, "MADE IT!"
END FUNCTION gen_tm
