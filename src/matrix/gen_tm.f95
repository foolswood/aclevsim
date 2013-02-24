!Transfer matrix generator
PROGRAM tm_test
        IMPLICIT NONE

        INTERFACE
                SUBROUTINE gen_tm(a,b,k,tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE gen_tm
                SUBROUTINE gen_plane(x, dims, res, pts)
                        REAL, INTENT(IN) :: x, dims(2)
                        INTEGER, INTENT(IN) :: res(2)
                        REAL, INTENT(OUT) :: pts(4,res(1)*res(2))
                END SUBROUTINE gen_plane
        END INTERFACE
        
        REAL, DIMENSION(4,2) :: a = 1, b = 3
        COMPLEX, ALLOCATABLE :: tm(:,:)

        REAL :: dims(2) = 1
        INTEGER :: res(2) = 2
        REAL :: plane(4,4)
        
        PRINT *, size(a,1)
        CALL gen_tm(a,b,3.0,tm)
        PRINT *, size(tm,1), size(tm,2)
        DEALLOCATE(tm)

        CALL gen_plane(0.0, dims, res, plane)
        PRINT *, "GONE THROUGH"
        PRINT *, plane
END PROGRAM tm_test

SUBROUTINE gen_tm(a, b, k, tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: a(:,:), b(:,:), k
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

SUBROUTINE gen_plane(x, dims, res, pts)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x, dims(2)
        INTEGER, INTENT(IN) :: res(2)
        REAL, INTENT(OUT) :: pts(4,res(1)*res(2))
        REAL :: y, z, a, steps(2)
        INTEGER :: i
        y = -dims(1)/2
        z = -dims(2)/2
        steps = dims/res
        a = steps(1)*steps(2)
        DO i = 1, res(1)*res(2)
                pts(:,i) = (/ x, y, z, a/)
                y = y + steps(1)
                IF (y > dims(1)/2) THEN
                        y = -dims(1)/2
                        z = z + steps(2)
                END IF
        END DO
END SUBROUTINE gen_plane
