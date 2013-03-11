!REAL FUNCTION NORM2(v)
!        !THIS IS MEANT TO BE PART OF F2008, UNCOMMENT IF YOU GET ERRORS
!        IMPLICIT NONE
!        REAL, INTENT(IN) :: v(:)
!        NORM2 = SQRT(SUM(v*v))
!END FUNCTION

SUBROUTINE gen_plane(center, aside, bside, res, pts)
        IMPLICIT NONE
!        INTERFACE
!                REAL FUNCTION NORM2(v)
!                        REAL, INTENT(IN) :: v(:)
!                END FUNCTION
!        END INTERFACE
        REAL, INTENT(IN) :: center(3), aside(3), bside(3)
        INTEGER, INTENT(IN) :: res(2)
        REAL, INTENT(OUT) :: pts(4,res(1)*res(2))
        REAL :: astep(3), bstep(3), area
        INTEGER :: i, j, n
        astep = aside/(res(1) - 1)
        bstep = bside/(res(2) - 1)
        area = (NORM2(aside)*NORM2(bside))/(res(1)*res(2))
        n = 1
        DO i = 0, res(1)-1
                DO j = 0, res(2)-1
                        pts(1:3,n) = center + (i*astep) - (aside/2) + (j*bstep) - (bside/2)
                        pts(4,n) = area
                        n = n+1
                END DO
        END DO
END SUBROUTINE gen_plane
