!REAL FUNCTION NORM2(v)
!        !THIS IS MEANT TO BE PART OF F2008, UNCOMMENT IF YOU GET ERRORS
!        IMPLICIT NONE
!        REAL, INTENT(IN) :: v(:)
!        NORM2 = SQRT(SUM(v*v))
!END FUNCTION

SUBROUTINE geom_plane(center, aside, bside, res, pts)
        IMPLICIT NONE
!        INTERFACE
!                REAL FUNCTION NORM2(v)
!                        REAL, INTENT(IN) :: v(:)
!                END FUNCTION
!        END INTERFACE
        REAL, INTENT(IN) :: center(3), aside(3), bside(3)
        INTEGER, INTENT(IN) :: res(2)
        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
        REAL :: astep(3), bstep(3), area
        INTEGER :: i, j, n
        astep = aside/(res(1) - 1)
        bstep = bside/(res(2) - 1)
        area = (NORM2(aside)*NORM2(bside))/(res(1)*res(2))
        ALLOCATE(pts(4,res(1)*res(2)))
        n = 1
        DO i = 0, res(1)-1
                DO j = 0, res(2)-1
                        pts(1:3,n) = center + (i*astep) - (aside/2) + (j*bstep) - (bside/2)
                        pts(4,n) = area
                        n = n+1
                END DO
        END DO
END SUBROUTINE

SUBROUTINE geom_line(center, line, res, pts)
        IMPLICIT NONE
        REAL, INTENT(IN) :: center(3), line(3)
        INTEGER, INTENT(IN) :: res
        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
        INTEGER :: i
        REAL :: step(3)
        step = line/(res - 1)
        ALLOCATE(pts(3,res))
        DO i = 0, res-1
                pts(:,i+1) = center + (i*step) - (line/2)
        END DO
END SUBROUTINE

SUBROUTINE geom_load(Path, isMeas, Npoints, pts)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN) :: Path
        LOGICAL, INTENT(IN) :: isMeas
        INTEGER, INTENT(IN) :: Npoints
        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
        IF (isMeas) THEN
                ALLOCATE(pts(3,Npoints))
        ELSE
                ALLOCATE(pts(4,Npoints))
        END IF
        OPEN(10, status='old', file=Path, form='unformatted')
        READ(10) pts
        CLOSE(10)
END SUBROUTINE
