!Transfer matrix generator
PROGRAM test
        IMPLICIT NONE
        INTERFACE
                SUBROUTINE geom_plane(center, aside, bside, res, pts)
                        REAL, INTENT(IN) :: center(3), aside(3), bside(3)
                        INTEGER, INTENT(IN) :: res(2)
                        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
                END SUBROUTINE geom_plane
                SUBROUTINE tm_gen(a, b, k, tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, INTENT(OUT) :: tm(SIZE(b,2), SIZE(a,2))
                END SUBROUTINE
                SUBROUTINE toend(emit, meas, k, p, cr)
                        REAL, INTENT(IN) :: emit(:,:), meas(:,:), k
                        COMPLEX, INTENT(IN) :: p(:)
                        COMPLEX, INTENT(INOUT) :: cr(:)
                END SUBROUTINE
                SUBROUTINE bounce(emA, emB, k, p, cr)
                        REAL, INTENT(IN) :: emA(:,:), emB(:,:), k
                        COMPLEX, INTENT(IN) :: p(:)
                        COMPLEX, INTENT(OUT) :: cr(:)
                END SUBROUTINE
        END INTERFACE
        REAL, PARAMETER :: PI = 3.141592653589793238462643383279502884197169399375
        REAL, PARAMETER :: c = 1481, rho = 1, RC = 0.2, omega = 7e6*2*PI
        REAL, PARAMETER :: k = omega/c, lambda = 2*PI*c/omega
        INTEGER, PARAMETER :: long_detail = 285, short_detail = 38, meas_detail = 81 !A little over 4 per wavelength
        REAL :: center(3) = (/ 0.01835, 0.0, 0.0 /), aside(3) = (/ 0.0, 0.015, 0.0 /), bside(3) = (/ 0.0, 0.0, 0.002 /)
        INTEGER :: res(2) = (/ long_detail, short_detail /)
        !REAL :: emitA(4,long_detail*short_detail), emitB(4,long_detail*short_detail), meas(4,meas_detail**2), r(meas_detail**2)
        REAL, ALLOCATABLE :: emitA(:,:), emitB(:,:), meas(:,:)
        REAL :: r(meas_detail**2)
        !COMPLEX :: tm(meas_detail**2, long_detail*short_detail), p(long_detail*short_detail) = 1, cr(meas_detail**2)
        COMPLEX :: p(long_detail*short_detail) = 1, cr(meas_detail**2)
        INTEGER :: pc, col
        CALL geom_plane(center, aside, bside, res, emitA)
        center = (/ -0.01835, 0.0, 0.0 /)
        aside = (/ 0.0, 0.015, 0.0 /)
        bside = (/ 0.0, 0.0, 0.002 /)
        CALL geom_plane(center, aside, bside, res, emitB)
        center = (/ 0.0, 0.0, 0.0 /)
        aside = (/ 0.0005, 0.0, 0.0 /)
        bside = (/ 0.0, 0.001, 0.0 /)
        CALL geom_plane(center, aside, bside, (/ meas_detail, meas_detail /), meas)
        !CALL tm_gen(emitA, meas, k, tm)
        !cr = MATMUL(tm, p)
        !CALL tm_gen(emitB, meas, k, tm)
        !cr = cr + MATMUL(tm, p)
        CALL toend(emitA, meas, k, p, cr)
        CALL toend(emitB, meas, k, p, cr)
        CALL bounce(emitA, emitB, k, p, p)
        p = p*COMPLEX(0.2,1.0/lambda)
        CALL toend(emitB, meas, k, p, cr)
        p = 1
        CALL bounce(emitB, emitA, k, p, p)
        p = p*COMPLEX(0.2, 1.0/lambda)
        CALL toend(emitA, meas, k, p, cr)
        r = ABS(cr)**2
        OPEN(10,file='fortrantrial.dat')
        DO pc = 0,meas_detail-1
                write(10,*) (r((pc*meas_detail)+col), col=1,meas_detail)
        END DO
        CLOSE(10)
END PROGRAM test

SUBROUTINE toend(emit, meas, k, p, cr)
        IMPLICIT NONE
        INTERFACE
                SUBROUTINE tm_gen(a, b, k, tm)
                        REAL, ALLOCATABLE, INTENT(IN) :: a(:,:), b(:,:)
                        REAL, INTENT(IN) :: k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
        END INTERFACE
        REAL, ALLOCATABLE, INTENT(IN) :: emit(:,:), meas(:,:)
        REAL, INTENT(IN) :: k
        COMPLEX, INTENT(IN) :: p(:)
        COMPLEX, INTENT(INOUT) :: cr(:)
        COMPLEX, ALLOCATABLE :: tm(:,:)
        CALL tm_gen(emit, meas, k, tm)
        cr = cr + MATMUL(tm, p)
        DEALLOCATE(tm)
END SUBROUTINE

SUBROUTINE bounce(emitA, emitB, k, p, cr)
        IMPLICIT NONE
        INTERFACE
                SUBROUTINE tm_gen(a, b, k, tm)
                        REAL, ALLOCATABLE, INTENT(IN) :: a(:,:), b(:,:)
                        REAL, INTENT(IN) :: k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
        END INTERFACE
        REAL, ALLOCATABLE, INTENT(IN) :: emitA(:,:), emitB(:,:)
        REAL, INTENT(IN) :: k
        COMPLEX, INTENT(IN) :: p(:)
        COMPLEX, INTENT(OUT) :: cr(:)
        COMPLEX, ALLOCATABLE :: tm(:,:)
        CALL tm_gen(emitA, emitB, k, tm)
        cr = MATMUL(tm, p)
        DEALLOCATE(tm)
END SUBROUTINE
