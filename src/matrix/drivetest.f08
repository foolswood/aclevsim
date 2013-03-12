!Transfer matrix generator
PROGRAM test
        IMPLICIT NONE
        INTERFACE
                SUBROUTINE gen_plane(center, aside, bside, res, pts)
                        REAL, INTENT(IN) :: center(3), aside(3), bside(3)
                        INTEGER, INTENT(IN) :: res(2)
                        REAL, INTENT(OUT) :: pts(4,res(1)*res(2))
                END SUBROUTINE gen_plane
                SUBROUTINE gen_tm(a, b, k, tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
                SUBROUTINE save_tm(Id, k, tm)
                        REAL, INTENT(IN) :: k
                        CHARACTER(LEN=*), INTENT(IN) :: Id
                        COMPLEX, ALLOCATABLE, INTENT(IN) :: tm(:,:)
                END SUBROUTINE
                SUBROUTINE load_tm(Id, asize, bsize, k, tm)
                        REAL, INTENT(IN) :: k
                        INTEGER, INTENT(IN) :: asize, bsize
                        CHARACTER(LEN=*), INTENT(IN) :: Id
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
        END INTERFACE
        REAL, PARAMETER :: PI = 3.141592653589793238462643383279502884197169399375
        REAL, PARAMETER :: c = 1481, rho = 1, RC = 0.2, omega = 7e6*2*PI
        REAL, PARAMETER :: k = omega/c
        INTEGER, PARAMETER :: long_detail = 285, short_detail = 38, meas_detail = 81 !A little over 4 per wavelength
        REAL :: center(3) = (/ 0.01835, 0.0, 0.0 /), aside(3) = (/ 0.0, 0.015, 0.0 /), bside(3) = (/ 0.0, 0.0, 0.002 /)
        INTEGER :: res(2) = (/ long_detail, short_detail /)
        REAL :: emitA(4,long_detail*short_detail), emitB(4,long_detail*short_detail), meas(4,meas_detail**2), r(meas_detail**2)
        !COMPLEX :: tm(meas_detail**2, long_detail*short_detail), p(long_detail*short_detail) = 1, cr(meas_detail**2)
        COMPLEX :: p(long_detail*short_detail) = 1, cr(meas_detail**2)
        INTEGER :: pc, col
        COMPLEX, ALLOCATABLE :: tm(:,:)
        CALL gen_plane(center, aside, bside, res, emitA)
        center = (/ -0.01835, 0.0, 0.0 /)
        aside = (/ 0.0, 0.015, 0.0 /)
        bside = (/ 0.0, 0.0, 0.002 /)
        CALL gen_plane(center, aside, bside, res, emitB)
        center = (/ 0.0, 0.0, 0.0 /)
        aside = (/ 0.001, 0.0, 0.0 /)
        bside = (/ 0.0, 0.001, 0.0 /)
        CALL gen_plane(center, aside, bside, (/ meas_detail, meas_detail /), meas)
        !CALL gen_tm(emitA, meas, k, tm)
        !cr = MATMUL(tm, p)
        !CALL save_tm('a', 'b', k, tm)
        !DEALLOCATE(tm)
        pc = (meas_detail**2)/2
        CALL gen_tm(emitB, meas(:,:pc), k, tm)
        cr(:pc) = MATMUL(tm, p)
        DEALLOCATE(tm)
        CALL gen_tm(emitB, meas(:,pc:), k, tm)
        cr(pc:) = MATMUL(tm, p)
        DEALLOCATE(tm)
        CALL load_tm('ab', SIZE(emitA,2), SIZE(meas,2), k, tm)
        cr = cr + MATMUL(tm, p)
        r = ABS(cr)**2
        OPEN(10,file='testresult.dat')
        DO pc = 0,meas_detail-1
                write(10,*) (r((pc*meas_detail)+col), col=1,meas_detail)
        END DO
        CLOSE(10)
END PROGRAM test

SUBROUTINE save_tm(Id, k, tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: k
        CHARACTER(LEN=*), INTENT(IN) :: Id
        COMPLEX, ALLOCATABLE, INTENT(IN) :: tm(:,:)
        OPEN(10, status='replace', file=Id, form='unformatted')
        WRITE(10) tm
        CLOSE(10)
END SUBROUTINE

SUBROUTINE load_tm(Id, asize, bsize, k, tm)
        IMPLICIT NONE
        REAL, INTENT(IN) :: k
        INTEGER, INTENT(IN) :: asize, bsize
        CHARACTER(LEN=*), INTENT(IN) :: Id
        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
        ALLOCATE(tm(bsize,asize))
        OPEN(10, status='old', file=Id, form='unformatted')
        READ(10) tm
        CLOSE(10)
END SUBROUTINE
