SUBROUTINE tm_gen(a, b, k, tm)
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
END SUBROUTINE

SUBROUTINE tm_save(Path, tm)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: Path
        COMPLEX, ALLOCATABLE, INTENT(IN) :: tm(:,:)
        OPEN(10, status='replace', file=Path, form='unformatted')
        WRITE(10) tm
        CLOSE(10)
END SUBROUTINE

SUBROUTINE tm_load(Path, asize, bsize, tm)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: asize, bsize
        CHARACTER(LEN=*), INTENT(IN) :: Path
        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
        ALLOCATE(tm(bsize,asize))
        OPEN(10, status='old', file=Path, form='unformatted')
        READ(10) tm
        CLOSE(10)
END SUBROUTINE
