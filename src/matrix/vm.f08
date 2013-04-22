!Transfer matrix high level VM
PROGRAM vm
        IMPLICIT NONE
        TYPE geom
                REAL, ALLOCATABLE :: geom(:,:)
        END TYPE
        TYPE cvector
                COMPLEX, ALLOCATABLE :: v(:)
        END TYPE
        INTERFACE
                SUBROUTINE geom_plane(center, aside, bside, res, pts)
                        REAL, INTENT(IN) :: center(3), aside(3), bside(3)
                        INTEGER, INTENT(IN) :: res(2)
                        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
                END SUBROUTINE
                SUBROUTINE geom_line(center, line, res, pts)
                        REAL, INTENT(IN) :: center(3), line(3)
                        INTEGER, INTENT(IN) :: res
                        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
                END SUBROUTINE
                SUBROUTINE geom_load(Path, isMeas, Npoints, pts)
                        CHARACTER(LEN=*), INTENT(IN) :: Path
                        LOGICAL, INTENT(IN) :: isMeas
                        INTEGER, INTENT(IN) :: Npoints
                        REAL, ALLOCATABLE, INTENT(OUT) :: pts(:,:)
                END SUBROUTINE
                SUBROUTINE tm_gen(a, b, k, tm)
                        REAL, INTENT(IN) :: a(:,:), b(:,:), k
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
                SUBROUTINE tm_save(Path, tm)
                        CHARACTER(LEN=*), INTENT(IN) :: Path
                        COMPLEX, ALLOCATABLE, INTENT(IN) :: tm(:,:)
                END SUBROUTINE
                SUBROUTINE tm_load(Path, asize, bsize, tm)
                        INTEGER, INTENT(IN) :: asize, bsize
                        CHARACTER(LEN=*), INTENT(IN) :: Path
                        COMPLEX, ALLOCATABLE, INTENT(OUT) :: tm(:,:)
                END SUBROUTINE
                SUBROUTINE result_save(Path, cr)
                        CHARACTER(LEN=*), INTENT(IN) :: Path
                        COMPLEX, ALLOCATABLE, INTENT(IN) :: cr(:)
                END SUBROUTINE
        END INTERFACE
        !Configuration constants
        INTEGER, PARAMETER :: n_geoms = 4, n_cv = 6
        !Program state variables
        REAL :: k, lambda
        REAL :: center(3), avec(3), bvec(3)
        INTEGER :: res(2)
        TYPE(geom) :: geoms(n_geoms)
        TYPE(cvector) :: cv(n_cv)
        COMPLEX, ALLOCATABLE :: tm(:,:)
        !Control system
        CHARACTER(LEN=16) :: cmd
        CHARACTER(LEN=256) :: path
        INTEGER :: i, j
        REAL :: r, s
        WRITE(6,*) "geoms", n_geoms
        WRITE(6,*) "cvs", n_cv
        DO
                READ(5,*) cmd
                SELECT CASE (cmd)
                        CASE ("quit")
                                WRITE(6,*) "ok"
                                EXIT
                        CASE ("setup")
                                WRITE(6,*) "k, lambda"
                                READ(5,*) k, lambda
                        CASE ("geom_plane")
                                WRITE(6,*) "id, center, avec, bvec, res"
                                READ(5,*) i, center, avec, bvec, res
                                CALL geom_plane(center, avec, bvec, res, geoms(i)%geom)
                        CASE ("geom_line")
                                WRITE(6,*) "id, center, avec, res"
                                READ(5,*) i, center, avec, res(1)
                                CALL geom_line(center, avec, res(1), geoms(i)%geom)
                        CASE ("geom_load")
                                WRITE(6,*) "id, path, nPoints" !isMeas
                                READ(5,*) i, path, j
                                CALL geom_load(path, .TRUE., j, geoms(i)%geom)
                        CASE ("tm_gen")
                                WRITE(6,*) "geomIdA, geomIdB"
                                READ(5,*) i, j
                                CALL tm_gen(geoms(i)%geom, geoms(j)%geom, k, tm)
                        CASE ("tm_save")
                                WRITE(6,*) "path"
                                READ(5,*) path
                                CALL tm_save(path, tm)
                        CASE ("tm_load")
                                WRITE(6,*) "path, lenA, lenB"
                                READ(5,*) path, i, j
                                CALL tm_load(path, i, j, tm)
                        CASE ("tm_use")
                                WRITE(6,*) "inCVId, outCVId"
                                READ(5,*) i, j
                                ALLOCATE(cv(j)%v(SIZE(tm,2)))
                                cv(j)%v = MATMUL(tm, cv(i)%v)
                        CASE ("cv_const")
                                WRITE(6,*) "CVId, length, value"
                                READ(5,*) i, j, r
                                ALLOCATE(cv(i)%v(j))
                                cv(i)%v = r
                        CASE ("cv_mult")
                                WRITE(6,*) "CVId, real, imag"
                                READ(5,*) i, r, s
                                cv(i)%v = cv(i)%v * CMPLX(r,s)
                        CASE ("cv_stack")
                                WRITE(6,*) "CVId_in1, CVId_in2, CVId_out"
                                READ(5,*) res, i
                                ALLOCATE(cv(i)%v(SIZE(cv(res(1))%v) + SIZE(cv(res(2))%v)))
                                cv(i)%v(:SIZE(cv(res(1))%v)) = cv(res(1))%v
                                cv(i)%v(SIZE(cv(res(1))%v):) = cv(res(2))%v
                        CASE ("cv_add")
                                WRITE(6,*) "CVAcc, CVIn"
                                READ(5,*) i, j
                                cv(i)%v = cv(i)%v + cv(j)%v
                        CASE ("free")
                                WRITE(6,*) "Id (+for cv, -for path, 0 for tm)"
                                READ(5,*) i
                                IF (i > 0) THEN
                                        DEALLOCATE(cv(i)%v)
                                ELSE IF (i < 0) THEN
                                        DEALLOCATE(geoms(-i)%geom)
                                ELSE
                                        DEALLOCATE(tm)
                                END IF
                        CASE ("result")
                                WRITE(6,*) "CVId, Path"
                                READ(5,*) i, path
                                CALL result_save(path, cv(i)%v)
                        CASE DEFAULT
                                WRITE(6,*) "unrecognised command: ", cmd
                                CYCLE
                END SELECT
                WRITE(6,*) "ok"
        END DO
END PROGRAM

SUBROUTINE result_save(Path, cr)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: Path
        COMPLEX, ALLOCATABLE, INTENT(IN) :: cr(:)
        INTEGER :: reclen
        REAL :: r(SIZE(cr))
        r = ABS(cr)**2
        INQUIRE(iolength=reclen)r
        OPEN(10, file=Path, form='unformatted', access='direct', recl=reclen)
        WRITE(10,rec=1) r
        CLOSE(10)
END SUBROUTINE
