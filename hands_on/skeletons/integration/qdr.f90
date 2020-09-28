    MODULE qdr
!
! sample (NOT production) code illustrating object-oriented 
! numerical integrator
!
      USE fgsl
      IMPLICIT NONE
      PRIVATE
      INTEGER, PUBLIC, PARAMETER :: rk = selected_real_kind(12, 100)
      INTEGER, PUBLIC, PARAMETER :: qdr_ok = 0
      INTEGER, PUBLIC, PARAMETER :: qdr_not_converged = 1
      INTEGER, PUBLIC, PARAMETER :: qdr_no_class = 2
      PUBLIC :: integral_1d
      TYPE, PUBLIC, ABSTRACT :: qdr_fun
        REAL (KIND=rk) :: eps = 1.0D-12
        INTEGER :: itmax = 10
!   user-defined data elements in extension
      CONTAINS
        PROCEDURE (qdr_if), DEFERRED :: eval
      END TYPE
!
! critique from efficiency point of view:
!   would prefer a vector-valued function of a vector x
!   to reduce virtual function call overhead
      ABSTRACT INTERFACE
        REAL (KIND=rk) FUNCTION qdr_if(this, x)
          IMPORT :: qdr_fun, rk
          CLASS (qdr_fun) :: this
          REAL (KIND=rk), INTENT (IN) :: x
        END FUNCTION
      END INTERFACE

    CONTAINS

      REAL (KIND=rk) FUNCTION integral_1d(intv, fun, status)
        USE, INTRINSIC :: iso_c_binding
        REAL (KIND=rk), INTENT (IN) :: intv(2)
        CLASS (qdr_fun), INTENT (IN), TARGET :: fun
        INTEGER, OPTIONAL, INTENT (OUT) :: status
!
        INTEGER :: i, imx, istat_gsl
        REAL (KIND=rk) :: result, intv_loc(2), abserr
        TYPE (c_ptr) :: params
!
        result = default(intv, fun, status)
        integral_1d = result
      END FUNCTION

      REAL (KIND=rk) FUNCTION default(intv, this, status)
        REAL (KIND=rk), INTENT (IN) :: intv(2)
        CLASS (qdr_fun), INTENT (IN) :: this
        INTEGER, INTENT (OUT), OPTIONAL :: status
!
        INTEGER, PARAMETER :: nintv_start = 128
        DOUBLE PRECISION, PARAMETER :: simp(2) = (/ 2.0D0, 4.0D0 /)
        DOUBLE PRECISION, PARAMETER :: simpinc(4) = (/ 0.0D0, 4.0D0, -2.0D0, &
          4.0D0 /)
        INTEGER :: i, it, nintv, loc_status
        DOUBLE PRECISION :: result, res_it, res_it_prev, absdiff, x, h

        x = intv(2)
        res_it = this%eval(x)
!    write(6, *) 'right edge ',x,' has value ',res_it
        x = intv(1)
        res_it = res_it + this%eval(x)
        nintv = nintv_start
        h = (intv(2)-intv(1))/nintv
        DO i = 1, nintv - 1
          x = intv(1) + i*h
          res_it = res_it + this%eval(x)*simp(mod(i,2)+1)
        END DO
        absdiff = 1.0D0
        result = h/3.0D0*res_it
        it = 0
        loc_status = qdr_ok
        DO WHILE (absdiff>this%eps*abs(result))
          it = it + 1
          res_it_prev = res_it
!      write(6, *) 'result for ',nintv,' interv. is ',result, &
!           ' absdiff: ', absdiff
          nintv = nintv*2
          h = (intv(2)-intv(1))/nintv
          DO i = 1, nintv - 1
            IF (mod(i,4)==0) CYCLE
            x = intv(1) + i*h
            res_it = res_it + this%eval(x)*simpinc(mod(i,4)+1)
          END DO
          absdiff = abs(result-h/3.0D0*res_it)
          result = h/3.0D0*res_it
          IF (it>=this%itmax) THEN
            loc_status = qdr_not_converged
            EXIT
          END IF
!      write(6, *) 'result for ',nintv,' interv. is ',result, &
!           ' absdiff: ', absdiff
        END DO
        default = result
        IF (present(status)) status = loc_status
      END FUNCTION

    END MODULE
