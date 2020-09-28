MODULE mod_integration
  !
  ! sample (NOT production) code illustrating reverse communication
  ! numerical integrator
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: initialize_integration, integrate
  INTEGER, PUBLIC, PARAMETER :: rk = selected_real_kind(12, 100), &
       stat_done = 0, stat_continue = 1, stat_nonconverged = 2
  INTEGER, PARAMETER :: nintv_start = 128, itmax = 16
  REAL (rk), PARAMETER :: simp(2) = [ 2.0_rk, 4.0_rk ]
  REAL (rk), PARAMETER :: simpinc(4) = [ 0.0_rk, 4.0_rk, -2.0_rk, 4.0_rk ]

  INTEGER :: nintv, n_current, it_current
  REAL (KIND=rk) :: intv(2), res_current, epsval
  !
CONTAINS
  SUBROUTINE initialize_integration(a, b, eps, x)
    REAL (rk), INTENT (IN) :: a, b, eps
    REAL (rk), INTENT (OUT) :: x

    nintv = nintv_start
    intv(1) = a
    intv(2) = b
    n_current = 0
    it_current = 1
    x = a
    res_current = 0.0_rk
    epsval = eps
  END SUBROUTINE initialize_integration
  SUBROUTINE integrate(fval, x, result, stat)
    REAL (rk), INTENT (IN) :: fval
    REAL (rk), INTENT (OUT) :: x
    REAL (rk), INTENT (INOUT) :: result
    INTEGER, INTENT (OUT) :: stat
    !
    DOUBLE PRECISION :: absdiff, h
    !
    stat = stat_continue
    h = (intv(2)-intv(1))/nintv
    IF (n_current==0 .OR. n_current==nintv) THEN
       res_current = res_current + fval
    ELSE IF (it_current==1) THEN
       res_current = res_current + fval*simp(mod(n_current,2)+1)
    ELSE
       res_current = res_current + fval*simpinc(mod(n_current,4)+1)
    END IF
    n_current = n_current + 1
    x = intv(1) + n_current*h
    IF (n_current==nintv) THEN
       absdiff = 1.0_rk
       IF (it_current>1) absdiff = abs(result-h/3.0D0*res_current)
       result = h/3.0D0*res_current
       IF (absdiff<epsval*abs(result)) THEN
          stat = stat_done
       ELSE
          nintv = nintv*2
          it_current = it_current + 1
          IF (it_current>itmax) THEN
             stat = stat_nonconverged
          ELSE
             n_current = 1
             x = intv(1) + h/2
          END IF
       END IF
    END IF
  END SUBROUTINE integrate
END MODULE mod_integration
