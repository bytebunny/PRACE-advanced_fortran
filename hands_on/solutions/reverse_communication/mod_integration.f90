MODULE mod_integration
  !
  ! sample (NOT production) code illustrating thread safe
  ! reverse communication numerical integrator
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: initialize_integration, integrate
  INTEGER, PUBLIC, PARAMETER :: rk = selected_real_kind(12, 100), &
       stat_done = 0, stat_continue = 1, stat_nonconverged = 2
  INTEGER, PARAMETER :: nintv_start = 128, itmax = 16
  REAL (rk), PARAMETER :: simp(2) = [ 2.0_rk, 4.0_rk ]
  REAL (rk), PARAMETER :: simpinc(4) = [ 0.0_rk, 4.0_rk, -2.0_rk, 4.0_rk &
       ]

  TYPE, PUBLIC :: integrator
     INTEGER :: nintv, n_current, it_current
     REAL (KIND=rk) :: intv(2), res_current, epsval
  END TYPE integrator
  !
CONTAINS
  TYPE (integrator) FUNCTION initialize_integration(a, b, eps, x)
    REAL (rk), INTENT (IN) :: a, b, eps
    REAL (rk), INTENT (OUT) :: x

    initialize_integration%nintv = nintv_start
    initialize_integration%intv(1) = a
    initialize_integration%intv(2) = b
    initialize_integration%n_current = 0
    initialize_integration%it_current = 1
    initialize_integration%res_current = 0.0_rk
    initialize_integration%epsval = eps
    x = a
  END FUNCTION initialize_integration
  SUBROUTINE integrate(this, fval, x, result, stat)
    TYPE (integrator), INTENT (INOUT) :: this
    REAL (rk), INTENT (IN) :: fval
    REAL (rk), INTENT (OUT) :: x
    REAL (rk), INTENT (INOUT) :: result
    INTEGER, INTENT (OUT) :: stat
    !
    DOUBLE PRECISION :: absdiff, h
    !
    stat = stat_continue
    h = (this%intv(2)-this%intv(1))/this%nintv
    IF (this%n_current==0 .OR. this%n_current==this%nintv) THEN
       this%res_current = this%res_current + fval
    ELSE IF (this%it_current==1) THEN
       this%res_current = this%res_current + fval*simp(mod(this%n_current,2)+1)
    ELSE
       this%res_current = this%res_current + fval*simpinc(mod(this%n_current,4)+1)
    END IF
    this%n_current = this%n_current + 1
    x = this%intv(1) + this%n_current*h
    IF (this%n_current==this%nintv) THEN
       absdiff = 1.0_rk
       IF (this%it_current>1) absdiff = abs(result-h/3.0D0*this%res_current)
       result = h/3.0D0*this%res_current
       IF (absdiff<this%epsval*abs(result)) THEN
          stat = stat_done
       ELSE
          this%nintv = this%nintv*2
          this%it_current = this%it_current + 1
          IF (this%it_current>itmax) THEN
             stat = stat_nonconverged
          ELSE
             this%n_current = 1
             x = this%intv(1) + h/2
          END IF
       END IF
    END IF
  END SUBROUTINE integrate
END MODULE mod_integration
