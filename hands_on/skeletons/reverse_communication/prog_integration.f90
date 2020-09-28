PROGRAM prog_integration
  USE mod_integration
  IMPLICIT NONE
  INTEGER, PARAMETER :: npar = 6
  INTEGER :: i
  REAL (rk) :: params(npar) = [ (real(i,rk)*0.134_rk,i=1,npar) ]
  REAL (rk) :: results(npar)
  REAL (rk) :: x, fval
  INTEGER :: stats(npar)
  ! OpenMP not possible because global variables would be
  ! accessed from multiple threads
  !NO$omp parallel do private(x,fval)
  DO i = 1, npar
     !
     CALL initialize_integration(1.0_rk, 2.0_rk, 1.0E-7_rk, x)
     DO
        CALL user_fun(x, params(i), fval)
        CALL integrate(fval, x, results(i), stats(i))
        IF (stats(i)/=stat_continue) EXIT
     END DO
  END DO
  !NO$omp end parallel do
  DO i = 1, npar
     fval = 2.0_rk/params(i)*(atan(exp(params(i)*2.0_rk))-atan(exp(params(i))))
     WRITE (*, &
          '(''Result: '',E17.10,'' Status: '',i0,'' Reference: '',E17.10)') &
          results(i), stats(i), fval
  END DO
CONTAINS
  SUBROUTINE user_fun(x, p, fval)
    REAL (rk), INTENT (IN) :: x, p
    REAL (rk), INTENT (OUT) :: fval

    fval = 1.0_rk/cosh(p*x)
  END SUBROUTINE user_fun
END PROGRAM prog_integration
