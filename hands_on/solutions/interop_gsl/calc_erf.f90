PROGRAM calc_erf
  USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double
  IMPLICIT NONE

  TYPE, BIND (C) :: gsl_sf_result
     REAL (c_double) :: val, err
  END TYPE gsl_sf_result

  INTERFACE
     INTEGER (c_int) FUNCTION gsl_sf_erf_e(x, r) BIND (C)
       IMPORT :: c_int, c_double, gsl_sf_result
       REAL (c_double), VALUE :: x
       TYPE (gsl_sf_result), INTENT (INOUT) :: r
     END FUNCTION gsl_sf_erf_e
  END INTERFACE

  INTEGER, PARAMETER :: idim = 11
  INTEGER :: i, istat
  REAL (c_double) :: x(idim) = [ (real(i,c_double)/5.0_c_double,i=0,idim-1) ]
  TYPE (gsl_sf_result) :: r(idim)

  WRITE (*, '(''  x       erf(x)      Error  '')')
  DO i = 1, idim
     istat = gsl_sf_erf_e(x(i), r(i))
     IF (istat/=0) THEN ! GSL_SUCCESS is zero
        WRITE (*, '('' execution unsuccessful: '', i0)') istat
     END IF
     WRITE (*, '(F5.2,2E12.5)') x(i), r(i)
  END DO

END PROGRAM calc_erf
