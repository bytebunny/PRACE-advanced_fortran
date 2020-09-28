    MODULE mod_test_qdr

      USE qdr
      IMPLICIT NONE

      TYPE, EXTENDS (qdr_fun) :: qdr_poly
        REAL (KIND=rk), ALLOCATABLE :: f(:)
      CONTAINS
        PROCEDURE :: eval => eval_poly
      END TYPE

      TYPE, EXTENDS (qdr_fun) :: qdr_mydisc
      CONTAINS
        PROCEDURE :: eval => eval_mydisc
      END TYPE

    CONTAINS

      REAL (KIND=rk) FUNCTION eval_poly(this, x)
        CLASS (qdr_poly) :: this
        REAL (KIND=rk), INTENT (IN) :: x
        INTEGER :: i, imx

        imx = size(this%f)
        eval_poly = this%f(imx)
        DO i = imx - 1, 1, -1
          eval_poly = eval_poly*x + this%f(i)
        END DO
      END FUNCTION

      REAL (KIND=rk) FUNCTION eval_mydisc(this, x)
        CLASS (qdr_mydisc) :: this
        REAL (KIND=rk), INTENT (IN) :: x

        IF (x<1.0_rk) THEN
          eval_mydisc = x
        ELSE
          eval_mydisc = x - 1.0_rk
        END IF
!    write(6, *) 'eval: x,y - ',x,eval_mydisc
      END FUNCTION
! analogous function for use with gsl
      FUNCTION fun_qag(x, params) BIND (C)
        USE, INTRINSIC :: iso_c_binding
        REAL (c_double), VALUE :: x
        TYPE (c_ptr), VALUE :: params
        REAL (c_double) :: fun_qag
!
        IF (x<1.0_rk) THEN
          fun_qag = x
        ELSE
          fun_qag = x - 1.0_rk
        END IF
      END FUNCTION
    END MODULE

    PROGRAM test_qdr

      USE mod_test_qdr
! FIXME
! the following two lines should not be present in the solution program
      USE fgsl
      USE, INTRINSIC :: iso_c_binding
!
      IMPLICIT NONE

      TYPE (qdr_poly) :: o1_qdr
      TYPE (qdr_mydisc) :: o2_qdr
      REAL (KIND=rk) :: intv(2), result, check
      INTEGER :: status
! GSL QAG - directly called from main program below
      INTEGER (fgsl_size_t) :: n = 25
      INTEGER :: istat_gsl
      TYPE (fgsl_integration_workspace) :: wk
      TYPE (fgsl_function) :: f
      TYPE (c_ptr) :: params = c_null_ptr
      REAL (KIND=rk) :: eps = 1.0D-12, abserr
!
      intv = (/ 0.0_rk, 2.0_rk /)
      ALLOCATE (o1_qdr%f(3))
      o1_qdr%f = (/ 1.0_rk, 2.5_rk, 4.0_rk /)
      result = integral_1d(intv, o1_qdr, status)
      check = 2.0_rk + 1.25_rk*4.0_rk + 4.0_rk/3.0_rk*8.0_rk
      WRITE (*, *) 'status = ', status, ' result: ', result, ' check: ', check
!
      result = integral_1d(intv, o2_qdr, status)
      check = 1.0_rk
      WRITE (*, *) 'Using default algorithm: '
      WRITE (*, *) 'status = ', status, ' result: ', result, ' check: ', check
! FIXME
! GSL QAG - uses an interface with different signature here
! The point of the exercise is to hide this different interface
! behind the uniform one defined in qdr.
      wk = fgsl_integration_workspace_alloc(n)
      f = fgsl_function_init(fun_qag, params)
      istat_gsl = fgsl_integration_qag(f, intv(1), intv(2), eps, eps, n, &
        fgsl_integ_gauss21, wk, result, abserr)
      CALL fgsl_function_free(f)
      CALL fgsl_integration_workspace_free(wk)
      check = 1.0_rk
      WRITE (*, *) 'Using GSL QAG algorithm: '
      WRITE (*, *) 'status = ', status, ' result: ', result, ' check: ', check

    END PROGRAM
