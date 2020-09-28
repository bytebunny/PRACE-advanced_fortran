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
        CLASS (qdr_opt), POINTER :: options => null()
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
! provide additional information to the integrator
      TYPE, PUBLIC, ABSTRACT :: qdr_opt
      END TYPE
      TYPE, PUBLIC, EXTENDS (qdr_opt) :: qdr_opt_disc
! internal discontinuities
        REAL (KIND=rk), ALLOCATABLE :: disc(:)
      END TYPE
      TYPE, PUBLIC, EXTENDS (qdr_opt) :: qdr_opt_qag
! GSL QAG
        INTEGER (fgsl_size_t) :: n = 25
        TYPE (fgsl_integration_workspace) :: wk
        TYPE (fgsl_function) :: f
        CLASS (qdr_fun), POINTER :: fun => null()
!  contains
!   finalizer missing (possible leak!)
!  
      END TYPE
    CONTAINS
      REAL (KIND=rk) FUNCTION integral_1d(intv, fun, status)
        USE, INTRINSIC :: iso_c_binding
        REAL (KIND=rk), INTENT (IN) :: intv(2)
        CLASS (qdr_fun), INTENT (INOUT), TARGET :: fun
        INTEGER, OPTIONAL, INTENT (OUT) :: status
!
        INTEGER :: i, imx, istat_gsl
        REAL (KIND=rk) :: result, intv_loc(2), abserr
        TYPE (c_ptr) :: params
!
        IF (associated(fun%options)) THEN
          SELECT TYPE (p=>fun%options)
          TYPE IS (qdr_opt_disc)
            imx = size(p%disc) + 1
            result = 0.0_rk
            DO i = 1, imx
              IF (i>1) THEN
                intv_loc(1) = p%disc(i-1)
              ELSE
                intv_loc(1) = intv(1)
              END IF
              IF (i<imx) THEN
                intv_loc(2) = p%disc(i)
              ELSE
                intv_loc(2) = intv(2)
              END IF
! assuming a < b here
              intv_loc(1) = intv_loc(1)*(1.0_rk+2*epsilon(1.0_rk))
              intv_loc(2) = intv_loc(2)*(1.0_rk-2*epsilon(1.0_rk))
              result = result + default(intv_loc, fun, status)
            END DO
          TYPE IS (qdr_opt_qag)
            p%wk = fgsl_integration_workspace_alloc(p%n)
            params = c_loc(p)
            p%f = fgsl_function_init(fun_qag, params)
            p%fun => fun
            istat_gsl = fgsl_integration_qag(p%f, intv(1), intv(2), fun%eps, &
              fun%eps, p%n, fgsl_integ_gauss21, p%wk, result, abserr)
            CALL fgsl_function_free(p%f)
            CALL fgsl_integration_workspace_free(p%wk)
            p%fun => null()
          CLASS DEFAULT
            IF (present(status)) status = qdr_no_class
          END SELECT
        ELSE
          result = default(intv, fun, status)
        END IF
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
      FUNCTION fun_qag(x, params) BIND (C)
        USE, INTRINSIC :: iso_c_binding
        REAL (c_double), VALUE :: x
        TYPE (c_ptr), VALUE :: params
        REAL (c_double) :: fun_qag
!
        TYPE (qdr_opt_qag), POINTER :: this

        CALL c_f_pointer(params, this)
        fun_qag = this%fun%eval(x)
      END FUNCTION
    END MODULE
