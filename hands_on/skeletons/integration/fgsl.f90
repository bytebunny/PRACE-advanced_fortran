    MODULE fgsl
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: fgsl_function_init, fgsl_function_free, &
        fgsl_integration_workspace_alloc, fgsl_integration_workspace_free, &
        fgsl_integration_qag, fgsl_well_defined
!
! Kind and length parameters are default integer
!
      INTEGER, PARAMETER, PUBLIC :: fgsl_double = c_double
      INTEGER, PARAMETER, PUBLIC :: fgsl_extended = selected_real_kind(13)
      INTEGER, PARAMETER, PUBLIC :: fgsl_int = c_int
      INTEGER, PARAMETER, PUBLIC :: fgsl_long = c_long
      INTEGER, PARAMETER, PUBLIC :: fgsl_size_t = c_size_t
      INTEGER, PARAMETER, PUBLIC :: fgsl_char = c_char
!
! Error codes
!
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_success = 0
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_failure = -1
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_continue = -2 ! iteration has not converged
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_edom = 1 ! input domain error, e.g. sqrt(-1)
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_erange = 2 ! output range error, e.g. exp(1e100)
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_efault = 3 ! invalid pointer
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_einval = 4 ! invalid argument supplied by user
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_efactor = 6 ! generic failure
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_esanity = 7 ! sanity check failed
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_enomem = 8 ! malloc failed
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ebadfunc = 9 ! problem with user-supplied function
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_erunaway = 10 ! iterative process is out of control
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_emaxiter = 11 ! exceeded max number of iterations
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ezerodiv = 12 ! tried to divide by zero
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ebadtol = 13 ! user specified an invalid tolerance
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_etol = 14 ! failed to reach the specified tolerance
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eundrflw = 15 ! underflow 
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eovrflw = 16 ! overflow
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eloss = 17 ! loss of accuracy  
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eround = 18 ! failed because of roundoff error
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ebadlen = 19 ! matrix, vector lengths are not conformant
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_enotsqr = 20 ! matrix not square
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_esing = 21 ! apparent singularity detected
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ediverge = 22 ! integral or series is divergent
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eunsup = 23 ! no hw support for requested feature
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eunimpl = 24 ! requested feature not (yet) implemented
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_ecache = 25 ! cache limit exceeded
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_etable = 26 ! table limit exceeded
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_enoprog = 27 ! iteration: no progress towards solution
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_enoprogj = 28 ! jacobian evals not improving the solution
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_etolf = 29 ! can't reach specified tolerance in F
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_etolx = 30 ! can't reach specified tolerance in X
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_etolg = 31 ! can't reach specified tolerance in gradient
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_eof = 32 ! end of file


!
! Types: 
!
      TYPE, PUBLIC :: fgsl_function
        PRIVATE
        TYPE (c_ptr) :: gsl_function = c_null_ptr
      END TYPE
      TYPE, PUBLIC :: fgsl_integration_workspace
        PRIVATE
        TYPE (c_ptr) :: gsl_integration_workspace = c_null_ptr
        LOGICAL :: status = .FALSE.
      END TYPE
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss15 = 1
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss21 = 2
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss31 = 3
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss41 = 4
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss51 = 5
      INTEGER (fgsl_int), PARAMETER, PUBLIC :: fgsl_integ_gauss61 = 6
!
! generics
!
      INTERFACE fgsl_well_defined
        MODULE PROCEDURE fgsl_integration_workspace_status
      END INTERFACE
!
! external Interfaces
!
      INTERFACE
        FUNCTION fgsl_function_cinit(func, params) BIND (C)
          IMPORT :: c_funptr, c_ptr
          TYPE (c_funptr), VALUE :: func
          TYPE (c_ptr), VALUE :: params
          TYPE (c_ptr) :: fgsl_function_cinit
        END FUNCTION
        SUBROUTINE fgsl_function_cfree(sfunc) BIND (C)
          IMPORT :: c_ptr
          TYPE (c_ptr), VALUE :: sfunc
        END SUBROUTINE
        FUNCTION gsl_integration_workspace_alloc(n) BIND (C)
          IMPORT :: c_size_t, c_ptr
          INTEGER (c_size_t), VALUE :: n
          TYPE (c_ptr) :: gsl_integration_workspace_alloc
        END FUNCTION
        SUBROUTINE gsl_integration_workspace_free(w) BIND (C)
          IMPORT :: c_ptr
          TYPE (c_ptr), VALUE :: w
        END SUBROUTINE
        FUNCTION gsl_integration_qag(f, a, b, epsabs, epsrel, limit, key, &
          workspace, result, abserr) BIND (C)
          IMPORT :: c_ptr, c_double, c_size_t, c_int
          TYPE (c_ptr), VALUE :: f
          REAL (c_double), VALUE :: a, b, epsabs, epsrel
          INTEGER (c_size_t), VALUE :: limit
          INTEGER (c_int), VALUE :: key
          TYPE (c_ptr), VALUE :: workspace
          REAL (c_double), INTENT (OUT) :: result, abserr
          INTEGER (c_int) :: gsl_integration_qag
        END FUNCTION
      END INTERFACE
    CONTAINS
      FUNCTION fgsl_function_init(func, params)
        INTERFACE
          FUNCTION func(x, params) BIND (C)
            USE, INTRINSIC :: iso_c_binding
            REAL (c_double), VALUE :: x
            TYPE (c_ptr), VALUE :: params
            REAL (c_double) :: func
          END FUNCTION
        END INTERFACE
        TYPE (c_ptr), INTENT (IN) :: params
        TYPE (fgsl_function) :: fgsl_function_init
!
        TYPE (c_funptr) :: fp

        fp = c_funloc(func)
        fgsl_function_init%gsl_function = fgsl_function_cinit(fp, params)
      END FUNCTION
      SUBROUTINE fgsl_function_free(sfunc)
        TYPE (fgsl_function), INTENT (INOUT) :: sfunc

        CALL fgsl_function_cfree(sfunc%gsl_function)
      END SUBROUTINE
      FUNCTION fgsl_integration_workspace_alloc(n)
        INTEGER (fgsl_size_t), INTENT (IN) :: n
        TYPE (fgsl_integration_workspace) :: fgsl_integration_workspace_alloc

        fgsl_integration_workspace_alloc%status = .FALSE.
        fgsl_integration_workspace_alloc%gsl_integration_workspace = &
          gsl_integration_workspace_alloc(n)
        fgsl_integration_workspace_alloc%status = .TRUE.
      END FUNCTION
      SUBROUTINE fgsl_integration_workspace_free(w)
        TYPE (fgsl_integration_workspace), INTENT (INOUT) :: w

        IF (c_associated(w%gsl_integration_workspace)) &
          CALL gsl_integration_workspace_free(w%gsl_integration_workspace)
        w%status = .TRUE.
      END SUBROUTINE
      FUNCTION fgsl_integration_qag(f, a, b, epsabs, epsrel, limit, key, &
        workspace, result, abserr)
        TYPE (fgsl_function), INTENT (IN) :: f
        REAL (fgsl_double), INTENT (IN) :: a, b, epsabs, epsrel
        INTEGER (fgsl_size_t), INTENT (IN) :: limit
        INTEGER (fgsl_int), INTENT (IN) :: key
        TYPE (fgsl_integration_workspace), INTENT (INOUT) :: workspace
        REAL (fgsl_double), INTENT (OUT) :: result, abserr
        INTEGER (fgsl_int) :: fgsl_integration_qag

        fgsl_integration_qag = gsl_integration_qag(f%gsl_function, a, b, &
          epsabs, epsrel, limit, key, workspace%gsl_integration_workspace, &
          result, abserr)
      END FUNCTION
      FUNCTION fgsl_integration_workspace_status(integration_workspace)
        TYPE (fgsl_integration_workspace), INTENT (IN) :: &
          integration_workspace
        LOGICAL :: fgsl_integration_workspace_status

        fgsl_integration_workspace_status = .TRUE.
        IF (.NOT. c_associated(integration_workspace%gsl_integration_workspace &
          )) fgsl_integration_workspace_status = .FALSE.
      END FUNCTION
    END MODULE
