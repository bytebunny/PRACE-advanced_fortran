    MODULE mod_fmain_ts
      USE f_lib_ts
      USE, INTRINSIC :: iso_c_binding
! the following type definition is used inside Fortran only
! is not absolutely required to be an interoperable type
      TYPE :: b
        INTEGER (c_int) :: scale
      END TYPE
      TYPE, EXTENDS (b) :: my_stuff
        REAL (c_float) :: offset
      END TYPE
    CONTAINS
      FUNCTION my_fun(x, params) BIND (C)
        REAL (c_float) :: my_fun
        REAL (c_float), VALUE :: x
        TYPE (*), TARGET :: params
!
        TYPE (my_stuff), POINTER :: obj_stuff
!   The following conversion is NOT type safe
        CALL c_f_pointer(c_loc(params), obj_stuff)
        my_fun = obj_stuff%scale*x + obj_stuff%offset
      END FUNCTION
    END MODULE
    PROGRAM fmain_ts
      USE mod_fmain_ts
      IMPLICIT NONE
      CLASS (my_stuff), ALLOCATABLE :: params
      REAL (c_float) :: result

      ALLOCATE (params, SOURCE=my_stuff(2_c_int,3.5_c_float))
      result = sum_fun(my_fun, params)
      WRITE (6, FMT='(''Result of calculation is: '',F13.7)') result
    END PROGRAM
