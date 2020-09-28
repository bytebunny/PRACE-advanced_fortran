    MODULE mod_fmain
      USE f_lib
      USE, INTRINSIC :: iso_c_binding
! the following type definition is used inside Fortran only
! is not absolutely required to be an interoperable type
      TYPE :: my_stuff
        INTEGER (c_int) :: scale
        REAL (c_float) :: offset
      END TYPE
    CONTAINS
      FUNCTION my_fun(x, params) BIND (C)
        REAL (c_float) :: my_fun
        REAL (c_float), VALUE :: x
        TYPE (c_ptr), VALUE :: params
!
        TYPE (my_stuff), POINTER :: obj_stuff
!   The following conversion is NOT type safe
        CALL c_f_pointer(params, obj_stuff)
        my_fun = obj_stuff%scale*x + obj_stuff%offset
      END FUNCTION
    END MODULE
    PROGRAM fmain
      USE mod_fmain
      IMPLICIT NONE
      TYPE (my_stuff), TARGET :: obj_stuff
      TYPE (c_ptr) :: params
      REAL (c_float) :: result

      obj_stuff = my_stuff(2_c_int, 3.5_c_float)
      params = c_loc(obj_stuff)
      result = sum_fun(my_fun, params)
      WRITE (6, FMT='(''Result of calculation is: '',F13.7)') result
    END PROGRAM
