    MODULE mod_fmain
      USE f_lib
! the following type definition is used inside Fortran only
      TYPE :: my_stuff
        INTEGER (c_int) :: scale
        REAL (c_float) :: offset
      END TYPE
    CONTAINS
      FUNCTION my_fun(x, params) BIND (C)
! fill in the missing stuff - make it work just like the C program
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
