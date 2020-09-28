    MODULE f_lib
!
! This module only contains interface
! descriptions since all stuff is implemented
! either in the C library or by the client
!
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: sum_fun
      ABSTRACT INTERFACE
        FUNCTION abs_fun(x, params) BIND (C)
          IMPORT :: c_float, c_ptr
          REAL (c_float) :: fun
          REAL (c_float), VALUE :: x
! Using the equivalent of void * is of course unsafe
          TYPE (c_ptr), VALUE :: params
        END FUNCTION
      END INTERFACE
      INTERFACE
        FUNCTION sum_fun(fun, params) BIND (C, NAME='Sum_fun')
          IMPORT :: c_float, c_ptr, abs_fun
          PROCEDURE (abs_fun) :: fun
          TYPE (c_ptr), VALUE :: params
        END FUNCTION
      END INTERFACE
    END MODULE
