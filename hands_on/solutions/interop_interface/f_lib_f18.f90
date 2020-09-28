    MODULE f_lib_ts
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
          IMPORT :: c_float
          REAL (c_float) :: fun
          REAL (c_float), VALUE :: x
! Using the equivalent of void * is of course unsafe
          TYPE (*), TARGET :: params
        END FUNCTION
      END INTERFACE
      INTERFACE
        FUNCTION sum_fun(fun, params) BIND (C, NAME='Sum_fun')
          IMPORT :: c_float, abs_fun
          PROCEDURE (abs_fun) :: fun
          TYPE (*) :: params
        END FUNCTION
      END INTERFACE
    END MODULE
