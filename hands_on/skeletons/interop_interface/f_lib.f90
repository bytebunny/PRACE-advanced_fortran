    MODULE f_lib
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: sum_fun
      INTERFACE
        FUNCTION sum_fun(fun, params) BIND (C, NAME='Sum_fun')
! fill in the missing stuff
        END FUNCTION
      END INTERFACE
    END MODULE
