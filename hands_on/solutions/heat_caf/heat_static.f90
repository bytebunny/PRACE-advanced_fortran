    MODULE mod_main_static
      USE mod_heat_static
!> auxiliary functions for boundary and initial conditions
!> as well as public module for entities from mod_heat
      IMPLICIT NONE
    CONTAINS
      PURE REAL (dk) FUNCTION fival(x, y)
        REAL (dk), INTENT (IN) :: x, y

        fival = 0._dk
      END FUNCTION
      PURE REAL (dk) FUNCTION fbval_top(x)
        REAL (dk), INTENT (IN) :: x

        fbval_top = 0.0_dk
      END FUNCTION
      PURE REAL (dk) FUNCTION fbval_bottom(x)
        REAL (dk), INTENT (IN) :: x

        fbval_bottom = 1.0_dk
      END FUNCTION
      PURE REAL (dk) FUNCTION fbval_sides(x)
        REAL (dk), INTENT (IN) :: x

        fbval_sides = x
      END FUNCTION
    END MODULE
    PROGRAM heat_static
      USE mod_main_static
      USE timer
      IMPLICIT NONE
!  logical, parameter :: print = .true.
      LOGICAL, PARAMETER :: print = .FALSE.
      REAL (dk), PARAMETER :: eps = 1.0E-8_dk
      REAL (dk) :: dt, ti, mflops
      REAL (dk) :: gdiff

      IF (num_images()/=nimg) ERROR STOP 'Unsupported image count'
      CALL heat_ival(fival)
      CALL heat_bval('N', fbval_top)
      CALL heat_bval('S', fbval_bottom)
      CALL heat_bval('W', fbval_sides)
      CALL heat_bval('E', fbval_sides)

      dt = 0.25_dk/real(ndim)**2
      ti = dwalltime()
      DO
        gdiff = heat_iter(dt, 100)
        CALL co_max(gdiff)
        IF (gdiff<eps) EXIT
!     if (heat_iter(dt,50000) > 0.0) exit
        IF (print .AND. mod(num_iter,100)==1) CALL heat_print()
      END DO
      ti = dwalltime() - ti

      CALL heat_print()

      IF (this_image()==1) THEN
        mflops = 11.0_dk*real(ndim-2, dk)*real(ndim-2, dk)*real(num_iter)/ &
          1.E6_dk
        WRITE (*, FMT= &
          '(''Completed '',i0,'' iterations in '',f10.3,'' seconds.'')') &
          num_iter, ti
        WRITE (*, FMT= &
          '(''FP performance of Jacobi kernel:'',f12.3,'' MFlop/s.'')') &
          mflops/ti
      END IF
    END PROGRAM
