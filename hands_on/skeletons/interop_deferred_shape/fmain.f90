    PROGRAM fmain
      USE mod_deferred
      USE, INTRINSIC :: iso_c_binding, ONLY: c_float
      IMPLICIT NONE
      REAL (c_float), ALLOCATABLE :: arr(:)
      INTEGER :: i

      CALL generate_data(arr)

      DO i = 1, size(arr)
        WRITE (*, FMT='('' Element '',i0,'' has value '',F10.3)') i, arr(i)
      END DO

    END PROGRAM
