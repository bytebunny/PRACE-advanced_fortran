    MODULE mod_deferred
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: generate_data

      INTEGER :: iunit = 20
    CONTAINS
!
! This is the Fortran interface
!
      SUBROUTINE generate_data(arr)
        REAL (c_float), ALLOCATABLE, INTENT (OUT) :: arr(:)
        INTEGER :: nsz, n

        OPEN (iunit, FILE='data.txt', FORM='FORMATTED', STATUS='OLD')
        READ (iunit, FMT=*) nsz
        ALLOCATE (arr(nsz))
        DO n = 1, nsz
          READ (iunit, FMT=*) arr(n)
        END DO
        CLOSE (iunit)
      END SUBROUTINE
    END MODULE
