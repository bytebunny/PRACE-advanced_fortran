    MODULE mod_file_handle
      USE mod_handle
      IMPLICIT NONE
      TYPE, EXTENDS (handle) :: file_handle
        PRIVATE
        INTEGER :: unit
      CONTAINS
        PROCEDURE :: open => file_open
        PROCEDURE :: close => file_close
        PROCEDURE :: send => send_real_array
      END TYPE
    CONTAINS
      SUBROUTINE file_open(this, info)
        CLASS (file_handle) :: this
        CLASS (*), INTENT (IN), OPTIONAL :: info

        SELECT TYPE (info)
        TYPE IS (CHARACTER(LEN=*))
          OPEN (FILE=info, NEWUNIT=this%unit, FORM='FORMATTED', &
            ACTION='READWRITE')
          CALL this%setstate(1)
        CLASS DEFAULT
          STOP 'mod_mystuff::file_open: incorrect type for argument info.'
        END SELECT
      END SUBROUTINE
      SUBROUTINE file_close(this)
        CLASS (file_handle) :: this

        CLOSE (this%unit)
        CALL this%setstate(0)
      END SUBROUTINE
      SUBROUTINE send_real_array(this, x)
        CLASS (file_handle), INTENT (IN) :: this
        CLASS (*), INTENT (IN) :: x(:)

        SELECT TYPE (x)
        TYPE IS (REAL)
          WRITE (this%unit, FMT='(3F12.5)') x
        CLASS DEFAULT
          STOP 'mod_mystuff::send_real_array: incorrect type for x'
        END SELECT
      END SUBROUTINE
    END MODULE
