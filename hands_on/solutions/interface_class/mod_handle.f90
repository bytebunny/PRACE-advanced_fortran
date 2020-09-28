    MODULE mod_handle
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: handle, create_handle
      TYPE, ABSTRACT :: handle
        PRIVATE
        INTEGER :: state = 0
      CONTAINS
        PROCEDURE (open_handle), DEFERRED :: open
        PROCEDURE (close_handle), DEFERRED :: close
        PROCEDURE (send_data), DEFERRED :: send
        PROCEDURE, NON_OVERRIDABLE :: getstate
        PROCEDURE, NON_OVERRIDABLE :: setstate
      END TYPE
      ABSTRACT INTERFACE
        SUBROUTINE open_handle(this, info)
          IMPORT :: handle
          CLASS (handle) :: this
          CLASS (*), INTENT (IN), OPTIONAL :: info
        END SUBROUTINE
        SUBROUTINE close_handle(this)
          IMPORT :: handle
          CLASS (handle) :: this
        END SUBROUTINE
        SUBROUTINE send_data(this, x)
          IMPORT :: handle
          CLASS (handle), INTENT (IN) :: this
          CLASS (*), INTENT (IN) :: x(:)
        END SUBROUTINE
      END INTERFACE
      INTERFACE
        MODULE SUBROUTINE create_handle(this, type)
          CLASS (handle), ALLOCATABLE, INTENT (OUT) :: this
          CHARACTER (LEN=*), INTENT (IN) :: type
        END SUBROUTINE
      END INTERFACE
    CONTAINS
      CHARACTER (LEN=6) FUNCTION getstate(this)
        CLASS (handle) :: this

        SELECT CASE (this%state)
        CASE (0)
          getstate = 'closed'
        CASE (1)
          getstate = 'open'
        CASE DEFAULT
          STOP 'mod_handle::getstate: undefined state'
        END SELECT
      END FUNCTION
      SUBROUTINE setstate(this, is)
        CLASS (handle) :: this
        INTEGER, INTENT (IN) :: is

        this%state = is
      END SUBROUTINE
    END MODULE
