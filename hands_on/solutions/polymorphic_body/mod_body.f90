    MODULE mod_body
      IMPLICIT NONE
      INTEGER, PARAMETER :: dk = kind(1.0D0)
      TYPE :: body
        REAL (dk) :: mass
        REAL (dk) :: pos(3), vel(3)
      CONTAINS
        PROCEDURE :: collide_body
        PROCEDURE :: collide_body_dummy
        GENERIC :: collide => collide_body, collide_body_dummy
      END TYPE

      TYPE, EXTENDS (body) :: charged_body
        REAL (dk) :: charge
      CONTAINS
        PROCEDURE :: collide_body_dummy => collide_charged_body
      END TYPE

      TYPE, EXTENDS (body) :: rotating_body
        REAL (dk) :: radius
        REAL (dk) :: omega(3)
      END TYPE
    CONTAINS
      RECURSIVE SUBROUTINE print_body(this)
        CLASS (body) :: this

        SELECT TYPE (this)
        TYPE IS (body)
          WRITE (*, '(''Mass is '',f10.3)') this%mass
          WRITE (*, '(''Position is '',3(f10.3,1X))') this%pos
          WRITE (*, '(''Velocity is '',3(f10.3,1X))') this%vel
        TYPE IS (charged_body)
          CALL print_body(this%body)
          WRITE (*, '(''Charge is '',f10.3)') this%charge
        TYPE IS (rotating_body)
          CALL print_body(this%body)
          WRITE (*, '(''Radius is '',f10.3)') this%radius
          WRITE (*, '(''Angular Velocity is '',3(f10.3,1X))') this%omega
        CLASS DEFAULT
          WRITE (*, *) &
            'mod_body::print_body: unknown extension of body. Continuing ...'
        END SELECT
      END SUBROUTINE
      SUBROUTINE collide_body(this, other)
!
! Only the velocities are updated because the positions depend on 
! the (unspecified) time.
        CLASS (body), INTENT (INOUT) :: this, other
        REAL (dk) :: vcm(3), wbefore1(3), wbefore2(3)

        vcm = (this%mass*this%vel+other%mass*other%vel)/(this%mass+other%mass)
        wbefore1 = this%vel - vcm
        wbefore2 = other%vel - vcm
        other%vel = this%mass/other%mass*wbefore1 + vcm
        this%vel = other%mass/this%mass*wbefore2 + vcm
      END SUBROUTINE
!
! The following only re-dispatches to collide_body
      SUBROUTINE collide_body_dummy(this, other, r1, r2)
        CLASS (body), INTENT (INOUT) :: this, other
        REAL (dk), INTENT (IN) :: r1, r2

        CALL this%collide_body(other)

      END SUBROUTINE
      SUBROUTINE collide_charged_body(this, other, r1, r2)
        CLASS (charged_body), INTENT (INOUT) :: this
        CLASS (body), INTENT (INOUT) :: other
        REAL (dk), INTENT (IN) :: r1, r2

        REAL (dk) :: qtot

        CALL this%collide_body(other)

        SELECT TYPE (other)
        CLASS IS (charged_body)
          qtot = this%charge + other%charge
          this%charge = qtot/(1.0_dk+r2/r1)
          other%charge = qtot/(1.0_dk+r1/r2)
        END SELECT
      END SUBROUTINE
    END MODULE
