    MODULE mod_body
      IMPLICIT NONE
      INTEGER, PARAMETER :: dk = kind(1.0D0)
      TYPE :: body
        REAL (dk) :: mass
        REAL (dk) :: pos(3), vel(3)
      END TYPE body


      type, extends(body) :: charged_body
          real (dk) :: charge
      end type charged_body

      type, extends(body) :: rotating_body
          real (dk) :: angular_vel(3)
      end type rotating_body


    CONTAINS
      SUBROUTINE print_body(this)
        class ( body ) :: this

        
        select type( this )
            
        type is ( body )
            
            WRITE (*, '(''Mass is '',f10.3)') this%mass
            WRITE (*, '(''Position is '',3(f10.3,1X))') this%pos
            WRITE (*, '(''Velocity is '',3(f10.3,1X))') this%vel
            
        type is ( charged_body )
            
            write (*, '( ''/// Charge is'',f10.3 )') this%charge
            
        type is ( rotating_body )
            
            write (*, '( ''/// Angular velocity is'', 3(f10.3,1X) )') this%angular_vel
            
        class default 
        
            STOP 'Error(mod_body::print_body): Type not recognized.'
    
        end select


      END SUBROUTINE
    END MODULE
