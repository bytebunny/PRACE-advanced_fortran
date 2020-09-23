    PROGRAM tbody
      USE mod_body
      IMPLICIT NONE

      ! declare class body to be flexible if it is body or 
      ! its children (charged and rotating bodies):
      class (body), ALLOCATABLE :: my_body, aux_body

      ALLOCATE (my_body, SOURCE=body(1.1D0,(/1.0D0,0.5D0,0.0D0/),(/1.0D0, &
        0.0D0,0.0D0/)))
      CALL print_body(my_body)

      ALLOCATE (aux_body, SOURCE=charged_body(body=my_body,charge=2.2E-1))
      CALL move_alloc(aux_body, my_body)
      CALL print_body(my_body)

      DEALLOCATE (my_body)

      ALLOCATE ( my_body, SOURCE = rotating_body( 2.1D0, &
             (/ -1.0D1, 0.0D0, 0.0D0 /), &
             (/  1.0D0, 0.0D0,-1.0D0 /), &
             (/  0.0D0, 0.0D0, 2.3D1 /) ) )
      CALL print_body(my_body)
      DEALLOCATE (my_body)



    END PROGRAM
