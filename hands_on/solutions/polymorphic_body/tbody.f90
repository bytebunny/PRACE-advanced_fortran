    PROGRAM tbody
      USE mod_body
      IMPLICIT NONE

      TYPE, EXTENDS (body) :: peabody
      END TYPE

      REAL (dk) :: m1, m2

      CLASS (body), ALLOCATABLE :: my_body, aux_body
      CLASS (*), ALLOCATABLE :: up_body

      ALLOCATE (my_body, SOURCE=body(1.1D0,(/1.0D0,0.5D0,0.0D0/),(/1.0D0, &
        0.0D0,0.0D0/)))
      CALL print_body(my_body)

      ALLOCATE (aux_body, SOURCE=charged_body(body=my_body,charge=2.2E-1))
      CALL move_alloc(aux_body, my_body)
      CALL print_body(my_body)

      DEALLOCATE (my_body)

      ALLOCATE (my_body, SOURCE=charged_body(8.1D0,(/-1.0D0,0.5D0, &
        0.0D0/),(/1.0D0,0.0D0,-1.0D0/),1.4E-2))
      CALL print_body(my_body)
      DEALLOCATE (my_body)

      ALLOCATE (my_body, SOURCE=rotating_body(2.1D0,(/-1.0D1,0.0D0, &
        0.0D0/),(/1.0D0,0.0D0,-1.0D0/),1.2D0,(/0.0D0,0.0D0,2.3D1/)))
      CALL print_body(my_body)
      DEALLOCATE (my_body)

      ALLOCATE (up_body, SOURCE=charged_body(8.1D0,(/-1.0D0,0.5D0, &
        0.0D0/),(/1.0D0,0.0D0,-1.0D0/),1.4E-2))
      SELECT TYPE (up_body)
      CLASS IS (body)
        CALL print_body(up_body)
      CLASS DEFAULT
        STOP 'tbody: up_body has incorrect dynamic type for print_body &
          &invocation.'
      END SELECT
      DEALLOCATE (up_body)

      ALLOCATE (my_body, SOURCE=peabody(1.1D0,(/1.0D0,0.5D0,0.0D0/),(/1.0D0, &
        0.0D0,0.0D0/)))
      CALL print_body(my_body)
      DEALLOCATE (my_body)

!
! collisions
      m1 = 1.4D-1
      m2 = 2.7D-1

      ALLOCATE (my_body, SOURCE=body(m1,(/0.0D0,0.0D0,0.0D0/),(/2.0D0,0.0D0, &
        0.0D0/)))
      ALLOCATE (aux_body, SOURCE=body(m2,(/0.0D0,0.0D0,10.0D0/),(/-2.0D0, &
        0.0D0,0.0D0/)))
      WRITE (*, *) 'Before collision of bodies: '
      CALL print_body(my_body)
      CALL print_body(aux_body)
      CALL my_body%collide(aux_body)
      WRITE (*, *) 'After collision of bodies: '
      CALL print_body(my_body)
      CALL print_body(aux_body)

      DEALLOCATE (my_body, aux_body)

      ALLOCATE (my_body, SOURCE=charged_body(m1,(/0.0D0,0.0D0,0.0D0/),(/2.0D0, &
        0.0D0,0.0D0/),1.4E-2))
      ALLOCATE (aux_body, SOURCE=charged_body(m2,(/0.0D0,0.0D0, &
        10.0D0/),(/-2.0D0,0.0D0,0.0D0/),2.0E-2))

      WRITE (*, *) 'Before collision of charged bodies: '
      CALL print_body(my_body)
      CALL print_body(aux_body)
      CALL my_body%collide(aux_body)
      WRITE (*, *) 'After collision of charged bodies: '
      CALL print_body(my_body)
      CALL print_body(aux_body)

      DEALLOCATE (my_body, aux_body)

      ALLOCATE (my_body, SOURCE=charged_body(m1,(/0.0D0,0.0D0,0.0D0/),(/2.0D0, &
        0.0D0,0.0D0/),1.4E-2))
      ALLOCATE (aux_body, SOURCE=charged_body(m2,(/0.0D0,0.0D0, &
        10.0D0/),(/-2.0D0,0.0D0,0.0D0/),2.0E-2))

!  select type (aux_body) 
!  class is (charged_body)
      CALL my_body%collide(aux_body, 3.0D0, 5.0D0)
!  end select
      WRITE (*, *) 'After collision of charged bodies with radii specified: '
      CALL print_body(my_body)
      CALL print_body(aux_body)

      DEALLOCATE (my_body, aux_body)

    END PROGRAM
