    MODULE mod_trajectory
      USE mod_body
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: update_trajectory, dk
      TYPE, PUBLIC :: trajectory
!
! contains state of a particle for multiple time steps
        PRIVATE
        CLASS (body), ALLOCATABLE :: particle(:)
        REAL (dk), ALLOCATABLE :: time(:)
        INTEGER :: present = 1
        INTEGER :: attached_unit = 0
      CONTAINS
        PROCEDURE :: update => update_trajectory
        PROCEDURE :: attach_io
        FINAL :: detach_io
      END TYPE
      INTERFACE trajectory
        MODULE PROCEDURE create_traj
        MODULE PROCEDURE create_traj_charged
      END INTERFACE
    CONTAINS
      FUNCTION create_traj(n_steps, mass, t_start, pos, vel) RESULT (tr)
        TYPE (trajectory) :: tr
        INTEGER, INTENT (IN) :: n_steps
        REAL (dk), INTENT (IN) :: mass, t_start, pos(3), vel(3)

        INTEGER :: i

        IF (allocated(tr%particle)) DEALLOCATE (tr%particle)
        IF (allocated(tr%time)) DEALLOCATE (tr%time)
        ALLOCATE (tr%particle(n_steps), SOURCE=(/body(mass,pos, &
          vel),(body(0.0_dk,(/0.0_dk,0.0_dk,0.0_dk/), &
          (/0.0_dk,0.0_dk,0.0_dk/)),i=2,n_steps)/) )
        ALLOCATE (tr%time(n_steps), SOURCE=(/t_start,((0.0_dk),i=2, &
          n_steps)/) )
      END FUNCTION
      FUNCTION create_traj_charged(n_steps, mass, t_start, pos, vel, charge) &
        RESULT (tr)
        TYPE (trajectory) :: tr
        INTEGER, INTENT (IN) :: n_steps
        REAL (dk), INTENT (IN) :: mass, t_start, pos(3), vel(3), charge

        INTEGER :: i

        IF (allocated(tr%particle)) DEALLOCATE (tr%particle)
        IF (allocated(tr%time)) DEALLOCATE (tr%time)
        ALLOCATE (tr%particle(n_steps), SOURCE=(/charged_body(mass,pos,vel, &
          charge),(charged_body(0.0_dk,(/0.0_dk,0.0_dk,0.0_dk/),(/0.0_dk, &
          0.0_dk,0.0_dk/),0.0_dk),i=2,n_steps)/) )
        ALLOCATE (tr%time(n_steps), SOURCE=(/t_start,((0.0_dk),i=2, &
          n_steps)/) )
      END FUNCTION
      SUBROUTINE update_trajectory(this, time_new, pos_new, vel_new)
        CLASS (trajectory), INTENT (INOUT) :: this
        REAL (dk), INTENT (IN) :: time_new, pos_new(3), vel_new(3)
        INTEGER :: n, n_flush

        IF (this%present==size(this%time)) THEN
!
! flush data to file if attached
          IF (this%attached_unit/=0) THEN
            n_flush = max(1, size(this%time)/5) ! write a fraction of the buffer
            DO n = 1, n_flush
              WRITE (this%attached_unit, FMT= &
                '(F10.3,1X,3(F10.3,1X),2X,3(F10.3,1X))') this%time(n), &
                this%particle(n)%pos, this%particle(n)%vel
            END DO
          ELSE
            n_flush = 1
          END IF
          DO n = 1, size(this%time) - n_flush
            SELECT TYPE (particle=>this%particle)
            TYPE IS (body)
              particle(n) = particle(n+n_flush)
            TYPE IS (charged_body)
              particle(n) = particle(n+n_flush)
            CLASS DEFAULT
              STOP 'mod_trajectory::update_trajectory: Unknown &
                &extension of body.'
            END SELECT
            this%time(n) = this%time(n+n_flush)
          END DO
          this%present = this%present - n_flush
        END IF
!
! update
        this%present = this%present + 1
        this%time(this%present) = time_new
        this%particle(this%present)%pos = pos_new
        this%particle(this%present)%vel = vel_new
        IF (this%present>1) THEN
          SELECT TYPE (particle=>this%particle)
          TYPE IS (body)
            particle(this%present)%mass = particle(this%present-1)%mass
          TYPE IS (charged_body)
            particle(this%present)%mass = particle(this%present-1)%mass
            particle(this%present)%charge = particle(this%present-1)%charge
          CLASS DEFAULT
            STOP &
              'mod_trajectory::update_trajectory: Unknown extension of body.'
          END SELECT
        END IF
      END SUBROUTINE
      SUBROUTINE attach_io(this, fname)
        CLASS (trajectory), INTENT (INOUT) :: this
        CHARACTER (LEN=*) :: fname

        IF (allocated(this%time)) THEN
          OPEN (NEWUNIT=this%attached_unit, FILE=fname, FORM='FORMATTED', &
            ACTION='WRITE')
          SELECT TYPE (particle=>this%particle)
          TYPE IS (body)
            WRITE (this%attached_unit, FMT='(''body '',F10.3)') particle(1) &
              %mass
          TYPE IS (charged_body)
            WRITE (this%attached_unit, FMT='(''charged_body '',2(F10.3,2X))') &
              particle(1)%mass, particle(1)%charge
          CLASS DEFAULT
            STOP 'mod_trajectory::attach_io: Unknown extension of body.'
          END SELECT
        END IF
      END SUBROUTINE
!
! flush out remaining elements of object and close
      SUBROUTINE detach_io(this)
        TYPE (trajectory) :: this
        INTEGER :: n
!   write(*, '(''Invoking detach_io.'')')
        IF (allocated(this%particle) .AND. this%attached_unit/=0) THEN
          DO n = 1, this%present
            WRITE (this%attached_unit, FMT= &
              '(F10.3,1X,3(F10.3,1X),2X,3(F10.3,1X))') this%time(n), &
              this%particle(n)%pos, this%particle(n)%vel
          END DO
          CLOSE (this%attached_unit)
        END IF
      END SUBROUTINE
    END MODULE
