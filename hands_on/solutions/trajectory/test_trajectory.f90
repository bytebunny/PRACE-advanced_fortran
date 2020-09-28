    PROGRAM test_trajectory
      USE mod_trajectory
      IMPLICIT NONE
      INTEGER, PARAMETER :: totalsteps = 127
      TYPE (trajectory), ALLOCATABLE :: my_traj
      REAL (dk) :: time, pos(3), vel(3)
      INTEGER :: i


      ALLOCATE (my_traj)

      time = 0.0_dk
      pos = (/ 2.0_dk, 0.0_dk, 0.0_dk /)
      vel = (/ 0.0_dk, 0.0_dk, 1.0_dk /)
      my_traj = trajectory(50, 2.0_dk, time, pos, vel)

      CALL my_traj%attach_io('body.dat')

      DO i = 1, totalsteps
        time = time + 1.0E-2
        pos = pos + (/ 0.0_dk, 0.0_dk, 1.0E-1_dk*time**2 /)
        vel = vel + (/ 0.0_dk, 0.0_dk, 2.0E-1_dk*time /)
        CALL my_traj%update(time, pos, vel)
      END DO

      DEALLOCATE (my_traj) ! should not be necessary, but some compilers
! do not invoke the finalizer on the next assignment

      time = 0.0_dk
      pos = (/ 2.0_dk, 0.0_dk, 0.0_dk /)
      vel = (/ 0.0_dk, 0.0_dk, 1.0_dk /)
      my_traj = trajectory(50, 2.0_dk, time, pos, vel, 1.0E+1_dk)

      CALL my_traj%attach_io('charged_body.dat')

      DO i = 1, totalsteps
        time = time + 1.0E-2
        pos = pos + (/ 0.0_dk, 0.0_dk, 1.0E-1_dk*time**2 /)
        vel = vel + (/ 0.0_dk, 0.0_dk, 2.0E-1_dk*time /)
        CALL my_traj%update(time, pos, vel)
      END DO

      DEALLOCATE (my_traj)


    END PROGRAM
