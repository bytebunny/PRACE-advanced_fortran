PROGRAM prog_passwd
  USE mod_passwd
  IMPLICIT NONE

  TYPE (c_ptr) :: my_pwent
  CHARACTER (LEN=:), ALLOCATABLE :: home, name
  CHARACTER (LEN=2) :: switch
  INTEGER :: len_name, stat
  !
  ! process command line after setting a default
  name = 'root' // c_null_char
  IF (command_argument_count()==2) THEN
     CALL get_command_argument(1, value=switch, status=stat)
     IF (stat/=0) THEN
        STOP 'ERROR(prog_passwd): command line processing failed. Aborting.'
     END IF
     IF (switch/='-u') THEN
        WRITE (*, '(''WARNING(prog_passwd): ignoring switch '',a)') switch
     ELSE
        CALL get_command_argument(2, length=len_name)
        DEALLOCATE (name)
        ALLOCATE (CHARACTER(LEN=len_name) :: name)
        CALL get_command_argument(2, value=name)
     END IF
  END IF
  !
  ! now invoke the C library routine
  my_pwent = getpwnam(name)

  IF (c_associated(my_pwent)) THEN
     home = get_homedir(my_pwent)
     WRITE (*, '(''Home directory of '',a,'' is '',a)') name, home
  ELSE
     WRITE (*, '(''User '',a,'' does not have an entry.'')') name
  END IF
END PROGRAM prog_passwd
