MODULE mod_passwd
  !
  ! provide a Fortran interface to the network C/POSIX API 
  ! call getpwnam(3p)
  !
  USE, INTRINSIC :: iso_c_binding
  IMPLICIT NONE

  PRIVATE :: strlen

  !
  ! type definition
  TYPE, BIND (C) :: passwd
     TYPE (c_ptr) :: pw_name, pw_passwd
     INTEGER (c_int) :: uid, gid ! defined as unsigned int
     TYPE (c_ptr) :: pw_gecos, pw_dir, pw_shell
  END TYPE passwd

  !
  ! C interface for POSIX calls
  INTERFACE
     !
     !    thin binding is used at cost of type safety
     TYPE (c_ptr) FUNCTION getpwnam(name) BIND (C)
       IMPORT :: c_char, c_ptr
       CHARACTER (KIND=c_char) :: name(*)
     END FUNCTION getpwnam
     !    this purely private variant needs to take a c_ptr
     !    and therefore can be misused to shoot oneself in the foot
     INTEGER (c_size_t) FUNCTION strlen(s) BIND (C)
       IMPORT :: c_ptr, c_size_t
       TYPE (c_ptr), VALUE :: s
     END FUNCTION strlen
  END INTERFACE
CONTAINS
  !
  ! module procedure to extract home directory from the pwent
  FUNCTION get_homedir(pwent)
    TYPE (c_ptr), VALUE :: pwent
    CHARACTER (LEN=:), ALLOCATABLE :: get_homedir

    CHARACTER (c_char), POINTER :: c(:)
    TYPE (passwd), POINTER :: p
    INTEGER :: h_len, i

    CALL c_f_pointer(pwent, p)
    h_len = strlen(p%pw_dir)
    CALL c_f_pointer(p%pw_dir, c, (/h_len/) )
    ALLOCATE (CHARACTER(LEN=h_len) :: get_homedir)
    DO i = 1, h_len
       get_homedir(i:i) = char(ichar(c(i)), c_char)
    END DO
  END FUNCTION get_homedir
END MODULE mod_passwd

