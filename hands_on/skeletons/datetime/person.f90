MODULE mod_person
  USE mod_date
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: person
  INTEGER, PARAMETER :: nmx = 128
  TYPE :: person
     PRIVATE
     CHARACTER (LEN=nmx) :: name
     CLASS (date), POINTER :: birthday => null()
  END TYPE person
  INTERFACE person
     MODULE PROCEDURE person1
  END INTERFACE person
CONTAINS
  !  FIXME: add module procedures
END MODULE mod_person
    PROGRAM test_person
      USE mod_person
      USE mod_date
      IMPLICIT NONE

      TYPE (person) :: p1, p2

      p1 = person('Joachim', date(year=1977,mon='Jan',day=23))
      p2 = person('Anneliese', datetime(day=2,mon=3,year=1981,hour=3,min=34, &
        sec=0))
!  FIXME: add statements that print birthdays of p1 and p2
    END PROGRAM
