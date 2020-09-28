MODULE mod_person
  USE mod_date
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: person, birthday
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
  TYPE (person) FUNCTION person1(name, d)
    CHARACTER (LEN=*), INTENT (IN) :: name
    CLASS (date), INTENT (IN) :: d

    person1%name = name
    IF (associated(person1%birthday)) DEALLOCATE (person1%birthday)
    ALLOCATE (person1%birthday, SOURCE=d)
  END FUNCTION person1
  FUNCTION birthday(p) RESULT (b)
    TYPE (person) :: p
    CLASS (date), POINTER :: b

    b => p%birthday
  END FUNCTION birthday
END MODULE mod_person
PROGRAM test_person
  USE mod_person
  USE mod_date
  IMPLICIT NONE

  TYPE (person) :: p1, p2

  p1 = person('Joachim', date(year=1977,mon='Jan',day=23))
  p2 = person('Anneliese', datetime(day=2,mon='Mar',year=1981,hour=3,min=34,sec=0))
  ASSOCIATE (b1=>birthday(p1), b2=>birthday(p2))
    CALL b1%write('stdout')
    CALL b2%write('stdout')
  END ASSOCIATE
END PROGRAM test_person
