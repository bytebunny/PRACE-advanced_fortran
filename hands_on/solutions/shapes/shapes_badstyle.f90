MODULE mod_shapes
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: set, print, adjust
  TYPE, PUBLIC :: rectangle
     PRIVATE
     REAL :: length = 0.0
     REAL :: breadth = 0.0
   CONTAINS
     PROCEDURE, NON_OVERRIDABLE :: area
     PROCEDURE :: stretch_length
     PROCEDURE :: stretch_breadth
  END TYPE rectangle

  TYPE, PUBLIC, EXTENDS (rectangle) :: square
     PRIVATE
   CONTAINS
     !    need to override since otherwise may not get a square ...
     PROCEDURE :: stretch_length => stretch_square_length
     PROCEDURE :: stretch_breadth => stretch_square_breadth
  END TYPE square
  INTERFACE set
     MODULE PROCEDURE set_rectangle
     MODULE PROCEDURE set_square
  END INTERFACE set
CONTAINS
  REAL FUNCTION area(this)
    CLASS (rectangle), INTENT (IN) :: this

    area = this%length*this%breadth
  END FUNCTION area
  FUNCTION set_rectangle(length, breadth) RESULT (r)
    REAL, INTENT (IN) :: length, breadth
    TYPE (rectangle) :: r

    r%length = length
    r%breadth = breadth
  END FUNCTION set_rectangle
  FUNCTION set_square(length) RESULT (r)
    REAL, INTENT (IN) :: length
    TYPE (square) :: r

    r%length = length
    r%breadth = length
  END FUNCTION set_square
  SUBROUTINE stretch_length(this, factor)
    CLASS (rectangle), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%length = factor*this%length
  END SUBROUTINE stretch_length
  SUBROUTINE stretch_breadth(this, factor)
    CLASS (rectangle), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%breadth = factor*this%breadth
  END SUBROUTINE stretch_breadth
  SUBROUTINE stretch_square_length(this, factor)
    CLASS (square), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%length = factor*this%length
    this%breadth = factor*this%breadth
  END SUBROUTINE stretch_square_length
  SUBROUTINE stretch_square_breadth(this, factor)
    CLASS (square), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%length = factor*this%length
    this%breadth = factor*this%breadth
  END SUBROUTINE stretch_square_breadth
  !
  ! the following function shows troublesome behaviour
  !
  SUBROUTINE adjust(r, val_min)
    TYPE (rectangle) :: r
    REAL, INTENT (IN) :: val_min
    !   note that we can hand in a polymorphic entity which is
    !   type compatible with rectangle here, i.e. a square. 
    !   The function would not corrupt a square 
    !   if the declaration were
    !   class(rectangle) :: r
    REAL :: x

    x = sqrt(r%area())
    IF (x<val_min) THEN
       CALL r%stretch_length(val_min/x)
    END IF
  END SUBROUTINE adjust
  SUBROUTINE print(this)
    CLASS (rectangle) :: this

    WRITE (*, '(''A rectangle with dimensions '',2(F10.3,2X))') &
         this%length, this%breadth
  END SUBROUTINE print
END MODULE mod_shapes

    PROGRAM test_shapes
      USE mod_shapes
      IMPLICIT NONE
      CLASS (rectangle), ALLOCATABLE :: o_rec

      ALLOCATE (o_rec, SOURCE=set(2.0,3.0))
      WRITE (*, *) 'o_rec a rectangle with dimensions: '
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      WRITE (*, *) 'o_rec stretched by 1.2 in length - new dimensions: '
      CALL o_rec%stretch_length(1.2)
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      DEALLOCATE (o_rec)

      ALLOCATE (o_rec, SOURCE=set(2.8))
      WRITE (*, *) 'o_rec a square with dimensions: '
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      WRITE (*, *) 'o_rec stretched by 1.2 in length - new dimensions: '
      CALL o_rec%stretch_breadth(1.2)
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      DEALLOCATE (o_rec)
!
      ALLOCATE (o_rec, SOURCE=set(2.8))
      WRITE (*, *) 'o_rec a square with dimensions: '
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      CALL adjust(o_rec, 3.2)
      WRITE (*, *) 'o_rec after call to adjust - new dimensions: '
      CALL print(o_rec)
      WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
      DEALLOCATE (o_rec)
    END PROGRAM
