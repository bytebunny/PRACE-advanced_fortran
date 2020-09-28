MODULE mod_shapes
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: set, print, adjust

  TYPE, PUBLIC, ABSTRACT :: shape
   CONTAINS
     PROCEDURE, NON_OVERRIDABLE :: area
     PROCEDURE (deform_fun), DEFERRED :: stretch
  END TYPE shape

  ABSTRACT INTERFACE
     SUBROUTINE deform_fun(this, factor)
       IMPORT :: shape
       CLASS (shape), INTENT (INOUT) :: this
       REAL, INTENT (IN) :: factor
     END SUBROUTINE deform_fun
  END INTERFACE

  TYPE, PUBLIC, EXTENDS (shape) :: rectangle
     PRIVATE
     REAL :: length = 0.0
     REAL :: breadth = 0.0
   CONTAINS
     PROCEDURE :: stretch => deform_rectangle
  END TYPE rectangle

  TYPE, PUBLIC, EXTENDS (shape) :: square
     PRIVATE
     REAL :: length = 0.0
   CONTAINS
     PROCEDURE :: stretch => deform_square
  END TYPE square
  INTERFACE set
     MODULE PROCEDURE set_rectangle
     MODULE PROCEDURE set_square
  END INTERFACE set
CONTAINS
  REAL FUNCTION area(this)
    CLASS (shape), INTENT (IN) :: this
    !
    SELECT TYPE (this)
    TYPE IS (rectangle)
       area = this%length*this%breadth
    TYPE IS (square)
       area = this%length*this%length
    CLASS DEFAULT
       STOP 'mod_shapes::area - unimplemented subclass of SHAPE.'
    END SELECT
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
  END FUNCTION set_square
  SUBROUTINE deform_rectangle(this, factor)
    CLASS (rectangle), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%length = factor*this%length
    this%breadth = factor*this%breadth
  END SUBROUTINE deform_rectangle
  SUBROUTINE deform_square(this, factor)
    CLASS (square), INTENT (INOUT) :: this
    REAL, INTENT (IN) :: factor

    this%length = factor*this%length
  END SUBROUTINE deform_square
  ! the following function is now OK - because the base type
  ! is abstract we're forced to declare it as class(shape)
  SUBROUTINE adjust(r, val_min)
    CLASS (shape) :: r
    REAL, INTENT (IN) :: val_min
    REAL :: x

    x = sqrt(r%area())
    IF (x<val_min) THEN
       CALL r%stretch(val_min/x)
    END IF
  END SUBROUTINE adjust
  SUBROUTINE print(this)
    CLASS (shape) :: this
    !    
    SELECT TYPE (this)
    TYPE IS (rectangle)
       WRITE (*, '(''A rectangle with dimensions '',2(F10.3,2X))') &
            this%length, this%breadth
    TYPE IS (square)
       WRITE (*, '(''A square with side '',F10.3,2X)') this%length
    CLASS DEFAULT
       STOP 'mod_shapes::print - unimplemented subclass of SHAPE.'
    END SELECT
  END SUBROUTINE print
END MODULE mod_shapes

        PROGRAM test_shapes
          USE mod_shapes
          IMPLICIT NONE
          CLASS (shape), ALLOCATABLE :: o_rec

          ALLOCATE (o_rec, SOURCE=set(2.0,3.0))
          WRITE (*, *) 'o_rec a rectangle with dimensions: '
          CALL print(o_rec)
          WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
          WRITE (*, *) 'o_rec stretched by 1.2 in length - new dimensions: '
          CALL o_rec%stretch(1.2)
          CALL print(o_rec)
          WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
          DEALLOCATE (o_rec)

          ALLOCATE (o_rec, SOURCE=set(2.8))
          WRITE (*, *) 'o_rec a square with dimensions: '
          CALL print(o_rec)
          WRITE (*, '(''o_rec area: '',F10.3)') o_rec%area()
          WRITE (*, *) 'o_rec stretched by 1.2 in length - new dimensions: '
!
! The naming convention is improved. 
! Note how we force overriding of the deformation function.
!
          CALL o_rec%stretch(1.2)

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

