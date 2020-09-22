MODULE mod_sparse
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: set_element, sparse, write, OPERATOR (*)
  INTEGER, PARAMETER :: dk = kind(1.0D0)
  TYPE :: sparse
     PRIVATE
     INTEGER :: index = 0
     REAL (dk) :: value
     TYPE (sparse), POINTER :: next => null()
  END TYPE sparse
  ! FIXME - add operator overload
  INTERFACE write
     PROCEDURE write
     PROCEDURE write_rank1
  END INTERFACE write
CONTAINS
  SUBROUTINE set_element(obj, index, value)
    TYPE (sparse), INTENT (INOUT), TARGET :: obj
    INTEGER, INTENT (IN) :: index
    REAL (dk), INTENT (IN) :: value
    TYPE (sparse), POINTER :: p_obj, p_new

    IF (index<=0) RETURN
    p_obj => obj
    DO
       IF (associated(p_obj)) THEN
          IF (p_obj%index==0) THEN
             p_obj%index = index
             p_obj%value = value
             EXIT
          END IF
          IF (p_obj%index>index) THEN
             ALLOCATE (p_new)
             WRITE (*, *) 'allocated inside'
             p_new%index = p_obj%index
             p_new%value = p_obj%value
             p_obj%index = 0
             p_new%next => p_obj%next
             p_obj%next => p_new
          ELSE IF (p_obj%index==index) THEN
             p_obj%index = 0
          ELSE
             p_new => p_obj
             p_obj => p_obj%next
          END IF
       ELSE
          ALLOCATE (p_obj)
          p_new%next => p_obj
          WRITE (*, *) 'allocated at end'
       END IF
    END DO
  END SUBROUTINE set_element
  ! FIXME add module procedure for Matrix-Vector Multiply
  SUBROUTINE write(obj)
    TYPE (sparse), INTENT (IN), TARGET :: obj
    TYPE (sparse), POINTER :: p_obj
    INTEGER :: nelems

    p_obj => obj
    nelems = 0
    DO
       IF (associated(p_obj)) THEN
          IF (p_obj%index==0) EXIT
          nelems = nelems + 1
          WRITE (*, ADVANCE='NO', FMT='(''('',i0,'','',1pe9.2,'')'')') &
               p_obj%index, p_obj%value
          p_obj => p_obj%next
       ELSE
          EXIT
       END IF
    END DO
    WRITE (*, FMT='('' total '',i0,'' elements.'')') nelems
  END SUBROUTINE write
  SUBROUTINE write_rank1(obj)
    TYPE (sparse), INTENT (IN) :: obj(:)
    INTEGER :: i

    DO i = 1, size(obj)
       CALL write(obj(i))
    END DO
  END SUBROUTINE write_rank1
END MODULE mod_sparse
