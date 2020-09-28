MODULE mod_sparse
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: set_element, sparse, write, OPERATOR (*)
  INTEGER, PARAMETER :: dk = kind(1.0D0)
  TYPE :: sparse
     PRIVATE
     REAL (dk), ALLOCATABLE :: value(:)
     INTEGER, ALLOCATABLE :: idx(:)
  END TYPE sparse
  INTERFACE sparse
     MODULE PROCEDURE create
  END INTERFACE sparse
  INTERFACE OPERATOR (*)
     PROCEDURE mvm
  END INTERFACE OPERATOR (*)
  INTERFACE write
     PROCEDURE write
     PROCEDURE write_rank1
  END INTERFACE write
CONTAINS
  FUNCTION create(colidx, values) RESULT (r)
    INTEGER, INTENT (IN) :: colidx(:)
    REAL (dk), INTENT (IN) :: values(:)
    TYPE (sparse) :: r

    INTEGER :: i, n

    n = size(values, 1)
    IF (n>size(colidx,1)) STOP &
         'Error(mod_sparse): insufficient number of columns'
    ! for production quality, checking index sortedness may be desirable
    r%idx = colidx(:n)
    r%value = values(:)
  END FUNCTION create
  SUBROUTINE set_element(obj, index, value)
    TYPE (sparse), INTENT (INOUT) :: obj
    INTEGER, INTENT (IN) :: index
    REAL (dk), INTENT (IN) :: value
    INTEGER :: i, iloc, n
    TYPE (sparse) :: aux

    IF (.NOT. allocated(obj%value)) THEN
       ALLOCATE (obj%idx(1), SOURCE=[index] )
       ALLOCATE (obj%value(1), SOURCE=[value] )
       RETURN
    ELSE
       iloc = 0
       n = size(obj%value, 1)
       row:      DO i = 1, n
          IF (obj%idx(i)==index) THEN
             obj%value(i) = value
             RETURN
          ELSE IF (obj%idx(i)>index) THEN
             EXIT row
          END IF
          iloc = iloc + 1
       END DO row
       ALLOCATE (aux%idx(n+1), aux%value(n+1))
       aux%idx(:iloc) = obj%idx(:iloc)
       aux%value(:iloc) = obj%value(:iloc)
       aux%idx(iloc+1) = index
       aux%value(iloc+1) = value
       aux%idx(iloc+2:) = obj%idx(iloc+1:)
       aux%value(iloc+2:) = obj%value(iloc+1:)

       CALL move_alloc(aux%idx, obj%idx)
       CALL move_alloc(aux%value, obj%value)
    END IF
  END SUBROUTINE set_element
  FUNCTION mvm(obj, vec) RESULT (r)
    TYPE (sparse), INTENT (IN) :: obj(:)
    REAL (dk), INTENT (IN) :: vec(:)
    REAL (dk) :: r(size(vec))

    INTEGER :: i, ni, j, nj

    ni = size(obj)
    IF (ni==size(vec,1)) THEN
       DO i = 1, ni
          r(i) = 0.0_dk
          nj = size(obj(i)%idx, 1)
          DO j = 1, nj
             ! no checks for correct range done here - should be added
             r(i) = r(i) + obj(i)%value(j)*vec(obj(i)%idx(j))
          END DO
       END DO
    END IF
  END FUNCTION mvm
  SUBROUTINE write(obj)
    TYPE (sparse), INTENT (IN) :: obj
    INTEGER :: i, nelem

    nelem = 0
    IF (allocated(obj%idx)) THEN
       nelem = size(obj%idx)
       DO i = 1, size(obj%idx)
          WRITE (*, ADVANCE='NO', FMT='(''('',i0,'','',1pe9.2,'')'')') &
               obj%idx(i), obj%value(i)
       END DO
    END IF
    WRITE (*, FMT='('' total '',i0,'' elements.'')') nelem
  END SUBROUTINE write
  SUBROUTINE write_rank1(obj)
    TYPE (sparse), INTENT (IN) :: obj(:)
    INTEGER :: i

    DO i = 1, size(obj)
       CALL write(obj(i))
    END DO
  END SUBROUTINE write_rank1
END MODULE mod_sparse
