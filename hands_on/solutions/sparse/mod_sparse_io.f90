MODULE mod_sparse_io
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: set_element, sparse, OPERATOR (*)
  INTEGER, PARAMETER :: dk = kind(1.0D0)
  TYPE :: sparse
     PRIVATE
     INTEGER :: index = 0
     REAL (dk) :: value
     TYPE (sparse), POINTER :: next => null()
   CONTAINS
     FINAL :: delete_sparse
     PROCEDURE :: wf
     GENERIC :: WRITE (FORMATTED) => wf
  END TYPE sparse
  INTERFACE OPERATOR (*)
     PROCEDURE mvm
  END INTERFACE OPERATOR (*)
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
       END IF
    END DO
  END SUBROUTINE set_element
  FUNCTION mvm(obj, vec) RESULT (r)
    TYPE (sparse), INTENT (IN), TARGET :: obj(:)
    REAL (dk), INTENT (IN) :: vec(:)
    REAL (dk) :: r(size(vec))
    TYPE (sparse), POINTER :: p_obj
    INTEGER :: i

    IF (size(obj)==size(vec)) THEN
       DO i = 1, size(obj)
          r(i) = 0.0_dk
          p_obj => obj(i)
          DO
             IF (associated(p_obj)) THEN
                IF (p_obj%index>0 .AND. p_obj%index<=size(vec)) THEN
                   r(i) = r(i) + vec(p_obj%index)*p_obj%value
                END IF
                p_obj => p_obj%next
             ELSE
                EXIT
             END IF
          END DO
       END DO
    END IF
  END FUNCTION mvm
  SUBROUTINE wf(obj, unit, iotype, vlist, iostat, iomsg)
    CLASS (sparse), INTENT (IN) :: obj
    INTEGER, INTENT (IN) :: unit, vlist(:)
    CHARACTER (LEN=*), INTENT (IN) :: iotype
    INTEGER, INTENT (OUT) :: iostat
    CHARACTER (LEN=*), INTENT (INOUT) :: iomsg

    CHARACTER (LEN=64) :: pfmt
    CLASS (sparse), POINTER :: p_obj

    CALL wf_target(obj, unit, iotype, vlist, iostat, iomsg)

  CONTAINS
    SUBROUTINE wf_target(obj, unit, iotype, vlist, iostat, iomsg)
      CLASS (sparse), INTENT (IN), TARGET :: obj
      INTEGER, INTENT (IN) :: unit, vlist(:)
      CHARACTER (LEN=*), INTENT (IN) :: iotype
      INTEGER, INTENT (OUT) :: iostat
      CHARACTER (LEN=*), INTENT (INOUT) :: iomsg

      IF (trim(iotype)=='DTsparse') THEN
         IF (size(vlist)<3) THEN
            iostat = 1
            iomsg = 'mod_sparse::wf vlist too short'
            RETURN
         END IF
         WRITE (pfmt, '(a,i0,a,i0,a,i0,a)') '(i', vlist(1), ',1pe', vlist(2), &
              '.', vlist(3), ')'
         p_obj => obj
         WRITE (unit, FMT='(''NewSparse:'')')
         DO
            IF (associated(p_obj)) THEN
               IF (p_obj%index==0) EXIT
               WRITE (unit, FMT=pfmt) p_obj%index, p_obj%value
               p_obj => p_obj%next
            ELSE
               EXIT
            END IF
         END DO
         WRITE (unit, FMT='(/)')
      ELSE IF (trim(iotype)=='LISTDIRECTED') THEN
         p_obj => obj
         WRITE (unit, FMT=*) 'NewSparseLD:'
         DO
            IF (associated(p_obj)) THEN
               IF (p_obj%index==0) EXIT
               WRITE (unit, FMT=*) p_obj%index, p_obj%value
               p_obj => p_obj%next
            ELSE
               EXIT
            END IF
         END DO
      ELSE
         iostat = 2
         iomsg = 'mod_sparse::wf unsupported iotype.'
      END IF
    END SUBROUTINE wf_target
  END SUBROUTINE wf
  ELEMENTAL SUBROUTINE delete_sparse(obj)
    TYPE (sparse), INTENT (INOUT) :: obj

    IF (associated(obj%next)) DEALLOCATE (obj%next)
  END SUBROUTINE delete_sparse
END MODULE mod_sparse_io
