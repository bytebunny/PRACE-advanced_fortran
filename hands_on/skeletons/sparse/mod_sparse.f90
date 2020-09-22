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


     contains
         final :: delete_sparse ! add finalizer.


  END TYPE sparse


  interface operator (*) ! add operator overload
      procedure :: multiply_sparse
  end interface operator (*)


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
  

  function multiply_sparse( mat, vec ) result(r)! add module procedure for Matrix-Vector Multiply
      type( sparse ), intent( in ), target :: mat(:)
      real( dk ), intent( in )             :: vec(:)
      real( dk )                           :: r( size(vec) ) 
      integer                              :: ix
      type( sparse ), pointer              :: p_mat
      
      if ( size(mat) == size( vec ) ) then ! if matrix multiplication is possible.
          do ix = 1, size(mat)
              r( ix ) = 0.0_dk
              p_mat => mat( ix ) ! get 1st element of the linked list that represents a row.
              
              do ! loop over the linked list.
                  if ( associated( p_mat ) ) then
                      ! if indices (column numbers) are within bounds:
                      if ( p_mat%index > 0 .and. p_mat%index <= size( vec ) ) then
                          r( ix ) = r( ix ) + p_mat%value * vec( p_mat%index )
                      end if
                      p_mat => p_mat%next
                  else
                      exit
                  end if
              end do
          end do
      end if


  end function multiply_sparse


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


  elemental subroutine delete_sparse( mat )
      type( sparse ), intent( inout ) :: mat

      if ( associated( mat%next ) ) deallocate( mat%next )
  end subroutine delete_sparse


END MODULE mod_sparse
