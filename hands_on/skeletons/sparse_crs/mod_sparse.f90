MODULE mod_sparse
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: set_element, sparse, write, OPERATOR (*)
  INTEGER, PARAMETER :: dk = kind(1.0D0)
  TYPE :: sparse
     PRIVATE


     integer, allocatable   :: indices(:)
     real (dk), allocatable :: values(:)

     ! contains
     !     final :: delete_sparse ! add finalizer.


  END TYPE sparse


  interface sparse
      module procedure create
  end interface sparse

  interface operator (*) ! add operator overload
      procedure :: multiply_sparse
  end interface operator (*)



  INTERFACE write
     PROCEDURE write
     PROCEDURE write_rank1
  END INTERFACE write
CONTAINS

    function create( col_ind, values ) result( r )
        integer, intent( in )    :: col_ind(:)
        real( dk ), intent( in ) :: values(:)
        type( sparse )           :: r
        
        integer                  :: n

        n = size( values )

        if ( ( n > size( col_ind ) ) ) then
            stop 'Error(mod_sparse): insufficient number of columns'
        end if

        ! for production quality, checking index sortedness may be desirable
        r%indices = col_ind(:n)
        r%values = values(:)
    end function create


  SUBROUTINE set_element(obj, index, value)
    TYPE (sparse), INTENT (INOUT), TARGET :: obj
    INTEGER, INTENT (IN) :: index
    REAL (dk), INTENT (IN) :: value
    
    integer                :: ix, iloc, n
    type( sparse )         :: aux

    if ( .not. allocated( obj%values ) ) then
        allocate( obj%indices(1), source=[index] )
        allocate( obj%values(1), source=[value] )
        
        return 

    else
        iloc = 0
        n = size( obj%values )

        do ix = 1, n ! loop over row
            if ( obj%indices( ix ) == index ) then
                obj%values( ix ) = value

                return

            else if ( obj%indices( ix ) > index ) then

                exit
                
            end if
            iloc = iloc + 1
        end do

        allocate( aux%indices( n + 1 ), aux%values( n + 1 ) )
        aux%indices( : iloc ) = obj%indices( : iloc )
        aux%values( : iloc ) = obj%values( : iloc )

        aux%indices( iloc + 1 ) = index
        aux%values( iloc + 1 ) = value
        
        aux%indices( iloc + 2 : ) = obj%indices( iloc + 1 : )
        aux%values( iloc + 2 : ) = obj%values( iloc + 1 : )

        call move_alloc( aux%indices, obj%indices )
        call move_alloc( aux%values, obj%values )

    end if

  END SUBROUTINE set_element
  

  function multiply_sparse( mat, vec ) result(r)! add module procedure for Matrix-Vector Multiply
      type( sparse ), intent( in ), target :: mat(:)
      real( dk ), intent( in )             :: vec(:)
      real( dk )                           :: r( size(vec) ) 
      integer                              :: ix, jx
      
      if ( size(mat) == size( vec ) ) then ! if matrix multiplication is possible.
          do ix = 1, size(mat)
              r( ix ) = 0.0_dk
              
              do jx = 1, size( mat( ix )%indices )
                  ! if indices (column numbers) are within bounds:
                  if ( mat( ix )%indices( jx ) > 0 .and. &
                         mat( ix )%indices( jx ) <= size( vec ) ) then
                      r( ix ) = r( ix ) + &
                             mat( ix )%values( jx ) * vec( mat( ix )%indices( jx ) )
                  end if
              end do
          end do
      end if

  end function multiply_sparse


  SUBROUTINE write(obj)
    TYPE (sparse), INTENT (IN), TARGET :: obj

    integer :: ix, n_elems

    n_elems = 0
    
    IF ( allocated( obj%indices ) ) THEN
        n_elems = size( obj%indices )
        DO ix = 1, size( obj%indices )
            WRITE (*, ADVANCE='NO', FMT='(''('',i0,'','',1pe9.2,'')'')') &
                   obj%indices( ix ), obj%values( ix )
        END DO
    END IF
    WRITE (*, FMT='('' total '',i0,'' elements.'')') n_elems
  END SUBROUTINE write
  SUBROUTINE write_rank1(obj)
    TYPE (sparse), INTENT (IN) :: obj(:)
    INTEGER :: i

    DO i = 1, size(obj)
       CALL write(obj(i))
    END DO
  END SUBROUTINE write_rank1


  ! elemental subroutine delete_sparse( mat )
  !     type( sparse ), intent( inout ) :: mat

  !     if ( associated( mat%next ) ) deallocate( mat%next )
  ! end subroutine delete_sparse


END MODULE mod_sparse
