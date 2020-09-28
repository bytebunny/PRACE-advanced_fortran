PROGRAM test_sparse_overload
  USE mod_sparse
  IMPLICIT NONE

  INTEGER, PARAMETER :: ndim = 10, dk = kind(1.0D0)
  INTEGER :: i, j
  TYPE (sparse), ALLOCATABLE :: sa(:)
  REAL (dk), ALLOCATABLE :: x(:)

  ALLOCATE (sa(ndim), x(ndim))
  WRITE (*, *) 'Before initialization:'
  CALL write(sa)

  sa(1) = sparse( [1,2,ndim-1,ndim], [1._dk,-1._dk,-2._dk,2._dk] )
  sa(2) = sparse( [1,2,ndim-1,ndim], [1._dk,-1._dk,-2._dk,2._dk] )
  DO i = 3, ndim - 2
     sa(i) = sparse( [1,2,i,ndim-1,ndim], [1._dk,-1._dk,real(i, &
          dk),-2._dk,2._dk] )
  END DO
  sa(ndim-1) = sparse( [1,2,ndim-1,ndim], [1._dk,-1._dk,-2._dk,2._dk] )
  sa(ndim) = sparse( [1,2,ndim-1,ndim], [1._dk,-1._dk,-2._dk,10._dk] )
  WRITE (*, *) 'After initialization:'
  CALL write(sa)
  x(:) = [ (real(i,dk),i=1,ndim) ]
  x(:) = sa*x
  WRITE (*, *) 'Result of MVM is: '
  WRITE (*, FMT='(10(1pe9.2))') x

  DEALLOCATE (sa, x)
END PROGRAM test_sparse_overload
