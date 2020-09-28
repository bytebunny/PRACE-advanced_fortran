PROGRAM test_sparse_simple
  USE mod_sparse_simple
  IMPLICIT NONE

  INTEGER, PARAMETER :: ndim = 10, dk = kind(1.0D0)
  INTEGER :: i, j
  TYPE (sparse), ALLOCATABLE :: sa(:)
  REAL (dk), ALLOCATABLE :: x(:)

  ALLOCATE (sa(ndim), x(ndim))
  CALL write(sa)
  DO i = 1, ndim
     CALL set_element(sa(i), 1, 1.0_dk)
     CALL set_element(sa(i), ndim, 2.0_dk)
     CALL set_element(sa(i), i, real(i,dk))
     CALL set_element(sa(i), 2, -1.0_dk)
     CALL set_element(sa(i), ndim-1, -2.0_dk)
  END DO
  CALL write(sa)
  x(:) = [ (real(i,dk),i=1,ndim) ]
  x(:) = sa*x
  WRITE (*, *) 'Result of MVM is: '
  WRITE (*, FMT='(10(1pe9.2))') x

  DO i = 1, size(sa)
     CALL delete_sparse(sa(i))
  END DO
  DEALLOCATE (sa, x)
END PROGRAM test_sparse_simple
