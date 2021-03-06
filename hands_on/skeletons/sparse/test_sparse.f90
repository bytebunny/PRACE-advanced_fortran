PROGRAM test_sparse
  USE mod_sparse
  IMPLICIT NONE

  INTEGER, PARAMETER :: ndim = 10, dk = kind(1.0D0)
  INTEGER :: i, j
  TYPE (sparse), ALLOCATABLE :: sa(:)
  REAL (dk), ALLOCATABLE :: x(:)

  ALLOCATE (sa(ndim), x(ndim))
  write (*, *) 'Before initialization:'
  CALL write(sa)
  DO i = 1, ndim
     CALL set_element(sa(i), 1, 1.0_dk)
     CALL set_element(sa(i), ndim, 2.0_dk)
     CALL set_element(sa(i), i, real(i,dk))
     CALL set_element(sa(i), 2, -1.0_dk)
     CALL set_element(sa(i), ndim-1, -2.0_dk)
  END DO
  write (*, *) 'After initialization:'
  CALL write(sa)
  x(:) = [ (real(i,dk),i=1,ndim) ]
  x(:) = sa*x
  WRITE (*, *) 'Result of MVM is: '
  WRITE (*, FMT='(10(1pe9.2))') x

  DEALLOCATE (sa, x)
END PROGRAM test_sparse
