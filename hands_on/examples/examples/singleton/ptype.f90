MODULE mod_ptype
  ! illustrates the static singleton:
  ! a single public entity of private type
  IMPLICIT none
  PRIVATE
  INTEGER, PARAMETER, PUBLIC :: pdim = 20
  TYPE, PRIVATE :: ptype
     REAL, PUBLIC :: field (pdim, pdim)
  END TYPE ptype

  TYPE (ptype), PUBLIC :: o_ptype
END MODULE mod_ptype

PROGRAM ptype
  USE mod_ptype
  IMPLICIT NONE

  INTEGER :: i, j, low, high
  INTEGER, parameter :: xdim = 3
  REAL :: x(xdim, xdim)

  !  type(ptype) :: o2  
  ! the above line will not compile

  DO j=1, pdim
     DO i=1, pdim
        o_ptype % field (i,j) = real(i + j)
     END DO
  END DO
  low = 7
  high = low + xdim - 1
  
  x(:,:) = o_ptype % field (low:high,low:high)
  WRITE (*, FMT='(a,/,3(3F10.2,/))') 'value of 3x3 section: ', x

  !  o_ptype = ptype(reshape([(0.0,i=1,pdim**2)],shape(o_ptype%field)))
  ! the use of the structure constructor is not allowed
END PROGRAM ptype
