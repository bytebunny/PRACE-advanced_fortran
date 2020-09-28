    MODULE mod_heat
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: heat_ival, heat_bval, heat_iter, heat_print
      INTEGER, PARAMETER, PUBLIC :: dk = selected_real_kind(12, 100)
      INTEGER, PARAMETER, PUBLIC :: ndim = 200
!
!> discretization of temperature field on a unit square
      REAL (dk) :: phi(ndim, ndim), phinew(ndim, ndim)
!
!> space increments depend on discretization
      REAL (dk) :: dx = 1.0_dk/real(ndim-1, dk), dy = 1.0_dk/real(ndim-1, dk)
!
!> iteration count 
      INTEGER, PUBLIC, PROTECTED, SAVE :: num_iter = 0
      INTEGER, SAVE :: initialized = 0
!
!> abstract interfaces for procedure arguments
      ABSTRACT INTERFACE
        PURE FUNCTION f1(x) RESULT (f)
          IMPORT :: dk
!  dk is not available via host association, hence must be imported
          REAL (dk), INTENT (IN) :: x
          REAL (dk) :: f
        END FUNCTION
        PURE FUNCTION f2(x, y) RESULT (f)
          IMPORT :: dk
          REAL (dk), INTENT (IN) :: x, y
          REAL (dk) :: f
        END FUNCTION
      END INTERFACE
    CONTAINS
!
!> set initial values for interior of temperature field
      SUBROUTINE heat_ival(fival)
        PROCEDURE (f2) :: fival

        INTEGER :: i, j
        REAL (dk) :: x, y

        initialized = initialized + 1

        DO j = 2, size(phi, 2) - 1
          y = dy*real(j-1, dk)
          DO i = 2, size(phi, 1) - 1
            x = dx*real(i-1, dk)
            phi(i, j) = fival(x, y)
          END DO
        END DO
      END SUBROUTINE
!
!> set boundary values of temperature field
!> these must be preserved throughout any field updates
      SUBROUTINE heat_bval(side, fbval)
        CHARACTER, INTENT (IN) :: side
        PROCEDURE (f1) :: fbval

        INTEGER :: i, j
        REAL (dk) :: x, y

        SELECT CASE (side)
        CASE ('n', 'N')
          DO j = 1, size(phi, 2)
            y = dy*real(j-1, dk)
            phi(1, j) = fbval(y)
          END DO
          initialized = initialized + 2
        CASE ('s', 'S')
          DO j = 1, size(phi, 2)
            y = dy*real(j-1, dk)
            phi(size(phi,1), j) = fbval(y)
          END DO
          initialized = initialized + 3
        CASE ('w', 'W')
          DO i = 1, size(phi, 1)
            x = dx*real(i-1, dk)
            phi(i, 1) = fbval(x)
          END DO
          initialized = initialized + 4
        CASE ('e', 'E')
          DO i = 1, size(phi, 1)
            x = dx*real(i-1, dk)
            phi(i, size(phi,2)) = fbval(x)
          END DO
          initialized = initialized + 5
        CASE DEFAULT
          WRITE (*, *) &
            'mod_heat::heat_bval: incorrect value for argument SIDE.'
          WRITE (*, *) '                     Aborting execution.'
          STOP
        END SELECT
      END SUBROUTINE
!
!> perform a specified number of Jacobi iterations 
!> with a provided time increment and return the 
!> absolute maximum deviation from the last iteration.
!> Note that the number of arguments differs from that on 
!> the slides, to enable inlining of a configurable
!> number of iterations.
      REAL (dk) FUNCTION heat_iter(dt, num)
        REAL (dk), INTENT (IN) :: dt
        INTEGER, INTENT (IN) :: num

        INTEGER :: i, j, it
        REAL (dk) :: dphi, dphimax, d2x, d2y

        IF (initialized/=15) THEN
          WRITE (*, *) 'mod_heat::heat_iter: not properly initialized.'
          WRITE (*, *) '                     Aborting execution.'
          STOP
        END IF

        d2x = 1.0_dk/(dx*dx)
        d2y = 1.0_dk/(dy*dy)
        DO it = 1, num
          num_iter = num_iter + 1
          dphimax = 0.0_dk
          DO j = 2, size(phi, 2) - 1
            DO i = 2, size(phi, 1) - 1
              dphi = dt*((phi(i+1,j)+phi(i-1,j)-2.0_dk*phi(i,j))*d2x+(phi(i, &
                j+1)+phi(i,j-1)-2.0_dk*phi(i,j))*d2y)
              dphimax = max(dphimax, abs(dphi))
              phinew(i, j) = phi(i, j) + dphi
            END DO
          END DO
          phi(2:size(phi,1)-1, 2:size(phi,2)-1) = phinew(2:size(phi,1)-1, &
            2:size(phi,2)-1)
        END DO
        heat_iter = dphimax
      END FUNCTION
!
!> write out (a subset of) values of field phi
      SUBROUTINE heat_print()
        INTEGER, PARAMETER :: pmax = 10, qmax = 30
        INTEGER :: i, imax, jmax

        jmax = min(pmax, size(phi,2))
        imax = min(qmax, size(phi,1))
        WRITE (*, FMT='(''Temperature field after iteration '',i0,'':'')') &
          num_iter
        DO i = 1, imax
          WRITE (*, FMT='(10(f7.4,1x))') phi(i, 1:jmax)
        END DO
        WRITE (*, FMT='(80(''-''))')
      END SUBROUTINE
    END MODULE
