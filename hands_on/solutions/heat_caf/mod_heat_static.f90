    MODULE mod_heat_static
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: heat_ival, heat_bval, heat_iter, heat_print
      INTEGER, PARAMETER, PUBLIC :: dk = selected_real_kind(12, 100)
! NOTE: mod(ndim, nimg) == 0
      INTEGER, PARAMETER, PUBLIC :: nimg = 4
      INTEGER, PARAMETER, PUBLIC :: ndim = 200, cdim = ndim/nimg
!
!> discretization of temperature field on a unit square
      REAL (dk) :: phi(ndim, 0:cdim+1), phinew(ndim, 0:cdim+1)[ * ]
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

        INTEGER :: i, j, jmin, jmax, me
        REAL (dk) :: x, y

        initialized = initialized + 1

        me = this_image()
        jmin = 0
        jmax = size(phi, 2) - 1

        IF (me==1) jmin = jmin + 2 ! global first column not filled
        IF (me==num_images()) jmax = jmax - 2 ! global ndim column not filled

        DO j = jmin, jmax
          y = dy*real(j+(me-1)*cdim-1, dk)
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

        INTEGER :: i, j, jmin, jmax, me
        REAL (dk) :: x, y

        me = this_image()
        jmin = 0
        jmax = size(phi, 2) - 1

        IF (me==1) jmin = jmin + 1 ! global zeroth column not filled
        IF (me==num_images()) jmax = jmax - 1 ! global ndim column not filled

        SELECT CASE (side)
        CASE ('n', 'N')
          DO j = jmin, jmax
            y = dy*real(j+(me-1)*cdim-1, dk)
            phi(1, j) = fbval(y)
          END DO
          initialized = initialized + 2
        CASE ('s', 'S')
          DO j = jmin, jmax
            y = dy*real(j+(me-1)*cdim-1, dk)
            phi(size(phi,1), j) = fbval(y)
          END DO
          initialized = initialized + 3
        CASE ('w', 'W')
          IF (me==1) THEN
            DO i = 1, size(phi, 1)
              x = dx*real(i-1, dk)
              phi(i, 1) = fbval(x)
            END DO
          END IF
          initialized = initialized + 4
        CASE ('e', 'E')
          IF (me==num_images()) THEN
            DO i = 1, size(phi, 1)
              x = dx*real(i-1, dk)
              phi(i, cdim) = fbval(x)
            END DO
          END IF
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

        INTEGER :: i, j, jmin, jmax, it, me
        REAL (dk) :: dphi, dphimax, d2x, d2y

        IF (initialized/=15) THEN
          WRITE (*, *) 'mod_heat::heat_iter: not properly initialized.'
          WRITE (*, *) '                     Aborting execution.'
          STOP
        END IF

        me = this_image()
        jmin = 1
        jmax = size(phi, 2) - 2

        IF (me==1) jmin = jmin + 1 ! global first column not modified
        IF (me==num_images()) jmax = jmax - 1 ! global cdim column not modified

        d2x = 1.0_dk/(dx*dx)
        d2y = 1.0_dk/(dy*dy)
        DO it = 1, num
          num_iter = num_iter + 1
          dphimax = 0.0_dk
          DO j = jmin, jmax
            DO i = 2, size(phi, 1) - 1
              dphi = dt*((phi(i+1,j)+phi(i-1,j)-2.0_dk*phi(i,j))*d2x+(phi(i, &
                j+1)+phi(i,j-1)-2.0_dk*phi(i,j))*d2y)
              dphimax = max(dphimax, abs(dphi))
              phinew(i, j) = phi(i, j) + dphi
            END DO
          END DO

          SYNC ALL ! to guarantee that all phinew values are up to date
          IF (me>1) phi(2:size(phi,1)-1, jmin-1) = phinew(2:size(phi,1)-1, &
            jmax)[ me-1 ]
          IF (me<num_images()) phi(2:size(phi,1)-1, jmax+1) &
            = phinew(2:size(phi,1)-1, jmin)[ me+1 ]

          phi(2:size(phi,1)-1, jmin:jmax) = phinew(2:size(phi,1)-1, jmin:jmax)


          SYNC ALL ! to guarantee that reads to phinew above are protected
! against subsequent updates

        END DO
        heat_iter = dphimax
      END FUNCTION
!
!> write out (a subset of) values of field phi
      SUBROUTINE heat_print()
        INTEGER, PARAMETER :: pmax = 10, qmax = 30, img_print = 1
        INTEGER :: i, imax, jmin, jmax, me

        jmin = 1
        jmax = min(pmax, size(phi,2)-2)
        imax = min(qmax, size(phi,1))
        IF (this_image()==img_print) THEN
          WRITE (*, FMT='(''Temperature field after iteration '',i0,'':'')') &
            num_iter
          DO i = 1, imax
            WRITE (*, FMT='(10(f7.4,1x))') phi(i, jmin:jmax)
          END DO
          WRITE (*, FMT='(80(''-''))')
        END IF
      END SUBROUTINE
    END MODULE
