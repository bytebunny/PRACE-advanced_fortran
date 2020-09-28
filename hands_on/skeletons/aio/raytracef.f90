    MODULE mod_raytracef
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: is, intersect, shade, calc_tile
      INTEGER, PARAMETER :: is = selected_int_kind(2)
      DOUBLE PRECISION, PARAMETER :: objs(4, 11) = reshape( (/0.D0,0.D0, &
        -100.5D0,10000.D0,0.D0,0.D0,0.D0,0.25D0,0.272166D0,0.272166D0, &
        0.544331D0,.027777D0,0.643951D0,0.172546D0,0.D0,.027777D0,0.172546D0, &
        0.643951D0,0.D0,.027777D0,-0.371785D0,0.099620D0,0.544331D0,.027777D0, &
        -0.471405D0,0.471405D0,0.D0,.027777D0,-0.643951D0,-0.172546D0,0.D0, &
        .027777D0,0.099620D0,-0.371785D0,0.544331D0,.027777D0,-0.172546D0, &
        -0.643951D0,0.D0,.027777D0,0.471405D0,-0.471405D0,0.D0,.027777D0/), &
        (/4,11/) )
      DOUBLE PRECISION, PARAMETER :: objs_shade(3, 3) = reshape( (/4.D0,3.D0, &
        2.D0,1.D0,-4.D0,4.D0,-3.D0,1.D0,5.D0/), (/3,3/) )
      DOUBLE PRECISION :: small = 1.0D-6
    CONTAINS
      INTEGER FUNCTION intersect(x, y, z, dx, dy, dz, pmax)
        DOUBLE PRECISION, INTENT (IN) :: x, y, z, dx, dy, dz
        DOUBLE PRECISION, INTENT (INOUT) :: pmax
!
        INTEGER :: i
        DOUBLE PRECISION :: xx, yy, zz, b, t, pmaxloc
!
        intersect = 0
        pmaxloc = pmax
        DO i = 1, 11
          xx = objs(1, i) - x
          yy = objs(2, i) - y
          zz = objs(3, i) - z
          b = xx*dx + yy*dy + zz*dz
          t = b*b - xx*xx - yy*yy - zz*zz + objs(4, i)
          IF (t<0) THEN
            CYCLE
          ELSE
            t = b - sqrt(t)
            IF (t<small .OR. t>pmaxloc) THEN
              CYCLE
            END IF
          END IF
          intersect = i
          pmaxloc = t
        END DO
        pmax = pmaxloc
      END FUNCTION
      RECURSIVE FUNCTION shade(x, y, z, dx, dy, dz, id) RESULT (res)
        DOUBLE PRECISION :: res
        DOUBLE PRECISION, VALUE :: x, y, z
        DOUBLE PRECISION, INTENT (IN) :: dx, dy, dz
        INTEGER, INTENT (IN) :: id
!
        DOUBLE PRECISION :: pmax, c, r, k
        DOUBLE PRECISION :: nx, ny, nz, ldx, ldy, ldz, rdx, rdy, rdz
        INTEGER :: i
!
        pmax = 1.0D6
        i = intersect(x, y, z, dx, dy, dz, pmax)
        c = 0.0D0
        IF (i==0) THEN
          res = 0.0D0
        ELSE
          x = x + pmax*dx
          y = y + pmax*dy
          z = z + pmax*dz
          nx = x - objs(1, i)
          ny = y - objs(2, i)
          nz = z - objs(3, i)
          r = sqrt(nx*nx+ny*ny+nz*nz)
          nx = nx/r
          ny = ny/r
          nz = nz/r
          k = nx*dx + ny*dy + nz*dz
          rdx = dx - 2*k*nx
          rdy = dy - 2*k*ny
          rdz = dz - 2*k*nz
          DO i = 1, 3
            ldx = objs_shade(1, i) - x
            ldy = objs_shade(2, i) - y
            ldz = objs_shade(3, i) - z
            r = sqrt(ldx*ldx+ldy*ldy+ldz*ldz)
            ldx = ldx/r
            ldy = ldy/r
            ldz = ldz/r
            IF (intersect(x,y,z,ldx,ldy,ldz,r)/=0) CYCLE
            r = ldx*nx + ldy*ny + ldz*nz
            IF (r<0.0D0) CYCLE
            c = c + r
            r = rdx*ldx + rdy*ldy + rdz*ldz
            IF (r>0.0D0) THEN
              c = c + 2.0D0*r**15
            END IF
          END DO
          IF (id<10) c = c + 0.5D0*shade(x, y, z, rdx, rdy, rdz, id+1)
          res = c
        END IF
      END FUNCTION
      SUBROUTINE calc_tile(size, ixstart, iystart, tilesize, tile)
        INTEGER, INTENT (IN) :: size, ixstart, iystart, tilesize
        INTEGER (KIND=is), INTENT (OUT) :: tile(size, *)
!
        DOUBLE PRECISION :: dx, dy, dz, c, r, xx, yy
        INTEGER :: ix, iy

        DO iy = 1, tilesize
          DO ix = 1, tilesize
            xx = (ixstart+ix-1)/dble(size-1)
            yy = 1.0D0 - (iystart+iy-1)/dble(size-1)
            dx = -0.847569D0 - xx*1.30741D0 - yy*1.19745D0
            dy = -1.98535D0 + xx*2.11197D0 - yy*0.741279D0
            dz = -2.72303D0 + yy*2.04606D0
            r = sqrt(dx*dx+dy*dy+dz*dz)
            c = 100.D0*shade(2.1D0, 1.3D0, 1.7D0, dx/r, dy/r, dz/r, 0)
            c = max(c, 0.0D0)
            c = min(c, 255.1D0)
            tile(ix, iy) = int(c, 1_is)
          END DO
        END DO
      END SUBROUTINE
    END MODULE
    PROGRAM raytracef
      USE mod_raytracef
      IMPLICIT NONE
      INTEGER, PARAMETER :: size = 4000, tilesize = 200
      INTEGER :: xtiles, ytiles, xc, yc
      INTEGER (KIND=is) :: picture(size, size)

      xtiles = size/tilesize
      ytiles = size/tilesize
      DO yc = 0, ytiles - 1
        DO xc = 0, xtiles - 1
          CALL calc_tile(size, xc*tilesize, yc*tilesize, tilesize, &
            picture(xc*tilesize+1,yc*tilesize+1))
        END DO
      END DO
      WRITE (*, *) 'Ready for I/O ...'
      OPEN (20, FILE='result.pnm', ACCESS='STREAM', FORM='UNFORMATTED', &
        STATUS='REPLACE')
      WRITE (20) 'P5' // new_line('') // int2str(size) // ' ' // &
        int2str(size) // new_line('') // int2str(255) // new_line('')
      WRITE (20) picture
      CLOSE (20)
      WRITE (*, *) '... done.'

    CONTAINS

      FUNCTION int2str(number) RESULT (string)
        INTEGER :: number
        CHARACTER (16) :: dum
        CHARACTER (:), ALLOCATABLE :: string

        WRITE (dum, *) number
        string = trim(adjustl(dum))
      END FUNCTION

    END PROGRAM
