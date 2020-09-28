    MODULE mod_triangular
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: tri_mvm
      TYPE, PUBLIC :: tri_matrix
        REAL, ALLOCATABLE :: row(:)
      END TYPE
    CONTAINS
      SUBROUTINE tri_mvm(mat, vec, n_local, res)
        TYPE (tri_matrix), INTENT (IN) :: mat(:)
        REAL, INTENT (INOUT) :: vec(:)
        INTEGER, INTENT (IN) :: n_local[ * ]
        REAL, INTENT (INOUT) :: res(:)[ * ]
        INTEGER :: i, i_local, me, n, nproc
        INTEGER, ALLOCATABLE :: idx(:)

        me = this_image()
        nproc = num_images()
        n = size(vec, 1)
!
! calculate scalar products
        i_local = 1
        DO i = me, n, nproc
          res(i_local) = dot_product(mat(i_local)%row, vec(1:n-i+1))
          i_local = i_local + 1
        END DO
!
! communicate result back to v on all images
! (probably not the most efficient way)
        SYNC ALL
        DO me = 1, nproc
          ALLOCATE (idx, SOURCE=[(me+(i_local-1)*nproc,i_local=1,n_local[me] &
            )] )
          vec(1:n_local[me]) = res(1:size(idx))[ me ]
          ! extra copy because gfortran + OC has a bug 
          vec(idx) = vec(1:n_local[me])
          DEALLOCATE (idx)
        END DO
      END SUBROUTINE
    END MODULE
    PROGRAM triangular_library
      USE mod_triangular
      IMPLICIT NONE
      TYPE (tri_matrix), ALLOCATABLE :: a(:)
      REAL, ALLOCATABLE :: v(:), b(:)[:]

      INTEGER :: n
      INTEGER :: nproc, me, rows_per_proc
      INTEGER :: i, ios, j, i_local, n_local[ * ]
      INTEGER, PARAMETER :: strmx = 32, errmx = 1024
      CHARACTER (LEN=strmx) :: sw, sw_val, fmtstring
      CHARACTER (LEN=errmx) :: err
!
      n = 12
      me = this_image()
      IF (command_argument_count()>0) THEN
        i = 1
        DO
          IF (i>command_argument_count()) EXIT
          CALL get_command_argument(i, value=sw)
          SELECT CASE (trim(sw))
          CASE ('-n')
            CALL get_command_argument(i+1, value=sw_val)
            WRITE (fmtstring, FMT='(a,i0,a)') '(i', len_trim(sw_val), ')'
            READ (sw_val, IOSTAT=ios, IOMSG=err, FMT=fmtstring) n
          END SELECT
          i = i + 2
          IF (ios/=0) THEN
            WRITE (*, *) 'failed with message: ', err
            STOP 'Aborting program.'
          END IF
        END DO
      END IF
      IF (me==1) WRITE (*, '(i0,'' rows.'')') n

      nproc = num_images()

      rows_per_proc = n/nproc
      IF (mod(n,nproc)>0) rows_per_proc = rows_per_proc + 1
!
! create triangular matrix and needed vectors
      ALLOCATE (a(rows_per_proc), v(n))
      ALLOCATE (b(rows_per_proc)[*] )
      i_local = 1
      DO i = me, n, nproc
        ALLOCATE (a(i_local)%row(n-i+1))
        i_local = i_local + 1
      END DO
      n_local = i_local - 1
!
! initialize matrix A(i, j) = i + j  
! initialize vector v
      i_local = 1
      DO i = me, n, nproc
        DO j = 1, n - i + 1
          a(i_local)%row(j) = real(i) + real(j)
        END DO
        i_local = i_local + 1
      END DO
      v = 1.0
!
      CALL tri_mvm(a, v, n_local, b)
!
! write result
      IF (this_image()==1) THEN
        WRITE (*, '('' Result:'')')
        WRITE (*, '(*(e12.5,1x))') v
      END IF
!  
! clean up
      DEALLOCATE (a) ! implies deallocation of components
      DEALLOCATE (v)
      DEALLOCATE (b)
    END PROGRAM
