    PROGRAM triangular
      IMPLICIT NONE
!
! A(i,j) is addressed as a(i)%row(j) 
!
      TYPE :: tri_matrix
        REAL, ALLOCATABLE :: row(:)
      END TYPE
      TYPE (tri_matrix), ALLOCATABLE :: a(:)
!
      INTEGER :: n, i_row
      INTEGER :: nproc, me, rows_per_proc
      INTEGER :: i, ios, j, i_local, n_elem
      INTEGER, PARAMETER :: strmx = 32, errmx = 1024
      CHARACTER (LEN=strmx) :: sw, sw_val, fmtstring
      CHARACTER (LEN=errmx) :: err
!
      n = 12
      i_row = 5
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
          CASE ('-r')
            CALL get_command_argument(i+1, value=sw_val)
            WRITE (fmtstring, FMT='(a,i0,a)') '(i', len_trim(sw_val), ')'
            READ (sw_val, IOSTAT=ios, IOMSG=err, FMT=fmtstring) i_row
          END SELECT
          i = i + 2
          IF (ios/=0) THEN
            WRITE (*, *) 'failed with message: ', err
            STOP 'Aborting program.'
          END IF
        END DO
      END IF
      IF (me==1) WRITE (*, '(i0,'' rows, print row '',i0,'':'')') n, i_row

      nproc = num_images()

      rows_per_proc = n/nproc
      IF (mod(n,nproc)>0) rows_per_proc = rows_per_proc + 1
!
! create triangular matrix:
      ALLOCATE (a(rows_per_proc))
      i_local = 1
      n_elem = 0
      DO i = me, n, nproc
        ALLOCATE (a(i_local)%row(n-i+1))
        n_elem = n_elem + n - i + 1
        i_local = i_local + 1
      END DO
!
! initialize matrix A(i, j) = i + j  
      i_local = 1
      DO i = me, n, nproc
        DO j = 1, n - i + 1
          a(i_local)%row(j) = real(i) + real(j)
        END DO
        i_local = i_local + 1
      END DO
!
! write row number i_row to standard output
      IF (mod(i_row-1,nproc)+1==me .AND. i_row<=n) THEN
        i_local = (i_row-1)/nproc + 1
        WRITE (*, FMT='(('' Row '',i0,'' on image '',i0,'':''),1000(1X,F0.1))' &
          ) i_row, me, (a(i_local)%row(j), j=1, n-i_row+1)
      END IF

      WRITE (*, FMT='(''Number of elements on image '',i0,'': '',i0)') me, &
        n_elem
!  
! clean up
      DEALLOCATE (a) ! implies deallocation of components
    END PROGRAM
