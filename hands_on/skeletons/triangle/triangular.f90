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
      INTEGER :: n
      INTEGER :: i, ios, j, i_row
      INTEGER, PARAMETER :: strmx = 32, errmx = 1024
      CHARACTER (LEN=strmx) :: sw, sw_val, fmtstring
      CHARACTER (LEN=errmx) :: err
!
      n = 12
      i_row = 5
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
      WRITE (*, '(i0,'' rows, print row '',i0,'':'')') n, i_row

!
! create triangular matrix :
      ALLOCATE (a(n))
      DO i = 1, n
        ALLOCATE (a(i)%row(n-i+1))
      END DO
!
! initialize matrix A(i, j) = i + j : 
      DO i = 1, n
        DO j = 1, n - i + 1
          a(i)%row(j) = real(i) + real(j)
        END DO
      END DO
!
! write row number i_row to standard output
      IF (i_row<=n .AND. i_row>0) THEN
        DO j = 1, n - i_row + 1
          WRITE (*, FMT='(F0.1)') a(i_row)%row(j)
        END DO
      END IF
!
! clean up
      DEALLOCATE (a) ! implies deallocation of components
    END PROGRAM

