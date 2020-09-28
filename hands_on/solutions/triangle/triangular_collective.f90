    PROGRAM triangular
      IMPLICIT NONE
! a is the data structure used in this program; a_serial was used
! in the serial program
! a_serial(i)  =  a(i/nproc) on image mod(i,nproc) 
!                                             i = 1..n
! a_serial(me+(i_local-1)*nproc) = a(i_local) on image me  
!                                             i_local = 1..rows_per_proc
!                                             me = 1..nproc 
! matrix-vector multiplication: scalar products are formed by 
! multiplying a(i)%row with the column vector
!
      TYPE :: tri_matrix
        REAL, ALLOCATABLE :: row(:)
      END TYPE
      TYPE (tri_matrix), ALLOCATABLE :: a(:)
      REAL, ALLOCATABLE :: v(:), b(:)[:], v_aux(:)
      INTEGER, ALLOCATABLE :: idx(:)

      INTEGER :: n
      INTEGER :: nproc, me, rows_per_proc
      INTEGER :: i, ios, j, i_local, n_elem, n_local[ * ]
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
      ALLOCATE (a(rows_per_proc), v_aux(rows_per_proc), idx(rows_per_proc), v(n))
      ALLOCATE (b(rows_per_proc)[*] )
      i_local = 1
      n_elem = 0
      DO i = me, n, nproc
        ALLOCATE (a(i_local)%row(n-i+1))
        n_elem = n_elem + n - i + 1
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
! calculate scalar products
      i_local = 1
      DO i = me, n, nproc
        b(i_local) = dot_product(a(i_local)%row, v(1:n-i+1))
        i_local = i_local + 1
      END DO
!
! communicate result back to v on all images
! using co_broadcast
      SYNC ALL

      DO me = 1, nproc
        n_elem = n_local[me]
        IF (this_image() == me) v_aux(1:n_elem) = b(1:n_elem)
        idx(1:n_elem) = [(me+(i_local-1)*nproc,i_local=1,n_elem)]
        CALL co_broadcast(v_aux(1:n_elem), SOURCE_IMAGE=me)
        v(idx(1:n_elem)) = v_aux(1:n_elem)  
      END DO
!
! write result
      IF (this_image()==1) THEN
        WRITE (*, '('' Result:'')')
        WRITE (*, '(*(e12.5,1x))') v
      END IF
!  
! clean up
      DEALLOCATE (a) ! implies deallocation of components
      DEALLOCATE (v, idx, v_aux)
      DEALLOCATE (b)
    END PROGRAM
