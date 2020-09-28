    PROGRAM prog_client
      USE mod_handle, ONLY: handle, create_handle
      IMPLICIT NONE
      CLASS (handle), ALLOCATABLE :: h
      REAL, ALLOCATABLE :: data(:)

      data = [ 2.3, 4.4, 9.7 ]

      CALL create_handle(h, 'file_handle')

      WRITE (*, *) 'Handle h has state ', h%getstate()
      CALL h%open('client_file.dat')
      WRITE (*, *) 'Handle h has state ', h%getstate()

      CALL h%send(data)

      CALL h%close()
      WRITE (*, *) 'Handle h has state ', h%getstate()

      DEALLOCATE (h, data)
    END PROGRAM
