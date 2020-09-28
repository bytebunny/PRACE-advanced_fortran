    PROGRAM write_list
      USE mod_list_person
      IMPLICIT NONE
      INTEGER :: unit, stat
      CHARACTER(len=128) :: msg
      TYPE (list_person) :: mylist
      

! set up list
      CALL add_person(mylist, 'Heinz', 34)
      CALL add_person(mylist, 'Peter', 43)
      CALL add_person(mylist, 'Gustav', 51)
      CALL add_person(mylist, 'Hermann', 71)
      CALL add_person(mylist, 'Robert', 38)
      CALL add_person(mylist, 'Gretel', 27)


      OPEN (NEWUNIT=unit, FILE='list.dat', FORM='FORMATTED', ACTION='WRITE')
      WRITE (unit, FMT='(DT "List" (4,20) )') mylist
      CLOSE (unit)


      WRITE (*, FMT=*, IOSTAT=stat, IOMSG=msg) mylist
      WRITE (*, *) 'After list directed attempt - iostat: ',stat, ' Message: ', msg 

      CALL delete_list(mylist)
    END PROGRAM
