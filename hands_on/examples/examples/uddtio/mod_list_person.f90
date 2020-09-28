    MODULE mod_list_person
!
! example for user-defined derived type I/O on a linked list
! of persons
      TYPE :: list_person
         CHARACTER (LEN=:), ALLOCATABLE :: name
         INTEGER :: age
         TYPE (list_person), POINTER :: next => null()
      END TYPE list_person
      INTERFACE WRITE (FORMATTED)
         PROCEDURE write_fmt_person_list 
      END INTERFACE WRITE (FORMATTED)
    CONTAINS
      SUBROUTINE add_person(this, name, age)
        TYPE (list_person), INTENT (INOUT), TARGET :: this
        CHARACTER (LEN=*), INTENT (IN) :: name
        INTEGER, INTENT (IN) :: age
        TYPE (list_person), POINTER :: new

        new => this
        DO
           IF (.NOT. associated(new%next)) EXIT
           new => new%next
        END DO
        new%name = name; new%age = age
        ALLOCATE (new%next)
      END SUBROUTINE
      SUBROUTINE delete_list(this)
        TYPE (list_person), INTENT (INOUT), TARGET :: this
        TYPE (list_person), POINTER :: p, next
        INTEGER :: istat
        p => this%next
        DEALLOCATE (this%name)
        DO
           IF (associated(p%next)) THEN
              next => p%next
              DEALLOCATE(p)
              p => next
           ELSE
              EXIT
           END IF
        END DO
      END SUBROUTINE
      RECURSIVE SUBROUTINE write_fmt_person_list(this, unit, iotype, vlist, iostat, iomsg)
        CLASS (list_person), INTENT (IN) :: this
        INTEGER,             INTENT (IN) :: unit
        INTEGER,             INTENT (IN) :: vlist(:)
        CHARACTER (LEN=*),   INTENT (IN) :: iotype
        INTEGER,             INTENT (OUT) :: iostat
        CHARACTER (LEN=*),   INTENT (INOUT) :: iomsg
! .. locals
        CHARACTER (LEN=128) :: pfmt

!    write(*,*) 'iotype, vlist: ',trim(iotype),vlist
        IF (trim(iotype)/='DTList') THEN
          iostat = 101
          iomsg = iotype // ': Unexpected value for iotype.'
          RETURN
        END IF
        IF (size(vlist)<2) THEN
          iostat = 1022
          iomsg = 'Vlist array too small'
          RETURN
        END IF
! perform internal IO to generate format descriptor
        WRITE (pfmt, '(a,i0,a,i0,a)') '(i', vlist(1), ',a', vlist(2), ',a)'
        iostat = 0
        IF (allocated(this%name)) THEN
          WRITE (unit, FMT=pfmt, IOSTAT=iostat, IOMSG=iomsg) this%age, this%name, new_line('a')
        END IF
        IF (iostat/=0) RETURN
        IF (associated(this%next)) THEN
! recursive call
          CALL write_fmt_person_list(this%next, unit, iotype, vlist, iostat, iomsg)
        END IF
      END SUBROUTINE write_fmt_person_list
    END MODULE mod_list_person

   
