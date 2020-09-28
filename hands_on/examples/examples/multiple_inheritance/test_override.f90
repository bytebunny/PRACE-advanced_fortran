module mod_my_funds
  use mod_admin_funds
!  use mod_funds
!  use mod_date
  implicit none

  type, extends(admin_funds) :: my_funds
   contains
     procedure :: inc_both => inc_my_funds
  end type my_funds
  interface my_funds
     module procedure init_my_funds
  end interface my_funds
contains
  subroutine inc_my_funds(this, ninc, by)
    class(my_funds), intent(inout) :: this
    integer(lk), intent(in) :: ninc
    real, intent(in) :: by

    write(*,*) 'Invoking override. Now cheating ...'
    call this%admin_funds%increment(ninc, 2.0*by)
  end subroutine inc_my_funds
  type(my_funds) function init_my_funds(o_funds, interest, o_date)
    type(funds), intent(in) :: o_funds
    real, intent(in) :: interest
    type(date) :: o_date
    init_my_funds%admin_funds = admin_funds(o_funds, interest, o_date)
  end function init_my_funds
end module mod_my_funds
program test_my_funds
  use mod_my_funds
  implicit none
  class(admin_funds), allocatable :: of_poly

!
! we start with admin_funds
  allocate(of_poly, source = &
       admin_funds(funds('USD',100.),0.04,date(1,'Jan',2000)))

  call of_poly%increment(12_lk,600.)
  call of_poly%write()
  deallocate(of_poly)

!
! then extend to my_funds
  allocate(of_poly, source = &
       my_funds(funds('USD',100.),0.04,date(1,'Jan',2000)))

  call of_poly%increment(12_lk,600.)
  call of_poly%write()
!  select type(of_poly)
!  type is (my_funds)
!    can only extract parent from extended type
!    (why is it necessary to do this at all?)
!     call of_poly%admin_funds%write()
!  end select

end program test_my_funds
