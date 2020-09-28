program test_admin_funds
  use mod_admin_funds
  implicit none
  type(admin_funds) :: of
  class(funds), allocatable :: of_poly

  of = admin_funds(funds('USD',123400.),0.34,date(12,'Jan',2012))
  allocate(of_poly, source = &
       admin_funds(funds('USD',100.),0.04,date(1,'Jan',2000)))

  call of%increment(12_lk,600.)
  call of%write()

  call of%increment(17_lk)
  call of%write()

  call of%increment(100.)
  call of%write()

  select type (of_poly)
  class is (admin_funds)
    call of_poly%increment(12_lk,600.)
  end select
! the next statement would not compile if uncommented.
! call of_poly%increment(12_lk,600.)
  call of_poly%write()

end program test_admin_funds
