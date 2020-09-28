program test_union
  use mod_union
  implicit none
  integer, parameter :: cases = 4
  character(len=8) :: files(cases) = &
       (/ 'int.dat ', 't2.dat  ', 't1.dat  ', 'real.dat' /)
  class(*), allocatable :: my_union
  integer :: ic

  do ic=1, cases
     write(*,fmt='(''Processing '',a)') trim(files(ic))
     call construct_discriminated_union(my_union,files(ic))
     call dispatch_union(my_union)
  end do
end program test_union
