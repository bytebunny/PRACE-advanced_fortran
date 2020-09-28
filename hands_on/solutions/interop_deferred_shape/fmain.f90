program fmain
  use mod_deferred
  use, intrinsic :: iso_c_binding, only : c_float
  implicit none
  real(c_float), allocatable :: arr(:)
  integer :: i

  call generate_data(arr)

  do i=1, size(arr)
     write(*, fmt='('' Element '',i0,'' has value '',F10.3)') i, arr(i)
  end do

end program fmain
