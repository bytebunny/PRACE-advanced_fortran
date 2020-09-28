module mod_deferred_ts
  use, intrinsic :: iso_c_binding
  private
  public :: generate_data

  integer :: iunit = 20
contains
!
! This is the Fortran interface
! which assumes that TS29113 is supported.
!
  subroutine generate_data(arr) bind(c)
    real(c_float), allocatable, intent(out) :: arr(:)
    integer :: nsz, n

    open(iunit, file='data.txt', form='FORMATTED', status='OLD')
    read(iunit, fmt=*) nsz
    allocate(arr(nsz))  
    do n=1, nsz
       read(iunit, fmt=*) arr(n)
    end do
    close(iunit)
  end subroutine generate_data
end module mod_deferred_ts
