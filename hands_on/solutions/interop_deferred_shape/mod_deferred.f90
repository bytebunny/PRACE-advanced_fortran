module mod_deferred
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: generate_data

  integer, parameter :: ndim = 10
  integer :: iunit = 20, icount = 1
  
  type :: buffer_container
     real(c_float), allocatable :: arr(:)
  end type buffer_container
!
! buffer management inside Fortran: using fixed size 
! (for more flexibility, use a linked list)
  type(buffer_container), target :: buffers(ndim)

  type, bind(c) :: handle
     type(c_ptr) :: data = c_null_ptr
     integer(c_int) :: size = 0
  end type handle

contains
!
! This is the Fortran interface
!
  subroutine generate_data(arr)
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
!
! The C interface wraps the Fortran interface
! and uses an interoperable handle
! 
 subroutine c_generate(h) bind(c, name='Generate_data')
    type(handle) :: h
    if (icount > ndim) return
    call generate_data(buffers(icount)%arr)
    h%data = c_loc(buffers(icount)%arr)
    h%size = size(buffers(icount)%arr)
    icount = icount + 1
  end subroutine c_generate
! Note: a destructor is missing. Would need to store
! icount inside handle to implement one. 
end module mod_deferred
