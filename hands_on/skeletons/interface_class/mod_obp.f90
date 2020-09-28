module mod_obp
  use mod_handle
  implicit none
  private
  public :: data, data_send_container, send
  type :: data
     real, allocatable :: x(:)
  end type data

  type :: data_send_container
     class(data), allocatable :: d
     procedure(send), pointer :: send => null()
  end type data_send_container

contains
  subroutine send(this, desc)
    class(data_send_container) :: this
    class(handle) :: desc
!
! very simple version: 
! only base type component is transferred
! through an existing interface capable of dealing with 
! real arrays
    call desc%send(this%d%x)
  end subroutine send
end module mod_obp
