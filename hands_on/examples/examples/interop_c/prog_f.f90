program prog_f
  use mod_dyn
  implicit none
  type(c_ptr) :: handle
  type(cdyn), pointer :: this
  integer, parameter :: len = 5
  real(c_float) :: a(5) = [ -1., 1., 3., 5., 7. ]

  handle = cdyn_create(len)
  call cdyn_add(handle, a)
  call c_f_pointer(handle, this)
  call cdyn_print(this)
  call cdyn_destroy(handle)
  
end program prog_f
