module mod_dyn
  use, intrinsic :: iso_c_binding
  implicit none
  type :: fdyn
     real(c_float), allocatable :: f(:)
  end type fdyn
  type, bind(c) :: cdyn
     integer(c_int) :: len
     type(c_ptr) :: f
  end type cdyn
  !
  ! prototypes for C function calls to be used from Fortran
  interface
     type(c_ptr) function cdyn_create(len) bind(c, name='Cdyn_create')
       import :: c_ptr, c_int
       integer(c_int), value :: len
     end function cdyn_create
     subroutine cdyn_add(v, a) bind(c, name='Cdyn_add')
       import :: c_ptr, c_float
       type(c_ptr), value :: v
       real(c_float), intent(in) :: a(*)
     end subroutine cdyn_add
     subroutine cdyn_destroy(v) bind(c, name='Cdyn_destroy')
       import :: c_ptr
       type(c_ptr), value :: v
     end subroutine cdyn_destroy
  end interface
contains
  ! implementation of an I/O facility that can also be called from C
  subroutine cdyn_print(this) bind(c, name='Cdyn_print')
    type(cdyn) :: this
    real(c_float), pointer :: f(:)
    !
    ! unpack array stored inside the struct
    call c_f_pointer( this%f, f, [this%len] )
    write(*, fmt='(''Cdyn object of size '',i0)') size(f,1)
    write(*, fmt='(''with elements '',5(f10.3,1x))') f
  end subroutine cdyn_print
  !
  ! construct a handle for a fdyn object
  type(c_ptr) function fdyn_create(len) bind(c, name='Fdyn_create')
    integer(c_int), value :: len
    type(fdyn), pointer :: p
    integer :: i
    allocate( p )  ! p is associated with an anonymous target
    allocate( p%f(len), source=[ (real(i), i=1, len) ] )
    
    fdyn_create = c_loc(p)
  end function fdyn_create
  subroutine fdyn_print(this) bind(c, name='Fdyn_print')
    type(c_ptr), value :: this
    type(fdyn), pointer :: this_p
    !
    ! unpack untyped C pointer to typed POINTER object
    call c_f_pointer( this, this_p )
    !
    if (allocated( this_p%f )) then
       write(*, fmt='(''Fdyn object of size '',i0)') size(this_p%f,1)
       write(*, fmt='(''with elements '',5(f10.3,1x))') this_p%f
    else
       write(*,fmt='(''Fdyn objec is uninitialized.'')') 
    end if
  end subroutine fdyn_print
  subroutine fdyn_destroy(this) bind(c, name='Fdyn_destroy')
    type(c_ptr), value :: this
    type(fdyn), pointer :: p
    call c_f_pointer(this, p)
    if ( allocated( p%f ) ) deallocate( p%f )
    deallocate( p )
  end subroutine fdyn_destroy
end module mod_dyn
