module mod_union
  implicit none
  integer, parameter, private :: funit = 20
!
! implements a discriminated union based on CLASS(*)
! here, the union can contain either a scalar of some intrinsic type
! or a scalar of a type out of a finite set of sequence types, the first
! integer component of which serves as tag.
! 
  type, private :: t
     sequence
     integer :: tag
  end type t
  type :: t1
     sequence
     integer :: tag = -1
     real :: f(3)
  end type t1
  type :: t2
     sequence
     integer :: tag = -2
     complex :: f(3)
  end type t2
contains
  subroutine construct_discriminated_union(this, fname)
    class(*), allocatable, intent(out) :: this
    character(len=*) :: fname
    integer :: tag
!   objects required for reading input
    integer :: this_i
    real    :: this_r
    type(t1) :: this_t1
    type(t2) :: this_t2
!
    open(funit, file=fname, form='FORMATTED', action='READ')
    read(funit, fmt=*) tag
    select case (tag)
    case(0)
       read(funit, fmt=*) this_i
       allocate(this, source=this_i)
    case(1)
       read(funit, fmt=*) this_r
       allocate(this, source=this_r)
    case(-1)
       read(funit, fmt=*) this_t1%f
       allocate(this, source=this_t1)
    case(-2)
       read(funit, fmt=*) this_t2%f
       allocate(this, source=this_t2)
    end select
    close(funit)
  end subroutine construct_discriminated_union
  subroutine dispatch_union(this)
    use, intrinsic :: iso_c_binding
    class(*), target :: this
    type(t), pointer :: o
    type(t), target :: ot
    type(t1), pointer :: p1
    type(t2), pointer :: p2

    o => null()
    select type (this)
    type is (integer)
       write(*,fmt='(''   have an integer with value '',i0)') this
    type is (real)
       write(*,fmt='(''   have a real with value '',f6.3)') this
    class default
       write(*,fmt='(''   have a derived type ... hang on'')')
       ot = transfer(this, mold=ot) 
! TRANSFER must be used because the assignment o=>this is non-conforming
! due to the dynamic type of "this" not being "t"
       o => ot
    end select

    if (.not. associated(o)) return
    select case (o%tag)
    case (-1)
       p1 => this
       write(*,fmt='(''   have a t1 with values '',3(f6.3,:,1x))') p1%f
    case (-2)
       p2 => this
       write(*,fmt='(''   have a t2 with values '',6(f6.3,:,1x))') p2%f
    case default
       write(*,fmt='(''   tag has invalid value '',i0)') o%tag
    end select
  end subroutine dispatch_union
end module mod_union
