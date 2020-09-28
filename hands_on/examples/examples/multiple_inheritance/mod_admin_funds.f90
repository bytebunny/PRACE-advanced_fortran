module mod_admin_funds
  use mod_date
  use mod_funds
  implicit none
  private
  public :: funds, date, lk
!
! emulate multiple inheritance
  type, public, extends(funds) :: admin_funds
     private
     real :: interest = 0.05  ! five percent
     type(date) :: d
!    %d is non-polymorphic since increments should always be by days
   contains
     procedure, private :: inc_day
     procedure, public :: inc_both
     procedure :: write => write_admin_funds
     generic, public :: increment => inc_day, inc_both
  end type admin_funds

  interface admin_funds
     module procedure init_admin_funds
  end interface admin_funds
contains
  type(admin_funds) function init_admin_funds(o_funds, interest, o_date)
    type(funds), intent(in) :: o_funds
    real, intent(in) :: interest
    type(date) :: o_date
    init_admin_funds%funds = o_funds
    init_admin_funds%interest = interest
    init_admin_funds%d = o_date
  end function init_admin_funds
  subroutine write_admin_funds(this)
    class(admin_funds), intent(in) :: this
    call this%funds%write()
    write(*,'(''Interest is: '',F5.2)') this%interest
    call this%d%write('stdout')
  end subroutine write_admin_funds
  
!
! redispatch to date
  subroutine inc_day(this, ninc)
    class(admin_funds), intent(inout) :: this
    integer(lk), intent(in) :: ninc
    call this%d%increment(real(ninc, kind(1.0d0)))
  end subroutine inc_day
!
! account for interest rate on increments when going forward in time
  subroutine inc_both(this, ninc, by)
    class(admin_funds), intent(inout) :: this
    integer(lk), intent(in) :: ninc
    real, intent(in) :: by
    real :: by_with_interest
    call this%d%increment(real(ninc, kind(1.0d0)))
    by_with_interest = by * (1.0 + this%interest) ** (real(ninc)/365.25)
    call this%increment(by_with_interest)
  end subroutine inc_both
end module mod_admin_funds
