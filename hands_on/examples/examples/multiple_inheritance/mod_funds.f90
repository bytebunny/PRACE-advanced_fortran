module mod_funds
  implicit none
  private
  type, public :: funds
     private
     character(len=3) :: currency
     real :: amount
   contains 
! the following statement must be rewritten as a definition for a generic TBP
! due to the type extension to admin_funds
!     procedure :: increment => increment_funds
     procedure, private :: increment_funds
     procedure :: write => write_funds
     generic, public :: increment => increment_funds
  end type funds
  
  interface funds
     module procedure init_funds
  end interface funds

contains
  type(funds) function init_funds(currency, amount)
    character(len=3), intent(in) :: currency
    real, intent(in) :: amount
    init_funds%currency = currency
    init_funds%amount = amount
  end function init_funds
  subroutine write_funds(this)
    class(funds), intent(in) :: this
    select type (this)
    type is (funds)
       write(*,'(''Amount is: '',A3,1X,F10.2)') this 
    class default
       stop 'mod_funds::write_funds: Unknown extension of funds found.'
    end select
  end subroutine write_funds
  subroutine increment_funds(this, by)
    class(funds), intent(inout) :: this
    real, intent(in) :: by
    this%amount = this%amount + by
  end subroutine increment_funds
end module mod_funds
