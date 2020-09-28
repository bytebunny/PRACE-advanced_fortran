module timer
! Timer implementation based on system_clock intrinsic
! Note that this facility is not thread-safe
  implicit none
  private
  public :: dwalltime
  integer, parameter :: ik = selected_int_kind(6)
  logical, save :: first = .true.
  integer(ik), save ::  count_rate, count_max
  double precision, save :: conversion = 0.0d0
contains
  double precision function dwalltime()
    integer(ik) :: count
    if (first) then
       first = .false.
       call system_clock(count, count_rate, count_max)
       conversion = 1.0d0 / dble(count_rate)
    else
       call system_clock(count)
    end if
    dwalltime = count * conversion
  end function dwalltime
end module timer
