module mod_date
!                               Calendar adopted 15 Oct 1582
!
  implicit none
  private
  public :: date, datetime

  integer, parameter, public :: ik = selected_int_kind(6), nmon = 12
  integer, parameter, public :: lk = selected_int_kind(10)
  integer, parameter, public :: rk = kind(1.0d0)
  integer(lk), parameter :: igreg_1 = 15 + 31*(10+12*1582), igreg_2 = 2299161
  character(len=3), parameter :: all_months(nmon) = &
       (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
  integer, parameter :: days_per_month(nmon) = &
       (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)


  type :: date
     private
     integer(ik) :: year = 0, mon = -1, day = -1
     ! invalid value for month or day indicates that object is uninitialized
  contains
     procedure, non_overridable :: write => write_date
     procedure :: increment => inc_date
  end type date
  type, extends(date) :: datetime
     private
     integer(ik) :: hour = -1, min = -1, sec = -1
   contains
     procedure :: increment => inc_datetime
  end type datetime

  interface date
     module procedure create_date
     module procedure create_date_safe
  end interface date
  interface datetime
     module procedure create_datetime
  end interface datetime
contains
  type(date) function create_date(year, mon, day)
    integer(kind=ik), intent(in) :: year, mon, day

    create_date%year = year
    create_date%mon = mon
    create_date%day = day
  end function create_date


  type(date) function create_date_safe(day, mon, year)
    integer, intent(in) :: day
    character(len=3), intent(in) :: mon
    integer(kind=ik), intent(in) :: year

    logical :: leap_year
    integer :: i, maxd

    leap_year = .false.
    if (mod(year,4) == 0 .and. mod(year,400) /= 0) then
       leap_year = .true.
    end if
    
    do i = 1, nmon
       if (mon == all_months(i)) exit
    end do
    
    if (i > nmon) return
    maxd = days_per_month(i) 
    if (leap_year .and. i == 2) maxd = 29
    if (day <= 0 .or. day > maxd) return
!
!   overloaded constructor cannot invoke itself, and the default constructor
!   is unavailable. Therefore, component-wise assignment is required.
    create_date_safe%year = year ; create_date_safe%mon = i ; create_date_safe%day = day
  end function create_date_safe


  type(datetime) function create_datetime(day, mon, year, hour, min, sec)
    integer, intent(in) :: day, hour, min, sec
    character(len=3), intent(in) :: mon
    integer, intent(in) :: year
!
!   fall back to overloaded constructor for parent component
    create_datetime%date = date(day, mon, year)
!   ... and sanity check for the others    
    if (hour >= 0 .and. hour < 24) create_datetime%hour = hour
    if (min >= 0 .and. min < 60) create_datetime%min = min
    if (sec >= 0 .and. sec < 60) create_datetime%sec = sec
  end function create_datetime

  subroutine inc_date(this, days)
    class(date), intent(inout) :: this
    real(rk), intent(in) :: days
    integer(lk) :: totdays, ninc

    ninc = int(days, lk)
    if (this%day < 0 .or. this%mon < 0) return

    totdays = julday(this%mon, this%day, this%year)
    totdays = totdays + ninc
    call caldat(totdays,this%mon, this%day, this%year)
  end subroutine inc_date

  subroutine inc_datetime(this, days)
    class(datetime), intent(inout) :: this
    real(rk), intent(in) :: days
    integer(lk) :: nsec_r, nmin_r, nhr_r, ninc
    real(rk) :: wholedays

    if (this%hour < 0 .or. this%min < 0 .or. this%sec < 0) return
    wholedays = real(int(days,lk),rk)
    if (days < 0) wholedays = wholedays - 1.0_rk
    call inc_date(this%date,wholedays)
    ninc = int((days - wholedays) * 24 * 3600, lk)

    nsec_r = 3600_lk * this%hour + 60_lk * this%min + this%sec + ninc
    nmin_r = nsec_r / 60_lk
    nsec_r = nsec_r - nmin_r * 60_lk
    nhr_r = nmin_r / 60_lk
    nmin_r = nmin_r - nhr_r * 60_lk

    wholedays = real(nhr_r, rk) / 24._rk
    nhr_r = nhr_r - int(wholedays, lk) * 24_lk
    call inc_date(this%date,wholedays)


    this%hour = nhr_r
    this%min = nmin_r
    this%sec = nsec_r
  end subroutine inc_datetime

  subroutine write_date(this, fname)
    use, intrinsic :: iso_fortran_env
    class(date), intent(in) :: this
    character(len=*), intent(in) :: fname
    integer(ik) :: unit

    if (trim(fname) == 'stdout') then
       unit = output_unit
    else
       open(file=fname, newunit=unit, form='FORMATTED', action='WRITE')
    end if

    select type (this)
    type is (date)
       if (this%mon < 0 .or. this%day < 0) then
          write(unit, '(''Date is invalid'')')
       else
          write(unit, '(''Date: '',a3,'' '',i2,'', '',i0)') all_months(this%mon), &
               this%day, this%year
       end if
    type is (datetime)
       if (this%mon < 0 .or. this%day < 0 .or. &
            this%hour < 0 .or. this%min < 0 .or. this%sec < 0) then
          write(unit, '(''Date or Time is invalid'')')
       else
          write(unit, '(''Date: '',a3,'' '',i2,'', '',i0, &
               & '' Time: '',2(i2,'':''),i2)') all_months(this%mon), &
               this%day, this%year, this%hour, this%min, this%sec
       end if
    class default
       write(unit, fmt='(''mod_date::write_date: &
            &Unknown extension of date. Continuing ...'')') 
    end select

    if (unit /= output_unit) close(unit)

  end subroutine write_date

  function julday(mon, day, yr) result(jd)
!   taken from Numerical recipes. No guarantees.
    integer(ik), intent(in) :: yr, mon, day 
    integer(lk) :: jd
    integer(ik) :: ja, jm, jy
    integer(lk) :: it
    jy = yr
    if (jy < 0) jy = jy + 1
    if (mon > 2) then
       jm = mon + 1
    else
       jy = jy - 1
       jm = mon + 13
    end if
    jd = 365_lk*int(jy,lk)+int(0.25d0*jy+2000.d0,lk)+int(30.6001d0*jm,lk)+int(day,lk)+1718995_lk
    it = 31_lk*(int(mon,lk) + 12_lk*int(yr,lk))
    if (it >= igreg_1) then
       ja = int(0.01d0*jy,lk)
       jd = jd + 2_lk - int(ja,lk) + int(0.25d0*ja,lk)
    end if
  end function julday
  subroutine caldat(julday, mon, day, yr)
!   taken from Numerical recipes. No guarantees.
    integer(lk), intent(in) :: julday
    integer(ik), intent(out) :: mon, day, yr
    integer(lk) :: ja, jalpha, jb, jc, jd, je
    if (julday >= igreg_2) then
       jalpha = int(((julday - 1867216_lk)-0.25d0)/36524.25d0,lk)
       ja = julday + 1_lk + jalpha - int(0.25d0*jalpha,lk)
    else
       ja = julday
    end if
    jb = ja + 1524_lk
    jc = int(6680.0d0+((jb-2439870_lk)-122.1d0)/365.25d0,lk)
    jd = 365_lk*jc + int(0.25d0*jc,lk)
    je = int((jb-jd)/30.6001d0,lk)
    day = jb - jd - int(30.6001d0*je,lk)
    mon = je - 1
    if (mon > 12) mon = mon - 12
    yr = jc  - 4715
    if (mon > 2) yr = yr - 1
    if (yr <= 0) yr = yr - 1
  end subroutine caldat
end module mod_date
