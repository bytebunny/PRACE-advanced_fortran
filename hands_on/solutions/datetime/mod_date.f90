MODULE mod_date
  !                               Calendar adopted 15 Oct 1582
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: date, datetime

  INTEGER, PARAMETER, PUBLIC :: ik = selected_int_kind(6), nmon = 12
  INTEGER, PARAMETER, PUBLIC :: lk = selected_int_kind(10)
  INTEGER, PARAMETER, PUBLIC :: rk = kind(1.0D0)
  INTEGER (lk), PARAMETER :: igreg_1 = 15 + 31*(10+12*1582), igreg_2 = 2299161
  CHARACTER (LEN=3), PARAMETER :: all_months(nmon) = [ 'Jan', 'Feb', &
       'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', &
       'Dec' ]
  INTEGER, PARAMETER :: days_per_month(nmon) = [ 31, 28, 31, 30, 31, 30, &
       31, 31, 30, 31, 30, 31 ]


  TYPE :: date
     PRIVATE
     INTEGER (ik) :: year = 0, mon = -1, day = -1
     ! invalid value for month or day indicates that object is uninitialized
   CONTAINS
     PROCEDURE, NON_OVERRIDABLE :: write => write_date
     PROCEDURE :: increment => inc_date
  END TYPE date
  TYPE, EXTENDS (date) :: datetime
     PRIVATE
     INTEGER (ik) :: hour = -1, min = -1, sec = -1
   CONTAINS
     PROCEDURE :: increment => inc_datetime
  END TYPE datetime

  INTERFACE date
     MODULE PROCEDURE create_date
     MODULE PROCEDURE create_date_safe
  END INTERFACE date
  INTERFACE datetime
     MODULE PROCEDURE create_datetime
  END INTERFACE datetime
CONTAINS
  TYPE (date) FUNCTION create_date(year, mon, day)
    INTEGER (KIND=ik), INTENT (IN) :: year, mon, day

    create_date%year = year
    create_date%mon = mon
    create_date%day = day
  END FUNCTION create_date


  TYPE (date) FUNCTION create_date_safe(day, mon, year)
    INTEGER, INTENT (IN) :: day
    CHARACTER (LEN=3), INTENT (IN) :: mon
    INTEGER (KIND=ik), INTENT (IN) :: year

    LOGICAL :: leap_year
    INTEGER :: i, maxd

    leap_year = .FALSE.
    IF (mod(year,4)==0 .AND. mod(year,400)/=0) THEN
       leap_year = .TRUE.
    END IF

    DO i = 1, nmon
       IF (mon==all_months(i)) EXIT
    END DO

    IF (i>nmon) RETURN
    maxd = days_per_month(i)
    IF (leap_year .AND. i==2) maxd = 29
    IF (day<=0 .OR. day>maxd) RETURN
    !
    !   overloaded constructor cannot invoke itself, and the default constructor
    !   is unavailable. Therefore, component-wise assignment is required.
    create_date_safe%year = year
    create_date_safe%mon = i
    create_date_safe%day = day
  END FUNCTION create_date_safe


  TYPE (datetime) FUNCTION create_datetime(day, mon, year, hour, min, sec)
    INTEGER, INTENT (IN) :: day, hour, min, sec
    CHARACTER (LEN=3), INTENT (IN) :: mon
    INTEGER, INTENT (IN) :: year
    !
    !   fall back to overloaded constructor for parent component
    create_datetime%date = date(day, mon, year)
    !   ... and sanity check for the others    
    IF (hour>=0 .AND. hour<24) create_datetime%hour = hour
    IF (min>=0 .AND. min<60) create_datetime%min = min
    IF (sec>=0 .AND. sec<60) create_datetime%sec = sec
  END FUNCTION create_datetime

  SUBROUTINE inc_date(this, days)
    CLASS (date), INTENT (INOUT) :: this
    REAL (rk), INTENT (IN) :: days
    INTEGER (lk) :: totdays, ninc

    ninc = int(days, lk)
    IF (this%day<0 .OR. this%mon<0) RETURN

    totdays = julday(this%mon, this%day, this%year)
    totdays = totdays + ninc
    CALL caldat(totdays, this%mon, this%day, this%year)
  END SUBROUTINE inc_date

  SUBROUTINE inc_datetime(this, days)
    CLASS (datetime), INTENT (INOUT) :: this
    REAL (rk), INTENT (IN) :: days
    INTEGER (lk) :: nsec_r, nmin_r, nhr_r, ninc
    REAL (rk) :: wholedays

    IF (this%hour<0 .OR. this%min<0 .OR. this%sec<0) RETURN
    wholedays = real(int(days,lk), rk)
    IF (days<0) wholedays = wholedays - 1.0_rk
    CALL inc_date(this%date, wholedays)
    ninc = int((days-wholedays)*24*3600, lk)

    nsec_r = 3600_lk*this%hour + 60_lk*this%min + this%sec + ninc
    nmin_r = nsec_r/60_lk
    nsec_r = nsec_r - nmin_r*60_lk
    nhr_r = nmin_r/60_lk
    nmin_r = nmin_r - nhr_r*60_lk

    wholedays = real(nhr_r, rk)/24._rk
    nhr_r = nhr_r - int(wholedays, lk)*24_lk
    CALL inc_date(this%date, wholedays)


    this%hour = nhr_r
    this%min = nmin_r
    this%sec = nsec_r
  END SUBROUTINE inc_datetime

  SUBROUTINE write_date(this, fname)
    USE, INTRINSIC :: iso_fortran_env
    CLASS (date), INTENT (IN) :: this
    CHARACTER (LEN=*), INTENT (IN) :: fname
    INTEGER (ik) :: unit

    IF (trim(fname)=='stdout') THEN
       unit = output_unit
    ELSE
       OPEN (FILE=fname, NEWUNIT=unit, FORM='FORMATTED', ACTION='WRITE')
    END IF

    SELECT TYPE (this)
    TYPE IS (date)
       IF (this%mon<0 .OR. this%day<0) THEN
          WRITE (unit, '(''Date is invalid'')')
       ELSE
          WRITE (unit, '(''Date: '',a3,'' '',i2,'', '',i0)') &
               all_months(this%mon), this%day, this%year
       END IF
    TYPE IS (datetime)
       IF (this%mon<0 .OR. this%day<0 .OR. this%hour<0 .OR. this%min<0 .OR. &
            this%sec<0) THEN
          WRITE (unit, '(''Date or Time is invalid'')')
       ELSE
          WRITE (unit, '(''Date: '',a3,'' '',i2,'', '',i0,  '' &
               &Time: '',2(i2,'':''),i2)') all_months(this%mon), this%day, &
               this%year, this%hour, this%min, this%sec
       END IF
    CLASS DEFAULT
       WRITE (unit, FMT='(''mod_date::write_date: Unknown extension &
            &of date. Continuing ...'')')
    END SELECT

    IF (unit/=output_unit) CLOSE (unit)

  END SUBROUTINE write_date

  FUNCTION julday(mon, day, yr) RESULT (jd)
    !   taken from Numerical recipes. No guarantees.
    INTEGER (ik), INTENT (IN) :: yr, mon, day
    INTEGER (lk) :: jd
    INTEGER (ik) :: ja, jm, jy
    INTEGER (lk) :: it

    jy = yr
    IF (jy<0) jy = jy + 1
    IF (mon>2) THEN
       jm = mon + 1
    ELSE
       jy = jy - 1
       jm = mon + 13
    END IF
    jd = 365_lk*int(jy, lk) + int(0.25D0*jy+2000.D0, lk) + &
         int(30.6001D0*jm, lk) + int(day, lk) + 1718995_lk
    it = 31_lk*(int(mon,lk)+12_lk*int(yr,lk))
    IF (it>=igreg_1) THEN
       ja = int(0.01D0*jy, lk)
       jd = jd + 2_lk - int(ja, lk) + int(0.25D0*ja, lk)
    END IF
  END FUNCTION julday
  SUBROUTINE caldat(julday, mon, day, yr)
    !   taken from Numerical recipes. No guarantees.
    INTEGER (lk), INTENT (IN) :: julday
    INTEGER (ik), INTENT (OUT) :: mon, day, yr
    INTEGER (lk) :: ja, jalpha, jb, jc, jd, je

    IF (julday>=igreg_2) THEN
       jalpha = int(((julday-1867216_lk)-0.25D0)/36524.25D0, lk)
       ja = julday + 1_lk + jalpha - int(0.25D0*jalpha, lk)
    ELSE
       ja = julday
    END IF
    jb = ja + 1524_lk
    jc = int(6680.0D0+((jb-2439870_lk)-122.1D0)/365.25D0, lk)
    jd = 365_lk*jc + int(0.25D0*jc, lk)
    je = int((jb-jd)/30.6001D0, lk)
    day = jb - jd - int(30.6001D0*je, lk)
    mon = je - 1
    IF (mon>12) mon = mon - 12
    yr = jc - 4715
    IF (mon>2) yr = yr - 1
    IF (yr<=0) yr = yr - 1
  END SUBROUTINE caldat
END MODULE mod_date
