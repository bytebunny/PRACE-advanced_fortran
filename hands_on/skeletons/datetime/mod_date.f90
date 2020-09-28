MODULE mod_date
  !                               Calendar adopted 15 Oct 1582
  !
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER, PUBLIC :: ik = selected_int_kind(6), nmon = 12
  INTEGER, PARAMETER, PUBLIC :: lk = selected_int_kind(10)
  INTEGER, PARAMETER, PUBLIC :: rk = kind(1.0D0)
  INTEGER (lk), PARAMETER :: igreg_1 = 15 + 31*(10+12*1582), &
       igreg_2 = 2299161
  CHARACTER (LEN=3), PARAMETER :: all_months(nmon) = [ 'Jan', 'Feb', &
       'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', &
       'Dec' ]
  INTEGER, PARAMETER :: days_per_month(nmon) = [ 31, 28, 31, 30, 31, 30, &
       31, 31, 30, 31, 30, 31 ]


  TYPE, PUBLIC :: date
     PRIVATE
     INTEGER (ik) :: year = 0, mon = -1, day = -1
     ! invalid value for month or day indicates that object is uninitialized
  END TYPE date
  TYPE, EXTENDS (date), PUBLIC :: datetime
     PRIVATE
     INTEGER (ik) :: hour = -1, min = -1, sec = -1
  END TYPE datetime
  ! FIXME: Add type-bound procedures above, and generic interfaces for 
  !        overloaded structure constructor below
CONTAINS
  ! FIXME: Add procedures inc_date and inc_datetime

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
