PROGRAM test_date
  USE mod_date
  IMPLICIT NONE
  TYPE (date) :: d1
  TYPE (datetime) :: d2
  CLASS (date), ALLOCATABLE :: d3
  CLASS (datetime), ALLOCATABLE :: d4

  !  simple version
  d1 = date(2007, 12, 20)
  !  safe version
  !  d1 = date(year = 2007, mon = 'Dec', day = 20)
  d2 = datetime(20, 'Dec', 2007, hour=20, min=12, sec=47)

  ! FIXME: remove comments below after implementing the type-bound procedures
  !  write(6, advance='no', fmt='(''Object d1: '')')
  !  call d1%write('stdout')
  !  write(6, advance='no', fmt='(''Object d2: '')')
  !  call d2%write('stdout')

  !  write(6, advance='no', fmt='(''Object d1+4days: '')')
  !  call d1%increment(4._rk)
  !  call d1%write('stdout')

  !  write(6, advance='no', fmt='(''Object d2+345600s: '')')
  !  call d2%increment(345600._rk/(24._rk*3600._rk))
  !  call d2%write('stdout')

  !  d1 = date(2007, 12, 20)
  !  d2 = datetime(20, 'Dec', 2007, hour=20, min = 12, sec = 47)
  !  call  d1%increment(-1.0_rk)
  !  call  d2%increment(-1.0_rk)
  !  write(6, advance='no', fmt='(''Object d1-1day: '')')
  !  call d1%write('stdout')
  !  write(6, advance='no', fmt='(''Object d2-1day: '')')
  !  call d2%write('stdout')

  !  d1 = date(2007, 12, 20)
  !  d2 = datetime(20, 'Dec', 2007, hour=20, min = 12, sec = 47)
  !  call  d1%increment(-20_rk/24._rk)
  !  call  d2%increment(-20_rk/24._rk)
  !  write(6, advance='no', fmt='(''Object d1-20hours: '')')
  !  call d1%write('stdout')
  !  write(6, advance='no', fmt='(''Object d2-20hours: '')')
  !  call d2%write('stdout')

  !  allocate(d3, source = d1)
  !  write(6, advance='no', fmt='(''Object d3 (cloned d1)+4 days: '')')
  !  call d3%increment(4._rk)
  !  call d3%write('stdout')
  !  deallocate(d3)

  !  allocate(d3, source = d2)
  !  write(6, advance='no', fmt='(''Object d3 (cloned d2)+345600s: '')')
  !  call d3%increment(345600._rk/(24._rk*3600._rk))
  !  call d3%write('stdout')
  !  deallocate(d3)

  !  allocate(d4, source = datetime(10,'Oct',1972,1,2,3))
  !  write(6, advance='no', fmt='(''Object d4 (polym. alloc.) + 3 days: '')')
  !  call d4%date%increment(3._rk)
  !  call d4%write('stdout')
  !  deallocate(d4)


  !  d1 = date(year = 2007, mon = 'Hif', day = 20)
  !  write(6, advance='no', fmt='(''Invalid Object d1: '')')
  !  call d1%write('stdout')
  !  write(6, advance='no', fmt='(''Increment d1: '')')
  !  call d1%increment(4._rk)
  !  call d1%write('stdout')

END PROGRAM test_date
