PROGRAM test_date
  USE mod_date
  IMPLICIT NONE
  TYPE (date) :: d1
  TYPE (datetime) :: d2
  CLASS (date), ALLOCATABLE :: d3
  CLASS (datetime), ALLOCATABLE :: d4

  !  simple version
  !  d1 = date(2007, 12, 20)
  !  safe version
  d1 = date(year=2007, mon='Dec', day=20)
  d2 = datetime(20, 'Dec', 2007, hour=20, min=12, sec=47)
  WRITE (6, ADVANCE='no', FMT='(''Object d1: '')')
  CALL d1%write('stdout')
  WRITE (6, ADVANCE='no', FMT='(''Object d2: '')')
  CALL d2%write('stdout')

  WRITE (6, ADVANCE='no', FMT='(''Object d1+4days: '')')
  CALL d1%increment(4.0_rk)
  CALL d1%write('stdout')

  WRITE (6, ADVANCE='no', FMT='(''Object d2+345600s: '')')
  CALL d2%increment(345600._rk/(24._rk*3600._rk))
  CALL d2%write('stdout')

  d1 = date(2007, 12, 20)
  d2 = datetime(20, 'Dec', 2007, hour=20, min=12, sec=47)
  CALL d1%increment(-1.0_rk)
  CALL d2%increment(-1.0_rk)
  WRITE (6, ADVANCE='no', FMT='(''Object d1-1day: '')')
  CALL d1%write('stdout')
  WRITE (6, ADVANCE='no', FMT='(''Object d2-1day: '')')
  CALL d2%write('stdout')

  d1 = date(2007, 12, 20)
  d2 = datetime(20, 'Dec', 2007, hour=20, min=12, sec=47)
  CALL d1%increment(-20_rk/24._rk)
  CALL d2%increment(-20_rk/24._rk)
  WRITE (6, ADVANCE='no', FMT='(''Object d1-20hours: '')')
  CALL d1%write('stdout')
  WRITE (6, ADVANCE='no', FMT='(''Object d2-20hours: '')')
  CALL d2%write('stdout')

  ALLOCATE (d3, SOURCE=d1)
  WRITE (6, ADVANCE='no', FMT='(''Object d3 (cloned d1)+4 days: '')')
  CALL d3%increment(4.0_rk)
  CALL d3%write('stdout')
  DEALLOCATE (d3)

  ALLOCATE (d3, SOURCE=d2)
  WRITE (6, ADVANCE='no', FMT='(''Object d3 (cloned d2)+345600s: '')')
  CALL d3%increment(345600._rk/(24._rk*3600._rk))
  CALL d3%write('stdout')
  DEALLOCATE (d3)

  ALLOCATE (d4, SOURCE=datetime(10,'Oct',1972,1,2,3))
  WRITE (6, ADVANCE='no', FMT='(''Object d4 (polym. alloc.) + 3 days: '')' &
       )
  CALL d4%date%increment(3._rk)
  CALL d4%write('stdout')
  DEALLOCATE (d4)


  d1 = date(year=2007, mon='Hif', day=20)
  WRITE (6, ADVANCE='no', FMT='(''Invalid Object d1: '')')
  CALL d1%write('stdout')
  WRITE (6, ADVANCE='no', FMT='(''Increment d1: '')')
  CALL d1%increment(4._rk)
  CALL d1%write('stdout')

END PROGRAM test_date
