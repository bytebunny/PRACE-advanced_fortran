    MODULE mod_test_qdr
      USE qdr
      IMPLICIT NONE
      TYPE, EXTENDS (qdr_fun) :: qdr_poly
        REAL (KIND=rk), ALLOCATABLE :: f(:)
      CONTAINS
        PROCEDURE :: eval => eval_poly
      END TYPE
      TYPE, EXTENDS (qdr_fun) :: qdr_mydisc
      CONTAINS
        PROCEDURE :: eval => eval_mydisc
      END TYPE
    CONTAINS
      REAL (KIND=rk) FUNCTION eval_poly(this, x)
        CLASS (qdr_poly) :: this
        REAL (KIND=rk), INTENT (IN) :: x
!
        INTEGER :: i, imx
!
        imx = size(this%f)
        eval_poly = this%f(imx)
        DO i = imx - 1, 1, -1
          eval_poly = eval_poly*x + this%f(i)
        END DO
      END FUNCTION
      REAL (KIND=rk) FUNCTION eval_mydisc(this, x)
        CLASS (qdr_mydisc) :: this
        REAL (KIND=rk), INTENT (IN) :: x
!
        IF (x<1.0_rk) THEN
          eval_mydisc = x
        ELSE
          eval_mydisc = x - 1.0_rk
        END IF
!    write(6, *) 'eval: x,y - ',x,eval_mydisc
      END FUNCTION
    END MODULE
    PROGRAM test_qdr
      USE mod_test_qdr
      IMPLICIT NONE
      TYPE (qdr_poly) :: o1_qdr
      TYPE (qdr_mydisc) :: o2_qdr
      TYPE (qdr_opt_disc), TARGET :: o2_sub
      TYPE (qdr_opt_qag), TARGET :: o2_sub_qag
      REAL (KIND=rk) :: intv(2), result, check
      INTEGER :: status
!
      intv = (/ 0.0_rk, 2.0_rk /)
      ALLOCATE (o1_qdr%f(3))
      o1_qdr%f = (/ 1.0_rk, 2.5_rk, 4.0_rk /)
      result = integral_1d(intv, o1_qdr, status)
      check = 2.0_rk + 1.25_rk*4.0_rk + 4.0_rk/3.0_rk*8.0_rk
      WRITE (6, *) 'status = ', status, ' result: ', result, ' check: ', check
!
      ALLOCATE (o2_sub%disc(1))
      o2_sub%disc(1) = 1.0_rk
!
      result = integral_1d(intv, o2_qdr, status)
      check = 1.0_rk
      WRITE (6, *) 'Using default algorithm: '
      WRITE (6, *) 'status = ', status, ' result: ', result, ' check: ', check
!
      o2_qdr%options => o2_sub
      result = integral_1d(intv, o2_qdr, status)
      check = 1.0_rk
      WRITE (6, *) 'Using disc algorithm: '
      WRITE (6, *) 'status = ', status, ' result: ', result, ' check: ', check
!
      o2_qdr%options => o2_sub_qag
      result = integral_1d(intv, o2_qdr, status)
      check = 1.0_rk
      WRITE (6, *) 'Using QAG algorithm: '
      WRITE (6, *) 'status = ', status, ' result: ', result, ' check: ', check

    END PROGRAM
