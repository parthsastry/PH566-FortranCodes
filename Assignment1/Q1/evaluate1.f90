PROGRAM evaluate1
    implicit NONE
    REAL :: a = 3.0
    REAL :: b = 4.0
    REAL :: eval1
    eval1 = (a**b)/(b*(b-a))
    write(*,*) eval1
ENDPROGRAM evaluate1