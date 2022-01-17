PROGRAM evaluate3
    REAL, PARAMETER :: PI = 3.14159265
    REAL :: x = 2.0
    REAL :: m = 1.0
    REAL :: a = 2.0
    REAL :: eval3
    eval3 = EXP(SQRT(2*a*(x - m)**3))/(a*SQRT(2*PI))
    write(*,*) eval3
ENDPROGRAM evaluate3