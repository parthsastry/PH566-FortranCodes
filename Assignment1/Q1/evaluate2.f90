PROGRAM evaluate2
    REAL*8, parameter :: PI = 4.D0*DATAN(1.D0)
    REAL*8 :: x = 1.0
    REAL*8 :: y = 2.0
    REAL*8 :: a = 15.0
    REAL*8 :: a_rad
    REAL*8 :: eval2
    a_rad = a*PI/180.0
    eval2 = log10(x) + cos(a_rad) + ABS(x**2 + y**2) + 2*SQRT(x*y)
    write(*,*) eval2
ENDPROGRAM evaluate2