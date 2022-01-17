PROGRAM evaluate2
    REAL, PARAMETER :: PI = 3.14159265
    REAL :: x = 1.0
    REAL :: y = 2.0
    REAL :: a = 15.0
    REAL :: a_rad
    REAL :: eval2
    a_rad = a*PI/180.0
    eval2 = log10(x) + cos(a_rad) + ABS(x**2 + y**2) + 2*SQRT(x*y)
    write(*,*) eval2
ENDPROGRAM evaluate2