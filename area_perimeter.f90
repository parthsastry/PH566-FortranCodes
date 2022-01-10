PROGRAM area_perimeter
    real a,b,c,area,perimeter,s
    a=3.0
    b=4.0
    c=5.0
    perimeter = a + b + c
    s = perimeter/2.0
    area = sqrt(s*(s-a)*(s-b)*(s-c))
    write(*,*) perimeter, area
ENDPROGRAM area_perimeter