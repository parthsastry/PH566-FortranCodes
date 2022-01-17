program quad_roots
    implicit none
    real :: a,b,c
    complex :: root1, root2, cdisc
    real :: disc
    WRITE(*,*) 'Give values for a,b,c'
    READ(*,*) a,b,c

    disc = b**2 - 4*a*c
    cdisc = cmplx(disc,0)

    if (a .eq. 0) then
        root1 = -1*c/b
        WRITE(*,*) 'The root is -', REALPART(root1)
    elseif (disc < 0) then
        WRITE(*,*) 'The roots are complex'
        root1 = (-1*b + CSQRT(cdisc))/(2*a)
        root2 = (-1*b - CSQRT(cdisc))/(2*a)
        WRITE(*,*) 'The roots are - ', root1, root2
    else
        root1 = (-1*b + SQRT(disc))/(2*a)
        root2 = (-1*b - SQRT(disc))/(2*a)
        WRITE(*,*) 'The roots are - ', REALPART(root1), REALPART(root2)
    endif

end program quad_roots

