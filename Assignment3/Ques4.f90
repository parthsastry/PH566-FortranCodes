program ques4
    real :: r(3), rdf(3,3)
    real, parameter :: pi = 4.D0*DATAN(1.D0)
    integer :: i
    1 format(F15.5, F15.5, F15.5)
    r(1) = 0.5
    r(2) = 1.0
    r(3) = 2.5

    do i = 1,3
        call one_s(r(i), rdf(i,1))
        rdf(i,1) = 4*pi*r(i)**2*rdf(i,1)**2
        call two_s(r(i), rdf(i,2))
        rdf(i,2) = 4*pi*r(i)**2*rdf(i,2)**2
        call two_p(r(i), rdf(i,3))
        rdf(i,3) = 4*pi*r(i)**2*rdf(i,3)**2
    enddo
    write(*,*) "Numerical values of RDF for r = 0.5 - "
    write(*,1) rdf(1,1), rdf(2,1), rdf(3,1)
    write(*,*) "Numerical values of RDF for r = 1.0 - "
    write(*,1) rdf(1,2), rdf(2,2), rdf(3,2)
    write(*,*) "Numerical values of RDF for r = 2.5 - "
    write(*,1) rdf(1,3), rdf(2,3), rdf(3,3)
endprogram

subroutine one_s(r, out)
    real :: r, out
    real, parameter :: e = exp(1.0)
    out = e**(-r/2.0)
    RETURN
end

subroutine two_s(r, out)
    real :: r,out
    real, parameter :: e = EXP(1.0)
    out = (1.0/sqrt(32.0))*(2 - r)*e**(-r/2.0)
    RETURN
end

subroutine two_p(r, out)
    real :: r,out
    real, parameter :: e = EXP(1.0)
    out = (1.0/sqrt(972.0))*(6 - 6*r + r**2)*e**(-r/2.0)
    RETURN
end

