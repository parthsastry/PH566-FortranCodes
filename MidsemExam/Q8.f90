program Q8

    real*16 :: T_d, C_v, Tval, integral, RHS, init = 0
    real*16 , parameter :: R = 8.3144598
    integer :: i = 1, j = 1, done = 0
    real*16 :: T(5), Cv(5), Td(5)

    OPEN(unit = 1, file = 'Debye_T.dat')

    T(1) = 300
    T(2) = 400
    T(3) = 500
    T(4) = 600
    T(5) = 700

    Cv(1) = 20.04
    Cv(2) = 21.95
    Cv(3) = 22.95
    Cv(4) = 23.53
    Cv(5) = 23.89

    do while (i < 6)
        C_v = Cv(i)
        Tval = T(i)

        j = 1
        done = 0
        do while (j < 402 .and. done.eq.0)
            T_d = 600 + (j-1)*0.5
            integral = 0
            call simpsons(init,T_d/Tval,100,integral)
            RHS = 9*(Tval/T_d)**3*integral
            if (abs(C_v/R - RHS) < 1e-2) then
                Td(i) = T_d
                done = 1
            endif
            j = j + 1
        enddo

        i = i + 1

    enddo
5   format (I10,F10.2,F10.1)
6   format (A10,A10,A10)
    do i = 1,5
        write(1,6) "T(K)", "Cv (SI)", "Td (K)"
        write(1,5) int(T(i)), Cv(i), Td(i)
    enddo

end program Q8

subroutine simpsons(a,b,n,integral)
    real*16 :: a, b, h, x, integral, f
    integer :: i, n

    h = (b-a)/float(n)
    x = a

    do i = 1,n+1
        if (i .eq. 1) then
            integral = integral + h/3! special case, function can't handle f(0). limit f(0) -> 1
        else if (i.eq.n) then
            integral = integral + h/3*f(x)
        else if (mod(i,2).eq.1) then
            integral = integral + 4*h/3*f(x)
        else
            integral = integral + 2*h/3*f(x)
        endif
        x = x + h
    enddo
    RETURN
end subroutine

real*16 function f(x)
    real*16 :: x
    f = x**4*exp(x)/(exp(x) - 1)**2
end function