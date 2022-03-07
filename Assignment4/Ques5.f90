module Functions

    implicit none

    contains

    real*16 function func(t)
        real*16, intent (in) :: t
        func = exp(-1*t**2)
    end function func


end module Functions

program ques5

    use Functions
    implicit none

    real*16, parameter :: pi = 4.D0*DATAN(1.D0)

    real*16 :: trap_guess, simp_guess, x0, x1
    integer :: n

    1 format(A20, F20.10, F20.10)
    2 format(A20, A20, A20)

    n = 100
    x0 = 0
    x1 = 0.5
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(0.5), n = 100"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.4795 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.4795 - simp_guess)

    n = 1000
    x0 = 0
    x1 = 0.5
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(0.5), n = 1000"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.4795 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.4795 - simp_guess)

    n = 10000
    x0 = 0
    x1 = 0.5
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(0.5), n = 10000"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.4795 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.4795 - simp_guess)

    n = 100
    x0 = 0
    x1 = 1.0
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(1), n = 100"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.157299 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.157299 - simp_guess)

    n = 1000
    x0 = 0
    x1 = 1.0
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(1), n = 1000"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.157299 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.157299 - simp_guess)

    n = 10000
    x0 = 0
    x1 = 1.0
    trap_guess = 0
    simp_guess = 0
    call trapezoidal(n, x0, x1, trap_guess)
    call simpsons(n, x0, x1, simp_guess)
    trap_guess = 2/sqrt(pi)*trap_guess
    simp_guess = 2/sqrt(pi)*simp_guess
    write(*,*) "Absolute error in erf(1), n = 10000"
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(0.157299 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(0.157299 - simp_guess)

end program ques5



subroutine trapezoidal(n, x0, x1, integral)

    use Functions

    integer :: n, i
    real*16 :: integral, x0, x1, x
    real*16 :: interval

    interval = (x1 - x0)/real(n)
    x = x0

    do i = 1,n
        integral = integral + interval*(func(x) + func(x + interval))/2
        x = x + interval
    enddo
    RETURN
end

subroutine simpsons(n, x0, x1, integral)

    use Functions

    integer :: n, i
    real*16 :: integral, x0, x1, x
    real*16 :: interval

    interval = (x1 - x0)/real(n)
    x = x0

    do i = 1,n/2
        integral = integral + interval/3.0*(func(x) + 4*func(x + interval) + func(x + 2*interval))
        x = x + 2*interval
    enddo
    RETURN
end