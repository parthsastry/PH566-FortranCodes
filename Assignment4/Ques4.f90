module Functions

    implicit none

    contains

    real function f1(x)
        real, intent (in) :: x
        f1 = sin(x)
    end function f1

    real function f2(x)
        real, intent (in) :: x
        f2 = sqrt(x + 1)
    end function f2

    real function f3(x)
        real, intent (in) :: x
        f3 = 1/sqrt(x + 1)
    end function f3

end module Functions


program ques4

    use Functions
    implicit none

    abstract interface
        function func (z)
            real :: func
            real, intent (in) :: z
        end function func
    end interface

    procedure (func), pointer :: f_ptr => null ()

    real :: midpt_guess, trap_guess, simp_guess, x0, x1
    real, parameter :: pi = 4.D0*DATAN(1.D0)
    integer :: n = 4, a

    1 format(A20, F20.4, F20.4)
    2 format(A20, A20, A20)
    
    a = 0
    x0 = 0
    x1 = pi
    midpt_guess = 0
    trap_guess = 0
    simp_guess = 0
    call midpoint(a, n, x0, x1, midpt_guess)
    call trapezoidal(a, n, x0, x1, trap_guess)
    call simpsons(a, n, x0, x1, simp_guess)

    write(*,*) "Part (i) Integrals - "
    write(*,2) "Midpoint Rule -", "Integral", "Error"
    write(*,1) "", midpt_guess, abs(2.0 - midpt_guess)
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(2.0 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(2.0 - simp_guess)

    a = 1
    x0 = 0
    x1 = 3
    midpt_guess = 0
    trap_guess = 0
    simp_guess = 0
    call midpoint(a, n, x0, x1, midpt_guess)
    call trapezoidal(a, n, x0, x1, trap_guess)
    call simpsons(a, n, x0, x1, simp_guess)

    write(*,*) "Part (i) Integrals - "
    write(*,2) "Midpoint Rule -", "Integral", "Error"
    write(*,1) "", midpt_guess, abs(14.0/3.0 - midpt_guess)
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(14.0/3.0 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(14.0/3.0 - simp_guess)

    a = 2
    x0 = 0
    x1 = 3
    midpt_guess = 0
    trap_guess = 0
    simp_guess = 0
    call midpoint(a, n, x0, x1, midpt_guess)
    call trapezoidal(a, n, x0, x1, trap_guess)
    call simpsons(a, n, x0, x1, simp_guess)

    write(*,*) "Part (i) Integrals - "
    write(*,2) "Midpoint Rule -", "Integral", "Error"
    write(*,1) "", midpt_guess, abs(2.0 - midpt_guess)
    write(*,2) "Trapezoidal Rule -", "Integral", "Error"
    write(*,1) "", trap_guess, abs(2.0 - trap_guess)
    write(*,2) "Simpson's Rule -", "Integral", "Error"
    write(*,1) "", simp_guess, abs(2.0 - simp_guess)

end program

subroutine midpoint(a, n, x0, x1, integral)

    use Functions

    abstract interface
        function func (z)
            real :: func
            real, intent (in) :: z
        end function func
    end interface

    integer :: a, n, i
    real :: integral, x0, x1, x
    real :: interval
    procedure (func), pointer :: f_ptr => null()

    interval = (x1 - x0)/real(n)
    x = x0 + interval/2

    if (a.eq.0) then
        f_ptr => f1
    elseif (a.eq.1) then
        f_ptr => f2
    else
        f_ptr => f3
    endif

    do i = 1,n
        integral = integral + f_ptr(x)*interval
        x = x + interval
    enddo
    RETURN
end

subroutine trapezoidal(a, n, x0, x1, integral)

    use Functions

    abstract interface
        function func (z)
            real :: func
            real, intent (in) :: z
        end function func
    end interface

    integer :: a, n, i
    real :: integral, x0, x1, x
    real :: interval
    procedure (func), pointer :: f_ptr => null()

    interval = (x1 - x0)/real(n)
    x = x0

    if (a.eq.0) then
        f_ptr => f1
    elseif (a.eq.1) then
        f_ptr => f2
    else
        f_ptr => f3
    endif

    do i = 1,n
        integral = integral + interval*(f_ptr(x) + f_ptr(x + interval))/2
        x = x + interval
    enddo
    RETURN
end

subroutine simpsons(a, n, x0, x1, integral)

    use Functions

    abstract interface
        function func (z)
            real :: func
            real, intent (in) :: z
        end function func
    end interface

    integer :: a, n, i
    real :: integral, x0, x1, x
    real :: interval
    procedure (func), pointer :: f_ptr => null()

    interval = (x1 - x0)/real(n)
    x = x0

    if (a.eq.0) then
        f_ptr => f1
    elseif (a.eq.1) then
        f_ptr => f2
    else
        f_ptr => f3
    endif

    do i = 1,n/2
        integral = integral + interval/3.0*(f_ptr(x) + 4*f_ptr(x + interval) + f_ptr(x + 2*interval))
        x = x + 2*interval
    enddo
    RETURN
end
