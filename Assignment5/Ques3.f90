module Functions

    implicit none

    contains

    real*16 function func(x, y)
        real*16, intent (in) :: x, y
        func = (x + y)*sin(x*y)
    end function func


end module Functions

program ques3

    use Functions
    implicit none

    real*16 :: y = 5, x = 0
    real*16 :: k1, k2, k3, k4
    real*16, parameter :: step_size = 0.1

    integer :: n, i
    n = 2/step_size

    1 format(A40, F10.5)

    do i = 1,n
        k1 = func(x, y)
        k2 = func(x + step_size/2.0, y + k1*step_size/2.0)
        k3 = func(x + step_size/2.0, y + k2*step_size/2.0)
        k4 = func(x + step_size, y + step_size*k3)
        y = y + step_size/3.0*(k1/2 + k2 + k3 + k4/2)
        x = x + step_size
    end do

    write(*,1) "Runge-Kutta Fourth Order Method answer = ", y

end program ques3