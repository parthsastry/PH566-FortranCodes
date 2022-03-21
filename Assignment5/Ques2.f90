module Functions

    implicit none

    contains

    real*16 function func(x, y)
        real*16, intent (in) :: x, y
        func = x + y**2
    end function func


end module Functions

program ques2

    use Functions
    implicit none

    real*16 :: y = 1, x = 0
    real*16 :: k1, k2, k3, k4
    real*16, parameter :: step_size = 0.1

    integer :: n, i
    n = 1

    1 format(A40, F10.5)

    do i = 1,n
        k1 = func(x, y)
        k2 = func(x + step_size, y + k1*step_size)
        y = y + step_size/2.0*(k1 + k2)
        x = x + step_size
    end do

    write(*,1) "Runge-Kutta Second Order Method answer = ", y

    y = 1
    x = 0
    do i = 1,n
        k1 = func(x, y)
        k2 = func(x + step_size/2.0, y + k1*step_size/2.0)
        k3 = func(x + step_size/2.0, y + k2*step_size/2.0)
        k4 = func(x + step_size, y + step_size*k3)
        y = y + step_size/3.0*(k1/2 + k2 + k3 + k4/2)
        x = x + step_size
    end do

    write(*,1) "Runge-Kutta Fourth Order Method answer = ", y

    ! The analytical solution of this differential equation is a nightmare. I didn't know how exactly to get the 'number of iterations' for 2nd decimal place accuracy.

end program ques2