module Functions

    implicit none

    contains

    real*16 function func(x)
        real*16, intent (in) :: x
        func = 37.5 - 3.5*x
    end function func


end module Functions

program ques4

    use Functions
    implicit none

    real*16 :: x_eul = 50, x_rk = 50, t = 0, ans
    real*16 :: k1, k2
    real*16 :: h = 1.5

    integer :: n, i
    n = 3/h

    1 format(A40, F10.5)
    write(*,1) "h = ", h

    do i = 1,n
        x_eul = x_eul + h*func(x_eul)
        t = t + h
    end do

    write(*,1) "Euler's Method answer = ", x_eul

    t = 0

    do i = 1,n
        k1 = func(x_rk)
        k2 = func(x_rk + k1*h)
        x_rk = x_rk + h/2.0*(k1 + k2)
        t = t + h
    end do

    write(*,1) "Runge-Kutta Second Order Method answer = ", x_rk

    ans = 10.7143 + 39.2857*exp(-3.5*3)

    write(*,1) "Percentage Error (Euler's) = ", ABS(ans - x_eul)/ans*100
    write(*,1) "Percentage Error (Runge-Kutta's) = ", ABS(ans - x_rk)/ans*100

    h = 0.1875
    write(*,1) "h = ", h
    n = 3/h

    do i = 1,n
        x_eul = x_eul + h*func(x_eul)
        t = t + h
    end do

    write(*,1) "Euler's Method answer = ", x_eul

    t = 0

    do i = 1,n
        k1 = func(x_rk)
        k2 = func(x_rk + k1*h)
        x_rk = x_rk + h/2.0*(k1 + k2)
        t = t + h
    end do

    write(*,1) "Runge-Kutta Second Order Method answer = ", x_rk

    ans = 10.7143 + 39.2857*exp(-3.5*3)

    write(*,1) "Percentage Error (Euler's) = ", ABS(ans - x_eul)/ans*100
    write(*,1) "Percentage Error (Runge-Kutta's) = ", ABS(ans - x_rk)/ans*100

end program ques4