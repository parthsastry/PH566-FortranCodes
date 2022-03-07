module Functions

    implicit none

    contains

    real*16 function func(x, y)
        real*16, intent (in) :: x, y
        func = 3*exp(-x) - 0.4*y
    end function func


end module Functions

program ques1

    use Functions
    implicit none

    real*16 :: y_val = 5, x_val = 0
    real*16, parameter :: step_size = 0.01

    integer :: n, i
    n = 3/step_size

    1 format(A30, F10.5)

    do i = 1,n
        y_val = y_val + step_size*func(x_val, y_val)
        x_val = x_val + step_size
    end do

    write(*,1) "Euler's Method answer = ", y_val
    ! RK 2nd order - h ~ 0.047 for second decimal place
    ! RK 4th order - h ~ 0.158 for second decimal place

end program ques1
