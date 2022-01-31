program sinx_series

    real*8, parameter :: pi = 4.D0*DATAN(1.D0)
    real*8 :: x, series, sinx, term, percent_error
    real*8 :: angle_arr(3)
    integer :: i,j,n

    write(*,*) 'Give value of n, upto which iterations are done'
    read(*,*) n

    angle_arr(1) = 45
    angle_arr(2) = 60
    angle_arr(3) = 90

1   format(F7.2,F10.4,I4,F10.4,F5.2)

    do i = 1,3
        x = angle_arr(i)*pi/180.0
        sinx = sin(x)

        term = x
        series = x

        do j = 2,n
            term = term*(-1)*x**2/((2*j-1)*(2*j-2))
            series = series + term
        enddo

        percent_error = abs(series - sinx)/sinx*100
        write(*,1) angle_arr(i), series, n, sinx, percent_error
    enddo

endprogram sinx_series