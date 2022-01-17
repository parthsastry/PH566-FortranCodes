program exp_calc
    implicit none
    integer :: i, n, fac
    real*8 :: val = 1
    real*8 :: actual_val
    real*8 :: x
    real*8 :: x_powers

    write(*,*) 'Give value of n, upto which iterations are done'
    read(*,*) n
    write(*,*) 'Give value of x'
    read(*,*) x

    fac = 1
    x_powers = 1

    do i = 1, n
        fac = fac*i
        x_powers = x_powers*x
        val = val + (x_powers/fac)
    enddo

    actual_val = EXP(x)

    write(*,*) 'The value of the series is -', val
    write(*,*) 'The actual value of the exponential is -', actual_val
    write(*,*) 'The ratio between the two is -', val/actual_val

endprogram exp_calc