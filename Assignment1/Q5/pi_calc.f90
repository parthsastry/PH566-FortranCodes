program pi_calc
    implicit none
    integer :: i, n
    real*8 :: val = 0
    real*8, parameter :: PI = 4.D0*DATAN(1.D0)

    write(*,*) 'Give value of n, upto which iterations are done'
    read(*,*) n

    do i = 1, n
        if (MOD(i,2) == 1) then
            val = val + 4.0/(2*i - 1)
        else
            val = val - 4.0/(2*i - 1)
        endif
    enddo

    write(*,*) 'The value of the series is -', val
    write(*,*) 'The value of pi is -', PI
    write(*,*) 'The ratio between the two is -', val/PI

endprogram pi_calc