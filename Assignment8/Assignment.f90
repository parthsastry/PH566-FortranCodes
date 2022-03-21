program density_states_lanczos

    real*16 :: a(40), b(41), junk, energy, delta = 0.001
    real*16 :: density_n(2001)
    complex*32 :: z, Tz, output
    integer :: i, j, k

    real*16, parameter :: pi = 4*atan(1.0_16)

    3 format(A10, A10)
    4 format(F10.2, F10.5)

    open(1, file='data.txt',status='old')
    open(2, file='DOS.dat',status='replace')

    b(1) = 1.0

    do i = 1,40
        read(1,*) junk, a(i), b(i+1)
    enddo

    do j = 1,2001
        energy = -10.0 + (j-1)*0.01
        z = cmplx(energy, delta, kind = 16)

        Tz = (z - a(40) - SQRT(complex((energy - a(40))**2 - 4*b(41)**2, delta)))/(2*b(41)**2)

        do k = 1,40
            if (k == 1) then
                output = b(41)**2*Tz
            else
                output = b(42 - k)**2/output
            endif
            output = z - a(41 - k) - output
        enddo

        output = b(1)**2/output
        density_n(j) = -1/pi*aimag(output)

    enddo

    write(2,3) "E", "n(E)"
    
    do i = 1,2001
        energy = -10.0 + (i-1)*0.01
        write(2,4) energy, density_n(i)
    enddo

endprogram density_states_lanczos