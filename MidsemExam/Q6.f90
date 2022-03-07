program Q6

    integer :: num
    integer :: factor = 2, count = 0

    OPEN(unit = 1, file = 'PrimeFactors.dat')
2   format(A9,I3,A3,I5)

    write(*,*) 'Input the number'
    read(*,*) num

    do while(factor .le. num)
10      if (MOD(num, factor) .eq. 0) then
            count = count + 1
            write(*,2) 'Factor # ', count, ' : ', factor
            write(1,*) 'Factor # ', count, ' : ', factor
            num = num/factor
            goto 10
        else
            factor = factor + 1
        endif
    enddo

end program Q6