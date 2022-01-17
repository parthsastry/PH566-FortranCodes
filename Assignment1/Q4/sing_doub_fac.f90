program sing_doub_fac
    implicit none
    integer :: n
    integer :: sing_doub
    integer :: fac
    integer :: i
    write(*,*) 'Provide the value of n'
    read(*,*) n
    write(*,*) 'Type 1 for Single factorial, 0 for Double factorial'
    read(*,*) sing_doub
    
    if(sing_doub == 1) then
        fac = 1
        do i = 1,n
            fac = fac*i
        enddo
        write(*,*) 'The Single factorial is -', fac
    elseif(sing_doub == 0) then
        fac = 1
        if (mod(n,2) == 0) then
            write(*,*) 'n is not an odd integer'
        else
            do i = 1,n,2
                fac = fac*i
            enddo
            write(*,*) 'The Double factorial is -', fac
        endif
    else
        write(*,*) 'Give proper value for computation of single or double factorial'
    endif

endprogram sing_doub_fac