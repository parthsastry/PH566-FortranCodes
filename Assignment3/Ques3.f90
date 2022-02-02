program ques3
    real :: inp_arr(100), average, root_mean_sq
    integer :: n, i
    1 format(F12.4)
    write(*,*) "Input the number of numbers in the array"
    read(*,*) n
    write(*,*) "Input the numbers below"
    do i = 1,n
        read(*,*) inp_arr(i)
    enddo
    call avg(inp_arr, n, average)
    write(*,*) "The average is - "
    write(*,1) average
    call rms(inp_arr, n, root_mean_sq)
    write(*,*) "The RMS is - "
    write(*,1) root_mean_sq
endprogram

subroutine avg(x, n, average)
    real :: x(100), average, sum = 0
    integer :: n
    do i = 1,n
        sum = sum + x(i)
    enddo
    average = sum/float(n)
    RETURN
end

subroutine rms(x, n, root_mean_sq)
    real :: x(100), root_mean_sq, sum_sq = 0
    integer :: n
    do i = 1,n
        sum_sq = sum_sq + x(i)**2
    enddo
    root_mean_sq = sqrt(sum_sq/float(n))
    RETURN
end