program ques2
    real :: guess(2), prev_guess(2)
    integer :: i
    1 format(F15.3, F15.3)
    prev_guess(1) = 10 
    guess(1) = 0
    prev_guess(2) = -10
    guess(2) = -5 ! initial estimates based on intuition

    do i = 1,2
        do while (ABS(guess(i) - prev_guess(i)) > 0.001)
            prev_guess(i) = guess(i)
            guess(i) = guess(i) - f(guess(i))/fp(guess(i))
        end do
    end do
    write(*,*) "The roots of the equation are - "
    write(*,1) guess(1), guess(2)

end program

real function f(x)
    real :: x
    f = exp(2*x) - x - 6
end

real function fp(x)
    real :: x
    fp = 2*exp(2*x) - 1
end