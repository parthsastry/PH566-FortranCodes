program ques1
    real :: guess, prev_guess
    1 format(F15.3)
    prev_guess = 10 ! random high guess to not trigger while condition
    guess = 0
    do while (ABS(guess - prev_guess) > 0.001)
        prev_guess = guess
        guess = guess - f(guess)/fp(guess)
    end do
    write(*,*) "The root of the equation is - "
    write(*,1) guess
end program

real function f(x)
    real :: x
    f = x**3 - x - 1
end

real function fp(x)
    real :: x
    fp = 3*x**2 - 1
end