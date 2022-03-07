program Q7

    real*16 :: kappa, hbar, inp, f, fp
    real*16, parameter :: l = 0.2e-9, m = 9.1e-31
    real*16 :: guess = -1, prev_guess = 10 ! prev_guess initialized to high separation from guess

1   format(A25, F8.5, A3)

    hbar = 6.58e-16*1.6e-19 ! SI units. easier to work with
    do while(abs(guess - prev_guess) > 1e-5)
        kappa = sqrt(2*m*guess*1.6e-19)/hbar
        inp = kappa*l
        prev_guess = guess
        guess = prev_guess - f(prev_guess)/fp(prev_guess)
    enddo

    write(*,1) 'Ground state energy is - ', guess, ' eV'

end program Q7

real*16 function f(x)
    real*16 :: x
    f = cos(x) - 0.226*x
end function

real*16 function fp(x)
    real*16 :: x
    fp = -1*sin(x) - 0.226
end function