program ques1
    real :: x(3), ans
    1 format(F15.4)
    x(1) = -0.5
    x(2) = 0.1
    x(3) = 2.5
    do i = 1,3
        ans = evaluate(x(i))
        write(*,1) ans
    enddo
endprogram

real function evaluate(x)
    real :: x
    if (x.lt.2.0) then
        evaluate = 5*x**2 + 3*x + 2
    elseif (x.eq.2.0) then
        evaluate = 0.0
    else
        evaluate = 5*x**2 - 3*x + 1
    endif
end
