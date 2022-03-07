program ques3
    implicit none
    
    ! problem states 2*theta = 2*(2*sin(theta)), equivalent to 
    ! 2*sin(x) - x = 0
    ! we need to solve this upto 18 decimal places. Can solve via
    ! N-R method


    real*16 :: theta, prev_theta, f, fp
    1 format(F23.20)
    prev_theta = 1 ! random high guess to not trigger while condition initially
    theta = 1.5
    do while (ABS(theta - prev_theta) > 1d-18)
        prev_theta = theta
        theta = theta - f(theta)/fp(theta)
    end do
    write(*,*) "The length of the chord is - "
    write(*,1) 2*theta ! length = 2*theta
end program

real*16 function f(x)
    real*16 :: x
    f = 2*SIN(x) - x
end

real*16 function fp(x)
    real*16 :: x
    fp = 2*COS(x) - 1
end