program ques2

    real*16 :: A(4,4), B(4)
    real*16 :: Xe(4), X(4), X_prev(4)
    integer :: i,j

    1 format(A15, A20, I3)
    2 format(A15, F10.5, F10.5, F10.5, F10.5)
    3 format(A20, F10.5)

    do i = 1,4
        Xe(i) = 1.0
        X(i) = 1.1
        X_prev(i) = 0.0
    enddo

    B(1) = -3.0
    B(2) = -2.0
    B(3) = -2.0
    B(4) = -3.0

    do i = 1,4
        do j = 1,4
            if  (i==j) then
                A(i,j) = -4.0
            else if (abs(i-j) == 1) then
                A(i,j) = 1.0
            else
                A(i,j) = 0.0
            endif
        enddo
    enddo

    i = 0

    do while (ABS(SUM(X - X_Prev)) > 1E-6)
        write(*,1) "---------------", "Iteration Number : ", i
        write(*,2) "Estimate = ", X(1), X(2), X(3), X(4)
        X_prev = X
        do j = 1,4
            X(j) = 1/A(j,j)*(B(j) - SUM(A(j,:)*X) + A(j,j)*X_prev(j))
        enddo
        write(*,3) "Error = ", log10(ABS(SUM(X - Xe)))
        i = i + 1
    enddo

    write(*,2) "Final Solution = ", X(1), X(2), X(3), X(4)

endprogram ques2