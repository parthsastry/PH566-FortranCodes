program ques4

    real*16 :: A(8,8), B(8)
    real*16 :: X(8), X_prev(8)
    integer :: i,j

    A = 0.0

    1 format(A15, A20, I3)
    2 format(A15, F10.5, F10.5, F10.5, F10.5, F10.5, F10.5, F10.5, F10.5)

    do i = 1,4
        X(i) = 1.0
        X_prev(i) = 10.0
    enddo

    B(1) = 18.0
    B(2) = 18.0
    B(3) = 4.0
    B(4) = 4.0
    B(5) = 26.0
    B(6) = 16.0
    B(7) = 10.0
    B(8) = 32.0

    A(1,1) = 4.0
    A(1,2) = -1.0
    A(1,3) = -1.0
    A(2,1) = -1.0
    A(2,2) = 4.0
    A(2,3) = -1.0
    A(2,4) = -1.0
    A(3,2) = -1.0
    A(3,3) = 4.0
    A(3,4) = -1.0
    A(3,5) = -1.0
    A(4,3) = -1.0
    A(4,4) = 4.0
    A(4,5) = -1.0
    A(4,6) = -1.0
    A(5,4) = -1.0
    A(5,5) = 4.0
    A(5,6) = -1.0
    A(5,7) = -1.0
    A(6,5) = -1.0
    A(6,6) = 4.0
    A(6,7) = -1.0
    A(6,8) = -1.0
    A(7,6) = -1.0
    A(7,7) = 4.0
    A(7,8) = -1.0
    A(8,7) = -1.0
    A(8,8) = 4.0

    i = 0

    do while (ABS(SUM(X - X_Prev)) > 1E-6)
        write(*,1) "---------------", "Iteration Number : ", i
        write(*,2) "Estimate = ", X(1), X(2), X(3), X(4), X(5), X(6), X(7), X(8)
        X_prev = X
        do j = 1,8
            X(j) = 1/A(j,j)*(B(j) - SUM(A(j,:)*X) + A(j,j)*X_prev(j))
        enddo
        i = i + 1
    enddo

    write(*,2) "Final Solution = ", X(1), X(2), X(3), X(4), X(5), X(6), X(7), X(8)

endprogram ques4