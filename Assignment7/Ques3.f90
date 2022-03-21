program ques3

    real*16 :: A(3,3), B(3)
    real*16 :: X(3), X_prev(3)
    integer :: i,j

    1 format(A15, A20, I3)
    2 format(A15, F10.2, F10.2, F10.2)

    do i = 1,3
        X(i) = 0.0
        X_prev(i) = 10.0
    enddo

    B(1) = 5.0
    B(2) = 5.0
    B(3) = 1.0

    A(1,1) = 3.0
    A(1,2) = -1.0
    A(1,3) = 0.0
    A(2,1) = 1.0
    A(2,2) = 3.0
    A(2,3) = -1.0
    A(3,1) = 0.0
    A(3,2) = 1.0
    A(3,3) = 2.0

    write(*,*) "Jacobi Method"

    i = 0

    do while (ABS(SUM(X - X_Prev)) > 1E-3)
        write(*,1) "---------------", "Iteration Number : ", i
        write(*,2) "Estimate = ", X(1), X(2), X(3)
        X_prev = X
        do j = 1,3
            X(j) = 1/A(j,j)*(B(j) - SUM(A(j,:)*X_prev) + A(j,j)*X_prev(j))
        enddo
        i = i + 1
    enddo

    write(*,2) "Final Solution = ", X(1), X(2), X(3)

    write(*,*) "Gauss-Seidel Method"

    do i = 1,3
        X(i) = 0.0
        X_prev(i) = 10.0
    enddo

    i = 0

    do while (ABS(SUM(X - X_Prev)) > 1E-3)
        write(*,1) "---------------", "Iteration Number : ", i
        write(*,2) "Estimate = ", X(1), X(2), X(3)
        X_prev = X
        do j = 1,3
            X(j) = 1/A(j,j)*(B(j) - SUM(A(j,:)*X) + A(j,j)*X_prev(j))
        enddo
        i = i + 1
    enddo

    write(*,2) "Final Solution = ", X(1), X(2), X(3)

endprogram ques3