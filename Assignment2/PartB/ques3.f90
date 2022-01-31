program matrix_mult

    real :: A(2,3), B(3,2), C(2,2), D(2,2)
    integer :: i,j,k
    open(1,file='input.txt',status='old')
    open(2,file='out.txt',status='new')

    do i = 1,2
        read(1,*) (A(i,j),j=1,3)
    enddo

    do i = 3,5
        read(1,*) (B(i-2,j),j=1,2)
    enddo

    do i = 1,2
        do j = 1,2
            C(i,j) = 0.0
            do k = 1,3
                C(i,j) = C(i,j) + A(i,k)*B(k,j)
            enddo
        enddo
    enddo

    D = matmul(A,B)

    do i = 1,2
        write(2,*) D(i,1), D(i,2)
    enddo

    do i = 1,2
        write(2,*) C(i,1), C(i,2)
    enddo

endprogram matrix_mult