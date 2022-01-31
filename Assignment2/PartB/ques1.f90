program students_marks
    implicit none

    real :: roll_marks(10,6)
    integer :: roll_nos(10)
    real :: total_marks(10)
    real :: percentage(10)
    real :: tot_marks
    integer :: i,j
    open(1,file='input_data.txt',status='old')
    open(2,file='data_out.txt',status='new')

    do i = 1,10
        read(1,*) (roll_marks(i,j),j=1,6)
    enddo
    
    do i = 1,10
        roll_nos(i) = int(roll_marks(i,1))
        tot_marks = 0.0
        do j = 2,6
            tot_marks = tot_marks + roll_marks(i,j)
        enddo
        total_marks(i) = tot_marks
        percentage(i) = tot_marks/5.0
        write(2,*) roll_nos(i), total_marks(i), percentage(i)
    enddo

endprogram students_marks