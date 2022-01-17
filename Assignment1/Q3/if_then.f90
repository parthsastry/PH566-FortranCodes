program if_then
    implicit none

    real :: val1, val2, val3
    val1 = f_val(-0.5)
    val2 = f_val(0.1)
    val3 = f_val(2.5)
    write(*,*) val1, val2, val3

    contains

    real function f_val(x)
    real :: x
    if (x < 2) then
        f_val = 5*x**2 + 3*x + 2
    elseif (x == 0) then
        f_val = 0
    else
        f_val = 5*x**2 - 3*x + 1
    endif
    end function f_val

end program if_then