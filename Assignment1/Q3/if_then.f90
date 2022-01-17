program if_then
    implicit none

    real :: inp, val
    write(*,*) 'Input value of x'
    read(*,*) inp
    val = f_val(inp)
    write(*,*) val

    contains

    real function f_val(x)
    real :: x
    if (x < 2) then
        f_val = 5*x**2 + 3*x + 2
    elseif (x == 2) then
        f_val = 0
    else
        f_val = 5*x**2 - 3*x + 1
    endif
    end function f_val

end program if_then