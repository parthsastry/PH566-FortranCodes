program ques2
    integer :: inp_int, ans_mod
    write(*,*) 'Give input integer to compute mod check 11 digit'
    read(*,*) inp_int
    
    1 format(I2)

    ans_mod = mod_check_11_digit(inp_int)
    write(*,1) ans_mod
endprogram

integer function mod_check_11_digit(input)
    integer :: input, sum = 0, digit, i = 2
    do while (input > 0)
        digit = mod(input, 10)
        sum = sum + i * digit
        input = input/10
        i = i + 1
    enddo
    mod_check_11_digit = 11 - mod(sum, 11)
end