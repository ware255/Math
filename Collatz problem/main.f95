program main
    use fmzm
    implicit none
    type(IM) :: n

    character(len=:), allocatable :: i_n
    integer :: digit, i, t

    print '(A)', "何桁の数値ですか？"
    read (*, *) digit
    allocate(character(len=digit) :: i_n)

    print '(A)', "数値を入力してください。"
    read (*, *) i_n

    n = 0
    do i = digit, 1, -1
        read (i_n(i:i), *) t
        n = n + t * (10 ** (digit - i))
    end do

    deallocate(i_n)

    do while (n > 1)
        if (mod(n, to_im(2)) .eq. 0) then
            n = n / 2
            call IMPRNT(n)
        else
            n = 3 * n + 1
            call IMPRNT(n)
        end if
    end do

    read *
end program main
