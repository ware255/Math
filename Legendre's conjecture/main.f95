program main
    use fmzm
    implicit none
    type(IM) :: n, n_, n_one_, x
    character(len=:), allocatable :: i_n
    integer :: digit, i, t
    logical :: logic

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

    n_ = n * n
    n_one_ = (n + 1) * (n + 1)

    x = n_
    do while (x <= n_one_)
        call SolovoyStrassen(x, 10, logic)
        if (logic) exit
        x = x + 1
    end do

    if (logic) then
        print *, "Prime numbers found!"
        call IMPRNT(x)
    else
        print *, "No prime numbers found!"
    end if

    read *
end program main

subroutine swap(a, b)
    use fmzm
    implicit none
    type(IM), intent(inout) :: a, b
    type(IM) :: t
    t = b
    b = a
    a = t
end subroutine swap

subroutine Jacobi(a, n, x)
    use fmzm
    implicit none
    type(IM), intent(in) :: a, n
    type(IM), intent(out) :: x
    type(IM) :: y, z, r

    if (a .eq. 0 .or. a .eq. 1) then
        x = a
        return
    end if

    y = a
    z = n

    y = mod(y, z)
    x = 1

    do while (y .ne. 0)
        do while (mod(y, to_im(2)) .eq. 0)
            y = y / 2
            r = mod(z, to_im(8))
            if (r .eq. 3 .or. r .eq. 5) x = -x
        end do
        call swap(y, z)
        if (mod(y, to_im(4)) .eq. 3 .and. mod(z, to_im(4)) .eq. 3) x = -x
        y = mod(y, z)
    end do

    if (z .eq. 1) return
    x = 0
end subroutine Jacobi

subroutine random_func(rnd_int)
    double precision :: rnd
    integer, intent(out) :: rnd_int
    integer :: seedsize, i
    integer, allocatable :: seed(:)

    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    do i = 1, seedsize
        call system_clock(count=seed(i))
    end do
    call random_seed(put=seed(:))

    call random_number(rnd)
    rnd_int = int(rnd * 1000000)
end subroutine random_func

subroutine SolovoyStrassen(p, k, logic)
    use fmzm
    implicit none
    type(IM), intent(in) :: p
    integer, intent(in) :: k
    type(IM) :: m, t, a
    integer :: i, rnd
    logical, intent(out) :: logic

    if (p < 2) then
        logic = .false.
        return
    end if

    if (p .ne. 2 .and. mod(p, to_im(2)) .eq. 0) then
        logic = .false.
        return
    end if

    do i = 1, k
        call random_func(rnd)
        a = mod(to_im(rnd), (p - 1) + 1)
        call Jacobi(a, p, t)
        m = power_mod(a, (p - 1) / 2, p)
        if (t .eq. 0 .or. m .ne. mod(t, p)) then
            logic = .false.
            return
        end if
    end do

    logic = .true.
end subroutine SolovoyStrassen
