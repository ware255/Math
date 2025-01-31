program main
    implicit none
    integer(8) :: nx = 1000, ny = 1000, i, j
    real(8) :: xmin = -2.0d0, ymin = -2.0d0
    real(8) :: xmax =  2.0d0, ymax =  2.0d0
    real(8) dx, dy, x, y
    complex(8) c

    dx = (xmax - xmin) / real(nx, 8)
    dy = (ymax - ymin) / real(ny, 8)

    do i = 0, ny - 1
        do j = 0, nx - 1
            x = xmin + dx * real(j, 8)
            y = ymin + dy * real(i, 8)
            c = cmplx(x, y, 8)
            if (is_mandelbrot_set(c)) then
                print *, x, y
            end if
        end do
    end do
contains
    logical function is_mandelbrot_set(c)
        implicit none
        complex(8), intent(in) :: c
        complex(8) z
        integer(8) i
        z = cmplx(0.0, 0.0, 8)
        do i = 0, 50 - 1
            if (abs(z) > 2.0d0) then
                is_mandelbrot_set = .false.
                return
            end if
            z = z * z + c
        end do
        is_mandelbrot_set = .true.
    end function is_mandelbrot_set
end program main
