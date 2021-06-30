program test
    ! gfortran -O3 matr4.f90 -o matr4
    implicit none
    integer :: n, stat, i, j
    character(len=32) :: nstr
    real(8), allocatable :: q(:,:), w(:), res(:)
    real :: dt, tic, toc
    real(8), parameter :: pi = 4._8*atan(1._8)
    real(8) :: a, b

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n

    allocate(q(n, n), w(n), res(n))

    do i = 1, n
        w(i) = real(i, 8)
    enddo

    a = sqrt(1._8/n)
    b = sqrt(2._8/n)
    do i = 1, n
        q(i, 1    ) = a
        q(i, 1+n/2) = a * (-1._8)**(i-1)
        do j = 2, n/2
            q(i, j    ) = b * cos((i-1)*(j-1)*2*pi/real(n, 8))
            q(i, j+n/2) = b * sin((i-1)*(j-1)*2*pi/real(n, 8))
        enddo
    enddo

    call cpu_time(tic)
    res = matmul(q, w)
    call cpu_time(toc)
    dt = toc - tic
    print *,'matqwr8 n = ', n, ' time = ', dt, ' secs res = ', sum(res)

    ! print *,'res '
    ! do i = 1, 100
    !    print *,res(i)
    ! enddo


end program test
