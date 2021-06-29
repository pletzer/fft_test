program test
    ! gfortran -O3 matr4.f90 -o matr4
    implicit none
    integer :: n, stat, i, j
    character(len=32) :: nstr
    real, allocatable :: q(:,:), w(:), res(:)
    real :: dt, tic, toc
    real, parameter :: pi = 4*atan(1.)

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n

    allocate(q(n, n), w(n), res(n))

    do i = 1, n
        w(i) = i
    enddo

    do i = 1, n
        q(i, 1    ) = 0
        q(i, 1+n/2) = 0
        do j = 2, n/2
            q(i, j    ) = cos((i-1)*(j-1)*2*pi/real(n, 4))
            q(i, j+n/2) = sin((i-1)*(j-1)*2*pi/real(n, 4))
        enddo
    enddo

    call cpu_time(tic)
    res = matmul(q, w)
    call cpu_time(toc)
    dt = toc - tic
    print *,'matqwr4 n = ', n, ' time = ', dt, ' secs res = ', sum(res)

    ! print *,'res '
    ! do i = 1, n
    !    print *,res(i)
    ! enddo


end program test
