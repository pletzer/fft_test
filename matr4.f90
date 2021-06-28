program test
    ! gfortran -O3 matr4.f90 -o matr4
    implicit none
    integer :: n, stat, i, j
    character(len=32) :: nstr
    real, allocatable :: fs(:), sines(:, :), res(:)
    real :: dt, tic, toc
    real, parameter :: pi = 4*atan(1.)

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n

    allocate(fs(n), sines(n, n), res(n))
    do j = 1, n
        fs(j) = j
        do i = 1, n
            sines(i, j) = sin(i*j*pi/real(n + 1, 8))
        enddo
    enddo

    call cpu_time(tic)
    res = matmul(sines, fs)
    call cpu_time(toc)
    dt = toc - tic
    print *,'res = ', sum(res), ' time = ', dt, ' secs'
end program test
