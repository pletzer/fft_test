program test
    ! gfortran -O3 matr8.f90 -o matr8 -fopenmp
    use omp_lib
    implicit none
    integer :: n, stat, i, j
    character(len=32) :: nstr
    real(8), allocatable :: fs(:), sines(:, :), res(:)
    real(8) :: dt, tic, toc
    real(8), parameter :: pi = 4*atan(1.)

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

    tic = omp_get_wtime()
    res = matmul(sines, fs)
    toc = omp_get_wtime()
    dt = toc - tic
    print *,'matr8 n = ', n, ' time = ', dt, ' secs res = ', sum(res)
end program test
