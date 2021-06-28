subroutine print_error(stat, fname)
    Use MKL_DFTI
    implicit none
    integer, intent(in):: stat
    character(len=*), intent(in) :: fname
    if (stat /= 0) then
        print *,'***ERROR*** stat = ', stat, ' after ', fname
        print *, DftiErrorMessage(stat)
    endif
end subroutine

program test

    ! gfortran -O3 fftr4.f90 -I"${MKLROOT}/include" $MKLROOT/include/mkl_cdft.f90 -o fftr4 -L${MKLROOT}/lib/lp64 -lmkl -lgomp

    Use MKL_DFTI

    implicit none
    integer :: n, stat, i, n1
    character(len=32) :: nstr
    real, allocatable :: fs(:)
    real, allocatable :: res(:)
    real :: dt, tic, toc

    type(dfti_descriptor), pointer :: handle
    complex, allocatable :: fbar(:)

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n
    n1 = n + 1

    allocate(fs(n), fbar(2*n1), res(n))
    do i = 1, n
        fs(i) = i
    enddo
    fbar = 0

    stat = DftiCreateDescriptor(handle, DFTI_SINGLE, DFTI_COMPLEX, 1, 2*n1)
    call print_error(stat, 'DftiCreateDescriptor')

    stat = DftiCommitDescriptor(handle)
    call print_error(stat, 'DftiCommitDescriptor')

    call cpu_time(tic)
    ! load the array
    fbar(2:n1) = fs

    stat = dftiComputeForward(handle, fbar)
    call print_error(stat, 'dftiComputeForward')

    res = -aimag(fbar(2:n1))
    call cpu_time(toc)

    dt = toc - tic
    print *, ' time = ', dt, ' secs res = ', sum(res)

    stat = dftiFreeDescriptor(handle)
    call print_error(stat, 'dftiFreeDescriptor')
end program test
