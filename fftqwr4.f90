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
    use mkl_dfti
    implicit none
    integer :: n, stat, i, j
    character(len=32) :: nstr
    real, allocatable :: w(:), res(:)
    complex, allocatable :: wbar(:)
    type(dfti_descriptor), pointer :: handle
    real :: dt, tic, toc
    real, parameter :: pi = 4*atan(1.)

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n

    stat = DftiCreateDescriptor(handle, DFTI_SINGLE, DFTI_COMPLEX, 1, n)
    call print_error(stat, 'DftiCreateDescriptor')

    stat = DftiCommitDescriptor(handle)
    call print_error(stat, 'DftiCommitDescriptor')

    allocate(wbar(n), w(n), res(n))

    do i = 1, n
        w(i) = i
    enddo

    call cpu_time(tic)
    wbar(1    ) = 0.
    wbar(1+n/2) = 0.
    do j = 2, n/2
        wbar(j    ) = w(j) + cmplx(0., 1.)*w(j+n/2)
        wbar(j+n/2) = 0
    enddo
    stat = dftiComputeForward(handle, wbar)
    call print_error(stat, 'dftiComputeForward')
    res = real(wbar, 4)
    call cpu_time(toc)
    dt = toc - tic

    print *,'fftqwr4 n = ', n, ' time = ', dt, ' secs res = ', sum(res)

    stat = dftiFreeDescriptor(handle)
    call print_error(stat, 'dftiFreeDescriptor')

    ! print *,'res '
    ! do i = 1, n
    !    print *,res(i)
    ! enddo

end program test
