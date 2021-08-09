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
    real(8), allocatable :: w(:), res(:)
    complex*16, allocatable :: wbar(:)
    type(dfti_descriptor), pointer :: handle
    real(8), parameter :: pi = 4._8*atan(1._8)
    real(8) :: a, b
    real :: dt, tic, toc

    if (command_argument_count() < 1) then
        stop 'ERROR must supply array size as command line argument'
    endif
    call get_command_argument(1, nstr)
    read(nstr, *, iostat=stat) n

    stat = DftiCreateDescriptor(handle, DFTI_DOUBLE, DFTI_COMPLEX, 1, n)
    call print_error(stat, 'DftiCreateDescriptor')

    stat = DftiCommitDescriptor(handle)
    call print_error(stat, 'DftiCommitDescriptor')

    allocate(wbar(n), w(n), res(n))

    do i = 1, n
        w(i) = i
    enddo

    a = sqrt(1._8/n)
    b = sqrt(2._8/n)
    call cpu_time(tic)
    wbar(1    ) = a * w(1)
    wbar(1+n/2) = a * w(1 + n/2)
    do j = 2, n/2
        wbar(j    ) = b*( w(j) + cmplx(0._8, 1._8)*w(j+n/2) )
        wbar(j+n/2) = 0
    enddo
    stat = dftiComputeForward(handle, wbar)
    call print_error(stat, 'dftiComputeForward')
    res = real(wbar, 8)
    call cpu_time(toc)
    dt = toc - tic

    print *,'fftqwr8 n = ', n, ' time = ', dt, ' secs res = ', sum(res)

    stat = dftiFreeDescriptor(handle)
    call print_error(stat, 'dftiFreeDescriptor')

    ! print *,'res '
    ! do i = 1, 100 
    !    print *,res(i)
    ! enddo

end program test
