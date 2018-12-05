!---------------------------------------------c
!  Exercise: Pi                               c
!                                             c
!  Compute the value of PI using the integral c
!  pi = 4* int 1/(1+x*x)    x in [0-1]        c
!                                             c
!  The integral is approximated by a sum of   c
!  n interval.                                c
!                                             c
!  The approximation to the integral in each  c
!  interval is: (1/n)*4/(1+x*x).              c
!---------------------------------------------c
program pigreco
    implicit none

    integer(selected_int_kind(18)) :: i 
    integer(selected_int_kind(18)), parameter :: intervals=1e8
    ! 'intervals' := numero di intervalli in cui divido [0,1]

    real(kind(1.d0)) :: dx,sum,x
    real(kind(1.d0)) :: f,pi

    real(kind(1.d0)), parameter :: PI25DT = acos(-1.d0)
    real :: time1, time2

    call cpu_time(time1)

    print *, 'Number of intervals: ', intervals
    sum=0.d0
    dx=1.d0/intervals
    
    !$omp parallel do private (x,f) reduction (+:sum)
    do i=1,intervals
        x=dx*(i-0.5d0)
        f=4.d0/(1.d0+x*x)
        sum=sum+f
    end do
    !$omp end parallel do

    pi=dx*sum

    call cpu_time(time2)

    PRINT '(a13,2x,f30.25)',' Computed PI =', pi
    PRINT '(a13,2x,f30.25)',' The True PI =', PI25DT
    PRINT *, ' '
    PRINT *, 'Elapsed time ', time2-time1 ,' s' 

end program
