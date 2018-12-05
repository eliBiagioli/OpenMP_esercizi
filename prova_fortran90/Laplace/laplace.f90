program laplace
   implicit none
   
   integer, parameter                 :: dp=kind(1.d0)
   integer                            :: n, maxIter, i, j, iter = 0
   real (dp), dimension(:,:), pointer :: T, Tnew, Tmp=>null()
   real (dp)                          :: tol, var = 1.d0, top = 100.d0
   real                               :: starttime, endtime
   integer                            :: ierr
    
   write(*,*) 'Enter mesh size, max iterations and tollerance:'
   read(*,*,iostat=ierr)  n, maxIter, tol
   if(ierr /= 0)  STOP 'Input error!'

   call cpu_time(startTime)

   allocate (T(0:n+1,0:n+1), Tnew(0:n+1,0:n+1),stat=ierr)
   if(ierr/=0) STOP 'T Tnew matrix allocation failed'

   T(0:n,0:n) = 0.d0

   T(n+1,1:n) = (/ (i, i=1,n) /) * (top / (n+1))
    
   T(1:n,n+1) = (/ (i, i=1,n) /) * (top / (n+1))

   Tnew = T

   do while (var > tol .and. iter <= maxIter)
      iter = iter + 1
      var = 0.d0       
      do j = 1, n
         do i = 1, n
            Tnew(i,j) = 0.25d0 * ( T(i-1,j) + T(i+1,j) + T(i,j-1) + T(i,j+1) )
            var = max(var, abs( Tnew(i,j) - T(i,j) ))
         end do
      end do
 
      Tmp =>T; T =>Tnew; Tnew => Tmp; 

      if( mod(iter,100) == 0 ) write(*,"(a,i8,e12.4)") &
         ' iter, variation:', iter, var

   end do

   call cpu_time(endTime)
   write(*,'(/A,F10.4)') ' Elapsed time (s)     =', endTime - startTime
   write(*,*) 'Mesh size            =', n
   write(*,*) 'Stopped at iteration =', iter
   write(*,*) 'The maximum error    =', var
    
   open(10, file='results', action='write', iostat=ierr)
   if(ierr /= 0)  STOP 'Error in opening output file!'
   write(10, "(i8, i8, e18.9)") (( i, j, T(i,j), i=1,n), j=1,n)
   close(10)

   deallocate (T, Tnew)
   nullify(Tmp)

end program laplace
