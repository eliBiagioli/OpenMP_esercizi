Program matrix_matrix_prod
   implicit none
   
   integer :: n
   real(kind(1.d0)), dimension(:,:), allocatable   :: a, b, c
   real(kind(1.d0)) :: d
   integer            :: i, j, k, ierr
   character(len=128) :: command
   character(len=80) :: arg

   ! in input è richiesto di passare la dimensione della matrice 
   ! che si desidera, e l'inout deve essere scritto quando si 
   ! lancia l'esecuzione del programma
   call get_command_argument(0,command)
   if (command_argument_count() /= 1) then
     write(0,*) 'Usage:', trim(command), '   matrix size'
     stop
   else
     call get_command_argument(1,arg)
     read(arg,*) n
   endif
   if (n > 0 ) then
     write(*,*) 'Matrix size is ', n
   else
     write(0,*) "Error, matrix size is ", n
   endif

   allocate(a(n,n),b(n,n),c(n,n),stat=ierr)
   ! controllo sulla regolare allocazione delle matrici
   if(ierr/=0) STOP 'a,b,c matrix allocation failed'

   call random_number(a)
   call random_number(b)
   c = 0.d0

   ! 'c' contiene il risultato del prodotto 'matrice x matrice': 'a x b'
   !$omp parallel do collapse (3) schedule (dynamic, 1)
   do j=1, n
      do i=1, n
         do k=1, n
            c(i,j) = c(i,j) + a(i,k)*b(k,j)
         end do
      end do
   end do
   !$omp end parallel do

   call random_number(d)
   i = int( d*n+1)
   call random_number(d)
   j = int( d*n+1)
   d = 0.d0
   do k=1, n
         d = d + a(i,k)*b(k,j)
   end do

   write(*,*) "Check on a random element:" , abs(d-c(i,j))

end program matrix_matrix_prod
