program prova2
  use omp_lib
  implicit none
  
  
  integer :: ID ! = omp_get_thrad_num()
  integer :: num_threads = 4
!  !$ call omp_set_num_threads(num_threads)
  !$OMP PARALLEL
  !$omp critical
  !$ ID = omp_get_thread_num()
  write(*,*) 'Hello world! ID = ', ID
  !$omp end critical
  !$OMP END PARALLEL


end program prova2
