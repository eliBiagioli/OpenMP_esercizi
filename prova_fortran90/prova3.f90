program prova2
  use omp_lib
  implicit none
  
  integer :: v(0:19), i, ID
  integer :: NUM_THREADS = 4

  !$ call omp_set_num_threads(NUM_THREADS)

  !$OMP PARALLEL
    !$omp critical
    ID = omp_get_thread_num()
    do i=ID,20,+NUM_THREADS
      v(i) = i
      write(*,*) 'v(', i, ')=', v(i), 'ID=', ID
    end do
    !$omp end critical
    !$omp barrier
    !$omp critical
    do i=ID,20,+NUM_THREADS
      v(i) = v(i) + 1
      write(*,*) 'v(', i, ')=', v(i), 'ID=', ID
    end do
    !$omp end critical
    
  !$OMP END PARALLEL


end program prova2
