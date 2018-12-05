program prova
  implicit none
  
  integer :: n = 100
  print *, 'Before parallel section n = ', n
  !$OMP PARALLEL private(n)
  n = n+1
  write(*,*) 'Hello world! Inside the parallel section n = ', n
  !$OMP END PARALLEL
  print *, 'After parallel section n = ', n

end program prova
