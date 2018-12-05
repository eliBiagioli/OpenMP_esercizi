#include <stdio.h>
#include "omp.h"

#define NUM_THREADS 4
int main(void)
{

  int v[20];
  omp_set_num_threads(NUM_THREADS);
  
  #pragma omp parallel
  {
    int i, ID, nthreads;
    ID = omp_get_thread_num();
    nthreads = omp_get_num_threads();

//    #pragma omp for
    for (i=ID; i<20; i+= nthreads) {
      v[i]=i;
      printf("v[%d] = %d, ID = %d\n",i, v[i], ID);
    }
    #pragma omp barrier
//    #pragma omp for
    for (i=ID; i<20; i+=nthreads) {
      v[i] ++;
      printf("v[%d] = %d, ID = %d\n",i, v[i], ID);
    }
   }  
  
  return 0;
}
