#include <stdio.h>
#include "omp.h"
// Calcolo dell'integrale 
//     1     4
//  int   --------- dx  = pi
//     0   1 + x^2

static long num_steps = 100000;
double step, time1, time2;
#define PAD 8  // assume 64 byte L1 cache line size
#define NUM_THREADS 4
void main(void)
{
  int i, num_threads;
  double pi, sum[NUM_THREADS][PAD];
  step = 1.0/(double) num_steps;
  omp_set_num_threads(NUM_THREADS);

  time1 = omp_get_wtime();
  #pragma omp parallel
  {
    int i, ID, nthreads;
    double x;
    ID = omp_get_thread_num();
    nthreads = omp_get_num_threads();
    if(ID==0) num_threads = nthreads;

    for (i=ID, sum[ID][0]=0.0; i<num_steps; i+=nthreads) {
      x=(i+0.5)*step;
      sum[ID][0] += 4.0/(1.0+x*x);
    }  
  }

  for (i=0, pi=0.0; i<num_threads; i++)
    pi += step*sum[i][0];
  time2 = omp_get_wtime();
  printf("pi = %f, time = %f\n",pi,time2-time1);
  
}

 
