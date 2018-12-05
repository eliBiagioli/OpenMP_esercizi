#include <stdio.h>

int main(void)
{
  #pragma omp parallel
  {
    int ID = omp_get_thread_num();
    printf("ID = %d\n",ID);
    printf("Hello (%d)", ID);
    printf(" world (%d)\n", ID);
  }
  return 0;
}
