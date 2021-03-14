#include <stdio.h>
#include <omp.h>
#define PI 3.14159265358979323846264

void func(int iterations)
{
    const double m = 1.0 / (double)iterations;
    double sum = 0.0;
    
    double start_time = omp_get_wtime();

    for (int i = 0; i < iterations; i++)
    {
        double ni = (i + 0.5) * m;
        sum = sum + 4.0 / (1.0 + ni * ni);
    }
    double MyPI = m * sum;
    double run_time = omp_get_wtime() - start_time;
    double error = MyPI - PI;
    printf("%.23lf,%.23lf,%f", MyPI, error, run_time);
}

void main()
{
    for (int i = 0; i < 5000; ++i)
    {
        func(24000000); // change # iterations
        printf("\n");
    }
}


