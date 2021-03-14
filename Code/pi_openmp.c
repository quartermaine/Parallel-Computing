#include <stdio.h>
#include <omp.h>
#define PI 3.14159265358979323846264

void func(int iterations, int num_threads)
{
    const double m = 1.0 / (double)iterations;

    double sum = 0.0;
    omp_set_num_threads(num_threads);

    double start_time = omp_get_wtime();
    #pragma omp parallel for reduction(+ : sum)

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
    /*printf("Iterarions: 96000000 --------- Threads: 48 ------- \n");*/
    for (int i = 0; i < 5000; ++i)
    {
        /*printf("iteration: %d: \n", i + 1);*/
        func(96000000, 48);
        printf("\n");
    }
    /*printf("=================================================\n");*/
}
