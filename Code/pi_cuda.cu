#include "cuda_runtime.h"
#include <stdio.h>

#define ITERATIONS 96000000
const int threads = 1024; // nVidia's optimal block size should be muptiple of 32

// Synchronous error checking call. Enable with nvcc -DDEBUG
inline void checkCUDAError(const char *fileName, const int line)
{ 
    #ifdef DEBUG
        cudaThreadSynchronize();
        cudaError_t error = cudaGetLastError();
        if(error != cudaSuccess){
        printf("Error at %s: line %i: %s\n", fileName, line, cudaGetErrorString(error));
        exit(-1);
        }
    #endif
}
__global__ void integrateSimple(float *sum)
{
    __shared__ float ssums[threads];
    // Each thread computes its own sum. 
    int global_idx = threadIdx.x + blockIdx.x * blockDim.x;
    if(global_idx < ITERATIONS){
        float step = 1.0f / ITERATIONS;
        float x = (global_idx + 0.5f) * step;
        ssums[threadIdx.x] = 4.0f / (1.0f + x * x);
    }
    else{
    ssums[threadIdx.x] = 0.0f;
    }
    // The 1st thread will gather all sums from all other threads of this block into one
	 __syncthreads();
    if(threadIdx.x == 0)
    {
    float local_sum = 0.0f;
        for(int i = 0; i < threads; ++i)
        {
        local_sum += ssums[i];
        }
        sum[blockIdx.x] = local_sum;
        }
}
int main()
{
    const float PI = 3.14159265358979323846264;
    int deviceCount = 0;
    cudaError_t error = cudaGetDeviceCount(&deviceCount);
    if (error != cudaSuccess)
    {
        printf("cudaGetDeviceCount returned %d\n-> %s\n", (int)error, cudaGetErrorString(error));
        return 1;
    }
    deviceCount == 0 ? printf("There are no available CUDA device(s)\n") : 
                                                printf("%d CUDA Capable device(s) detected\n" , deviceCount);
    /*--------- Simple Kernel ---------*/
    int blocks = (ITERATIONS + threads - 1) / threads;
    float *sum_d;
    float step = 1.0f / ITERATIONS;

	for (int i = 0; i < 5000; ++i)
    {   // Allocate device memory
        cudaMallocManaged((void **)&sum_d, blocks * sizeof(float));
        // CUDA events needed to measure execution time
        cudaEvent_t start, stop;
        float gpuTime;
        // Start timer
        cudaEventCreate(&start);
        cudaEventCreate(&stop);
        cudaEventRecord(start, 0);
        // calculate pi 
        integrateSimple<<<blocks, threads>>>(sum_d);
        cudaDeviceSynchronize(); // wait until the kernel execution is completed
        checkCUDAError(__FILE__, __LINE__);
        // Stop timer
        cudaEventRecord(stop, 0);
        cudaEventSynchronize(stop);
        cudaEventElapsedTime(&gpuTime, start, stop);
        // Sum result on host
        float MyPI = 0.0f;
        for (int i = 0; i < blocks; i++)
        {
            MyPI += sum_d[i];
        }
        MyPI *= step;
        cudaFree(sum_d);
        /*printf("\n======================================\n\n");*/
        printf("%.23lf,%.23lf,%f", MyPI, fabs(MyPI - PI), gpuTime/1000);
        printf("\n");
    }
    // Reset Device
    cudaDeviceReset();
    return 0;
}


