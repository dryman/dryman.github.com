---
layout: post
title: "Grand central dispatch vs OpenMP"
date: 2012-08-05 12:41
comments: true
categories: 
---

In 2009 Apple released a new task parallelism technology called 
[Grand Central Dispatch (GCD)][gcd]. Apple worked hard on tuning GCD; stated that only 15
instructions are required to queue up a work unit in GCD, while creating a
traditional thread could easily require several hundred instructions.  
The main advantage of GCD is that programmer/user does not required to choose
how many thread for application that optimize the most. It can save a lot of
time for programmers because getting the most power out of CPUs requires a lot
of measurement. With GCD, let the operating system decide it for you, *it just
works.*

<!-- more -->

Most GCD documentation provided by Apple take focus on user applications:
background process, asynchronous callbacks, non-blocking UI, dispatched IOs
...etc. GCD and c/obj-c blocks works pretty good in those scenarios, but we
want a more general comparison between GCD and traditional thread models. **Which
is faster?**
Currently no one has made a general benchmark for this. I targeted to use an
industry standard benchmark for GCD vs threads, and I ended up by picking 
[Conjugate Gradient][cg] computation in 
[NAS Parallel Benchmark (NPB) maintained by NASA][npb] as my benchmark model.

I uses [OpenMP][omp] implementation in CG problem. It is an shared memory
threading API which is much easier to use then POSIX thread. However it is still
required for programmer/user to pick thread number in run time or in compile time.
NASA only provide fortran code, so I uses [Ohio's C implementation][ohio].

## Benchmark result

![NPB CG](/images/npb_cg.png "figure 1")

The result is quite promising! Problem sizes in NPB are predefined and indicated
as different classes: 

* Class W: vector size: 7000, iterations: 15 (90's workstation size, now likely
  too small)
* Class A: vector size: 14000, iterations: 15
* Class B: vector size: 75000, iterations: 75

I tested OpenMP with different thread numbers and it performs differently on
different problem size. It not quite obvious to choose a correct thread number
for the problem, and GCD implementations beats them all.

## Bottleneck implementation

The OpenMP implementation looks like this:

{% codeblock lang:c %}
    #pragma omp parallel {  // Spawn threads for all computations in the block
    /* 
      some other code... 
    */

    #pragma omp for private(i,k)  // Bottleneck
      for (j = 1; j <= lastrow-firstrow+1; j++) {
        int iresidue;
        double sum1, sum2;
        i = rowstr[j]; 
        iresidue = (rowstr[j+1]-i) % 2;
        sum1 = 0.0;
        sum2 = 0.0;
        if (iresidue == 1) sum1 = sum1 + a[i]*p[colidx[i]];
        for (k = i+iresidue; k <= rowstr[j+1]-2; k += 2) {
          sum1 = sum1 + a[k]   * p[colidx[k]];
          sum2 = sum2 + a[k+1] * p[colidx[k+1]];
        }
          q[j] = sum1 + sum2;
      }

      /* more code */
      } /* end parallel */
{% endcodeblock %}

Other code instead of bottleneck are basically vector initialization, copy,
multiply and norm computations. I tested all of these, but they don't make
big differences between OpenMP, GCD, and BLAS1 functions.

GCD implementation looks much like the original code:

{% codeblock lang:c %}
      // c_queue is a concurrent queue
      dispatch_apply (NA, c_queue, ^(size_t idx){ 
        size_t j = idx+1;
        double sum = 0.0;
        double sum1 = 0.0, sum2 = 0.0;
        size_t i = rowstr[j];
        size_t iresidue = (rowstr[j+1]-i) %2; 
        if (iresidue == 1) sum1 = sum1 + a[i]*p[colidx[i]];
        for (size_t k = i+iresidue; k <= rowstr[j+1]-2; k += 2) {
          sum1 = sum1 + a[k]   * p[colidx[k]];
          sum2 = sum2 + a[k+1] * p[colidx[k+1]];
        }   
        q[j] = sum1 + sum2;
      }); 
{% endcodeblock %}

What a great news! It is much easier then I thought to transfer the original
code into GCD.

## Parallel reduction in OpenMP, GCD, and BLAS

As I concluded before, it doesn't make big difference between three of these.
The implementations are:

{% codeblock OpenMP lang:c %}
    #pragma omp parallel private (i,j,k)
    {
    #pragma omp single nowait
      rho = 0.0;
    #pragma omp for reduction(+:rho)
      for (j=1; j < NA; j++) {
        rho = rho + x[j]*x[j];
      }
    }
{% endcodeblock %}

{% codeblock GCD lang:c %}
    /* clang -fblocks -O3 -DCACHE_LINE_SIZE=$(shell sysctl -n hw.cachelinesize) */
    #include <dispatch/dispatch.h>
    #define STRIDE (1024*(CACHE_LINE_SIZE/sizeof(double)))
    #define DIVIDE (NA/STRIDE)
    #define RESIDUE (NA%STRIDE)

    __block double rho = 0.0;
    dispatch_async(s_queue,^{
      for (size_t j = DIVIDE*STRIDE+1; j < NA+1; j++){
        rho += x[j]*x[j];
      }
    });
    dispatch_apply(DIVIDE, c_queue, ^(size_t idx){
      size_t j = idx * STRIDE+1;
      size_t j_e = j + STRIDE;
      double sum=0.0;
      do {
        sum += x[j]*x[j];
      } while (++j < j_e);
      dispatch_sync(s_queue,^{
        rho += sum;
      });
    }); 
{% endcodeblock %}

{% codeblock CBLAS lang:c %}
    /* clang -lcblas 
       when linking */
    #include <Accelerate/Accelerate.h> // header for OSX

    // rho = x.x
    //               size, vectorX,  incX, vectorY, incY
    rho = cblas_ddot(  NA, &(x[1]),  1  ,  &(x[1]),    1);
{% endcodeblock %}

I think it does not make difference because those operations are all one
dimensional BLAS1 problems.

#### Note on cache line size

I thought that cache line size matters when I start implementing GCD version of
parallel reduction. But it ended up that you just need to give it a large enough
size for compiler to generate SIMD optimization. Note that you can get the CPU
cache line size by command `sysctl -n hw.cachelinesize` from shell.

## Conclusion

I think the best practice so far is to use BLAS whenever you can. It's cleaner
and highly optimized by libraries developed by Apple, Intel, or other HPC
foundations. For other operation that BLAS don't support, GCD is a good choice
and easy to migrate to. The pros and cons go as follows:

#### OpenMP over GCD

* Supported by gcc, while clang doesn't.
* Can be used on C, CPP, and Fortran (and many more?)
* Many existing numerical code uses OpenMP
* Easier to get start with. eg. `#omp parallel for`
* Better syntax in reduction: `#omp reduction (+:sum)`

#### GCD over OpenMP

* Much easier to tune performance.
* Tighter logic construction. Everything is encapsulated in blocks.
* No separated thread spawns and private variables like OpenMP.
* Much less parameters to adjust in compile time and run time.
* Highly optimized in any kinks of problem sizes.
* Works on iOS (OpenMP does not)

I think the greatest advantage you can gain from GCD is that it is highly optimized 
on different problem sizes, because operating system takeovers task load balancing. 
It surprised me that on class W problem, OpenMP version with 16 threads is twice
as slow compares to 1 thread version. With GCD, you no longer need to handle
this unexpected result! Cheers.

[gcd]: http://developer.apple.com/library/ios/#documentation/Performance/Reference/GCD_libdispatch_Ref/Reference/reference.html
[npb]: http://www.nas.nasa.gov/publications/npb.html
[cg]: http://en.wikipedia.org/wiki/Conjugate_gradient_method
[omp]: http://openmp.org/wp/
[ohio]: http://jason.cse.ohio-state.edu/examples/npb/index.html
