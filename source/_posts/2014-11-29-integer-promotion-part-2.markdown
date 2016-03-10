---
layout: post
title: "Integer Promotion Part 2"
date: 2014-11-29 18:34
comments: true
categories: C
---

It's been a while since I wrote my last article. I recently read an old book [Expert C Programming](http://www.amazon.com/Expert-Programming-Peter-van-Linden/dp/0131774298) and found there are many C langauge details that I never think about. Review and rethink what C integer promotion rules meant to C is one of the interesting topic that I want to share with you.

<!--more-->

## What is type systems?

Type is the methods that interpret and process your data.
Sounds familiar to OOP? Definiing a class **IS** defining a method.
In C, expressions on different data type produces different
instructions. What instruction to generate? Depends on the data
type. (int is different than float, for example).

A chunk of data in memory or register can mean anything
on a CPU. The type system defines how operators works on different types.
Given the example below:


```c
int* p = 0x00007ffffffffcc0;
int  i = 1;
int* q = p + i; // => 0x00007fffffffffcc4 because the type "pointer to int"
                // defines the multiplier of the offset is 4

int* r = p + q; // Invalid in C type system.
```

When adding an integer to a pointer, the compiler knows the offset multiplier
by the type of the pointer points to. If `p` where defined as `char* p`, then
the multiplier would be 1. Also, the `+` operator only make sense to add an
offset to a pointer, therefore adding two pointer together is invalid in C
syntax.

## Review C integer promotion rules

I wrote an article about C integer promotion 2 years ago: [Deep C: Integer Promotion](http://www.idryman.org/blog/2012/11/21/integer-promotion/). The standard integer promotion rules in C99 is listed below.

> If an int can represent all values of the original type, the value is converted
> to an int; otherwise, it is converted to an unsigned int. These are called the
> integer promotions. All other types are unchanged by the integer promotions.

This is called *value preserving*. On machine level, the arithmetic works best on
a whole register (or 32 bit register). Remember that C's philosophy is to make
your program runs as fast as possible, so it loads your small data chunk into
the whole register, then do the calculation.

However, things worked differently on K&R C. It's was defined as *unsigned
preserving*. When mixing a signed and unsigned integer, the result is unsigned
type. Below is the example copied from **Expert C Programming** book.

```c
if ( -1 < (unsigned char) 1 ) {
  printf("C dialect after ANSI C, including C89, C99, and after\n");
} else {
  printf("Pre ANSI C (K&R) dialect. -1 is treated as unsigned value 0xFFFFFFFF and thus greater than 1 ! \n");
}
```

## Mixing unsigned and signed integer in modern C

Although most of the time C will preserve the value of the type for you,
you can still get surprising result. When the signed integer can't hold
the unsigned value, everything is converted to unsigned integer.

```c
  if ( -1 < sizeof(int) ) { 
    printf("-1 is less than 4\n");
  } else {
    printf("-1 is treated as 0xffffffff. Because sizeof returns size_t which is an unsigned int\n");
  }
  return 0;
```

This program will execute the **else** branch and print:

```
-1 is treated as 0xffffffff. Because sizeof returns size_t which is an unsigned int
```

## Conclusion

Always be careful when you mix unsigned and signed integer in expressions.
When possible, type cast it to signed int. C is designed to make program fast,
but not safe for beginners. Additional checks would create a lot of overhead
in the runtime, so the developer need to know the system and language well
to avoid shooting at their own foot. Another possible reason may be that 
the original C developers were all operating system writers and compiler 
writers. Therefore, the mappings between Assembly and C langauge is
straight forward to them.
