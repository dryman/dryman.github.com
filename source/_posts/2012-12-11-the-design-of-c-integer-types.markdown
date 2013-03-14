---
layout: post
title: "Deep C: Understanding The Design of C Integer Types"
date: 2012-12-03 15:32
comments: true
published: true 
categories: C
---

C is a popular language designed for cross platform development. However, when
you dig deeper and deeper, you might get confused for the ambiguity of C
integer types. Take `char` for example, number of bits can be 8, 9, or more; the
minimum of a signed char is not strictly defined as -128 but *-127 or less*.

<div class="table" markdown="1">

|   name        |    express                                        |   value       |
|:------:       |:-------------:                                    |:-------------:|
| `CHAR_BIT`    | Number of bits for a char object (byte)           |8 or greater   |
| `SCHAR_MIN`   | Minimum value for an object of type signed char   | -127 or less  |
| `SCHAR_MAX`   | Maximum value for an object of type signed char   | 127 or more   |
| `UChar_MAX`   | Maximum value for an object of type unsigned char | 255 or more   |

</div>

Why does C is designed like so? In this article I'll discuss the design and the
sprits of C.

<!--more-->

# C type system

Before we dig into C's integer types, we need to understand what a *type*
actually means to C language.

> The meaning of a value stored in an object or returned by a function is
> determined by the *type* of the expression used to access it.
> <footer><cite>C99 6.2.5</cite></footer>

In other words, a region of data storage (specified as *object* in C99) is
treated as some kinds of human readable value via the type system.

## Float

Take `float` for example. A float number `1.0` is stored as `0x3f800000` on my
intel mac. `0x3f800000` can be a unsigned int, int, long, or other self defined
struct type as well. If we treated it as float, we use floating point register to
operate it; If we use other type, the register may be different, the arithmetic
operation may be different, too.

```c
/* cc -std=c99 -Wall play_float.c -o play_float */
#include <stdio.h>

int main (void)
{
    unsigned int object = 0;
    object |= 127 << 23; 

    float* force_float = (float*)&object;

    printf("stored object is 0x%08x, float representation is %f\n", object, *force_float);

    (*force_float)++;
    printf("++\n");

    printf("stored object is 0x%08x, float representation is %f\n", object, *force_float);

    return 0;
}
```

The output would be:

```
stored object is 0x3f800000, float representation is 1.000000
++
stored object is 0x40000000, float representation is 2.000000
```


[Ridiculous fish][fish] has a great article 
[about floating point representation][float]. That is a really interesting
article. I can't explain better than he does.

## Integer promotion

C integer promotion is another example of same data storage, but different
arithmetic operation depend on type. On x86 machine, when you do arithmetic
operations (+, -, /, ==, etc.) on a `signed short`, it is *promoted* to an 
`int` with extended signed bit. 

```c
short a = 0xffff; // 0xffff on disk space

/* 
 * loaded into register as 0xffffffff 
 * calculated as 0xffffffff + 0x00000001
 * then store back to disk with possible 
 * truncation
 */
a++;              
```

If you use `unsigned short` instead of `signed short` the arithmetic operation
would be different! It will loaded into register without signed bit. 
A unsigned short with `0xffff` stored on disk, would be `0x0000ffff` in
register. So, if you have a code like this:

```c
short          a = 0xffff;
unsigned short b = 0xffff;

printf("%s\n", a==b? "equal" : "not equal");
/* 
 * not equal, because one is 0xffffffff and 
 * another is 0x0000ffff in register
 */
```

It will not equal, even if they have the same disk storage! For further
discussion see my previous post [Deep C: Integer Promotion][promotion].

# The spirit of C

Some of the facets of the spirit of C can be summarized in phrases like:

> * Trust the programmer.
> * Don't prevent the programmer from doing what needs to be done.
> * Keep the language small and simple.
> * Make it fast, even if it is not guaranteed to be portable.
>
> The last proverb needs a little explanation. The potential for efficient code
> generation is one of the most important strengths of C. To help ensure that no
> code explosion occurs for what appears to be a very simple operation, many
> operations are defined to be how the target  machine's hardware does it rather
> than by a general abstract rule.
> <footer><cite>C99 Rationale v5.10 page 3</cite></footer>

When you look at C spec, you should remember that C is designed *to run fast on
target machine*, not designed *for simplicity of abstract machine*. This design
goal directly affect the spec of C's type system, since C's type system is the
rule of how machine arithmetic logic unit operates on data.

## int

`int` is a special type, it is defined to be **the fastest implementation to
represent an integer**. It is mostly implemented to be the fastest register on
the machine. For 16-bit machine, it is 16-bit; for 24-bit machine, it is 24-bit;
for 32-bit machine, it is 32-bit; for 64-bit machine, well, the fastest register
is 32-bit, so the most seen implementation is 32-bit.

The size of `int` is also the size that integer promotion promotes to.
On 32-bit machine, smaller integers like 8-bit and 16-bit will promote to 32-bit
register when need to do calculations. On 16-bit machine, 16-bit integers don't
do promotion, but 8-bit integers do.

## Size of integer types in `limit.h`

The numerical limits were, and still are, presented as *minimum maxima*. That is,
lower limit defined in SPEC, upper limits specified by the implementation. For
example, the minimum of a `signed short` is `-128` on modern two's complement
machine. But on an one's complement or sign-magnitude machine, the minimum value
of `signed short` can only be `-127`. Some of the limits are listed below:

<div class="table" markdown="1">

|  name           |    expresses                                                          |         value                       |
|:--------------- |:--------------------------------------------------------------------- |:----------------------------------- |
| CHAR_BIT        |Number of bits for a char object (byte) 8 or greater                   |                                     |
| SCHAR_MIN       |Minimum value for an object of type signed char                        |   -127 or less                      |
| SCHAR_MAX       |Maximum value for an object of type signed char                        |   127 or more                       |
| UCHAR_MAX       |Maximum value for an object of type unsigned char                      |   255 or more                       |
| CHAR_MIN        |Minimum value for an object of type char                               |   either SCHAR_MIN or 0             |
| CHAR_MAX        |Maximum value for an object of type char                               |   either SCHAR_MAX or UCHAR_MAX     |
| SHRT_MIN        |Minimum value for an object of type short int                          |   -32767 or less                    |
| SHRT_MAX        |Maximum value for an object of type short int                          |   32767 or greater                  |
| USHRT_MAX       |Maximum value for an object of type unsigned short int                 |   65535 or greater                  |
| LONG_MIN        |Minimum value for an object of type long int                           |   -2147483647 or less               |
| LONG_MAX        |Maximum value for an object of type long int                           |   2147483647 or greater             |
| ULONG_MAX       |Maximum value for an object of type unsigned long int                  |   4294967295 or greater             |

</div>

# Summary

The spirit of C targets on program speed instead of consistency of abstract
machine across different platforms. It makes us easy to write programs that run
fast for free, but the programmer have to take care to make the program
**safe**. For calculations that is sensitive to data limits, one should use
unambiguous type specifications like `int8_t`, `int32_t` specified in
`inttypes.h`, and check the bounds with `limits.h` and static analyzers.

# See also

* [C99 specifications][c99]
* [C99 rationale][c99]
* [The UNIX system -- 64bit and Data Size Neutrality][unix]
* [Ridiculous fish's article on float][float]
* [C Integer Promotion][promotion]

[fish]: http://ridiculousfish.com
[float]: http://ridiculousfish.com/blog/posts/float.html
[promotion]: http://www.idryman.org/blog/2012/11/21/integer-promotion/
[unix]: http://www.unix.org/whitepapers/64bit.html
[c99]: http://www.open-std.org/jtc1/sc22/wg14
