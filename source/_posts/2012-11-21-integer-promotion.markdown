---
layout: post
title: "Deep C: Integer Promotion"
date: 2012-11-21 14:15
comments: true
categories: C
---

Almost every programmer has learned about C, and a lot of them use it for their
career. It is certainly one of the most popular programming languages on
[TIOBE][TIOBE] (first place in November 2012). Yet, C can be really tricky and
behave unexpectedly sometimes. One of those dodgy side of C is **integer
promotion**. See the following example that illustrate the issue:

{% codeblock lang:c %}
#include <stdio.h>

int main(void)
{
    unsigned char a = 0xff;
    char b = 0xff;
    int c = a==b; // true, or false?
    printf("C: %d\n",c);
}
{% endcodeblock %}

You might think the output is `1`, yet the answer is `0`. Oops.

<!--more-->

## C99 SPEC

In the prior implementation of K&amp;R and C89, arithmetic operands on `short`
and `char` fell into two major camps and may produce different results from the
above C snippet. In C99, integer promotion is clearly defined in following rule
(6.3.1.1):

> If an int can represent all values of the original type, the value is converted
> to an int; otherwise, it is converted to an unsigned int. These are called the
> integer promotions. All other types are unchanged by the integer promotions.

Recall that the range of integer types:

* signed char: -127 to 127
* unsigned char: 0 to 255
* signed short: -32767 to 32767
* unsigned short: 0 to 65535
* signed int: -2147483647 to 2147483647

You can see that signed and unsigned char, short all can be represented in
signed int, so they are all converted to signed int when doing arithmetic
operations.

In the previous example, `unsigned char a = 0xff` is presenting *255*. However,
`char b = 0xff` is presenting *-1*. When both converted to int type, `a`
remains *255*, or `0x000000ff`; `b` will be `0xffffffff` which is *-1*
represented in int type. You can see how it works in this C snippet:

{% codeblock lang:c %}
#include <stdio.h>

int main(void)
{
    unsigned char a = 0xff;
    char b = 0xff;
    printf("A: %08x, B: %08x\n", a, b); 
    return 0;
}
{% endcodeblock %}

The output would be:

    A: 000000ff, B: ffffffff

This is why the result of expression `a==b` is `0`.

## Understand it at assembly level

When I first understood integer promotion rules, I got even more confused: why
is this rule so awkward? To understand why it is designed like so, you must dig
into compiled assembly code.

Let's start with an easy example:

{% codeblock lang:c %}
int main(void)
{
    unsigned char a = 0xff;
    char b = 0xff;
    int c = a + b;
    return 0;
}
{% endcodeblock %}

The compiled assembly is:

{% codeblock lang:gas %}
movl    $0, -4(%rbp)        # The return value of main is 0
movb    $-1, -5(%rbp)       # unsigned char a = 0xff;
movb    $-1, -6(%rbp)       # char b = 0xff;
movzbl  -5(%rbp), %eax
movsbl  -6(%rbp), %ecx
addl    %eax, %ecx          # int c = a + b
movl    %ecx, -12(%rbp)     # store c onto the stack
movl    -4(%rbp), %eax
popq    %rbp
ret                         # return value 0 from eax
{% endcodeblock %}

If you are not familiar with GAS syntax, you can check out 
[X86 Assembly/GAS Syntax][gas]. GAS assembly instructions are generally suffixed
with the letters "b", "s", "w", "l", "q" or "t" to determine what size operand
is being manipulated.

* b = byte (8 bit)
* s = short (16 bit integer) or single (32-bit floating point)
* w = word (16 bit)
* l = long (32 bit integer or 64-bit floating point)
* q = quad (64 bit)
* t = ten bytes (80-bit floating point)

GAS convention is to push parameter from left-to-right. For instance, 
`movl $0, -4(%rbp)` means to move `0x00000000` to address `-4(%rbp)`.

The instruction `movzbl` means moving a byte to long (32 bit int) with **zero
fill**.  `movzbl -5(%rbp), %eax` take `0xff` to `%eax` and fill the rest with
zero.  `%eax` is now `0x000000ff`.

The instruction `movsbl` means moving a byte to long with **signed fill**. 
`movsbl -6(%rbp), %ecx` take `0xff` to `%eax` and fill the rest with signed
value, which will cause `%ecx` to be `0xffffffff`.  Finally, `addl %eax, %ecx`
do the add operation, and `movl %ecx, -12(%rbp)` store the result onto the
stack.

Now, you understand that integer promotion is a rule how C types being mapped
directly to machine instructions. All arithmetics operands are applied to
smaller integers **after** they are transformed into int with *signed* or
*unsigned fill*.  You can think it this way: though `short` and `char` are stored
in one or two byte, they are treated as int when you use it with arithmetic
operations. The rule how they are transformed to int is called **integer
promotion**.

## Summary

Typically, CPUs are fastest at operating on integers of their native integer
size. On x86, 32-bit arithmetics are can be twice as fast compare to 16-bit
operands. C is a language focused on performance, so it will do the integer
promotion to make the program as fast as possible. Though, you need to
keep the integer promotion rule in mind to prevent some integer overflow
vulnerability issues.


[TIOBE]: http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html
[gas]: http://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax
