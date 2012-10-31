---
layout: post
title: "Type Qualifiers and friends"
date: 2012-10-29 08:55
comments: true
categories: C Objective-C
---

Type qualifiers are heavily used in C and Objective C. In C99 there are four
type qualifiers: `const`, `restrict`, `register` and `volatile`. In objective C,
Apple introduced `__weak`, `__strong`, `__unsafe_unretained`, and
`__autoreleasing` for automatic reference counting.


It is easy to get confused with complicated type qualifiers. For example:

{% codeblock lang:c %}
char* const * x;  // x is a pointer to const pointer to char
{% endcodeblock %}

In this post I'll go through what type qualifiers are, and how do we read and
write it in correct way.

<!-- more -->

## Names and definitions

Type qualifiers should not be confused with **storage specifiers** like `static`
`auto`, `extern` and `register`. To illustrate this, allow me to use Mike Ash's
example:

{% codeblock lang:c %}
static const int x;
{% endcodeblock %}

Here, `static` is a storage specifier to tell complier how `x` is stored, and
`const` is a type qualifier to tell complier **the type** of x is read-only
data. Since `const` is part of the type, you can write:

{% codeblock lang:c %}
typedef const int MyInt;
{% endcodeblock %}

but you cannot write:

{% codeblock lang:c %}
typedef static int MyInt;
{% endcodeblock %}

This is because `static` is not part of the type.

You might wonder: is `__block` a type qualifier or storage specifier?  The 
[clang block language spec][block spec] said that it is a **storage qualifier**.
Don't get confused.  `__block` is a *storage qualifer/specifer* which modifies
how variable is stored.  For more curious on `__block`, you can check out my
previous post [Block byref internals][byref].

### ARC ownership qualifiers

What about `__strong`, `__weak`, `__unsafe_unretained`, and `__autoreleasing`?
You *can* use it with `typedef`, and they are truly part of the type. However, they
are a bit different from C type qualifiers. ARC generated code have an runtime
API supports it. You can manually use some of those: `objc_storeWeak`,
`objc_destroyWeak`...etc. In Clang specification, these qualifiers are named
**ownership qualifiers**. Luckily, they share the same rule of type
qualifiers.

## The grammar

C declaration can be really complicated. In this section I'll just cover the
basics and the most commonly seen ones.

* Rule 1: find the identifier (the variable), read from right to left.

{% codeblock lang:c %}
int x;                // x is int
int * x;              // x is a 'pointer to' int
int * * x;            // x is a 'pointer to' 'pointer to' int
{% endcodeblock %}

When there is a type qualifier, it applies to its immediate left:

{% codeblock lang:c %}
int const x;          // x is a const int
int * const x;        // x is a 'const pointer to' int
int const * x;        // x is a 'pointer to' const int
int * const * x;      // x is a 'pointer to' 'const pointer to' int
{% endcodeblock %}

* Rule 2: If next to type specifier, it applies to type-specifier

{% codeblock lang:c %}
const int x;          // x is a const int
const int * const x;  // x is a 'const pointer to' const int
{% endcodeblock %}

* Rule 3: If there are parenthesis or bracelets, reorder it to postfix form:

{% codeblock lang:c %}
int * const * ( * p)();
// postfix: p * () * const * int
// p is a pointer to a function returning a
// pointer to const-pointer-to-int
{% endcodeblock %}

For more curious, checkout [Deciphering Complex C Declarations][complex c] and
[cdecl][cdecl].

## restrict, volatile, and register

### volatile

> Every reference to the variable will reload the contents from memory rather
> than take advantage of situations where a copy can be in a register.

The `volatile` qualifier maintains consistency of memory access to data objects.
Volatile variable are read from memory each time their values is needed, and
writen back to memory each time they are changed. However, volatile variables
are not automic. If you want to write thread safe operation, you can write
something like:

{% codeblock OSAtomic.h lang:c http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/libkern/libkern/OSAtomic.h %}
static __inline bool OSAtomicCompareAndSwapInt(int oldi, int newi, int volatile *dst) {
    int original = InterlockedCompareExchange(dst, newi, oldi);
    return (original == oldi);
}
{% endcodeblock %}

This is a function that is thread and multiprocessor safe to swap/update an integer.
Objective-C runtime uses these functions defined in [OSAtomic.h][OSAtomic] to
manage retain counts.

### register

`register` is a opposite keyword to `volatile`.
The register type modifier tells the compiler to store the variable being
declared in a CPU register (if possible), to optimize access.

### restrict

`restrict` is a keyword purely for the purpose of optimization. 

> In the C programming language, as of the C99 standard, restrict is a keyword
> that can be used in pointer declarations. The restrict keyword is a
> declaration of intent given by the programmer to the compiler. It says that
> for the lifetime of the pointer, only it or a value directly derived from it
> (such as `pointer + 1`) will be used to access the object to which
> it points. This limits the effects of pointer aliasing, aiding caching
> optimizations.

`restrict` is a qualifier for pointers. It claims that the memory that pointer
points to can only be accessed by this pointer.  Consider this case:

{% codeblock lang:c %}
char *src, *dst;
for(int i = 0; i < len; i++)
    dst[i] = src[i];
{% endcodeblock %}

If the `*dst` overlapped with `*src`, compiler can only generate code that load a
small piece of memory and operate it once at a time. Fortran does not have this
problem because it does not have pointers. Thus Fortran can do ambitious optimization
to load a big chunck of memory and operate it all at once. `restrict` is a new
keyword defined in C99 to address this problem. The original code can be
rewritten as:

{% codeblock lang:c %}
void *memcpy(void *restrict s1, const void *restrict s2, size_t n);
{% endcodeblock %}

and compiler can optimize this code like Fortran does!

Note that `restrict` is a type qualifier for **pointers**.

{% codeblock lang:c %}
int * restrict x; // correct
int restrict * x; // wrong
restrict int * x; // wrong
{% endcodeblock %}

## ARC ownership qualifiers

If you understand all the above, then Objective-C Automatic Reference Counting
qualifiers should be easy to you! Here is the definition from Apple:

1. `__strong` is the default. An object remains **alive** as long as there is a strong
  pointer to it.
2. `__weak` specifies a reference that does not keep the referenced object alive. A
  weak reference is set to `nil` when there are no strong references to the object.
3. `__unsafe_unretained` specifies a reference that does not keep the referenced
  object alive and is not set to `nil` when there are no strong references to the
  object. If the object it references is deallocated, the pointer is left
  dangling.
4. `__autoreleasing` is used to denote arguments that are passed by reference `(id *)`
  and are autoreleased on return.

All ownership qualifiers should decorate on Objective-C object pointers.

{% codeblock lang:objc %}
MyClass * __weak _weakSelf = self;    // correct
MyClass __weak * _weakSelf = self;    // wrong! May cause serious bugs!
__weak MyClass * _weakSelf = self;    // wrong!

// Explicitly transfer CGColorRef ownership from UIColor to layer's backgroundColor
CALayer* layer = [CALayer layer];
CGColorRef redRef;
@autorelease{
    UIColor * __autoreleasing redColor = [[UIColor redColor] colorWithAlphaComponent:.5f];
    redRef = CFRetain([redColor CGColor]);
} // UIColor released
layer.backgroundColor = redRef;
CFRelease(redRef);
{% endcodeblock %}

For more curious on why do we need to write verbose code for `CGColor`, you can
take a look at Big Nerd Ranch's 
[ARC Gotcha -- Unexpectedly Short Lifetimes][cgcolor] and Amatten's
[ARC Best Practices][arc best].

## References

* [Deciphering complex c declarations][complex c]
* [Mike Ash Friday QA: Type qualifiers in C part 1][mike ash]
* [ARC Gotcha -- Unexpectedly Short Lifetimes][cgcolor] and Amatten's
* [ARC Best Practices][arc best]
* [Block language spec][block spec]
* [Clang ARC spec][clang arc]
* [Transitioning to ARC Release Notes][apple arc]

[byref]: http://www.idryman.org/blog/2012/09/29/c-objc-block-byref-internals/
[complex c]: http://www.vineetgupta.com/2011/03/deciphering-complex-c-declarations/
[mike ash]: http://www.mikeash.com/pyblog/friday-qa-2009-06-26-type-qualifiers-in-c-part-1.html
[cdecl]: http://cdecl.org
[cgcolor]: http://weblog.bignerdranch.com/296-arc-gotcha-unexpectedly-short-lifetimes/
[arc best]: http://amattn.com/2011/12/07/arc_best_practices.html
[block spec]: http://clang.llvm.org/docs/BlockLanguageSpec.txt
[clang arc]: http://clang.llvm.org/docs/AutomaticReferenceCounting.html
[apple arc]: http://developer.apple.com/library/mac/#releasenotes/ObjectiveC/RN-TransitioningToARC/Introduction/Introduction.html
[OSAtomic]: http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/libkern/libkern/OSAtomic.h
