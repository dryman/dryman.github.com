---
layout: post
title: "C/ObjC block byref internals"
date: 2012-09-29 15:21
comments: true
categories: cocoa, block
---

In the last post, I mentioned that `__block` variable (here we named it block
byref) will be retained if multiple blocks referenced it. Here are some sample
code to show how runtime deals with reference counts.

<!-- more -->

In order to *move* the `__block` variable to the heap, the compiler must rewrite
access to such a variable to be indirect through the structures forwarding
pointer.  For example:

{% codeblock lang:c %}
    int __block i = 10;
    i = 11;
{% endcodeblock %}

would be rewritten to be:
{% codeblock lang:c %}
    struct _block_byref_i {
      void *isa;
      struct _block_byref_i *forwarding;
      int flags;   //refcount;
      int size;
      int captured_i;
    } i = { NULL, &i, 0, sizeof(struct _block_byref_i), 10 };

    i.forwarding->captured_i = 11;
{% endcodeblock %}

## Print runtime information

As long as we know how block byref is structured, we can access the memory and
dump it with internal function `_Block_byref_dump`.

{% codeblock more_curious.c lang:c https://github.com/dryman/C-ObjC-block-quiz/blob/master/more_curious.c source%}
/*
 * clang -Wall -fblocks -framework Foundation more_curious.c -o more_curious
 */
#include <stdio.h>
#include <Block.h>

struct Block_byref {
    void *isa;
    struct Block_byref *forwarding;
    int flags; /* refcount; */
    int size;
    void (*byref_keep)(struct Block_byref *dst, struct Block_byref *src);
    void (*byref_destroy)(struct Block_byref *); 
    /* long shared[0]; */
};

static __inline struct Block_byref* derefBlockVar(char* src) 
{
    return (struct Block_byref*) (src - 2*sizeof(int) - 2*sizeof(void *));
}

extern const char *_Block_dump(const void *block);
extern const char *_Block_byref_dump(struct Block_byref *src);



typedef void(^BoringBlock)(void);
void (^boringBlock)(void);

BoringBlock blockRefCountTest(void)
{
    __block int x = 1;

    printf("Before local block:\n%s\n\n",_Block_byref_dump(derefBlockVar((char*)&x)));
    BoringBlock localBlock = ^{
        x++;
        printf("Execute block:\n%s\n",_Block_byref_dump(derefBlockVar((char*)&x)));
        printf("x is %d, &x is %p\n", x, &x);
    };
    printf("After local block generated:\n%s\n\n",_Block_byref_dump(derefBlockVar((char*)&x)));

    boringBlock = Block_copy(localBlock);
    printf("After first block copy:\n%s\n\n",_Block_byref_dump(derefBlockVar((char*)&x)));

    BoringBlock retBlock = Block_copy(localBlock);
    printf("After second block copy:\n%s\n\n",_Block_byref_dump(derefBlockVar((char*)&x)));
    return retBlock;
}

int main (void)
{
    BoringBlock retBlock = blockRefCountTest();
    boringBlock();
    Block_release(boringBlock);
    retBlock();
    Block_release(retBlock);

    return 0;
}

{% endcodeblock %}

The execution result is

    $ ./more_curious 
    Before local block:
    byref data block 0x7fff6e8034f0 contents:
      forwarding: 0x7fff6e8034f0
      flags: 0x0
      size: 32


    After local block generated:
    byref data block 0x7fff6e8034f0 contents:
      forwarding: 0x7fff6e8034f0
      flags: 0x0
      size: 32


    After first block copy:
    byref data block 0x7fc191c13f60 contents:
      forwarding: 0x7fc191c13f60
      flags: 0x1000004
      size: 32


    After second block copy:
    byref data block 0x7fc191c13f60 contents:
      forwarding: 0x7fc191c13f60
      flags: 0x1000006
      size: 32


    Execute block:
    byref data block 0x7fc191c13f60 contents:
      forwarding: 0x7fc191c13f60
      flags: 0x1000004
      size: 32

    x is 2, &x is 0x7fc191c13f78
    Execute block:
    byref data block 0x7fc191c13f60 contents:
      forwarding: 0x7fc191c13f60
      flags: 0x1000002
      size: 32

    x is 3, &x is 0x7fc191c13f78

## What does it mean?

We can find some interesting things in this log:

1. Block byref flags and address doesn't change until first copy.
2. After copy, the flag becomes `0x1000004`. There's a `(1 << 24)` flag in the
   front.
3. Block releases does decrease flag number in times of 2.

The `(1 << 24)` flag (the number one in `0x100xxxxx`) means `BLOCK_NEEDS_FREE`
in this `enum`:

{% codeblock Block_private.h lang:c http://opensource.apple.com/source/libclosure/libclosure-38/Block_private.h source %}
enum {
    BLOCK_REFCOUNT_MASK =     (0xffff),
    BLOCK_NEEDS_FREE =        (1 << 24),
    BLOCK_HAS_COPY_DISPOSE =  (1 << 25),
    BLOCK_HAS_CTOR =          (1 << 26), /* Helpers have C++ code. */
    BLOCK_IS_GC =             (1 << 27),
    BLOCK_IS_GLOBAL =         (1 << 28),
    BLOCK_HAS_DESCRIPTOR =    (1 << 29) 
};
{% endcodeblock %}

So, flag changes until first copy makes sense, because block byref doesn't need
free until it is copied to heap.

The reference count is actually taken out from flags like so:

{% codeblock lang:c %}
    refcount = shared_struct->flags & BLOCK_REFCOUNT_MASK;
{% endcodeblock %}

I didn't find out why reference count is in times of two. The actual code that
increase and decrease reference count is this:

{% codeblock runtime.c lang:c http://opensource.apple.com/source/libclosure/libclosure-38/runtime.c source %}
static int latching_incr_int(int *where) {
    while (1) {
        int old_value = *(volatile int *)where;
        if ((old_value & BLOCK_REFCOUNT_MASK) == BLOCK_REFCOUNT_MASK) {
            return BLOCK_REFCOUNT_MASK;
        }
        if (OSAtomicCompareAndSwapInt(old_value, old_value+1, (volatile int *)where)) {
            return old_value+1;
        }
    }
}

static int latching_decr_int(int *where) {
    while (1) {
        int old_value = *(volatile int *)where;
        if ((old_value & BLOCK_REFCOUNT_MASK) == BLOCK_REFCOUNT_MASK) {
            return BLOCK_REFCOUNT_MASK;
        }
        if ((old_value & BLOCK_REFCOUNT_MASK) == 0) {
            return 0;
        }
        if (OSAtomicCompareAndSwapInt(old_value, old_value-1, (volatile int *)where)) {
            return old_value-1;
        }
    }
}
{% endcodeblock %}

`OSAtomicCompareAndSwapInt` is a function that can change value of a `int`
thread and multiprocessor safe.

## Conclusion

Block seems magical at the first seen. With block we no longer have to do the
_function pointer + struct cast + void*_ tricks. Block automatically captures
variables for us, and we can use `__block` storage qualifier to declare mutable
ones. Behind the scene is really cool hack to make all this happen. However, it
not quite easy to debug blocks and byrefs. We'd need to write some helper
functions for `gdb` or `lldb`. These will be discussed in my next post.

## References:

* [BNR Advanced Mac OSX programming: Blocks Internals][bnr]
* [Block language spec][spec]
* [Block ABI Apple][abi]
* [Block private header][private]
* [Block runtime][runtime]
* [Mac OS X Manual Page For OSAtomicCompareAndSwapInt][swap]

[bnr]: http://www.informit.com/articles/article.aspx?p=1749597&seqNum=12
[spec]: http://clang.llvm.org/docs/BlockLanguageSpec.txt
[abi]: http://clang.llvm.org/docs/Block-ABI-Apple.txt
[private]: http://opensource.apple.com/source/libclosure/libclosure-38/Block_private.h
[runtime]: http://opensource.apple.com/source/libclosure/libclosure-38/runtime.c
[swap]: http://developer.apple.com/library/ios/#documentation/System/Conceptual/ManPages_iPhoneOS/man3/OSAtomicCompareAndSwapInt.3.html
