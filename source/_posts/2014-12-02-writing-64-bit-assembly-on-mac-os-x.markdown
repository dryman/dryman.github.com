---
layout: post
title: "Writing 64 bit assembly on Mac OS X"
date: 2014-12-02 17:18
comments: true
categories: ASM
---


Many assembly tutorials and books doesn't cover
how to write a simple assembly program on the Mac OS X.
Here are some baby steps that can help people who
are also interested in assembly to get started
easier.

<!--more-->

## Mach-O file format

To get started on writing OSX assembly, you need to
understand OSX executable file format -- the [Mach-O
file format][mach-o]. It's similar to ELF, but instead
of sections of data, bss, and text, it has segments that
contains sections.

A common assembly in Linux like

```gas
.section data
.section text
# your code here
```

would translate into this in Mach-O

```gas
.section __DATA,__data
# __DATA is segment, __data is section
.section __TEXT,__text
# __TEXT is segment, __text is section

# your code here
```

Mach-O is pretty flexible. You can embed a
`cstring` section in your `__TEXT` segment instead
of putting it in `__DATA,__data`. Actually this is
the default behavior that compiler does on your Mac.

## Hello Assembly

Now we know how to translate common linux assembly
to mac, let's write a basic program -- do a system call
with an exit code.

On x86 you do a system call by `int x80` instruction. On
64 bit machine, you do this by `syscall`. Here's the sample
code:

```gas
# Simple exit program
.section __TEXT,__text
.globl _main
_main:
  movl $0x2000001, %eax # system call $1 with $0x2000000 offset
  movl $0, %ebx         # set the exit code to be $0
  syscall
```

you can compile the code by the following commands:

```bash
as exit.s -o exit.o
ld exit.o -e _main -o exit     # -e specify the entry point of the executable
./exit
echo $? # show last return code, should show 0
```

To perform a system call, you put the system call number in
`%eax`, and put the actual exit code to `%ebx`. The system
call number can be found in `/usr/include/sys/syscall.h`.

```c
#define	SYS_syscall        0
#define	SYS_exit           1
#define	SYS_fork           2
#define	SYS_read           3
#define	SYS_write          4
#define	SYS_open           5
#define	SYS_close          6
#define	SYS_wait4          7
```

The system call number need to add an offset `0x2000000`, because
OSX has 4 different class of system calls. You can find the reference
here [XNU syscall][xnu].

[xnu]: http://www.opensource.apple.com/source/xnu/xnu-1699.26.8/osfmk/mach/i386/syscall_sw.h

## System call by using wrapper functions

If you're like me that had no assembly background, you might
feel that `syscall` is alien to you. In C, we usually use
wrapper functions to perform the call:

```gas
# exit2.s
# To compile it, type the following in shell
# as exit2.s -o exit2.o
# ld exit2.o -e _main -lc -o exit

.section __TEXT,__text
.globl _main
_main:
  pushq %rbp
  movq %rsp, %rbp
  movl $5, %edi        # exit(5);
  callq _exit
```

Now we call a `libc` function instead of performing a system
call. To do this we need to link to libc by passing `-lc`
to linker `ld`. There are several things you need to do
to make a function call. 

### Call frame

We need to prepare the stack before we call a function. Else
you would probably get a segmentation fault.
The values in `%rsp` and `%rbp` is used to preserve frame information.
To maintain the stack, you first push the base register `%rbp`
onto the stack by `pushq %rbp`;
then you copy the stack register `%rsp` to the base register.
 
If you have local variables, you subtract `%rsp` for space.
Remember, stack grows down and heap grows up.
When releasing the frame, you add the space back to `%rsp`.

A live cycle of a function would look like this:

```gas
# function start
pushq %rbp
movq %rsp, %rbp
subq $4, %rsp      # reserve 4 bytes on stack
movl $5, -4(%rbp)  # We usually use base register instead
                   # of stack pointer to access data
addq $4, %rsp      # release the stack
popq %rbp          # restore old %rbp
retq               # function ends
```

The stack size can be set at link time. On OSX, below are the
example parameters you can pass to `ld` to set the stack size:

```bash
ld stack.o -stack_size 0x4000 -stack_addr 0x7fff5fbff000 -lc -e _start -o stack
```

When setting the stack size, you also have to set the stack address.
On the [System V Application Binary Interface][abi] it says

> Although the AMD64 architecture uses 64-bit pointers, implementations
> are only required to handle 48-bit addresses. Therefore, conforming processes may only
> use addresses from `0x00000000 00000000` to `0x00007fff ffffffff`




I don't know a good answer of how to chose a good stack address.
I just copy whatever a normal code produces.

### Parameters passing

The rules for parameter passing can be found in [System V
Application Binary Interface][abi]:

1. If the class is MEMORY, pass the argument on the stack.
If the size of an object is larger than four eight bytes, or
it contains unaligned fields, it has class MEMORY.
2. If the class is INTEGER, the next available register of the sequence `%rdi`,
`%rsi`, `%rdx`, `%rcx`, `%r8` and `%r9` is used.
3. If the class is SSE, the next available vector register is used, the registers
are taken in the order from `%xmm0` to `%xmm7`.

The `exit()` function only need one integer parameter, therefore we put
the exit code in `%edi`. Since the parameter is type `int`, we use 32 bit
variance of register `%rdi` and the instruction is `movl` (mov long) instead
of `movq` (mov quad).

## Hello world

Now we know the basics of how to perform
a system call, and how to call a function.
Let's write a hello world program.

```gas
# hello_asm.s
# as hello_asm.s -o hello_asm.o
# ld hello_asm.o -e _main -o hello_asm
.section __DATA,__data
str:
  .asciz "Hello world!\n"

.section __TEXT,__text
.globl _main
_main:
  movl $0x2000004, %eax           # preparing system call 4
  movl $1, %edi                    # STDOUT file descriptor is 1
  movq str@GOTPCREL(%rip), %rsi   # The value to print
  movq $100, %rdx                 # the size of the value to print
  syscall

  movl $0, %ebx
  movl $0x2000001, %eax           # exit 0
  syscall
```

The global variable `str` can only be accessed through GOT
(Global Offset Table). And the GOT needs to be access from
the instruction pointer `%rip`. For more curious you can
read [Mach-O Programming Topics: x86-64 Code Model][].

The register used for `syscall` parameters are a little
bit different than the normal function call.
It uses `%rdi`, `%rsi`, `%rdx`, `%r10`, `%r8` and `%r9`.
You cannot pass more than 6 parameters in `syscall`, nor
can you put the parameters on the stack.

## Hello world using printf

Now you know the basics of assembly. A hello world
example using printf should be trivial to read:

```gas
# hello_asm2.s
# as hello_asm2.s -o hello_asm2.o
# ld hello_asm2.o -e _main -lc -o hello_asm2
.section __DATA,__data
str:
  .asciz "Hello world!\n"

.section __TEXT,__text
.globl _main
_main:
  pushq %rbp
  movq %rsp, %rbp
  movq str@GOTPCREL(%rip), %rdi
  movb $0, %al 
  callq _printf
  popq %rbp
  movl $0x2000001, %eax
  syscall
```

## Conclusion

The 64 bit assembly looks more vague than the tutorials
written in X86 assembly. Once you know these basic differences,
it's easy for you to learn assembly in depth on your own,
even if the material is designed for x86. I highly recommend
the book "Programming from the ground up". It is well written
for self study purpose.

## References

1. [OS X ABI Mach-O File Format Reference][mach-o]
2. [System V Application Binary Interface][abi]
3. [OS X Assembler Reference][] Assembler Directives
4. [Mach-O Programming Topics][]
5. [Mach-O Executables - Build Tools][]
6. Book: Programming from the ground up.

[Mach-O Programming Topics: x86-64 Code Model]: https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachOTopics/1-Articles/x86_64_code.html

[mach-o]: https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachORuntime/index.html

[abi]: http://www.x86-64.org/documentation/abi.pdf

[OS X Assembler Reference]: https://developer.apple.com/library/mac/documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html#//apple_ref/doc/uid/TP30000851-CH211-SW1

[Mach-O Programming Topics]: https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachOTopics/0-Introduction/introduction.html#//apple_ref/doc/uid/TP40001827-SW1

[Mach-O Executables - Build Tools]: http://www.objc.io/issue-6/mach-o-executables.html

