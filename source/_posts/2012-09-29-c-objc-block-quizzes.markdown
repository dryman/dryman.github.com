---
layout: post
title: "C/ObjC block quizzes"
date: 2012-09-29 12:15
comments: true
categories: cocoa, block
---

Apple introduced blocks (anonymous functions or lambdas) as C extensions for its
parallel programming model [Grand Central Dispatch][gcd]. Unlike ordinary C
functions, blocks can capture surrounding variable contexts. The captured
variables are casts to `const` by default, and for mutable variables you can mark
it with `__block` storage qualifier. However, there is a lot of pitfalls in
`__block` variables.  Can you identify all of them?

[gcd]: https://developer.apple.com/documentation/Performance/Reference/GCD_libdispatch_Ref/Reference/reference.html

<!-- more -->

<script language="javascript">
function toggle(divId) {
  var div = document.getElementById(divId);
  var link = document.getElementById(divId+"_a");
  if (div.style.display == "block") {
    div.style.display = "none";
    link.innerHTML = "Show answer";
  } else {
    div.style.display = "block";
    link.innerHTML = "Toggle answer";
  }
}
</script>

## Testing environment

{% codeblock lang:c %}
/*
 * clang -Wall -fblocks -framework Foundation quiz.c -o quiz
 */
#include <stdio.h>
#include <Block.h>


typedef void (^BoringBlock)(void);
BoringBlock boringBlock;

void quiz_1 (void) {...}
void quiz_2 (void) {...}
...

int main (void)
{
    quiz_1()
    boringBlock();
    Block_release(boringBlock);
    ...

    return 0;
}
{% endcodeblock %}

The above is compile configuration and the program structure of the quiz.
You can download and test the code form [Github][source].

## Quiz 1

{% codeblock lang:c %}
void quiz_1 (void)
{
    __block int x = 1;
    printf("x address is %p\n", &x);

    BoringBlock localBlock = ^{
        x++; // Dummy use of x
        printf("End of quiz 1\n\n");
    };
    boringBlock = Block_copy(localBlock);

    printf("after copy, x address is %p\n", &x);
}
{% endcodeblock %}

What would be printed if we execute `quiz_1()` then `boringBlock()`? Would `&x` be
printed in same address or different addresses?

<a id="quiz_1_a" href="javascript:toggle('quiz_1')">Show answer</a>

<div id="quiz_1" style="display: none;">
    <p> In block implementation spec, captured <code>__block</code> variable
    <code>x</code> will be moved to heap after we execute <code>Block_copy</code>.
    On my machine it prints: </p>

<pre><code>x address is 0x7fff613d04f8
after copy, x address is 0x7fe9a1c13f78
End of quiz 1
</code></pre>

    <p> Memory allocation on stack is much faster then heap, so variable and block
    literal are both allocated on stack by default. It is copied to heap only when
    necessary. </p>
</div>

## Quiz 2

{% codeblock lang:c %}
void quiz_2 (void)
{
    __block int x = 1;

    BoringBlock localBlock = ^{
        printf("x is %d\n", x);
        printf("End of quiz 2\n\n");
    };
    boringBlock = Block_copy(localBlock);

    x++;
}
{% endcodeblock %}

Now, if we change the variable `x` in `quiz_2()` scope, would captured variable
`x` also changes its value?

<a id="quiz_2_a" href="javascript:toggle('quiz_2')">Show answer</a>

<div id="quiz_2" style="display: none;">
    <p> Actually it does! Though <code>x</code> is in different memory address,
    we can just use it as normal <code>int</code> value, and it behaves just as
    expected.  On my machine it prints:</p>

<pre><code>x is 2
End of quiz 2
</code></pre>
</div>

## Quiz 3

{% codeblock lang:c %}
void quiz_3 (void)
{
    __block int x = 1;
    __block int* ptr = &x;

    BoringBlock localBlock = ^{
        printf("x is %d, *ptr is %d\n", x, *ptr);
        printf("End of quiz 3\n\n");
    };
    boringBlock = Block_copy(localBlock);

    x++;
}
{% endcodeblock %}

Would `x` and `*ptr` be the same value?

<a id="quiz_3_a" href="javascript:toggle('quiz_3')">Show answer</a>

<div id="quiz_3" style="display: none;">

    <p>Well, if you are lucky, it would print</p>

<pre><code>x is 2, *ptr is 1
End of quiz 3
</code></pre>

    <p> Though <code>ptr</code> and <code>x</code> are both moved to the heap,
    <code>ptr</code> still points to the original address of <code>x</code>.
    Thus, the value in <code>*ptr</code> is garbage.  If there are other
    functions that use the stack before you use <code>boringBlock()</code>.  It
    would print:</p>

<pre><code>clean up stack
x is 2, *ptr is 24
End of quiz 3
</code></pre>
<p><strong>Oops</strong></p>
</div>

## Quiz 4
{% codeblock lang:c %}
void quiz_4(void)
{
    __block int x[1] = {1};

    void (^localBlock)(void) = ^{
        printf("x[0] is %d\n", x[0]);
        printf("End of quiz 4\n\n");
    }
    x[0] = 2;
}
{% endcodeblock %}

What about array?

<a id="quiz_4_a" href="javascript:toggle('quiz_4')">Show answer</a>

<div id="quiz_4" style="display: none;">

    <p> Actually, complier won't let you compile this. C array and
    <code>struct</code> contains C array are both invalid with
    <code>__block</code>. </p>
</div>

## Quiz 5

{% codeblock lang:c %}
BoringBlock quiz_5(void)
{
    __block int x = 1;

    printf("x address is %p\n", &x);

    BoringBlock localBlock = ^{
        x++;
        printf("x is %d, &x is %p\n", x, &x);
    };
    boringBlock = Block_copy(localBlock);
    printf("x address is %p\n", &x);

    BoringBlock retBlock = Block_copy(localBlock);
    printf("x address is %p\n", &x);

    return retBlock;
}
{% endcodeblock %}

Block execution:

{% codeblock lang:c %}
    BoringBlock retBlock = quiz_5();
    boringBlock();
    retBlock();
    Block_release(boringBlock);
    Block_release(retBlock);
    printf("End of quiz 5\n\n");
{% endcodeblock %}

What if we copied the block twice. Would the address change twice also?

<a id="quiz_5_a" href="javascript:toggle('quiz_5')">Show answer</a>

<div id="quiz_5" style="display: none;">

<p>The address of <code>x</code> only changes once in first copy:</p>

<pre><code>x address is 0x7fff613d04f8
x address is 0x7fe9a1c13f78
x address is 0x7fe9a1c13f78
x is 2, &x is 0x7fe9a1c13f78
x is 3, &x is 0x7fe9a1c13f78
End of quiz 5
</code></pre>

    <p> So, how does memory management work? Actually, compiler use reference
    counting on <code>__block</code> variables instead of block literals. For
    more curious, see my next post.</p>
</div>

## References

* [BNR Advanced Mac OSX programming: Blocks Internals][bnr]
* [Block language spec][spec]
* [Block ABI Apple][abi]

## Source code:

You can download source code of this quiz from [Github][source].

[source]: https://github.com/dryman/C-ObjC-block-quiz
[bnr]: http://www.informit.com/articles/article.aspx?p=1749597&seqNum=12
[spec]: http://clang.llvm.org/docs/BlockLanguageSpec.txt
[abi]: http://clang.llvm.org/docs/Block-ABI-Apple.txt


