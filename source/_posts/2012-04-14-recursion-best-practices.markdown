---
layout: post
title: "Recursion best practices"
date: 2012-04-14 08:26
comments: true
categories: 
---

There are many programming languages that support recursion. However, recursion
is hard to write, and even harder to write it well.

> To iterate is human, to recurse, divine.

If you also surveyed deeply in FP area, you will find a lot of patterns, such as
tail recursion, continuous passing style, combination of higher order functions,
fixed point y-combinator, memoization, and many more. You can see that a simple
factorial can be written in [23 different forms][evolution]. How do we
pick a better one?

> In short, the only pattern you should use heavily is *tail recursion*. Use
> other patterns only when necessary.

<!-- more -->

## Straight (body) recursion, the problem

Let's begin with mostly seen recursions: factorial and fibonacci.

{% codeblock lang:haskell Haskell %}
fac 0 = 1
fac n = n * fac (n-1)

fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
{% endcodeblock %}

When you are programming in functional style, keep in mind of "non-functional"
execution.

    fac (5) = 5 * fac (4)
            = 5 * 4 * fac (3)
            = 5 * 4 * 3 * fac (2)
            = 5 * 4 * 3 * 2 * fac (1)
            = 5 * 4 * 3 * 2 * 1 * fac (0)
            = 5 * 4 * 3 * 2 * 1 * 1
            = 5 * 4 * 3 * 2 * 1
            = 5 * 4 * 3 * 2
            = 5 * 4 * 6
            = 5 * 24
            = 120

The problem is that the function has to use stack to hold number and multiply
to the value returned by the recursive function.[^1]

Also the fibonacci executing model:

{% graphviz %}
    digraph {
      fib [label="fib 5"];
      l [label="fib 4"];
      ll [label="fib 3"];
      lll [label="fib 2"];
      llll [label="fib 1"];
      lllr [label="fib 0"];
      llr [label="fib 1"];
      lr [label="fib 2"];
      lrl [label="fib 1"];
      lrr [label="fib 0"];
      r [label="fib 3"];
      rl [label="fib 2"];
      rll [label="fib 1"];
      rlr [label="fib 0"];
      rr [label="fib 1"];

      fib -> l; fib-> r;
      l -> ll; l -> lr; ll -> lll; ll -> llr;
      lll -> llll; lll -> lllr;
      lr -> lrl; lr -> lrr;
      r -> rl; r -> rr; rl -> rll; rl -> rlr;
    }
{% endgraphviz %}

This is even worse, the complexity of fibonacci function cost $O(\phi^n)$ where
$\phi=\frac{1+\sqrt{5}}{2}$. 

> Whenever you use a returned value in your function body, there is a cost.

We can reduce both factorial and fibonacci in tail recursion style using some
_accumulators_.

## Tail recursion

The common way to translate a body recursion into a tail recursion is to add a
accumulator in argument list.

{% codeblock lang:haskell %}
fac 0 acc = acc
fac n acc = fac (n-1) (n*acc)
{% endcodeblock %}

    fac 5 1 = fac 4 5
            = fac 3 20
            = fac 2 60
            = fac 1 120
            = fac 0 120
            = 120

We reduce the execution steps from $2 n$ to $n$, and there is no stack variables
any more! Moreover, in assembly level, it only have to use `goto` to the front
of the function and no need to set up environment again.

Fibonacci can be transformed to tail recursive function like this[^2]:

{% codeblock lang:haskell %}
fib' 0 f1 f2 = f1
fib' n f1 f2 = fib' (n-1) f2 (f1+f2)

fib n = fib' n 1 1
{% endcodeblock %}

    fib' 5 1 1 = fib' 4 1 2
               = fib' 3 2 3
               = fib' 2 3 5
               = fib' 1 5 8
               = fib' 0 8 13
               = 8

This time we use two accumulator `f1` and `f2` to record the state and make it
more "iterative". The original input `n` is treated as a "counter."

> Use multiple accumulators to make double recursion (like fibonacci) tail
> recursive

For different kinds of functional programming languages, you can abstract the
interface and hide details in language suggested ways:

{% codeblock lang:haskell Haskell %}
fib n = fib' n 1 1
    where
      fib' 0 f1 f2 = f1
      fib' n f1 f2 = fib' (n-1) f2 (f1+f2)
{% endcodeblock %}
{% codeblock lang:erlang Erlang %}
fib(N) -> fib(N,1,1).

fib(0,F1,F2) -> F1;
fib(N,F1,F2) -> fib(N-1,F2,F1+F2).
{% endcodeblock %}
{% codeblock lang:cl Common Lisp %}
(defun fib (n)
  (labels ((rec (n f1 f2)
             (if (zerop n) f1
                 (rec (1- n) f2 (+ f1 f2)))))
    (rec n 1 1)))
{% endcodeblock %}

## Returning a list

When using tail recursion, we can also construct a returning list instead of a
atom:

{% codeblock lang:haskell %}
naive_reverse []     = []
naive_reverse (x:xs) = (naive_reverse xs) ++ x -- oops, used ++ operator!

good_reverse []     acc = acc
good_reverse (x:xs) acc = good_reverse xs (x:acc)
{% endcodeblock %}

Or, in common lisp:

{% codeblock lang:cl %}
(defun good-reverse (rest acc)
  (if (null rest) acc
      (good-reverse (cdr rest) (cons (car rest) acc))))
{% endcodeblock %}

The key is to use `cons` (`:` in haskell, `|` in erlang) instead of list
concatenation operator (`++` in haskell and erlang.) The result of the left hand
side of the operator would be copied again and again and cause a quadratic
space and time complexity.

> In common practices, use cons to build a reversed list, then reverse it at the
  end of recursion

For example, if we want to return a list of fibonacci numbers[^3]:

{% codeblock lang:haskell %}
bad_fibs 0 f1 f2 fibs = fibs
bad_fibs n f1 f2 fibs = bad_fibs (n-1) f2 (f1+f2) (fibs++f1) -- oops

good_fibs 0 f1 f2 fibs = reverse fibs -- here is the magic
good_fibs n f1 f2 fibs = good_fibs (n-1) f2 (f1+f2) (f1:fibs)
{% endcodeblock %}

## Manipulating a tree

This is the best part of this article, it might be a little bit hard to
understand at the first time. But once you get the idea, you'd just love it as I
did. I first saw this idea in [Paul Graham's on lisp][on lisp]. He named this
pattern as "doubly recursion." Later on I saw people use this pattern in
[Quicksort (Erlang) - LiteratePrograms][qsort], without naming the pattern. For
convenience I'd just use the term "doubly recursive" to express the idea :)

> The key idea of doubly recursive is to use a returned accumulator as another
> recursion's accumulator

I want to write the flattening function that takes a nested list and return a
flatten one in haskell, however different depths of list are different types and
thus you cannot simply write a general function to deal it. So I turned back to
use lisp to express it:

{% codeblock lang:cl %}
(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) 
                              (rec (cdr x) acc)))))) ; use the returned acc as
                                                     ; another rec's acc arg
    (rec x nil)))
{% endcodeblock %}

Note that `labels` are like `where` clause in haskell. The `rec` function will
take a input x and return a reversed order of flatten list.

You can also apply this pattern on quick sort[^4]:

{% codeblock lang:haskell %}
qsort []     acc = acc
qsort [x]    acc = x:acc   -- one element case
qsort (x:xs) acc = partition xs [] [x] []
    where
      partition [] less equal greater = qsort less (equal ++ (qsort greater acc))
      partition (y:ys) less equal greater
          | y > x     = partition ys less     equal     (y:greater)
          | y < x     = partition ys (y:less) equal     greater
          | otherwise = partition ys less     (y:equal) greater
{% endcodeblock %}

## Other patterns

#### 1. Continuous passing style (CPS)

CPS is to pass a exit function to a function and let it call it at the end. The
concept is similar to tail recursion, but I think it is better to "make no
assumption about what user will do with your functions." Leave your function
interface simple and elegant, because using CPS is **micro optimization**. 

CPS is a powerful techniques, but you must use it wisely. For example, you can use it
in regular expression back tracing or garbage collection generation step. In
those example the function has to *jump* back to somewhere in the control flow,
and CPS can make the job done nice and clean.

But please don't use CPS like this:

{% codeblock lang:scheme %}
(define (fib k n) (if (< n 2) (k 1) 
  (fib (lambda (fib-of-n-1) 
         (fib (lambda (fib-of-n-2) 
                (k (+ fib-of-n-1 fib-of-n-2))) ; CPS
              (- n 2)))   ; inner fib
       (- n 1))))         ; outer fib
{% endcodeblock %}

The CPS above will still cause $O(\phi^n)$ time and space order to do the
calculation!

#### 2. Fixed point y-combinator

If you don't know about y-combinator, just skip it.

[The evolution of Haskell][evolution] suggested that fixed point
y-combinator is the fastest implementation of writing factorial in haskell, even
faster than tail recursion. However, it depends. In my benchmark it made no
differences on factorial function. And why do you want to make your function
hard to debug in a y-combinator? It is even hard to trace function calls in
Lisp's `trace` commands.

#### 3. Memoization

Memoization is also a powerful techniques that can benefit on rapid function
calls. It is a trade off of memory and speed. As CPS mentioned before, use it
wisely and not just for **cool**.

## Conclusion

There are not much design patterns on functional programming. I hope these
guidelines can be a start:

1. Whenever you use a returned value in your function body, there is a cost.
2. Use multiple accumulators to make double recursion (like fibonacci) tail
   recursive
3. In common practices, use cons to build a reversed list, then reverse it at the
   end of recursion
4. The key idea of doubly recursive is to use a returned accumulator as another
   recursion's accumulator
5. CPS and Memoization are powerful, but use it wisely

Any discussion and suggestions are welcomed! Thanks!

* * *

[^1]: In modern compiler the trivial straight recursion such as factorial will be
      optimized and make no big performance difference compare to the one written in
      tail recursion form.

[^2]: Take a look at how many possible ways to write 
      [fibonacci in common lisp](http://www.cliki.net/Fibonacci). There is also
      a way to write fibonacci in $O(log(n))$ order.

[^3]: The example is taken from 
      [Erlang performance tuning -- List handling](http://www.erlang.org/doc/efficiency_guide/listHandling.html).

[^4]: Example taken from [Quicksort (haskell) - Literate Programs](http://en.literateprograms.org/Quicksort_(Haskell)).

[on lisp]: http://www.bookshelf.jp/texi/onlisp/onlisp_5.html#SEC32
[qsort]: http://en.literateprograms.org/Quicksort_(Erlang)
[evolution]: http://www.willamette.edu/~fruehr/haskell/evolution.html
