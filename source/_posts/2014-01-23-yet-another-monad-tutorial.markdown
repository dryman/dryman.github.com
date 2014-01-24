---
layout: post
title: "Yet Another Monad Tutorial in 15 Minutes"
date: 2014-01-23 19:21
comments: true
categories: Haskell
published: true
---

Functional programming has become popular these days, but unlike object-oriented languages, each FP language is so different from the other. Some of these use strict evaluation while others use lazily evaluated models; tons of new concurrent models were introduced; further more, states are handled differently too.

Haskell, for example, does not have states, but uses its powerful type system to construct the stateful program flow normally used in other languages. As you might guess, Monad is one of the type that does the trick. Defining a Monad type is pretty much like defining a class in an object oriented language. However, Monad can do much more than a class. It's a type that can be used for exception handling, constructing parallel program workflow or even a parser generator!

By learning Monad, You'll know a different perspective of how to program, and rethink the composition of data logic beyond the object-oriented programming kingdom.

<!--more-->

What is a type
--------------

Before we jump into Monad itself, let's quickly go through the type systems. What is a type? We usually think of type as a static thing like `int` or `float`, but actually, type is more dynamic; **types are the rules associated with the data, not the actual data itself**. It is important to remember this point.

For example, an `int` is treated as `int` only when you use `addl` CPU instruction to present `+` operation on a 64 bit machine. On the other hand, `float` uses `addss` the instruction for `+` and `%xmm` registers for computations. It's the *rules*, or the *generated instructions*, that define the type.

The whole concept of Object-Oriented Programming is to let you use classes/interfaces to define types, the rules (methods) that interacts with the actual data. For more advanced needs people use templates(c++) or generics(java) to define more abstracted rules that are more reusable. Monad, is pretty much like that generic class.

Now we understand what a type is; it's just a set of rules, or methods in Object-Oriented terms. A Monad, is just yet another type, and the definition of this type is defined by four rules:

1. bind `(>>=)`
2. then `(>>)`
3. `return`
4. `fail`

By going through these four rules and looking at the examples below, you'll understand what a Monad is and why is it so awesome and powerful!

Monad lesson 1: the bind operator `(>>=)`
----------------------------------------

The Monad bind operator is a function type signature. A type signature is like an abstract method defined in an abstract class:

```hs
class  Monad m  where
    (>>=) :: m a -> (a -> m b) -> m b
```

You may not familiar with Haskell syntax, but actually it's pretty straight forward.

<img width="60%" style="margin-left:20%;" src="/images/monad/type-signature.png" />

It takes 2 inputs and returns an output.

1. A Monad that contains type `a` as the first input

2. A function `(a -> m b)` as the second input. `(a -> m b)` is a first order function that takes `a` as input, and returns Monad `m b`. You can think of this as a delegate design pattern in Object-Oriented world, except you don't need to pass in a whole object, but only a function itself is sufficient.

3. The implementation will return Monad `m b` as a result.

<img width="50%" style="margin-left:25%;" src="/images/monad/monad-structure.png" />


Why is this definition powerful? Well, one benefit is that you can wrap common logic in Monad and save yourself time.

* * *

## Application 1: exception handling

Exception handling is always a pain in the ass. Lets demonstrate how to use a Monad to reduce the code size and un-nest if-else blocks.


```c
int errno = 0;
if (errno = io_function1( input1, &output1) == 0) {
    /* do some logic */
    if (errno = io_function2( input2, &output2) == 0) {
        /*
         * some more logic
         * and maybe more nested functions
         */
    } else {
      /* handle error 2 */
    }
} else {
    /* handle error 1 */
}
```

<!-- need to fix this part -->
There's nothing wrong with nested if-else blocks. The logic is straightforward and easy to understand. To un-nest the if-else statements, there are several ways to do it. You can either use a `goto` statement and pass the control to a barrier label, or use try-catch block in C++ or a similar language that supports it. Or, here comes the Maybe Monad for the rescue!

```hs
data  Maybe a  =  Nothing | Just a

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
```

A Maybe Monad has two constructors: `Nothing` and `Just a`. It's a simple kind of error Monad, where all errors are represented by `Nothing`. The rules are simple:

1. If the first input `M a` is `Just x`, run the second input (the function) with value `x`
2. If the first input is `Nothing`, just return `Nothing`

When you combine several Maybe Monad handling functions together, if one of the upstream function went wrong by returning `Nothing`, all the downstream function(s) won't be executed.

```hs
maybeHalf :: Int -> Maybe Int         -- Haskell type definition
maybeHalf a                           -- Actual function body
         | even a = Just (div a 2)
         | otherwise = Nothing
```

The first line is Haskell function type definition. `maybeHalf` takes an `Int` and returns `Maybe Int` type. Other than that, the function body tells us:

1. If input `a` is even, return `a / 2` wrapped in `Maybe` Monad constructor `Just`.

2. Otherwise, return `Maybe` Monad constructor `Nothing`.

```hs
*Main> Just 10 >>= maybeHalf
Just 5
*Main> Just 10 >>= maybeHalf >>= maybeHalf
Nothing
*Main> Just 10 >>= maybeHalf >>= maybeHalf >>= maybeHalf
Nothing
```

Cool, now the nested if-else logic is replaced! What do we learn from this?

1. Error handling if-else logic is encapsulated from the user.

2. User can use the defined data type `Just a` or `Nothing` to **lift** the information (correct or error) to upper Monad.

A programming language's type system deeply colors the way we think and write code in that language.
In the object-oriented kingdom, we use objects to encapsulate error messages, throw it out to try-catch block.
In contrast, you can use a type to build the try-catch block, and also encapsulate error or success state!



## Application 2: accumulate states

So far, we've seen that Monad can help us wrap common (usually nested) logic into a type.
User can **lift** the data to the upward Monad, and Monad will do the if-else logic for you.
Nevertheless, Monad can do more for you! `Maybe` Monad only passes the data to the next
user defined function, and here we introduce the `Writer` Monad, which will accumulate the
data you lift to it.

#### Writer Monad, type definition

Here is the type definition of Writer Monad:

```hs
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

Don't be scared! Writer Monad uses `Monoid` to concat the data you lift to it.
`Monoid` defines `mempty` and `mappend` functions. `mappend`, as it's name suggests, appends
the data you passed to it; and `mempty` is just an empty element.
`mappend` and `mempty` are just abstract ways to merge data together.

```
Prelude Data.Monoid> mappend [1,2,3] [4,5,6]
[1,2,3,4,5,6]
Prelude Data.Monoid> mappend "hello " "world"
"hello world"
Prelude Data.Monoid> mappend "hello " mempty
"hello "
```

The Writer Monad's definition simply takes the return value of user defined function `f`, extracts
the value `v'` out of it, and uses `mappend` to append the old `v` and new `v'`.

#### Writer Monad, in action

Let me take a modified example from [Learn You a Haskell for Great good][]. To log the process of deriving the greatest common divisor, we can do this:

[Learn You a Haskell for Great good]: http://learnyouahaskell.com/for-a-few-monads-more

```hs
import Control.Monad.Writer

gcd' :: [Int] -> Writer [String] [Int]
gcd' [a, b]
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return [a]
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return [b, (a `mod` b)]
```

The Writer Monad does not have a constructor like Maybe Monad does, so you need to use `tell` and `return` to build the Writer Monad and return it back. To show the result of the writer log, you'll also need function `runWriter`.

```hs
*Main> runWriter $ gcd' [8,3]
([3,2],["8 mod 3 = 2"])
*Main> runWriter $ gcd' [8,3] >>= gcd'
([2,1],["8 mod 3 = 2","3 mod 2 = 1"])
*Main> runWriter $ gcd' [8,3] >>= gcd' >>= gcd'
([1,0],["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0"])
*Main> 
```

Monad Lesson 2: the then operator `>>`
--------------------------------------

So, now we learned you can put different logic into a Monad to operate states. All the state passing is done by the bind operator `>>=`, which wraps the data and passes it to the downstream handler. However, Sometimes, we don't care about the wrapped value and just want to pass the state downstream. For example, performing side effects.

The definition of *then* operator `>>` looks like this:

```hs
class Monad m where
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
```

Unlike *bind* operator `>>=` which unwraps the value passed between user defined functions, *then* operator ignores the wrapped value (it uses `_` as variable) and only captures the states `x` and `y`.

What is the use case of this? Recall that Haskell is a purely a functional language that doesn't have states or variables; However, the Monad system can encapsulate hidden states, so the solution is to put a hidden `#realworld#` variable in `IO Monad` that records the standard input-output status, and also the file seek position, etc.

## Application 3: IO Monad

It's easier to understand in examples. Let's try the `putStrLn` function, which takes a `String`, and returns an `IO Monad`.

```hs
putStrLn :: String -> IO ()
```

Since it doesn't need to take things from standard input, we have no need to *bind* variables.
The usage is trivial:

```hs
*Main> putStrLn "hello, world"
hello, world
*Main> putStrLn "hello, world" >> putStrLn "baby"
hello, world
baby
```

With every `>>` operator, the `IO ()` Monad changes the `#realworld#` variable in the background. It's just like `Writer` Monad changes the accumulated states, but more abstract. For more details about the `IO ()` Monad, please visit [IO inside - HaskellWiki][].

### do notation

For convenience, Haskell provides us **do notation** to simplify the writing of *bind* and *then*, so instead of writing:

```hs
main = putStr "What is your name?"
       >> readLn
       >>= \a -> putStr "How old are you?"
       >> readLn
       >>= \b -> print (a,b)
```

you can write:

```hs
main = do putStr "What is your name?"
          a <- readLn
          putStr "How old are you?"
          b <- readLn
          print (a,b)
```

This example is taken from [IO inside - HaskellWiki][]. All the Monads above can be rewritten in this form. Hooray!

[IO inside - HaskellWiki]: http://www.haskell.org/haskellwiki/IO_inside


Monad Lesson 3: `return` and `fail`
-----------------------------------

We only have two functions left to describe Monad, which are `return` and `fail`. Actually, we already know them! The complete `Maybe` definition is:

```hs
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return              = Just
    fail _              = Nothing
```

The `return` function is the wrapper that we have used so far, and `fail` is the function to represent, as you can guess, failure. The definition of `return` and `fail` in Monad is:

```hs
class Monad m where
  return :: a -> m a
  fail :: String -> m a
```

`fail` can take an additional string to report the failure message. With *bind*, *then*, *return*, and *fali* functions, we then know the whole definition of the Monad type!

Monad Lesson 4: Beyond states
-----------------------------

So far, we introduced `Maybe`, `Writer`, and `IO ()` Monads which use states heavily, but Monad can do more than abstract states. Remember, the core of Monad type is to wrap around logic. One of the applications it can do is define work flow. Here comes the [Eval Monad][] to the rescue; let's take a look on the example in the online book:

[Eval Monad]: http://chimera.labs.oreilly.com/books/1230000000929/ch02.html#fig_rpar_rpar

```hs
runEval $ do
   a <- rpar (f x)
   b <- rpar (f y)
   return (a,b)
```

Once you wrap the function `f` in `Eval` Monad, it generates the logic to evaluate the code in parallel. You can further use the combination of the `rpar` and `rseq` functions defined by `Eval`, to construct the dependencies of each individual job. Check the link for more details!

Another nontrivial example of Monad is [Parsec parser combinator library][]. In this case, the wrapped logic is used for generating a parser from smaller components. Read [Write Yourself a Scheme in 48 hours][scheme48] to explore this decent Monad!

[Parsec parser combinator library]: http://www.haskell.org/haskellwiki/Parsec

[scheme48]: http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours


Conclusion
----------

I started to learn Haskell a while ago, but I found explaining what a Monad type is in a easy way was extremely difficult. The final approach I choose was to break it down into four functions and explore them through examples. 
I found this helped me understand it better, and I hope it helps you out as well!

Once you understand what the Monad type is, you can easily use for all sorts of Haskell libraries on the fly. In the Haskell world, Monad is THE design pattern that is used for almost every important library. Functional programming introduced a different way to convolute the logic and data. It's also elegant and powerful. I hope you enjoyed reading this post!

Any comments are welcome. ;)

References
---------

* [Monads in small bites][]

* [IO inside - HaskellWiki][]

* [Learn You a Haskell for Great good][]

* [Eval Monad][]

* [Parsec parser combinator library][]

* [Write Yourself a Scheme in 48 hours][scheme48]

[Monads in small bites]: http://www.leonardoborges.com/writings/2012/12/08/monads-in-small-bites-part-iv-monads/
