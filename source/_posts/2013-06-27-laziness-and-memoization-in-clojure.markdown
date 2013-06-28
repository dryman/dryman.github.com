---
layout: post
title: "Laziness and memoization in Clojure"
date: 2013-06-27 20:01
comments: true
categories: Clojure
---

I'm now having a job at supplyframe Inc., and luckily I can use Clojure for work! Clojure is a young language created by Rich Hickey on 2007. It uses Lisp syntax, immutable data structures by default, and supports both strict and lazy evaluations. As Christ Okasaki suggested:

> Strict evaluation is useful in implementing worst-case data structures and lazy evaluation is useful in implementing amortized data structures.

It's really cheap to define lazy or strict data structures in Clojure that has low amortized cost even in a persistent manner. Let's dig into the source code and see how does Clojure implement it.

<!--more-->

Before we go into Clojure's java source code, we can first look at the `memoize` function.

## Memoize

```clj
(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))
```

The function is elegant; it captures whatever arguments that pass in the function, and pairs the arguments and the returned value to a persistent map. In order to make it thread safe, the persistent map is cast into an `atom` and can be modified via `swap!`.

This is nice. And since Clojure uses $\log_{32}(N)$ hash map, it is also fast enough to do a memoized lookup. For the implementation of Clojure's persistent hash map, you can check out [this post][].

[this post]: http://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice/

## lazy-seq

In contrast to `memoize`, which is implemented in Clojure, `lazy-seq` is implemented in java. It contains three fields:

```java
public final class LazySeq extends Obj implements ISeq, Sequential, List, IPending, IHashEq{

private IFn fn;
private Object sv;
private ISeq s;
```

`fn` is an un-evaluated thunk (function without arguments), and `sv` is the captured value after executing the thunk. The `ISeq s` is the realized version of the sequence.

When the program tries to realize the lazy sequence, it calls `seq()` and `sval()` functions.

```java
final synchronized Object sval(){
	if(fn != null)
		{
		try
			{
			sv = fn.invoke();
			fn = null;
			}
		catch(RuntimeException e)
			{
			throw e;
			}
		catch(Exception e)
			{
			throw Util.sneakyThrow(e);
			}
		}
	if(sv != null)
		return sv;
	return s;
}

final synchronized public ISeq seq(){
	sval();
	if(sv != null)
		{
		Object ls = sv;
		sv = null;
		while(ls instanceof LazySeq)
			{
			ls = ((LazySeq)ls).sval();
			}
		s = RT.seq(ls);
		}
	return s;
}
```

In the `sval()` function, Clojure handles the caching elegantly. If `fn` is not null, execute it and stores the value to `sv`. If the `LazySeq` is dereferenced, the whole object will be recycled by the garbage collector, else the object will hold the value and is thread-safe to be accessed by other threads.

The `seq()` function is the wrapper around `sval()`. It realizes all `LazySeq` objects recursively, and wrap it into a `seq` object that implements `ISeq` interface.

With the realized `seq`, it can support common sequence functions like `first` and `next`:

```java
public Object first(){
	seq();
	if(s == null)
		return null;
	return s.first();
}

public ISeq next(){
	seq();
	if(s == null)
		return null;
	return s.next();	
}
```

## Conclusion

In Clojure's documentation, it said that `lazy-seq` is cached after realizing it, but it didn't document how it does it. Luckily the source code is pretty easy to understand. Clojure uses lazy sequences a lot, so knowing that it handles lazy sequence efficiently is important for all Clojure programmers. :) 
