---
layout: post
title: "Writing a damn fast hash table with tiny memory footprints"
date: 2017-05-03 18:26
comments: true
categories: C, HashTable, Algorithm
---

Hash table is probably *the* most commonly used data structure in
software industry. Most implementations focus on its speed instead
of memory usage, yet small memory footprint has significant impact
on large in-memory tables and database hash indexes.

In this post, I"ll provide a step by step guide for writing a modern
hash table that optimize for both speed and memory efficiency. I'll
also give some mathematical bounds on how well the hash table could
achieve, and shows how close we are to the optimal.

<!--more-->

Let me start with a disclaimer. I now work at google, and this project
(OPIC including the hash table implementation) is approved by google
[Invention Assignment Review Committee][iarc] as my personal
project. The work is done only in my spare time with my own machine
and does not use and/or reference any of the google internal resources.

[iarc]: https://opensource.google.com/docs/iarc/

## Common hash table memory usages

As mentioned earlier, most hash hash table focus on its speed, not
memory usage. Consequently there's not much benchmark compares the
memory these hash table implementation consumes. Here is a very basic
table for some high performance hash table I found. The input is 8 M
key-value pairs; size of each key is 6 bytes and size of each value is
8 bytes. The lower bound memory usage is $(6+8)\cdot 2^{23} =$ 117MB
. Memory overhead is computed as memory usage divided by the
theoretical lower bound. Currently I only collect 5 hash table
implementations. More to be added in future.

|                    | Memory Usage | 　Memory Overhead | 　Insertion Time | 　Query Time |
|--------------------|-------------:|-----------------:|---------------:|------------:|
| std::unordered_map |         588M |            5.03x |      2.626 sec |  2.134 sec |
| sparse_hash_map    |         494M |            4.22x |      7.393 sec |  2.112 sec |
| dense_hash_map     |        1280M |           10.94x |      1.455 sec |  1.436 sec |
| libcuckoo          |         708M |            6.05x |      2.026 sec |  2.120 sec |
| klib khash         |         642M |            5.48x |      4.232 sec |  1.647 sec |


* * *

The metrics above actually surprises me. For example,
[sparse hash map][shm] is advertised to use 4-10 bits per entry,
but the overhead is actually 4 times the lower bound. If the
hash table were implemented as large key-value store index, and
you have 1 TB of data, you'll need at least 4-5TB of space to
hold the data. That's not very space efficient. Can we do better?

## Overview of hash table types

There's two major types of hash table, one is [chaining][chaining] and
the other is [open addressing][open_addr]. Chaining is quite common
in most standard libraries, where the collision is handled by
appending items into a linked list headed by the bucket the key is
mapped to.  Open addressing uses a different mechanism to handle
collision: the key (and value) is inserted to another bucket if the
bucket it attempt to insert is already occupied.

Open addressing has some clear advantages over chaining. First, it
does not require extra memory allocation. This reduces memory allocation
overhead and can possibly improve cpu caching. Moreover, in open
addressing the developer has more control on memory layout -- placing
elements in buckets with certain order to make probing (search on
alternative location for key) fast. Best of all, open addressing
gives us better memory lower bound over chaining.

The hash collision rate affects the chaining memory usage.  Given a
hash table with $N$ buckets, we insert $M$ elements into the
table. The expected collision number in the table is $M(1 - (1 -
1/N)^{M-1})$. For a table with 1000 buckets the expected collisions
under high loads ($M/N > 80%$) are:

* 80% -> 440
* 90% -> 534
* 100% -> 632

![collision](/images/collision_rate.png)

Accounting the extra payload that chaining requires, we can now compute
the lower bound for the overhead under different loads.

| load | 　Chaining | 　Open Addressing |
|-----:|-----------:|------------------:|
| 100% |      1.31x |             1.00x |
|  90% |      1.37x |             1.11x |
|  80% |      1.47x |             1.25x |
|  70% |      1.60x |             1.42x |
|  50% |      2.09x |             2.00x |
|  25% |      4.03x |             4.00x |

Here I assume if the collision rate were 60%, half of it is chained
and half of it fits the buckets. The actual number may have some
digits off, but it doesn't change my conclusion on choosing open
addressing for hash table implementation.

## Probing methods

In open addressing, hash collisions are resolved by probing, a search
through alternative buckets until the target record is found, or some
failure criteria is met. The following all belongs to some kinds of
probing strategies:

* Linear Probing
* Quadratic Probing
* Double Hashing
* Hopscotch Hashing
* Robin Hood Hashing
* Cuckoo Hashing

For each of the probing method, we're interested in their worst case
and average case probing numbers, and is their space bound. 

### Linear Probing and Quadratic Probing

Linear probing can be represented as a hash function of a key and a
probe number $h(k, i) = (h(k) + i) \mod N$. Similarly, quadratic
probing is usually written as $h(k, i) = (h(k) + i^2) \mod N$. Both
methods has worst case probing count $O(N)$, and are bounded on
space usage. In other words, there no condition where we need to
increase the bucket count and rehash.

### Double hashing

Double hashing can be written as
$h(k, i) = (h1(k) + i \cdot h2(k)) \mod N$.
Same as linear probing and quadratic probing, it has worst
case probing count $O(N)$, and is bounded on space usage.

### Hopscotch Hashing

Here is the algorithm copied from [wikipedia][hopscotch].
This is how the collision is handled

>  If the empty entry's index j is within H-1 of entry i, place x
>  there and return. Otherwise, find an item y whose hash value lies
>  between i and j, but within H-1 of j. Displacing y to j creates a
>  new empty slot closer to i. If no such item y exists, or if the
>  bucket i already contains H items, resize and rehash the table.

This mechanism has a good worst case probing number $O(H)$. However,
since it could resize the hash table, the hash table size is unbounded.

### Robin Hood Hashing

The concept for robin hood hashing is simple and clever. When
a collision occur, compare the two items' probing count, the one
with larger probing number stays and the other continue to probe.
Repeat until the probing item finds an empty spot. For more detailed
analysis checkout [the original paper][rhh]. It's worth to read.

The expected probing length is

$$
\begin{align}
E\lbrack i \rbrack & = \frac{n}{m}\left(\sum_{x=1}^n\frac{1}{x} -
\sum_{x=1}^{n-m}\frac{1}{x}\right) \\
& = \frac{n}{m}\left(H_n - H_{n-m} \right) \\
& \approx \frac{n}{m}\ln\left(\frac{1}{1-\frac{m}{n}}\right)
\end{align}
$$


![psl](/images/psl.png)

Even under a high load, we still get very good probing numbers.
The best thing about robin hood hashing is it does not expand
the hash table, which is important because we want to build a
hash table with bounded size. This is the probing strategy I chose.

### Cuckoo hashing

The following description is also copied from [wikipedia][cuckoo].

> It uses two or more hash functions, which means any key/value pair
> could be in two or more locations. For lookup, the first hash
> function is used; if the key/value is not found, then the second hash
> function is used, and so on. If a collision happens during insertion,
> then the key is re-hashed with the second hash function to map it to
> another bucket.

The expected probing number is below 2. However, the load factor has
to be below 50% to achieve good performance. For using 3 hash functions,
the load can increase to 91%. Combining linear/quadratic probing with
cuckoo, the load factor can go beyond 80%. (All numbers comes from
wikipedia).

## Optimizing Division for Hash Table Size

I implemented a robin hood hashing prototype a month ago. The prototype
satisfy the low memory footprint, but hard to get it fast. The major
reason is the modulo operation is very slow on most platforms. For example,
on Intel Haswell the `div` instruction on 64bit integer can take 32-96
cycles. Almost all major hash implementation use power of 2 table size,
so that the modulo is just one bitwise and operation. The problem with
power of 2 table size is it scales too fast! If our data size is 1 bit
above 2GB, the table must be at least 4GB, giving us 50% load. Finding
a fast alternative modulo operation is critical for creating a table
with high load without loosing much performance.

Professor Lemire is probably the first person that addresses this issue.
He wrote a blog post that provides [a fast alternative to modulo][fastscale].

```c
uint32_t reduce(uint32_t x, uint32_t N) {
  return ((uint64_t) x * (uint64_t) N) >> 32 ;
}
```

He named this method as *fast range*. Another intuitive way to think
about it is **scaling**. Number $x$ ranges $\lbrack 0, 2^{32}-1\rbrack$,
multiplying it by $N$ then divide by $2^{32}$, the range becomes
$\lbrack 0, N\rbrack$.

There's one big problem to apply *fast range* on probing. Probing
usually add the probe bias to *lower bits* of the hashed key. Modulo
and bitwise and preserves the lower bits information, but *fast range*
only use the *higher bits* and the probe would have no effect on the
output. The first bits where it can bias the output in *fast range* is
$\frac{2^{32}}{N}$. Hence, writing a linear probing using *fast range*
would be:

```c
uint32_t fast_range_probing(uint32_t hashed_key, uint32_t probe, uint32_t N)
{
  return ((uint64_t)hashe_key + ((uint64_t)probe << 32)/N) * N >> 32;
}
```

To make the output correct we used division again, which makes it slow.
Is there a better way?

### Fast mod and scale

I created an alternative method with a more relaxed condition.
Instead of finding a fast modulo replacement for **all N**, I want to
find **some N** that satisfy fast modulo and can preserve the biases
of probing.

The actual algorithm is pretty simple: First, mask the hashed key to
the next power of 2 boundary, then multiply it by
$\frac{N}{16}, N=8..15$. This is a combination of traditional power
of 2 modulo and professor Lemire's scaling method. The difference is
now the scale can only get up to 2 times. In other words, only the least
significant bit will get omitted when scaling. The probing
implementation can be written as:

```c
static inline uintptr_t
hash_with_probe(RobinHoodHash* rhh, uint64_t key, int probe)
{
  uintptr_t mask = (1ULL << (64 - rhh->capacity_clz)) - 1;

  // linear probing
  // uint64_t probed_hash = key + probe * 2;

  // quadratic probing
  uint64_t probed_hash = key + probe * probe * 2;

  // Fast mod and scale
  return (probed_hash & mask) * rhh->capacity_ms4b >> 4;
}
```

This is [the straight copy][fastmodandscale] of my robin hood hash
implementation.  When the probe is scaled by 2 it is guaranteed to
have biases on the output. The mask can be derived from leading zeros
of the capacity `capacity_clz`, the scale is defined by the most
significant 4 bits of the capacity `capacity_ms4b`. The `capacity_ms4b`
is pre-computed on hash table creation or resizing time. It's a round
up of desired capacity with finer granularity compare to power of 2
tables.

I used [Intel Architecture Code Analyzer][iaca] to analyze the instruction
throughput of my methods, and the result is very satisfying:

* Power of 2 table with quadratic probing
  - Block Throughput: 4.10 Cycles
  - Total Num Of Uops: 9
* Fast mod and scale with quadratic probing
  - Block Throughput: 4.15 Cycles
  - Total Num Of Uops: 12

## Benchmarks

I hope all these analysis didn't bored you all! Turns out these analysis
are all useful. We now have a hash table with very optimal memory usage
but still having great performance.

![Memory usage](/images/short_key_mem.png)

![Insert time](/images/short_key_insert_time.png)

![Lookup time](/images/short_key_lookup_time.png)

The most impressive part is the memory usage. Under load 89% we
achieve overhead 1.20x ~ 1.50x. The ideal overhead should be 1.12 but
we have an extra byte used per bucket to determine whether the bucket
is emptied or tumbstoned.

The insertion time is not as good as `dense_hash_map` under high load.
The reason is robin hood hashing moves the buckets around during the
insert, but `dense_hash_map` simply probe and insert it to an empty
bucket if found.

Luckily, robin hood hashing gets a faster lookup time compare to
`dense_hash_map`. I think the major reason is robin hood hashing
results a great expected probing number, and the overall throughput
benefits from it.

The benchmark code is available at [hash_bench](https://github.com/dryman/hash_bench). My robin hood
hashing implementation is available at [opic robin hood hashing][opic].

## Summary

Hash table implementations has been focused on its speed over memory
usages. Turns out we can sacrifice some insertion time to gain way
better memory utilization, and also improve the look up time. I believe
this can be the new state of the art implementation for hash tables.
Let me know what you think in the comments. :)

Many details were omitted in this post, but will be discussed on my
next post. Some outlines for the things I'd like to cover would be

* Probe distributions under different probing strategies (linear probing, quadratic probing, double hashing, and some probing methods I created).
* Optimize probing by using gcc/clang vector extensions
* Deletion mechanisms, its performance, and how it affects probe distributions.
* Serialization and deserialization performance
* Performance with different popular hash functions
* Benchmark with other robin hood implementations
* Benchmark with other embedded key-value store.

I may not be able to cover all the above in my next post, so please
put down your comment to let me know what do you want to read the next.

## One more thing...

This robin hood hashing is implemented using my project
[Object Persistence In C (OPIC)][opic]. OPIC is a new general serialization
framework I just released. Any in-memory object created with OPIC can
be serialized without knowing how it was structured. Deserializing
objects from OPIC only requires one mmap syscall. That's say, this
robin hood implementation can work not only for a living process, the
data it stored can be used as a key-value store after the process exits.

Right now, the throughput of OPIC robin hood hash map on small keys (6bytes)
is 9M (1048576/0.115454). This is way better than most
[NoSQL key-value stores][nosql_bench]. Of course, OPIC robin hood hash
map does not support transactions or concurrent writes. I probably
should compare it to other embedded databases instead of general
purpose NoSQL key-value store instead.

[chaining]: https://en.wikipedia.org/wiki/Hash_table#Separate_chaining
[open_addr]: https://en.wikipedia.org/wiki/Open_addressing
[hopscotch]: https://en.wikipedia.org/wiki/Hopscotch_hashing
[rhh]: https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf
[cuckoo]: https://en.wikipedia.org/wiki/Cuckoo_hashing
[fastscale]: http://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction/
[fastmodandscale]: https://github.com/dryman/opic/blob/master/opic/hash/robin_hood.c#L155
[iaca]: https://software.intel.com/en-us/articles/intel-architecture-code-analyzer
[opic]: https://github.com/dryman/opic 
[nosql_bench]: http://www.datastax.com/nosql-databases/benchmarks-cassandra-vs-mongodb-vs-hbase
