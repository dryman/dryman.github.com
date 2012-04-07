---
layout: post
title: "persistent red black tree in lisp (4)"
date: 2012-04-01 14:48
comments: true
categories: Lisp, Algorithm
---

### Okasaki's purely functional red black tree

The peristent red black tree in last post works ok, but the rotate functions and
color flip is not efficient for purely functional data sturctures.
In 1999, Okasaki introduced a new way to balance the insertion, and the function
only takes care of four unbalanced cases.

~~~~
Chris Okasaki,
"Red-Black Trees in a Functional Setting",
Journal of Functional Programming, 9(4),
pp471-477,
July 1999
~~~~

The algorithm is easy to present in Haskell code:

{% codeblock lang:hs Okasaki's red black tree insertion http://www.mew.org/~kazu/proj/red-black-tree/ Copied from Kazu Yamamoto's website%}
data RBTree a = Leaf | Fork Color (RBTree a) a (RBTree a)
data Color = R | B

insert :: Ord a => a -> RBTree a -> RBTree a
insert a b = Fork B d e f
  where
    Fork _ d e f = ins a b
    ins x Leaf = Fork R Leaf x Leaf
    ins x t@(Fork c l y r) = case compare x y of
        LT -> balanceL c (ins x l) y r
        GT -> balanceR c l y (ins x r)
        EQ -> t

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (Fork R (Fork R a x b) y c) z d = Fork R (Fork B a x b) y (Fork B c z d)
balanceL B (Fork R a x (Fork R b y c)) z d = Fork R (Fork B a x b) y (Fork B c z d)
balanceL k a x b                           = Fork k a x b

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B a x (Fork R b y (Fork R c z d)) = Fork R (Fork B a x b) y (Fork B c z d)
balanceR B a x (Fork R (Fork R b y c) z d) = Fork R (Fork B a x b) y (Fork B c z d)
balanceR k a x b 
{% endcodeblock %}

<!-- more -->

Cool! This can be even reduced to only two cases in our `mtree-expand` and
`mtree-let` macro!

{% codeblock lang:cl %}
(defun balance-o (root dir)
  (mtree-let dir root (a x (b y (c z d)))
             ((z-b (to-b z)))
             (mtree-expand dir ((a x b) y (c z-b d)))))

(defun balance-i (root dir)
  (mtree-let dir root (a x ((b z c) y d))
             ((y-b (to-b y)))
             (mtree-expand dir ((a x b) z (c y-b d)))))

(defun insert-oka (root data)
  (declare (type fixnum data))
  (cond ((null root) `(nil ,(make-rb :data data) nil))
        ((= (node-data root) data) root)
        (T (let* ((dir (> data (node-data root)))
                 (a (kid root (not dir)))
                 (b (insert-oka (kid root dir) data))
                 (root (if dir (list a (cadr root) b) (list b (cadr root) a))))
            (cond ((is-red root) root)
                  ((and (is-red b) (is-red (kid b dir))) (balance-o root dir))
                  ((and (is-red b) (is-red (kid b (not dir)))) (balance-i root dir))
                  (T root))))))

(defparameter *tree* nil)
(defun rb-insert (data)
  (declare (type fixnum data))
  (setq *tree* (insert-oka *tree* data)))
{% endcodeblock %}

To test the running time:
{% codeblock lang:cl %}
(time (loop for i from 1 to 1000000 do (rb-insert i)))

;; The original algorithm using rotate-s and rotate-d
  19.796 seconds of real time
  18.570645 seconds of total run time (17.423563 user, 1.147082 system)
  [ Run times consist of 3.043 seconds GC time, and 15.528 seconds non-GC time. ]
  93.81% CPU
  47,299,915,377 processor cycles
  6,522,724,144 bytes consed

;; The Okasaki's algorithm
  13.005 seconds of real time
  12.193227 seconds of total run time (11.213534 user, 0.979693 system)
  [ Run times consist of 2.513 seconds GC time, and 9.681 seconds non-GC time. ]
  93.76% CPU
  31,073,022,018 processor cycles
  4,278,336,384 bytes consed
{% endcodeblock %}

## Conclusions

Lisp is designed for bottom-up programming. You first draft what you want to do,
then you can start to write some functions and macros to simplify it. When there
are more and more utilities you written, you can use it to experiment more
complicated algorithms, in a more elegant and self expressive style.

In purely functional structure, Haskell code  seems to be more elegant because
it has built in pattern matching, while we have to write one for Lisp. But Lisp
provides things more than functional programming, it can also be written in
procedure style, object-oriented style, or any other DSL that is best suitable
for your objective.

The macro system in lisp can also improve your thinking of designing a program.
Because you can always abstract your program structure as you writing it. In
other language you are trained to think top-down, while in lisp you are
encouraged to think back and forth. This process can shorten required time to
get enough experiences of programming. You don't need a lot experiences to build
a complex algorithm in a bottom-up design process. It's just come up naturally.


