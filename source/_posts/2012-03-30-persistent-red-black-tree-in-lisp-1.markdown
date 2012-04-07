---
layout: post
title: "persistent red black tree in lisp (1)"
date: 2012-03-30 13:57
comments: true
categories: Lisp, Algorithm
---

I'm a newbie to Lisp programing language. Lisp is one of the most oldest
programming language that is still being used today. People have a lot of
defences on programming languages, because there are too many choices to us
today. Every language has their strength and weakness. It is not easy to make
the choice.

In my opinion, the best way to decide what languages I must learn is to
see which kinds of people are loving it. 
For example, Ruby community does great supports for business plans; Python is
the best open sourced language for academia uses (scientific libraries, machine
learning, computer vision, statistics, visualizations...etc.) Many old school
hackers (including me) still love to use Perl; Java is widely used in big
companies; VBA is friendly for dealing excel data...etc. And **Lisp**, are
highly recommended by great hackers. 

<!-- more -->

* * *

If you are also interested in Lisp, I recommend [Practical Common
Lisp][clbook1] written by [Peter Seibel][peter], and [On Lisp][onlisp] written
by [Paul Graham][graham]. Both of them are free, downloadable PDF files, and
there are also online version and epub format.

* * *

### Basic thoughts

Let's begin with basic structures. At first, I design the structure similar to
what you will write in C code.

{% codeblock lang:cl %}
(defstruct rb
  left
  right
  red
  data)
{% endcodeblock %}

Here we can see the power of Lisp. It does not only create the structure, but
also create constructor and accessors for the structure. Now you can create the
`rb` and access it like so:

{% codeblock lang:cl %}
(setq node
  (make-rb :left nil :right nil :red T :data 3))

==> #S(RB :left nil :right nil :red T :data 3)

(rb-left node)

==> nil

(rb-data node)

==> 3

(setf (rb-right node) (make-rb :data 5)) ; Other field are default as nil

==> #S(RB :left nil 
          :right #S(RB :left nil :right nil :red nil :data 5) 
          :red T :data 3)
{% endcodeblock %}

This seems to be nice. But we can rewrite it in more lispy style:

{% codeblock lang:cl %}
(defstruct rb
  (red T) ; default value for red
  data)

;; A node can be represented as
;; (nil #S(RB :red T :data 3) nil)
;; To generate this:
(list nil (make-rb :data 3) nil)

==> (nil #S(RB :red T :data 3) nil)

;; You can also use lisp syntax candy
`(list nil ,(make-rb :data 3) nil)

==> (nil #S(RB :red T :data 3) nil)
{% endcodeblock %}

The s-expression with a preceding `` ` `` means that it is a expression that
lisp don't evaluate, except expression inside it with a preceding `,`. It is
convenient for us to generate new list object.

To access the elements in a list:

1. `car` the first
2. `cadr` the second
3. `caddr` the third

And now we can wirte a persistant basic binary insert function!

{% codeblock lang:cl %}
(defun binary-insert-r (root data)
  (cond ((null root)                            ; cond works like "switch"
          `(nil ,(make-rb :data data) nil))     ; make node when reach leaf
        ((= data (rb-data (cadr root))) root)   ; return self if data is the same
        ((> data (rb-data (cadr root)))
         (list (car root)                       
               (cadr root) 
               (binary-insert-r (caddr root) data))) ; case insert to right
        (T
         (list (binary-insert-r (car root) data) 
               (cadr root)
               (caddr root)))))                      ; case insert to left

(defparameter *tree* nil)                     ; global variable *tree*
(defun binary-insert (data)
  (setf *tree* (binary-insert-r *tree* data)) ; set *tree* as the returned tree
  *tree*)                                     ; return *tree* value

{% endcodeblock %}

Let's exam the function:
{% codeblock lang:cl %}
BINARY-INSERT-R
CL-USER> (binary-insert 6)
(NIL #S(RB :RED T :DATA 6) NIL)
CL-USER> (binary-insert 7)
(NIL #S(RB :RED T :DATA 6) (NIL #S(RB :RED T :DATA 7) NIL))
CL-USER> (binary-insert 5)
((NIL #S(RB :RED T :DATA 5) NIL) #S(RB :RED T :DATA 6)
 (NIL #S(RB :RED T :DATA 7) NIL))
CL-USER> (binary-insert 8)
((NIL #S(RB :RED T :DATA 5) NIL) #S(RB :RED T :DATA 6)
  (NIL #S(RB :RED T :DATA 7) (NIL #S(RB :RED T :DATA 8) NIL)))
{% endcodeblock %}

### Wait! Hold on!

The code is god damn ugly! We can fix it with some utilities:

{% codeblock lang:cl %}
(defun kid (root &rest dirs)
  (cond ((consp dirs)
         (if (car dirs)
             (apply #'kid (caddr root) (cdr dirs))
             (apply #'kid (car root) (cdr dirs))))
        (T root)))

(defun node-data (root)
  (rb-data (cadr root)))
{% endcodeblock %}

The `kid` takes a node and a series of `T` and `nil`; When it saw a `T`, it
returns the right node, or it will return the left one. For example:

{% codeblock lang:cl %}
CL-USER> (kid *tree* nil)            ; left node
(NIL #S(RB :RED T :DATA 5) NIL) 
CL-USER> (kid *tree* T)              ; right node
(NIL #S(RB :RED T :DATA 7) (NIL #S(RB :RED T :DATA 8) NIL))
CL-USER> (kid *tree* T T)            ; right right node
(NIL #S(RB :RED T :DATA 8) NIL)
{% endcodeblock %}

Using `nil` and `T` as left and right dirctions, it is easier for us to rewrite
`binary-insert-r` because we can change left and right cases into variables.

{% codeblock lang:cl %}
(defun binary-insert-r (root data)
  (cond ((null root) `(nil ,(make-rb :data data) nil))
        ((= data (node-data root)) root)
        (T (let* ((dir (> data (node-data root)))
                  (a (kid root (not dir)))
                  (b (binary-insert-r (kid root dir) data)))
             (if dir
                 (list a (cadr root) b)
                 (list b (cadr root) a))))))
{% endcodeblock %}

[clbook1]: http://www.gigamonkeys.com/book/
[peter]: http://www.gigamonkeys.com/
[onlisp]: http://paulgraham.com/onlisp.html
[graham]: http://paulgraham.com/
