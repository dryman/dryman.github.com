---
layout: post
title: "persistent red black tree in lisp (2)"
date: 2012-03-31 09:22
comments: true
categories: Lisp, Algorithm
---

### Single rotate

Using the `kid` utility, we can make rotate single to be simpler too:
{% codeblock lang:cl %}
;;       x              y
;;     /   \          /   \
;;    y     c   =>   a     x       
;;  /   \                 / \
;; a     b               b   c
;;
(defun rotate-s (root dir)
  (let ((x (cadr root))
        (y (cadr (kid (not dir))))
        (a (kid (not dir) (not dir)))
        (b (kid (not dir) dir))
        (c (kid dir)))
    (if dir
        (list a y (list b x c))
        (list (list c x b) y a))))
{% endcodeblock %}

Observe that the two return form is nested reversed. Why not write a macro that
generate this form? Then we only need to write the right case!

### Reverse the tree

First we need to write a function that take a nested form and return it in
reversed order (also nested.) To achieve this, we use double recursion.

{% codeblock lang:cl %}
(defun tree-to-rev (tree acc)
   (cond ((null tree) acc)
         ((consp (car tree))
          (tree-to-rev (cdr tree)
               (cons (tree-to-rev (car tree) nil) acc)))
         (T (tree-to-rev (cdr tree) (cons (car tree) acc)))))

CL-USER> (tree-to-rev '(a b (c d e)) nil)
((E D C) B A)
{% endcodeblock %}

Recall that we need to add `list` at the beginning of returned form. So rewrite
the function as

{% codeblock lang:cl %}
(defun tree-to-rev-list (tree acc)
   (cond ((null tree) (cons 'list acc))
         ((consp (car tree))
          (tree-to-rev-list (cdr tree)
               (cons (tree-to-rev-list (car tree) nil) acc)))
         (T (tree-to-rev-list (cdr tree) (cons (car tree) acc)))))


(defun tree-to-list (tree acc)
   (cond ((null tree) (cons 'list (reverse acc)))
         ((consp (car tree))
          (tree-to-list (cdr tree)
               (cons (tree-to-list (car tree) nil) acc)))
         (T (tree-to-list (cdr tree) (cons (car tree) acc)))))

CL-USER> (tree-to-rev-list '(a b (c d e)) nil)
(LIST (LIST E D C) B A)

CL-USER> (tree-to-list '(a b (c d e)) nil)
(LIST A B (LIST C D E))
{% endcodeblock %}

### Macro that simplify the return form

Now we write a macro that we only need to pass it the right case:

{% codeblock lang:cl %}
(defmacro mtree-expand (dir tree)
  `(if ,dir
      ,(tree-to-list tree nil)
      ,(tree-to-rev-list tree nil)))

CL-USER> (macroexpand-1 '(mtree-expand dir (a y (b x c))))
(IF DIR
    (LIST A Y (LIST B X C))
    (LIST (LIST C X B) Y A))
{% endcodeblock %}

Now the `insert-binary-r` and `rotate-s` can be re-written only in right form!

{% codeblock lang:cl %}
;;       x              y
;;     /   \          /   \
;;    y     c   =>   a     x       
;;  /   \                 / \
;; a     b               b   c
;;
(defun rotate-s (root dir)
  (let ((x (cadr root))
        (y (cadr (kid (not dir))))
        (a (kid (not dir) (not dir)))
        (b (kid (not dir) dir))
        (c (kid dir)))
    (mtree-expand dir (a y (b x c)))))

(defun insert-binary-r (root data)
  (cond ((null root) `(nil ,(make-rb :data data) nil))
        ((= data (node-data root)) root)
        (T (let* ((dir (> data (node-data root)))
                  (x (cadr root))
                  (a (kid root (not dir)))
                  (b (binary-insert-r (kid root dir) data)))
             (mtree-expand dir (a x b))))))
{% endcodeblock %}

### Macro that simplify the input form

We also want to simplify that ugly `let` form, so we create this function and
macro:

{% codeblock lang:cl %}
(defun gen-let (dir node form dirs)
  (if (atom form)
    `((,form (apply #'kid ,node ,(cons 'list (reverse dirs)))))
    (cons `(,(cadr form) 
            (cadr (apply #'kid ,node ,(cons 'list (reverse dirs)))))
          (append (gen-let dir node (car form) (cons `(not ,dir) dirs))
                  (gen-let dir node (caddr form) (cons dir dirs))))))

(defmacro mtree-let (dir node form let-var &body body)
   `(let ,(gen-let dir node form nil)
       (let* ,let-var
           ,@body)))
{% endcodeblock %}

And then you can rewrite `rotate-s`:

{% codeblock lang:cl %}
(defun rotate-s (node dir)
  (mtree-let dir node ((a y b) x c)
    nil
    (mtree-expand (a y (b x c)))))
{% endcodeblock %}

See? We don't need to write the comment to remind us the relative position of
variables. The code express itself! You can see what this code will expand
to by expanding the macro:

{% codeblock lang:cl %}
CL-USER> (macroexpand-1 '(mtree-let dir node ((a y b) x c)
                          nil
                          (mtree-expand dir (a y (b x c)))))
(LET ((X (CADR (APPLY #'KID NODE (LIST))))
      (Y (CADR (APPLY #'KID NODE (LIST (NOT DIR)))))
      (A (APPLY #'KID NODE (LIST (NOT DIR) (NOT DIR))))
      (B (APPLY #'KID NODE (LIST (NOT DIR) DIR)))
      (C (APPLY #'KID NODE (LIST DIR))))
  (LET* ()
    (MTREE-EXPAND DIR (A Y (B X C)))))
{% endcodeblock %}

