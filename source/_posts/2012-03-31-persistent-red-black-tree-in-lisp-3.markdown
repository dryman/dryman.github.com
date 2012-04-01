---
layout: post
title: "persistent red black tree in lisp (3)"
date: 2012-03-31 16:33
comments: true
categories: Lisp, Algorithm
---

## Red black tree algorithms

There are two good articles that have good explanation on red-black tree
algorithms. For your references:

1. [Julienne Walker's red black tree tutorial][jsw rbtree]
2. [Kazu Yamamoto's purely functional left-leaning red black trees][fp rbtree]

Julienne wrote a beautiful review article that show us not only the algorithm of
red black tree, but *how it is designed like so*. He also implemented an elegant
C program that can balance the tree in bottom-up or top-down ways. My previous
function `(kid root dir dir)` was inspired from his implementation.

Kazu reorganized several red black tree insertion algorithms, including Chris
Okasaki's purely functional way. He also introduced a left-leaning insertion
algorithm that reduces one pattern matching compare to Okasaki's one. The
programs are elegantly written in Haskell.

### Orinal red black tree algorithm

In 1979, Guibas and Sedgewick published the original imperative red black trees:

~~~~
Leo J. Guibas and Robert Sedgewick.
"A dichromatic framework for balanced trees",
In Proceedings of the 19th Annual Symposium on Computer Science,
pp8-21,
IEEE Computer Society,
1978
~~~~

The original one has eight *unbalanced* cases to deal with, while two are
reduced in "Introduction to Algorithms". The algorithm was derived from
symmetric binary B-tree (2-3-4 tree) which was suggested by Rudof Bayer. All
paths from the root to a leaf in a SBB-tree contains the same number of nodes,
thus make it a perfectly balanced tree. However, it is not a binary search tree.
So Rober Sedgewick and Leonidas Guibas came up with a mnemonic abstraction that
can use red-nodes and black-nodes of a binary tree to simulate SBB-Tree. This is
how the algorithm is formed. To know the details, see 
[Julienne's guide][jsw rbtree].

Julienne modified the original bottom up algorithm to a no parent pointers
style:

{% codeblock Julienne's red black tree insert implementation lang:c %}
struct jsw_node *jsw_single ( struct jsw_node *root, int dir )
{
  struct jsw_node *save = root->link[!dir];

  root->link[!dir] = save->link[dir];
  save->link[dir] = root;

  root->red = 1;  // Note that there's color changing here!
  save->red = 0;

  return save;
}

struct jsw_node *jsw_double ( struct jsw_node *root, int dir )
{
  root->link[!dir] = jsw_single ( root->link[!dir], !dir );
  return jsw_single ( root, dir );
}

struct jsw_node *jsw_insert_r ( struct jsw_node *root, int data )
{
  if ( root == NULL )
    root = make_node ( data );
  else if ( data != root->data ) {
    int dir = root->data < data;

    root->link[dir] = jsw_insert_r ( root->link[dir], data );

    if ( is_red ( root->link[dir] ) ) {
      if ( is_red ( root->link[!dir] ) ) {
        /* Case 1 Color flip */
        root->red = 1;
        root->link[0]->red = 0;
        root->link[1]->red = 0;
      }
      else {
        /* Cases 2 & 3 */
        if ( is_red ( root->link[dir]->link[dir] ) )
          root = jsw_single ( root, !dir );
        else if ( is_red ( root->link[dir]->link[!dir] ) )
          root = jsw_double ( root, !dir );
      }
    }
  }

  return root;
}

int jsw_insert ( struct jsw_tree *tree, int data )
{
  tree->root = jsw_insert_r ( tree->root, data );
  tree->root->red = 0;
  return 1;
}
{% endcodeblock %}

### Implementation in Lisp

Julienne's bottom-up algorithm can be easily to be re-written in purely
functional style. The ugly part is the color flipping and assign new branches to
nodes.

Though we can do the color flipping as

{% codeblock lang:cl %}
(setf (rb-red (cadr root)) nil
      (rb-red (cadr a)) T
      (rb-red (cadr b)) T)
{% endcodeblock lang:cl %}

but the tree would be non-persistent. So we need to create new node with new
property of red or black.

I also separate the cases into two function `color-flip` and
`rb-insert-case-rest`. Thus the code would be easier to debug and profile.

{% codeblock lang:cl %}
(defun rotate-s (node dir)
  (mtree-let dir node ((a y b) x c)
            ((x-r (to-r x)) (y-b (to-b y)))
            (mtree-expand dir (a y-b (b x-r c)))))

(defun rotate-d (node dir)
  (mtree-let dir node (a x b)
            ((a-new (rotate-s a (not dir))))
            (rotate-s (mtree-expand dir (a-new x b)) dir)))

;; Color flipping utilities 
(defun to-r (x) (make-rb :data (rb-data x) :red T))
(defun to-b (x) (make-rb :data (rb-data x) :red nil))

(defun color-flip (root dir)
  (mtree-let dir root ((a y b) x (c z d))
             ((x-r (to-r x))
              (y-b (to-b y))
              (z-b (to-b z)))
             (mtree-expand dir ((a y-b b) x-r (c z-b d)))))

(defun rb-insert-case-rest (root dir)
  (cond ((is-red (kid root dir dir)) (rotate-s root (not dir)))
        ((is-red (kid root dir (not dir))) (rotate-d root (not dir)))
        (T root)))

(defun rb-insert-r (root data)
  (declare (type fixnum data))
  (cond ((null root) `(nil ,(make-rb :data data) nil))
        ((= data (node-data root)) root)
        (T (let ((dir (> data (node-data root))))
             (mtree-let dir root (a x b) 
               ((b (rb-insert-r b data))
                (root (mtree-expand dir (a x b))))
               (if (is-red b)
                 (if (is-red a)
                   (color-flip root dir)            ; case 1
                   (rb-insert-case-rest root dir))  ; case 2 and 3
                 root))))))


(defparameter *tree* nil)
(defun rb-insert (data)
  (declare (type fixnum data))
  (let* ((ret (rb-insert-r *tree* data))
         (a (car ret))            ;; Set the root node to be black
         (x-b (to-b (cadr ret)))
         (b (caddr ret)))
    (setf *tree* (list a x-b b))
    *tree*))
{% endcodeblock %}

When the cases in function are separated, it is easy to tell how the program is being called:

{% codeblock lang:cl %}
CL-USER> (trace rb-insert-r rb-insert-case-rest color-flip rotate-s rotate-d)

CL-USER> (rb-insert 3)
  0: (RB-INSERT-R ((NIL 5-b NIL) 6-b (NIL 7-b (NIL 8-r NIL))) 3)
    1: (RB-INSERT-CASE-REST ((NIL 3-r NIL) 5-b NIL) NIL)
    1: RB-INSERT-CASE-REST returned ((NIL 3-r NIL) 5-b NIL)
  0: RB-INSERT-R returned (((NIL 3-r NIL) 5-b NIL) 6-b (NIL 7-b (NIL 8-r NIL)))

CL-USER> (rb-insert 100)
  0: (RB-INSERT-R (((NIL 3-r NIL) 5-b NIL) 6-b (NIL 7-b (NIL 8-r NIL))) 100)
    1: (RB-INSERT-CASE-REST (NIL 8-r (NIL 100-r NIL)) T)
    1: RB-INSERT-CASE-REST returned (NIL 8-r (NIL 100-r NIL))
    1: (RB-INSERT-CASE-REST (NIL 7-b (NIL 8-r (NIL 100-r NIL))) T)
      2: (ROTATE-S (NIL 7-b (NIL 8-r (NIL 100-r NIL))) NIL)
      2: ROTATE-S returned ((NIL 7-r NIL) 8-b (NIL 100-r NIL))
    1: RB-INSERT-CASE-REST returned ((NIL 7-r NIL) 8-b (NIL 100-r NIL))
  0: RB-INSERT-R returned
       (((NIL 3-r NIL) 5-b NIL) 6-b ((NIL 7-r NIL) 8-b (NIL 100-r NIL)))
{% endcodeblock %}

[jsw rbtree]: http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx
[fp rbtree]: http://www.mew.org/~kazu/proj/red-black-tree/
