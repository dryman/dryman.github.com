---
layout: post
title: "The recursion theorem"
date: 2012-03-12 08:42
comments: true
categories: [Theorem of comptation]
---

I'm studying theorem of computation myself. Theorem of computation is an
interesting field. It addresses questions as: What is a theorem? What is a
proof? What is truth? Can an algorithm decide which statements are true? Can
a computer calculates everything in the universe? These questions are linked by
the question:

> What are the fundamental capabilities and limitations of computers?

This question goes back to the 1930s when mathematical logicians first began to
explore the meaning of computation. Thus, three major theorem of computation has
born: *automata*, *computability*, and *complexity*. There are a lot of algebras
and proofs in this field. Of all the theorems, I love the **recursion
theorem** the most.

* * *

### SELF-REFERENCE

First we introduce a Turing Machine that *ignores its input and prints out a
copy of its own description*. We call this machine $SELF$.

**LEMMA**
: There is a computable function $q: \Sigma^\ast\longrightarrow\Sigma^\ast$,
: where if $w$ is any string, $q(w)$ is the description of a Turing machine $P_w$
: that prints out $w$ and then halts.

**PROOF**
: The following TM $Q$ computes $q(w)$


$$
\begin{align}
   Q=& \mbox{On input string $w$:} \\
     & 1.\; \mbox{Construct the following Turing machine $P_w$} \\
     & \quad    P_w = \mbox{On any input:} \\
     & \qquad   1.\; \mbox{Erase input.} \\
     & \qquad   2.\; \mbox{Write w on the tape.} \\
     & \qquad   3.\; \mbox{Halt.}  \\
     & 2.\; \mbox{Output }\langle P_w\rangle  \\
\end{align}
$$

Now we construct $SELF$ in two parts $A$ and $B$. We want $SELF$ to print out
$\langle SELF\rangle = \langle AB\rangle$.

$$
\begin{align}
A=&P_{\langle B\rangle}, \\
B=&\mbox{On input $\langle M\rangle$, where $M$ is a portion of a TM:}\\
  & 1.\;\mbox{Compute $q(\langle M\rangle)$.} \\
  & 2.\;\mbox{Combine the result with $\langle M\rangle$ to make a complete TM.} \\
  & 3.\;\mbox{Print the description of this TM and halt.} \\
\end{align}
$$
