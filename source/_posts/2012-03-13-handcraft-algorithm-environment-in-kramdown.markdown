---
layout: post
title: "handcraft algorithm environment in kramdown"
date: 2012-03-13 09:13
comments: true
categories: [Octopress, Math]
---

I love $\LaTeX$. The typesetting in $\LaTeX$ is so beautiful that that no other
system can compete with. However, I cannot write $\LaTeX$ for blog post, I need
to use [kramdown](http://kramdown.rubyforge.org/) or other wiki/markup languages.

I missed some features in $\LaTeX$. One of those is [Algorithm and Pseudocode
packages](http://en.wikibooks.org/wiki/LaTeX/Algorithms_and_Pseudocode).
So, I discovered some tricks to form algorithm-like typesettings.

1. Use `\begin{align}` to align the lines.
2. Use `\mbox{text...}` to put descriptions in.
3. Use `\quad` and `\qquad` to make indentions.

Here is the example input:

{% codeblock lang:latex%}
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
{% endcodeblock %}

and **Voilà**!

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

