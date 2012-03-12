---
layout: post
title: "Writing math equations on Octopress"
date: 2012-03-10 16:23
comments: true
categories: [Octopress, Math]
---

Octopress is a wonderful blogging framework for hackers, but it does not support
math typesetting by default. What hacker doesn't use Latex math?


The followings are configs to make you write latex math equations in Octopress.
Then you can translate this

{% codeblock lang:latex %}
$$
\begin{align}
\mbox{Union: } & A\cup B = \{x\mid x\in A \mbox{ or } x\in B\} \\
\mbox{Concatenation: } & A\circ B  = \{xy\mid x\in A \mbox{ and } y\in B\} \\
\mbox{Star: } & A^\star  = \{x_1x_2\ldots x_k \mid  k\geq 0 \mbox{ and each } x_i\in A\} \\
\end{align}
$$
{% endcodeblock %}

into this

$$
\begin{align}
\mbox{Union: } & A\cup B = \{x\mid x\in A \mbox{ or } x\in B\} \\
\mbox{Concatenation: } & A\circ B  = \{xy\mid x\in A \mbox{ and } y\in B\} \\
\mbox{Star: } & A^\star  = \{x_1x_2\ldots x_k \mid  k\geq 0 \mbox{ and each } x_i\in A\} \\
\end{align}
$$

**whala!**

* * *

Here are the instructions:
--------

1. Use [kramdown](http://kramdown.rubyforge.org/) instead of rdiscount


kramdown is a free GPL-licensed Ruby library for parsing and converting a
superset of Markdown. It is completely written in Ruby, supports standard
Markdown and latex math equations.

~~~~
gem install kramdown
~~~~

2. Change settings in `_config.yml`

`_config.yml` is configurations for Jekyll's settings. find `markdown` and
change `rdiscount` to `kramdown`

3. Change `gem 'ridiscount'` to `gem 'kramdown` in `Gemfile`[^1].

4. Put [MathJax](http://www.mathjax.org/) CDN and configs in
`source/_layouts/default.html`.

{% codeblock lang:html %}
<!-- mathjax config similar to math.stackexchange -->
<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  jax: ["input/TeX", "output/HTML-CSS"],
  tex2jax: {
    inlineMath: [ ['$', '$'] ],
    displayMath: [ ['$$', '$$']],
    processEscapes: true,
    skipTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
  },
  messageStyle: "none",
  "HTML-CSS": { preferredFont: "TeX", availableFonts: ["STIX","TeX"] }
});
</script>
<script src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML" type="text/javascript"></script>
{% endcodeblock %}

5. Fix [MathJax](http://www.mathjax.org/) right-click bug

I found the solution on [luikore's blog](http://luikore.github.com/2011/09/good-things-learned-from-octopress/). The problem was when you right-click on a MathJax formula, the whole browser becomes white. To fix it, open `sass/base/_theme.scss` and change the div under body from

{% codeblock lang:scss %}
body {
  > div {
    background: $sidebar-bg $noise-bg;
{% endcodeblock %}

to

{% codeblock lang:scss %}
body {
  > div#main {
    background: $sidebar-bg $noise-bg;
{% endcodeblock %}

and it is done.


[^1]: It's useful if you want to depoloy your code on differnt machines.
