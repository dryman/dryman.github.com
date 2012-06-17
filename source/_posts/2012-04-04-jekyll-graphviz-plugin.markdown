---
layout: post
title: "Jekyll/Octopress graphviz plugin"
date: 2012-04-04 17:37
comments: true
categories: Ruby, Octopress
---

I created a graphviz plugin for Octopress/Jekyll today. For example:

{% raw %}
    {% graphviz %}
    digraph G {
      compound=true;
      subgraph cluster0 {
      a -> b;
      a -> c;
      b -> d;
      c -> d;
      }
      subgraph cluster1 {
      e -> g;
      e -> f;
      }
      b -> f [lhead=cluster1];
      d -> e;
      c -> g [ltail=cluster0, lhead=cluster1];
      c -> e [ltail=cluster0];
      d -> h;
    }
    {% endgraphviz %}
{% endraw %}

will produce:

{% graphviz %}
digraph G {
  compound=true;
  subgraph cluster0 {
  a -> b;
  a -> c;
  b -> d;
  c -> d;
  }
  subgraph cluster1 {
  e -> g;
  e -> f;
  }

  b -> f [lhead=cluster1];
  d -> e;
  c -> g [ltail=cluster0, lhead=cluster1];
  c -> e [ltail=cluster0];
  d -> h;
}
{% endgraphviz %}

<!-- more -->

You can see how it works on real post:

1. [Practical Software Verification Using SPIN][spin]
2. [Recursion Best Practices][recursion]

* * *

### Installation:

1. Download the file from [here](https://github.com/dryman/dryman.github.com/blob/src/plugins/graphviz_block.rb).
2. Put it into your `my_octopress/plugin` folder.
3. Add one line `source/images/graphviz` into your `.gitignore` file

Remeber that you should have graphviz installed in your system. Else it will
raise an error.

[spin]: http://www.idryman.org/blog/2012/04/10/practical-software-verification-using-SPIN/
[recursion]: http://www.idryman.org/blog/2012/04/14/recursion-best-practices/
