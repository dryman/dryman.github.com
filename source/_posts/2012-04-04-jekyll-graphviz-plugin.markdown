---
layout: post
title: "jekyll graphviz plugin"
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
