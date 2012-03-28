---
layout: page
title: "Elementary Graph Algorithms"
date: 2012-03-28 11:28
comments: true
sharing: true
footer: true
---


### Breadth-first search


**BFS(G,S)**

$$
\begin{align}
& \mbox{for each vertex } u \in G.V-\{s\}\\
& \qquad u.color = WHITE\\
& \qquad u.d = \infty\\
& \qquad u.\pi = NIL\\
& s.color = GRAY\\
& s.d = 0 \\
& s.\pi = NIL\\
& Q=\emptyset\\
& ENQUEUE(Q,s)\\
& \mbox{while } Q\neq \emptyset\\
& \qquad u = DEQUEUE(Q)\\
& \qquad \mbox{for each } v\in G.Adj[u] \\
& \qquad\qquad \mbox{if } v.color == WHITE \\
& \qquad\qquad\qquad v.color=GRAY \\
& \qquad\qquad\qquad v.d=u.d+1\\
& \qquad\qquad\qquad v.\pi = u \\
& \qquad\qquad\qquad ENQUEUE(Q,v) \\
& \qquad u.color = BLACK
\end{align}
$$

Breadth-first-search can discover shortest paths in a graph.

**Shortest path distance**$\delta(s,v)$ from $s$ to $v$ is the minimum number of
edges in any path from vertex $s$ to vertex $v$.

### Lemma
Let $G=(V,E)$ be a directed or undirected graph, and let $s\in V$ be an arbitary
vertex. Then, for any edge $(u,v) \in E$,

$$
\delta(s,v)\leq\delta(s,u)+1
$$

Note that $u\rightarrow v$ or $u-v$. If $s$ can reach $u$, $s$ can reach $v$. If
$\delta(s,u)=\infty$, the inequality holds.

* * *

### Depth-first search

**DFS(G)**

$$
\begin {align}
&\mbox{for each vertex } u\in G.V \\
&\qquad u.color = WHITE \\
&\qquad u.\pi = NIL \\
&time = 0 \\
&\mbox{for each vertex } u \in G.V \\
&\qquad \mbox{if } u.color == WHITE \\
&\qquad\qquad \mbox{DFS-VISIT}(G,u)\\
&\end{align}
$$

**DFS-VISIT(G,u)**

$$
\begin{align}
& time = time + 1 \\
& u.d = time \\
& u.color = GRAY \\
& \mbox{for each } v\in G.Adj[u] \\
& \qquad\mbox{if } v.color == WHITE \\
& \qquad\qquad v.\pi = u \\
& \qquad\qquad \mbox{DFS-VISIT}(G,v) \\
& u.color = BLACK \\
& time = time + 1 \\
& u.f = time \\
\end{align}
$$

**Corollary Nesting of descendants' intervals**
Vertex $v$ is a proper descendant of vertex $u$ in the depth-first forest for a
(directed or undirected) graph $G$ if and only if $u.d < v.d < v.f < u.f$.

### Classification of edges

1. **Tree edges** are edges in the depth-first forest $G_\pi$, Edge $(u,v)$ is a
   tree edge if $v$ was first discovered by exploring edge $(u,v)$.
2. **Back edges** are those edges $(u,v)$ connecting a vertex $u$ to an ancestor $v$
   in a depth-first tree. We consider self-loops, which may occur in directed
   graphs, to be back edges.
3. **Forward edges** are those non-tree-edges $(u,v)$ connecting a vertex $u$ to
   a descendant $v$.
4. **Cross edges** are all other edges.

{% img /images/graph-theory/Tree_edges.svg %}
<!-- wikipedia -->

* * *

### Topological sort

A **topological sort** of a dag $G=(V,E)$ is a linear ordering of all its
vertices such that if $G$ contains an edge $(u,v)$, then $u$ appears before $v$
in the ordering. *If the graph contains a cycle, then no linear ordering is
possible.*

**TOPOLOGICAL-SORT**

$$
\begin{align}
& 1.\;\mbox{Call DFS(G) to compute finishing times $v.f$ for each vertex $v$.}\\
& 2.\;\mbox{insert it onto the front of a linked list as each vertex is finished.}\\
& 3.\;\mbox{return the linked list.}\\
\end{align}
$$

{% img /images/graph-theory/topoSortProfBumstead.gif %}

* * *

## Strongly connected components

Vertices $u$ and $v$ are reachable from each other.

**SRONGLY-CONNECTED-COMPONENTS**

$$
\begin{align}
& 1.\;\mbox{DFS(G) to compute finishing times $u.f$ for each vertex $u$} \\
& 2.\;\mbox{compute }G^T \\
& 3.\;\mbox{call DFS}(G^T)\mbox{, but in order of decreasing }u.f \\
& 4.\;\mbox{output each tree in the forest} \\
\end{align}
$$
