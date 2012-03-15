---
layout: post
title: "The NLP Day"
date: 2012-03-14 19:55
comments: true
categories: NLP
---

Yesterday (3/13) was the NLP day, and we have the honor to invite Eduard Hovy to
give us three talks about the current status and future of NLP researches.
The talks were excellent! The first talk was about how machine learning perform
so well then previous AI systems. It seems that computer can be more intelligent
not because it is smart, but to be trained trained by huge amount of data. The
second talk was "A new semantics: merging propositional and distributional
information". Eduard introduced a new model that combine theories and
computability that can be used in machine learning. The third talk was "Text
harvesting and ontology constructing using a powerful new method." Using a
simple "$N_p \mbox{ such as } N_p \mbox{ and } N_p * $" query and search on Google
seems to be trivial, but the results are awesomely incredible! Thus we can mark
the "is-a" relationship automatically and the data is satisfying and robust,
plus, much more than any relationships in existing wordnets! There are so many
interesting topics to write down, but for mow, I'll focus on the second talk
first. You can find the slide for the second talk 
[here](http://projects.ict.usc.edu/rwt2011/presentations/hovy.pdf).

Though machine learning told us that "you don't have to be smart, you just need
enough training data," deep in our heart, we still believe that all of our human
behaviors is not a huge training table. There must be some rules that guide us,
as a theory, to know what we will do and what wouldn't. We believe that there
are theories that can measure info contents, not only treat input strings as
meaningless characters that the only purpose is to be sent to machine to do
statistical analysis.

### Defining a concept in a new way

A concept $C$ is a list of triples

$$
C=\left\{(r_1w_1s_1),(r_2w_2s_2),(r_3w_3s_3)\right\}
$$

$$
\begin{align}
\mbox{where } &r_i\varepsilon\;\{Relations\}=&\mbox{e.g., :subj, :agent, :color-of}\\
& w_i\varepsilon\;\{Words\}=&\mbox{e.g., happy, run, apple}\\
& s_i\varepsilon\;[0,1]=&\mbox{normalized weight}
\end{align}
$$

For example, a dog can be represented as

{% codeblock lang:scheme %}
(defparameter *dog*
  '((:type "Jack Russel" 0.2)
    (:type "Retriever" 0.4)
    (:color "brown" 0.4)
    (:color "black" 0.3)
    (:agent-of "eat" 0.4)
    (:patient-of "chase" 0.3)))
{% endcodeblock %}

And it can be expressed in a more complexed form:

{% codeblock lang:scheme %}
(defparameter *dog*
  '((:type (("Retriever" 0.4) ("Jack Russel" 0.2) ("Terrier" 0.4)))
    (:color (("brown" 0.4) ("black" 0.3) ("patched" 0.3) ("white" 0.2)))
    (:name (("Spot" 0.3) ("Lassie" 0.2)))
    (:agent-of (("eat" 0.4) ("run" 0.4) ("bark" 0.4) ("pant" 0.3)))
    (:patient-of (("chase" 0.3) ("walk" 0.4) ("love" 0.4 )))))
{% endcodeblock %}
