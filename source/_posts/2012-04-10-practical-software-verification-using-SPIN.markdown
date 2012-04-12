---
layout: post
title: "Practical software verification using SPIN"
date: 2012-04-10 10:31
comments: true
categories: Talk
---

This is a note for the talk [New frontiers in formal software verification][talk] 
spoken by [Gerard J. Holzmann][holz], software verification architect for [NASA
Jet Propulsion Laboratory (JPL)][jpl]. He introduced a fast automatic software
verification tool for distributed software system -- [SPIN][spin].

{% graphviz %}
    
    digraph {
      ltl [label="LTL\n requirement"];
      logic [label="Logical\n negation"];
      model [label="Model\n extractor"];
      c [label="C code"];
      abs [label="Abstraction\n map"];
      env [label="Environment\n model"];
      automata [label="Omega\n automation"];
      bug [label="Bug report" shape=doublecircle];

      ltl -> logic;
      logic -> model;
      c -> abs -> model;
      env -> model;
      model -> automata;
      automata -> bug;
    }
{% endgraphviz %}
<!-- more -->

### Background

After some ~30 years of development, formal software verification is still
rarely used on industrial applications. A typical computer science verification
proof would take 300+ of pages, much more than other mathematical proofs.

It is considered to be too difficult, and takes too long (months to years) to
verify a software (like UNIX, IBM360, garbage collectors...etc.) Even in safety
critical applications, software verification is often restricted to the
verification of models of software, instead of software.

>   Goal: Make software verification as simple as testing, and as fast as
>   compilation.

### Practical software verification

[Holzmann][holz] is the author of [SPIN][spin], a popular open-source software
tool, used for the formal verification of distributed software systems. It has
been applied at different fields:

* Commercial phone switch
* Spacecraft for Mars mission
* Flood Control 
* Toyota Camry MY05
* Medical device transmission protocols

Back in 1999, SPIN can verify 4 million lines of code of phone switch system
within 40 minutes. And now it is used in spacecraft system and many other
mission critical softwares.

### PathStar switch (1999)

SPIN was designed for PathStar switch, it is

* A commercial data/phone switch designed in Bell Labs research (for Lucent
  Technologies)
* Newly written code for the core call processing engine
* The first commercial call processing code that is formally verified
* After the product is released, in decades every updates of the product 
  code was proofed in fully automated procedure

In a complex feature precedence relations detecting **undesired feature**
interaction is a serious problem. The verification system has to deal with:

* feature interaction
* feature breakage
* concurrency problems
* race conditions
* deadlock scenarios
* non compliance with legal requirements...etc.

SPIN breaks the whole verification problem into five steps[^1] :

{% graphviz %}
    digraph {
        code    [label="ANSI C code"];
        spin    [label="SPIN"];
        abs     [label="Abstraction Map"];
        context [label="Context"];
        feature [label="Feature requirements"];
        bug     [label="Bug reports, property violations"];
        user    [label="Users/Developers"];

        code -> spin     [label=" 1. build model"];
        spin -> abs      [label="2"];
        spin -> context  [label="3"];
        spin -> feature  [label="4"];
        spin -> bug      [label="5"];
        bug  -> user;
    }
{% endgraphviz %}

#### Building the model

The first step is one of the hardest part in SPIN system. SPIN is not a silver
bullet that can analysis any input of C code. They designed a subset of ANSI C
with [some rules][manual], to make the program as a distributed finite state
machine.

For example, the following code

{% codeblock lang:c %}
    @dial:
      switch(op) {
      default: goto error; /* unexpected input */
      case Crdtmf:         /* digit collector ready */
        x->drv->progress(x, Tdial);
        time = MSEC(16000); /* set timmer and wait event */

      /* continue here */
      @dial1: switch(op) {
        default: goto error;
        case crconn: goto B@lb;
        case Cronhook: /* caller hangs up */
          x->drv->disconnect(x);
      @dial2:  if(op!=Crconn && op!= rdis)
            goto Aidle;
            // ...etc
      }
    }
{% endcodeblock %}

can generate this FSM:

{% graphviz %}
    digraph {
      dial->dial1 [label="Crdtmf"];
      dial1->dial1 [label="Cronn"];
      dial1->dial2 [label="Cronhook"];
      dial->error [label="else"];
      dial1->error [label="else"];
      dial2->error [label="else"];
    }
{% endgraphviz %}

### Abstraction and context of SPIN model

To verify the code we convert it into an automation: a labeled transition
system. The labels (transitions) are the basic statements from C.
Each statement can be converted via an abstraction -- which is encoded as a
lookup table. While analysing the code, SPIN doesn't care how the C code runs,
but deal with the abstracted FSM and tokenized input. This is why we call it
a "model analyser."

The context of a SPIN looks like this:

{% graphviz %}
    
    digraph {
      ltl [label="LTL\n requirement"];
      logic [label="Logical\n negation"];
      model [label="Model\n extractor"];
      c [label="C code"];
      abs [label="Abstraction\n map"];
      env [label="Environment\n model"];
      automata [label="Omega\n automation"];
      bug [label="Bug report" shape=doublecircle];

      ltl -> logic;
      logic -> model;
      c -> abs -> model;
      env -> model;
      model -> automata;
      automata -> bug;
    }
{% endgraphviz %}


Note that the *environment model* is not that hard to define. You just need to
declare that how many outlets of a switch: switch-to-switch, telephone inputs,
the amount of switches...etc.

### The linear time temporal logic formulae (LTL)

For example, a requirement statement[^2]:

>  When the phone goes *offhook*, a *dialtone* should occur.

It can be expressed in LTL:

    !( !offhook U (offhook /\ X [] (!dialtone /\ !onhook)) )

* U $\bigcup$ is strong until
* X $\bigcirc$ is next
* [] $\Box$ is always[^3]

You can also express it in formal symbols:

$$
    \neg\left(\neg offhook \bigcup 
    \left( offhook \wedge \bigcirc \;\Box \left(\neg dialtone \bigcirc \neg onhook
    \right)\right)\right)
$$


If the requirement changed:

>   Assume, an $event_i$ should be added in between *offhook* and *response*

In ITL:

    X((eventi /\ !onhook) U (eventi /\ !onhook))

In formal logic:

$$
    \bigcirc \left(\left(event_i \wedge \neg onhook\right) \bigcup 
    \left(event_i \wedge \neg onhook\right)\right)
$$

Once we have every thing prepared, SPIN will analysis the context using the
$\omega-automation$ technique and find out in what situations will cause bugs.


## Fast verification

Traditionally we uses breadth first search or depth first search to iterate the
execution model for finding bugs. Since the execution is a non-determinism
model, the possible paths are in exponential order thus will cause stack
overflow before we find bugs.

Holzmann introduced [swarm verification preparation script][swarm] that can
separate the problem into small pieces of jobs and can be executed in parallel.

> Swarm generates a script that performs many small verification jobs in
> parallel, that can increase the problem coverage for very large verification
> problems by about an order of magnitude compared to standard bitstate
> verification runs. It is meant to be used on models for which standard
> verification with exhaustive, bitstate, hash-compaction etc. either runs out
> of memory, or takes more time than is available (e.g., days or weeks). Swarm
> uses parallelism and search diversification to reach its objectives.

> The user can use a configuration file to define how many processing cores are
> available, how much memory can be used, and how much time is maximally
> available, among a range of other optional parameter settings. Based on this
> information, swarm generates the script that runs as many independent jobs as
> possible in parallel, without exceeding any of the user-defined constraints.
> Swarm can run jobs using local CPU cores or remote machines in a grid network.

* * *

Holzmann also showed us swarm's performance on NASA/JDL's code:

* 11 bugs reports (50% of total) after 1 seconds. Faster than compilation!
* 10 seconds and 90% of bugs are found.

That is truly amazing!

## Conclusion:

Human brains are not perfect. It is easy to convince ourselves something is true
even it isn't. A verification system like SPIN is not a project in ivory towel.
Instead, it can reproduce critical bugs in zero-failure missions (like mission to
Mars) or serious business programs. 

Distributed system is a really hard problem and it is playing a important role
in cloud computing and parallel systems. For now we have bunch of map reduce
frameworks and no-SQL databases, but sometimes they just fails. How do we use a
good algorithm to check the consistency of the data? How do we verify our model
is correct? With SPIN, we can now verify the code within seconds ,and no more bug
tracing at 3:00AM. :D

* * *

[talk]: http://www.iis.sinica.edu.tw/page/events/FILE/120409.pdf
[holz]: http://spinroot.com/gerard/
[jpl]: http://www.jpl.nasa.gov/index.cfm
[mars]: http://www.jpl.nasa.gov/missions/details.cfm?id=5918
[spin]: http://spinroot.com/spin/whatispin.html
[manual]: http://spinroot.com/spin/Man/Manual.html
[swarm]: http://spinroot.com/swarm/index.html
[^1]: All the plots are generated by my [graphviz plugin]({{ root_url }}/blog/2012/04/04/jekyll-graphviz-plugin/ )
[^2]: The example is referenced from [this slide](http://www.sable.mcgill.ca/~ebodde/mcs/timeline.ppt)
[^3]: Check [SPIN ITL reference](http://spinroot.com/spin/Man/ltl.html)

