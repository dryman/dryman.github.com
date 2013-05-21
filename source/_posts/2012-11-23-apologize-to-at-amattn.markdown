---
layout: post
title: "apologize to @amattn"
date: 2012-11-23 15:10
comments: true
published: false
categories: 
---

My last post [Effective Obj-C ARC and Pitfalls in It][effect objc] had some
content copied directly from [@amattn][amattn]'s blog post 
[ARC best practices][best practices] without referencing it. More specifically
is the section **ARC and Objective-C properties**. I've now replaced the section
with direct link to [@amattn][amattn]'s blog and an toggled quote cite to
his post.

<!--more-->

[@amattn][amattn] did an awesome pioneer work on discovering great patterns when
you code Objective-C with ARC. It is really hard to write an article about ARC
without using the concepts in [ARC best practices][best practices]. We all
talked about:

* Common pattern of specifying Objective-C properties
* ARC type qualifiers
* Toll free bridging
* NSError and `__autoreleasing`
* Block and `__weak` reference
* `__autoreleasing` and CGColor

[ARC best practices][best practices] has detailed examination of how each
statement works. My article focused on correct format of type qualifiers.

Still, I have to mention that Clang will compile your code without diagnostics when
you put type qualifier in the wrong place. In the runtime, the weak objects and
auto release objects may not behave as you might think it is. This is really
dangerous and super hard to debug. Imagine:

```obj-c
__weak MyClass* w_self = self;
self.block = ^{
    MyClass* s_self = w_self;
    ...
};
```

You thought it should work, and Clang didn't report any diagnostic about it. But
it does cause a retain cycle, and thus a memory leak. The correct format is
replace the line with `MyClass* __weak w_self = self`. I have a detailed 
[article for type qualifiers][type], I recommend everyone to take a look at it.

I really appreciate [@amattn][amattn] and his work.  We both share our knowledge
to the public because the passion of programming. I feel really sorry of my
mistake for copying his work without referencing his post.

[effect objc]: http://www.idryman.org/blog/2012/11/22/arc-best-practices-and-pitfalls/
[amattn]: https://twitter.com/amattn
[best practices]: http://amattn.com/2011/12/07/arc_best_practices.html
[type]: http://www.idryman.org/blog/2012/10/29/type-qualifiers/
