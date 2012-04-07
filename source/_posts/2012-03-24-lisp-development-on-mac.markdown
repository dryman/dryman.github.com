---
layout: post
title: "Lisp development on Mac"
date: 2012-03-24 16:46
comments: true
categories: Lisp
---

Setting up a development environment is such a pain. In short, here is my final
configs:

* [Aquamacs emacs][aquamacs]
* [SLIME] without SLDB mode
* [clisp] with debugger commands working
* [quicklisp][quicklisp] (lisp package manager)
  * [asdf.lisp] that ships with [quicklisp][quicklisp] (Bridge between lisp and
    SLIME)

<!-- more -->

I also tried many other combinations. For example, [sbcl] should be a faster
alternative to [clisp], but I can't make its debugger work. [sbcl] also support
better linking to slime and sldb, but sldb isn't working well. [Emacs 24] seems to
be a better emacs environment because it has package manager built right into
it. However, I tired [emacs starter-kit] and slime installation through its
package manager, and it just don't work. Any suggestions would be greatly
welcome. ;)

* * *

### Instructions

1. Download [osx-gcc] if you haven't.
2. Install [homebrew].
3. `brew install clisp`.
4. Setting up [quicklisp].
    1. Download [quicklisp.lisp][quicklisp]
    2. `clisp -i quicklisp.lisp`
    3. `[1]> (quicklisp-quickstart:install)`
    4. `[2]> (ql:add-to-init-file)`
    5. `[3]> (quit)`
5. Download Aquamacs emacs
6. Install [Aquamacs emacs SLIME plugin][slime plugin]
7. `cat '(setq inferior-lisp-program "/usr/local/bin/clisp")' >> ~/.emacs`

Then you can use `trace`, `step` and other debug utilities in emacs SLIME. Enjoy
it!

* * *

### Update

I found out how to enable `STEP-MODE` in [sbcl] form [this post](http://www.lispforum.com/viewtopic.php?f=2&t=628). 
In short, put this in your `.sbclrc`.

{% codeblock lang:cl %}
(declaim (optimize (debug 3)))
{% endcodeblock %}

and in your `.emacs`

{% codeblock lang:cl %}
(setq inferior-lisp-program "/usr/local/bin/sbcl")
{% endcodeblock %}

[aquamacs]: http://aquamacs.org/
[SLIME]: http://common-lisp.net/project/slime/
[clisp]: http://www.clisp.org/
[quicklisp]: http://www.quicklisp.org/beta/
[asdf.lisp]: http://www.cliki.net/asdf
[sbcl]: http://www.sbcl.org/
[osx-gcc]: https://github.com/kennethreitz/osx-gcc-installer
[homebrew]: http://mxcl.github.com/homebrew/
[slime plugin]: http://braeburn.aquamacs.org/plugins/
[Emacs 24]: http://emacsformacosx.com/builds
[emacs starter-kit]: https://github.com/technomancy/emacs-starter-kit
