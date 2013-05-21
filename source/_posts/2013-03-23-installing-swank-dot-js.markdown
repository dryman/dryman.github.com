---
layout: post
title: "Installing swank.js"
date: 2013-03-23 08:05
comments: true
categories: emacs
---

People say that emacs is the most extendable editor in the world; however, I didn't really understand how powerful it is until one of it extension **Swank.js** blowed my mind. Take a look at this awesome screen cast produced by [emacs rocks][emacs rocks].

<iframe width="560" height="315" src="http://www.youtube.com/embed/qwtVtcQQfqc" frameborder="0" allowfullscreen></iframe>

<!--more-->

Awesome, is it? However, installing `swank.js` is really nontrivial. As a result, I want to share my note on setting up `swank.js` on my machine.

System information
------------------

* Mac OSX 10.8
* Emacs 24.3
* package manager: [homebrew][homebrew]
* C compiler: Apple LLVM version 4.2 (clang-425.0.27) (based on LLVM 3.2svn) Target: x86_64-apple-darwin12.3.0

My Emacs is installed via [homebrew][homebrew].

```sh
brew install emacs --cocoa
```

Installation steps
------------------

### 1. 
Setting up emacs ecosystem. In `~/.emacs` or `~/.emacs.d/init.el`:

```scm
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
'("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Remember to evaluate the buffer `M-x eval-buffer`.

### 2. 
`node.js`, `npm`, and `swank-js` server.

```sh
brew install npm
export PATH="/usr/local/share/npm/bin:$PATH"    # put this in .bash_profile
npm install -g swank-js
```

### 3. 
Setting up `slime` (lisp-emacs protocol)

```sh
brew install sbcl
cd ~/.emacs.d/
git clone https://github.com/antifuchs/slime.git
```

In `.emacs`:

```scm
(add-to-list 'load-path "~/.emacs.d/slime")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup)
```

### 4. 
Install `swank.js` dependencies in emacs:

~~~
M-x package-install exec-path-from-shell
M-x package-install js2-mode
M-x package-install slime
M-x package-install slime-js
M-x package-isntall js2-refactor
M-x package-install ac-slime
~~~

### 5. 
Useful configurations written by [Magnar Sveen](https://github.com/magnars) (author of [emacs rocks][emacs rocks].

```sh
cd ~/.emacs.d/
curl -LO https://raw.github.com/magnars/.emacs.d/master/setup-slime-js.el
```

### 6. 
Final configurations in `.emacs`

```scm
;; swank-js settings
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
(load-file "~/.emacs.d/setup-slime-js.el")
```

Usage
-----

First, you need a real server running at port 3000. You can do that either using `python -m SimpleHTTPServer 3000` or other server programs like Ruby on Rails. You can change the default port setting by overwritten the variable `slime-js-target-url` in your `.emacs`.

Next, use the command `M-x slime-js-jack-in-browser` in emacs. It should open up Chrome in your workspace. However, my Chrome browser didn't work well with `swank-js`; an alternative solution is to open `localhost:8009` in safari. It worked for me anyway. Now, you should see that emacs shows a prompt like this:

~~~
; SLIME 2013-03-12
Remote attached: (browser) Safari6.0
 
NODE> 
~~~

Type in `alert("hello world!")` and press enter. You should see your browser pop up an alert window with **hello world**, and that's it! You've successfully installed `swank.js`!!

![Swank JS Screenshot](/images/emacs/swank-js-screenshot-50.png)


Wow, it's really complicated. I can't remember it unless I write it down.


References
----------

* [Swank JS](https://github.com/swank-js/swank-js)
* [Slime github mirror](https://github.com/antifuchs/slime)
* [Setup slime js](https://github.com/magnars/.emacs.d/blob/master/setup-slime-js.el)
* [Emacs Rocks discussions](http://emacsrocks.com/e11.html)

[emacs rocks]: http://emacsrocks.com
[homebrew]: http://mxcl.github.com/homebrew/
