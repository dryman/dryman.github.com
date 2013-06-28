---
layout: post
title: "Emacs loves iPython Notebook"
date: 2013-05-29 11:41
comments: true
categories: emacs
---

<!--more-->

## Install

Installing emacs ipython notebook is pretty simple.

1. follow the steps in [Setting up Python on OSX Mountain Lion][].

2. Refresh emacs packages `M-x package-refresh-contents`

3. `M-x package-install` `ein`

## Usage

1. `cd directory`

2. `ipython notebook --pylab inline`

3. `M-x ein:notebooklist-open`

Than, check on [Emacs IPython Notebook][] to see what key-bindings you can use in EIN mode. I only use `C-c C-c` to execute a buffer, and `C-c C-u` for changing the cell type. 

[Setting up Python on OSX Mountain Lion]: /blog/2013/03/13/python-setup-on-mac-osx/

[Emacs IPython Notebook]: https://github.com/tkf/emacs-ipython-notebook

