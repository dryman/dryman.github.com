---
layout: post
title: "my emacs setting on servers"
date: 2013-08-29 10:28
comments: true
categories: emacs
---

My desktop emacs config is complecated, however I need a minimal config for emacs installed on ubuntu servers. This is my note of how to configure emacs on servers that works for me.

<!--more-->

## Emacs24

First, I need to get Emacs24 installed on ubuntu, which is not by default. This link [Install Emacs 24 in Ubuntu][] is a great guide of how to install emacs 24. Here are the commands you need to type:

```
sudo add-apt-repository ppa:cassou/emacs
sudo apt-get update
sudo apt-get install emacs24
```

[Install Emacs 24 in Ubuntu]: http://www.mikeyboldt.com/2011/11/30/install-emacs-24-in-ubuntu/

## Configure Packges Archives

With Emacs 24, you can use the package managing system easily. Open your `~/.emacs` file and enter

```scm
(require 'package)
(add-to-list 'package-archives '("marmalade"."http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"."http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Save the file, and `M-x eval-buffer`, then `M-x package-refresh-contents`. Now you're ready to install most of the emacs plugins.

## Plugins I installed

1. [Paredit.el][]: a mode for editing structured s-expressions, very useful for lisp based programs including emacs-lisp and clojure.

2. [clojure mode][]: mode for editing clojure files 

3. [Ace Jump][]: Move your cursor to where you eyes are looking at.

[Paredit.el]: http://www.emacswiki.org/emacs/ParEdit

[clojure mode]: https://github.com/clojure-emacs/clojure-mode

[Ace Jump]: http://www.emacswiki.org/emacs/AceJump

## Some other configs

```scm
(global-visual-line-mode t)
(tool-bar-mode -1)
(show-paren-mode t)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)

;; it's dangerous to keep backup files in the same directory on the server
(setq backup-directory-alist '(("." . "~/.backup_emacs"))
      auto-save-file-name-transforms '((".*" "~/.backup_emacs" t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
)

;; configure paredit
(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

;; auto complete for emacs lisp
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; ace jump mode
(global-set-key (kbd "<ESC><ESC>") 'ace-jump-mode)
```

Hope these helps :D
