---
layout: post
title: "emacs and pdf"
date: 2013-05-20 22:07
comments: true
categories: emacs
---

Emacs has a built-in pdf viewer named `doc-view-mode`. It require ghostscript to convert the pdf files. On my OSX 10.8 laptop, I use `ghostscript 9.07 (2013-02-14)` version, installed by

```bash
brew install ghostscript
```

### screen shot

![Screen shot](/images/emacs/emacs-docview.png)

### Usage and configs

Using docview is pretty simple, just use your emacs to open the file and it's done. I added two keyboard macro to help me navigate the PDF.

```scm
;; view docs
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)
```

The keybindings is only for two windows layout. You can use `M-[` or `M-]` to navigate the pages while your cursor is in another window. So that you can write your notes and navigate without leaving your notes! Pretty neat, right?
