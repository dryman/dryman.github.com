---
layout: post
title: "Writing java programs on a remote server"
date: 2013-07-14 16:52
comments: true
categories: java vim
---

Recently I started to work on hadoop and big data processing, but I was frustrated on eclipse and the development environment. We run hadoop on a remote cluster, but develop map-reduce programs on laptop. The development cycle was pretty slow because we need to upload the jar for every release. Another thing is Eclipse is too inefficient for a Vim and Emacs hacker like me. Thankfully I'm not the only one who think this way; Eric Van Dewoestine developed [Eclim][] which can let you work on java programs on headless eclipse and vim/emacs! Here comes the installation steps:

<!--more-->

### Install Eclipse

I thought it wasn't that difficult to install the Eclim on remote server, but actually it's not that trivial to do. The key is to use local eclipse in your home directory instead of using package manager to install eclipse. I tried a whole afternoon, but still couldn't make Eclim work with Ubuntu pre-packaged eclipse (using apt-get, of course).

To install eclipse, follow the links on it's official website, and click linux download link to get the actual url. After it starts to download, stop the download process and copy the url. I couldn't find direct download link on Eclipse website, maybe this shadowed link would help:

```bash
wget http://carroll.aset.psu.edu/pub/eclipse/technology/epp/downloads/release/kepler/R/eclipse-standard-kepler-R-linux-gtk-x86_64.tar.gz
```

Extract the tar-ball somewhere in your home directory. The rest of the steps are as same as Eclim official installation guild.

### Other dependencies

Although we want to run Eclim in CLI environment, we still need X11 to run Eclipse daemon. To do so we need to install `xvfb` and other build tools.

```bash
sudo apt-get install xvfb build-essential
```

### Install Eclim

The rest of the steps are pretty easy. Note that I put eclipse at home directory; you may need to modify this parameter to your eclipse location.

```bash
wget http://sourceforge.net/projects/eclim/files/eclim/2.2.7/eclim_2.2.7.jar/download
mv download eclim_2.2.7.jar
java -Dvim.files=$HOME/.vim -Declipse.home=$HOME/eclipse \
     -jar eclim_2.2.7.jar install
```

### Starting Eclim

To start Eclim, first start a headless X11 display, then start the Eclim daemon.

```bash
Xvfb :1 -screen 0 1024x768x24 &
DISPLAY=:1 ~/eclipse/eclimd
```

By default it will create a `workspace` in your home directory, you should put all your java projects there. When creating new projects, use `:ProjectCreate` in Vim (do not create yourself by `mkdir`, Eclim don't know what that is). If you want to delete or rename a project, use `:ProjectDelete` and `:PorjectRename`.

Now you can reach whole eclipse ecosystem directly from vim! Enjoy!


[Eclim]: http://eclim.org/

