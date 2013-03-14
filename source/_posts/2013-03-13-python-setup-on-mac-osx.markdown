---
layout: post
title: "Setting up Python on OSX Mountain Lion"
date: 2013-03-13 08:47
comments: true
categories: python
---

Setting up programming environment is frustrating for newcomers. Although it is much easier than it was before, it's still worth it to take note.

<!--more-->

## 1. Apple developer tools

Not only the python development but all kinds of programming tasks on OSX require Apple developer tools. The instruction is easy; you download XCode from Mac AppStore, then you download *Command Line Tools* in the Downloads menu from the XCode Preference panel.

## 2. Homebrew

Next, install OSX most popular package manager [homebrew](http://mxcl.github.com/homebrew/). Just copy and paste this command in the terminal.

```bash
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
```

If it shows an error about user permission, do the following

```bash
cd /usr/local
sudo chown -R USER_NAME local
```

Now you can install the followings with *homebrew*.

```bash
brew install python
brew install gfortran
```

Years before Mountain Lion, I used to install fortran via [High Performance Computing for Mac OSX](http://hpc.sourceforge.net), but it doesn't work well with homebrew. Now it's best to install everything with *homebrew*.

## 3. Python

Install scipy, numpy, ipython, and matplot.

```bash
pip install scipy
pip install numpy
pip install ipython
pip install matplot
```

Finally, install opencv

```bash
brew install opencv
```

If you want a additional nice GUI application for ipython, you can download it from [IPython Notebook](https://github.com/liyanage/ipython-notebook#readme).

<img width="50%" style="margin-left:25%;" src="/images/ipython_notebook.png"/>

Trouble shooting
----------------

Sometimes homebrew has trouble with fetching the source code. If you found it is stuck on downloading the files, do the following

```bash
brew install -v opencv
```

It will show the URL of the file it is going to download. Stop the command with ctrl-c, paste the URL to your browser and download it. After the download is complete, move the file to `/Library/Cache/Homebrew`.

```bash
mv OpenCV-2.4.4a.tar.bz2 /Library/Caches/Homebrew/
```

Continue the installation by

```bash
brew install opencv
```
