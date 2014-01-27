---
layout: post
title: "Setting up Python on OSX Mountain Lion (updated at May 21, 2013)"
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
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
```

If it shows an error about user permission, do the following

```bash
cd /usr/local
sudo chown -R USER_NAME local
```

and setup environment variables in `~/.bash_profile` (if the file does not exist, create one)

```bash
export PATH="~/bin:/usr/local/share/python:/usr/local/bin:$PATH"
```

Now you can install the followings with *homebrew*.

```bash
source ~/.bash_profile
brew install python --with-brewed-openssl
brew install gfortran
brew install freetype
brew install zmq
```

Years before Mountain Lion, I used to install fortran via [High Performance Computing for Mac OSX](http://hpc.sourceforge.net), but it doesn't work well with homebrew. Now it's best to install everything with *homebrew*.

## 3. Python Virtualenv

```bash
pip install virtualenv
mkdir ~/.virtualenvs ~/.pip
export VIRTUALENV_DISTRIBUTE=true
export PIP_VIRTUALENV_BASE=$HOME/.virtualenvs
export PIP_REQUIRE_VIRTUALENV=true
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

cat <<END >> ~/.bash_profile
# virtualenv should use Distribute instead of legacy setuptools
export VIRTUALENV_DISTRIBUTE=true
# Centralized location for new virtual environments
export PIP_VIRTUALENV_BASE=$HOME/.virtualenvs
# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true
# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache
END

cd ~/.virtualenvs
virtualenv data-scientists
source data-scientists/bin/activate
echo "source $HOME/.virtualenvs/data-scientists/bin/activate" >> ~/.bash_profile
```

Now, if you type `which pip` it should print `/Users/YOUR_HOME/.virtualenvs/data-scientists/bin/pip`.


Install scipy, numpy, ipython, and matplot.

```bash
pip install numpy
pip install scipy
pip install ipython
pip install matplotlib
pip install pandas
pip install tornado
pip install pyzmq
pip install ipython
```

Finally, install opencv

```bash
brew tap homebrew/science
brew install opencv
```

If you want a additional nice GUI application for ipython, you can download it from [IPython Notebook](https://github.com/liyanage/ipython-notebook#readme).

![ipython notebook](/images/ipython_notebook.png)



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
cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr . && make all install
```

Reference
---------
1. [Python and Virtualenv on Mac OSX Mountain Lion 10.8 - Hacker Codex](http://hackercodex.com/guide/python-virtualenv-on-mac-osx-mountain-lion-10.8/)
2. [Virtualenv with numpy and scipy on Mac OSX - Calvin's](http://www.calvinx.com/2012/11/02/virtualenv-with-numpy-scipy/)
3. [Virtualenv 1.9.1 : Python Package Index](https://pypi.python.org/pypi/virtualenv)
