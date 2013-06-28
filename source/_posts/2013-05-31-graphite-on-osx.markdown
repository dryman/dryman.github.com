---
layout: post
title: "Install Graphite on OSX"
date: 2013-05-31 16:59
comments: true
categories: 
---

Installing graphite on OSX is a *pain*. This post is a quick note that I would never want to go through again. If you are looking this post, I'd suggest you to install graphite on linux.

Most installation steps are from this [gist][].

<!--more-->

## Xquartz and Cairo

If you don't have X11 installed, go to [xquartz][] and install it on your mac. Then, install cairo via `homebrew`.

```bash
brew install cairo
```

## Pythonbrew

Install pythonbrew and setup environment variables.

```bash
curl -kL http://xrl.us/pythonbrewinstall | bash
echo '[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc' >> ~/.bashrc
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc
pythonbrew install -framework -f 2.7.2
pythonbrew use 2.7.2
pythonbrew venv create graphite
pythonbrew venv use graphite
```

### Python-cairo

This part is the real pain. On linux, just use `sudo apt-get install python-cairo`, it's much much more simpler.

```bash
brew install autotools

cd ~/tmp
git clone git://git.cairographics.org/git/py2cairo
cd py2cairo

export ACLOCAL_FLAGS="-I /usr/local/Cellar/pkg-config/0.28/share/aclocal/"
export MY_PYTHON_PATH=`python -c 'import sys; print sys.prefix'|perl -nle 'm|^.*(?=/bin)|');print $&`
./autogen.sh --prefix=$MY_PYTHON_PATH
./configure

PKG_CONFIG_PATH=`brew --prefix cairo`/lib/pkgconfig ./configure --prefix=$MY_PYTHON_PATH

export CAIRO_CFLAGS="-I/usr/local/Cellar/cairo/1.12.14/include/cairo/"
export CAIRO_LIBS="-L/usr/local/Cellar/cairo/1.12.14/lib/ -lcairo"

C_INCLUDE_PATH=/opt/X11/include/ make
make install
```

## Python dependencies

For the rest of the steps are all copied from the [gist][].

```bash
pip install carbon
pip install whisper
pip install django==1.3.1
pip install django-tagging
pip install graphite-web
```

## Setup graphite

```bash
cd /opt/graphite/webapp/graphite
python manage.py syncdb

cd /opt/graphite/conf
cp storage-schemas.conf.example storage-schemas.conf
cp carbon.conf.example carbon.conf

cd /opt/graphite/webapp/graphite
echo "TIME_ZONE = 'America/Los_Angeles'" > local_settings.py
# use appropriate timezone that you're in
```

## Running graphite

1. `/opt/graphite/bin/carbon-cache.py start`

2. `/opt/graphite/bin/run-graphite-devel-server.py /opt/graphite`

3. Open your browser and hopefully `http://localhost:8080` is up

[xquartz]: http://xquartz.macosforge.org/landing/
[gist]: https://gist.github.com/oyiptong/2430817
