---
layout: post
title: "My Octopress configs"
date: 2012-03-10 16:12
comments: true
categories: Octopress
---

installation note
-----------------

This is an installation note for [Octopress](http://octopress.org) blogging framework. My configuration is developed on OSX with ruby 1.9.2, and xcode 4.2.

<!-- more -->

Required developement tools
---------------------------

1. [osx-gcc installer](https://github.com/kennethreitz/osx-gcc-installer)
2. install [homebrew](http://mxcl.github.com/homebrew/) if not yet installed

Manage Rubies with [RVM](http://beginrescueend.com/)
------------------------

Install RVM

    bash -s stable < <(curl -s https://raw.github.com/wayneeseguin/rvm/master/binscripts/rvm-installer)

Reload your Shell environment:

    source ~/.bash_profile

Install ruby:

    rvm install 1.9.2 --with-gcc=gcc-4.2

Note: Octopress requires ruby 1.9.2

Use ruby 1.9.2

    rvm use 1.9.2 --default


Setup [Octopress](http://octopress.org)
-----------------

    git clone git://github.com/imathis/octopress.git your-blog
    cd your-blog
    gem install bundler
    bundle install
    rake install

## Setup github pages
add new repository as *your-name.github.com*

    rake setup_github_pages
    Enter the read/write url for your repository: git@github.com:your-name/your-name.github.com.git
    rake generate
    rake deploy

Now you can go to github.com/your-name.github.com to see your repository

    git add .
    git commit -m 'blog init'
    git push origin source

Wait about 10 minutes and you can see your inited blog on your-name.github.com!
