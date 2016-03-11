---
layout: post
title: "Autoconf Tutorial Part-1"
date: 2016-03-10 14:55
comments: true
categories: autoconf
---

It's been more than a year since my last update to my blog. I learnt a lot new
stuffs in last year, but was too busy on work to write down what I've learnt.
Luckily I got some breaks recently, and I'll pick up some of the posts that
I've wanted to write about. First I'll start with a autoconf tutorial series.
This is one of the difficult material to learn, but I'll try to re-bottle it
to make it more accessible to everyone.

<!-- more -->

What is Autoconf?
----------------

If you have ever installed third party packages, you probably already used the
result of autoconf. Autoconf, automake, and libtool are the GNU Autotools
family that generate the installation script:

```bash
./configure
make
make install
```

Many unix or unix-like system make use of the simplicity of these installation
steps. The linux distros usually provides custom command line options to the
`./configure` to customize the build, and further repackage it with rpm or dpkg.

Autoconf is not only a build system, it also does many system compatibility
checks. Does your operating system support memory-mapped file? Does your
environment has X11? The standard autoconf already support a wide variety of
checks, and there are 500 more in [Autoconf Archive][archives]. It's the defacto
standard build standard for building small and large linux/unix programs.

Though the output of autoconf is easy for user to install, writing autoconf
build script is less intuitive, compare to other fancier solution like
[CMake][CMake] or [Scons][Scons]. And that's why I'm writing this tutorial - to
reduce the learning curve of using autoconf.

Through out this series, I'll start with a minimal autoconf project, and later
introduce how to bring in debug setup, how to build a library, how to setup unit
test, how to write your own cross platform checks etc.

[archives]: http://www.gnu.org/software/autoconf-archive/The-Macros.html
[CMake]: https://cmake.org
[SCons]: http://www.scons.org

Hello Autoconf
--------------

The best way to learn is to practice through examples. Let's start with a very
simple one. First create a directory holding your project, 

```bash
$ mkdir example-1
$ cd example-1
```

Install the autoconf on your system if it wasn't installed

```bash
# OSX
$ brew install autoconf automake libtool
# Ubuntu/Debian
$ sudo apt-get install autoconf automake libtool
# RHEL/CentOS
$ sudo yum install autoconf automake libtool
```

And create three files: `configure.ac`, `Makefile.am`, and the program itself
`hello.c`.


```bash configure.ac
# Must init the autoconf setup
# The first parameter is project name
# second is version number
# third is bug report address
AC_INIT([hello], [1.0])

# Safety checks in case user overwritten --srcdir
AC_CONFIG_SRCDIR([hello.c])

# Store the auxiliary build tools (e.g., install-sh, config.sub, config.guess)
# in this dir (build-aux)
AC_CONFIG_AUX_DIR([build-aux])

# Init automake, and specify this program use relaxed structures.
# i.e. this program doesn't follow the gnu coding standards, and doesn't have
# ChangeLog, COPYING, AUTHORS, INSTALL, README etc. files.
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Check for C compiler
AC_PROG_CC
# We can add more checks in this section

# Tells automake to create a Makefile
# See https://www.gnu.org/software/automake/manual/html_node/Requirements.html
AC_CONFIG_FILES([Makefile])

# Generate the output
AC_OUTPUT
```

```make Makefile.am
bin_PROGRAMS = hello
hello_SOURCES = hello.c
```

```c hello.c
#include <stdio.h>
int main(int argc, char** argv) {
  printf("hello world!\n");
  return 0;
}
```

That's the minimal build script you need for your first autoconf program.
Let's try what we've got with this setup. Make sure your are in the `example-1`
directory.

```bash
# this creates the configure script
$ autoreconf --verbose --install --force
$ ./configure --help
$ ./configure
ecking for a BSD-compatible install... /usr/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... build-aux/install-sh -c -d
checking for mawk... no
...
config.status: creating Makefile
config.status: executing depfiles commands
# Now try the makefile
$ make
gcc -DPACKAGE_NAME=\"hello\" -DPACKAGE_TARNAME=\"hello\" -DPACKAGE_VERSION=\"1.0\" -DPACKAGE_STRING=\"hello\ 1.0\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -DPACKAGE=\"hello\" -DVERSION=\"1.0\" -I.     -g -O2 -MT hello.o -MD -MP -MF .deps/hello.Tpo -c -o hello.o hello.c
mv -f .deps/hello.Tpo .deps/hello.Po
gcc  -g -O2   -o hello hello.o  
# We now have the hello program built
$ ./hello
hello world!
# Create hello-1.0.tar.gz that contains the configure script
$ make dist
```

You  might think this is overkill for a hello world program, but you can also
think in another way. Just adding the `configure.ac` and `Makefile.am` made a
simple hello world program looks like a serious production ready project (with
all these fancy configure checks and compiler flags).

Let's iterate through each of the build script.

configure.ac
============

The syntax for `configure.ac` is `MACRO_NAME([param-1],[param-2]..)`. The
parameter passed to the macro must be quoted by square brackets, (unless it is
another macro that you want to expand BEFORE calling the outer macro, which is
very rare). The macros will expands to shell script that perform the actual
checks. You can also write shell script in your configure.ac file. Just one
difference, you should use `if test <expression>; then...` instead of 
`if [[ <expression> ]]; then...` for condition branching, because the square
brackets would get expanded by the autoconf macro system.

* `AC_INIT(package, version, [bug-report], [tarname], [url])` In every autoconf
  configure script, you must first initialize autoconf with this macro. The
  square braket that wraps around each parameter cannot be omitted.

* `AC_CONFIG_SRCDIR(dir)` Next we specify a unique file identifying we are in
  the right directory. This is a safety check in case user override the --srcdir
  command line option.

* `AC_CONFIG_AUX_DIR(dir)` By default autoconf will create many auxiliary files
  that help to build and distribute the programs. However we don't want to have
  these files to mess up the project home directory. In convention we call this
  macro with `[build-aux]` so that it put these extra files in `build-aux/`
  instead of project home.

* `AM_INIT_AUTOMAKE([options])` Initializes automake. An important note here is
  in early phase of your project development, you probably want to provide the
  option `foreign` to init automake. If foreign wasn't provided, automake will
  complain that your project didn't confirm to gnu coding standards, which would
  require you to have README, ChangLog, AUTHORS, and many other files in your
  project's home directory.

* `AC_PROG_CC` Checks for a valid C compiler. There are hundreds more checks you
  can put in this section.

* `AC_CONFIG_FILES(files)` Required by automake to create the output file. Here
  we simply put the `Makefile` in. Checks the automake documentation for more
  detail.
  [automake](https://www.gnu.org/software/automake/manual/html_node/Requirements.html).

* `AC_OUTPUT` Creates the configure script

Makefile.am
===========

The automake file `Makefile.am` is an extension to Makefile. You can write
standard make syntax, but normally you only need to define variables that
conforms to the [uniform naming scheme][scheme]. In this post I'll only give
rough explanation, and dive in more detail in next post.

* `bin_PROGRAMS = hello` The output is a PROGRAM (other options are LIBRARY,
  HEADER, MAN, etc.) named `hello`, and will be installed in bin directory
  (default to `/usr/local/bin`, but can be configured when invoking
  `./configure`.

* `hello_SOURCES = hello.c` The sources of hello program is hello.c

[scheme]: https://www.gnu.org/software/automake/manual/html_node/Uniform.html

The complete program can be found in my github repository: 
[Example 1](https://github.com/dryman/autoconf-tutorials/tree/master/example-1).

More make targets
==================

The Makefile generated by Autoconf and automake has more commands that you can
run:

* `make all`
Build programs, libraries, documentation, etc. (same as make).

* `make install`
Install what needs to be installed, copying the files from the package’s tree to system-wide directories.

* `make install-strip`
Same as make install, then strip debugging symbols. Some users like to trade space for useful bug reports...

* `make uninstall`
The opposite of make install: erase the installed files. (This needs to be run from the same build tree that was installed.)

* `make clean`
Erase from the build tree the files built by make all.

* `make maintainer-clean`
Erase files that generated by autoconf.

* `make distclean`
Additionally erase anything ./configure created.

* `make check`
Run the test suite, if any.

* `make installcheck`
Check the installed programs or libraries, if supported.

* `make dist`
Recreate package-version.tar.gz from all the source files.


When I first survey what build system I should pick for my own projects, I often
see other alternatives claiming autoconf is old and hard to use. This is
partially true, but the more I dig in the more I found how powerful autoconf is.
As you see, this example can already cover many common cases, with a succinct
build script and very powerful output. The package created by `make dist`
only requires a minimal unix compatible environment (shell and make) to run.

In the next post I'll cover more detail in the autoconf syntax and Automake
syntax.

References
----------

* [Autoconf](http://www.gnu.org/software/autoconf/autoconf.html)
* [Automake](https://www.gnu.org/software/automake/)
* [Autoconf myth buster](https://autotools.io/index.html)
* [Autotools tutorials](http://www.lrde.epita.fr/~adl/autotools.html)
