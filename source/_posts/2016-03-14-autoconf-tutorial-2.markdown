---
layout: post
title: "Autoconf Tutorial Part-2"
date: 2016-03-14 16:28
comments: true
categories: autoconf
---

This is the second post of the autoconf tutorial series. In this post I'll cover
some fundamental units in autoconf and automake, and an example cross platform
X11 program that uses the concepts in this post. After reading this post, you
should be able to write your own build script for small scope projects.

<!-- more -->

Autoconf
--------

Autoconf is part of the GNU Autotools build system. Autotools is a collection of
three main packages: autoconf, automake, and libtools. Each of the package has
smaller sub-packages including autoheader, aclocal, autoscan etc. I won't cover
the details of all the packages; instead I'll only focus on how autoconf plays
its role in the build chain.

Autoconf is mainly used to generate the `configure` script.  `configure` is a
**shell script** that detects the build environment, and output proper build
flags to the Makefile, and preprocessor macros (like `HAVE_ALLOCA_H`) to
`config.h`. However, writing a good portable, extensible shell script isn't
easy. This is where the [gnu m4 macro][m4] comes in. [Gnu m4 macro][m4] is an
implementation of the traditional UNIX macro processor. By using m4, you can
easily create portable shell script, include different pre-defined macros, and
define your own extensions easily.

In short, autoconf syntax is shell script wrapped by [gnu m4 macro][m4].

In the early days, writing portable shell scripts wasn't that easy. For example
not all the `mkdir` support `-p` option, not all the shells are bash
compatible, etc. Using the m4 macro to perform the regular shell logics, like
`AS_IF` instead if `if [[ ]]; then...`, `AS_MKDIR_P` instead of `mkdir -p`,
`AS_CASE` instead of `case ... esac` makes your configure script works better on
all unix/unix-like environment, and more conventional. Most of the time you'll
be using macros instead of bare bone shell script, but keep in mind that behind
the scene your final output is still shell script.

[m4]: http://www.gnu.org/software/m4/m4.html

### M4 Macro Basics

Though the first look at M4 macros is very alien and unfriendly, but it only
consist two basic concepts:

* Macro expansion
* Quoting

You can define a macro like so:

```bash
# define a macro MY_MACRO that expands to text ABC
m4_define([MY_MACRO], [ABC])
MY_MACRO => ABC

# define a macro that is visible to other m4 scripts
AC_DEFUN([MY_MACRO], [ABC])
MY_MACRO => ABC
```

It's pretty much similar to C macro or Lisp macro. The macro expands at compile
time (`configure.ac` => `configure`). You can define a macro `MY_MACRO` that
expands to a snippet of shell script. Here we just expands it to `ABC`, which
doesn't have any meaning in shell script and can trigger an error.

Every symbol in your script is expandable. For example if you simply write `ABC`
in your script, is it a shell symbol, or is it a m4 symbol that needs to expand?
The m4 system uses **quoting** to differentiate the two. The default quoting in
autoconf is square brackets `[`, `]`. Though you can change it, but it is highly
unrecommended.

```bash
ABC   # m4 would try to find *macro* definition of ABC and try to expand it
[ABC] # shell symbol ABC
```

Why does it matter? Consider these two examples

```bash
ABC="hello world"     # m4 would try to expand ABC, hello, and world
[ABC="hello world"]   # m4 would just produce ABC="hello world" to the output

# m4 will expand MY_MACRO and BODY *before* defining MY_MACRO as a symbol to
# BODY.
AC_DEFUN(MY_MACRO, BODY)

# safe
AC_DEFUN([MY_MACRO], [BODY])
```

This is the base of all m4 macros. To recap, **always quote the arguments for
the macros, including symbols, expressions, or body statements**. (I skipped
some edge cases that requires double quoting or escapes, for the curious please
check [the autoconf language][language]).

[language]: https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Autoconf-Language.html

### Printing Messages

Now we know the basic syntax of m4, let's see what are the functions it
provides. In the configure script, if you invoke `echo` directly the output
would be redirected to different places. The convention to print message in
autoconf, is to use `AC_MSG_*` macros. Here are the two macros that is most
commonly used:

```bash
# Printing regular message
AC_MSG_NOTICE([Greetings from Autoconf])

# Prints an error message and stops the configure script
AC_MSG_ERROR([We have an error here!]
```

For the more curious, check the [Printing Messages][print] section in autoconf
manual.

[print]: https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Printing-Messages.html

### If-condition

To write an if condition in autoconf, simply invoke 
`AS_IF(test-1, [run-if-true-1], ..., [run-if-false])`.
The best way to see how it works is by looking an example:

```bash
abc="yes"
def="no"
AS_IF([test "X$abc" = "Xyes"],             # test condition
      [AC_MSG_NOTICE([abc is yes])],       # then case
      [test "X$def" = "Xyes"],             # else if
      [AC_MSG_NOTICE([def is yes])],
      [AC_MSG_ERROR([abc check failed])]   # else case
     )

# expands to the following shell script
abc="yes"
def="no"
if test "X$abc" = "Xyes"; then :
  # test condition
       $as_echo "$as_me: abc is yes" >&6
elif # then case
       test "X$def" = "Xyes"; then :
  # else if
       $as_echo "$as_me: def is yes" >&6
else
  as_fn_error $? "abc check failed"   # else case
fi
```

Note that we don't use common shell test operator `[[` and `]]`, instead we use
`test` because the square bracket is preserved for macro expansion. The
recommended way to invoke test is `test "X$variable" = "Xvalue"`. This is how we
avoid null cases of the shell variable.

Another common branching function is `AS_CASE(word, [pattern1], [if-matched1], ..., [default])`
the logic is pretty much the same.

That all the basics we need to know for autoconf, let's take a break and switch
to automake.

Automake
--------

Like autoconf, automake is additional semantics on top of another existing
language -- the Makefile syntax. Unlike autoconf, it's not using m4 to extend
the syntax. It uses a naming convention that converts to the actual logic. Most
of the time, we only need to use the following two rules, which we'll discuss in
detail.

* `where_PRIMARY = targets`
* `target_SECONDARY = inputs`

### `where_PRIMARY = targets`

This syntax has three parts, `targets`, type `PRIMARY`, and where to install
`where`. Some examples shown as below:

```make
# target "hello" is a program that will be installed in $bindir
bin_PROGRAMS = hello

# target "libabc" is a library that will be installed in $libdir
lib_LIBRARIES = libabc.la
```

The `targets` is a list of targets with the type `PRIMARY`. Depending on what
`PRIMARY` is, it can be a program, a library, a shell script, or whatever
`PRIMARY` supports. The current primary names are "PROGRAMS", "LIBRARIES",
"LTLIBRARIES", "LISP", "PYTHON", "JAVA", "SCRIPTS", "DATA", "HEADERS", "MANS",
and "TEXINFOS".

There are three possible type of variables you can put into the `where` clause.

* GNU standard directory variables (bindir, sbindir, includedir, etc.) omitting
  the suffix "dir". See [GNU Coding Standard - Directory Variables][dir] for
  list of predefined directories. Automake extends this list with `pkgdatadir`,
  `pkgincludedir`, `pkglibdir`, and `pkglibexecdir` Automake will check if your
  target is valid to install the directory you specified.

* Self-defined directories. You can hack around automake default type check by
  defining your own directories. Do not do this unless you have a good reason!

```make
# Work around forbidden directory combinations.  Do not use this
# without a very good reason!
my_execbindir = $(pkglibdir)
my_doclibdir = $(docdir)
my_execbin_PROGRAMS = foo
my_doclib_LIBRARIES = libquux.a
```

* Special prefixes `noinst_`, `check_`, `dist_`, `nodist_`, `nobase_`, and
 `notrans_`. `noinst_` indicates the targets that you don't want to install;
 `check_` is used for unit tests. For the others are less common, please check
 the automake manual for detail.

[dir]: https://www.gnu.org/prep/standards/html_node/Directory-Variables.html

### `target_SECONDARY = inputs`

Depending on what your `PRIMARY` type is, there are different `SECONDARY` types
you can use for further logic. The common `SECONDARY` types are

* `_SOURCES` defines the source for primary type `_PROGRAMS` or `_LIBRARIES`
* `_CFLAGS`, `_LDFLAGS`, etc. compiler flags used for primary type `_PROGRAMES`
 or `_LIBRARIES`

Note that the invalid character in `target` name will get substituted with
underscore. The following example illustrate all the above:

```make
lib_LTLIBRARIES = libgettext.la
# the dot got substituted with underscore
libgettext_la_SOURCES = gettext.c gettext.h
include_HEADERS = gettext.h
```

The example above requires [libtool][libtool]. You need to declare
`AC_PROG_LIBTOOL` in your `configure.ac` for it to work.

[libtool]: https://www.gnu.org/software/libtool/

Wraps it up - A X11 example program
-----------------------------------

With everything we learnt so far, let's write a more complicated autoconf
program. This is a very simple X11 program that aims to be portable on all
existing platforms with valid X11 installed. To test if X11 is installed, we use
the macro `AC_PATH_XTRA`, the manual for this macro is defined in 
[autoconf existing test for system services][x11].

The manual says: An enhanced version of `AC_PATH_X`. It adds the C compiler flags
that X needs to output variable `X_CFLAGS`, and the X linker flags to `X_LIBS`.
Define `X_DISPLAY_MISSING` if X is not available. And in the `AC_PATH_X` it
states "If this method fails to find the X Window System ... set the shell
variable no_x to ‘yes’; otherwise set it to the empty string". We can use the
logic and write our `configure.ac` script as following:

```bash
AC_INIT([x11-example], [1.0])

# safety check in case user overwritten --srcdir
AC_CONFIG_SRCDIR([x11-example.c])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Check for C compiler
AC_PROG_CC

# Check for X11
# It exports variable X_CFLAGS and X_LIBS
AC_PATH_XTRA

# AC_PATH_XTRA doesn't error out by default,
# hence we need to do it manually
AS_IF([test "X$no_x" = "Xyes"],
  [AC_MSG_ERROR([Could not find X11])])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
```

Note that the `AC_PATH_XTRA` export variables `X_CFLAGS` and `X_LIBS`. To use
these variables in `Makefile.am`, just surround it with `@`.

```make
bin_PROGRAMS = x11-example

x11_example_SOURCES = x11-example.c
x11_example_CFLAGS = @X_CFLAGS@
# AX_PATH_XTRA only specify the root of X11
# we still have to include -lX11 ourselves
x11_example_LDFLAGS = @X_LIBS@ -lX11
```

That all we need to build a platform independent X11 program! Check the full
source on [github][github]. The X11 example program was written by Brian Hammond
2/9/96. He generously released this to public for any use.

[github]: https://github.com/dryman/autoconf-tutorials/tree/master/example-2
[x11]: https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/System-Services.html

This program can easily work on Linux. I'll use OSX as an example of how cross
platform works. Before you run the example, make sure you have
[XQuartz][xquartz] installed.

[xquartz]: http://www.xquartz.org

```bash
cd example-2
autoreconf -vif # shortcut of --verbose --install --force
./configure --with-x --x-includes=/opt/X11/include/ --x-libraries=/opt/X11/lib
make
./x11-example
```

Change the `--x-includes` and `--x-libraries` to proper directory if you
installed the xquartz to a different location.

I only introduced very little syntax for autoconf (if-else, print message) and
automake (primary/secondary rules, use of export variables by `@`). But just
using these basic component is already very sufficient for writing conventional
build scripts. How to do it? Check the [existing tests provided by
autoconf][exsisting test]. Here are some of the most commonly used existing
checks:

* Library checks: `AC_CHECK_LIB` `AC_SEARCH_LIBS`. [library documentation][lib_doc].
* Header checks: `AC_CHECK_HEADER[S]`. [header documentation][header_doc].
* [Compiler characteristics][compiler_doc].

[existing test]: https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Existing-Tests.html
[lib_doc]: https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Libraries.html
[header_doc]: https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Headers.html
[compiler_doc]: https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Compilers-and-Preprocessors.html

For the checks that are not included in the default autoconf package, it
probably exists in the extended package [autoconf archive][archive], which I'll
cover in the next post.

[archive]: http://www.gnu.org/software/autoconf-archive
