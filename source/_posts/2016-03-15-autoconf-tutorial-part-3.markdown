---
layout: post
title: "Autoconf Tutorial Part-3"
date: 2016-03-15 10:02
comments: true
categories: autoconf
---

In this post I'll show an example of how to write a cross-plaform OpenGL
program. We'll explore more autoconf features, including `config.h`, third party
libraries, and many more.

<!-- more -->

Cross plaform OpenGL
--------------------

Although OpenGL API is basically the same on all platforms, their headers and
linking options are very different on different plaforms! To use OpenGL on OSX,
you must include `<OpenGL/gl.h>`, however on other platform you have to use
`<GL/gl.h>`. Sometimes you might have multiple possible OpenGL implementation on
the same platform. If you search for OpenGL tutorials, most of it can only built
on one platform.

And that where autoconf comes to play its role. I recently submit a new version
of [AX_CHECK_GL][gl], that can address these complicated portability issues.

Check out the three OpenGL checking macros: [AX_CHECK_GL][gl],
[AX_CHECK_GLU][glu], and [AX_CHECK_GLUT][glut] on autoconf archive. The syntax
is very easy.

* `AX_CHECK_GL([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])`
* `AX_CHECK_GLU([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])`
* `AX_CHECK_GLUT([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])`

However, it doesn't come with the default autoconf package, you need to include
the third party [autoconf archive][archive] in your build script. Here's how to
do it.

[gl]: http://www.gnu.org/software/autoconf-archive/ax_check_gl.html
[glu]: http://www.gnu.org/software/autoconf-archive/ax_check_glu.html
[glut]: http://www.gnu.org/software/autoconf-archive/ax_check_glut.html
[archive]: http://www.gnu.org/software/autoconf-archive/

Adding Extra Macros
-------------------

First, install third party macros by git submodule. Alternatively you can just
copy the macros you need, but be sure to include all the dependent macro it uses. 

```bash
git submodule add git@github.com:peti/autoconf-archive.git
```

Next, in your `configure.ac` add the following line:

```bash
# before invoking AM_INIT_AUTOMAKE
AC_CONFIG_MACRO_DIR([autoconf-archive/m4])
```

After these two steps you are free to invoke 500+ macros in the archive package.

C Preprocessor macros
---------------------

Just adding the macro is not enough. You also have to pass the C preprocessor
macros to your C program. To do so, add another line to your `configure.ac`.

```bash
AC_CONFIG_HEADERS([config.h])
```

And now in your C program you can write the following to make it portable on all
systems. The listing is availabe in the [AX_CHECK_GL document][gl].

```c
# include "config.h"

#if defined(HAVE_WINDOWS_H) && defined(_WIN32)
# include <windows.h>
#endif
#ifdef HAVE_GL_GL_H
# include <GL/gl.h>
#elif defined(HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#else
# error no gl.h
#endif
```

Wrapping it up
--------------

The full working example can be [downloaded from here][example]. Here is the
listing of each code:

```bash configure.ac
AC_INIT([gl-example], [1.0])

AC_CONFIG_SRCDIR([gl-example.c])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([autoconf-archive/m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_PROG_CC

AX_CHECK_GL
AX_CHECK_GLUT

# For glew you can simply use
# AC_CHECK_LIB([GLEW], [glewInit])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
```

The default rule for `gl_example_SOURCES` is to look at the c program with the
same name, thus can be omitted.

```make Makefile.am
bin_PROGRAMS = gl-example
```

```c gl-example.c
#include "config.h"
#include <stdlib.h>
# if HAVE_WINDOWS_H && defined(_WIN32)
  #include <windows.h>
# endif

#ifdef HAVE_GL_GL_H
# include <GL/gl.h>
#elif defined(HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#else
# error no gl.h
#endif

# if defined(HAVE_GL_GLUT_H)
#  include <GL/glut.h>
# elif defined(HAVE_GLUT_GLUT_H)
#  include <GLUT/glut.h>
# else
#  error no glut.h
# endif

static void render(void);

int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(640, 640);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("Hello World!");
  glutDisplayFunc(&render);
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

  glutMainLoop();
  return 0;
}

void render(void) {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glBegin(GL_TRIANGLES);
  glVertex3f( 0.0f, 0.0f, 0.0f);
  glVertex3f( 0.5f, 1.0f, 0.0f);
  glVertex3f( 1.0f, 0.0f, 0.0f);
  glEnd();
  glutSwapBuffers();
}

```

Try out the configure options by invoking `./configure --help`. You'll find it
provides rich options that is familiar to power users.

```bash
./configure --help
`configure' configures gl-example 1.0 to adapt to many kinds of systems.

Usage: ./configure [OPTION]... [VAR=VALUE]...
...
Optional Packages:
  --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
  --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
  --with-xquartz-gl[=DIR] On Mac OSX, use opengl provided by X11/XQuartz
                          instead of the built-in framework. If enabled, the
                          default location is [DIR=/opt/X11]. This option is
                          default to false.
...
  PKG_CONFIG  path to pkg-config utility
  PKG_CONFIG_PATH
              directories to add to pkg-config's search path
  PKG_CONFIG_LIBDIR
              path overriding pkg-config's built-in search path
  GL_CFLAGS   C compiler flags for GL, overriding configure script defaults
  GL_LIBS     Linker flags for GL, overriding configure script defaults
  CPP         C preprocessor
  GLUT_CFLAGS C compiler flags for GLUT, overriding configure script defaults
  GLUT_LIBS   Linker flags for GLUT, overriding configure script defaults

```

So far I haven't seen other build system that can do OpenGL cross platform
setup. (I only searched for CMake and Scons). Though autoconf is said to be
harder to learn, but by learning through these three articles, now the
syntax shouldn't be that alien anymore, right?

In the next post, I'll give another example of how to build a library, with unit
tests and debugger setup.

[example]: https://github.com/dryman/autoconf-tutorials/tree/master/example-3
