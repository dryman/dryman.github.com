---
layout: post
title: "LibSVM on Mac OSX"
date: 2013-05-21 11:51
comments: true
categories: ML
---

```bash
brew install gnuplot
brew install qt4
brew install libsvm
```

Installing `libsvm` itself in system path is easy, however hombrew does not include demo-data, python/java integration, or other goodies. The finally installation in homebrew only consists these files:


```
libsvm
libsvm/3.17
libsvm/3.17/bin
libsvm/3.17/bin/svm-predict
libsvm/3.17/bin/svm-scale
libsvm/3.17/bin/svm-train
libsvm/3.17/COPYRIGHT
libsvm/3.17/include
libsvm/3.17/include/svm.h
libsvm/3.17/INSTALL_RECEIPT.json
libsvm/3.17/lib
libsvm/3.17/lib/libsvm.2.dylib
libsvm/3.17/lib/libsvm.dylib
libsvm/3.17/README
```
In this post, I'll show you how to modify some Makefiles and create mac specific binaries.

<!--more-->

## Python, java, or GUI applications (Qt)

First, fetch the source code of `libsvm`, and compile it.

```bash
curl -LO http://www.csie.ntu.edu.tw/~cjlin/cgi-bin/libsvm.cgi?+http://www.csie.ntu.edu.tw/~cjlin/libsvm+tar.gz
tar xzvpf libsvm+tar.gz
cd libsvm-3.17
make
```

### SVM-Toy (GUI)

Now, goto `svm-toy/qt` directory. Change the CFLAGS and MOC variable in the `Makefile`.

```make
CXX? = g++

CFLAGS = -Wall -O3 -I$(INCLUDE) -I$(INCLUDE)/QtGui -framework QtCore -framework QtGui -F/usr/local/Cellar/qt/4.8.4/lib/
INCLUDE = /usr/local/Cellar/qt/4.8.4/include/
MOC = moc

svm-toy: svm-toy.cpp svm-toy.moc ../../svm.o
	$(CXX) $(CFLAGS) svm-toy.cpp ../../svm.o -o svm-toy

svm-toy.moc: svm-toy.cpp
	$(MOC) svm-toy.cpp -o svm-toy.moc

../../svm.o: ../../svm.cpp ../../svm.h
	make -C ../.. svm.o

clean:
	rm -f *~ svm-toy svm-toy.moc ../../svm.o

```


Then `make`. The compile step should run successfully. Try the program `./svm-toy`, press **load** button and select a demo svm-file (heart_scale for example), then press **run**, you should see the result!

![Select heart_scale](/images/select_heart_scale.png)
![svm toy](/images/svm_toy.png)


### Python interface

To use the python interface, just export the `PYTHONPATH` variable to where python utility fuction located at.

```bash
cd libsvm-3.17/python
echo "export PYTHONPATH=\$PYTHONPATH:$(pwd)" >> ~/.bash_profile
source ~/.bash_profile
```

Then read the README file in the same directory, and try out the example usage of python interface.
