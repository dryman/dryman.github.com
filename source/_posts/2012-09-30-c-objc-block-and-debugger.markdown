---
layout: post
title: "C-ObjC block and debugger"
date: 2012-09-30 14:32
comments: true
categories: Cocoa, block
---

In the last post, we discussed block byref internal structures and showed the
sample code of how to `printf` it. However, we don't want to write that much
helper functions every time! Thanks for
[Big Nerd Ranch: Advanced Mac OSX Programming][bnr], we can now load
`gdb`/`lldb` plugins and no need to do that anymore! ;D

<!-- more -->

## GDB

Add these line into your `~.gdbinit` file. It was originally created by Big Nerd
Ranch's author Mark Dalrymple.
I renamed the functions from `dump-block-literal` and `dump-block-byref` to
`pblock` and `pbyref`.

    define pblock
        printf "%s", (const char*)_Block_dump($arg0)
    end

    document pblock
        Dumps runtime information about the supplied block reference
        Argument is the name or address of a block literal
    end

    define pbyref
        printf "%s", (const char*)_Block_byref_dump((char*)&$arg0 - 2*sizeof(int) - 2*sizeof(void*))
    end

    document pbyref
        Dumps runtime information about the supplied __block variable
        Argument is a pointer to the variable embeded in a block byref structure
    end


To use it, you just simply type `pbyref` and `pblock` followed by a variable in `gdb`.

    (gdb) pbyref x
    byref data block 0x7fff5fbff480 contents:
      forwarding: 0x7fff5fbff480
      flags: 0x0
      size: 32

    (gdb) pblock localBlock
    ^0x7fff5fbff450 (new layout) =
    isa: stack Block
    flags: HASSIGNATURE HASHELP
    refcount+deallocating: 0
    invoke: 0x100001430
    descriptor: 0x100002070
    descriptor->reserved: 0
    descriptor->size: 40
    descriptor->copy helper: 0x100001470
    descriptor->dispose helper: 0x1000014b0
    descriptor->signature: 0x100001c29 'v8@?0'
    descriptor->layout: 0x0 '(null)'


## LLDB

I made a `lldb` version this weekend. You can find it on [Github][github].
I tried to use `command alias` or `command regex` at first, but it just won't
work. So I just write a python plugin for lldb and load it in `.lldbinit`. The
code is quite trivial:

{% codeblock blockHelper.py https://github.com/dryman/lldb-block-helper/blob/master/blockHelper.py source %}
#!/usr/bin/env python

import lldb
import commands

def pbyref(debugger, command, result, internal_dict):
    cmd = "expr (void)printf(\"%s\",(const char*)_Block_byref_dump((char*)&" + \ 
    command + "- 2*sizeof(int) - 2*sizeof(void *)));"
    lldb.debugger.HandleCommand(cmd)

def pblock(debugger, command, result, internal_dict):
    cmd = "expr (void)printf(\"%s\",(const char*)_Block_dump(" + command + "));"
    lldb.debugger.HandleCommand(cmd)

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f blockHelper.pbyref pbyref')
    debugger.HandleCommand('command script add -f blockHelper.pblock pblock')
    print 'The "pbyref" command has been installed and is ready of use.'
    print 'The "pblock" command has been installed and is ready of use.'
{% endcodeblock %}

### INSTALL

* Download this git repostory to your home directory as `.lldb`.

~~~~
git clone https://github.com/dryman/lldb-block-helper.git ~/.lldb
~~~~

* add this line into your `~/.lldbinit`.

~~~~
command script import ~/.lldb/blockHelper.py 
~~~~

### USAGE

    (lldb) pbyref x
    <no result>
    byref data block 0x100713f90 contents:
      forwarding: 0x100713f90
      flags: 0x1000004
      size: 32

    (lldb) pblock localBlock
    <no result>
    ^0x7fff5fbff480 (new layout) =
    isa: stack Block
    flags: HASSIGNATURE HASHELP
    refcount+deallocating: 0
    invoke: 0x100001430
    descriptor: 0x100002070
    descriptor->reserved: 0
    descriptor->size: 40
    descriptor->copy helper: 0x100001470
    descriptor->dispose helper: 0x1000014b0
    descriptor->signature: 0x100001c29 'v8@?0'
    descriptor->layout: 0x0 '(null)'

## References

* [lldb python references][python]
* [Big Nerd Ranch: Advanced Mac OSX Programming][bnr]

[bnr]: http://www.informit.com/articles/article.aspx?p=1749597&seqNum=12
[github]: https://github.com/dryman/lldb-block-helper
[python]: http://lldb.llvm.org/python-reference.html

