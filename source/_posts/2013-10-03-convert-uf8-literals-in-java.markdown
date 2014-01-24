---
layout: post
title: "Convert utf8 literals in Java"
date: 2013-10-03 20:37
comments: true
categories: Java
---

I thought this problem is already been solved, but it's not: consider a string like `\xe6\x84\x8f\xe6\xb3\x95\xe5\x8d\x8a\xe5\xaf\xbc hello world`, how can you transform it to an utf8 encoded string `意法半导 hello world`? Note that the string you get is encoded in ascii encoding, not utf8; the original utf8 is transfered into hex literals. I thought that I can use whatever library I found on the first result returned by google, but actually there's no trivial solution out there on the web.

<!--more-->

## ICU4J

The only library that you can use for handling utf8 on java platform, is [ICU4J][]. THE Unicode processing library devloped by IBM. If you know any other library that can process the literal string, please tell me, I'll be really appreciated.

With [ICU4J][] you can use `com.ibm.icu.impl.Utility.unescape(String s)` to convert the literal string to utf8 string. However, java string internally doesn't use utf8 encoding, instead it uses UTF-16 (Big Endian) to present unicode characters. To fully convert the string from utf8 literal to java unicode representation, you need to decode it with **ISO-8859-1** then read the bytes back to string using encoding **UTF-8**.

```java
import com.ibm.icu.impl.Utility;

String utf_literals = "\\xe6\\x84\\x8f\\xe6\\xb3\\x95\\xe5\\x8d\\x8a\\xe5\\xaf\\xbc hello world";

String utf8_str = Utility.unescape(utf_literals);
byte[] b = utf8_str.getBytes("ISO-8859-1");
String java_utf_str = new String(b, "UTF-8");

System.out.println(java_utf_str);
// ==>  意法半导 hello world
```

One more thing. In order to print the utf string in Eclipse, you have to set the encoding of the output to utf8, else you'll see a bunch of question marks.

I'm quite surprised that no one ever write a post of how do you solved this task. I know the solution is short yet not that trivial, but it still took me several hours to dig in and out on different libraries and solutions on the web to reach the final answer. Hope this post can save your time if you encountered the same problem!

[ICU4J]: http://icu-project.org/apiref/icu4j/
