---
layout: post
title: "Introducing Hadoop-FieldFormat"
date: 2014-03-06 14:25
comments: true
categories: Hadoop
---

[Hadoop FieldFormat](https://github.com/dryman/hadoop-fieldformat) is the new library I released that is flexible and robust for reading and setting schema information in Hadoop map-reduce program. We use this library to record the meta information for the data, and improve the semantic when building large map-reduce pipe-lined tasks. The project is quite stable now and we already used it in our production system. Any suggestion is welcome!

<!--more-->

The problem
-----------

The map-reduce architecture is really good at aggregating large dataset and ad-hoc perform computation; however, when the number dataset increases, it becomes difficult to manage the meta data of those dataset. The biggest issue is data by default is semi-structured; there's no schema or header information to tell you the semantic of the data. When working in raw map-reduce, this is typical code that I write:

```java
void map (LongWritable keyIn, Text valIn, Context context) throws IOException, InterruptedException{
    String [] fields = valIn.toString().split("\\t");
    String ip = fields[0];
    String cookie = fields[1];
    String ua = fields[5];
    ...
}
```

There's no semantic associated with the data, so you can only hard code the semantic and hope the fields order will stay the same forever. If the upstream process inserted a new field to this dataset, your program may still run, but produce wrong result that might be difficult to catch by downstream program.

The same issue happens in [Pig][pig] and [cascading][cascading] too. Pig, for example:

```
tomcat = LOAD 'catalina.out' USING PigStorage('\t') AS (ip, cookie, query, url, time, ua);
```

If the input format changed, you'll need to be very careful to make sure all the downstream process are corrected. Moreover, if you want to run map-reduce across different versions of dataset, you may not be able to run it because the order of the fields is different!

[pig]: https://pig.apache.org
[cascading]: http://www.cascading.org

### Hive and HCatalog


Goal: lightweight semantic attached to the data

Eat our own dog food -- introducing Hadoop FieldFormat!
------------------------------------------------------

You may be surprised by how simple the solution is. First, answer this:
Where does hadoop store the meta data for map-reduce jobs? `_logs`.

What hadoop FieldFormat does is reading and writing header.tsv. Also, provides
a convenient API in java to access the data field using the java Map interface.
