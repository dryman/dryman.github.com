---
layout: post
title: "Capture directory context in Hadoop Mapper"
date: 2014-01-26 15:12
comments: true
categories: Hadoop
---

I have been using hadoop for data processing and datawarehousing for a while. One of the problem we encountered was map-reduce framework abstracts the input from files to lines, and thus it's really difficult to apply logic based on different file or directories. Things got worse when we need to aggregate data across various versions of input sources. After digging in Hadoop source code, here is my solution.

<!--more-->

Meets InputSplit in Mapper
--------------------------

Here is the first solution, it's a bit ugly but works. In Hadoop `Mapper` class, you can override the `setup` method to initiate mapper with corresponding context. The context contains `inputSplit`, which can be cast to `FileInputSplit`, and contains the file and directory information in it. This is how I did it:

```java
protected void setup(Context context) throws IOException{
  FileSplit fileSplit;
  InputSplit is = context.getInputSplit();
  FileSystem fs = FileSystem.get(context.getConfiguration());
  fileSplit = (FileSplit) is;
  Path filePath = fileSplit.getPath();
}
```

If the input directory looks like this:

```
/input/part-r-00000
       part-r-00001
       part-r-00002
       part-r-00003
```

And the path argument you passed to `FileInputFormat` is `/input`. The resulting paths in the snippet would be one of these:

```
/input/part-r-00000
/input/part-r-00001
/input/part-r-00002
/input/part-r-00003
```

Each Mapper would get different **file** path instead of getting the directory `/input`. If you want to handle the logic better, you can do this:

```java
Path finalPath;
if (fs.isFile(filePath)){
  finalPath = new Path(filePath.getParrent());
} else {
  finalPath = filePath;
}
```

TaggedInputSplit
----------------

This works for most of the time; however, if you set the input to be `/input/*/part*`. The `InputSplit` would be an internal type called `TaggedInputSplit` instead of `FileInputSplit`. Although `TaggedInputSplit` has a method called `getInputSplit` to get the wrapped class, it is a private class and you can only use java reflection to hack it.

```
InputSplit is = context.getInputSplit();
Method method = is.getMethod("getInputSplit");
method.setAccessible(true);
fileSplit = (FileSplit) method.invoke(is);
Path filePath = fileSplit.getPath();
```

More general solutions
----------------------

The solutions above is working on production environment. However, it is a bit too hacky and not general enough. On the next post, I'll show you how to implement `InputFomrat`, `RecordReader`, and `Writable` classes to solve this problem with lower level APIs.
