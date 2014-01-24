---
layout: post
title: "Process Small Files on Hadoop using CombineFileInputFormat (1)"
date: 2013-09-22 14:39
comments: true
categories: Hadoop Java
---

Processing small files is an old typical problem in hadoop; On [Stack Overflow][stack overflow] it suggested people to use [CombineFileInputFormat][],  but I haven't found a good step-to-step article that teach you how to use it. So, I decided to write one myself.

[stack overflow]: http://stackoverflow.com/questions/14541759/how-can-i-work-with-large-number-of-small-files-in-hadoop

[CombineFileInputFormat]: http://hadoop.apache.org/docs/r1.1.1/api/org/apache/hadoop/mapred/lib/CombineFileInputFormat.html

<!--more-->

From [Cloudera's blog][]:

> A small file is one which is significantly smaller than the HDFS block size (default 64MB).
> If you’re storing small files, then you probably have lots of them (otherwise you wouldn’t turn to Hadoop),
> and the problem is that HDFS can’t handle lots of files.

[Cloudera's blog]: http://blog.cloudera.com/blog/2009/02/the-small-files-problem/

In my benchmark, just using a custom `CombineFileInputFormat` can speedup the program from 3 hours to 23 minutes, and after some further tuning, the same task can be run in 6 minutes!

## Benchmark Setup

To test the raw performance of different approaches to solve small problems, I setup a map only hadoop job that basically just do grep and perform a small binary search. The binary search part is to generate the reduce side keys that I'll use in further data processing; it took only a little resource (8MB index) to run, so it does not affect the result of the benchmark.

The data to process is some server log data, 53.1 GB in total. The hadoop clusters consist 6 nodes, using hadoop version 1.1.2. In this benchmark I implemented `CombineFileInputFormat` to shrink the map jobs; I also tested the difference of reusing JVM or not, and different number of block sizes to combine files.

## CombineFileInputFormat

The code listed here is modified from [Hadoop example code][]. To use `CombineFileInputFormat` you need to implement three classes. The class `CombineFileInputFormat` is an abstract class with no implementation, so you must create a subclass to support it; we'll name the subclass `CFInputFormat`. The subclass will initiate a delegate `CFRecordReader` that extends `RecordReader`; this is the code that does the file processing logic. We'll also need a class for `FileLineWritable`, which replaces `LongWritable` normally used as a key to file lines.

[Hadoop example code]: https://svn.apache.org/repos/asf/hadoop/common/trunk/hadoop-mapreduce-project/hadoop-mapreduce-examples/src/main/java/org/apache/hadoop/examples/MultiFileWordCount.java

### CFInputFormat.java

The `CFInputFormat.java` doesn't do much. You implement `createRecordReader` to pass in the record reader that does the combine file logic, that's all. Note that you can call `setMaxSplitSize` in the initializer to control the size of each chunk of files; if you don't want to split files into half, remember to return `false` in `isSplitable` method, which defaults to `true`.

```java
package org.idryman.combinefiles;

import java.io.IOException;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.CombineFileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.CombineFileRecordReader;
import org.apache.hadoop.mapreduce.lib.input.CombineFileSplit;

import org.idryman.combinefiles.CFRecordReader;
import org.idryman.combinefiles.FileLineWritable;

public class CFInputFormat extends CombineFileInputFormat<CFInputFormat, Text> {
  public CFInputFormat(){
    super();
    setMaxSplitSize(67108864); // 64 MB, default block size on hadoop
  }
  public RecordReader<FileLineWritable, Text> createRecordReader(InputSplit split, TaskAttemptContext context) throws IOException{
    return new CombineFileRecordReader<FileLineWritable, Text>((CombineFileSplit)split, context, CFRecordReader.class);
  }
  @Override
  protected boolean isSplitable(JobContext context, Path file){
    return false;
  }
}
```

### CFRecordReader.java

`CFRecordReader` is a delegate class of `CombineFileRecordReader`, a built in class that pass each split (typically a whole file in this case) to our class `CFRecordReader`. When the hadoop job starts, `CombineFileRecordReader` reads all the file sizes in HDFS that we want it to process, and decides how many splits base on the `MaxSplitSize` we defined in `CFInputFormat`. For every split (must be a file, because we set `isSplitabe` to false), `CombineFileRecordReader` creates a `CFRecrodReader` instance via a custom constructor, and pass in `CombineFileSplit`, context, and index for `CFRecordReader` to locate the file to process with.

When processing the file, the `CFRecordReader` creates a `FileLineWritable` as the key for hadoop mapper class. With each line a `FileLineWritable` consists the file name and the offset length of that line. The difference between `FileLineWritable` and the normally used `LongWritable` in mapper is `LongWritable` only denote the offset of a line in a file, while `FileLineWritable` adds the file information into the key.

```java
package org.idryman.combinefiles;

import java.io.IOException;
import org.idryman.combinefiles.FileLineWritable;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.CombineFileSplit;
import org.apache.hadoop.util.LineReader;


public class CFRecordReader extends RecordReader<FileLineWritable, Text>{
  private long startOffset;
  private long end;
  private long pos;
  private FileSystem fs;
  private Path path;
  private FileLineWritable key;
  private Text value;

  private FSDataInputStream fileIn;
  private LineReader reader;

public CFRecordReader(CombineFileSplit split, TaskAttemptContext context, Integer index) throws IOException{
  this.path = split.getPath(index);
  fs = this.path.getFileSystem(context.getConfiguration());
  this.startOffset = split.getOffset(index);
  this.end = startOffset + split.getLength(index);

  fileIn = fs.open(path);
  reader = new LineReader(fileIn);
  this.pos = startOffset;
}

@Override
public void initialize(InputSplit arg0, TaskAttemptContext arg1)
    throws IOException, InterruptedException {
  // Won't be called, use custom Constructor
  // `CFRecordReader(CombineFileSplit split, TaskAttemptContext context, Integer index)`
  // instead
}

@Override
public void close() throws IOException {}

@Override
public float getProgress() throws IOException{
  if (startOffset == end) {
    return 0;
  }
  return Math.min(1.0f, (pos - startOffset) / (float) (end - startOffset));
}

@Override
public FileLineWritable getCurrentKey() throws IOException, InterruptedException {
  return key;
}

@Override
public Text getCurrentValue() throws IOException, InterruptedException {
  return value;
}

@Override
public boolean nextKeyValue() throws IOException{
  if (key == null) {
    key = new FileLineWritable();
    key.fileName = path.getName();
  }
  key.offset = pos;
  if (value == null){
    value = new Text();
  }
  int newSize = 0;
  if (pos < end) {
    newSize = reader.readLine(value);
    pos += newSize;
  }
  if (newSize == 0) {
    key = null;
    value = null;
    return false;
  } else{
    return true;
  }
}
}
```


The reason to use a custom constructor
is not documented anywhere in hadoop api nor document. You can only find it in [hadoop source code][hadoop_src], line 40:


```java
   static final Class [] constructorSignature = new Class []
                                          {CombineFileSplit.class,
                                           TaskAttemptContext.class,
                                           Integer.class};
```

[hadoop_src]: http://grepcode.com/file/repo1.maven.org/maven2/com.ning/metrics.collector/1.2.1/org/apache/hadoop/mapreduce/lib/input/CombineFileRecordReader.java#40
 


### FileLineWritable.java

This file is very simple: store the file name and offset, and override the `compareTo` method to compare the file name first, then compare the offset.

```java
package org.idryman.combinefiles;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.WritableComparable;

public class FileLineWritable implements WritableComparable<FileLineWritable>{
  public long offset;
  public String fileName;

  public void readFields(DataInput in) throws IOException {
    this.offset = in.readLong();
    this.fileName = Text.readString(in);
  }

  public void write(DataOutput out) throws IOException {
    out.writeLong(offset);
    Text.writeString(out, fileName);
  }

  public int compareTo(FileLineWritable that) {
    int cmp = this.fileName.compareTo(that.fileName);
    if (cmp != 0) return cmp;
    return (int)Math.signum((double)(this.offset - that.offset));
  }

  @Override
  public int hashCode() {               // generated hashCode()
    final int prime = 31;
    int result = 1;
    result = prime * result + ((fileName == null) ? 0 : fileName.hashCode());
    result = prime * result + (int) (offset ^ (offset >>> 32));
    return result;
  }

  @Override
  public boolean equals(Object obj) {  // generated equals()
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    CFInputFormat other = (CFInputFormat) obj;
    if (fileName == null) {
      if (other.fileName != null)
        return false;
    } else if (!fileName.equals(other.fileName))
      return false;
    if (offset != other.offset)
      return false;
    return true;
  }
}
```

## job setup

Finally is the job setup for hadoop cluster to run. We just need to assign the classes to job:

```java
import org.apache.hadoop.mapreduce.Job;
// standard hadoop conf
Job job = new Job(getConf());
job.setInputFormatClass(CFInputFormat.class);
job.setMapperClass(MyMapper.class);
job.setNumReduceTasks(0); // map only
```

The benchmark result is in the next post.
