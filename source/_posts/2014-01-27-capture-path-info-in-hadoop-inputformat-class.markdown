---
layout: post
title: "Capture path info in hadoop InputFormat class"
date: 2014-01-27 15:11
comments: true
categories: Hadoop
---

On the last post I presented how to use Mapper context object to obtain Path information. This is a nice way to hack for ad-hoc jobs; however, it's not really reusable and abstract. In this post, I'll show you how to subclass `Text`, `TextInputFormat`, and `LineRecordReader` and create reusable components across all of your hadoop tasks.

<!--more-->

Input WorkFlow
--------------

Before we go through all the classes, let me show you how hadoop read the files in.

1. By default, hadoop uses `TextInputFormat`, which inherits `FileInputFormat`, to process the input files.

2. `TextInputFormat` allocates `LineRecordReader` and passed it to `Task` runtime.

3. `Task` first initiates `LineRecordReader`, then wrap the `LineRecordReader` into `Context` object.

4. In `Mapper` `run` methods, it calls the method `nextKeyValue()` in `Context`, and reads the `LongWritable key` from `context.getCurrentKey()` and `Text value` from `context.getCurrentValue()`. Those methods delegates to `LineRecordReader`'s methods `nextKeyValue()`, `getCurrentKey()`, and `getCurrentValue()`.

5. Finally, `Mapper` passes the key-value pair to `map` method that we usually overrides.

In order to put the path information into this workflow, we can extend the `Text` class and put the path information into it. To make this work, we need to create three new classes: `TextWithPath`, `TWPInputFormat`, and `TWPRecordReader`.


TextWithPath.java
-----------------

Here is our content wrapper -- `TextWithPath`. It doesn't do much; there's a new constructor which accepts `Path`, and there's a getter method to get `Path`.

```java
package org.idryman;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;

public class TextWithPath extends Text {
  private Path path;

  public TextWithPath(Path path){
    super();
    this.path = path;
  }
  
  public Path getPath(){
    return path;
  }
}
```


TWPInputFormat.java
-------------------

The new `TWPInputFormat` is almost identical to `TextInputFormat`, except it uses `TextWithPath` instead of `Text`, and the `createRecordReader` method returns `TWPRecordReader` instead of `LineRecordReader`.

```java
package org.idryman;

import java.io.IOException;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.CompressionCodecFactory;
import org.apache.hadoop.io.compress.SplittableCompressionCodec;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;

public class TWPInputFormat extends FileInputFormat <LongWritable, TextWithPath>{
  @Override
  public RecordReader createRecordReader(InputSplit split,
      TaskAttemptContext context) throws IOException, InterruptedException {
    String delimiter = context.getConfiguration().get(
        "textinputformat.record.delimiter");
    byte[] recordDelimiterBytes = null;
    if (null != delimiter)
      recordDelimiterBytes = delimiter.getBytes();
    return new TWPRecordReader(recordDelimiterBytes);
  }

  @Override
  protected boolean isSplitable(JobContext context, Path file) {
    CompressionCodec codec = 
      new CompressionCodecFactory(context.getConfiguration()).getCodec(file);
    if (null == codec) {
      return true;
    }
    return codec instanceof SplittableCompressionCodec;
  }
}

```

TWPRecordReader.java
--------------------

Finally, in the `TWPRecordReader`, this is where I put my logic in. In the `initialize` method, you can get the `FileSplit` and get the `Path` object out of it. Next, let's override `nextKeyValue`, and updates the `value` on every call. Lastly, remember to override `getCurrentValue()`, else it will only return parent's value (Text), not the value with `TextWithPath` class.

```java
package org.idryman;

import java.io.IOException;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.mapreduce.lib.input.LineRecordReader;

public class TWPRecordReader extends LineRecordReader{
  private TextWithPath value = null;
  private Path path = null;
  
  public TWPRecordReader(byte[] recordDelimiterBytes) {
    super(recordDelimiterBytes);
  }
  
  @Override
  public void initialize(InputSplit genericSplit, TaskAttemptContext context) throws IOException{
    super.initialize(genericSplit, context);
    FileSplit split = (FileSplit) genericSplit;
    path = split.getPath();
  }
  
  @Override
  public boolean nextKeyValue() throws IOException {
    if (super.nextKeyValue()){
      if (value == null)
        value = new TextWithPath(path);
      value.set(super.getCurrentValue());
      return true;
    } else {
      value = null;
      return false;
    }
  }
  
  @Override
  public TextWithPath getCurrentValue(){
    return value;
  }
}
```

Demo
----

Here is a demo code to test the output. In addition to normal map reduce tasks, we set the input format class to `TWPInpuFormat`; on the Mapper side, we expect the input is `TextWithPath`, not `Text`. The whole program can be downloaded from this github repo. [Hadoop TextWithPath][]

```java
package org.idryman;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.reduce.IntSumReducer;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class DemoRun extends Configured implements Tool {

  public static void main(String[] args) throws Exception {
    System.exit(ToolRunner.run(new Configuration(), new DemoRun(), args));
  }

  @Override
  public int run(String[] args) throws Exception {
    Configuration conf = getConf();
    Job job = new Job(conf);
    job.setJobName("test TextWithPath Input");
    job.setJarByClass(DemoRun.class);
    
    TWPInputFormat.addInputPath(job, new Path(args[0]));
    job.setInputFormatClass(TWPInputFormat.class);
    job.setMapperClass(TestMapper.class);
    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(NullWritable.class);
    job.setReducerClass(IntSumReducer.class);
    job.setNumReduceTasks(1);
    FileOutputFormat.setOutputPath(job, new Path(args[1]));
    
    job.submit();
    job.waitForCompletion(true);
    return 0;
  }

  public static class TestMapper extends Mapper<LongWritable, TextWithPath, Text, IntWritable>{
    
    /**
     * Only override `run` instead of `map` method; because we just want to see one output
     * per mapper, instead of printing every line.
     */
    @Override
    public void run(Context context) throws IOException, InterruptedException{
      context.nextKeyValue();
      TextWithPath twp = context.getCurrentValue();
      context.write(new Text(twp.getPath().toString()), new IntWritable(1));
    }
  }

}
```

[Hadoop TextWithPath]: https://github.com/dryman/Hadoop-TextWithPath

One more thing
--------------

I wrote another hadoop utility that reads a header file from HDFS input source, and passes a `FieldWritable` object to `Mapper` class instead of `Text`. The `FieldWritable` implements `Map` interface and can obtain TSV fields by it's header key. The project is on [github][] but still highly experimental. Once the API and implementation is stable, I'll write another post to introduce it. Enjoy!

[github]: https://github.com/dryman/hadoop-fieldformat
