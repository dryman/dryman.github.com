---
layout: post
title: "Hadoop performance tuning best practices"
date: 2014-03-05 11:17
comments: true
categories: Hadoop
---

I have been working on Hadoop in production for a while. Here are some of the performance tuning tips I learned from work. Many of my tasks had performance improved over 50% in general. Those guide lines work perfectly in my work place; hope it can help you as well.

<!--more-->

## Tuning Hadoop run-time parameters

Hadoop provides a set of options on cpu, memory, disk, and network for performance tuning. Most hadoop tasks are not cpu bounded, what we usually look into is to optimize usage of memory and disk spills.

### Memory tuning

The general rule for memory tuning is: use as much memory as you can, but don't trigger swapping. The parameter you can set for task memory is `mapred.child.java.opts`. You can put it in your configuration file.

```xml
<property>
    <name>mapred.child.java.opts</name>
    <value>-Xms1024M -Xmx2048M</value>
</property>
```

You can tune the best parameters for memory by monitoring memory usage on server using Ganglia, Cloudera manager, or Nagios. Cloudera has a slide focused on memory usage tuning, the link is [here][cloudera]

### Minimize the map disk spill

Disk IO is usually the performance bottleneck. There are a lot of parameters you can tune for minimizing spilling. What I use the most are:

* compress mapper output
* Use 70% of heap memory for spill buffer in mapper

In your configuration file, you can write:

```xml
<property>
    <name>mapred.compress.map.output</name>
    <value>true</value>
</property>
<property>
    <name>mapred.map.output.compression.codec</name>
    <value>com.hadoop.compression.lzo.LzoCodec</value>
</property>
<property>
    <name>io.sort.mb</name>
    <value>800</value>
</property>
```

Although you can further tune reducer buffer, mapper sort record percent, and various of stuff, I found the best thing to do is reduce the mapper output size. Most of the time, the performance is fast enough after I refactor the mapper to output as little data as possible. For more information, check the same [cloudera's performance tuning guide][cloudera].

### Tuning mapper tasks

Unlike reducer tasks which you can specify the number of reducer, the number of mapper tasks is set implicitly. The tuning goal for the mapper is control the amount of mapper and the size of each job. When dealing with large files, hadoop split the file in to smaller chunk so that mapper can run it in parallel. However, the initializing new mapper job usually takes few seconds, this is also a overhead that we want to minimize. These are the things you can do:

* Reuse jvm task

* If the average mapper running time is shorter than one minute, you can increase the `mapred.min.split.size`, so that less mappers are allocated in slot and thus reduces the mapper initializing overhead.

* Use Combine file input format for bunch of smaller files. I had an implementation that also use `mapred.min.split.size` to implicitly control the mapper size. You can find the [project on github][]. The explanation of the project can be found on [my blog][].

The configuration file would look like this:

```xml
<property>
    <name>mapred.job.reuse.jvm.num.tasks</name>
    <value>-1</value>
</property>
<property>
    <name>mapred.max.split.size</name>
    <value>268435456</value>
</property>
<property>
    <name>mapred.min.split.size</name>
    <value>134217728</value>
</property>
```

[cloudera]: http://www.slideshare.net/Hadoop_Summit/optimizing-mapreduce-job-performance

[project on github]: https://github.com/dryman/Hadoop-CombineFileInputFormat

[my blog]: http://www.idryman.org/blog/2013/09/22/process-small-files-on-hadoop-using-combinefileinputformat-1/


### Use configuration file and command line arguments to set parameters

When I first started on hadoop, I setup those parameters in java program, but it is so hard-coded and inflexible. Thankfully, hadoop provides `Tool` interface and `ToolRunner` class to parse those parameters for you. Here's a sample program:

```java
public class ExampleJob extends Configured implements Tool{

  public static void main (String[] args) throws Exception{
    System.exit(ToolRunner.run(new ExampleJob(), args));
  }
  
  public int run(String[] args) throws Exception {
    Configuration conf = getConf();
    Job job = new Job(conf);
    // configure the rest of the job
  }
}
```

If your main class implements the interface, your program can take the config file as input:

```bash
hadoop jar ExampleJob-0.0.1.jar ExampleJob -conf my-conf.xml arg0 arg1
```

You can even pass extra parameters through command line like this:

```bash
hadoop jar ExampleJob-0.0.1.jar ExampleJob -Dmapred.reduce.tasks=20 arg0 arg1
```

Setting configuration as run-time arguments make you easier to test different parameters without recompile the program.

## Tuning application-specific performance

Beyond general hadoop parameter setup, you can optimize your map-reduce program using some small tricks. Here are the tricks that I used the most.

### Minimize your mapper output

Recall that mapper spill size is a serious performance bottleneck. The size of mapper output is sensitive to disk IO, network IO, and memory sensitive on shuffle phase. Minimizing the mapper output can improve the general performance a lot.

To do this, you can try the following

1. Filter out records on mapper side, not on reducer side.

2. Use minimal data to form your map output key and map output value.

3. Extends `BinaryComparable` interface or use Text for your map output key

4. Set mapper output to be compressed

Above all the optimization tips, I found this make the biggest change to many of my tasks, unless I can't find a smaller key to reduce the mapper output. 

### Balancing reducer's loading

Another common performance issue that you might encounter is unbalanced reducer tasks: one or several reducer takes most of the output from mapper and ran extremely long compare to other reducers.

To solve this, you can either

1. Implement a better hash function in `Partitioner` class.

2. If you know what keys are causing the issue, you can write a preprocess job to separate keys using MultipleOutputs. Then use another map-reduce job to process the special keys that cause the problem.

## Conclusion

It's fun to write raw map-reduce jobs because it gives you more precise control over performance tuning. If you already experienced hive or pig, I encourage you to try how to optimize the same job using raw map-reduce. You can find a lot of performance gain and more space to tune the performance. For more curious, you can also check the [Yahoo's tuning hadoop performance guides][yahoo].


[yahoo]: http://www.slideshare.net/ydn/hadoop-summit-2010-tuning-hadoop-to-deliver-performance-to-your-application
