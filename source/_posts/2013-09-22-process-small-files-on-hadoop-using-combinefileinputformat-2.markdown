---
layout: post
title: "Process Small Files on Hadoop using CombineFileInputFormat (2)"
date: 2013-09-22 18:41
comments: true
categories: Hadoop Java
---

Followed the previous article, in this post I ran several benchmarks and tuned the performance from 3 hours 34 minutes to 6 minutes 8 seconds!

<!--more-->

## Original job without any tuning

* `job_201308111005_0317`
* NumTasks: 9790
* Reuse JVM: false
* mean complete time: 9-Sep-2013 10:08:47 (17sec)
* Finished in: 3hrs, 34mins, 26sec

We had 9790 files to process, and the total size of the files is 53 GB. Note that for every task it still took 17 seconds to process the file.

## Using CombineFileInputFormat without setting the MaxSplitSize

* `job_201308111005_0330`
* NumTasks: 1
* Reuse JVM: false

In this benchmark I didn't set the `MaxSplitSize` in `CFInputFormat.java`, and thus Hadoop merge all the files into one super big task.
After running this task for 15 minutes, hadoop killed it. Maybe its a timeout issue, I didn't dig into this.
The start and the end of the task logs look like this:
 
    13/09/09 16:17:29 INFO mapred.JobClient:  map 0% reduce 0%
    13/09/09 16:32:45 INFO mapred.JobClient:  map 40% reduce 0%
 
    13/09/09 16:33:02 INFO mapred.JobClient: Task Id : attempt_201308111005_0330_m_000000_0, Status : FAILED
    java.lang.Throwable: Child Error
        at org.apache.hadoop.mapred.TaskRunner.run(TaskRunner.java:271)
        Caused by: java.io.IOException: Task process exit with nonzero status of 255.
        at org.apache.hadoop.mapred.TaskRunner.run(TaskRunner.java:258)

## Using CombineFileInputFormat with block size 64 MB

* `job_201308111005_0332`
* Reuse JVM = false
* max split size = 64MB
* NumTasks: 760
* mean complete time: 9-Sep-2013 16:55:02 (24sec)
* Finished in: 23mins, 6sec

After modifying `MaxSplitSize` the total runtime has reduced to 23 minutes! The total tasks drops from 9790 to 760, about 12 times smaller. The time difference is 9.3 times faster, pretty nice! However, the mean complete time doesn't scale like other factors. The reason was it's a big overhead to start JVM over and over again.

## Using CombineFileInputFormat with block size 64MB and reuse JVM

To reuse the JVM, just set `mapred.job.reuse.jvm.tasks` to `-1`. 

```java
  public static void main(String[] argv) throws Exception {
    Configuration conf = new Configuration();
    conf.setInt("mapred.job.reuse.jvm.num.tasks", -1);
    int res = ToolRunner.run(conf, new HadoopMain(), argv);
    System.exit(res);
  }
```

The result is awesome! **6 minutes and 8 seconds**, wow!

* `job_201308111005_0333`
* Reuse JVM = true
* max split size = 64MB
* NumTasks: 760
* mean complete time: 9-Sep-2013 17:30:23 (5sec)
* Finished in: 6mins, 8sec

## Use FileInputFormat and reuse JVM

Just curious the performance difference if we only change the JVM parameter:

* `job_201308111005_0343 `
* NumTasks: 9790
* mean complete time: 10-Sep-2013 17:04:18 (3sec)
* Reuse JVM = true
* Finished in: 24mins, 49sec

## Tuning performance over block size

Let's jump to the conclusion first: changing the block size doesn't affect the performance that much, and I found 64 MB is the best size to use. Here are the benchmarks:

### 512 MB

* `job_201308111005_0339`
* Reuse JVM = true
* max split size = 512MB
* NumTasks: 99
* mean complete time: 10-Sep-2013 11:55:26 (24sec)
* Finished in: 7min 13sec

### 128 MB

* `job_201308111005_0340`
* Reuse JVM = true
* max split size = 128 MB
* NumTasks: 341
* mean complete time: 10-Sep-2013 13:13:20 (9sec)
* Finished in: 6mins, 41sec

# Conclusion

So far the best practice I learned from these benchmarks are:

1. Setup the `mapred.job.reuse.jvm.num.tasks` flag in configuration. This is the easiest tuning to do, and it makes nearly 10 times performance improvement.
2. Write your own `CombineFileInputFormat` implementation.
3. The block size can be 64 MB or 128 MB, but doesn't make big difference between the two.

Still, try to model your problems into sequence file or map file in hadoop. HDFS should handle localities with these files automatically.
What about `CFInputFormat`? Does it handle locality in HDFS system too?
I can't confirm it but I guess sorting the keys based on line offset first then file name also guarantees the locality of assigning data to mapper. When I have time to dig more from HDFS API, I'll look back to this benchmark and see what can I further tune the program.
