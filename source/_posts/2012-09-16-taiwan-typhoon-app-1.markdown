---
layout: post
title: "Taiwan Typhoon App (1)"
date: 2012-09-16 16:11
comments: true
categories: Cocoa
---

Taiwan is an island next to Pacific ocean, and one kind of serious disaster in
Taiwan is typhoon. Fortunately, We have best typhoon researches in the world,
and I want to make a step further to let people read typhoon predictions on iOS.

<img width="50%" style="margin-left:25%;" src="/images/typhoon_tw/typhoon-08-23.png"/>

<!-- more -->

There are two main resources that we can get typhoon prediction. One is 
[Central Weather Bureau, Taiwan (CWB)][cwb], and the other is 
[Joint Typhoon Warning Center (JTWC)][jtwc]. Here I'll focus on [CWB][cwb] data.

## Setup inspect environment

At the first scene, [CWB][cwb] seems hard to parse because it uses images
to show potential track area instead of using JSON API and google map.

![cwb pta](/images/typhoon_tw/cwb_pta.png "figure 2")

The html code is quite dirty and hard to inspect. The tip is to find the `iframe`
element from the inspector.

![inspect iframe](/images/typhoon_tw/inspect_iframe.png "figure 3")

Then, open the direct link in another window.

![cwb iframe](/images/typhoon_tw/cwb_pta_iframe.png "figure 4")

Now we have a relatively cleaner html to inspect.

![inspector](/images/typhoon_tw/inspector.png "figure 5")

### Get the information source

I'm lucky. The source is right in JavaScript file.

![ty info](/images/typhoon_tw/ty_infos.png "figure 6")

Though it's not JSON, but almost (lol). The direct link is

![ty info dir](/images/typhoon_tw/ty_infos_dir.png "figure 7")

The parameter of the link seems to be a clue that we can query history records,
but it doesn't. With or without the parameter it always gives us the newest
information of typhoons.

## Parse

We only interested in `var typhs= [...]`. It's easy to get data out using regular
expression.

I tested regular expression with Perl before I actually implement it in
Objective-C. If you need a tool to do text processing, Perl is always the best
choice. I strongly recommend people to learn Perl, it's worthy.

{% codeblock lang:perl https://github.com/dryman/Taiwan-Typhoon-iOS/blob/master/parse.pl source %}
    #!/usr/bin/env perl
    use 5.010;
    use English;

    undef $INPUT_RECORD_SEPARATOR;  # disable input seprator "\n"
    open my $fh, "ty_infos.js";
    $_ = <$fh>;                     # read endire file as string into $_

    /\[.+?\];/s;                    # /s modifier let '.' match "\n"
                                    # +? match not greedly
    say $MATCH;
{% endcodeblock %}

Beware that not to match greedly, else you won't get what you want. Now we can
write it in much more verbose Objective-C.

{% codeblock lang:objc %}
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"\\[.+?\\];" options:NSRegularExpressionDotMatchesLineSeparators error:&err];
    NSRange range_of_match = [regex rangeOfFirstMatchInString:ty_infos options:NSRegularExpressionDotMatchesLineSeparators range:NSMakeRange(0, ty_infos.length)];
    NSString *json = [ty_infos substringWithRange:NSMakeRange(range_of_match.location, range_of_match.length-1)];
    NSArray* arr = [NSJSONSerialization JSONObjectWithData:[json dataUsingEncoding:NSUTF8StringEncoding] options:0 error:nil];
{% endcodeblock %}


[cwb]:http://www.cwb.gov.tw/V7e/prevent/typhoon/ty.htm?
[cwb-pta]:http://www.cwb.gov.tw/V7/prevent/typhoon/Data/PTA_NEW/index_eng.htm
[jtwc]:http://jtwccdn.appspot.com/JTWC/
