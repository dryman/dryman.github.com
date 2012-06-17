---
layout: post
title: "Debug UIWebView in your iOS app"
date: 2012-06-17 15:44
comments: true
categories: Cocoa
---

It is often to embed a UIWebView in an iOS app. However it doesn't provide the
powerful webkit inspector by default. Nathan de Vries has written a
[post][reference] to solve the problem, but the solution didn't work on my XCode
4 with Clang front end.

{% codeblock lang:objc Nathan's solution %}
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // ...Snipped...
    [NSClassFromString(@"WebView") _enableRemoteInspector];
    // ...Snipped...
}
{% endcodeblock %}

The compiler will complain that you can't force `WebView` to perform private
method `_enableRemoteInspector`. Thus, I use `performSelector:@selector()` instead
of direct method call. Guess what? The compiler accept the hack. Awesome!

{% codeblock lang:objc %}
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // enable the hack only when we use simulator
#if (TARGET_IPHONE_SIMULATOR)
    [NSClassFromString(@"WebView") performSelector:@selector(_enableRemoteInspector)];
#endif
    // ...Snipped...
}
{% endcodeblock %}

Now simply run your iphone simulator and open the url `localhost:9999` to view your inspector. **Voilà!**


[reference]: http://atnan.com/blog/2011/11/17/enabling-remote-debugging-via-private-apis-in-mobile-safari/
