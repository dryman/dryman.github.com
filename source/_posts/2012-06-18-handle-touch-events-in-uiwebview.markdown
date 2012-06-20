---
layout: post
title: "Handle touch events in UIWebView"
date: 2012-06-18 17:53
comments: true
categories: Cocoa
---

This is an annoying problem that related to my recent project. The goal is
simple: 

> handle **single touch event** in `UIWebView` and let it handle other touch
> events as defaults.

However this is really hard to achive...

### Edit: 6/20

I found this post:[DETECTING TAPS AND EVENTS ON UIWEBVIEW – THE RIGHT
WAY](http://mithin.in/2009/08/26/detecting-taps-and-events-on-uiwebview-the-right-way/).
He handles touch event via `- (void) sendEvent:(UIEvent*) event` instead of
UIGesutreRecognizer. His approach is more general and less hacky than mine. Have
a look at it also!

<!-- more -->

### responder chain

First we need to review cocoa event responder chain, aka event delivery paths.
According to apple [Event Handling Guide for iOS][eventRef]:

> The window object uses hit-testing and the responder chain to find the view to
> receive the touch event. In hit-testing, a window calls `hitTest:withEvent:` on
> the top-most view of the view hierarchy; this method proceeds by recursively
> calling `pointInside:withEvent:` on each view in the view hierarchy that returns
> `YES`, proceeding down the hierarchy until it finds the subview within whose
> bounds the touch took place. That view becomes the hit-test view.

For example, we have a single view application with a `UIWebView` in it. The
touch event on UIWebView will be delivered in following order:

1. UIWebView
2. Container view
3. Custom view controller
4. Singleton application delegate

Even if we added a `UIGestureRecognizer` on top of `UIWebVIew` in Interface
Builder, it would not be on the event deliver path.

### Add `UIGestureRecognizer` programmatically

The standard way to add touch event listener goes as follows:

1. alloc and init `UITapGestureRecognizer`, setup tap count and delegate
2. attach the recognizer to container view
3. adopt `<UIGestureRecognizerDelegate>` protocol and return `YES` on method `- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldRecognizeSimultaneouslyWithGesture    Recognizer:(UIGestureRecognizer *)otherGestureRecognizer`

{% codeblock lang:objc %}
- (void)viewDidLoad
{
    [super viewDidLoad];
    // init your view here
    UITapGestureRecognizer *singleTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:nil];
    singleTap.numberOfTapsRequired = 1;
    singleTap.delegate = self;
    [self.view addGestureRecognizer:singleTap];
}

// UIGestureRecognizerDelegate
- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldRecognizeSimultaneouslyWithGestureRecognizer:(UIGestureRecognizer *)otherGestureRecognizer{
    return YES;
}
{% endcodeblock %}

The last step we let cocoa event system pass the event through every possible
handler in the responder chain. Thus both `UIWebView` and
`UITapGestureRecognizer` will handle the touch event. Now we only have to make
sure it handles single tap event, not long touch, double tap, or drag event.

### Problem of `UITapGestureRecognizer`

At first, I tried to implement my event handler with cocoa target action
mechanism, and use `requireGestureRecognizerToFail` to tell cocoa that I don't
want it to trigger double tap nor triple tap events.

{% codeblock lang:objc %}
- (void)viewDidLoad
    // Touch event handler
    UITapGestureRecognizer *tripleTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:nil];
    UITapGestureRecognizer *doubleTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:nil];

    UITapGestureRecognizer *singleTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(singleTap:)];
    tripleTap.numberOfTapsRequired = 3;
    doubleTap.numberOfTapsRequired = 2;
    singleTap.numberOfTapsRequired = 1;
    tripleTap.delegate = self;
    doubleTap.delegate = self;
    singleTap.delegate = self;
    [self.view addGestureRecognizer:tripleTap];
    [doubleTap requireGestureRecognizerToFail:tripleTap];
    [self.view addGestureRecognizer:doubleTap];
    [singleTap requireGestureRecognizerToFail:doubleTap];
    [self.view addGestureRecognizer:singleTap];{
}
- (void) singleTap:(UITapGestureRecognizer*)gesture {
// handle event
}
{% endcodeblock %}

However, **it just don't work!!** It always triggered the `singleTap:` method
when I tapped twice or more. I need to find another way to fix the problem.

### Hack on `<UIGestureRecognizerDelegate>`

There are two useful instance method in `<UIGestureRecognizerDelegate>`:

1. `- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch`
  return `YES` (the default) to allow the gesture recognizer to examine the touch object,
  NO to prevent the gesture recognizer from seeing this touch object.
  This method is called before `touchesBegan:withEvent:` is called on the gesture
  recognizer for a new touch.

2. `- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldRecognizeSimultaneouslyWithGestureRecognizer:(UIGestureRecognizer *)otherGestureRecognizer`
  return `YES` to allow both gestureRecognizer and otherGestureRecognizer to recognize
  their gestures simultaneously. The default implementation returns `NO`—-no two
  gestures can be recognized simultaneously. This method would be called
  frequently during long touch, drag, double tapped or any other kinds of
  events.

The `(UITouch *) touch` in first method has property `tapCount`, which is what
we want. Sadly this method is called immediately as user tap on the device.
That is, if we log out the message like this:

{% codeblock lang:objc %}
- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch {
      NSLog(@"gestureRecognizer shouldReceiveTouch: tapCount = %d",(int)touch.tapCount);
}
{% endcodeblock %}

And tap twice, it will print out

    gestureRecognizer shouldReceiveTouch: tapCount = 1
    gestureRecognizer shouldReceiveTouch: tapCount = 2

because `gestureRecognizer shouldReceiveTouch:` will be triggered every time
you touch the screen.

The way to differ single tap and others is to use a `NSTimer` that triggers the
handler later and cancel the timer if `tapCount >= 2`.

{% codeblock lang:objc %}
@interface WTViewController ()
@property (nonatomic,strong) NSTimer *timer;
@property (nonatomic,assign) UIGestureRecognizerState gestureState;
@end

@implementation WTViewController
@synthesize timer;
@synthesize gestureState;

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch {
    if (touch.tapCount ==1) {
        self.timer = [NSTimer timerWithTimeInterval:0.5 target:self selector:@selector(handleSingleTap:) userInfo:nil repeats:NO];
        [[NSRunLoop mainRunLoop] addTimer:self.timer forMode:NSRunLoopCommonModes];
        self.gestureState = UIGestureRecognizerStateBegan;
        return YES;
    }
    else if (touch.tapCount ==2 && self.timer) {
        [self.timer invalidate];
        self.timer = nil;
    }
    return NO;
}

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldRecognizeSimultaneouslyWithGestureRecognizer:(UIGestureRecognizer *)otherGestureRecognizer{
    self.gestureState = gestureRecognizer.state;
    return YES;
}

// Handler will be called from timer
- (void)handleSingleTap:(UITapGestureRecognizer*)sender {
    if (self.gestureState==UIGestureRecognizerStateRecognized) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"SingleTap" message:@"Oh yes!" delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil, nil];
        [alert show];
    }
}
@end
{% endcodeblock %}

The timer can solve multiple taps issue, but it cannot recognize long touch. The
long touch cannot be recognized in `gestureRecognizer shouldReceiveTouch:`
method, but can be detected in
`shouldRecognizeSimultaneouslyWithGestureRecognizer` method. If the touch went
too long, the `gestureRecognizer.state` would be
`UIGestureRecognizerStateFailed`. Thus I record the state and check it when the
timer finished.

There is one more thing that I should mentioned:
`shouldRecognizeSimultaneouslyWithGestureRecognizer` may not be called as
frequently as we might expected. If we tap once, wait, and long touch the
device, `handleSingleTap` may still recognize the event to be a quick tap
because `self.gestureState` may still be `UIGestureRecognizerStateRecognized` at
the time. The way to prevent this result is to reset `self.gestureState`
every time we start the timer.

### Source code

You can find the source code on my [github page][github].

[eventRef]: http://developer.apple.com/library/ios/#documentation/EventHandling/Conceptual/EventHandlingiPhoneOS/EventsiPhoneOS/EventsiPhoneOS.html#//apple_ref/doc/uid/TP40009541-CH2-SW1
[github]: https://github.com/dryman/UIWebTouch
