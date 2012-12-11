---
layout: post
title: "Objective-C ARC common pitfalls and best practices"
date: 2012-11-22 09:28
comments: true
categories: Objective-C
---

Objective-C is a really cool programming language that is designed for Mac OSX and
iOS software development. TIOBE has announced November Haedline: [Objective-C on
its way to become "language of the year" again][tiobe]. It is popular not only because
the platform, but also its great performance on mobile devices. Objective-C
featured in its manually memory management instead of garbage collection. Yet,
its not that *manual* in modern Objective-C. Apple introduced Automatic
Reference Counting, ARC, that inserts memory management code for you on compile
time. In most cases of Objective-C development, it JUST WORKS. However, it
is often confusing when you mix ARC code with Core Foundation objects (low level
C references on apple's platform). Today, I'll talk about pitfalls and concepts
of ARC especially when gluing CF objects with **toll-free bridging**.

<!-- more -->

## Daily ARC

I'll first make a quick go through of ARC in two major uses: Objective-C
properties and ARC type qualifiers. Then we'll cut deep into memory management
of Core Foundation Framework.

### ARC and Objective-C properties

<script language="javascript">
function toggleContent(){
    var link = document.getElementById("expand_link");
    var content = document.getElementById("expand_content");
    if (content.style.display == 'block') {
        content.style.display = 'none';
        link.innerHTML = "Expand my highlight of ARC best pracitices by @amattn";
    } else {
        content.style.display = 'block';
        link.innerHTML = "Toggle highlight";
    }
}
</script>

It is really hard to write a new article about ARC without [@amattn][amattn]'s
collection of [ARC Best Practices][best practices]. You can find the complete
best practices there, and below <a href="javascirpt:toggleContent()">link</a>
is my highlight of his article [^foot]:

<a id="expand_link" href="javascript:toggleContent()"> Expand my highlight of ARC best pracitices by @amattn</a> 

<blockquote markdown="1" style="display: none;" id="expand_content" cite="http://amattn.com/2011/12/07/arc_best_practices.html">

* object instance variables which needs to be retained should use `strong`.

```obj-c
@property (nonatomic, strong) id childObject;
```

* To break reference cycle, use `weak` [^foot2].

```obj-c
@property (nonatomic, weak) id delegate;
@property (nonatomic, weak) NSTimer timer; // NSTimer retains target
```

* use `assign` for scalar properties

```obj-c
@property (nonatomic, assign) CGFloat width;
@property (nonatomic, assign) CGFloat height;
```

* use `copy` for immutable containers, strings and block. Avoid using mutable
  containers in properties (`NSMutableArray`, for example.) If you use mutable
  containers, use `strong`.

```obj-c
@property (nonatomic, copy) NSString* name;
@property (nonatomic, copy) NSArray* components;
@property (nonatomic, copy) (void (^)(void)) job;
@property (nonatomic, strong) NSMutableArray* badPatterns;
```

* In dealloc
    - remove observers
    - unregister notifications

* IBOutlets should generally be `weak` except File’s Owner to top-level objects in
  a nib file. If you set it to `strong`, you should set it to `nil` in
  `-(void)viewDidUnload`.

<footer><cite><a href="http://amattn.com/2011/12/07/arc_best_practices.html"> ARC Best Practices</a></cite></footer>
</blockquote>

### ARC specific type qualifiers

About type qualifier rules, you can see my [previous post][qualifiers]. There
are four ARC specific type qualifiers introduced by Apple:

1. `__strong` is the default. An object remains **alive** as long as there is a strong
  pointer to it.
2. `__weak` specifies a reference that does not keep the referenced object alive. A
  weak reference is set to `nil` when there are no strong references to the object.
3. `__unsafe_unretained` specifies a reference that does not keep the referenced
  object alive and is not set to `nil` when there are no strong references to the
  object. If the object it references is deallocated, the pointer is left
  dangling.
4. `__autoreleasing` is used to denote arguments that are passed by reference `(id *)`
  and are autoreleased on return.

Beware that ARC type qualifiers are used for POINTER TYPE. That is, you must put
the qualifier at the right hand side of the star. 

```obj-c
MyClass * __weak w_self = self;    // correct
MyClass __weak * w_self = self;    // wrong! May cause serious bugs!
__weak MyClass * w_self = self;    // wrong!

__weak typeof(self) w_self = self; 
// correct, will expand to (See gcc manual of typeof)
// __weak (MyClass *) w_self = self;

typeof(self) __weak w_self = self; // correct, its safer
```

You might wonder that there is so many wrong format on the internet. But
[Apple officially] said so:

> You should decorate variables correctly. When using qualifiers in an object
> variable declaration, the correct format is:
> `ClassName * qualifier variableName;`

## ARC and toll-free bridging

The biggest problem of ARC occurs when you mix it with Core Foundation
references. The rules of thumb are

* When you transfer an Objective-C object to a CF reference, you
  retain it.
* When you transfer a CF reference to an Objective-C object, you
  release it.
* It is dangerous if you didn't change ownership of objects. Sometimes Clang
  corrects it for you, sometimes don't.
* There is no autorelease in Core Foundation, and you must follow the Core
  Foundation memory management naming convention:
  - Those object returned from function with `Create` or `Copy`, you **own** the
    object, thus you must release it.
  - If the function name contains the word `Get`, you do not own the object. Do
    not release it.

There are two ways to retain a CF object: a type casting like syntax
`(__bridge_retained)` or C function `CFBridgingRetain`.  Though clang show up
diagnostics to use the former syntax, I prefer to use the latter one because it
is easier to read for me. 

```obj-c
CFArrayRef arr = CFBridgingRetain( @[@"abc", @"def", @3.14] );
// or CFArrayRef arr = (__bridge_retained CFArrayRef)@[...];
// do stuffs..
CFRelease(arr);
```

When you get an object from Core Foundation with name containing `Create` or
`Copy`, use `(__bridge_transfer)` or `CFBridgingRelease`.

```obj-c
- (void)logFirstNameOfPerson:(ABRecordRef)person {
 
    NSString *name = (NSString *)CFBridgingRelease(ABRecordCopyValue(person, kABPersonFirstNameProperty));
    NSLog(@"Person's first name: %@", name);
}
```

## Pitfalls in toll-free bridging

When you see a code like this:

```obj-c
- (CGColorRef)foo {
    UIColor* color = [UIColor redColor];
    return [color CGColor];
}
```

Beware! It might crash at any time. Since we do not hold the reference of
UIColor, it would be released right after you create it! The CGColor it owns
would be released as well and thus cause a crash. There are three ways to fix
it:

* Use `__autorelease` type qualifier. UIColor would be released at the end of
current run loop. It can fix the crash. I believe [@amattn][amattn] is the first
one who discovered this solution.

```obj-c
- (CGColorRef)getFooColor {
    UIColor* __autoreleasing color = [UIColor redColor];
    return [color CGColor];
}
```

* Use Core Foundation naming convention and change the owner ship to the
   receiver.

```obj-c
- (CGColorRef)fooColorCopy {
    UIColor* color = [UIColor redColor];
    CGColorRef c = CFRetain([color CGColor]);
    return c;
}


CGColorRef c = [obj fooColorCopy];
// do stuffs
CFRelease(c);
```

* Owns the CF object by self. If self is dealloced, the reference would still
cause a crash.

```obj-c
- (CGColorRef)getFooColor {
    CGColorRef c = self.myColor.CGColor;
    return c;
}
```

## Pitfalls in block and ARC

When you use a ivar in self owned block, it will implicitly contain self in your
block and thus cause a retain cycle:

```obj-c
@interface MyClass {
    id child;
}
@property (nonatomic, strong) (void(^)(void)) job;
@end

@implementation MyClass
- (void)foo {
    self.job = ^{
        [child work];
        // will expand to [self->child work]
    };
}
```

The only way to implement this safely is to use weak reference of self, and
setup a strong reference to weak self only in the scope of this block. The
reason we need to use a strong reference in scope is weak reference can be zero
out at any time. We must claim we own the object when we are using it.

```obj-c
- (void)foo {
    MyClass* __weak w_self = self;
    self.block = ^{
        MyClass* s_self = w_self; // self retained, but only in this scope!
        if (s_self) {
            [s_self->child work];
            // do other stuffs
        }
    };
}
```

## Pitfalls in NSError

If you are implementing methods that take NSError, be sure to use the correct
format of type qualifier!

```obj-c
- (void)doStuffWithError:(NSError* __autoreleasing *)error; // correct
- (void)doStuffWithError:(__autoreleasing NSError **)error; // wrong!
```

Actually, when you craete a NSError object, it is always best to declare it is
an autoreleasing object:

```obj-c
NSError* __autoreleasing error = nil; // correct
__autoreleasing NSError* error = nil; // wrong
NSError* error = nil; // Will be corrected by clang
```

## Summary

ARC is handy, but not easy. When you facing complex memory ownership model,
wrting some testing code to know how retain count being managed is still a
recommanded pactice.  The below snippet is how I test retain count when mixing
CF object, block, and objective-c object:

```obj-c
- (void)testCGColorRetainCount1
{
    CGColorRef s_ref;
    @autoreleasepool {
        UIColor * __autoreleasing shadowColor = [UIColor colorWithRed:0.12 green:0.12 blue:0.12 alpha:1.0];
        s_ref = shadowColor.CGColor;
        CFRetain(s_ref);
    }
    STAssertEquals(CFGetRetainCount(s_ref), 1L, @"retain count owned by us");
    
    CGColorRef(^strangeBlock)(void) = ^{
        return CGColorCreateCopy(s_ref);
    };
    
    CGColorRef myCopy = strangeBlock();
    STAssertEquals(CFGetRetainCount(s_ref), 2L, @"retain count owned by block and us");
    CFRelease(s_ref);
    STAssertEquals(CFGetRetainCount(s_ref), 1L, @"retain count owned by block");
    STAssertEquals(CFGetRetainCount(myCopy), 1L, @"retain count owned by us");
    CFRelease(myCopy);
}
```

Hope these helps! Comments and sharing are welcome!

[tiobe]: http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html
[qualifiers]: http://www.idryman.org/blog/2012/10/29/type-qualifiers/
[apple]: http://developer.apple.com/library/mac/#releasenotes/ObjectiveC/RN-TransitioningToARC/Introduction/Introduction.html
[amattn]: https://twitter.com/amattn
[best practices]: http://amattn.com/2011/12/07/arc_best_practices.html
[^foot]: I didn't reference back to @amattn's ARC best practices when I first
    post this article.  I am really sorry that I didn't do it and I really
    appreciate his pioneer work. I couldn't write this article without his awesome
    collection of how to write good ARC code.

[^foot2]: Thanks to Tinghui's comment that `NSTimer` should not be invalidated in
    dealloc. `NSTimer` retains its target, so one should  use weak references if
    the timer is a member of target's property, else just leave it is fine.
