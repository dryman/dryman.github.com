---
layout: post
title: "C/Objc-C block syntax explained"
date: 2012-09-21 20:33
comments: true
categories: Cocoa
---

Block is heavily used in objective-c  APIs. If you want to
write concurrent and responsive interface, you will need blocks and grand
central dispatch. Blocks also bring many goods form functional programming to cocoa.
It is just awesome.

However, when I first learn blocks I found the syntax confuses me a lot. 
This is why I wrote this article. Hope this article can help people who have the
same problem as I did.

<!-- more -->

## Declare a block variable

Block syntax inherited form C's function pointers. To declare a block variable,
you write:

{% codeblock lang:objc %}
    int (^multiply)(int, int) = ^int(int a, int b) { return a*b; };
  //^^^^^^^^^^^^^^^^^^^^^^^^          ^^^^^^^^^^^  ^^^^^^^^^^^^^^^
  //  declare block variable          block          block body
  //  "multiply"                      argument

    int result = multiply(3, 5); // 15
  // execute a block
{% endcodeblock %}

It is similar to function pointer:

{% codeblock lang:c %}
    int mutiply(int a, int b){
        return a*b;
    }

    int (*functionPt)(int, int) = &mutiply;

    functionPt(3, 5); // 15
{% endcodeblock %}

### Block literal syntax shortcut

Block literal can be written in various ways:

{% codeblock lang:objc %}
    int (^myBlock)(int) = ^int(int a) {...};
    int (^myBlock)(int) = ^(int a) {...};   // same

    int (^myBlock)(void) = ^(void) {...};
    int (^myBlock)(void) = ^{...};          // same
    int (^myBlock)() = ^{...};              // same

    void (^myBlock)() = ^{...};             // valid
    (^myBLock)() = ^{...};                  // invalid
    void (^myBlock) = ^{...};               // invalid
{% endcodeblock %}

### Anonymous block

You don't need a block variable to use a block. A block without a block variable
is called *anonymous block*.

{% codeblock lang:objc %}
    // An anonymous block
    ^int (id obj1, id obj2) {...};
{% endcodeblock %}

Many objective-c methods accepts anonymous block:
{% codeblock lang:objc %}
    NSArray *sortedArray = [unsortedArray sortedArrayUsingComparator:
        ^(id obj1, id obj2){
            ...
        }];
{% endcodeblock %}

### Compare with function pointer

{% codeblock lang:objc %}
    struct s_data
    {
        int a;
        char* name[10];
        unsigned b;
    };

    NSInteger compareFunction(id obj1, id obj2, void* context){
        struct s_data *data = (struct s_data *) context;
        // do things
    };

    sturct s_data *my_data = (struct s_data *)malloc(sizeof(s_data));
    s_data.a = -3;
    s_data.name = "abc";
    s_data.b = 5;
    void *context = my_data;
    NSArray *sortedArray = [unsortedArray sortedArrayUsingFunction: &compareFunction
                                                           context:context];
{% endcodeblock %}

What does above mean? Well, if you want to pass a callback function to
elsewhere, sometimes you might also need to pass data. To do that, you first
pack your data into a `struct`, and use a `void` pointer points to it. Then you
pass the callback function and the `void` pointer to the function. Finally you
dereference the `void` pointer back to the `struct`.

With block, all variables in it are captured. You no longer need to do that type
casting hack to pass data.

{% codeblock lang:objc %}
    NSNumber *num = @3;

    NSArray *sortedArray = [unsortedArray sortedArrayUsingComparator:
        ^(id obj1, id obj2){
            return [num compare: obj1];
            // num is retained inside the block
        }];
{% endcodeblock %}

## Typedef

We can use `typedef` to define a reusable type:

{% codeblock lang:objc %}
    typedef int (^MyBlockType)(int, int);
    
    MyBlockType myBlock = ^(int a, int b){...};
{% endcodeblock %}

## Type cast

As other types, you can also type cast a block. The syntax is a little weired,
though.

{% codeblock lang:objc %}
    void* someContext; // Probably comes from a function's argument;

    int (^myBlock)(int,int) = (int (^)(int,int))someContext;
    // block

    int (*myFnPt)(int,int) = (int (*)(int,int))someContext;
    // function pointer
{% endcodeblock %}

## Block in Objective-C class

### property

Block in objective C is quite trivial:

{% codeblock lang:objc %}
@interface MyClass : NSObject
@property (nonatomic, copy) int(^myBlock)(int,int);
@end
{% endcodeblock %}

### Accessors and method arguments

However, it's strange in method declaration and accessors.

{% codeblock lang:objc %}
-(int(^)(int,int)) getMyBlock;
-(void) setMyBlock: (int(^)(int a, int b)) inputBlock;
{% endcodeblock %}

The syntax is weird because Apple uses type cast syntax as type declaration syntax.
This is now the only way to use anonymous type in Objective-C method argument
instead of using `typedef`. This syntax won't work in other places, either.

That's all for block syntax! There are still topics to discuss like memory
management and grand central dispatch. I'll discuss them in next few posts.
