---
layout: post
title: "Notes on Obj-C blocks"
date: 2012-08-01 16:11
comments: true
published: false 
categories: Cocoa
---

## Syntax

    int ^(myBlock)(int,int);

    void ^(myVoidBlock)(int,int);
    ^(myVoidBlock)(int,int); // same

    ^(myVoidBlock2)(void);
    ^(myVoidBlock2); // same

    typedef int (^BlockType)(int,int);
    BlockType myBlock1, myBlock2;

    // Annoymous block
    ^int(int x, int y) {
      return x + y;
    };

    myBlock = ^int(int x, int y) {
      return x + y;
    };
    int ^(myBlock)(int, int) = ^int(int x, int y){
      return x + y;
    };


    @interface MyObj : NSObject
    @property (nonautomic,copy) int ^(myBlock)(int,int);
    - (void) setMyBlock: (int (^)(int,int))block;
    @end


### dispatch_sync

    bool debit_account(Account *account, Transaction *transaction)
    {
      __block bool result = false;
      dispatch_sync(account->queue, ^{
        if (transaction->amount > account->balance) return; // Beautiful logic!
        account->balance -= transaction->amount;
        result = true;
      });
      return result;
    }

    dispatch_sync(queue, ^{
      dispatch_sync(queue, ^{
        // NOT REACHED: DEADLOCK
      });
    }); // Queues are strictly FIFO


### dispatch_apply

    // for (index = 0; index < count; index++)
    // NOTE: should work on rows instead of pixels in image processing
    // striding
    dispatch_apply(count, queue, ^(size_t index) {
      outputs[index] = perform_computation(inputs[index]);
    });

http://www.slideshare.net/robby_brown/grand-central-dispatch-design-patterns
