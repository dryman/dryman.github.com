---
layout: post
title: "Lisp Debugging Tools"
date: 2012-03-26 09:51
comments: true
categories: Lisp
---

## Debug in Step Mode

I'm an old school debugger, and I love to use gdb like debug tools. The
alternative debug environment in lisp is the `step mode`. To enter it, simply
enter:

    CL-USER> (step (whatever-function arg1 arg2 arg3))
    step 1 --> (WHATEVER-FUNCTION ARG1 ARG2 ARG3)
    Step 1 [2]>

Now you can enter most commands you are familiar in gdb and other cool stuffs
lisp provides to you. Try type `:h` and see what [clisp] outputs:

~~~~~
Commands may be abbreviated as shown in the second column.
COMMAND        ABBR     DESCRIPTION
Help           :h, ?    print this command list
Error          :e       print the last error message
Inspect        :i       inspect the last error
Abort          :a       abort to the next recent input loop
Unwind         :uw      abort to the next recent input loop
Reset          :re      toggle *PACKAGE* and *READTABLE* between the
                          local bindings and the sane values
Quit           :q       quit to the top-level input loop
Where          :w       inspect this frame
Up             :u       go up one frame, inspect it
Top            :t       go to top frame, inspect it
Down           :d       go down one frame, inspect it
Bottom         :b       go to bottom (most recent) frame, inspect it
Mode mode      :m       set stack mode for Backtrace: 1=all the stack elements
             2=all the frames                         3=only lexical frames
             4=only EVAL and APPLY frames (default)   5=only APPLY frames
Frame-limit n  :fl      set the frame-limit for Backtrace. This many frames
                          will be printed in a backtrace at most.
Backtrace [mode [limit]] :bt  inspect the stack
Break+         :br+     set breakpoint in EVAL frame
Break-         :br-     disable breakpoint in EVAL frame
Redo           :rd      re-evaluate form in EVAL frame
Return value   :rt      leave EVAL frame, prescribing the return values
Step           :s       step into form: evaluate this form in single step mode
Next           :n       step over form: evaluate this form at once
Over           :o       step over this level: evaluate at once up to the next return
Continue       :c       switch off single step mode, continue evaluation
-- Step-until :su, Next-until :nu, Over-until :ou, Continue-until :cu --
           same as above, specify a condition when to stop
~~~~~

**Voilà!** Note that `Step` will step into every S-expression and `Next` will
only evaluate the expression and return the value. The great news is you can
do any evaluations in `Step mode`. For example, you can type in a S-expression
to exam a variable, use `(inspect object)` to view detail information, or even
redefine some functions at runtime!

## Basic usage of trace

The `trace` macro can save you a lot of time in debugging. For simple usage:

~~~~~
USER(17): (trace list-reverse-aux)
(LIST-REVERSE-AUX)
USER(18): (list-reverse '(1 2 3 4))
 0: (LIST-REVERSE (1 2 3 4))
   1: (LIST-REVERSE-AUX (1 2 3 4) NIL)
     2: (LIST-REVERSE-AUX (2 3 4) (1))
       3: (LIST-REVERSE-AUX (3 4) (2 1))
         4: (LIST-REVERSE-AUX (4) (3 2 1))
           5: (LIST-REVERSE-AUX NIL (4 3 2 1))
           5: returned (4 3 2 1)
         4: returned (4 3 2 1)
       3: returned (4 3 2 1)
     2: returned (4 3 2 1)
   1: returned (4 3 2 1)
 0: returned (4 3 2 1)
(4 3 2 1)
~~~~~

### Advanced usage of trace

If you want more information in execution, you can tell `trace` to give your
more. 

**(TRACE function-name ...)** makes the functions *function-name*, ... traced. Each *function-name* should be either a function name or a LIST `(function-name &KEY :SUPPRESS-IF :MAX-DEPTH :STEP-IF :BINDINGS :PRE :POST :PRE-BREAK-IF :POST-BREAK-IF :PRE-PRINT :POST-PRINT :PRINT)`, where

* `:SUPPRESS-IF form`
      no trace output as long as form is true
* `:MAX-DEPTH form`
      no trace output as long as `(> *trace-level* form)`. This is useful for
      tracing functions that are use by the tracer itself, such as PRINT-OBJECT,
      or otherwise when tracing would lead to an infinite recursion.
* `:STEP-IF form`
      invokes the stepper as soon as form is true
* `:BINDINGS ((variable form)...)`
      binds variables to the result of evaluation of forms around evaluation of
      all of the following forms
* `:PRE form`
      evaluates form before calling the function
* `:POST form`
      evaluates form after return from the function
* `:PRE-BREAK-IF form`
      goes into the break loop before calling the function if form is true
* `:POST-BREAK-IF form`
      goes into the break loop after return from the function if form is true
* `:PRE-PRINT form`
      prints the values of form before calling the function
* `:POST-PRINT form`
      prints the values of form after return from the function
* `:PRINT form`
      prints the values of form both before calling and after return from the function


In all these forms you can access the following variables:


* `EXT:*TRACE-FUNCTION*`
      the traced function itself
* `EXT:*TRACE-ARGS*`
      the arguments to the function
* `EXT:*TRACE-FORM*`
      the function/macro call as form
* `EXT:*TRACE-VALUES*`
      after return from the function: the list of return values from the function call

I copied the exapmple from [25.2. Debugging Utilities](http://www.clisp.org/impnotes/debugger.html#trace-call-id).

~~~~~
(defun f0 (x)
  (cond ((zerop x) 1)
        ((zerop (random 2)) (* x (f0 (1- x))))
        (t (* x (f1 (1- x))))))
⇒ F0
(defun f1 (x)
  (cond ((zerop x) 1)
        ((zerop (random 2)) (* x (f0 (1- x))))
        (t (* x (f1 (1- x))))))
⇒ F1
(defvar *f0-call-count* 0)
⇒ *F0-CALL-COUNT*
(defvar *id0*)
⇒ *ID0*
(defvar *cc0*)
⇒ *CC0*
(defvar *f1-call-count* 0)
⇒ *F1-CALL-COUNT*
(defvar *id1*)
⇒ *ID1*
(defvar *cc1*)
⇒ *CC1*
(trace (f0 :bindings ((*cc0* (incf *f0-call-count*))
                      (*id0* (gensym "F0-")))
           :pre-print (list 'enter *id0* *cc0*)
           :post-print (list 'exit *id0* *cc0*))
       (f1 :bindings ((*cc1* (incf *f1-call-count*))
                      (*id1* (gensym "F1-")))
           :pre-print (list 'enter *id1* *cc1*)
           :post-print (list 'exit *id1* *cc1*)))
;; Tracing function F0.
;; Tracing function F1.
⇒ (F0 F1)
(f0 10)
1. Trace: (F0 '10)
(ENTER #:F0-2926 1)
2. Trace: (F1 '9)
(ENTER #:F1-2927 1)
3. Trace: (F0 '8)
(ENTER #:F0-2928 2)
4. Trace: (F1 '7)
(ENTER #:F1-2929 2)
5. Trace: (F1 '6)
(ENTER #:F1-2930 3)
6. Trace: (F1 '5)
(ENTER #:F1-2931 4)
7. Trace: (F1 '4)
(ENTER #:F1-2932 5)
8. Trace: (F0 '3)
(ENTER #:F0-2933 3)
9. Trace: (F1 '2)
(ENTER #:F1-2934 6)
10. Trace: (F0 '1)
(ENTER #:F0-2935 4)
11. Trace: (F1 '0)
(ENTER #:F1-2936 7)
(EXIT #:F1-2936 7)
11. Trace: F1 ==> 1
(EXIT #:F0-2935 4)
10. Trace: F0 ==> 1
(EXIT #:F1-2934 6)
9. Trace: F1 ==> 2
(EXIT #:F0-2933 3)
8. Trace: F0 ==> 6
(EXIT #:F1-2932 5)
7. Trace: F1 ==> 24
(EXIT #:F1-2931 4)
6. Trace: F1 ==> 120
(EXIT #:F1-2930 3)
5. Trace: F1 ==> 720
(EXIT #:F1-2929 2)
4. Trace: F1 ==> 5040
(EXIT #:F0-2928 2)
3. Trace: F0 ==> 40320
(EXIT #:F1-2927 1)
2. Trace: F1 ==> 362880
(EXIT #:F0-2926 1)
1. Trace: F0 ==> 3628800
⇒ 3628800
*f0-call-count*
⇒ 4
*f1-call-count*
⇒ 7
~~~~~

### Pretty backtrace

The original backtrace dump message is not good enough, thanks to [Juho's
post][backtrace], you can put this in your `.sbclrc`:

{% codeblock lang:cl %}
(defun backtrace-with-extra-info (&key (start 1) (end 20))
  (swank-backend::call-with-debugging-environment
   (lambda ()
     (loop for i from start to (length (swank-backend::compute-backtrace
                                        start end))
           do (ignore-errors (print-frame i))))))
(defun print-frame (i)
  (destructuring-bind (&key file position &allow-other-keys)
      (apply #'append
             (remove-if #'atom
                        (swank-backend:frame-source-location-for-emacs i)))
    (let* ((frame (swank-backend::nth-frame i))
           (line-number (find-line-position file position frame)))
      (format t "~2@a: ~s~%~
                   ~:[~*~;~:[~2:*    At ~a (unknown line)~*~%~;~
                             ~2:*    At ~a:~a~%~]~]~
                   ~:[~*~;    Local variables:~%~{      ~a = ~s~%~}~]"
              i
              (sb-debug::frame-call (swank-backend::nth-frame i))
              file line-number
              (swank-backend::frame-locals i)
              (mapcan (lambda (x)
                        ;; Filter out local variables whose variables we
                        ;; don't know
                        (unless (eql (getf x :value) :<not-available>)
                          (list (getf x :name) (getf x :value))))
                      (swank-backend::frame-locals i))))))
(defun find-line-position (file char-offset frame)
  ;; It would be nice if SBCL stored line number information in
  ;; addition to form path information by default Since it doesn't
  ;; we need to use Swank to map the source path to a character
  ;; offset, and then map the character offset to a line number
  (ignore-errors
   (let* ((location (sb-di::frame-code-location frame))
          (debug-source (sb-di::code-location-debug-source location))
          (line (with-open-file (stream file)
                  (1+ (loop repeat char-offset
                            count (eql (read-char stream) #\Newline))))))
     (format nil "~:[~a (file modified)~;~a~]"
             (= (file-write-date file)
                (sb-di::debug-source-created debug-source))
             line))))
{% endcodeblock %}

And now in your REPL, the following code:

{% codeblock lang:cl %}
(declaim (optimize debug))
(defun foo (x)
  (let ((y (+ x 3)))
    (backtrace)
    (backtrace-with-extra-info)
    (+ x y)))
(defmethod bar ((n fixnum) (y (eql 1)))
  (foo (+ y n)))
{% endcodeblock %}

The old backtrace would look like:

~~~~
1: (FOO 4)
2: ((SB-PCL::FAST-METHOD BAR (FIXNUM (EQL 1)))
    #<unused argument>
    #<unused argument>
    3
    1)
3: (SB-INT:SIMPLE-EVAL-IN-LEXENV (BAR 3 1) #<NULL-LEXENV>)
~~~~

And the new one:

~~~~
1: FOO
   At /tmp/testlisp:5
   Local variables:
     X = 4
     Y = 7
2: (SB-PCL::FAST-METHOD BAR (FIXNUM (EQL 1)))
   At /tmp/testlisp:8
   Local variables:
     N = 3
     Y = 1
3: SB-INT:SIMPLE-EVAL-IN-LEXENV
   At /scratch/src/sbcl/src/code/evallisp:93 (file modified)
   Local variables:
     ARG-0 = (BAR 3 1)
     ARG-1 = #<NULL-LEXENV>
~~~~

**Voilà!** 



[aquamacs]: http://aquamacs.org/
[SLIME]: http://common-lisp.net/project/slime/
[clisp]: http://www.clisp.org/
[quicklisp]: http://www.quicklisp.org/beta/
[asdf.lisp]: http://www.cliki.net/asdf
[sbcl]: http://www.sbcl.org/
[osx-gcc]: https://github.com/kennethreitz/osx-gcc-installer
[homebrew]: http://mxcl.github.com/homebrew/
[slime plugin]: http://braeburn.aquamacs.org/plugins/
[Emacs 24]: http://emacsformacosx.com/builds
[emacs starter-kit]: https://github.com/technomancy/emacs-starter-kit
[backtrace]: http://jsnell.iki.fi/blog/archive/2007-12-19-pretty-sbcl-backtraces.html
