# dependently-typed-cl

I have been told [dependently-typed-functions](https://github.com/digikar99/dependently-typed-functions) are not really dependently typed, but to me, they seem interesting none the less. This system is built over it, but also over a couple of other systems that have been in development since a while.

## Iterators

```lisp
(in-package :dependently-typed-cl.impl)

(defun reduce/iterator (fn l iv)
  (declare (optimize speed)
           (type (simple-array single-float 1) l))
  (dept:imlet ((a (dept:vector-iterator l 0)))
    (declare (dynamic-extent a))
    (dept:reduce-iterator fn a iv)))

(defun reduce/native (fn l iv)
  (declare (optimize speed)
           (type (simple-array single-float 1) l))
  (reduce fn l :initial-value iv))
```

We exceeded SBCL's performance!

```
IMPL> (let ((l (aops:rand* 'single-float 100)))
        (time (loop repeat 100000 do (reduce/iterator #'+ l 0))))
Evaluation took:
  0.408 seconds of real time
  0.408714 seconds of total run time (0.408714 user, 0.000000 system)
  100.25% CPU
  902,427,486 processor cycles
  0 bytes consed
  
NIL
; processing (DEFUN REDUCE/NATIVE ...)
IMPL> (let ((l (aops:rand* 'single-float 100)))
        (time (loop repeat 100000 do (reduce/native #'+ l 0))))

Evaluation took:
  0.460 seconds of real time
  0.461501 seconds of total run time (0.461501 user, 0.000000 system)
  100.43% CPU
  1,018,979,962 processor cycles
  0 bytes consed
  
NIL
```

Of course, this is a toy example, there is no way to start from end, etc, well, at least not for a unidirectional iterator.

## Type propagating lets: imlet, imlet*

Of crucial importance to the performance of iterators is the automagic type inference. This is enabled by IMLET and IMLET* - im for immutable - which expect their bindings to be unmutated,
and thereby automatically add in the type declarations for the bindings without violating their correctness.

In simpler words, they simply expand into the appropriate CL:LET or CL:LET* while also sprinkling it with type declarations:

```lisp
IMPL> (macroexpand-1 `(dept:imlet* ((a 2)
                                    (b 3))))
(COMMON-LISP:LET* ((A 2) (B 3))
  (DECLARE (COMMON-LISP:TYPE (EQL 2) A)
           (EXTYPE (EQL 2) A)
           (COMMON-LISP:TYPE (EQL 3) B)
           (EXTYPE (EQL 3) B)))
T
```

This gets fairly extensive with iterators:

```lisp
IMPL> (macroexpand-1 `(dept:imlet* ((it1 (dept:vector-iterator v 0))
                                    (it2 (dept:filter-iterator 'evenp it1))
                                    (it3 (dept:map-iterator fn it2)))
                        (declare (dynamic-extent it1 it2 it3))
                        (let (a)
                          (loop do
                            (multiple-value-bind (elt validp) (dept:iterator-next it3)
                              (setq a elt)
                              (when (null validp) (return))))
                          a)))
(COMMON-LISP:LET* ((IT1 (DEPT:VECTOR-ITERATOR V 0))
                   (IT2 (DEPT:FILTER-ITERATOR 'EVENP IT1))
                   (IT3 (DEPT:MAP-ITERATOR FN IT2)))
  (DECLARE (COMMON-LISP:TYPE DEPT:VECTOR-ITERATOR IT1)
           (EXTYPE (DEPT:VECTOR-ITERATOR T) IT1)
           (COMMON-LISP:TYPE DEPT:FILTER-ITERATOR IT2)
           (EXTYPE (DEPT:FILTER-ITERATOR (EQL EVENP) (DEPT:VECTOR-ITERATOR T)) IT2)
           (COMMON-LISP:TYPE DEPT:MAP-ITERATOR IT3)
           (EXTYPE
            (DEPT:MAP-ITERATOR T
                               (DEPT:FILTER-ITERATOR (EQL EVENP)
                                                     (DEPT:VECTOR-ITERATOR T)))
            IT3))
  (DECLARE (DYNAMIC-EXTENT IT1 IT2 IT3))
  (LET (A)
    (LOOP DO (MULTIPLE-VALUE-BIND (ELT VALIDP)
                 (DEPT:ITERATOR-NEXT IT3)
               (SETQ A ELT)
               (WHEN (NULL VALIDP) (RETURN))))
    A))
T
```

## Orthogonally specializing types and dependently typed functions

To actually propagate the types, we need expressive types. This expressive-ness is brought to you by orthogonally-specializing-types, which essentially enables a class with a type specifier, such that each element of the type specifier can be uniquely determined from the object. This closely relates to the notion of principal type in ML-like systems, but is limited only to concrete values.

However, in the general case, given the expressiveness of the CL type system, propagating these types can be hard. But it is possible to work around the hard work by using dependently typed functions, which essentially allow the return type to be computed or aborted using the parameter types. All of them can depend on the runtime values passed to the function, and it is in this sense, that this system is dependently-typed.

Above, `dept:vector-iterator dept:filter-iterator dept:map-iterator` are three ortho- types, each with one or more arguments. This information in the types can then be used by the `dept:iterator-next` to optimize itself.
