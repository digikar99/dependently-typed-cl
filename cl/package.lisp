(uiop:define-package #:dependently-typed-cl
  (:use)

  (:import-from #:cl
                #:in-package
                #:use-package
                #:defpackage
                #:declare
                #:optimize
                #:speed
                #:safety

                #:fixnum
                #:single-float
                #:double-float
                #:eql
                #:t
                #:nil

                #:disassemble
                #:lambda
                #:defmacro

                #:svref)
  (:export #:cl
           #:in-package
           #:use-package
           #:defpackage
           #:declare
           #:optimize
           #:speed
           #:safety

           #:fixnum
           #:single-float
           #:double-float
           #:eql
           #:t
           #:nil

           #:disassemble
           #:lambda
           #:defmacro

           #:svref)

  (:mix #:dependently-typed-functions
        #:extensible-compound-types)

  ;; CLTL2 declaration propagating lets
  (:export #:imlet
           #:imlet*
           #:imlet+)
  ;; simple-math
  (:export #:coerce-number

           #:add
           #:1+
           #:incf
           #:subtract
           #:1-
           #:decf

           #:multiply
           #:divide

           #:mod
           #:rem
           #:log

           #:sin
           #:cos
           #:tan
           #:sinh
           #:cosh
           #:tanh
           #:asinh
           #:acosh
           #:atanh

           #:asin
           #:acos
           #:atan)
  ;; lisp-lists
  (:export #:list-of
           #:list-of*
           #:list
           #:make-list
           #:car
           #:cdr
           #:nth
           #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth)
  ;; array
  (:export #:make-array
           #:vector
           #:aref
           #:row-major-aref)
  ;; simple-functional
  (:export #:funcall
           #:apply)
  ;; iterator
  (:export #:make-iterator
           #:iterator
           #:define-iterator
           #:iterator-next
           #:list-iterator
           #:vector-iterator
           #:map-iterator
           #:filter-iterator
           #:do-iterator
           #:reduce-iterator)
  ;; sequence
  (:export #:iterable)
  ;; functional
  (:export #:reduce
           #:map!
           #:map)
  ;; math
  (:export #:+
           #:-
           #:*
           #:/
           #:floor))

(defpackage #:dependently-typed-cl.impl
  (:use #+extensible-compound-types
        #:extensible-compound-types-cl
        #-extensible-compound-types
        #:cl
        #:dependently-typed-functions
        #:polymorphic-functions)
  (:import-from #:extensible-compound-types
                #:typexpand
                #:specializing
                #:extype
                #:upgraded-cl-type)
  (:import-from #:extensible-compound-types-cl.impl
                #:extype-declarations
                #:cl-type-declarations
                #:prepare-extype-checks)
  (:import-from #:extensible-compound-types.impl
                #:simplify-and-type)
  (:import-from #:cl-form-types
                #:combine-values-types
                #:constant-form-value)
  (:import-from #:dependently-typed-functions
                #:wrap-in-eql/compile-time
                #:wrap-in-eql/run-time/rest
                #:wrap-in-eql/run-time/key)
  (:local-nicknames (#:dept #:dependently-typed-cl)
                    (#:a #:alexandria)))
