(in-package #:dependently-typed-cl.impl)

;;; Idea credits: commander-trashdin / Andrew

(defstruct dept:iterator)
(define-polymorphic-function dept:iterator-next (dept:iterator) :overwrite t)

(defmacro dept:define-iterator (name (slots specializers) read-only-slots &body body)
  (let ((make-function-name (intern (uiop:strcat "MAKE-" (symbol-name name)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,make-function-name))
       (cl:defstruct (,name (:include dept:iterator)
                            (:constructor ,make-function-name ,slots))
         ,@slots)
       (define-orthogonally-specializing-type ,name
           (&optional ,@(loop :for s :in specializers :collect `(,s 'cl:*)))
         (,@(loop :for s :in specializers :collect
                  `(,s :accessor (cl:lambda (o)
                                   (orthogonally-specializing-type-of
                                    (,(intern (uiop:strcat (string name) "-"
                                                           (string s)))
                                     o))))))
         :to-cl-type (cl:lambda (type) (declare (ignore type)) ',name))
       (def-dept-fun ,name ,slots
           (list ',name ,@specializers)
         (,make-function-name ,@slots))
       ,@(a:with-gensyms (s st iterator iterator-type env type-decl specializer-types)
           `((defpolymorph (dept:iterator-next :inline t) ((,iterator ,name)) t
               (let (,@(loop :for s :in read-only-slots
                             :collect `(,s (,(intern (uiop:strcat (string name) "-"
                                                                  (string s)))
                                            ,iterator))))
                 (with-slots ,(set-difference slots read-only-slots) ,iterator
                   ,@body)))
             (defpolymorph-compiler-macro dept:iterator-next (,name)
                 (,iterator &environment ,env)
               (let* ((,iterator-type (cl-form-types:nth-form-type ,iterator ,env 0 t t))
                      (,type-decl
                        (optima:match (typexpand ,iterator-type)
                          ((list* 'specializing ',name ,specializer-types)
                           (loop :for ,s :in ',specializers
                                 :for ,st :in ,specializer-types
                                 :nconcing `((extype ,,st ,,s)
                                             (cl:type ,(upgraded-cl-type ,st) ,,s)))))))
                 `(let (,@(loop :for ,s :in ',read-only-slots
                                :collect `(,,s (,(intern (uiop:strcat (string ',name) "-"
                                                                      (string ,s)))
                                                ,,iterator))))
                    (declare ,@,type-decl)
                    (with-slots ,',(set-difference slots read-only-slots) ,,iterator
                      ,@',body)))))))))

(dept:define-iterator dept:list-iterator ((list) ()) ()
  (if list
      (values (let ((first (car list)))
                (setf list (cdr list))
                first)
              t)
      (values nil nil)))

(dept:define-iterator dept:vector-iterator ((vector index) (vector)) (vector)
  (locally (declare (type fixnum index))
    (if (cl:< index (length vector))
        (values (let ((elt (aref vector index)))
                  (incf index)
                  elt)
                t)
        (values nil nil))))

(dept:define-iterator dept:map-iterator
    ((function iterator) (function iterator))
    (function iterator)
  (multiple-value-bind (elt validp) (dept:iterator-next iterator)
    (values (when validp (funcall function elt)) validp)))

(dept:define-iterator dept:filter-iterator
    ((filter-function iterator) (filter-function iterator))
    (filter-function iterator)
  (loop do
    (multiple-value-bind (elt validp) (dept:iterator-next iterator)
      (cond ((and validp
                  (funcall filter-function elt))
             (return (values elt t)))
            ((not validp)
             (return (values nil nil)))))))

(def-dept-fun dept:do-iterator (iterator)
    `(values &optional)
  (loop do
    (multiple-value-bind (elt validp) (dept:iterator-next iterator)
      (declare (ignore elt))
      (unless validp (return (values))))))

#|

(defun iterate/iterator (fn1 fn2 l)
  (declare (optimize speed)
           (type list l))
  (dept:imlet* ((it1 (dept:list-iterator l))
                (it2 (dept:filter-iterator fn1 it1))
                (it3 (dept:map-iterator fn2 it2)))
    (declare (dynamic-extent it1 it2 it3))
    (let (a)
      (loop do
        (multiple-value-bind (elt validp) (dept:iterator-next it3)
          (setq a elt)
          (when (null validp) (return))))
      a)))

(defun iterate/native (fn1 fn2 l)
  (declare (optimize speed)
           (type list l))
  (let (a)
    (loop :for elt :in l
          :do (when (funcall fn1 elt)
                (setq a (funcall fn2 elt))))
    a))

|#

(def-dept-fun dept:reduce-iterator (function iterator initial-value)
    `(values t &optional)
  (loop with final-value = initial-value
        do (multiple-value-bind (elt validp) (dept:iterator-next iterator)
             (if (not validp)
                 (return final-value)
                 (setq final-value (funcall function final-value elt))))))

#|

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

|#
