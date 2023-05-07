(in-package #:dependently-typed-cl.impl)

(deftype dept:vector (&optional (element-type 'cl:*) (size 'cl:*))
  `(cl:vector ,element-type ,size))

(def-dept-fun dept:make-array (dimensions
                               &rest args
                               &key (element-type t element-type-p)
                               initial-element
                               initial-contents
                               (adjustable nil adjustablep)
                               (fill-pointer nil fill-pointer-p)
                               (displaced-to nil displaced-to-p)
                               displaced-index-offset)
    (let ((dim-value (constant-type-value dimensions 'cl:*))
          (elt-value (if element-type-p
                         (constant-type-value element-type 'cl:*)
                         t)))
      (if (every #'null (list adjustablep fill-pointer-p displaced-to))
          `(values (simple-array ,elt-value ,dim-value) &optional)
          `(values (array ,elt-value ,dim-value) &optional)))
  (declare (ignorable element-type element-type-p initial-element initial-contents
                      adjustable adjustablep
                      fill-pointer fill-pointer-p
                      displaced-to displaced-to-p
                      displaced-index-offset))
  (apply #'make-array dimensions args))

(def-dept-fun dept:vector (&rest objects)
    `(values (simple-vector ,(length objects)) &optional)
  (apply #'vector objects))

(def-dept-fun dept:row-major-aref (array index)
    (cond ((constant-type-value array nil)
           `(values ,(array-element-type (constant-type-value array nil)) &optional))
          ((subtypep array 'array)
           (optima:ematch (typexpand array)
             ((list* 'specializing 'array element-type _)
              `(values ,element-type &optional))))
          ((subtypep 'array array)
           `(values t &optional))
          (t
           `(values nil &optional)))
  (row-major-aref array index))

(def-dept-fun dept:aref (array &rest indices)
    (cond ((constant-type-value array nil)
           `(values ,(array-element-type (constant-type-value array nil)) &optional))
          ((subtypep array 'array)
           (optima:ematch (typexpand array)
             ((list* 'specializing 'array element-type _)
              `(values ,element-type &optional))))
          ((subtypep 'array array)
           `(values t &optional))
          (t
           `(values nil &optional)))
  (apply #'aref array indices))
