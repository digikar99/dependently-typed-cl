(defsystem "dependently-typed-cl"
  :pathname #P"cl/"
  :depends-on ("dependently-typed-functions"
               "typo"
               "polymorphic-functions"
               "extensible-compound-types"
               "extensible-compound-types-cl"
               "extensible-compound-types-interfaces")
  :components ((:file "package")
               (:file "utils")
               (:file "immutable-let")
               (:file "simple-math")
               (:file "lisp-lists")
               (:file "array")
               (:file "iterator")))
