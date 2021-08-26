(defpackage #:sylva
  (:documentation "Library for various types of trees with a unified interface
in Common Lisp.")
  (:use #:cl)
  (:nicknames #:syl)
  (:export

   ;; general-tree.lisp
   #:sexp->tree
   #:tree->sexp
   #:equal?
   #:leaf?
   #:preorder-traversal
   #:postorder-traversal
   #:inorder-traversal

   ))
