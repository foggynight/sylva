(in-package #:sylva)

(defstruct general-tree
  "General tree structure representing a tree with no constraints on the order
of its nodes, nor the number of children its nodes have."
  data
  (children nil :type list))

(defun sexp->tree (sexp)
  "Convert a sexp into a general tree."
  (if (consp sexp)
      (make-general-tree :data (first sexp)
                         :children (mapcar #'sexp->tree (rest sexp)))
      sexp))

(defun tree->sexp (tree)
  "Convert a general tree into a sexp."
  (if (general-tree-p tree)
      (cons (general-tree-data tree)
            (mapcar #'tree->sexp (general-tree-children tree)))
      tree))
