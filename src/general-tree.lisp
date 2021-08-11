(in-package #:sylva)

(defstruct general-tree
  "General tree structure representing a tree with no constraints on the order
of its nodes, nor the number of children its nodes have."
  data
  (children nil :type list))

(defun sexp->tree (sexp)
  "Convert a sexp into a general tree."
  (unless (null sexp)
    (let ((data nil)
          (children nil))
      (typecase sexp
        (atom (setq data sexp))
        (list (setq data (first sexp)
                    children (map 'list #'sexp->tree (rest sexp)))))
      (make-general-tree :data data
                         :children children))))

(defun tree->sexp (tree)
  "Convert a general tree into a sexp."
  (cons (general-tree-data tree)
        (mapcar #'tree->sexp (general-tree-children tree))))
