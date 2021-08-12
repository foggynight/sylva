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

(defun preorder-traversal (tree)
  "Perform a preorder traversal of a general tree and collect the data at each
visited node in a list."
  (if (general-tree-p tree)
      (cons (general-tree-data tree)
            (reduce #'append
                    (mapcar #'preorder-traversal
                            (general-tree-children tree))))
      (list tree)))

(defun postorder-traversal (tree)
  "Perform a postorder traversal of a general tree and collect the data at each
visited node in a list."
  (if (general-tree-p tree)
      (append (reduce #'append
                      (mapcar #'postorder-traversal
                              (general-tree-children tree)))
              (list (general-tree-data tree)))
      (list tree)))

(defun inorder-traversal (tree)
  "Perform an inorder traversal of a general tree and collect the data at each
visited node in a list."
  (if (general-tree-p tree)
      (let* ((children (general-tree-children tree))
             (center-index (ceiling (length children) 2)))
        (append (reduce #'append
                        (mapcar #'inorder-traversal
                                (subseq children 0 center-index)))
                (list (general-tree-data tree))
                (reduce #'append
                        (mapcar #'inorder-traversal
                                (subseq children center-index)))))
      (list tree)))
