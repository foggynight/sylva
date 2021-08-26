(in-package #:sylva)

(defstruct general-tree
  "General tree structure representing a tree with no constraints on the order
of its nodes, nor the number of children its nodes have."
  data
  (children nil :type list))

(defun %%sexp->tree (sexp constructor)
  (if (atom sexp)
      sexp
      (funcall constructor
               :data (first sexp)
               :children (mapcar (lambda (e)
                                   (%%sexp->tree e constructor))
                                 (rest sexp)))))

(defmethod %sexp->tree (sexp (tree-type (eql 'general-tree)))
  (%%sexp->tree sexp #'make-general-tree))

(defmacro sexp->tree (sexp &optional (tree-type 'general-tree))
  "Convert SEXP into a tree.

Optionally, the type of tree may be specified by passing a symbol to the
TREE-TYPE parameter."
  `(%sexp->tree ,sexp ',tree-type))

(defun tree->sexp (tree)
  "Convert a general tree into a sexp."
  (if (general-tree-p tree)
      (cons (general-tree-data tree)
            (mapcar #'tree->sexp (general-tree-children tree)))
      tree))

(defun leaf? (tree)
  "Is a general tree a leaf?"
  (endp (general-tree-children tree)))

(defun equal? (tree-0 tree-1)
  "Are two general trees equal?"
  (tree-equal (tree->sexp tree-0)
              (tree->sexp tree-1)))

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
