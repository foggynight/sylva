(in-package #:sylva)

(defstruct (binary-tree (:include general-tree))
  "BINARY-TREE struct representing a binary tree. That is, a tree with no
constraints on the order of its nodes, but where each of its nodes have at most
two children.")

(defmethod %sexp->tree (sexp (tree-type (eql :binary-tree)))
  (%%sexp->tree sexp #'make-binary-tree))

(defun left (binary-tree)
  "Get the left sub-tree of BINARY-TREE."
  (first (binary-tree-children binary-tree)))

(defun right (binary-tree)
  "Get the right sub-tree of BINARY-TREE."
  (second (binary-tree-children binary-tree)))

(defmethod insert ((data binary-tree) (tree binary-tree))
  "Insert DATA into TREE by performing a breadth-first search on TREE and adding
DATA to the list of children of the first node encountered which has less than
two children."
  (do ((queue (list tree)
              (reduce #'append
                      (remove-if #'null
                                 (mapcar #'binary-tree-children
                                         queue))))
       (found nil
              (dolist (node queue)
                (when (< (child-count node) 2)
                  (add-child data node)
                  (return t)))))
      (found)))

(defmethod insert (data (tree binary-tree))
  "Create a new BINARY-TREE with DATA as the value of its DATA slot, and insert
it into TREE using the INSERT method."
  (insert (make-binary-tree :data data) tree))

(defun sorted? (binary-tree &key (test #'<=))
  "Is BINARY-TREE sorted?

BINARY-TREE is deemed to be sorted if any of the following conditions are met:
- BINARY-TREE is not a struct of type BINARY-TREE
- BINARY-TREE is a leaf node, that is, it has no children
- The list resulting from an inorder traversal of BINARY-TREE is sorted

The test used to determine if the list resulting from an inorder traversal of
BINARY-TREE is sorted may be specified using the TEST key parameter."
  (flet ((sorted-list? (list)
           (every test list (rest list))))
    (or (not (binary-tree-p binary-tree))
        (leaf? binary-tree)
        (sorted-list? (traverse binary-tree :order :inorder)))))
