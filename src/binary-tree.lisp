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
