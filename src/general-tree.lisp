(in-package #:sylva)

(defstruct general-tree
  "GENERAL-TREE struct representing a tree with no constraints on the order of
its nodes, nor the number of children its nodes have."
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

(defmethod %sexp->tree (sexp (tree-type (eql :general-tree)))
  (%%sexp->tree sexp #'make-general-tree))

(defun sexp->tree (sexp &optional (tree-type :general-tree))
  "Convert SEXP into a tree.

The type of tree to convert SEXP into may be specified by passing a symbol
naming the tree type to the TREE-TYPE parameter."
  (%sexp->tree sexp tree-type))

(defun tree->sexp (tree)
  "Convert TREE into a sexp."
  (if (general-tree-p tree)
      (cons (general-tree-data tree)
            (mapcar #'tree->sexp (general-tree-children tree)))
      tree))

(defun equal? (tree-0 tree-1 &key test)
  "Are two trees equal?

The test function may be specified by passing a function to the TEST key
parameter, the default test is the default test of TREE-EQUAL, which is EQL."
  (tree-equal (tree->sexp tree-0)
              (tree->sexp tree-1)
              :test test))

(defun size (tree)
  "Get the number of nodes in TREE."
  (if (general-tree-p tree)
      (1+ (reduce #'+ (mapcar #'size (general-tree-children tree))))
      (if (null tree) 0 1)))

(defun empty? (tree)
  "Is TREE empty?"
  (zerop (size tree)))

(defun leaf? (tree)
  "Is TREE a leaf node?"
  (endp (general-tree-children tree)))

(defun depth (tree root &optional (start 0))
  "Get the depth of TREE relative to ROOT, where depth is defined as the number
of branches between a node and the root of its containing tree."
  (if (eql tree root)
      start
      (if (and (general-tree-p root)
               (not (leaf? root)))
          (apply #'max (mapcar (lambda (e) (depth tree e (1+ start)))
                               (general-tree-children root)))
          -1)))

(defun height (tree &optional (start 0))
  "Get the height of TREE, where height is defined as the depth of the deepest
node in a tree."
  (if (and (general-tree-p tree)
           (not (leaf? tree)))
      (apply #'max (mapcar (lambda (e) (height e (1+ start)))
                           (general-tree-children tree)))
      start))

(defmethod %traverse (tree (order (eql :preorder)) function)
  (if (general-tree-p tree)
      (cons (if function
                (funcall function (general-tree-data tree))
                (general-tree-data tree))
            (reduce #'append
                    (mapcar (lambda (e) (%traverse e :preorder function))
                            (general-tree-children tree))))
      (list (if function
                (funcall function tree)
                tree))))

(defmethod %traverse (tree (order (eql :postorder)) function)
  (if (general-tree-p tree)
      (append (reduce #'append
                      (mapcar (lambda (e) (%traverse e :postorder function))
                              (general-tree-children tree)))
              (list (if function
                        (funcall function (general-tree-data tree))
                        (general-tree-data tree))))
      (list (if function
                (funcall function tree)
                tree))))

(defmethod %traverse (tree (order (eql :inorder)) function)
  (if (general-tree-p tree)
      (let* ((children (general-tree-children tree))
             (center-index (ceiling (length children) 2)))
        (append (reduce #'append
                        (mapcar (lambda (e) (%traverse e :inorder function))
                                (subseq children 0 center-index)))
                (list (if function
                          (funcall function (general-tree-data tree))
                          (general-tree-data tree)))
                (reduce #'append
                        (mapcar (lambda (e) (%traverse e :inorder function))
                                (subseq children center-index)))))
      (list (if function
                (funcall function tree)
                tree))))

(defun traverse (tree &key (order :preorder) function)
  "Traverse a tree and collect the data at each visited node in a list.

The order of the traversal may be specified by passing a symbol naming the
traversal order to the ORDER key parameter.

A function may be passed to the FUNCTION key parameter, it will be called on
each of the nodes in the tree by passing the data point of the node as the only
argument to FUNCTION, its result taking the place of the node's data point in
the list returned from this function."
  (%traverse tree order function))
