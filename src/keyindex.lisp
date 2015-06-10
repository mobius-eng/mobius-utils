(in-package cl-user)

(defpackage #:keyindex
  (:use #:cl
        #:alexandria
        #:mobius.utils)
  (:export #:make-indexed-keys
           #:key-index
           #:index-key))

(in-package keyindex)

(defclass indexed-keys ()
  ((keys :initarg :keys :reader ik-keys)
   (indices :initarg :indices :reader ik-indices)))

(defun make-indexed-keys (keys)
  (let* ((length (length keys))
         (keys-with-indices (alist->hash-table
                             (mapcar #'(lambda (k n) (cons k n)) keys (iota length))
                             :test 'eq))
         (keys-array (make-array length :initial-contents keys)))
    (make-instance 'indexed-keys :indices keys-with-indices :keys keys-array)))

(defmethod print-object ((obj indexed-keys) out)
  (print-unreadable-object (obj out :type t :identity nil)
    (format out "~A" (hash-table->alist (ik-indices obj)))))

(defun key-index (indexed-keys key)
  (gethash key (ik-indices indexed-keys) -1))

(defun index-key (indexed-keys index)
  (aref (ik-keys indexed-keys) index))
