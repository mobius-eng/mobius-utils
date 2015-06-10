;;; Expand PLIST structure
;;; MAPPLIST : MAP over PLIST, treating Key and Value as one entity
;;; MAPPL : Simialar to MAPC - map for side effects
;;; PLIST-KEYS : extracts keys of the plist
;;; PLIST-VALUES : extracts values of the plist
;;; KEYS-ARE-SYMBOLS : check if every even entry is a symbol
;;; PLISTP : checks if the object is a plist (VERY SLOW!)
;;; EVERY-VALUE : similar to EVERY, but only checks values, skipping
;;;               keys
;;; PLIST-LENGTH : length of plist
(in-package mobius.plist)

(defun mapplist (fun plist &rest plists)
  "Specialized version of map: act like a mapcar, but
consumes a property list (plist) and produces a plist of
the same shape (i.e. with the same keys). fun must accept a key,
value for the key of the plist and more values if mapplist used with
optional plists argument. If a particular key from plist cannot be found
some of plists NIL will be passed to fun in place of that argument"
  (declare (type function fun))
  (let ((result nil))
   (loop for k in plist by #'cddr
      and v in (cdr plist) by #'cddr
      do (progn
           (pushnew k result)
           (pushnew (apply fun k v
                             (mapcar (lambda (plist)
                                       (getf plist k))
                                     plists))
                    result)))
   (funcall! #'reverse result)))

(defun mappl (fun plist &rest plists)
  "Traverse (for side effectes) through the plist with fun."
  (declare (type function fun))
  (loop for k in plist by #'cddr
     and v in (cdr plist) by #'cddr
     do (apply fun k v (mapcar (lambda (pl) (getf pl k)) plists)))
  plist)

(defun plist-keys (plist)
  "Returns keys from the plist"
  (loop for k in plist by #'cddr
     collect k))

(defun plist-values (plist)
  "Returns values from the plist"
  (loop for x in plist
     and i = 0 then (1+ i)
     when (oddp i)
     collect x
     end))


(defun keys-are-symbols (list)
  "Returns true if all even (starting from 0) items are symbols"
  (every #'symbolp (plist-keys list)))

(deftype plist ()
  `(and list
        (satisfies keys-are-symbols)))

(defun plistp (obj)
  "Return true if obj is a plist"
  (typep obj 'plist))

(defun every-value (pred plist &rest plists)
  (apply #'every pred (plist-values plist)
         (mapcar #'plist-values plists)))

(defun plist-length (plist)
  (mvbind (length reminder) (truncate (length plist) 2)
    (unless (zerop reminder)
      (error "PLIST-LENGTH: Odd number of elements in the property list"))
    length))
