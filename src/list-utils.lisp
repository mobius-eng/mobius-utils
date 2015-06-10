;;; Extra list utilities
;;; SPLIT-TO-CHUNKS : split list to parts of specified length and with
;;;                   spesified step
;;; INTERLEAVE : interleaves two or more lists
;;; FREQUENCIES : computes alist of each item frequences in original
;;;               list
;;; LAST1 : depricated
;;; SINGLE : OnLisp - Checks if the argument is a list of one item
;;; APPEND1 : OnLisp - appends obj to a list
;;; CONC1 : OnLisp - destractive version of APPEND1
;;; MKLIST : OnLisp -  if argument is not a list, makes singleton list
;;; LONGER : OnLisp - compares lengths of two lists
;;; FILTER : OnLisp - combination of REMOVE-IF-NOT and MAPCAR
;;; PRUNE : OnLisp - prune the tree?
;;; FIND2 : Finds the first entry of item in list for which the
;;;         application of the function provided as an argument is not
;;;         NIL. Returns both item and the result of function
;;;         application.
;;; BEFORE : checks if one item appears before another in the list
;;; AFTER : checks if one item appears after another in the list
;;; DUPLICATE : checks if an object appears in the list at least twice
;;; SPLIT-LIST-IF : splits the list at the first value for which the
;;;                 function passed as the first argument returns NIL
;;; MAP-> MAPA-B : OnLisp - variations on MAP
;;; FIXED-POINT-SERIES : Produces infinite series of x, f(x), f(f(x)),...
(in-package mobius.list-utils)

;;; Use series
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;  (series::install))

(defun split-to-chunks (list part-length &optional (step part-length))
  "Clojure's partition (not to be confused with depricated PARTITION from
CL-UTILS): partitions the list into sublists of length part-length and
with step. If step is not provided, partitions do not overlap"
  (declare (type fixnum part-length step))
  (loop
    for count = 0 then (if (>= count (1- step)) 0 (1+ count))
    for cdrs on list
    when (zerop count)
    collecting (loop
                 for i from 0 below part-length
                 for el in cdrs
                 collect el into x
                 if (= i (1- part-length))
                 do (return x)
                 end
                 finally (return nil)) into chunks
    end
    finally (return (funcall! #'remove-if #'null chunks))))


(defun interleave (list1 list2 &rest more-lists)
  "Interleave two or more lists. Will stop as soon as on of the lists
is exhausted"
  (if (null more-lists)
      (mapcan #'(lambda (x y) (list x y)) list1 list2)
      (apply #'mapcan
             #'(lambda (x y &rest others) (cons x (cons y others)))
             list1 list2 more-lists)))

(defun frequencies (list &key (test #'eql) (key #'identity))
  "Computes the alist of frequencies of each item in the original list"
  (floop freq-loop ((l list) (freq-list nil))
    (if (null l)
        (funcall! #'reverse freq-list)
        (let ((exist-pair (assoc (car l) freq-list :key key :test test)))
          (if exist-pair
              (progn
                (incf (cdr exist-pair))
                (freq-loop (cdr l) freq-list))
              (freq-loop (cdr l) (cons (cons (car l) 1) freq-list)))))))

;; (let (freq-list)
;;     (loop for i in list
;;        do (if-let (exist-pair (assoc i freq-list))
;;             (incf (cdr exist-pair))
;;             (pushnew (cons i 1) freq-list)))
;;     (funcall! #'reverse freq-list))

;;; From On Lisp
(declaim (inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "DEPRICATED: use LASTCAR from ALEXANDRIA"
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (funcall! #'append lst (list obj)))

(def! append1 conc1)

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
			     (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "More efficient analog of
  (mapcar fn (remove-if-not fn lst))"
  (declare (type function fn))
  (let ((acc nil))
    (dolist (x lst)
      (when-let (val (funcall fn x))
        (push val acc)))
    (funcall! #'reverse acc)))

(defun prune (test tree)
  (declare (type function test))
  (labels ((rec (tree acc)
		(cond ((null tree) (funcall! #'reverse acc))
		      ((consp (car tree))
		       (rec (cdr tree)
			    (cons (rec (car tree) nil) acc)))
		      (t (rec (cdr tree)
			      (if (funcall test (car tree))
				  acc
				(cons (car tree) acc)))))))
    (rec tree nil)))


(defun find2 (fn lst)
  "Find the first entry of lst for which FN is not NIL.
Returns both item and (FN item)"
  (declare (type function fn))
  (if (null lst)
      nil
      (if-let (val (funcall fn (car lst)))
        (values (car lst) val)
        (find2 fn (cdr lst)))))

(defun before (x y lst &key (test #'eql))
  "Checks if x appers before y in lst"
  (declare (type function test))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Checks if x appers after y in lst"
  (declare (type function test))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (declare (type function test))
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-list-if (fn lst)
  (declare (type function fn))
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (funcall! #'reverse acc) src))
      (push (car src) acc))))

(defun map-> (fn start test-fn succ-fn)
  (declare (type function fn test-fn succ-fn))
  (floop map-loop ((i start) (result nil))
      (cond ((funcall test-fn i) (funcall! #'reverse result))
            (t (map-loop (funcall succ-fn i)
                         (cons (funcall fn i) result))))))

;; (do ((i start (funcall succ-fn i))
;;        (result nil))
;;       ((funcall test-fn i) (nreverse result))
;;     (push (funcall fn i) result))


(defun mapa-b (fun a b &optional (step 1))
  "DEPRICATED: use MAP-IOTA from ALEXANDRIA"
  (declare (type function fun))
  (loop for i from a to b by step
       collect (funcall fun i)))

;;; This is the only one that uses SERIES
;; (defun fixed-point-series (init-value fun)
;;   "Produces infinite series of x, f(x), f(f(x)), ..."
;;   (declare (optimizable-series-function))
;;   (scan-fn t (lambda () init-value) (lambda (x) (funcall fun x))))


(defun take-first (n list)
  (loop for x in list
     for p from 1
     collect x into xs
     when (= p n)
     return xs
     finally (return xs)))

(defun drop-last (n list list-length)
  (let1 (m (- list-length n))
    (if (plusp m)
        (take-first (- list-length n) list)
        nil)))

(defun drop-first (n list)
  (floop drop-first-loop ((k 0) (l list))
    (cond ((null l) nil)
          ((= k n) l)
          (t (drop-first-loop (1+ k) (cdr l))))))

