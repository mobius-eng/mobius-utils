;;;; -*- Mode: Lisp; -*-
;;; General utilities package
;;; FN : from OnLisp - function constructor
;;;      combines a number of function in different ways
;;;      For example: (FN (LIST (COMPOSE 1+ (LAMBDA (X) (* X 2))) 1-))
;;;      will build the function that constructs the list [(2x+1), (x-1)],
;;;      where x is its argument.
;;; ABBREV : from OnLisp - macro abreviation
;;; MVBIND : short for MULTIPLE-VALUE-BIND (OnLisp)
;;; DSBIND : short for DESTRUCTURING-BIND (OnLisp)
;;; -> and --> : Clojure's threading macros
;;; IF-AREF : the same as AREF, but checks if array is nil, in which case
;;;           evaluates to default value
;;; LET1 : Simplified LET for just one variable
;;; CLOJURE-LAMBDA : transforms Clojure style anonymous functions #f(* 2%)
;;;             to LAMBDA-expression
;;; LIST-FUNCTIONS-IN-PACKAGE : lists available (including private)
;;;                             functions in package
;;; AVERAGE : calculate average of its arguments
;;; PARTIAL : partial function application
;;; *!EQUIVS* : from OnLisp -- hash-table of destructive equivalents
;;; ! FUNCALL! APPLY! DEF! : get, call, apply and define destructive
;;;                          equivalent
;;;       (funcall (! reverse) ...) <=> (funcall! #'reverse ...)
;;; COMPUTE-VECTOR : constuct vector using FUN : Index -> Value
;;; MAPVEC MAPVECI : map over vectors
;;; INTERN-KEYWORD : DEPRICATED! Use ALEXANDRA's MAKE-KEYWORD
;;; TAGGED-LIST-P  : checks if the first arg is a pair and if the first
;;;                  item is a symbol EQ to the second arg
;;; BUILD-SYMBOL : intern symbol, whose name is the concatenation of
;;;                parts. Each part can be either a symbol or a
;;;                string.
;;; FLOOP : Functional manual tail-call recursion
;;; MEMOIZE! : replaces function with its memoized version.
;;; LAMBDA-case   : matching syntax for multi-arity lambda-s
(in-package mobius.utils) 

(defmacro fn (expr)
  "Function builder macro"
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
        (build-compose (cdr expr))
        (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))
 
(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
	  (rec fns)))))

(defmacro abbrev (short long)
  "Makes SHORT (macro) to be an alias of LONG"
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev mvbind multiple-value-bind)
(abbrev dsbind destructuring-bind)


(defmacro destructuring-array (items (array &rest indices) &body body)
  "Get items from the array. Example:
  (destructuring-array (x y z) (array (1 1) (1 2) (3 4))
    ...)"
  (let ((arr (gensym "ARRAY")))
    `(let ((,arr ,array))
       (let (,@(mapcar (lambda (item index)
                         (if (atom index)
                             `(,item (aref ,arr ,index))
                             `(,item (aref ,arr ,@index))))
                      items
                      indices))
        ,@body))))

(abbrev dsarray destructuring-array)

(defmacro if-aref (array &rest inds-default)
  "As AREF, but checks if ARRAY is NIL, in which case evaluates to DEFAULT"
  (with-gensyms (v)
    (let ((inds (butlast inds-default))
          (default (car (last inds-default))))
      `(let ((,v ,array))
         (if ,v (aref ,v ,@inds) ,default)))))


;; threading macro from Clojure
(defmacro -> (arg &rest forms)
  "Threading macro analogous to Clojure's ->:
(-> arg form1 form2 ...) will put arg as the second
element in the form1, ... formn can be just the name
of the function, than the expression (formn arg) will be formed"
  (reduce #'(lambda (processed-forms new-form)
              (cond ((listp new-form)
                     (let ((op (car new-form))
                           (args (cdr new-form)))
                       `(,op ,processed-forms ,@args)))
                    ((symbolp new-form) `(,new-form ,processed-forms))
                    (t (error (format nil "Malformed expression: ~a~%Expected either a form or funcation name"
                                      new-form)))))
            forms
            :initial-value arg))

(defmacro ->> (arg &rest forms)
  "Threading macro analogoues to Clojure's ->>:
(->>arg form1 form2 ... formm) will place arg as
the last argument of form1, then the result of it as an
argument of form2, etc. formn can be either a lisp form or
a name of the function, in which case the expression
(formn arg) will be formed"
  (reduce #'(lambda (processed-forms new-form)
              (cond ((listp new-form)
                     (let ((op (car new-form))
                           (args (cdr new-form)))
                       `(,op ,@args ,processed-forms)))
                    ((symbolp new-form) `(,new-form ,processed-forms))
                    (t (error (format nil "Malformed expression: ~a~%Expected either a form or funcation name"
                                      new-form)))))
            forms
            :initial-value arg))


(defmacro let1 (binding &body body)
  "LET for just one variable (LET1 (x (foo)) (bar x))"
  `(let ((,@binding))
     ,@body))

(defun deep-substitute (new-value old-value list)
  (cond ((null list) nil)
        (t (let ((current-part (car list))
                 (more-parts (cdr list)))
             (cond ((listp current-part) (cons (deep-substitute new-value old-value current-part)
                                               (deep-substitute new-value old-value more-parts)))
                   ((equal current-part old-value) (cons new-value (deep-substitute new-value
                                                                                    old-value
                                                                                    more-parts)))
                   (t (cons current-part (deep-substitute new-value old-value more-parts))))))))

(defun clojure-lambda (stream subchar arg)
  "Constructing LAMBDA from Clojure-like anonymous function"
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t))
         (l-arg (gensym "ARG"))
         (sexp-1 (deep-substitute l-arg '% sexp)))
    `(lambda (,l-arg) ,sexp-1)))

(set-dispatch-macro-character #\# #\f #'clojure-lambda)


;;; Useful functions:

(defun list-functions-in-package (package-name)
  (loop for x being the external-symbol of package-name
     when (fboundp x) collect x))



(defun average (x &rest xs)
  (let ((n (length xs)))
    (/ (apply #'+ x xs) (1+ n))))

(defun partial (fun &rest args)
  (declare (type function fun))
  (lambda (&rest more-args)
    (apply fun (append args more-args))))

;;; Destructive equivalent functions
(defvar *!equivs* (make-hash-table)
  "Hash table of functions and their destructive equivalents")

(defmacro ! (fn)
  "Returns a distructive equivalent of fn"
  (let ((args (gensym "ARGS")))
   `(lambda (&rest ,args)
      (apply (or (gethash (function ,fn) *!equivs*) (function ,fn)) ,args))))

(defmacro funcall! (fn &rest args)
  `(funcall (or (gethash ,fn *!equivs*) ,fn) ,@args))

(defmacro apply! (fn arg &rest args)
  `(apply (or (gethash ,fn *!equivs*) ,fn) ,arg ,@args))

(defmacro def! (fn fn!)
  "Define fn! as a destructive equivalent of fn. fn! can be accessed
using (! fn)"
  `(setf (gethash (function ,fn) *!equivs*) (function ,fn!)))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
  (def! reverse nreverse)
  (def! append nconc)
  (def! remove delete)
  (def! remove-if delete-if)
  (def! remove-if-not delete-if-not)
  (def! remove-duplicates delete-duplicates)
  (def! substitute nsubstitute)
  (def! substitute-if nsubstitute-if)
  (def! substitute-if-not nsubstitute-if-not)
  (def! revappend nreconc)
;; )

;;; Vector functions:
(defun compute-vector (dim fun)
  (let ((v (make-array dim)))
    (loop for i from 0 below dim
       do (setf (aref v i) (funcall fun i)))
    v))

(defun mapvec (fun vec &rest other-vectors)
  "Map fun over vector vec"
  (declare (type function fun)
           (type (vector * *) vec))
  (compute-vector (length vec)
                  (lambda (i)
                    (apply fun (aref vec i) (mapcar (lambda (w)
                                                      (aref w i))
                                                    other-vectors)))))

(defun mapveci (fun vec &rest other-vectors)
  "Map fun over vec. fun: index v ... -> x"
  (declare (type function fun)
           (type (vector * *) vec))
  (compute-vector (length vec)
                  (lambda (i)
                    (apply fun i (aref vec i)
                           (mapcar (lambda (w)
                                     (aref w i))
                                    other-vectors)))))

(defun intern-keyword (symb)
  "DEPRICATED. Use MAKE-KEYWORD from ALEXANDRIA"
  (intern (symbol-name symb) 'keyword))

(defun tagged-list-p (exp tag)
  "Trivial type-tag checker"
  (and (consp exp) (eq (car exp) tag)))

(defun build-symbol (&rest parts)
  "Intern symbol whose name is a concatenation of parts"
  (let ((string-parts (mapcar (lambda (x)
                                (cond ((stringp x) (string-upcase x))
                                      ((symbolp x) (symbol-name x))
                                      (t (format nil "~A" x))))
                              parts)))
    (intern (apply #'concatenate 'string string-parts))))

(defmacro floop (name bindings &body body)
  "f(unctional)loop for tail-calls. Use NAME to go to next cycle.
  Example:
  (floop factorial ((i n) (fact 1))
    (if (zerop i)
        fact
        (factorial (1- i) (* i fact))))"
  (let* ((gs (loop for i in bindings collect (gensym)))
         (gargs (loop for i in bindings collect (gensym)))
         (gblock (gensym))
         (gtag (gensym))
         (init-bindings (loop for s in gs
                           for val in (mapcar #'cadr bindings)
                           collect (list s val)))
         (recur-bindings (loop for var in (mapcar #'car bindings)
                            for val in gs
                            collect (list var val))))
    `(macrolet
         ((,name ,gargs
            `(progn
               (psetq ,@(apply #'nconc
                               (mapcar #'list 
                                       ',gs
                                       (list ,@gargs))))
               (go ,',gtag))))
       (block ,gblock
         (let ,init-bindings
           (tagbody ,gtag
              (let ,recur-bindings
                (return-from ,gblock (progn ,@body)))))))))

(defmacro memoize! (fun)
  "Replaces FUN with memoized version of it"
  (let ((examine-hash (intern (format nil "~a-HASH" (symbol-name fun)))))
    (with-gensyms (goldfun gprev gargs)
      `(let ((,goldfun (symbol-function ',fun))
             (,gprev (make-hash-table :test 'equalp)))
         (defun ,examine-hash () ,gprev)
         (defun ,fun (&rest ,gargs)
           (or (gethash ,gargs ,gprev)
               (setf (gethash ,gargs ,gprev) (apply ,goldfun ,gargs))))))))

(defmacro lambda-case (&body clauses)
  (let ((args (gensym)))
    `(lambda (&rest ,args)
       (match ,args ,@clauses))))

(defun hash-table->alist (h)
  (let ((keys (hash-table-keys h)))
    (mapcar (lambda (key) (cons key (gethash key h))) keys)))

(defun hash-table-eq-of (&rest key-values)
  (mvbind (keys values) (loop for kv on key-values by #'cddr
                           collect (car kv) into keys
                           collect (cadr kv) into values
                           finally (return (values keys values)))
          (let1 (h (make-hash-table :test 'eq))
            (mapc (lambda (k v) (setf (gethash k h) v)) keys values)
            h)))

(defun alist->hash-table (alist &key (test 'eq))
  (let1 (h (make-hash-table :test test))
    (mapc (lambda (pair) (setf (gethash (car pair) h) (cdr pair))) alist)
    h))

(defun hash-table-change (h key new-key new-value)
  (remhash key h)
  (setf (gethash new-key h) new-value))


(defmacro with-vector-items (items vector &body body)
  "Another approach to destructure vectors"
  (let ((gv (gensym "VECTOR")))
    (let ((bindings (mapcar (lambda (var-index)
                              (destructuring-bind (var index) var-index
                                `(,var (aref ,gv ,index))))
                            items)))
      `(let ((,gv ,vector))
         (symbol-macrolet (,@bindings)
           ,@body)))))
