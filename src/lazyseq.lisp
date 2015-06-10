;;; Effectively, STREAMs
;;; but CL reserves STREAM to use with I/O
;;; EMPTY-LAZYSEQ : empty lazy sequence
;;; LAZYSEQ-EMPTY?
;;; LAZYSEQ-CONS, LAZYSEQ-CAR, LAZYSEQ-CDR : constructor and selectors
;;; LIST->LAZYSEQ, LAZYSEQ->LIST : converters from/to list
;;; LAZYSEQ : easy constructor
;;; LAZYSEQ-ITERATE : lazyli construct x, f(x), f(f(x)),...
(in-package mobius.lazyseq)

(defvar *uncalc* (gensym))

(defstruct delay thunk (cache *uncalc*))

(defmacro delay (&body expressions)
  `(make-delay :thunk (lambda () ,@expressions)))

(defun force (obj)
  (if (delay-p obj)
      (if (eq (delay-cache obj) *uncalc*)
          (setf (delay-cache obj) (funcall (delay-thunk obj)))
          (delay-cache obj))
      obj))

(defstruct (lazyseq (:constructor make-lazyseq (head tail))) head tail)

(defvar *empty-lazyseq-indicator* (gensym))

(defvar empty-lazyseq (make-lazyseq  *empty-lazyseq-indicator*
                                     (delay *empty-lazyseq-indicator*)))
(declaim (inline lazyseq?))
(defun lazyseq? (obj) (lazyseq-p obj))

(defun lazyseq-empty? (seq)
  (eq seq empty-lazyseq))

(defmethod print-object ((obj lazyseq) out)
  (print-unreadable-object (obj out :type t)
    (if (lazyseq-empty? obj)
        (format out "EMPTY")
        (format out "~a TAIL" (lazyseq-head obj)))))

(defmacro lazyseq-cons (x y)
  `(make-lazyseq ,x (delay ,y)))

(defun lazyseq-car (lazyseq)
  ; (assert (not (eq lazyseq empty-lazyseq)))
  (lazyseq-head lazyseq))

(defun lazyseq-cdr (lazyseq)
  ; (assert (not (eq lazyseq empty-lazyseq)))
  (force (lazyseq-tail lazyseq)))

(defun list->lazyseq (list)
  (cond ((null list) empty-lazyseq)
        (t (lazyseq-cons (car list) (list->lazyseq (cdr list))))))

(defun lazyseq->list (lazyseq)
  (cond ((lazyseq-empty? lazyseq) nil)
        (t (cons (lazyseq-car lazyseq) (lazyseq->list (lazyseq-cdr lazyseq))))))

(defun lazyseq (&rest args) (list->lazyseq args))

(defun lazyseq-iterate (fun val)
  (lazyseq-cons val (lazyseq-iterate fun (funcall fun val))))

(defmethod fold (fun init (data lazyseq))
  (floop fold-loop ((s data) (result init))
    (cond ((lazyseq-empty? s) result)
          ((folded? result) (folded-value result))
          (t (fold-loop (lazyseq-cdr s) (funcall fun result (lazyseq-car s)))))))

(defmethod conj ((data lazyseq) val)
  (lazyseq-cons val data))
