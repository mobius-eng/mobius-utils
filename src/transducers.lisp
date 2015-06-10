;;; Clojure's transducers
;;; FOLDED : class representing a folded value
;;; FOLD : Generic FOLD over collections.
;;; TRANSDUCE : transduce and FOLD the items of the collection
;;; LAST-ITEM : selects the last item of the collection (as determined
;;;             by FOLD for this collection)
;;; FMAP : MAP transducer (Haskell's functor-map)
;;; TAKE : takes N items from the collection (transducer)
;;; DROP : drops N items from the collection (transducer)
;;; TAKE-WHILE DROP-WHILE : take (drop) while predicate is true
;;; FILTER : generalized REMOVE-IF-NOT
;;; CONJ : generic addition of an element into collection
;;; INTO : adds items to the collection (uses CONJ)
;;;
;;; This package includes transducers implementation for lists
(in-package mobius.transducers)

(defclass folded ()
  ((value :initarg :value :initform (error "FOLDED: must provide value") :reader folded-value))
  (:documentation "Indicates if the value has been already folded causing FOLD return the value immidiately"))


(defun folded? (obj)
  (typep obj 'folded))

(defun folded (value)
  (make-instance 'folded :value value))

(defmethod print-object ((obj folded) out)
  (print-unreadable-object (obj out :type t :identity nil)
    (format out "~a" (folded-value obj))))

(defgeneric fold (fun init data)
  (:documentation "Generic FOLD over collections"))

(defun transduce (xform fun data)
  (let ((f (funcall xform fun)))
    (fold f (funcall fun) data)))

(defun last-item (xform data &optional default)
  (transduce xform
             (lambda-case
               ((list) default)
               ((list result) result)
               ((list result input) (declare (ignore result)) input))
             data))

(defmacro basic-transducer (name args env-bindings &body main-body)
  "Anaphoric macro to create a basic transducer. The following symbols
  a bound in the body: F is folding function, RESULT is intermediate result and
  INPUT is a new collection input"
  (let ((internal `(lambda (f)
                     (lambda-case
                       ((list) (funcall f))
                       ((list result) (funcall f result))
                       ((list result input) ,@main-body)))))
    (if env-bindings
        `(defun ,name ,args
           (let (,@env-bindings)
             ,internal))
        `(defun ,name ,args
           ,internal))))


(defun fmap (fun)
  "F(unctor) MAP"
  (lambda (f)
    (lambda-case
      ((list ) (funcall f))
      ((list result) (funcall f result))
      ((list result input)
       (funcall f
                result
                (funcall fun input))))))

(defun take (n)
  "Take n items from a collection"
  (let ((taken n))
    (lambda (f)
      (lambda-case
        ((list) (funcall f))
        ((list result) (funcall f result))
        ((list result input)
         (let ((result (if (plusp taken)
                           (funcall f result input)
                           result)))
           (decf taken)
           (if (not (plusp taken))
               (folded result)
               result)))))))

(defun drop (n)
  "Drop first n items from the collection"
  (let ((dropped n))
    (lambda (f)
      (lambda-case
        ((list) (funcall f))
        ((list result) (funcall f result))
        ((list result input)
         (cond ((not (plusp dropped)) (funcall f result input))
               (t (decf dropped) result)))))))

(defun filter (pred)
  "Choose only items that satisfy predicate PRED"
  (lambda (f)
    (lambda-case
      ((list) (funcall f))
      ((list result) (funcall f result))
      ((list result input)
       (if (funcall pred input)
           (funcall f result input)
           result)))))

(defun take-while (pred)
  (lambda (f)
    (lambda-case
      ((list) (funcall f))
      ((list result) (funcall f result))
      ((list result input)
       (if (funcall pred input)
           (funcall f result input)
           (folded result))))))

(defun drop-while (pred)
  (lambda (f)
    (lambda-case
      ((list) (funcall f))
      ((list result) (funcall f result))
      ((list result input)
       (if (funcall pred input)
           result
           (funcall f result input))))))

(defgeneric conj (data val)
  (:documentation "CONJugate value to a data collection"))

(defun into (recipient xform data)
  "Collect transduced (through xform) data into recipient"
  (transduce xform
             (lambda-case
               ((list) recipient)
               ((list x) x)
               ((list x y) (conj x y)))
             data))

;;; Some implementations:

;; Cannot use REDUCE because of FOLDED case
(defmethod fold (fun init (data list))
  (floop fold-loop ((l data) (result init))
    (cond ((null l) result)
          ((folded? result) (folded-value result))
          (t (fold-loop (cdr l) (funcall fun result (car l)))))))

(defmethod conj ((data list) val)
  (cons val data))


;;; Some other transducers

(basic-transducer partition (len step) ((chunk nil) (p 0))
  (incf p)
  (when (plusp p)
    (setf chunk (funcall! #'append chunk (list input))))
  (cond ((= p len)
         (let1 (output (copy-seq chunk))
           (setf chunk (drop-first step chunk))
           (setf p (- p step))
           (funcall f result output)))
        (t (funcall f result))))
