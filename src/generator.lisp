;;; GENERATOR is similar to LAZYSEQ, but it
;;; does not keep the whole sequence of values,
;;; instead it produces values one by one
;;; This is a persisten implementation of GENERATOR
;;;
;;; GENERATOR : representation
;;; GENERATOR-SEED : current value of generator
;;; ITERATION-GENERATOR : generator producing x, f(x), f(f(x)),...
;;; GENERATOR-FINISHED?
;;; NEXT : fetches next value from generator.
;;;
;;; GENERATOR implements FOLD, thus can be used with transducers 
(in-package mobius.generator)

(defstruct generator seed producer finished-p)

(defmethod print-object ((obj generator) out)
  (print-unreadable-object (obj out :type t :identity nil)
    (format out "SEED ~A"
            (generator-seed obj))))

(defun update-generator (gen new-seed)
  (make-generator :seed new-seed
                  :producer (generator-producer gen)
                  :finished-p (generator-finished-p gen)))

(defun iteration-generator (x0 fun finished?)
  "Produces a generator of x0, f(x0), f(f(x0))... FINISHED? is a function of x
indicating when the sequence reached the fixed point"
  (make-generator :seed x0
                  :producer (lambda (gen)
                              (let ((x (funcall fun (generator-seed gen))))
                                (update-generator gen x)))
                  :finished-p finished?))

(defun generator-finished? (gen)
  "Tests if the generator is finished and no more items can be produced"
  (let ((finished? (generator-finished-p gen))
        (x (generator-seed gen)))
    (funcall finished? x)))

(defun next (gen)
  "Produce new value from generator."
  (if (generator-finished? gen)
      (progn
        (warn "generator is finished: ~A. Using last produce" gen)
        gen)
      (funcall (generator-producer gen) gen)))

;;; implement FOLD, so can use with transducers
(defmethod fold (fun init (data generator))
  (floop fold-loop ((result init) (g data))
    (cond ((folded? result) (folded-value result))
          ((generator-finished? g) result)
          (t (let1 (new-g (next g))
               (fold-loop (funcall fun result new-g) new-g))))))
