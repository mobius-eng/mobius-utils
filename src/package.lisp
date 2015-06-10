(in-package cl-user)

(defpackage :mobius.utils
  (:use #:cl
        #:alexandria
        #:optima
        #:optima.core
        #:optima.extra)
  (:export :fn
           :abbrev
           :mvbind
           :dsbind
           :let1
           :if-aref
           :destructuring-array
           :dsarray
           :->
           :->>
           :%
           :list-functions-in-package
           :average
           :partial
           :*!equivs*
           :!
           :funcall!
           :apply!
           :def!
           :compute-vector
           :mapvec
           :mapveci           
           :intern-keyword
           :build-symbol
           :tagged-list-p
           :floop
           :memoize!
           :lambda-case
           :hash-table->alist
           :hash-table-eq-of
           :alist->hash-table
           :hash-table-change))

(defpackage :mobius.list-utils
  (:use #:cl
        #:alexandria
        #:mobius.utils)
  (:export #:split-to-chunks
           #:interleave
           #:frequencies
           #:last1
           #:single
           #:append1
           #:mklist
           #:longer
           #:filter
           #:prune
           #:find2
           #:before
           #:after
           #:split-list-if
           #:map->
           #:mapa-b
           #:fixed-point-series
           #:take-fisrt
           #:drop-last
           #:drop-first))

(defpackage :mobius.plist
  (:use #:cl
        #:alexandria
        #:mobius.utils
        #:mobius.list-utils)
  (:export :mapplist
           :mappl
           :plist-keys
           :plist-values
           :plist-length
           :every-value
           :plistp))

(defpackage :mobius.transducers
  (:use #:cl #:mobius.utils)
  (:export #:folded
           #:folded-value
           #:folded?
           #:fold
           #:transduce
           #:last-item
           #:take
           #:take-while
           #:drop
           #:drop-while
           #:filter
           #:fmap
           #:conj
           #:into
           #:basic-transducer
           #:f
           #:result
           #:input
           #:partition))

(defpackage :mobius.lazyseq
  (:use #:cl #:mobius.utils #:mobius.transducers)
  (:export #:lazyseq
           #:lazyseq-cons
           #:lazyseq-car
           #:lazyseq-cdr
           #:empty-lazyseq
           #:lazyseq-empty?
           #:lazyseq?
           #:lazyseq->list
           #:list->lazyseq
           #:lazyseq-iterate))

(defpackage :mobius.generator
  (:use #:cl
        #:mobius.utils
        #:mobius.transducers)
  (:export #:generator
           #:generator-p
           #:generator-seed
           #:iteration-generator
           #:generator-finished?
           #:next))
