# MOBIUS-UTILS System #

`MOBIUS-UTILS` system defines a number of packages providing general
utilities, extending existing packages (`ALEXANDRIA`, `SERIES` and
`OPTIMA`) and adding new functionality (such as transducers).

## Motivation ##

Although CL comes with a large set of functions there are some general
purpose functions that seems to be missing. Lots of developers started
their own sets of such functions, some of those made their way to
general libraries such as `ALEXANDRIA`. Here is my set of such
functions that appeared as a side-effect of working on a larger
project.

Additionally, I have found bits and pieces of useful code from
different sources, that made its way into this library (NB: if I am
using some else's code without acknowledging it, please let me know!).
Most notably, there are quite a few examples from Paul Graham's *On
Lisp* book.

## Dependencies ##

Dependencies include:

+ `ALEXANDRIA`: general utilities
+ `SERIES`: CL stream-like library
+ `OPTIMA`: pattern matching library
+ `LPARALLEL`: although `MOBIUS-UTILS` does not use parallel
  computation capabilities, it uses `LPARALLEL` unified access to
  future/promises and delays.

## Packages ##

+ `MOBIUS.UTILS` general utilities (see below for more detailed
explanation).
+ `MOBIUS.LIST-UTILS` more functions on lists
+ `MOBIUS.PLIST` helps to treat plists as proper data structures
+ `MOBIUS.TRANSDUCERS` is a port of Clojure's transducers
+ `MOBIUS.LAZYSEQ` yet another lazy sequence implementation
+ `MOBIUS.GENERATOR` generator data structure compatible with
transducers.

### `MOBIUS.UTILS` ###

Utilities in this package are generally uncategorized. It started as a
collection of useful functions and macros. Half-way through though I
realized that it started replicating too much of functionality of
other packages, such as `ALEXANDRIA`. So, it has undergone a
significant cleanup. Roughly, the following groups are present:

- Clojure-like macros: threading macros `->` and `->>` to write
  something like this:

```

#!common-lisp

(->> '(1 2 3)
    (mapcar #'1+)
    (reduce #'+))

```

Also, Clojure-like shorthand for an anonymous function: `#f(* 2%)`
will expand into `(lambda (#:G221) (* 2 #:G221))`. This macro
supports only functions of one argument, thus Clojure's `#(* %1 %2)`
is not possible.

- Abbreviations (inspired by and some borrowed from *On Lisp*):
  `MVBIND`, `DSBIND`, `DSARRAY`, `IF-REF`, `LET1`. The first two are
  shorthands for `MULTIPLE-VALUE-BIND` and `DESTRUCTURING-BIND` (which
  have horribly long names). `DSARRAY` is my own take on how to easily
  get a few items out of the array. Usage:

```

#!common-lisp
(dsarray (x y z) (#2A((1 2 3) (4 5 6)) (0 0) (1 1) (1 2))
   (list x y z))

```

  This will produce `(1 5 6)`.
  `IF-AREF` works the same as `AREF` except it checks if array is not
  nil first. If array is nil, the last argument of `IF-AREF` is
  treated as an alternative value. For example, `(if-aref arr 1 1 20)`
  will get `(aref arr 1 1)` if `arr` is not nil and 20 otherwise.
  `LET1` creates let-binding for just one variable, thus making one
  set of parentheses redundant: `(let1 (x 10) (+ x 1))`.

- Destructive functions (inspired by *On Lisp*): `!`, `FUNCALL!`,
  `APPLY!`, `DEF!` and `*!EQUIVS*`. CL naming of destructive
  equivalents is rather bizzare: sometimes it adds `N` in front,
  sometimes it changes the word (from `REMOVE` to `DELETE`), sometimes
  it changes the wording completely. These functions unify access to
  destructive functions. `DEF!` defines destructive equivalent: `(def!
  remove-if delete-if)`. `!` fetches destructive equivalent: `(!
  remove-if)` will return `#'delete-if`. `FUNCALL!` and `APPLY!` will
  call the destructive equivalent of the function: `(funcall!
  #'remove-if #'oddp list)`.

- Functional programming: `PARTIAL` (Clojure-like) `FN` (from *On
  Lisp*), `FLOOP`, `MEMOIZE!`, `LAMBDA-CASE`. `PARTIAL` partially
  applies function. For example, `(partial #'mapcar #'1+)` will
  return the function that applied to a list, would increament each
  element of the list: `(funcall (partial #'mapcar #'1+) '(1 2 3))`
  will result in `(2 3 4)`. `FN` is a generalized function-builder
  from *On Lisp*. `FLOOP` (functional loop) is, in fact, an optimized
  version of Scheme named-lambda for tail-recursive functions.
  Semantic:
  
```

#!common-lisp
(floop my-loop ((x 0) (l '(1 2 3)))
  (cond ((null l) x)
        (t (my-loop (+ x (car l)) (cdr l)))))

```

  Most importantly, `FLOOP` is safe to return clausures that depend on
  `FLOOP` argument. For example, the following code will produce
  unintendent result:

```

#!common-lisp

(defvar *l* (loop for i from 0 below 5
              collecting (lambda () i)))
(mapcar (lambda (f) (funcall f)) *l*)
(5 5 5 5 5)

```

  `FLOOP` does work correcly:

```

#!common-lisp
(setf *l* (floop my-loop ((r '()) (i 0))
            (cond ((= i 5) (funcall! #'reverse r))
                           (t (my-loop (cons (lambda () i) r)
                                       (1+ i))))))

(mapcar (lambda (f) (funcall f)) *l*)
(0 1 2 3 4 5)

```

  `MEMOIZE!` will replace function with its memoized version. This
  operation is destructive to original function! This is useful for
  recursive functions (for example, naive implementation of Fibonacci
  numbers generator). `LAMBDA-CASE` provides pattern matching for
  `LAMBDA`: it is slight extension on `LAMBDA-MATCH` of `OPTIMA`
  package: while `LAMBDA-MATCH` accepts one argument, which gets
  matched, `LAMBDA-CASE` bundles all function arguments into `&rest`
  and matches this list against the pattern. It is mostly useful in
  defining multi-arity functions.

- Vector manipulation: `MAPVEC`, `MAPVECI`, `COMPUTE-VECTOR`. Both
  `MAPVEC` and `MAPVECI` iterate over a vector producing a new vector
  as a result. The iteratee of `MAPVECI` must accept current index of
  the vector as the first argument. `COMPUTE-VECTOR` builds a vector
  from the function taking single integer argument (element index).

- Symbol manipulation: `INTERN-KEYWORD` (depricated), `BUILD-SYMBOL`
  and `TAGGED-LIST-P` (*SICP*). `INTERN-KEYWORD` makes a keyword out
  of the symbol. It duplicates the functionality of `MAKE-KEYWORD`
  from `ALEXANDRIA` and is discouraged from being used. It will be
  removed in next revision of the package. `BUILD-KEYWORD` produces a
  keyword made of parts: `(build-symbol "hello-" 'wolrd)` produces
  symbol `hello-world`. `TAGGED-LIST-P` is used for simple compound
  type representation via tagged lists: the firts item of the list is
  type tag (symbol), the rest: slots of the type. This function,
  effectively, checks if the object is a tagged list with a particular
  tag. CL provides classes and structures to deal with compound
  objects and they are preferrable way of dealing with them. However,
  tagged lists for small objects that do not require inheretance offer
  less peformance overhead.

- Other: `AVERAGE` and `LIST-FUNCTIONS-IN-PACKAGE`. I guess,
  self-explanatory.

### `MOBIUS.LIST-UTILS` ###

Functions inspired by Clojure: `SPLIT-TO-CHUNKS`, `INTEERLEAVE`,
`FREQENCIES`. The first partitions list to parts of fixed length,
walking along the list with the fixed step. Thus, one can split the
list into either overlapping or non-overlapping chunks: `
(split-to-chunks '(1 2 3 4 5 6) 2)` will split the list into sublists,
each of length 2: `((1 2) (3 4) (5 6))`. By default, it uses step that
is equal to the length of chunks, thus chunks do not overlap.
`(split-to-chunks '(1 2 3 4 5 6) 2 1)` will result in `((1 2) (2 3) (3
4) (4 5) (5 6))`. `INTERLEAVE` mingles two or more lists. It is
convenient for constructing plists: `(interleave '(:name :surname
:country) '("John" "Smith" "UK"))` => `(:NAME "John" :SURNAME "Smith"
:COUNTRY "UK")`. `FREQUENCIES` finds the number of entries of each
item in the list, returning alist of `(item . number)` for each unique
item. It accepts key arguments `TEST` (defaults to `#'EQL`) and `KEY`
(defualts to `#'IDENTITY`).

Functions from *On Lisp*: `LAST1`, `SINGLE`, `APPEND1` (with
destructive equivalent), `MKLIST`, `LONGER`, `FILTER`, `PRUNE`,
`FIND2`, `BEFORE`, `AFTER`, `SPLIT-LIST-IF`, `MAP->`, `MAPA-B`.

Function/macro `FIXED-POINT-SERIES` produces series (see `SERIES`
package) of subsequent values `x`, `f(x)`, `f(f(x))`... More about it
see below.

### `MOBIUS.PLIST` ###

PList is a great way to keep small dictionaries (for tests I did,
plists outperformed hashtables on dictionaries with less than 100
elements). However, the support of plists in core CL is not so great.
This package extends this support.

`MAPPLIST` : `FUN` `PLIST` ...

Iterates over `PLIST` and other plists with function `FUN`. `FUN` must
accept:

- `KEY`: plist key
- `VALUE`: value associated with `KEY` from the firts `PLIST`
- more values: values pulled from other plists, that correspond to the
  same `KEY`.

Thus, the order of key-values pairs in plists does not matter. `FUN`
must return just a value. The result of `MAPPLIST` is a plist with
keys from the first `PLIST` and values being results of `FUN`
application.


`MAPPL` is the same as `MAPPLIST` but does not produce a plist and can
be used for side effects.

`PLIST-KEYS` and `PLIST-VALUES` extracts keys and values from the
plist respectively.

This package also introduces type `PLIST` and predicate `PLISTP`.
However, the predicate is very slow!

`PLIST-LENGTH` : `PLIST`

Computes the length of the plist, treating each key-value pair as one
entry. 


### MOBIUS.TRANSDUCERS ###

Clojure 1.7 introduced the concept of transducers: functions that
perform some actions on the collection without knowing what kind of
collection it is. In a sense, transducers are another take on
enumerators.

At the core of transducers lies generic function `FOLD`: it is the
only interface into the collection for transducers. `FOLD` is a
generalized `REDUCE` that can be used on any collection. The main
difference is that `FOLD` must provide a treatment to a special case,
already folded value: this helps to stop traversing the collection
once the answer is found (in Scheme one might use continuations to
stop it, although, current method is a bit cruder, it does the job).

`FOLD` : `FUN` `INIT` `DATA`

(Generic function) Works similarly to `REDUCE` with `FUN` :
`INTERMEDIATE-RESULT` `INPUT` -> `RESULT`. `INIT` is used for the
first `INTERMEDIATE-RESULT`. The dispatchof `FOLD` is done on `DATA`.
Implementation for lists is provided.

Class `FOLDED` describes folded value. `FOLDED?` checks if the value
is a type of `FOLDED` and `FOLDED-VALUE` extracts the value out of
`FOLDED` object. Function `FOLDED` constructs `FOLDED` object.

The implementation of `FOLD` for lists is as follows:

```

#!common-lisp

(defmethod fold (fun init (data list))
  (floop fold-loop ((l data) (result init))
    (cond ((null l) result)
          ((folded? result) (folded-value result))
          (t (fold-loop (cdr l) (funcall fun result (car l)))))))

```

Generic function `CONJ` adds item to a collection (for lists it falls
to `CONS`: `(conj list x)` => `(cons x list)`).

Transducers: `FMAP`, `TAKE`, `DROP`, `TAKE-WHILE`, `DROP-WHILE` and
`FILTER`. To be precise, these functions *return* transducers as a
result. Transducer is an object that determines an action to be
performed on the item of the collection prior to it folding. It is
represented as a function of folding function `F`, returning a
function, that determines the action in three cases:

- what to do if there is neither result nor input,
- what to do if there is a result but no input,
- what to do if there are both result and input.

Thus the most generic form of a transducer is:

```

#!common-lisp

(lambda (f)
  (lambda-case
    ((list) (no-result-input-action))
    ((list r) (result-action))
    ((list r input) (result-input-action))))


```

Most commonly, however, the first two cases are trivial (I have not
seen a transducer that will do something different there):

```

#!common-lisp

(lambda (f)
  (lambda-case
    ((list) (funcall f))
    ((list r) (funcall f r))
    ((list r input) (result-input-action))))

```

Transducers can be combined using `COMPOSE` from `ALEXANDRIA`. For
example, `(compose (take 10) (fmap #'1+))` will make a transducer that
takes first 10 elements out of the collection and adds 1 to each of
them. Due to the way of how arguments get passed, the actions of the
transducers take place from left to right in `COMPOSE` instead of
right to left as usually expected.

Transducers as `TAKE` or `TAKE-WHILE` need to indicate if they had
consumed enough input. In this case they must wrap `R` argument in
`FOLDED`, causing `FOLD` to stop traversing the collection.
Additionally, `TAKE` must also keep a local state, counting how many
items it has processed already.

Application of the transducers to a collection is done through
`TRANSDUCE` (uses `FOLD`).

`TRANSDUCE`: `XFORM` `FUN` `DATA`

`XFORM` is a transducer, `FUN` is a folding function. In addition to
`FOLD` semantics, `FUN` might be called without arguments or just with
`INTERMEDIATE-RESULT` and must provide a sane behaviour for these
cases. `DATA` is any collection that implements `FOLD`. `TRANSDUCE`
will apply `XFORM` to each item of `DATA` prior passing it to `FUN` as
`INPUT`.

`INTO` : `RECIPIENT` `XFORM` `DATA`

collects transformed items from `DATA` (must implement `FOLD`) into
`RECIPIENT` (must implement `INTO`).


### MOBIUS.LAZYSEQ ###

This is yet another implementation of lazy-sequences (AKA streams in
Scheme, CL reserves "stream" for I/O operations, thus *lazy-sequnces*
are used instead).

The implementation is quite straight-forward: the head of the sequence
is calculated, while the tail is delayed. The request of `LAZYSEQ-CDR`
forces the tail (but not the tail of this tail). The implementation
does not uses CONS-cells, however. This is done to distinguish
lazy-sequences from normal lists and use them as data type in generic
functions.

Basic constructors and selectors: `LAZYSEQ`, `LAZYSEQ-CONS`,
`LAZYSEQ-CAR` and `LAZYSEQ-CDR`. Example: `(lazyseq 1 2 3 4)`,
`(lazyseq cons 1 (lazyseq 2 3 4))`, `(lazyseq-cdr l)`.

Empty lazy-sequence is not `NIL`, but `EMPTY-LAZYSEQ`.

Predicates: `LAZYSEQ?`, `LAZYSEQ-EMPTY?`.

Conversion: `LAZYSEQ->LIST` and `LIST->LAZYSEQ`.

Lazy-sequence implements `FOLD` and `CONJ` and thus can be used with
transducers.

Function `LAZYSEQ-ITERATE` produces infinite sequence of `x`, `f(x)`,
`f(f(x))`... (see below for more explanation).

### MOBIUS.GENERATOR ###

Generator is another take on lazy-sequences and suspended evaluations:
instead of accumulating forced values like lazy-sequence, generator
just produces a new value all the time, discarding the old value. This
package implements persistent generator. The implementation works, but
API is very alpha and can change.

Generator is defined through the class `GENERATOR` with three slots:

- `SEED` is the current value of generator
- `PRODUCE` is the function that generates next value. To keep
  generator persistent, `PRODUCE` must produce a new generator object
  with updated `SEED`.
- `FINISHED?` function that check if the generator cannot produce more
  values.

`ITERATION-GENERATOR` constructs the generator that produces the
sequence `x`, `f(x)`, `f(f(x))`...

Function `NEXT` gets a new value from generator (actually, new
generator, the value must be extracted with `GENERATOR-SEED`).

Generator implements `FOLD` (but not `INTO`!) and can be used with
transducers.

### Iterations and contraction mapping ###

In this library there are three takes on producing the sequence of
`x`, `f(x)`, `f(f(x))`...: one uses `SERIES`, one uses `LAZYSEQ` and
the last one uses `GENERATOR`.

First, let's talk about the importance of these kind of sequences.
They arise from solving `x=f(x)` where `f(.)` is a contraction
mapping. Interestingly, these kind of equations can pop in different
places and are not restricted to just numerical computations. As for
numerical computations, almost every iterative algorithm can be
expressed as `x=f(x)`.

Since I am dealing with mostly numerical computations and spent past
year generalizing some algorithms, I have a personal quest to find the
most expressive, flexible and yet efficient way to describe the
solution of `x=f(x)`. Three takes presented in this library have
relative pros and cons:

- `SERIES` provides excellent optimization, but there are number of
  restrictions on its use to keep the efficiency.
- `LAZYSEQ` provide the easiest and the most straight-forward
  interface to this computation, but create quite a lot of overhead.
- `GENERATOR` still not as efficient as `SERIES`, but much faster than
  `LAZYSEQ`. Thanks to transducers, the interface to it can be as
  straight-forward as for lazy-sequences.


# Links and references #

- [Paul Graham, *On Lisp*](http://www.paulgraham.com/onlisp.html)
- [H. Abelson, G.J. Sussman, J. Sussman, *SICP*](http://mitpress.mit.edu/sicp/full-text/book/book.html)
- [Clojure](http://clojure.org)
- [Examples of Clojure's transducers](http://ianrumford.github.io/blog/2014/08/08/Some-trivial-examples-of-using-Clojure-Transducers/)
- [`ALEXANDRIA`](http://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow)
- [`SERIES`](http://sourceforge.net/projects/series/)
- [`OPTIMA`](https://github.com/m2ym/optima)
- [`LPARALLEL`](http://lparallel.org)
- [Contraction mapping](http://en.wikipedia.org/wiki/Contraction_mapping)


# Copyright #

Copyright 2014, Alexey Cherkaev (aka mobius-eng). The library is
distributed under LGPLv3 license.
