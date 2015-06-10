(asdf:defsystem #:mobius-utils
  :serial t
  :description "Utility functions and extensions"
  :author "Alexey Cherkaev <Alexey.Cherkaev@gmail.com>"
  :license "LGPLv3"
  :version "0.1.0"
  :depends-on (:alexandria :optima)
  :components ((:module
                "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "list-utils")
                             (:file "plist")
                             (:file "transducers")
                             (:file "lazyseq")
                             (:file "generator")))))

