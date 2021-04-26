;;;; quicklisp-systems.asd

(asdf:defsystem #:quicklisp-systems
  :description "Utilities for querying Quicklisp systems"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "quicklisp-systems"))
  :depends-on (:alexandria :dexador))
