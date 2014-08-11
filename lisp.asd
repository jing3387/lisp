(defpackage :lisp-system
  (:use :cl :asdf))

(in-package :lisp-system)

(defsystem :lisp
  :description "Simple Lisp interpreter"
  :serial t
  :depends-on (:alexandria :optima :iterate)
  :components ((:file "chapter-one")))
