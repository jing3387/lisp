(defpackage :lisp-system
  (:use :cl :asdf))

(in-package :lisp-system)

(defsystem :lisp
  :description "Lisp in Small Pieces"
  :serial t
  :depends-on (:alexandria :optima :iterate)
  :components ((:file "chapter-one")))
