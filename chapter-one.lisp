(defpackage :chapter-one
  (:nicknames :one)
  (:use :cl :optima :iterate)
  (:import-from :alexandria
                :copy-hash-table)
  (:export :repl))

(in-package :chapter-one)

(defparameter *functions*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql) (write prin1)
    (display princ) (newline terpri) (end end)))

(defparameter *variables*
  '((t t) (nil) (foo) (bar) (fib) (fact)))

(defparameter *env.init* (make-hash-table))

(defparameter *env.global* *env.init*)

(defun evaluate (exp env)
  (match exp
    ((satisfies symbolp) (lookup exp env))
    ((satisfies atom) exp)
    ((list 'quote obj) obj)
    ((list 'if test then else) (if (evaluate test env)
                                   (evaluate then env)
                                   (evaluate else env)))
    ((cons 'begin body) (eprogn body env))
    ((list 'set! var val) (update! var env (evaluate val env)))
    ((list* 'lambda params body) (make-function params body env))
    ((cons fn args) (invoke (evaluate fn env) (evlis args env)))))

(defun lookup (id env)
  (or (gethash id env) (error "No such binding ~s" id)))

(defun update! (id env val)
  (setf (gethash id env) val))

(defun evlis (exps env)
  (mapcar #'(lambda (exp) (evaluate exp env))
          exps))

(defun eprogn (exps env)
  (iter (for exp in exps)
        (for val = (evaluate exp env))
        (finally (return val))))

(defun extend (env vars vals)
  (iter (for var in vars)
        (for val in vals)
        (setf (gethash var env) val)
        (finally (return env))))

(defun make-function (vars body env)
  #'(lambda (&rest args)
      (eprogn body (extend (copy-hash-table env) vars args))))

(defun function-name (fn)
  (iter (for (key val) in-hashtable *env.global*)
        (when (eq val fn)
          (finish))
        (finally (if key (return key) (return 'lambda)))))

(defun invoke (fn args)
  (if (functionp fn)
      (let ((result (apply fn args)))
        (format t "(~s~{ ~a~}) => ~s~&" (function-name fn) args result)
        result)
      (error "Not a function ~s" fn)))

(defun init-env.global ()
  (iter (for f in *functions*)
        (match f
          ((list g h) (setf (gethash g *env.global*) (symbol-function h)))
          (f (setf (gethash f *env.global*) (symbol-function f)))))
  (iter (for v in *variables*)
        (setf (gethash (car v) *env.global*) (cdr v))))

(defun end ()
  (throw 'end nil))

(defun repl ()
  (catch 'end
    (iter (initially (format t "~&? "))
          (for input = (read))
          (time (evaluate input *env.global*))
          (format t "~&? "))))

(init-env.global)
