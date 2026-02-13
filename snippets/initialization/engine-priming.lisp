;;;; engine-priming.lisp — Create engine and prime it for faster initial operations
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/engine-priming.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:engine-priming
  (:use #:cl #:sz-sdk))

(in-package #:engine-priming)

(defparameter *instance-name* "engine-priming")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (format t "Priming Senzing engine...~%")
        (prime-engine engine)
        ;; Do work...
        ))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
