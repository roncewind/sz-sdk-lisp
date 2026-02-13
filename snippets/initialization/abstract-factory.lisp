;;;; abstract-factory.lisp — Basic factory creation with all subsystems
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/abstract-factory.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:abstract-factory
  (:use #:cl #:sz-sdk))

(in-package #:abstract-factory)

(defparameter *instance-name* "abstract-factory")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((config-manager (create-config-manager factory))
            (diagnostic (create-diagnostic factory))
            (engine (create-engine factory))
            (product (create-product factory)))
        (declare (ignore config-manager diagnostic engine product))
        ;; Do work...
        ))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
