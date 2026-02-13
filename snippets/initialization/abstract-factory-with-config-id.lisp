;;;; abstract-factory-with-config-id.lisp — Factory with explicit config ID
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/abstract-factory-with-config-id.lisp
;;;
;;; NOTE: The config ID value is made up — this example will fail if you run it.

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:abstract-factory-with-config-id
  (:use #:cl #:sz-sdk))

(in-package #:abstract-factory-with-config-id)

(defparameter *instance-name* "abstract-factory-with-config-id")
(defparameter *config-id* 2787481550)

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings* :config-id *config-id*)
      (create-engine factory))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
