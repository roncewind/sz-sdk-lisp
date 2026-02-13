;;;; abstract-factory-with-debug.lisp — Factory with verbose logging enabled
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/abstract-factory-with-debug.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:abstract-factory-with-debug
  (:use #:cl #:sz-sdk))

(in-package #:abstract-factory-with-debug)

(defparameter *instance-name* "abstract-factory-with-debug")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    ;; Create factory with verbose logging enabled
    (with-sz-factory (factory *instance-name* *settings* :verbose-logging 1)
      (create-engine factory))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
