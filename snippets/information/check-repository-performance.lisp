;;;; check-repository-performance.lisp — Run a Senzing performance check
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/information/check-repository-performance.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:check-repository-performance
  (:use #:cl #:sz-sdk))

(in-package #:check-repository-performance)

(defparameter *instance-name* "check-repository-performance")
(defparameter *seconds-to-run* 3)

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((diagnostic (create-diagnostic factory)))
        (format t "~A~%" (check-repository-performance diagnostic *seconds-to-run*))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
