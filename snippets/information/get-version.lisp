;;;; get-version.lisp — Get Senzing version information
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/information/get-version.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:get-version
  (:use #:cl #:sz-sdk))

(in-package #:get-version)

(defparameter *instance-name* "get-version")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((product (create-product factory)))
        (format t "~A~%" (get-version product))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
