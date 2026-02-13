;;;; get-license.lisp — Get Senzing license information
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/information/get-license.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:get-license
  (:use #:cl #:sz-sdk))

(in-package #:get-license)

(defparameter *instance-name* "get-license")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((product (create-product factory)))
        (format t "~A~%" (get-license product))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
