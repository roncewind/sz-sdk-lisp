;;;; get-repository-info.lisp — Get Senzing repository information
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/information/get-repository-info.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:get-repository-info
  (:use #:cl #:sz-sdk))

(in-package #:get-repository-info)

(defparameter *instance-name* "get-repository-info")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((diagnostic (create-diagnostic factory)))
        (format t "~A~%" (get-repository-info diagnostic))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
