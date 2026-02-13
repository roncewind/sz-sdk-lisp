;;;; get-config-registry.lisp — Get the Senzing config registry
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/configuration/get-config-registry.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:get-config-registry
  (:use #:cl #:sz-sdk))

(in-package #:get-config-registry)

(defparameter *instance-name* "get-config-registry")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let* ((config-manager (create-config-manager factory))
             (response (get-config-registry config-manager)))
        (format t "~A~%" response)))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
