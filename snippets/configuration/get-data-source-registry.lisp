;;;; get-data-source-registry.lisp — Get the data source registry
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/configuration/get-data-source-registry.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:get-data-source-registry
  (:use #:cl #:sz-sdk))

(in-package #:get-data-source-registry)

(defparameter *instance-name* "get-data-source-registry")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let* ((config-manager (create-config-manager factory))
             (config-id (get-default-config-id config-manager))
             (config (create-config-from-config-id config-manager config-id))
             (response (get-data-source-registry config)))
        (format t "~A~%" response)))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
