;;;; purge-repository.lisp — Purge the Senzing datastore (interactive confirmation)
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/purge-repository.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:purge-repository
  (:use #:cl #:sz-sdk))

(in-package #:purge-repository)

(defparameter *instance-name* "purge-repository")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *purge-msg* "
**************************************** WARNING ****************************************

This example will purge all currently loaded data from the Senzing datastore!
Before proceeding, all instances of Senzing (custom code, tools, etc.) must be shut down.

*****************************************************************************************

Are you sure you want to continue and purge the Senzing datastore? Type YESPURGESENZING to purge: ")

(format t "~A" *purge-msg*)
(force-output)
(unless (string= (read-line) "YESPURGESENZING")
  (uiop:quit))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((diagnostic (create-diagnostic factory)))
        (purge-repository diagnostic)
        (format t "~%Senzing datastore purged~%")))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
