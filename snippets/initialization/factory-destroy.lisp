;;;; factory-destroy.lisp — Explicit factory destroy with unwind-protect
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/factory-destroy.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:factory-destroy
  (:use #:cl #:sz-sdk))

(in-package #:factory-destroy)

(defparameter *instance-name* "factory-destroy")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(let ((factory (make-instance 'sz-abstract-factory
                               :instance-name *instance-name*
                               :settings *settings*)))
  (unwind-protect
       (handler-case
           (let ((engine (create-engine factory)))
             (declare (ignore engine))
             ;; Do work...
             )
         (sz-error (e)
           (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
    ;; Destroys the factory and all objects it created, such as engine above.
    ;; In Lisp, with-sz-factory handles this automatically. This snippet
    ;; shows the explicit approach using unwind-protect.
    (destroy-factory factory)))
