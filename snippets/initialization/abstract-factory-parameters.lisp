;;;; abstract-factory-parameters.lisp — Factory with programmatic settings
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/abstract-factory-parameters.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:abstract-factory-parameters
  (:use #:cl #:sz-sdk))

(in-package #:abstract-factory-parameters)

;; Build settings as a JSON string programmatically
(defparameter *settings*
  (format nil "{\"PIPELINE\":{\"CONFIGPATH\":\"~A\",\"RESOURCEPATH\":\"~A\",\"SUPPORTPATH\":\"~A\"},\"SQL\":{\"CONNECTION\":\"~A\"}}"
          "/etc/opt/senzing"
          "/opt/senzing/er/resources"
          "/opt/senzing/data"
          "sqlite3://na:na@/tmp/sqlite/G2C.db"))

(handler-case
    (with-sz-factory (factory "abstract-factory-parameters" *settings*)
      (let ((config-manager (create-config-manager factory))
            (diagnostic (create-diagnostic factory))
            (engine (create-engine factory))
            (product (create-product factory)))
        (declare (ignore config-manager diagnostic engine product))
        ;; Do work...
        ))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
