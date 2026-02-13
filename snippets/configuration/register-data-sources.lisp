;;;; register-data-sources.lisp — Register data sources in Senzing configuration
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/configuration/register-data-sources.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:register-data-sources
  (:use #:cl #:sz-sdk))

(in-package #:register-data-sources)

(defparameter *instance-name* "register-data-sources")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let* ((config-manager (create-config-manager factory))
             (current-config-id (get-default-config-id config-manager))
             (config (create-config-from-config-id config-manager current-config-id)))
        ;; Register data sources
        (dolist (data-source '("CUSTOMERS" "REFERENCE" "WATCHLIST"))
          (register-data-source config data-source))
        ;; Export, register, and atomically replace the default config
        (let* ((new-config (config-export config))
               (new-config-id (register-config config-manager new-config
                                               "Code snippet register_data_source example")))
          (replace-default-config-id config-manager current-config-id new-config-id)
          (format t "New default config ID: ~A~%" new-config-id))))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
