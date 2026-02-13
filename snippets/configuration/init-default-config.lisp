;;;; init-default-config.lisp — Initialize a default Senzing configuration
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/configuration/init-default-config.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:init-default-config
  (:use #:cl #:sz-sdk))

(in-package #:init-default-config)

(defparameter *instance-name* "init-default-config")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((config-manager (create-config-manager factory)))
        ;; Check if a config already exists
        (let ((current-config-id (get-default-config-id config-manager)))
          (when (plusp current-config-id)
            (format t "~%A configuration exists in the repository, replace it with a template configuration? (y/n)  ")
            (force-output)
            (let ((answer (read-line)))
              (unless (member answer '("y" "yes") :test #'string-equal)
                (uiop:quit 1)))))
        ;; Create config from template and set as default
        (let* ((config (create-config-from-template config-manager))
               (new-config-def (config-export config))
               (new-config-id (set-default-config config-manager new-config-def
                                                  "Code snippet init_default_config example")))
          (format t "New default config ID: ~A~%" new-config-id))))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
