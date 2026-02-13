;;;; config-manager.lisp — sz-config-manager CLOS class and methods
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.config-manager)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

(defclass sz-config-manager ()
  ((instance-name
    :initform ""
    :accessor sz-config-manager-instance-name)
   (settings
    :initform ""
    :accessor sz-config-manager-settings)
   (verbose-logging
    :initform 0
    :accessor sz-config-manager-verbose-logging)
   (destroyedp
    :initform nil
    :accessor sz-config-manager-destroyed-p))
  (:documentation "Senzing configuration manager interface."))

;;; ---------------------------------------------------------------------------
;;; Per-component exception function bindings
;;; ---------------------------------------------------------------------------

(defvar *get-exc-fn* #'%sz-config-mgr-get-last-exception
  "Function to retrieve the last config manager exception message.")
(defvar *clear-exc-fn* #'%sz-config-mgr-clear-last-exception
  "Function to clear the last config manager exception.")
(defvar *get-code-fn* #'%sz-config-mgr-get-last-exception-code
  "Function to retrieve the last config manager exception code.")

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric initialize-config-manager (mgr instance-name settings &key verbose-logging)
  (:documentation "Initialize the Senzing config manager subsystem."))

(defmethod initialize-config-manager ((mgr sz-config-manager) instance-name settings
                                      &key (verbose-logging 0))
  (setf (sz-config-manager-instance-name mgr) instance-name
        (sz-config-manager-settings mgr) settings
        (sz-config-manager-verbose-logging mgr) verbose-logging)
  (let ((rc (%sz-config-mgr-init instance-name settings verbose-logging)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric destroy-config-manager (mgr)
  (:documentation "Destroy the Senzing config manager subsystem."))

(defmethod destroy-config-manager ((mgr sz-config-manager))
  (unless (sz-config-manager-destroyed-p mgr)
    (%sz-config-mgr-destroy)
    (setf (sz-config-manager-destroyed-p mgr) t))
  (values))

;;; ---------------------------------------------------------------------------
;;; Public methods
;;; ---------------------------------------------------------------------------

(defgeneric create-config-from-config-id (mgr config-id)
  (:documentation "Create an sz-config from a registered configuration ID."))

(defmethod create-config-from-config-id ((mgr sz-config-manager) config-id)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((config-definition
          (with-sz-string-result (result
                                  (%sz-config-mgr-get-config-helper config-id)
                                  *get-exc-fn* *clear-exc-fn* *get-code-fn*)
            result))
        (config (make-instance 'sz-sdk.config:sz-config)))
    (sz-sdk.config:import-config-definition config config-definition)
    (sz-sdk.config:initialize-config config
                                     (sz-config-manager-instance-name mgr)
                                     (sz-config-manager-settings mgr)
                                     :verbose-logging (sz-config-manager-verbose-logging mgr))
    config))

(defgeneric create-config-from-string (mgr config-definition)
  (:documentation "Create an sz-config from a configuration JSON string."))

(defmethod create-config-from-string ((mgr sz-config-manager) config-definition)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((config (make-instance 'sz-sdk.config:sz-config)))
    (sz-sdk.config:initialize-config config
                                     (sz-config-manager-instance-name mgr)
                                     (sz-config-manager-settings mgr)
                                     :verbose-logging (sz-config-manager-verbose-logging mgr))
    (sz-sdk.config:verify-config-definition config config-definition)
    (sz-sdk.config:import-config-definition config config-definition)
    config))

(defgeneric create-config-from-template (mgr)
  (:documentation "Create an sz-config from the default template."))

(defmethod create-config-from-template ((mgr sz-config-manager))
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((config (make-instance 'sz-sdk.config:sz-config)))
    (sz-sdk.config:initialize-config config
                                     (sz-config-manager-instance-name mgr)
                                     (sz-config-manager-settings mgr)
                                     :verbose-logging (sz-config-manager-verbose-logging mgr))
    (sz-sdk.config:import-template config)
    config))

(defgeneric get-config-registry (mgr)
  (:documentation "Return the config registry as a JSON string."))

(defmethod get-config-registry ((mgr sz-config-manager))
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (with-sz-string-result (result
                          (%sz-config-mgr-get-config-registry-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-default-config-id (mgr)
  (:documentation "Return the default configuration ID."))

(defmethod get-default-config-id ((mgr sz-config-manager))
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (with-sz-int64-result (result
                         (%sz-config-mgr-get-default-config-id-helper)
                         *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric register-config (mgr config-definition config-comment)
  (:documentation "Register a configuration. Returns the new config ID."))

(defmethod register-config ((mgr sz-config-manager) config-definition config-comment)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (with-sz-int64-result (result
                         (%sz-config-mgr-register-config-helper
                          config-definition config-comment)
                         *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric replace-default-config-id (mgr current-default-config-id new-default-config-id)
  (:documentation "Replace the default config ID atomically."))

(defmethod replace-default-config-id ((mgr sz-config-manager)
                                      current-default-config-id
                                      new-default-config-id)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((rc (%sz-config-mgr-replace-default-config-id
             current-default-config-id new-default-config-id)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric set-default-config (mgr config-definition config-comment)
  (:documentation "Register and set as default. Returns the new config ID."))

(defmethod set-default-config ((mgr sz-config-manager) config-definition config-comment)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((config-id (register-config mgr config-definition config-comment)))
    (set-default-config-id mgr config-id)
    config-id))

(defgeneric set-default-config-id (mgr config-id)
  (:documentation "Set the default configuration ID."))

(defmethod set-default-config-id ((mgr sz-config-manager) config-id)
  (check-not-destroyed (sz-config-manager-destroyed-p mgr) "Config Manager")
  (let ((rc (%sz-config-mgr-set-default-config-id config-id)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))
