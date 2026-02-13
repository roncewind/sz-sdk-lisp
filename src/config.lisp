;;;; config.lisp — sz-config CLOS class and methods
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.config)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

(defclass sz-config ()
  ((config-definition
    :initform ""
    :accessor sz-config-config-definition
    :documentation "The in-memory Senzing configuration JSON document."))
  (:documentation "Senzing configuration interface."))

;;; ---------------------------------------------------------------------------
;;; Per-component exception function bindings
;;; ---------------------------------------------------------------------------

(defvar *get-exc-fn* #'%sz-config-get-last-exception
  "Function to retrieve the last config exception message.")
(defvar *clear-exc-fn* #'%sz-config-clear-last-exception
  "Function to clear the last config exception.")
(defvar *get-code-fn* #'%sz-config-get-last-exception-code
  "Function to retrieve the last config exception code.")

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric initialize-config (config instance-name settings &key verbose-logging)
  (:documentation "Initialize the Senzing config subsystem."))

(defmethod initialize-config ((config sz-config) instance-name settings
                              &key (verbose-logging 0))
  (let ((rc (%sz-config-init instance-name settings verbose-logging)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric destroy-config (config)
  (:documentation "Destroy the Senzing config subsystem."))

(defmethod destroy-config ((config sz-config))
  (%sz-config-destroy)
  (values))

;;; ---------------------------------------------------------------------------
;;; Public methods
;;; ---------------------------------------------------------------------------

(defgeneric config-export (config)
  (:documentation "Return the current configuration definition as a JSON string."))

(defmethod config-export ((config sz-config))
  (sz-config-config-definition config))

(defgeneric import-config-definition (config config-definition)
  (:documentation "Set the internal configuration from a JSON string."))

(defmethod import-config-definition ((config sz-config) config-definition)
  (setf (sz-config-config-definition config) config-definition)
  (values))

(defgeneric import-template (config)
  (:documentation "Load the default Senzing configuration template."))

(defmethod import-template ((config sz-config))
  ;; Create an in-memory config from the template
  (with-sz-handle-result (config-handle
                          (%sz-config-create-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         ;; Export it to a JSON string
         (with-sz-string-result (config-str
                                 (%sz-config-export-helper config-handle)
                                 *get-exc-fn* *clear-exc-fn* *get-code-fn*)
           (setf (sz-config-config-definition config) config-str))
      ;; Always close the in-memory config handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))))
  (values))

(defgeneric verify-config-definition (config config-definition)
  (:documentation "Verify that a configuration JSON document is valid. Signals on error."))

(defmethod verify-config-definition ((config sz-config) config-definition)
  ;; Load the config to verify it
  (with-sz-handle-result (config-handle
                          (%sz-config-load-helper config-definition)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         ;; Export to verify round-trip
         (with-sz-string-result (exported
                                 (%sz-config-export-helper config-handle)
                                 *get-exc-fn* *clear-exc-fn* *get-code-fn*)
           (declare (ignore exported)))
      ;; Always close the handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))))
  (values))

(defgeneric get-data-source-registry (config)
  (:documentation "Return the data source registry as a JSON string."))

(defmethod get-data-source-registry ((config sz-config))
  ;; Load the config
  (with-sz-handle-result (config-handle
                          (%sz-config-load-helper
                           (sz-config-config-definition config))
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         ;; Get the data source registry
         (with-sz-string-result (result
                                 (%sz-config-get-data-source-registry-helper
                                  config-handle)
                                 *get-exc-fn* *clear-exc-fn* *get-code-fn*)
           result)
      ;; Always close the handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)))))

(defgeneric register-data-source (config data-source-code)
  (:documentation "Register a data source. Returns the registration response JSON."))

(defmethod register-data-source ((config sz-config) data-source-code)
  ;; Load the config
  (with-sz-handle-result (config-handle
                          (%sz-config-load-helper
                           (sz-config-config-definition config))
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         (let ((result-json
                 ;; Register the data source
                 (with-sz-string-result (result
                                         (%sz-config-register-data-source-helper
                                          config-handle
                                          (build-dsrc-code-json data-source-code))
                                         *get-exc-fn* *clear-exc-fn* *get-code-fn*)
                   result)))
           ;; Export the modified config back
           (with-sz-string-result (new-config
                                   (%sz-config-export-helper config-handle)
                                   *get-exc-fn* *clear-exc-fn* *get-code-fn*)
             (setf (sz-config-config-definition config) new-config))
           result-json)
      ;; Always close the handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)))))

(defgeneric unregister-data-source (config data-source-code)
  (:documentation "Unregister a data source."))

(defmethod unregister-data-source ((config sz-config) data-source-code)
  ;; Load the config
  (with-sz-handle-result (config-handle
                          (%sz-config-load-helper
                           (sz-config-config-definition config))
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         (progn
           ;; Unregister the data source
           (let ((rc (%sz-config-unregister-data-source-helper
                      config-handle
                      (build-dsrc-code-json data-source-code))))
             (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
           ;; Export the modified config back
           (with-sz-string-result (new-config
                                   (%sz-config-export-helper config-handle)
                                   *get-exc-fn* *clear-exc-fn* *get-code-fn*)
             (setf (sz-config-config-definition config) new-config)))
      ;; Always close the handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))))
  (values))
