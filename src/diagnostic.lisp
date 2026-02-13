;;;; diagnostic.lisp — sz-diagnostic CLOS class and methods
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.diagnostic)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

(defclass sz-diagnostic ()
  ((destroyedp
    :initform nil
    :accessor sz-diagnostic-destroyed-p))
  (:documentation "Senzing diagnostic interface."))

;;; ---------------------------------------------------------------------------
;;; Per-component exception function bindings
;;; ---------------------------------------------------------------------------

(defvar *get-exc-fn* #'%sz-diagnostic-get-last-exception
  "Function to retrieve the last diagnostic exception message.")
(defvar *clear-exc-fn* #'%sz-diagnostic-clear-last-exception
  "Function to clear the last diagnostic exception.")
(defvar *get-code-fn* #'%sz-diagnostic-get-last-exception-code
  "Function to retrieve the last diagnostic exception code.")

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric initialize-diagnostic (diag instance-name settings
                                   &key config-id verbose-logging)
  (:documentation "Initialize the Senzing diagnostic subsystem."))

(defmethod initialize-diagnostic ((diag sz-diagnostic) instance-name settings
                                  &key (config-id 0) (verbose-logging 0))
  (let ((rc (if (zerop config-id)
                (%sz-diagnostic-init instance-name settings verbose-logging)
                (%sz-diagnostic-init-with-config-id
                 instance-name settings config-id verbose-logging))))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric destroy-diagnostic (diag)
  (:documentation "Destroy the Senzing diagnostic subsystem."))

(defmethod destroy-diagnostic ((diag sz-diagnostic))
  (unless (sz-diagnostic-destroyed-p diag)
    (%sz-diagnostic-destroy)
    (setf (sz-diagnostic-destroyed-p diag) t))
  (values))

(defgeneric reinitialize-diagnostic (diag config-id)
  (:documentation "Reinitialize with a new configuration ID."))

(defmethod reinitialize-diagnostic ((diag sz-diagnostic) config-id)
  (check-not-destroyed (sz-diagnostic-destroyed-p diag) "Diagnostic")
  (let ((rc (%sz-diagnostic-reinit config-id)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

;;; ---------------------------------------------------------------------------
;;; Public methods
;;; ---------------------------------------------------------------------------

(defgeneric check-repository-performance (diag seconds-to-run)
  (:documentation "Run a performance test for the given number of seconds.
   Returns a JSON string with performance metrics."))

(defmethod check-repository-performance ((diag sz-diagnostic) seconds-to-run)
  (check-not-destroyed (sz-diagnostic-destroyed-p diag) "Diagnostic")
  (with-sz-string-result (result
                          (%sz-diagnostic-check-repository-performance-helper
                           seconds-to-run)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-repository-info (diag)
  (:documentation "Return repository information as a JSON string."))

(defmethod get-repository-info ((diag sz-diagnostic))
  (check-not-destroyed (sz-diagnostic-destroyed-p diag) "Diagnostic")
  (with-sz-string-result (result
                          (%sz-diagnostic-get-repository-info-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-feature (diag feature-id)
  (:documentation "Get a feature by ID. This function is unsupported and undocumented."))

(defmethod get-feature ((diag sz-diagnostic) feature-id)
  (check-not-destroyed (sz-diagnostic-destroyed-p diag) "Diagnostic")
  (with-sz-string-result (result
                          (%sz-diagnostic-get-feature-helper feature-id)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric purge-repository (diag)
  (:documentation "Purge all data from the repository. DESTRUCTIVE."))

(defmethod purge-repository ((diag sz-diagnostic))
  (check-not-destroyed (sz-diagnostic-destroyed-p diag) "Diagnostic")
  (let ((rc (%sz-diagnostic-purge-repository)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))
