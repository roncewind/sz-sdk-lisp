;;;; factory.lisp — sz-abstract-factory CLOS class + with-sz-factory macro
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.factory)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

;;; FUTURE WORK (Option B — Radical Refactoring):
;;;
;;; The current design uses "phantom objects" — sz-engine, sz-product,
;;; sz-diagnostic, and sz-config-manager are CLOS classes that carry no state
;;; beyond a `destroyedp` flag.  The underlying C library uses global state,
;;; so these objects serve only as initialization tokens / proof-of-init
;;; handles.  Methods accept them but never read per-instance fields.
;;;
;;; A more idiomatic CL approach would be one of:
;;;
;;;   (a) Replace phantom classes with a single `sz-context` object that
;;;       tracks all initialization state, config-id, settings, etc.
;;;       Methods would take the context as their first argument.
;;;
;;;   (b) Use special variables (`*sz-engine-initialized*`, etc.) set by
;;;       the factory, checked by a macro/function.  This eliminates the
;;;       need to thread objects through call sites entirely.
;;;
;;;   (c) Make the factory itself the primary handle — `(add-record factory
;;;       ...)` — since it already holds all the initialization parameters.
;;;
;;; Any of these would simplify the API surface but would be a breaking
;;; change to the public interface.

(defclass sz-abstract-factory ()
  ((instance-name
    :initarg :instance-name
    :initform ""
    :reader factory-instance-name)
   (settings
    :initarg :settings
    :initform ""
    :reader factory-settings)
   (config-id
    :initarg :config-id
    :initform 0
    :reader factory-config-id)
   (verbose-logging
    :initarg :verbose-logging
    :initform 0
    :reader factory-verbose-logging)
   (destroyedp
    :initform nil
    :accessor factory-destroyed-p)
   ;; Track created objects for cleanup
   (created-objects
    :initform nil
    :accessor factory-created-objects))
  (:documentation "Factory pattern for accessing Senzing subsystems."))

;;; ---------------------------------------------------------------------------
;;; Factory methods
;;; ---------------------------------------------------------------------------

(defgeneric create-engine (factory)
  (:documentation "Create and initialize an sz-engine."))

(defmethod create-engine ((factory sz-abstract-factory))
  (let ((engine (make-instance 'sz-sdk.engine:sz-engine)))
    (sz-sdk.engine:initialize-engine engine
                                     (factory-instance-name factory)
                                     (factory-settings factory)
                                     :config-id (factory-config-id factory)
                                     :verbose-logging (factory-verbose-logging factory))
    (push engine (factory-created-objects factory))
    engine))

(defgeneric create-product (factory)
  (:documentation "Create and initialize an sz-product."))

(defmethod create-product ((factory sz-abstract-factory))
  (let ((product (make-instance 'sz-sdk.product:sz-product)))
    (sz-sdk.product:initialize-product product
                                       (factory-instance-name factory)
                                       (factory-settings factory)
                                       :verbose-logging (factory-verbose-logging factory))
    (push product (factory-created-objects factory))
    product))

(defgeneric create-config-manager (factory)
  (:documentation "Create and initialize an sz-config-manager."))

(defmethod create-config-manager ((factory sz-abstract-factory))
  (let ((mgr (make-instance 'sz-sdk.config-manager:sz-config-manager)))
    (sz-sdk.config-manager:initialize-config-manager
     mgr
     (factory-instance-name factory)
     (factory-settings factory)
     :verbose-logging (factory-verbose-logging factory))
    (push mgr (factory-created-objects factory))
    mgr))

(defgeneric create-diagnostic (factory)
  (:documentation "Create and initialize an sz-diagnostic."))

(defmethod create-diagnostic ((factory sz-abstract-factory))
  (let ((diag (make-instance 'sz-sdk.diagnostic:sz-diagnostic)))
    (sz-sdk.diagnostic:initialize-diagnostic
     diag
     (factory-instance-name factory)
     (factory-settings factory)
     :config-id (factory-config-id factory)
     :verbose-logging (factory-verbose-logging factory))
    (push diag (factory-created-objects factory))
    diag))

;;; ---------------------------------------------------------------------------
;;; Internal generic dispatch for component lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric destroy-component (component)
  (:documentation "Destroy a Senzing component. Dispatches to the appropriate
   component-specific destroy method."))

(defmethod destroy-component ((e sz-sdk.engine:sz-engine))
  (sz-sdk.engine:destroy-engine e))

(defmethod destroy-component ((p sz-sdk.product:sz-product))
  (sz-sdk.product:destroy-product p))

(defmethod destroy-component ((m sz-sdk.config-manager:sz-config-manager))
  (sz-sdk.config-manager:destroy-config-manager m))

(defmethod destroy-component ((d sz-sdk.diagnostic:sz-diagnostic))
  (sz-sdk.diagnostic:destroy-diagnostic d))

(defgeneric reinitialize-component (component config-id)
  (:documentation "Reinitialize a Senzing component with a new config ID.
   Default is a no-op for components that don't support reinitialization.")
  (:method (component config-id)
    (declare (ignore component config-id))
    (values)))

(defmethod reinitialize-component ((e sz-sdk.engine:sz-engine) config-id)
  (sz-sdk.engine:reinitialize-engine e config-id))

(defmethod reinitialize-component ((d sz-sdk.diagnostic:sz-diagnostic) config-id)
  (sz-sdk.diagnostic:reinitialize-diagnostic d config-id))

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric destroy-factory (factory)
  (:documentation "Destroy all objects created by this factory and the factory itself."))

(defmethod destroy-factory ((factory sz-abstract-factory))
  (unless (factory-destroyed-p factory)
    (dolist (obj (factory-created-objects factory))
      (ignore-errors (destroy-component obj)))
    (setf (factory-created-objects factory) nil)
    (setf (factory-destroyed-p factory) t))
  (values))

(defgeneric reinitialize-factory (factory config-id)
  (:documentation "Reinitialize all relevant subsystems with a new config ID."))

(defmethod reinitialize-factory ((factory sz-abstract-factory) config-id)
  (dolist (obj (factory-created-objects factory))
    (reinitialize-component obj config-id))
  (values))

;;; ---------------------------------------------------------------------------
;;; Convenience macro
;;; ---------------------------------------------------------------------------

(defmacro with-sz-factory ((factory-var instance-name settings
                            &key (config-id 0) (verbose-logging 0))
                           &body body)
  "Create an sz-abstract-factory, execute BODY, and ensure destroy is called.
   FACTORY-VAR is bound to the factory instance within BODY."
  `(let ((,factory-var (make-instance 'sz-abstract-factory
                                      :instance-name ,instance-name
                                      :settings ,settings
                                      :config-id ,config-id
                                      :verbose-logging ,verbose-logging)))
     (unwind-protect
          (progn ,@body)
       (destroy-factory ,factory-var))))
