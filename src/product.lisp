;;;; product.lisp — sz-product CLOS class and methods
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.product)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

(defclass sz-product ()
  ((destroyedp
    :initform nil
    :accessor sz-product-destroyed-p))
  (:documentation "Senzing product information interface."))

;;; ---------------------------------------------------------------------------
;;; Per-component exception function bindings
;;; ---------------------------------------------------------------------------

(defvar *get-exc-fn* #'%sz-product-get-last-exception
  "Function to retrieve the last product exception message.")
(defvar *clear-exc-fn* #'%sz-product-clear-last-exception
  "Function to clear the last product exception.")
(defvar *get-code-fn* #'%sz-product-get-last-exception-code
  "Function to retrieve the last product exception code.")

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric initialize-product (product instance-name settings &key verbose-logging)
  (:documentation "Initialize the Senzing product subsystem."))

(defmethod initialize-product ((product sz-product) instance-name settings
                               &key (verbose-logging 0))
  (let ((rc (%sz-product-init instance-name settings verbose-logging)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric destroy-product (product)
  (:documentation "Destroy the Senzing product subsystem."))

(defmethod destroy-product ((product sz-product))
  (unless (sz-product-destroyed-p product)
    (%sz-product-destroy)
    (setf (sz-product-destroyed-p product) t))
  (values))

;;; ---------------------------------------------------------------------------
;;; Public methods
;;; ---------------------------------------------------------------------------

(defgeneric get-license (product)
  (:documentation "Return license information as a JSON string."))

(defmethod get-license ((product sz-product))
  (check-not-destroyed (sz-product-destroyed-p product) "Product")
  (%sz-product-get-license))

(defgeneric get-version (product)
  (:documentation "Return version/build information as a JSON string."))

(defmethod get-version ((product sz-product))
  (check-not-destroyed (sz-product-destroyed-p product) "Product")
  (%sz-product-get-version))
