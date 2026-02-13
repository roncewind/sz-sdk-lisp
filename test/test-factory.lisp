;;;; test-factory.lisp — Tests for sz-abstract-factory (requires Senzing)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite factory-tests)

;;; ---------------------------------------------------------------------------
;;; Factory creation tests
;;; ---------------------------------------------------------------------------

(test factory-create-engine
  "create-engine returns a working sz-engine"
  (let ((factory (make-test-factory)))
    (unwind-protect
         (let ((engine (sz-sdk.factory:create-engine factory)))
           (is (typep engine 'sz-sdk.engine:sz-engine))
           ;; Verify it works
           (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
             (is (plusp config-id))))
      (sz-sdk.factory:destroy-factory factory))))

(test factory-create-product
  "create-product returns a working sz-product"
  (let ((factory (make-test-factory)))
    (unwind-protect
         (let ((product (sz-sdk.factory:create-product factory)))
           (is (typep product 'sz-sdk.product:sz-product))
           ;; Verify it works
           (let ((version (sz-sdk.product:get-version product)))
             (is (search "PRODUCT_NAME" version))))
      (sz-sdk.factory:destroy-factory factory))))

(test factory-create-config-manager
  "create-config-manager returns a working sz-config-manager"
  (let ((factory (make-test-factory)))
    (unwind-protect
         (let ((mgr (sz-sdk.factory:create-config-manager factory)))
           (is (typep mgr 'sz-sdk.config-manager:sz-config-manager))
           ;; Verify it works
           (let ((config-id (sz-sdk.config-manager:get-default-config-id mgr)))
             (is (plusp config-id))))
      (sz-sdk.factory:destroy-factory factory))))

(test factory-create-diagnostic
  "create-diagnostic returns a working sz-diagnostic"
  (let ((factory (make-test-factory)))
    (unwind-protect
         (let ((diag (sz-sdk.factory:create-diagnostic factory)))
           (is (typep diag 'sz-sdk.diagnostic:sz-diagnostic))
           ;; Verify it works
           (let ((info (sz-sdk.diagnostic:get-repository-info diag)))
             (is (search "dataStores" info))))
      (sz-sdk.factory:destroy-factory factory))))

;;; ---------------------------------------------------------------------------
;;; Factory lifecycle tests
;;; ---------------------------------------------------------------------------

(test factory-destroy
  "destroy-factory cleans up all created objects"
  (let ((factory (make-test-factory)))
    (sz-sdk.factory:create-engine factory)
    (sz-sdk.factory:create-product factory)
    (finishes (sz-sdk.factory:destroy-factory factory))))

(test factory-destroy-idempotent
  "destroying factory twice is safe"
  (let ((factory (make-test-factory)))
    (sz-sdk.factory:create-engine factory)
    (finishes (sz-sdk.factory:destroy-factory factory))
    (finishes (sz-sdk.factory:destroy-factory factory))))

(test factory-with-sz-factory-macro
  "with-sz-factory macro creates and destroys cleanly"
  (finishes
    (sz-sdk.factory:with-sz-factory (factory *test-instance-name* *test-settings*)
      (let ((engine (sz-sdk.factory:create-engine factory)))
        (is (typep engine 'sz-sdk.engine:sz-engine))
        (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
          (is (plusp config-id)))))))

(test factory-reinitialize
  "reinitialize-factory updates engine and diagnostic"
  (let ((factory (make-test-factory)))
    (unwind-protect
         (let ((engine (sz-sdk.factory:create-engine factory)))
           (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
             (finishes
               (sz-sdk.factory:reinitialize-factory factory config-id))))
      (sz-sdk.factory:destroy-factory factory))))
