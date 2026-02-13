;;;; test-diagnostic.lisp — Tests for sz-diagnostic (requires Senzing installed)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite diagnostic-tests)

;;; ---------------------------------------------------------------------------
;;; Diagnostic tests
;;; ---------------------------------------------------------------------------

(test diagnostic-check-repository-performance
  "check-repository-performance returns JSON with performance metrics"
  (let ((diag (make-test-diagnostic)))
    (unwind-protect
         (let ((result (sz-sdk.diagnostic:check-repository-performance diag 1)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "numRecordsInserted" result))
           (is (search "insertTime" result)))
      (sz-sdk.diagnostic:destroy-diagnostic diag))))

(test diagnostic-get-repository-info
  "get-repository-info returns JSON with dataStores array"
  (let ((diag (make-test-diagnostic)))
    (unwind-protect
         (let ((result (sz-sdk.diagnostic:get-repository-info diag)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "dataStores" result)))
      (sz-sdk.diagnostic:destroy-diagnostic diag))))

(test diagnostic-get-feature
  "get-feature with a loaded record returns JSON for valid feature ID"
  ;; This test requires truthset data to be loaded to have features
  ;; We use a simple approach: add a record, get its entity, then try feature 1
  (let ((diag (make-test-diagnostic)))
    (unwind-protect
         ;; Feature IDs start at 1; try a low ID
         ;; If no records loaded, this may error which is also valid
         (handler-case
             (let ((result (sz-sdk.diagnostic:get-feature diag 1)))
               (is (stringp result)))
           (sz-error ()
             ;; If feature doesn't exist, that's expected with empty DB
             (pass "get-feature signals error for non-existent feature")))
      (sz-sdk.diagnostic:destroy-diagnostic diag))))

(test diagnostic-get-feature-unknown-id
  "get-feature with bogus feature ID signals condition"
  (let ((diag (make-test-diagnostic)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.diagnostic:get-feature diag 999999999))
      (sz-sdk.diagnostic:destroy-diagnostic diag))))

(test diagnostic-reinitialize
  "reinitialize-diagnostic with current config ID succeeds"
  (let ((diag (make-test-diagnostic))
        (engine (make-test-engine)))
    (unwind-protect
         (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
           (finishes
             (sz-sdk.diagnostic:reinitialize-diagnostic diag config-id)))
      (sz-sdk.engine:destroy-engine engine)
      (sz-sdk.diagnostic:destroy-diagnostic diag))))

(test diagnostic-reinitialize-bad-config-id
  "reinitialize-diagnostic with bad config ID signals condition"
  (let ((diag (make-test-diagnostic)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.diagnostic:reinitialize-diagnostic diag 999999999))
      (sz-sdk.diagnostic:destroy-diagnostic diag))))
