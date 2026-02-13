;;;; test-conditions.lisp — Tests for the condition hierarchy
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite condition-tests)

;;; ---------------------------------------------------------------------------
;;; Condition hierarchy tests
;;; ---------------------------------------------------------------------------

(test condition-base-type
  "sz-error is the base condition type"
  (let ((c (make-condition 'sz-error :error-code 42 :error-message "test")))
    (is (typep c 'sz-error))
    (is (typep c 'error))
    (is (= 42 (sz-error-code c)))
    (is (string= "test" (sz-error-message c)))))

(test condition-hierarchy-bad-input
  "Bad input conditions inherit correctly"
  (is (subtypep 'sz-bad-input-error 'sz-error))
  (is (subtypep 'sz-not-found-error 'sz-bad-input-error))
  (is (subtypep 'sz-not-found-error 'sz-error))
  (is (subtypep 'sz-unknown-data-source-error 'sz-bad-input-error)))

(test condition-hierarchy-general
  "General conditions inherit correctly"
  (is (subtypep 'sz-general-error 'sz-error))
  (is (subtypep 'sz-configuration-error 'sz-general-error))
  (is (subtypep 'sz-replace-conflict-error 'sz-general-error))
  (is (subtypep 'sz-sdk-error 'sz-general-error)))

(test condition-hierarchy-retryable
  "Retryable conditions inherit correctly"
  (is (subtypep 'sz-retryable-error 'sz-error))
  (is (subtypep 'sz-database-connection-lost-error 'sz-retryable-error))
  (is (subtypep 'sz-database-transient-error 'sz-retryable-error))
  (is (subtypep 'sz-retry-timeout-exceeded-error 'sz-retryable-error)))

(test condition-hierarchy-unrecoverable
  "Unrecoverable conditions inherit correctly"
  (is (subtypep 'sz-unrecoverable-error 'sz-error))
  (is (subtypep 'sz-database-error 'sz-unrecoverable-error))
  (is (subtypep 'sz-license-error 'sz-unrecoverable-error))
  (is (subtypep 'sz-not-initialized-error 'sz-unrecoverable-error))
  (is (subtypep 'sz-unhandled-error 'sz-unrecoverable-error)))

;;; ---------------------------------------------------------------------------
;;; Exception map tests
;;; ---------------------------------------------------------------------------

(test exception-map-exists
  "The exception map is a hash table with entries"
  (is (hash-table-p *engine-exception-map*))
  (is (> (hash-table-count *engine-exception-map*) 100)))

(test exception-map-known-codes
  "Known error codes map to correct condition classes"
  (is (eq 'sz-bad-input-error (lookup-exception-class 2)))
  (is (eq 'sz-retry-timeout-exceeded-error (lookup-exception-class 10)))
  (is (eq 'sz-not-found-error (lookup-exception-class 33)))
  (is (eq 'sz-not-initialized-error (lookup-exception-class 48)))
  (is (eq 'sz-license-error (lookup-exception-class 999)))
  (is (eq 'sz-database-error (lookup-exception-class 1000)))
  (is (eq 'sz-database-connection-lost-error (lookup-exception-class 1006)))
  (is (eq 'sz-database-transient-error (lookup-exception-class 1008)))
  (is (eq 'sz-unknown-data-source-error (lookup-exception-class 2207)))
  (is (eq 'sz-replace-conflict-error (lookup-exception-class 7245)))
  (is (eq 'sz-unhandled-error (lookup-exception-class 87))))

(test exception-map-unknown-code
  "Unknown error codes fall back to sz-error"
  (is (eq 'sz-error (lookup-exception-class 99999))))

(test condition-report-format
  "Conditions format their report correctly"
  (let ((c (make-condition 'sz-not-found-error
                           :error-code 33
                           :error-message "Unknown record")))
    (is (search "33" (format nil "~A" c)))
    (is (search "Unknown record" (format nil "~A" c)))))
