;;;; test-helpers.lisp — Tests for sz-sdk.helpers (pure Lisp, no database)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite helper-tests)

;;; ---------------------------------------------------------------------------
;;; build-entities-json tests
;;; ---------------------------------------------------------------------------

(test build-entities-json-basic
  "Single entity ID produces correct JSON"
  (let ((result (sz-sdk.helpers:build-entities-json '(42))))
    (is (stringp result))
    (is (search "ENTITIES" result))
    (is (search "ENTITY_ID" result))
    (is (search "42" result))))

(test build-entities-json-multiple
  "Multiple entity IDs produce correct JSON"
  (let ((result (sz-sdk.helpers:build-entities-json '(1 2 3))))
    (is (search "ENTITIES" result))
    (is (search "\"ENTITY_ID\":1" result))
    (is (search "\"ENTITY_ID\":2" result))
    (is (search "\"ENTITY_ID\":3" result))))

(test build-entities-json-empty
  "Nil returns empty JSON object"
  (is (string= "{}" (sz-sdk.helpers:build-entities-json nil))))

;;; ---------------------------------------------------------------------------
;;; build-records-json tests
;;; ---------------------------------------------------------------------------

(test build-records-json-basic
  "Single record key produces correct JSON"
  (let ((result (sz-sdk.helpers:build-records-json '(("CUSTOMERS" "1001")))))
    (is (stringp result))
    (is (search "RECORDS" result))
    (is (search "DATA_SOURCE" result))
    (is (search "CUSTOMERS" result))
    (is (search "RECORD_ID" result))
    (is (search "1001" result))))

(test build-records-json-multiple
  "Multiple record keys produce correct JSON"
  (let ((result (sz-sdk.helpers:build-records-json
                 '(("CUSTOMERS" "1001") ("REFERENCE" "2001")))))
    (is (search "RECORDS" result))
    (is (search "CUSTOMERS" result))
    (is (search "REFERENCE" result))
    (is (search "1001" result))
    (is (search "2001" result))))

(test build-records-json-empty
  "Nil returns empty JSON object"
  (is (string= "{}" (sz-sdk.helpers:build-records-json nil))))

;;; ---------------------------------------------------------------------------
;;; build-data-sources-json tests
;;; ---------------------------------------------------------------------------

(test build-data-sources-json-basic
  "Single data source produces correct JSON"
  (let ((result (sz-sdk.helpers:build-data-sources-json '("CUSTOMERS"))))
    (is (stringp result))
    (is (search "DATA_SOURCES" result))
    (is (search "CUSTOMERS" result))))

(test build-data-sources-json-multiple
  "Multiple data sources produce correct JSON"
  (let ((result (sz-sdk.helpers:build-data-sources-json '("CUSTOMERS" "REFERENCE"))))
    (is (search "DATA_SOURCES" result))
    (is (search "CUSTOMERS" result))
    (is (search "REFERENCE" result))))

(test build-data-sources-json-empty
  "Nil returns empty JSON object"
  (is (string= "{}" (sz-sdk.helpers:build-data-sources-json nil))))

;;; ---------------------------------------------------------------------------
;;; build-dsrc-code-json tests
;;; ---------------------------------------------------------------------------

(test build-dsrc-code-json-basic
  "Single data source code produces correct JSON"
  (let ((result (sz-sdk.helpers:build-dsrc-code-json "CUSTOMERS")))
    (is (stringp result))
    (is (search "DSRC_CODE" result))
    (is (search "CUSTOMERS" result))))

;;; ---------------------------------------------------------------------------
;;; check-result tests (unit tests with mock functions)
;;; ---------------------------------------------------------------------------

(test check-result-zero
  "Zero return code does not signal"
  (finishes
    (sz-sdk.helpers:check-result 0
                                 (lambda (buf size) (declare (ignore buf size)) 0)
                                 (lambda () nil)
                                 (lambda () 0))))

(test check-result-nonzero
  "Non-zero return code signals appropriate condition"
  (signals sz-error
    (sz-sdk.helpers:check-result -1
                                 (lambda (buf size)
                                   (declare (ignore buf size))
                                   0)
                                 (lambda () nil)
                                 (lambda () 33))))
