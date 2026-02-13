;;;; test-flags.lisp — Tests for engine flags
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite flag-tests)

;;; ---------------------------------------------------------------------------
;;; Basic flag value tests
;;; ---------------------------------------------------------------------------

(test flag-no-flags
  "SZ_NO_FLAGS is zero"
  (is (zerop +sz-no-flags+)))

(test flag-bit-positions
  "Individual flags are at the correct bit positions"
  (is (= (ash 1 0) +sz-export-include-multi-record-entities+))
  (is (= (ash 1 1) +sz-export-include-possibly-same+))
  (is (= (ash 1 2) +sz-export-include-possibly-related+))
  (is (= (ash 1 3) +sz-export-include-name-only+))
  (is (= (ash 1 4) +sz-export-include-disclosed+))
  (is (= (ash 1 5) +sz-export-include-single-record-entities+))
  (is (= (ash 1 6) +sz-entity-include-possibly-same-relations+))
  (is (= (ash 1 10) +sz-entity-include-all-features+))
  (is (= (ash 1 11) +sz-entity-include-representative-features+))
  (is (= (ash 1 12) +sz-entity-include-entity-name+))
  (is (= (ash 1 16) +sz-entity-include-record-json-data+))
  (is (= (ash 1 25) +sz-find-path-strict-avoid+))
  (is (= (ash 1 26) +sz-include-feature-scores+))
  (is (= (ash 1 34) +sz-include-match-key-details+))
  (is (= (ash 1 62) +sz-with-info+)))

;;; ---------------------------------------------------------------------------
;;; Composite flag tests
;;; ---------------------------------------------------------------------------

(test flag-all-entities-composite
  "SZ_EXPORT_INCLUDE_ALL_ENTITIES is multi + single"
  (is (= (logior +sz-export-include-multi-record-entities+
                 +sz-export-include-single-record-entities+)
         +sz-export-include-all-entities+)))

(test flag-all-relations-composite
  "SZ_ENTITY_INCLUDE_ALL_RELATIONS is all four relation types"
  (is (= (logior +sz-entity-include-possibly-same-relations+
                 +sz-entity-include-possibly-related-relations+
                 +sz-entity-include-name-only-relations+
                 +sz-entity-include-disclosed-relations+)
         +sz-entity-include-all-relations+)))

(test flag-entity-core-composite
  "SZ_ENTITY_CORE_FLAGS contains the expected flags"
  (is (not (zerop (logand +sz-entity-core-flags+
                          +sz-entity-include-representative-features+))))
  (is (not (zerop (logand +sz-entity-core-flags+
                          +sz-entity-include-entity-name+))))
  (is (not (zerop (logand +sz-entity-core-flags+
                          +sz-entity-include-record-summary+))))
  (is (not (zerop (logand +sz-entity-core-flags+
                          +sz-entity-include-record-data+))))
  (is (not (zerop (logand +sz-entity-core-flags+
                          +sz-entity-include-record-matching-info+)))))

(test flag-entity-default-includes-core
  "SZ_ENTITY_DEFAULT_FLAGS includes core flags"
  (is (= +sz-entity-core-flags+
         (logand +sz-entity-default-flags+ +sz-entity-core-flags+))))

(test flag-search-aliases
  "Search include flags alias export flags correctly"
  (is (= +sz-search-include-resolved+ +sz-export-include-multi-record-entities+))
  (is (= +sz-search-include-possibly-same+ +sz-export-include-possibly-same+))
  (is (= +sz-search-include-possibly-related+ +sz-export-include-possibly-related+))
  (is (= +sz-search-include-name-only+ +sz-export-include-name-only+)))

(test flag-with-info-is-bit-62
  "SZ_WITH_INFO is bit 62"
  (is (= (ash 1 62) +sz-with-info+))
  ;; Can be combined with other flags
  (let ((combined (logior +sz-add-record-default-flags+ +sz-with-info+)))
    (is (not (zerop (logand combined +sz-with-info+))))))

(test flag-record-default
  "SZ_RECORD_DEFAULT_FLAGS is record JSON data"
  (is (= +sz-entity-include-record-json-data+ +sz-record-default-flags+)))

(test flag-why-defaults-include-scores
  "Why defaults include feature scores"
  (is (not (zerop (logand +sz-why-entities-default-flags+ +sz-include-feature-scores+))))
  (is (not (zerop (logand +sz-why-records-default-flags+ +sz-include-feature-scores+))))
  (is (not (zerop (logand +sz-why-record-in-entity-default-flags+ +sz-include-feature-scores+))))
  (is (not (zerop (logand +sz-how-entity-default-flags+ +sz-include-feature-scores+)))))

;;; ---------------------------------------------------------------------------
;;; Constants tests
;;; ---------------------------------------------------------------------------

(test constants-values
  "Convenience constants have correct values"
  (is (zerop +sz-without-info+))
  (is (zerop +sz-initialize-with-default-configuration+))
  (is (string= "" +sz-no-attributes+))
  (is (string= "" +sz-no-avoidances+))
  (is (string= "" +sz-no-info+))
  (is (zerop +sz-no-logging+))
  (is (string= "" +sz-no-required-datasources+))
  (is (string= "" +sz-no-search-profile+))
  (is (= 1 +sz-verbose-logging+)))
