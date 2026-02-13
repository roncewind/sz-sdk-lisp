;;;; flags.lisp — Engine flags as constants
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0
;;;
;;; Mirrors szengineflags.py exactly.

(in-package #:sz-sdk.flags)

;;; All flag constants are unsigned 64-bit integers (highest bit used: 62).
(declaim (type (unsigned-byte 64)
               +sz-no-flags+
               +sz-include-feature-scores+ +sz-include-match-key-details+
               +sz-export-include-multi-record-entities+
               +sz-export-include-possibly-same+
               +sz-export-include-possibly-related+
               +sz-export-include-name-only+
               +sz-export-include-disclosed+
               +sz-export-include-single-record-entities+
               +sz-export-include-all-entities+
               +sz-export-include-all-having-relationships+
               +sz-entity-include-possibly-same-relations+
               +sz-entity-include-possibly-related-relations+
               +sz-entity-include-name-only-relations+
               +sz-entity-include-disclosed-relations+
               +sz-entity-include-all-relations+
               +sz-entity-include-all-features+
               +sz-entity-include-representative-features+
               +sz-entity-include-entity-name+
               +sz-entity-include-record-summary+
               +sz-entity-include-record-types+
               +sz-entity-include-record-data+
               +sz-entity-include-record-matching-info+
               +sz-entity-include-record-dates+
               +sz-entity-include-record-json-data+
               +sz-entity-include-record-unmapped-data+
               +sz-entity-include-record-features+
               +sz-entity-include-record-feature-details+
               +sz-entity-include-record-feature-stats+
               +sz-entity-include-related-entity-name+
               +sz-entity-include-related-matching-info+
               +sz-entity-include-related-record-summary+
               +sz-entity-include-related-record-types+
               +sz-entity-include-related-record-data+
               +sz-entity-include-internal-features+
               +sz-entity-include-feature-stats+
               +sz-find-path-strict-avoid+
               +sz-find-path-include-matching-info+
               +sz-find-network-include-matching-info+
               +sz-search-include-stats+
               +sz-search-include-resolved+
               +sz-search-include-possibly-same+
               +sz-search-include-possibly-related+
               +sz-search-include-name-only+
               +sz-search-include-all-entities+
               +sz-search-include-all-candidates+
               +sz-search-include-request+
               +sz-search-include-request-details+
               +sz-record-default-flags+
               +sz-entity-core-flags+
               +sz-entity-default-flags+
               +sz-entity-brief-default-flags+
               +sz-export-default-flags+
               +sz-find-path-default-flags+
               +sz-find-network-default-flags+
               +sz-why-entities-default-flags+
               +sz-why-records-default-flags+
               +sz-why-record-in-entity-default-flags+
               +sz-why-search-default-flags+
               +sz-how-entity-default-flags+
               +sz-virtual-entity-default-flags+
               +sz-add-record-default-flags+
               +sz-delete-record-default-flags+
               +sz-record-preview-default-flags+
               +sz-redo-default-flags+
               +sz-reevaluate-record-default-flags+
               +sz-reevaluate-entity-default-flags+
               +sz-find-interesting-entities-default-flags+
               +sz-search-by-attributes-all+
               +sz-search-by-attributes-strong+
               +sz-search-by-attributes-minimal-all+
               +sz-search-by-attributes-minimal-strong+
               +sz-search-by-attributes-default-flags+
               +sz-with-info+))

;;; Base

(defconstant +sz-no-flags+ 0)

;;; Feature inclusion

(defconstant +sz-include-feature-scores+    (ash 1 26))
(defconstant +sz-include-match-key-details+ (ash 1 34))

;;; Export flags

(defconstant +sz-export-include-multi-record-entities+ (ash 1 0))
(defconstant +sz-export-include-possibly-same+         (ash 1 1))
(defconstant +sz-export-include-possibly-related+      (ash 1 2))
(defconstant +sz-export-include-name-only+             (ash 1 3))
(defconstant +sz-export-include-disclosed+             (ash 1 4))
(defconstant +sz-export-include-single-record-entities+ (ash 1 5))

(defconstant +sz-export-include-all-entities+
  (logior +sz-export-include-multi-record-entities+
          +sz-export-include-single-record-entities+))

(defconstant +sz-export-include-all-having-relationships+
  (logior +sz-export-include-possibly-same+
          +sz-export-include-possibly-related+
          +sz-export-include-name-only+
          +sz-export-include-disclosed+))

;;; Entity relation flags

(defconstant +sz-entity-include-possibly-same-relations+    (ash 1 6))
(defconstant +sz-entity-include-possibly-related-relations+ (ash 1 7))
(defconstant +sz-entity-include-name-only-relations+        (ash 1 8))
(defconstant +sz-entity-include-disclosed-relations+        (ash 1 9))

(defconstant +sz-entity-include-all-relations+
  (logior +sz-entity-include-possibly-same-relations+
          +sz-entity-include-possibly-related-relations+
          +sz-entity-include-name-only-relations+
          +sz-entity-include-disclosed-relations+))

;;; Entity feature flags

(defconstant +sz-entity-include-all-features+            (ash 1 10))
(defconstant +sz-entity-include-representative-features+ (ash 1 11))

;;; Entity info flags

(defconstant +sz-entity-include-entity-name+          (ash 1 12))
(defconstant +sz-entity-include-record-summary+       (ash 1 13))
(defconstant +sz-entity-include-record-types+         (ash 1 28))
(defconstant +sz-entity-include-record-data+          (ash 1 14))
(defconstant +sz-entity-include-record-matching-info+ (ash 1 15))
(defconstant +sz-entity-include-record-dates+         (ash 1 39))
(defconstant +sz-entity-include-record-json-data+     (ash 1 16))
(defconstant +sz-entity-include-record-unmapped-data+ (ash 1 31))
(defconstant +sz-entity-include-record-features+      (ash 1 18))
(defconstant +sz-entity-include-record-feature-details+ (ash 1 35))
(defconstant +sz-entity-include-record-feature-stats+   (ash 1 36))
(defconstant +sz-entity-include-related-entity-name+    (ash 1 19))
(defconstant +sz-entity-include-related-matching-info+  (ash 1 20))
(defconstant +sz-entity-include-related-record-summary+ (ash 1 21))
(defconstant +sz-entity-include-related-record-types+   (ash 1 29))
(defconstant +sz-entity-include-related-record-data+    (ash 1 22))

;;; Extra feature data

(defconstant +sz-entity-include-internal-features+ (ash 1 23))
(defconstant +sz-entity-include-feature-stats+     (ash 1 24))

;;; Path / network flags

(defconstant +sz-find-path-strict-avoid+             (ash 1 25))
(defconstant +sz-find-path-include-matching-info+    (ash 1 30))
(defconstant +sz-find-network-include-matching-info+ (ash 1 33))

;;; Search flags

(defconstant +sz-search-include-stats+ (ash 1 27))

(defconstant +sz-search-include-resolved+
  +sz-export-include-multi-record-entities+)
(defconstant +sz-search-include-possibly-same+
  +sz-export-include-possibly-same+)
(defconstant +sz-search-include-possibly-related+
  +sz-export-include-possibly-related+)
(defconstant +sz-search-include-name-only+
  +sz-export-include-name-only+)

(defconstant +sz-search-include-all-entities+
  (logior +sz-search-include-resolved+
          +sz-search-include-possibly-same+
          +sz-search-include-possibly-related+
          +sz-search-include-name-only+))

(defconstant +sz-search-include-all-candidates+ (ash 1 32))
(defconstant +sz-search-include-request+         (ash 1 37))
(defconstant +sz-search-include-request-details+ (ash 1 38))

;;; ---------------------------------------------------------------------------
;;; Recommended default flag combinations
;;; ---------------------------------------------------------------------------

(defconstant +sz-record-default-flags+
  +sz-entity-include-record-json-data+)

(defconstant +sz-entity-core-flags+
  (logior +sz-entity-include-representative-features+
          +sz-entity-include-entity-name+
          +sz-entity-include-record-summary+
          +sz-entity-include-record-data+
          +sz-entity-include-record-matching-info+))

(defconstant +sz-entity-default-flags+
  (logior +sz-entity-core-flags+
          +sz-entity-include-all-relations+
          +sz-entity-include-related-entity-name+
          +sz-entity-include-related-record-summary+
          +sz-entity-include-related-matching-info+))

(defconstant +sz-entity-brief-default-flags+
  (logior +sz-entity-include-record-matching-info+
          +sz-entity-include-all-relations+
          +sz-entity-include-related-matching-info+))

(defconstant +sz-export-default-flags+
  (logior +sz-export-include-all-entities+
          +sz-entity-default-flags+))

(defconstant +sz-find-path-default-flags+
  (logior +sz-find-path-include-matching-info+
          +sz-entity-include-entity-name+
          +sz-entity-include-record-summary+))

(defconstant +sz-find-network-default-flags+
  (logior +sz-find-network-include-matching-info+
          +sz-entity-include-entity-name+
          +sz-entity-include-record-summary+))

(defconstant +sz-why-entities-default-flags+
  +sz-include-feature-scores+)

(defconstant +sz-why-records-default-flags+
  +sz-include-feature-scores+)

(defconstant +sz-why-record-in-entity-default-flags+
  +sz-include-feature-scores+)

(defconstant +sz-why-search-default-flags+
  (logior +sz-include-feature-scores+
          +sz-search-include-request-details+
          +sz-search-include-stats+))

(defconstant +sz-how-entity-default-flags+
  +sz-include-feature-scores+)

(defconstant +sz-virtual-entity-default-flags+
  +sz-entity-core-flags+)

(defconstant +sz-add-record-default-flags+ +sz-no-flags+)
(defconstant +sz-delete-record-default-flags+ +sz-no-flags+)

(defconstant +sz-record-preview-default-flags+
  +sz-entity-include-record-feature-details+)

(defconstant +sz-redo-default-flags+ +sz-no-flags+)
(defconstant +sz-reevaluate-record-default-flags+ +sz-no-flags+)
(defconstant +sz-reevaluate-entity-default-flags+ +sz-reevaluate-record-default-flags+)

(defconstant +sz-find-interesting-entities-default-flags+ +sz-no-flags+)

(defconstant +sz-search-by-attributes-all+
  (logior +sz-search-include-all-entities+
          +sz-entity-include-representative-features+
          +sz-entity-include-entity-name+
          +sz-entity-include-record-summary+
          +sz-include-feature-scores+
          +sz-search-include-stats+))

(defconstant +sz-search-by-attributes-strong+
  (logior +sz-search-include-resolved+
          +sz-search-include-possibly-same+
          +sz-entity-include-representative-features+
          +sz-entity-include-entity-name+
          +sz-entity-include-record-summary+
          +sz-include-feature-scores+
          +sz-search-include-stats+))

(defconstant +sz-search-by-attributes-minimal-all+
  (logior +sz-search-include-all-entities+
          +sz-search-include-stats+))

(defconstant +sz-search-by-attributes-minimal-strong+
  (logior +sz-search-include-resolved+
          +sz-search-include-possibly-same+
          +sz-search-include-stats+))

(defconstant +sz-search-by-attributes-default-flags+
  +sz-search-by-attributes-all+)

;;; With-info flag (bit 62)

(defconstant +sz-with-info+ (ash 1 62))
