;;;; packages.lisp — Package definitions for sz-sdk
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:cl-user)

;;; ---------------------------------------------------------------------------
;;; Internal: Raw CFFI bindings
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk.bindings
  (:use #:cl #:cffi)
  (:export
   ;; Struct types (exported for type reference)
   #:sz-string-result
   #:sz-int64-result
   #:sz-handle-result

   ;; Free helper
   #:%sz-helper-free

   ;; Product bindings
   #:%sz-product-init
   #:%sz-product-destroy
   #:%sz-product-get-license
   #:%sz-product-get-version
   #:%sz-product-validate-license-file-helper
   #:%sz-product-validate-license-string-base64-helper
   #:%sz-product-get-last-exception
   #:%sz-product-clear-last-exception
   #:%sz-product-get-last-exception-code

   ;; Engine bindings
   #:%sz-init
   #:%sz-init-with-config-id
   #:%sz-destroy
   #:%sz-prime-engine
   #:%sz-reinit
   #:%sz-get-last-exception
   #:%sz-clear-last-exception
   #:%sz-get-last-exception-code
   #:%sz-add-record
   #:%sz-add-record-with-info-helper
   #:%sz-delete-record
   #:%sz-delete-record-with-info-helper
   #:%sz-get-entity-by-entity-id-v2-helper
   #:%sz-get-entity-by-record-id-v2-helper
   #:%sz-get-record-v2-helper
   #:%sz-get-record-preview-helper
   #:%sz-get-redo-record-helper
   #:%sz-get-virtual-entity-by-record-id-v2-helper
   #:%sz-search-by-attributes-v3-helper
   #:%sz-count-redo-records
   #:%sz-process-redo-record
   #:%sz-process-redo-record-with-info-helper
   #:%sz-reevaluate-entity
   #:%sz-reevaluate-entity-with-info-helper
   #:%sz-reevaluate-record
   #:%sz-reevaluate-record-with-info-helper
   #:%sz-export-csv-entity-report-helper
   #:%sz-export-json-entity-report-helper
   #:%sz-fetch-next-helper
   #:%sz-close-export-report-helper
   #:%sz-find-path-by-entity-id-v2-helper
   #:%sz-find-path-by-record-id-v2-helper
   #:%sz-find-path-by-entity-id-with-avoids-v2-helper
   #:%sz-find-path-by-record-id-with-avoids-v2-helper
   #:%sz-find-path-by-entity-id-including-source-v2-helper
   #:%sz-find-path-by-record-id-including-source-v2-helper
   #:%sz-find-network-by-entity-id-v2-helper
   #:%sz-find-network-by-record-id-v2-helper
   #:%sz-find-interesting-entities-by-entity-id-helper
   #:%sz-find-interesting-entities-by-record-id-helper
   #:%sz-why-entities-v2-helper
   #:%sz-why-records-v2-helper
   #:%sz-why-record-in-entity-v2-helper
   #:%sz-why-search-v2-helper
   #:%sz-how-entity-by-entity-id-v2-helper
   #:%sz-get-active-config-id-helper
   #:%sz-stats-helper

   ;; Config bindings
   #:%sz-config-init
   #:%sz-config-destroy
   #:%sz-config-create-helper
   #:%sz-config-load-helper
   #:%sz-config-export-helper
   #:%sz-config-close-helper
   #:%sz-config-get-data-source-registry-helper
   #:%sz-config-register-data-source-helper
   #:%sz-config-unregister-data-source-helper
   #:%sz-config-get-last-exception
   #:%sz-config-clear-last-exception
   #:%sz-config-get-last-exception-code

   ;; Config Manager bindings
   #:%sz-config-mgr-init
   #:%sz-config-mgr-destroy
   #:%sz-config-mgr-register-config-helper
   #:%sz-config-mgr-get-config-helper
   #:%sz-config-mgr-get-config-registry-helper
   #:%sz-config-mgr-get-default-config-id-helper
   #:%sz-config-mgr-set-default-config-id
   #:%sz-config-mgr-replace-default-config-id
   #:%sz-config-mgr-get-last-exception
   #:%sz-config-mgr-clear-last-exception
   #:%sz-config-mgr-get-last-exception-code

   ;; Diagnostic bindings
   #:%sz-diagnostic-init
   #:%sz-diagnostic-init-with-config-id
   #:%sz-diagnostic-destroy
   #:%sz-diagnostic-reinit
   #:%sz-diagnostic-purge-repository
   #:%sz-diagnostic-check-repository-performance-helper
   #:%sz-diagnostic-get-repository-info-helper
   #:%sz-diagnostic-get-feature-helper
   #:%sz-diagnostic-get-last-exception
   #:%sz-diagnostic-clear-last-exception
   #:%sz-diagnostic-get-last-exception-code))

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk.conditions
  (:use #:cl)
  (:export
   ;; Base condition
   #:sz-error
   #:sz-error-code
   #:sz-error-message
   ;; Category conditions
   #:sz-bad-input-error
   #:sz-general-error
   #:sz-retryable-error
   #:sz-unrecoverable-error
   ;; Detail conditions — bad input
   #:sz-not-found-error
   #:sz-unknown-data-source-error
   ;; Detail conditions — general
   #:sz-configuration-error
   #:sz-replace-conflict-error
   #:sz-sdk-error
   ;; Detail conditions — retryable
   #:sz-database-connection-lost-error
   #:sz-database-transient-error
   #:sz-retry-timeout-exceeded-error
   ;; Detail conditions — unrecoverable
   #:sz-database-error
   #:sz-license-error
   #:sz-not-initialized-error
   #:sz-unhandled-error
   ;; Error map
   #:*engine-exception-map*
   #:lookup-exception-class))

;;; ---------------------------------------------------------------------------
;;; Engine flags
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk.flags
  (:use #:cl)
  (:export
   ;; Base
   #:+sz-no-flags+
   ;; Feature inclusion
   #:+sz-include-feature-scores+
   #:+sz-include-match-key-details+
   ;; Export
   #:+sz-export-include-multi-record-entities+
   #:+sz-export-include-possibly-same+
   #:+sz-export-include-possibly-related+
   #:+sz-export-include-name-only+
   #:+sz-export-include-disclosed+
   #:+sz-export-include-single-record-entities+
   #:+sz-export-include-all-entities+
   #:+sz-export-include-all-having-relationships+
   ;; Entity relations
   #:+sz-entity-include-possibly-same-relations+
   #:+sz-entity-include-possibly-related-relations+
   #:+sz-entity-include-name-only-relations+
   #:+sz-entity-include-disclosed-relations+
   #:+sz-entity-include-all-relations+
   ;; Entity features
   #:+sz-entity-include-all-features+
   #:+sz-entity-include-representative-features+
   ;; Entity info
   #:+sz-entity-include-entity-name+
   #:+sz-entity-include-record-summary+
   #:+sz-entity-include-record-types+
   #:+sz-entity-include-record-data+
   #:+sz-entity-include-record-matching-info+
   #:+sz-entity-include-record-dates+
   #:+sz-entity-include-record-json-data+
   #:+sz-entity-include-record-unmapped-data+
   #:+sz-entity-include-record-features+
   #:+sz-entity-include-record-feature-details+
   #:+sz-entity-include-record-feature-stats+
   #:+sz-entity-include-related-entity-name+
   #:+sz-entity-include-related-matching-info+
   #:+sz-entity-include-related-record-summary+
   #:+sz-entity-include-related-record-types+
   #:+sz-entity-include-related-record-data+
   ;; Internal/stats
   #:+sz-entity-include-internal-features+
   #:+sz-entity-include-feature-stats+
   ;; Path/network
   #:+sz-find-path-strict-avoid+
   #:+sz-find-path-include-matching-info+
   #:+sz-find-network-include-matching-info+
   ;; Search
   #:+sz-search-include-stats+
   #:+sz-search-include-resolved+
   #:+sz-search-include-possibly-same+
   #:+sz-search-include-possibly-related+
   #:+sz-search-include-name-only+
   #:+sz-search-include-all-entities+
   #:+sz-search-include-all-candidates+
   #:+sz-search-include-request+
   #:+sz-search-include-request-details+
   ;; Recommended defaults
   #:+sz-record-default-flags+
   #:+sz-entity-core-flags+
   #:+sz-entity-default-flags+
   #:+sz-entity-brief-default-flags+
   #:+sz-export-default-flags+
   #:+sz-find-path-default-flags+
   #:+sz-find-network-default-flags+
   #:+sz-why-entities-default-flags+
   #:+sz-why-records-default-flags+
   #:+sz-why-record-in-entity-default-flags+
   #:+sz-why-search-default-flags+
   #:+sz-how-entity-default-flags+
   #:+sz-virtual-entity-default-flags+
   #:+sz-add-record-default-flags+
   #:+sz-delete-record-default-flags+
   #:+sz-record-preview-default-flags+
   #:+sz-redo-default-flags+
   #:+sz-reevaluate-record-default-flags+
   #:+sz-reevaluate-entity-default-flags+
   #:+sz-find-interesting-entities-default-flags+
   #:+sz-search-by-attributes-all+
   #:+sz-search-by-attributes-strong+
   #:+sz-search-by-attributes-minimal-all+
   #:+sz-search-by-attributes-minimal-strong+
   #:+sz-search-by-attributes-default-flags+
   ;; With-info flag
   #:+sz-with-info+))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk.constants
  (:use #:cl)
  (:export
   #:+sz-without-info+
   #:+sz-initialize-with-default-configuration+
   #:+sz-no-attributes+
   #:+sz-no-avoidances+
   #:+sz-no-info+
   #:+sz-no-logging+
   #:+sz-no-required-datasources+
   #:+sz-no-search-profile+
   #:+sz-verbose-logging+))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk.helpers
  (:use #:cl #:cffi #:sz-sdk.bindings #:sz-sdk.conditions)
  (:export
   #:check-result
   #:with-sz-string-result
   #:with-sz-int64-result
   #:with-sz-handle-result
   #:check-not-destroyed
   #:build-entities-json
   #:build-records-json
   #:build-data-sources-json
   #:build-dsrc-code-json))

;;; ---------------------------------------------------------------------------
;;; Component packages
;;; ---------------------------------------------------------------------------

;;; Design note: Each component uses defgeneric/defmethod rather than plain
;;; defun.  This is intentional — the upstream Senzing SDK defines abstract
;;; interfaces (SzEngineAbstract, SzProductAbstract, etc.) to support
;;; pluggable backends (e.g. gRPC transport, mock implementations for
;;; testing).  CLOS generic functions are the natural CL equivalent,
;;; allowing future specializations without changing the public API.
;;;
;;; FUTURE WORK: If alternative backends are never needed, the generics
;;; could be collapsed to plain defun for simplicity and reduced dispatch
;;; overhead.

(defpackage #:sz-sdk.product
  (:use #:cl #:sz-sdk.bindings #:sz-sdk.conditions #:sz-sdk.helpers)
  (:export
   #:sz-product
   #:get-license
   #:get-version
   #:initialize-product
   #:destroy-product))

(defpackage #:sz-sdk.engine
  (:use #:cl #:sz-sdk.bindings #:sz-sdk.conditions #:sz-sdk.helpers #:sz-sdk.flags)
  (:export
   #:sz-engine
   #:initialize-engine
   #:destroy-engine
   #:prime-engine
   #:reinitialize-engine
   #:add-record
   #:delete-record
   #:get-entity-by-entity-id
   #:get-entity-by-record-id
   #:get-record
   #:get-record-preview
   #:get-redo-record
   #:get-virtual-entity-by-record-id
   #:search-by-attributes
   #:count-redo-records
   #:process-redo-record
   #:reevaluate-entity
   #:reevaluate-record
   #:export-csv-entity-report
   #:export-json-entity-report
   #:fetch-next
   #:close-export-report
   #:find-path-by-entity-id
   #:find-path-by-record-id
   #:find-network-by-entity-id
   #:find-network-by-record-id
   #:find-interesting-entities-by-entity-id
   #:find-interesting-entities-by-record-id
   #:why-entities
   #:why-records
   #:why-record-in-entity
   #:why-search
   #:how-entity-by-entity-id
   #:get-active-config-id
   #:get-stats))

(defpackage #:sz-sdk.config
  (:use #:cl #:sz-sdk.bindings #:sz-sdk.conditions #:sz-sdk.helpers)
  (:export
   #:sz-config
   #:sz-config-config-definition
   #:initialize-config
   #:destroy-config
   #:config-export
   #:import-config-definition
   #:import-template
   #:verify-config-definition
   #:get-data-source-registry
   #:register-data-source
   #:unregister-data-source))

(defpackage #:sz-sdk.config-manager
  (:use #:cl #:sz-sdk.bindings #:sz-sdk.conditions #:sz-sdk.helpers)
  (:export
   #:sz-config-manager
   #:initialize-config-manager
   #:destroy-config-manager
   #:create-config-from-config-id
   #:create-config-from-string
   #:create-config-from-template
   #:get-config-registry
   #:get-default-config-id
   #:register-config
   #:replace-default-config-id
   #:set-default-config
   #:set-default-config-id))

(defpackage #:sz-sdk.diagnostic
  (:use #:cl #:sz-sdk.bindings #:sz-sdk.conditions #:sz-sdk.helpers)
  (:export
   #:sz-diagnostic
   #:initialize-diagnostic
   #:destroy-diagnostic
   #:reinitialize-diagnostic
   #:check-repository-performance
   #:get-repository-info
   #:get-feature
   #:purge-repository))

(defpackage #:sz-sdk.factory
  (:use #:cl)
  (:export
   #:sz-abstract-factory
   #:create-engine
   #:create-product
   #:create-config-manager
   #:create-diagnostic
   #:destroy-factory
   #:reinitialize-factory
   #:with-sz-factory))

;;; ---------------------------------------------------------------------------
;;; Umbrella package — re-exports the public API
;;;
;;; Uses all component packages so their exported symbols are accessible,
;;; then :export controls which symbols form the public API. Lifecycle
;;; methods (initialize-*, destroy-*) are deliberately excluded — users
;;; should use the factory pattern (with-sz-factory / create-*) instead.
;;;
;;; FUTURE WORK: Consider uiop:define-package with :reexport to reduce
;;; this to a single symbol listing per package.
;;; ---------------------------------------------------------------------------

(defpackage #:sz-sdk
  (:use #:cl
        #:sz-sdk.conditions
        #:sz-sdk.flags
        #:sz-sdk.constants
        #:sz-sdk.product
        #:sz-sdk.engine
        #:sz-sdk.config
        #:sz-sdk.config-manager
        #:sz-sdk.diagnostic
        #:sz-sdk.factory)
  (:export
   ;; Conditions
   #:sz-error #:sz-error-code #:sz-error-message
   #:sz-bad-input-error #:sz-general-error
   #:sz-retryable-error #:sz-unrecoverable-error
   #:sz-not-found-error #:sz-unknown-data-source-error
   #:sz-configuration-error #:sz-replace-conflict-error #:sz-sdk-error
   #:sz-database-connection-lost-error #:sz-database-transient-error
   #:sz-retry-timeout-exceeded-error
   #:sz-database-error #:sz-license-error
   #:sz-not-initialized-error #:sz-unhandled-error
   ;; Flags — base
   #:+sz-no-flags+
   ;; Flags — feature inclusion
   #:+sz-include-feature-scores+ #:+sz-include-match-key-details+
   ;; Flags — export
   #:+sz-export-include-multi-record-entities+
   #:+sz-export-include-possibly-same+
   #:+sz-export-include-possibly-related+
   #:+sz-export-include-name-only+
   #:+sz-export-include-disclosed+
   #:+sz-export-include-single-record-entities+
   #:+sz-export-include-all-entities+
   #:+sz-export-include-all-having-relationships+
   ;; Flags — entity relations
   #:+sz-entity-include-possibly-same-relations+
   #:+sz-entity-include-possibly-related-relations+
   #:+sz-entity-include-name-only-relations+
   #:+sz-entity-include-disclosed-relations+
   #:+sz-entity-include-all-relations+
   ;; Flags — entity features
   #:+sz-entity-include-all-features+
   #:+sz-entity-include-representative-features+
   ;; Flags — entity info
   #:+sz-entity-include-entity-name+
   #:+sz-entity-include-record-summary+
   #:+sz-entity-include-record-types+
   #:+sz-entity-include-record-data+
   #:+sz-entity-include-record-matching-info+
   #:+sz-entity-include-record-dates+
   #:+sz-entity-include-record-json-data+
   #:+sz-entity-include-record-unmapped-data+
   #:+sz-entity-include-record-features+
   #:+sz-entity-include-record-feature-details+
   #:+sz-entity-include-record-feature-stats+
   #:+sz-entity-include-related-entity-name+
   #:+sz-entity-include-related-matching-info+
   #:+sz-entity-include-related-record-summary+
   #:+sz-entity-include-related-record-types+
   #:+sz-entity-include-related-record-data+
   ;; Flags — internal/stats
   #:+sz-entity-include-internal-features+
   #:+sz-entity-include-feature-stats+
   ;; Flags — path/network
   #:+sz-find-path-strict-avoid+
   #:+sz-find-path-include-matching-info+
   #:+sz-find-network-include-matching-info+
   ;; Flags — search
   #:+sz-search-include-stats+
   #:+sz-search-include-resolved+
   #:+sz-search-include-possibly-same+
   #:+sz-search-include-possibly-related+
   #:+sz-search-include-name-only+
   #:+sz-search-include-all-entities+
   #:+sz-search-include-all-candidates+
   #:+sz-search-include-request+
   #:+sz-search-include-request-details+
   ;; Flags — recommended defaults
   #:+sz-record-default-flags+
   #:+sz-entity-core-flags+
   #:+sz-entity-default-flags+
   #:+sz-entity-brief-default-flags+
   #:+sz-export-default-flags+
   #:+sz-find-path-default-flags+
   #:+sz-find-network-default-flags+
   #:+sz-why-entities-default-flags+
   #:+sz-why-records-default-flags+
   #:+sz-why-record-in-entity-default-flags+
   #:+sz-why-search-default-flags+
   #:+sz-how-entity-default-flags+
   #:+sz-virtual-entity-default-flags+
   #:+sz-add-record-default-flags+
   #:+sz-delete-record-default-flags+
   #:+sz-record-preview-default-flags+
   #:+sz-redo-default-flags+
   #:+sz-reevaluate-record-default-flags+
   #:+sz-reevaluate-entity-default-flags+
   #:+sz-find-interesting-entities-default-flags+
   #:+sz-search-by-attributes-all+
   #:+sz-search-by-attributes-strong+
   #:+sz-search-by-attributes-minimal-all+
   #:+sz-search-by-attributes-minimal-strong+
   #:+sz-search-by-attributes-default-flags+
   ;; Flags — with-info
   #:+sz-with-info+
   ;; Constants
   #:+sz-without-info+
   #:+sz-initialize-with-default-configuration+
   #:+sz-no-attributes+
   #:+sz-no-avoidances+
   #:+sz-no-info+
   #:+sz-no-logging+
   #:+sz-no-required-datasources+
   #:+sz-no-search-profile+
   #:+sz-verbose-logging+
   ;; Product
   #:sz-product #:get-license #:get-version
   ;; Engine
   #:sz-engine #:add-record #:delete-record
   #:get-entity-by-entity-id #:get-entity-by-record-id
   #:get-record #:get-record-preview #:get-redo-record
   #:search-by-attributes #:get-virtual-entity-by-record-id
   #:count-redo-records #:process-redo-record
   #:reevaluate-entity #:reevaluate-record
   #:export-csv-entity-report #:export-json-entity-report
   #:fetch-next #:close-export-report
   #:find-path-by-entity-id #:find-path-by-record-id
   #:find-network-by-entity-id #:find-network-by-record-id
   #:find-interesting-entities-by-entity-id
   #:find-interesting-entities-by-record-id
   #:why-entities #:why-records #:why-record-in-entity
   #:why-search #:how-entity-by-entity-id
   #:get-active-config-id #:get-stats #:prime-engine
   ;; Config
   #:sz-config #:config-export
   #:get-data-source-registry #:register-data-source #:unregister-data-source
   ;; Config Manager
   #:sz-config-manager
   #:create-config-from-config-id #:create-config-from-string
   #:create-config-from-template
   #:get-config-registry #:get-default-config-id
   #:register-config #:replace-default-config-id
   #:set-default-config #:set-default-config-id
   ;; Diagnostic
   #:sz-diagnostic
   #:check-repository-performance #:get-repository-info
   #:get-feature #:purge-repository
   ;; Factory
   #:sz-abstract-factory
   #:create-engine #:create-product
   #:create-config-manager #:create-diagnostic
   #:destroy-factory #:reinitialize-factory
   #:with-sz-factory))
