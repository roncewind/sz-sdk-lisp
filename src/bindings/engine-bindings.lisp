;;;; engine-bindings.lisp — CFFI bindings for Sz (engine) functions
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_init" %sz-init) :int64
  (instance-name :string)
  (settings :string)
  (verbose-logging :int64))

(defcfun ("Sz_initWithConfigID" %sz-init-with-config-id) :int64
  (instance-name :string)
  (settings :string)
  (config-id :int64)
  (verbose-logging :int64))

(defcfun ("Sz_destroy" %sz-destroy) :int64)

(defcfun ("Sz_primeEngine" %sz-prime-engine) :int64)

(defcfun ("Sz_reinit" %sz-reinit) :int64
  (config-id :int64))

;;; ---------------------------------------------------------------------------
;;; Exception handling
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_getLastException" %sz-get-last-exception) :int64
  (buf :pointer)
  (buf-size :size))

(defcfun ("Sz_clearLastException" %sz-clear-last-exception) :void)

(defcfun ("Sz_getLastExceptionCode" %sz-get-last-exception-code) :int64)

;;; ---------------------------------------------------------------------------
;;; Record operations
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_addRecord" %sz-add-record) :int64
  (data-source-code :string)
  (record-id :string)
  (json-data :string))

(defcfun ("Sz_addRecordWithInfo_helper" %sz-add-record-with-info-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (json-data :string)
  (flags :int64))

(defcfun ("Sz_deleteRecord" %sz-delete-record) :int64
  (data-source-code :string)
  (record-id :string))

(defcfun ("Sz_deleteRecordWithInfo_helper" %sz-delete-record-with-info-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Entity retrieval
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_getEntityByEntityID_V2_helper" %sz-get-entity-by-entity-id-v2-helper)
    (:struct sz-string-result)
  (entity-id :int64)
  (flags :int64))

(defcfun ("Sz_getEntityByRecordID_V2_helper" %sz-get-entity-by-record-id-v2-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

(defcfun ("Sz_getRecord_V2_helper" %sz-get-record-v2-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

(defcfun ("Sz_getRecordPreview_helper" %sz-get-record-preview-helper)
    (:struct sz-string-result)
  (json-data :string)
  (flags :int64))

(defcfun ("Sz_getRedoRecord_helper" %sz-get-redo-record-helper)
    (:struct sz-string-result))

(defcfun ("Sz_getVirtualEntityByRecordID_V2_helper"
          %sz-get-virtual-entity-by-record-id-v2-helper)
    (:struct sz-string-result)
  (record-list :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Search
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_searchByAttributes_V3_helper" %sz-search-by-attributes-v3-helper)
    (:struct sz-string-result)
  (json-data :string)
  (profile :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Redo queue
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_countRedoRecords" %sz-count-redo-records) :int64)

(defcfun ("Sz_processRedoRecord" %sz-process-redo-record) :int64
  (json-data :string))

(defcfun ("Sz_processRedoRecordWithInfo_helper"
          %sz-process-redo-record-with-info-helper)
    (:struct sz-string-result)
  (json-data :string))

;;; ---------------------------------------------------------------------------
;;; Reevaluation
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_reevaluateEntity" %sz-reevaluate-entity) :int64
  (entity-id :int64)
  (flags :int64))

(defcfun ("Sz_reevaluateEntityWithInfo_helper"
          %sz-reevaluate-entity-with-info-helper)
    (:struct sz-string-result)
  (entity-id :int64)
  (flags :int64))

(defcfun ("Sz_reevaluateRecord" %sz-reevaluate-record) :int64
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

(defcfun ("Sz_reevaluateRecordWithInfo_helper"
          %sz-reevaluate-record-with-info-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Export
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_exportCSVEntityReport_helper" %sz-export-csv-entity-report-helper)
    (:struct sz-handle-result)
  (csv-column-list :string)
  (flags :int64))

(defcfun ("Sz_exportJSONEntityReport_helper" %sz-export-json-entity-report-helper)
    (:struct sz-handle-result)
  (flags :int64))

(defcfun ("Sz_fetchNext_helper" %sz-fetch-next-helper)
    (:struct sz-string-result)
  (export-handle :uintptr))

(defcfun ("Sz_closeExportReport_helper" %sz-close-export-report-helper) :int64
  (export-handle :uintptr))

;;; ---------------------------------------------------------------------------
;;; Path finding
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_findPathByEntityID_V2_helper" %sz-find-path-by-entity-id-v2-helper)
    (:struct sz-string-result)
  (entity-id-1 :int64)
  (entity-id-2 :int64)
  (max-degree :int64)
  (flags :int64))

(defcfun ("Sz_findPathByRecordID_V2_helper" %sz-find-path-by-record-id-v2-helper)
    (:struct sz-string-result)
  (data-source-code-1 :string)
  (record-id-1 :string)
  (data-source-code-2 :string)
  (record-id-2 :string)
  (max-degree :int64)
  (flags :int64))

(defcfun ("Sz_findPathByEntityIDWithAvoids_V2_helper"
          %sz-find-path-by-entity-id-with-avoids-v2-helper)
    (:struct sz-string-result)
  (entity-id-1 :int64)
  (entity-id-2 :int64)
  (max-degree :int64)
  (avoided-entities :string)
  (flags :int64))

(defcfun ("Sz_findPathByRecordIDWithAvoids_V2_helper"
          %sz-find-path-by-record-id-with-avoids-v2-helper)
    (:struct sz-string-result)
  (data-source-code-1 :string)
  (record-id-1 :string)
  (data-source-code-2 :string)
  (record-id-2 :string)
  (max-degree :int64)
  (avoided-records :string)
  (flags :int64))

(defcfun ("Sz_findPathByEntityIDIncludingSource_V2_helper"
          %sz-find-path-by-entity-id-including-source-v2-helper)
    (:struct sz-string-result)
  (entity-id-1 :int64)
  (entity-id-2 :int64)
  (max-degree :int64)
  (avoided-entities :string)
  (required-dsrcs :string)
  (flags :int64))

(defcfun ("Sz_findPathByRecordIDIncludingSource_V2_helper"
          %sz-find-path-by-record-id-including-source-v2-helper)
    (:struct sz-string-result)
  (data-source-code-1 :string)
  (record-id-1 :string)
  (data-source-code-2 :string)
  (record-id-2 :string)
  (max-degree :int64)
  (avoided-records :string)
  (required-dsrcs :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Network finding
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_findNetworkByEntityID_V2_helper"
          %sz-find-network-by-entity-id-v2-helper)
    (:struct sz-string-result)
  (entity-list :string)
  (max-degree :int64)
  (build-out-degree :int64)
  (max-entities :int64)
  (flags :int64))

(defcfun ("Sz_findNetworkByRecordID_V2_helper"
          %sz-find-network-by-record-id-v2-helper)
    (:struct sz-string-result)
  (record-list :string)
  (max-degree :int64)
  (build-out-degree :int64)
  (max-entities :int64)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Interesting entities
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_findInterestingEntitiesByEntityID_helper"
          %sz-find-interesting-entities-by-entity-id-helper)
    (:struct sz-string-result)
  (entity-id :int64)
  (flags :int64))

(defcfun ("Sz_findInterestingEntitiesByRecordID_helper"
          %sz-find-interesting-entities-by-record-id-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Why / How analysis
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_whyEntities_V2_helper" %sz-why-entities-v2-helper)
    (:struct sz-string-result)
  (entity-id-1 :int64)
  (entity-id-2 :int64)
  (flags :int64))

(defcfun ("Sz_whyRecords_V2_helper" %sz-why-records-v2-helper)
    (:struct sz-string-result)
  (data-source-code-1 :string)
  (record-id-1 :string)
  (data-source-code-2 :string)
  (record-id-2 :string)
  (flags :int64))

(defcfun ("Sz_whyRecordInEntity_V2_helper" %sz-why-record-in-entity-v2-helper)
    (:struct sz-string-result)
  (data-source-code :string)
  (record-id :string)
  (flags :int64))

(defcfun ("Sz_whySearch_V2_helper" %sz-why-search-v2-helper)
    (:struct sz-string-result)
  (json-data :string)
  (entity-id :int64)
  (search-profile :string)
  (flags :int64))

(defcfun ("Sz_howEntityByEntityID_V2_helper" %sz-how-entity-by-entity-id-v2-helper)
    (:struct sz-string-result)
  (entity-id :int64)
  (flags :int64))

;;; ---------------------------------------------------------------------------
;;; Statistics / Config
;;; ---------------------------------------------------------------------------

(defcfun ("Sz_getActiveConfigID_helper" %sz-get-active-config-id-helper)
    (:struct sz-int64-result))

(defcfun ("Sz_stats_helper" %sz-stats-helper)
    (:struct sz-string-result))
