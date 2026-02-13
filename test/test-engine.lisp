;;;; test-engine.lisp — Tests for sz-engine (requires Senzing installed)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite engine-tests)

;;; ---------------------------------------------------------------------------
;;; Setup: register data sources and load truthset
;;; Tests run in definition order due to :serial t in ASDF.
;;; ---------------------------------------------------------------------------

(test engine-setup-datasources
  "Register truthset data sources in config"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (finishes (setup-truthset-datasources mgr))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test engine-load-truthset
  "Load all truthset records into the engine"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (finishes (add-truthset-records engine))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Record operations
;;; ---------------------------------------------------------------------------

(test engine-add-record
  "add-record returns empty string on success"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:add-record
                        engine "CUSTOMERS" "9001"
                        "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"9001\",\"PRIMARY_NAME_LAST\":\"Test\",\"PRIMARY_NAME_FIRST\":\"Record\"}")))
           (is (stringp result))
           (is (string= "" result))
           ;; Clean up the test record
           (sz-sdk.engine:delete-record engine "CUSTOMERS" "9001"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-add-record-with-info
  "add-record with SZ_WITH_INFO returns JSON info"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:add-record
                        engine "CUSTOMERS" "9002"
                        "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"9002\",\"PRIMARY_NAME_LAST\":\"Info\",\"PRIMARY_NAME_FIRST\":\"Test\"}"
                        :flags (logior +sz-add-record-default-flags+ +sz-with-info+))))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "DATA_SOURCE" result))
           (is (search "RECORD_ID" result))
           ;; Clean up
           (sz-sdk.engine:delete-record engine "CUSTOMERS" "9002"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-delete-record
  "delete-record returns empty string on success"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (progn
           (sz-sdk.engine:add-record
            engine "CUSTOMERS" "9003"
            "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"9003\",\"PRIMARY_NAME_LAST\":\"Delete\",\"PRIMARY_NAME_FIRST\":\"Test\"}")
           (let ((result (sz-sdk.engine:delete-record engine "CUSTOMERS" "9003")))
             (is (stringp result))
             (is (string= "" result))))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-delete-record-with-info
  "delete-record with SZ_WITH_INFO returns JSON info"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (progn
           (sz-sdk.engine:add-record
            engine "CUSTOMERS" "9004"
            "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"9004\",\"PRIMARY_NAME_LAST\":\"DelInfo\",\"PRIMARY_NAME_FIRST\":\"Test\"}")
           (let ((result (sz-sdk.engine:delete-record
                          engine "CUSTOMERS" "9004"
                          :flags (logior +sz-delete-record-default-flags+ +sz-with-info+))))
             (is (stringp result))
             (is (plusp (length result)))
             (is (search "DATA_SOURCE" result))))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-add-record-bad-data-source
  "add-record with bad data source signals sz-unknown-data-source-error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-unknown-data-source-error)
           (sz-sdk.engine:add-record engine "BOGUS_SOURCE" "1"
                                     "{\"DATA_SOURCE\":\"BOGUS_SOURCE\",\"RECORD_ID\":\"1\"}"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-delete-record-bad-data-source
  "delete-record with bad data source signals sz-error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:delete-record engine "BOGUS_SOURCE" "1"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-delete-record-bad-record-id
  "delete-record with non-existent record completes without error (idempotent)"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:delete-record engine "CUSTOMERS" "999999999")))
           (is (stringp result))
           (is (string= "" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-add-record-bad-record
  "add-record with invalid JSON signals sz-error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:add-record engine "CUSTOMERS" "9999"
                                     "THIS IS NOT JSON"))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Entity retrieval
;;; ---------------------------------------------------------------------------

(test engine-get-entity-by-entity-id
  "get-entity-by-entity-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id (get-entity-id-from-record-id
                            engine "CUSTOMERS" "1001"))
                (result (sz-sdk.engine:get-entity-by-entity-id engine entity-id)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "RESOLVED_ENTITY" result))
           (is (search "ENTITY_ID" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-entity-by-record-id
  "get-entity-by-record-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:get-entity-by-record-id
                        engine "CUSTOMERS" "1001")))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "RESOLVED_ENTITY" result))
           (is (search "ENTITY_ID" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-entity-by-record-id-bad-data-source
  "get-entity-by-record-id with bad data source signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:get-entity-by-record-id engine "BOGUS_SOURCE" "1"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-entity-by-record-id-bad-record-id
  "get-entity-by-record-id with bad record ID signals sz-not-found-error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-not-found-error)
           (sz-sdk.engine:get-entity-by-record-id engine "CUSTOMERS" "999999999"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-record
  "get-record returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:get-record engine "CUSTOMERS" "1001")))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "DATA_SOURCE" result))
           (is (search "RECORD_ID" result))
           (is (search "JSON_DATA" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-record-bad-data-source
  "get-record with bad data source signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:get-record engine "BOGUS_SOURCE" "1"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-record-bad-record-id
  "get-record with bad record ID signals sz-not-found-error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-not-found-error)
           (sz-sdk.engine:get-record engine "CUSTOMERS" "999999999"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-record-preview
  "get-record-preview returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:get-record-preview
                        engine
                        "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"9999\",\"PRIMARY_NAME_LAST\":\"Preview\",\"PRIMARY_NAME_FIRST\":\"Test\"}")))
           (is (stringp result))
           (is (plusp (length result))))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Search
;;; ---------------------------------------------------------------------------

(test engine-search-by-attributes
  "search-by-attributes returns JSON with results"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:search-by-attributes
                        engine
                        "{\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\"}")))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "RESOLVED_ENTITIES" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-search-by-attributes-bad-attributes
  "search-by-attributes with bad JSON signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:search-by-attributes engine "NOT VALID JSON"))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Redo queue
;;; ---------------------------------------------------------------------------

(test engine-count-redo-records
  "count-redo-records returns non-negative integer"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((count (sz-sdk.engine:count-redo-records engine)))
           (is (integerp count))
           (is (>= count 0)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-process-redo-record
  "process-redo-record processes without error when redo exists"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((count (sz-sdk.engine:count-redo-records engine)))
           (when (plusp count)
             (let ((redo (sz-sdk.engine:get-redo-record engine)))
               (finishes
                 (sz-sdk.engine:process-redo-record engine redo)))))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Reevaluation
;;; ---------------------------------------------------------------------------

(test engine-reevaluate-entity
  "reevaluate-entity completes without error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((entity-id (get-entity-id-from-record-id
                           engine "CUSTOMERS" "1001")))
           (finishes
             (sz-sdk.engine:reevaluate-entity engine entity-id)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-reevaluate-entity-with-info
  "reevaluate-entity with SZ_WITH_INFO returns JSON or empty string"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id (get-entity-id-from-record-id
                            engine "CUSTOMERS" "1001"))
                (result (sz-sdk.engine:reevaluate-entity
                         engine entity-id
                         :flags (logior +sz-reevaluate-entity-default-flags+
                                        +sz-with-info+))))
           (is (stringp result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-reevaluate-record
  "reevaluate-record completes without error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (finishes
           (sz-sdk.engine:reevaluate-record engine "CUSTOMERS" "1001"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-reevaluate-record-with-info
  "reevaluate-record with SZ_WITH_INFO returns JSON or empty string"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:reevaluate-record
                        engine "CUSTOMERS" "1001"
                        :flags (logior +sz-reevaluate-record-default-flags+
                                       +sz-with-info+))))
           (is (stringp result)))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Export
;;; ---------------------------------------------------------------------------

(test engine-export-json-entity-report
  "JSON entity report export: open, fetch, close"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((handle (sz-sdk.engine:export-json-entity-report engine)))
           (is (plusp handle))
           ;; Fetch at least one chunk
           (let ((chunk (sz-sdk.engine:fetch-next engine handle)))
             (is (stringp chunk)))
           ;; Close the export
           (finishes (sz-sdk.engine:close-export-report engine handle)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-export-csv-entity-report
  "CSV entity report export: open, fetch, close"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((handle (sz-sdk.engine:export-csv-entity-report
                        engine "RESOLVED_ENTITY_ID,RESOLVED_ENTITY_NAME")))
           (is (plusp handle))
           ;; Fetch at least one chunk
           (let ((chunk (sz-sdk.engine:fetch-next engine handle)))
             (is (stringp chunk)))
           ;; Close the export
           (finishes (sz-sdk.engine:close-export-report engine handle)))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Path finding
;;; ---------------------------------------------------------------------------

(test engine-find-path-by-entity-id
  "find-path-by-entity-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id-1 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1001"))
                (entity-id-2 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1005"))
                (result (sz-sdk.engine:find-path-by-entity-id
                         engine entity-id-1 entity-id-2 10)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "ENTITY_PATHS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-find-path-by-entity-id-bad-entity-ids
  "find-path-by-entity-id with bad entity IDs signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:find-path-by-entity-id
            engine 0 99999999 3))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-find-path-by-record-id
  "find-path-by-record-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:find-path-by-record-id
                        engine
                        "CUSTOMERS" "1001"
                        "CUSTOMERS" "1005"
                        10)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "ENTITY_PATHS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-find-path-by-record-id-bad-data-source
  "find-path-by-record-id with bad data source signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:find-path-by-record-id
            engine "BOGUS_SOURCE" "1" "CUSTOMERS" "1001" 3))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Network finding
;;; ---------------------------------------------------------------------------

(test engine-find-network-by-entity-id
  "find-network-by-entity-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id-1 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1001"))
                (entity-id-2 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1005"))
                (result (sz-sdk.engine:find-network-by-entity-id
                         engine (list entity-id-1 entity-id-2)
                         10 3 1000)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "ENTITY_PATHS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-find-network-by-record-id
  "find-network-by-record-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:find-network-by-record-id
                        engine '(("CUSTOMERS" "1001") ("CUSTOMERS" "1005"))
                        10 3 1000)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "ENTITY_PATHS" result)))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Why / How analysis
;;; ---------------------------------------------------------------------------

(test engine-why-entities
  "why-entities returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id-1 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1001"))
                (entity-id-2 (get-entity-id-from-record-id
                              engine "CUSTOMERS" "1005"))
                (result (sz-sdk.engine:why-entities
                         engine entity-id-1 entity-id-2)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "WHY_RESULTS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-why-entities-bad-entity-ids
  "why-entities with bad entity IDs signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:why-entities engine 0 99999999))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-why-records
  "why-records returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:why-records
                        engine "CUSTOMERS" "1001" "CUSTOMERS" "1002")))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "WHY_RESULTS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-why-records-bad-data-source
  "why-records with bad data source signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:why-records
            engine "BOGUS_SOURCE" "1" "CUSTOMERS" "1001"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-why-record-in-entity
  "why-record-in-entity returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:why-record-in-entity
                        engine "CUSTOMERS" "1001")))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "WHY_RESULTS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-why-record-in-entity-bad-data-source
  "why-record-in-entity with bad data source signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:why-record-in-entity engine "BOGUS_SOURCE" "1"))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-how-entity-by-entity-id
  "how-entity-by-entity-id returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let* ((entity-id (get-entity-id-from-record-id
                            engine "CUSTOMERS" "1001"))
                (result (sz-sdk.engine:how-entity-by-entity-id
                         engine entity-id)))
           (is (stringp result))
           (is (plusp (length result)))
           (is (search "HOW_RESULTS" result)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-how-entity-by-entity-id-bad-entity-id
  "how-entity-by-entity-id with bad entity ID signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:how-entity-by-entity-id engine 0))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Statistics / Config
;;; ---------------------------------------------------------------------------

(test engine-get-active-config-id
  "get-active-config-id returns positive integer"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
           (is (integerp config-id))
           (is (plusp config-id)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-get-stats
  "get-stats returns valid JSON"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((result (sz-sdk.engine:get-stats engine)))
           (is (stringp result))
           (is (plusp (length result))))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-prime-engine
  "prime-engine completes without error"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (finishes (sz-sdk.engine:prime-engine engine))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(test engine-reinitialize
  "reinitialize-engine with current config ID succeeds"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (let ((config-id (sz-sdk.engine:get-active-config-id engine)))
           (finishes (sz-sdk.engine:reinitialize-engine engine config-id)))
      (sz-sdk.engine:destroy-engine engine))))

(test engine-reinitialize-bad-config-id
  "reinitialize-engine with bad config ID signals condition"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.engine:reinitialize-engine engine 999999999))
      (sz-sdk.engine:destroy-engine engine))))

;;; ---------------------------------------------------------------------------
;;; Cleanup: delete all truthset records
;;; ---------------------------------------------------------------------------

(test engine-cleanup-truthset
  "Delete all truthset records to clean up"
  (let ((engine (make-test-engine)))
    (unwind-protect
         (finishes (delete-truthset-records engine))
      (sz-sdk.engine:destroy-engine engine))))
