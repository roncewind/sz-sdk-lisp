;;;; engine.lisp — sz-engine CLOS class and methods
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.engine)

;;; ---------------------------------------------------------------------------
;;; Class
;;; ---------------------------------------------------------------------------

(defclass sz-engine ()
  ((destroyedp
    :initform nil
    :accessor sz-engine-destroyed-p))
  (:documentation "Senzing entity resolution engine interface."))

;;; ---------------------------------------------------------------------------
;;; Per-component exception function bindings
;;; ---------------------------------------------------------------------------

(defvar *get-exc-fn* #'%sz-get-last-exception
  "Function to retrieve the last engine exception message.")
(defvar *clear-exc-fn* #'%sz-clear-last-exception
  "Function to clear the last engine exception.")
(defvar *get-code-fn* #'%sz-get-last-exception-code
  "Function to retrieve the last engine exception code.")

;;; Mask for removing SDK-specific flags (SZ_WITH_INFO) before passing to C
(defun %strip-sdk-flags (flags)
  (logand flags (lognot +sz-with-info+)))

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------

(defgeneric initialize-engine (engine instance-name settings
                               &key config-id verbose-logging)
  (:documentation "Initialize the Senzing engine."))

(defmethod initialize-engine ((engine sz-engine) instance-name settings
                              &key (config-id 0) (verbose-logging 0))
  (let ((rc (if (zerop config-id)
                (%sz-init instance-name settings verbose-logging)
                (%sz-init-with-config-id instance-name settings
                                         config-id verbose-logging))))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric destroy-engine (engine)
  (:documentation "Destroy the Senzing engine."))

(defmethod destroy-engine ((engine sz-engine))
  (unless (sz-engine-destroyed-p engine)
    (%sz-destroy)
    (setf (sz-engine-destroyed-p engine) t))
  (values))

(defgeneric prime-engine (engine)
  (:documentation "Prime the engine for faster initial operations."))

(defmethod prime-engine ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (let ((rc (%sz-prime-engine)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

(defgeneric reinitialize-engine (engine config-id)
  (:documentation "Reinitialize the engine with a new configuration ID."))

(defmethod reinitialize-engine ((engine sz-engine) config-id)
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (let ((rc (%sz-reinit config-id)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

;;; ---------------------------------------------------------------------------
;;; Record operations
;;; ---------------------------------------------------------------------------

(defgeneric add-record (engine data-source-code record-id record-definition
                        &key flags)
  (:documentation "Add a record. Returns with-info JSON if SZ_WITH_INFO is set, else empty string."))

(defmethod add-record ((engine sz-engine) data-source-code record-id
                       record-definition
                       &key (flags +sz-add-record-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      (with-sz-string-result (result
                              (%sz-add-record-with-info-helper
                               data-source-code record-id record-definition
                               (%strip-sdk-flags flags))
                              *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        result)
      (let ((rc (%sz-add-record data-source-code record-id record-definition)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        "")))

(defgeneric delete-record (engine data-source-code record-id &key flags)
  (:documentation "Delete a record. Returns with-info JSON if SZ_WITH_INFO is set."))

(defmethod delete-record ((engine sz-engine) data-source-code record-id
                          &key (flags +sz-delete-record-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      (with-sz-string-result (result
                              (%sz-delete-record-with-info-helper
                               data-source-code record-id
                               (%strip-sdk-flags flags))
                              *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        result)
      (let ((rc (%sz-delete-record data-source-code record-id)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        "")))

;;; ---------------------------------------------------------------------------
;;; Entity retrieval
;;; ---------------------------------------------------------------------------

(defgeneric get-entity-by-entity-id (engine entity-id &key flags)
  (:documentation "Get entity data by entity ID. Returns JSON string."))

(defmethod get-entity-by-entity-id ((engine sz-engine) entity-id
                                    &key (flags +sz-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-entity-by-entity-id-v2-helper entity-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-entity-by-record-id (engine data-source-code record-id &key flags)
  (:documentation "Get entity data by record ID. Returns JSON string."))

(defmethod get-entity-by-record-id ((engine sz-engine) data-source-code record-id
                                    &key (flags +sz-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-entity-by-record-id-v2-helper
                           data-source-code record-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-record (engine data-source-code record-id &key flags)
  (:documentation "Get a specific record. Returns JSON string."))

(defmethod get-record ((engine sz-engine) data-source-code record-id
                       &key (flags +sz-record-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-record-v2-helper
                           data-source-code record-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-record-preview (engine record-definition &key flags)
  (:documentation "Preview how a record would be processed. Returns JSON string."))

(defmethod get-record-preview ((engine sz-engine) record-definition
                               &key (flags +sz-record-preview-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-record-preview-helper record-definition flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-redo-record (engine)
  (:documentation "Get the next redo record. Returns JSON string."))

(defmethod get-redo-record ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-redo-record-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-virtual-entity-by-record-id (engine record-keys &key flags)
  (:documentation "Get a virtual entity composed of the given records.
   RECORD-KEYS is a list of (data-source-code record-id) pairs."))

(defmethod get-virtual-entity-by-record-id ((engine sz-engine) record-keys
                                            &key (flags +sz-virtual-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-get-virtual-entity-by-record-id-v2-helper
                           (build-records-json record-keys) flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

;;; ---------------------------------------------------------------------------
;;; Search
;;; ---------------------------------------------------------------------------

(defgeneric search-by-attributes (engine attributes &key flags search-profile)
  (:documentation "Search for entities matching the given attributes JSON."))

(defmethod search-by-attributes ((engine sz-engine) attributes
                                 &key (flags +sz-search-by-attributes-default-flags+)
                                      (search-profile ""))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-search-by-attributes-v3-helper
                           attributes search-profile flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

;;; ---------------------------------------------------------------------------
;;; Redo queue
;;; ---------------------------------------------------------------------------

(defgeneric count-redo-records (engine)
  (:documentation "Return the number of redo records in the queue."))

(defmethod count-redo-records ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (let ((result (%sz-count-redo-records)))
    (when (minusp result)
      (check-result result *get-exc-fn* *clear-exc-fn* *get-code-fn*))
    result))

(defgeneric process-redo-record (engine redo-record &key flags)
  (:documentation "Process a redo record. Returns with-info JSON if SZ_WITH_INFO is set."))

(defmethod process-redo-record ((engine sz-engine) redo-record
                                &key (flags +sz-redo-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      (with-sz-string-result (result
                              (%sz-process-redo-record-with-info-helper redo-record)
                              *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        result)
      (let ((rc (%sz-process-redo-record redo-record)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        "")))

;;; ---------------------------------------------------------------------------
;;; Reevaluation
;;; ---------------------------------------------------------------------------

(defgeneric reevaluate-entity (engine entity-id &key flags)
  (:documentation "Reevaluate an entity. Returns with-info JSON if SZ_WITH_INFO is set."))

(defmethod reevaluate-entity ((engine sz-engine) entity-id
                              &key (flags +sz-reevaluate-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      (with-sz-string-result (result
                              (%sz-reevaluate-entity-with-info-helper
                               entity-id (%strip-sdk-flags flags))
                              *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        result)
      (let ((rc (%sz-reevaluate-entity entity-id (%strip-sdk-flags flags))))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        "")))

(defgeneric reevaluate-record (engine data-source-code record-id &key flags)
  (:documentation "Reevaluate a record. Returns with-info JSON if SZ_WITH_INFO is set."))

(defmethod reevaluate-record ((engine sz-engine) data-source-code record-id
                              &key (flags +sz-reevaluate-record-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      (with-sz-string-result (result
                              (%sz-reevaluate-record-with-info-helper
                               data-source-code record-id
                               (%strip-sdk-flags flags))
                              *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        result)
      (let ((rc (%sz-reevaluate-record data-source-code record-id
                                       (%strip-sdk-flags flags))))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
        "")))

;;; ---------------------------------------------------------------------------
;;; Export
;;; ---------------------------------------------------------------------------

(defgeneric export-csv-entity-report (engine csv-column-list &key flags)
  (:documentation "Start a CSV entity report export. Returns an export handle."))

(defmethod export-csv-entity-report ((engine sz-engine) csv-column-list
                                     &key (flags +sz-export-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-handle-result (handle
                          (%sz-export-csv-entity-report-helper
                           csv-column-list flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    handle))

(defgeneric export-json-entity-report (engine &key flags)
  (:documentation "Start a JSON entity report export. Returns an export handle."))

(defmethod export-json-entity-report ((engine sz-engine)
                                      &key (flags +sz-export-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-handle-result (handle
                          (%sz-export-json-entity-report-helper flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    handle))

(defgeneric fetch-next (engine export-handle)
  (:documentation "Fetch the next chunk of an export report. Returns a string."))

(defmethod fetch-next ((engine sz-engine) export-handle)
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-fetch-next-helper export-handle)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric close-export-report (engine export-handle)
  (:documentation "Close an export report handle."))

(defmethod close-export-report ((engine sz-engine) export-handle)
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (let ((rc (%sz-close-export-report-helper export-handle)))
    (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*))
  (values))

;;; ---------------------------------------------------------------------------
;;; Path finding
;;; ---------------------------------------------------------------------------

(defgeneric find-path-by-entity-id (engine start-entity-id end-entity-id
                                    max-degrees
                                    &key avoid-entity-ids required-data-sources flags)
  (:documentation "Find a path between two entities by entity ID."))

(defmethod find-path-by-entity-id ((engine sz-engine) start-entity-id end-entity-id
                                   max-degrees
                                   &key avoid-entity-ids required-data-sources
                                        (flags +sz-find-path-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (cond
    ;; With avoids and required data sources
    (required-data-sources
     (with-sz-string-result (result
                             (%sz-find-path-by-entity-id-including-source-v2-helper
                              start-entity-id end-entity-id max-degrees
                              (build-entities-json avoid-entity-ids)
                              (build-data-sources-json required-data-sources)
                              flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))
    ;; With avoids only
    (avoid-entity-ids
     (with-sz-string-result (result
                             (%sz-find-path-by-entity-id-with-avoids-v2-helper
                              start-entity-id end-entity-id max-degrees
                              (build-entities-json avoid-entity-ids)
                              flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))
    ;; Simple path
    (t
     (with-sz-string-result (result
                             (%sz-find-path-by-entity-id-v2-helper
                              start-entity-id end-entity-id max-degrees flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))))

(defgeneric find-path-by-record-id (engine
                                    start-data-source-code start-record-id
                                    end-data-source-code end-record-id
                                    max-degrees
                                    &key avoid-record-keys required-data-sources flags)
  (:documentation "Find a path between two entities by record ID.
   AVOID-RECORD-KEYS is a list of (data-source-code record-id) pairs."))

(defmethod find-path-by-record-id ((engine sz-engine)
                                   start-data-source-code start-record-id
                                   end-data-source-code end-record-id
                                   max-degrees
                                   &key avoid-record-keys required-data-sources
                                        (flags +sz-find-path-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (cond
    (required-data-sources
     (with-sz-string-result (result
                             (%sz-find-path-by-record-id-including-source-v2-helper
                              start-data-source-code start-record-id
                              end-data-source-code end-record-id
                              max-degrees
                              (build-records-json avoid-record-keys)
                              (build-data-sources-json required-data-sources)
                              flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))
    (avoid-record-keys
     (with-sz-string-result (result
                             (%sz-find-path-by-record-id-with-avoids-v2-helper
                              start-data-source-code start-record-id
                              end-data-source-code end-record-id
                              max-degrees
                              (build-records-json avoid-record-keys)
                              flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))
    (t
     (with-sz-string-result (result
                             (%sz-find-path-by-record-id-v2-helper
                              start-data-source-code start-record-id
                              end-data-source-code end-record-id
                              max-degrees flags)
                             *get-exc-fn* *clear-exc-fn* *get-code-fn*)
       result))))

;;; ---------------------------------------------------------------------------
;;; Network finding
;;; ---------------------------------------------------------------------------

(defgeneric find-network-by-entity-id (engine entity-ids max-degrees
                                       build-out-degrees build-out-max-entities
                                       &key flags)
  (:documentation "Find a network among a set of entities by entity ID.
   ENTITY-IDS is a list of integer entity IDs."))

(defmethod find-network-by-entity-id ((engine sz-engine) entity-ids max-degrees
                                      build-out-degrees build-out-max-entities
                                      &key (flags +sz-find-network-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-find-network-by-entity-id-v2-helper
                           (build-entities-json entity-ids)
                           max-degrees build-out-degrees build-out-max-entities
                           flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric find-network-by-record-id (engine record-keys max-degrees
                                       build-out-degrees build-out-max-entities
                                       &key flags)
  (:documentation "Find a network among a set of entities by record ID.
   RECORD-KEYS is a list of (data-source-code record-id) pairs."))

(defmethod find-network-by-record-id ((engine sz-engine) record-keys max-degrees
                                      build-out-degrees build-out-max-entities
                                      &key (flags +sz-find-network-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-find-network-by-record-id-v2-helper
                           (build-records-json record-keys)
                           max-degrees build-out-degrees build-out-max-entities
                           flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

;;; ---------------------------------------------------------------------------
;;; Interesting entities
;;; ---------------------------------------------------------------------------

(defgeneric find-interesting-entities-by-entity-id (engine entity-id &key flags)
  (:documentation "Find interesting entities by entity ID."))

(defmethod find-interesting-entities-by-entity-id ((engine sz-engine) entity-id
                                                   &key (flags +sz-find-interesting-entities-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-find-interesting-entities-by-entity-id-helper
                           entity-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric find-interesting-entities-by-record-id (engine data-source-code record-id
                                                    &key flags)
  (:documentation "Find interesting entities by record ID."))

(defmethod find-interesting-entities-by-record-id ((engine sz-engine) data-source-code
                                                   record-id
                                                   &key (flags +sz-find-interesting-entities-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-find-interesting-entities-by-record-id-helper
                           data-source-code record-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

;;; ---------------------------------------------------------------------------
;;; Why / How analysis
;;; ---------------------------------------------------------------------------

(defgeneric why-entities (engine entity-id-1 entity-id-2 &key flags)
  (:documentation "Determine why two entities resolved or related."))

(defmethod why-entities ((engine sz-engine) entity-id-1 entity-id-2
                         &key (flags +sz-why-entities-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-why-entities-v2-helper
                           entity-id-1 entity-id-2 flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric why-records (engine data-source-code-1 record-id-1
                         data-source-code-2 record-id-2 &key flags)
  (:documentation "Determine why two records resolved or related."))

(defmethod why-records ((engine sz-engine) data-source-code-1 record-id-1
                        data-source-code-2 record-id-2
                        &key (flags +sz-why-records-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-why-records-v2-helper
                           data-source-code-1 record-id-1
                           data-source-code-2 record-id-2
                           flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric why-record-in-entity (engine data-source-code record-id &key flags)
  (:documentation "Determine why a record is in its resolved entity."))

(defmethod why-record-in-entity ((engine sz-engine) data-source-code record-id
                                 &key (flags +sz-why-record-in-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-why-record-in-entity-v2-helper
                           data-source-code record-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric why-search (engine attributes entity-id &key flags search-profile)
  (:documentation "Determine why an entity matches a search."))

(defmethod why-search ((engine sz-engine) attributes entity-id
                       &key (flags +sz-why-search-default-flags+)
                            (search-profile ""))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-why-search-v2-helper
                           attributes entity-id search-profile flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric how-entity-by-entity-id (engine entity-id &key flags)
  (:documentation "Determine how an entity was constructed."))

(defmethod how-entity-by-entity-id ((engine sz-engine) entity-id
                                    &key (flags +sz-how-entity-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-how-entity-by-entity-id-v2-helper entity-id flags)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

;;; ---------------------------------------------------------------------------
;;; Statistics / Config
;;; ---------------------------------------------------------------------------

(defgeneric get-active-config-id (engine)
  (:documentation "Return the active configuration ID."))

(defmethod get-active-config-id ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-int64-result (result
                         (%sz-get-active-config-id-helper)
                         *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))

(defgeneric get-stats (engine)
  (:documentation "Return engine statistics as a JSON string."))

(defmethod get-stats ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-stats-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))
