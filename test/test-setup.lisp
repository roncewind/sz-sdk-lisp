;;;; test-setup.lisp — Test infrastructure: settings, truthset data, helpers
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

;;; ---------------------------------------------------------------------------
;;; Test configuration (matches Python conftest.py)
;;; ---------------------------------------------------------------------------

(defparameter *test-instance-name* "Testing")

(defparameter *test-settings*
  (format nil "~A"
          (concatenate 'string
                       "{\"PIPELINE\":{"
                       "\"CONFIGPATH\":\"/etc/opt/senzing\","
                       "\"RESOURCEPATH\":\"/opt/senzing/er/resources\","
                       "\"SUPPORTPATH\":\"/opt/senzing/data\""
                       "},"
                       "\"SQL\":{"
                       "\"CONNECTION\":\"sqlite3://na:na@/tmp/sqlite/G2C.db\""
                       "}}"))
  "Senzing engine settings JSON matching Python conftest.py.")

;;; ---------------------------------------------------------------------------
;;; Truthset data sources
;;; ---------------------------------------------------------------------------

(defparameter *truthset-datasources*
  '("CUSTOMERS" "REFERENCE" "WATCHLIST"))

;;; ---------------------------------------------------------------------------
;;; Truthset records — CUSTOMERS
;;; ---------------------------------------------------------------------------

(defparameter *truthset-customer-records*
  '((:data-source "CUSTOMERS" :id "1001"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1001\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\",\"DATE_OF_BIRTH\":\"12/11/1978\",\"ADDR_TYPE\":\"MAILING\",\"ADDR_LINE1\":\"123 Main Street, Las Vegas NV 89132\",\"PHONE_TYPE\":\"HOME\",\"PHONE_NUMBER\":\"702-919-1300\",\"EMAIL_ADDRESS\":\"bsmith@work.com\"}")
    (:data-source "CUSTOMERS" :id "1002"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1002\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Bob\",\"DATE_OF_BIRTH\":\"11/12/1978\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"1515 Adela Lane, Las Vegas NV 89111\",\"PHONE_TYPE\":\"MOBILE\",\"PHONE_NUMBER\":\"702-919-1300\",\"EMAIL_ADDRESS\":\"bsmith@work.com\"}")
    (:data-source "CUSTOMERS" :id "1003"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1003\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Bob\",\"PRIMARY_NAME_MIDDLE\":\"J\",\"DATE_OF_BIRTH\":\"12/11/1978\",\"EMAIL_ADDRESS\":\"bsmith@work.com\"}")
    (:data-source "CUSTOMERS" :id "1004"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1004\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"B\",\"DATE_OF_BIRTH\":\"12/11/1978\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"123 Main Street, Las Vegas NV 89132\",\"EMAIL_ADDRESS\":\"bsmith@work.com\"}")
    (:data-source "CUSTOMERS" :id "1005"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1005\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Jones\",\"PRIMARY_NAME_FIRST\":\"Margaret\",\"DATE_OF_BIRTH\":\"05/03/1975\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"500 Oak Blvd, Henderson NV 89015\",\"PHONE_TYPE\":\"HOME\",\"PHONE_NUMBER\":\"702-555-1234\"}")
    (:data-source "CUSTOMERS" :id "1006"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1006\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Jones\",\"PRIMARY_NAME_FIRST\":\"Maggie\",\"DATE_OF_BIRTH\":\"03/05/1975\",\"ADDR_TYPE\":\"MAILING\",\"ADDR_LINE1\":\"500 Oak Boulevard, Henderson NV 89015\"}")
    (:data-source "CUSTOMERS" :id "1007"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1007\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Williams\",\"PRIMARY_NAME_FIRST\":\"John\",\"DATE_OF_BIRTH\":\"06/15/1980\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"200 Elm Ave, Las Vegas NV 89101\",\"PHONE_TYPE\":\"HOME\",\"PHONE_NUMBER\":\"702-555-5678\"}")
    (:data-source "CUSTOMERS" :id "1008"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1008\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Davis\",\"PRIMARY_NAME_FIRST\":\"Jennifer\",\"DATE_OF_BIRTH\":\"09/22/1985\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"750 Pine St, Henderson NV 89052\",\"EMAIL_ADDRESS\":\"jdavis@email.com\"}")
    (:data-source "CUSTOMERS" :id "1009"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1009\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Garcia\",\"PRIMARY_NAME_FIRST\":\"Carlos\",\"DATE_OF_BIRTH\":\"03/17/1990\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"300 Maple Dr, Las Vegas NV 89109\",\"PHONE_TYPE\":\"MOBILE\",\"PHONE_NUMBER\":\"702-555-9999\"}")
    (:data-source "CUSTOMERS" :id "1010"
     :json "{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1010\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Brown\",\"PRIMARY_NAME_FIRST\":\"Lisa\",\"DATE_OF_BIRTH\":\"07/04/1982\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"425 Cherry Lane, Las Vegas NV 89145\",\"EMAIL_ADDRESS\":\"lbrown@email.com\"}")))

;;; ---------------------------------------------------------------------------
;;; Truthset records — REFERENCE
;;; ---------------------------------------------------------------------------

(defparameter *truthset-reference-records*
  '((:data-source "REFERENCE" :id "2001"
     :json "{\"DATA_SOURCE\":\"REFERENCE\",\"RECORD_ID\":\"2001\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\",\"DATE_OF_BIRTH\":\"12/11/1978\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"123 Main Street, Las Vegas NV 89132\",\"PHONE_TYPE\":\"HOME\",\"PHONE_NUMBER\":\"702-919-1300\"}")
    (:data-source "REFERENCE" :id "2002"
     :json "{\"DATA_SOURCE\":\"REFERENCE\",\"RECORD_ID\":\"2002\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Williams\",\"PRIMARY_NAME_FIRST\":\"John\",\"DATE_OF_BIRTH\":\"06/15/1980\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"200 Elm Avenue, Las Vegas NV 89101\"}")
    (:data-source "REFERENCE" :id "2003"
     :json "{\"DATA_SOURCE\":\"REFERENCE\",\"RECORD_ID\":\"2003\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Garcia\",\"PRIMARY_NAME_FIRST\":\"Carlos\",\"DATE_OF_BIRTH\":\"03/17/1990\",\"PHONE_TYPE\":\"MOBILE\",\"PHONE_NUMBER\":\"702-555-9999\"}")))

;;; ---------------------------------------------------------------------------
;;; Truthset records — WATCHLIST
;;; ---------------------------------------------------------------------------

(defparameter *truthset-watchlist-records*
  '((:data-source "WATCHLIST" :id "3001"
     :json "{\"DATA_SOURCE\":\"WATCHLIST\",\"RECORD_ID\":\"3001\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\",\"DATE_OF_BIRTH\":\"12/11/1978\"}")
    (:data-source "WATCHLIST" :id "3002"
     :json "{\"DATA_SOURCE\":\"WATCHLIST\",\"RECORD_ID\":\"3002\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Jones\",\"PRIMARY_NAME_FIRST\":\"Margaret\",\"DATE_OF_BIRTH\":\"05/03/1975\",\"ADDR_TYPE\":\"HOME\",\"ADDR_LINE1\":\"500 Oak Blvd, Henderson NV 89015\"}")
    (:data-source "WATCHLIST" :id "3003"
     :json "{\"DATA_SOURCE\":\"WATCHLIST\",\"RECORD_ID\":\"3003\",\"RECORD_TYPE\":\"PERSON\",\"PRIMARY_NAME_LAST\":\"Davis\",\"PRIMARY_NAME_FIRST\":\"Jennifer\",\"EMAIL_ADDRESS\":\"jdavis@email.com\"}")))

;;; ---------------------------------------------------------------------------
;;; All data sources mapping
;;; ---------------------------------------------------------------------------

(defparameter *all-data-sources*
  `(("CUSTOMERS" . ,*truthset-customer-records*)
    ("REFERENCE" . ,*truthset-reference-records*)
    ("WATCHLIST" . ,*truthset-watchlist-records*)))

;;; ---------------------------------------------------------------------------
;;; Database setup
;;; ---------------------------------------------------------------------------

(defvar *test-database-initialized* nil
  "Flag to avoid re-initializing the test database within one session.")

(defun ensure-test-database ()
  "Copy the Senzing template G2C.db to /tmp/sqlite/G2C.db and register
   a default configuration if not already done in this session."
  (unless *test-database-initialized*
    (let ((template "/opt/senzing/er/resources/templates/G2C.db")
          (target "/tmp/sqlite/G2C.db")
          (target-dir "/tmp/sqlite/"))
      (ensure-directories-exist target-dir)
      (when (probe-file template)
        (uiop:copy-file template target)))
    ;; Register a default config from template so engine/diagnostic can init
    (let ((mgr (make-instance 'sz-sdk.config-manager:sz-config-manager)))
      (sz-sdk.config-manager:initialize-config-manager
       mgr *test-instance-name* *test-settings*)
      (unwind-protect
           (let ((config (sz-sdk.config-manager:create-config-from-template mgr)))
             (unwind-protect
                  (let ((config-def (sz-sdk.config:config-export config)))
                    (sz-sdk.config-manager:set-default-config
                     mgr config-def "Initial test configuration"))
               (sz-sdk.config:destroy-config config)))
        (sz-sdk.config-manager:destroy-config-manager mgr)))
    (setf *test-database-initialized* t))
  "/tmp/sqlite/G2C.db")

;;; ---------------------------------------------------------------------------
;;; Component creation helpers
;;; ---------------------------------------------------------------------------

(defun make-test-engine ()
  "Create and initialize an sz-engine for testing."
  (ensure-test-database)
  (let ((engine (make-instance 'sz-sdk.engine:sz-engine)))
    (sz-sdk.engine:initialize-engine engine *test-instance-name* *test-settings*)
    engine))

(defun make-test-product ()
  "Create and initialize an sz-product for testing."
  (ensure-test-database)
  (let ((product (make-instance 'sz-sdk.product:sz-product)))
    (sz-sdk.product:initialize-product product *test-instance-name* *test-settings*)
    product))

(defun make-test-config-manager ()
  "Create and initialize an sz-config-manager for testing."
  (ensure-test-database)
  (let ((mgr (make-instance 'sz-sdk.config-manager:sz-config-manager)))
    (sz-sdk.config-manager:initialize-config-manager
     mgr *test-instance-name* *test-settings*)
    mgr))

(defun make-test-config ()
  "Create and initialize an sz-config for testing."
  (ensure-test-database)
  (let ((config (make-instance 'sz-sdk.config:sz-config)))
    (sz-sdk.config:initialize-config config *test-instance-name* *test-settings*)
    config))

(defun make-test-diagnostic ()
  "Create and initialize an sz-diagnostic for testing."
  (ensure-test-database)
  (let ((diag (make-instance 'sz-sdk.diagnostic:sz-diagnostic)))
    (sz-sdk.diagnostic:initialize-diagnostic
     diag *test-instance-name* *test-settings*)
    diag))

(defun make-test-factory ()
  "Create an sz-abstract-factory for testing."
  (ensure-test-database)
  (make-instance 'sz-sdk.factory:sz-abstract-factory
                 :instance-name *test-instance-name*
                 :settings *test-settings*))

;;; ---------------------------------------------------------------------------
;;; Record management helpers
;;; ---------------------------------------------------------------------------

(defun add-records (engine records)
  "Add a list of truthset records to the engine."
  (dolist (rec records)
    (sz-sdk.engine:add-record engine
                              (getf rec :data-source)
                              (getf rec :id)
                              (getf rec :json))))

(defun delete-records (engine records)
  "Delete a list of truthset records from the engine."
  (dolist (rec records)
    (handler-case
        (sz-sdk.engine:delete-record engine
                                     (getf rec :data-source)
                                     (getf rec :id))
      (sz-sdk.conditions:sz-error ()
        ;; Ignore errors during cleanup (record may not exist)
        nil))))

(defun add-truthset-records (engine)
  "Add all truthset records to the engine."
  (add-records engine *truthset-customer-records*)
  (add-records engine *truthset-reference-records*)
  (add-records engine *truthset-watchlist-records*))

(defun delete-truthset-records (engine)
  "Delete all truthset records from the engine."
  (delete-records engine *truthset-customer-records*)
  (delete-records engine *truthset-reference-records*)
  (delete-records engine *truthset-watchlist-records*))

(defun get-entity-id-from-record-id (engine data-source record-id)
  "Look up the entity ID for a given data source + record ID."
  (let ((result (sz-sdk.engine:get-entity-by-record-id
                 engine data-source record-id)))
    ;; Parse ENTITY_ID from JSON: find "ENTITY_ID": followed by number
    (let* ((key "\"ENTITY_ID\":")
           (pos (search key result)))
      (when pos
        (let ((start (+ pos (length key))))
          ;; Skip whitespace
          (loop while (and (< start (length result))
                           (member (char result start) '(#\Space #\Tab)))
                do (incf start))
          ;; Read the number
          (parse-integer result :start start :junk-allowed t))))))

;;; ---------------------------------------------------------------------------
;;; Data source registration helper
;;; ---------------------------------------------------------------------------

(defun setup-truthset-datasources (config-manager)
  "Register the truthset data sources (CUSTOMERS, REFERENCE, WATCHLIST)
   in a fresh config and set it as default."
  (let ((config (sz-sdk.config-manager:create-config-from-template config-manager)))
    (unwind-protect
         (progn
           (dolist (ds *truthset-datasources*)
             (sz-sdk.config:register-data-source config ds))
           (let ((config-def (sz-sdk.config:config-export config)))
             (sz-sdk.config-manager:set-default-config
              config-manager config-def "Test truthset data sources")))
      (sz-sdk.config:destroy-config config))))
