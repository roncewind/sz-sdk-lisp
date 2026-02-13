;;;; force-unresolve.lisp — Force unresolve records using TRUSTED_ID
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/stewardship/force-unresolve.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:force-unresolve
  (:use #:cl #:sz-sdk #:sz-sdk.flags))

(in-package #:force-unresolve)

(defparameter *instance-name* "force-unresolve")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *purge-msg* "
**************************************** WARNING ****************************************

This example will purge all currently loaded data from the Senzing datastore!
Before proceeding, all instances of Senzing (custom code, tools, etc.) must be shut down.

*****************************************************************************************

Are you sure you want to continue and purge the Senzing datastore? Type YESPURGESENZING to purge: ")

(defparameter *records*
  '(("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"4\",\"PRIMARY_NAME_FULL\":\"Elizabeth Jonas\",\"ADDR_FULL\":\"202 Rotary Dr, Rotorville, RI, 78720\",\"SSN_NUMBER\":\"767-87-7678\",\"DATE_OF_BIRTH\":\"1/12/1990\"}"
     "TEST" "4")
    ("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"5\",\"PRIMARY_NAME_FULL\":\"Beth Jones\",\"ADDR_FULL\":\"202 Rotary Dr, Rotorville, RI, 78720\",\"SSN_NUMBER\":\"767-87-7678\",\"DATE_OF_BIRTH\":\"1/12/1990\"}"
     "TEST" "5")
    ("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"6\",\"PRIMARY_NAME_FULL\":\"Betsey Jones\",\"ADDR_FULL\":\"202 Rotary Dr, Rotorville, RI, 78720\",\"PHONE_NUMBER\":\"202-787-7678\"}"
     "TEST" "6")))

(defun json-encode (ht)
  "Encode a hash-table to a JSON string."
  (with-output-to-string (s)
    (yason:encode ht s)))

(format t "~A" *purge-msg*)
(force-output)
(unless (string= (read-line) "YESPURGESENZING")
  (uiop:quit))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((diagnostic (create-diagnostic factory))
            (engine (create-engine factory)))
        (purge-repository diagnostic)
        (terpri)
        ;; Add records
        (dolist (rec *records*)
          (destructuring-bind (json-str data-source record-id) rec
            (add-record engine data-source record-id json-str)
            (format t "Record ~A added~%" record-id)))
        (terpri)
        ;; Show current entity resolution
        (dolist (record-id '("4" "5" "6"))
          (let* ((response (get-entity-by-record-id engine "TEST" record-id
                                                    :flags +sz-entity-brief-default-flags+))
                 (json (yason:parse response))
                 (entity-id (gethash "ENTITY_ID"
                                     (gethash "RESOLVED_ENTITY" json))))
            (format t "Record ~A currently resolves to entity ~A~%" record-id entity-id)))
        ;; Update records with TRUSTED_ID to force unresolve
        (format t "~%Updating records with TRUSTED_ID to force unresolve...~%~%")
        (let* ((record4-json (yason:parse (get-record engine "TEST" "4")))
               (record6-json (yason:parse (get-record engine "TEST" "6")))
               (json-data-4 (gethash "JSON_DATA" record4-json))
               (json-data-6 (gethash "JSON_DATA" record6-json)))
          (setf (gethash "TRUSTED_ID_NUMBER" json-data-4) "TEST_R4-TEST_R6")
          (setf (gethash "TRUSTED_ID_TYPE" json-data-4) "FORCE_UNRESOLVE")
          (setf (gethash "TRUSTED_ID_NUMBER" json-data-6) "TEST_R6-TEST_R4")
          (setf (gethash "TRUSTED_ID_TYPE" json-data-6) "FORCE_UNRESOLVE")
          (add-record engine "TEST" "4" (json-encode json-data-4))
          (add-record engine "TEST" "6" (json-encode json-data-6)))
        ;; Show updated entity resolution
        (dolist (record-id '("4" "5" "6"))
          (let* ((response (get-entity-by-record-id engine "TEST" record-id
                                                    :flags +sz-entity-brief-default-flags+))
                 (json (yason:parse response))
                 (entity-id (gethash "ENTITY_ID"
                                     (gethash "RESOLVED_ENTITY" json))))
            (format t "Record ~A now resolves to entity ~A~%" record-id entity-id)))))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
