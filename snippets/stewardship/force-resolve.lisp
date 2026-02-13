;;;; force-resolve.lisp — Force resolve records using TRUSTED_ID
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/stewardship/force-resolve.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:force-resolve
  (:use #:cl #:sz-sdk #:sz-sdk.flags))

(in-package #:force-resolve)

(defparameter *instance-name* "force-resolve")

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
  '(("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"1\",\"PRIMARY_NAME_FULL\":\"Patrick Smith\",\"AKA_NAME_FULL\":\"Paddy Smith\",\"ADDR_FULL\":\"787 Rotary Dr, Rotorville, RI, 78720\",\"PHONE_NUMBER\":\"787-767-2688\",\"DATE_OF_BIRTH\":\"1/12/1990\"}"
     "TEST" "1")
    ("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"2\",\"PRIMARY_NAME_FULL\":\"Patricia Smith\",\"ADDR_FULL\":\"787 Rotary Dr, Rotorville, RI, 78720\",\"PHONE_NUMBER\":\"787-767-2688\",\"DATE_OF_BIRTH\":\"5/4/1994\"}"
     "TEST" "2")
    ("{\"DATA_SOURCE\":\"TEST\",\"RECORD_ID\":\"3\",\"PRIMARY_NAME_FULL\":\"Pat Smith\",\"ADDR_FULL\":\"787 Rotary Dr, Rotorville, RI, 78720\",\"PHONE_NUMBER\":\"787-767-2688\"}"
     "TEST" "3")))

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
        (dolist (record-id '("1" "2" "3"))
          (let* ((response (get-entity-by-record-id engine "TEST" record-id
                                                    :flags +sz-entity-brief-default-flags+))
                 (json (yason:parse response))
                 (entity-id (gethash "ENTITY_ID"
                                     (gethash "RESOLVED_ENTITY" json))))
            (format t "Record ~A currently resolves to entity ~A~%" record-id entity-id)))
        ;; Update records with TRUSTED_ID to force resolve
        (format t "~%Updating records with TRUSTED_ID to force resolve...~%~%")
        (let* ((record1-json (yason:parse (get-record engine "TEST" "1")))
               (record3-json (yason:parse (get-record engine "TEST" "3")))
               (json-data-1 (gethash "JSON_DATA" record1-json))
               (json-data-3 (gethash "JSON_DATA" record3-json)))
          (setf (gethash "TRUSTED_ID_NUMBER" json-data-1) "TEST_R1-TEST_R3")
          (setf (gethash "TRUSTED_ID_TYPE" json-data-1) "FORCE_RESOLVE")
          (setf (gethash "TRUSTED_ID_NUMBER" json-data-3) "TEST_R1-TEST_R3")
          (setf (gethash "TRUSTED_ID_TYPE" json-data-3) "FORCE_RESOLVE")
          (add-record engine "TEST" "1" (json-encode json-data-1))
          (add-record engine "TEST" "3" (json-encode json-data-3)))
        ;; Show updated entity resolution
        (dolist (record-id '("1" "2" "3"))
          (let* ((response (get-entity-by-record-id engine "TEST" record-id
                                                    :flags +sz-entity-brief-default-flags+))
                 (json (yason:parse response))
                 (entity-id (gethash "ENTITY_ID"
                                     (gethash "RESOLVED_ENTITY" json))))
            (format t "Record ~A now resolves to entity ~A~%" record-id entity-id)))))
  (sz-error (e)
    (format *error-output* "~A - ~A~%" (type-of e) (sz-error-message e))))
