;;;; add-records.lisp — Add inline records to Senzing
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/loading/add-records.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:add-records
  (:use #:cl #:sz-sdk))

(in-package #:add-records)

(defparameter *instance-name* "add-records")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *records*
  '((("DATA_SOURCE" . "TEST") ("RECORD_ID" . "1001") ("RECORD_TYPE" . "PERSON")
     ("PRIMARY_NAME_LAST" . "Smith") ("PRIMARY_NAME_FIRST" . "Robert")
     ("DATE_OF_BIRTH" . "12/11/1978") ("ADDR_TYPE" . "MAILING")
     ("ADDR_FULL" . "123 Main Street, Las Vegas NV 89132")
     ("PHONE_TYPE" . "HOME") ("PHONE_NUMBER" . "702-919-1300")
     ("EMAIL_ADDRESS" . "bsmith@work.com"))
    (("DATA_SOURCE" . "TEST") ("RECORD_ID" . "1002") ("RECORD_TYPE" . "PERSON")
     ("PRIMARY_NAME_LAST" . "Smith II") ("PRIMARY_NAME_FIRST" . "Bob")
     ("DATE_OF_BIRTH" . "11/12/1978") ("ADDR_TYPE" . "HOME")
     ("ADDR_LINE1" . "1515 Adela Lane") ("ADDR_CITY" . "Las Vegas")
     ("ADDR_STATE" . "NV") ("ADDR_POSTAL_CODE" . "89111")
     ("PHONE_TYPE" . "MOBILE") ("PHONE_NUMBER" . "702-919-1300"))
    (("DATA_SOURCE" . "TEST") ("RECORD_ID" . "1003") ("RECORD_TYPE" . "PERSON")
     ("PRIMARY_NAME_LAST" . "Smith") ("PRIMARY_NAME_FIRST" . "Bob")
     ("PRIMARY_NAME_MIDDLE" . "J") ("DATE_OF_BIRTH" . "12/11/1978")
     ("EMAIL_ADDRESS" . "bsmith@work.com"))
    (("DATA_SOURCE" . "TEST") ("RECORD_ID" . "1004") ("RECORD_TYPE" . "PERSON")
     ("PRIMARY_NAME_LAST" . "Smith") ("PRIMARY_NAME_FIRST" . "B")
     ("ADDR_TYPE" . "HOME") ("ADDR_LINE1" . "1515 Adela Ln")
     ("ADDR_CITY" . "Las Vegas") ("ADDR_STATE" . "NV")
     ("ADDR_POSTAL_CODE" . "89132") ("EMAIL_ADDRESS" . "bsmith@work.com"))
    (("DATA_SOURCE" . "TEST") ("RECORD_ID" . "1005") ("RECORD_TYPE" . "PERSON")
     ("PRIMARY_NAME_LAST" . "Smith") ("PRIMARY_NAME_FIRST" . "Rob")
     ("PRIMARY_NAME_MIDDLE" . "E") ("DRIVERS_LICENSE_NUMBER" . "112233")
     ("DRIVERS_LICENSE_STATE" . "NV") ("ADDR_TYPE" . "MAILING")
     ("ADDR_LINE1" . "123 E Main St") ("ADDR_CITY" . "Henderson")
     ("ADDR_STATE" . "NV") ("ADDR_POSTAL_CODE" . "89132"))))

(defun alist-to-json (alist)
  "Encode an alist as a JSON string."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    (with-output-to-string (s)
      (yason:encode ht s))))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (dolist (record *records*)
          (let ((data-source (cdr (assoc "DATA_SOURCE" record :test #'string=)))
                (record-id (cdr (assoc "RECORD_ID" record :test #'string=)))
                (record-json (alist-to-json record)))
            (add-record engine data-source record-id record-json)
            (format t "Record ~A added~%" record-id)
            (force-output)))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
