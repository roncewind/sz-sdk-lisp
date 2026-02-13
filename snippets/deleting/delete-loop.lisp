;;;; delete-loop.lisp — Delete records from a JSONL file one at a time
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/deleting/delete-loop.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:delete-loop
  (:use #:cl #:sz-sdk))

(in-package #:delete-loop)

(defparameter *instance-name* "delete-loop")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *input-file*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (if env
        (merge-pathnames "data/del-500.jsonl" env)
        (merge-pathnames "../../resources/data/del-500.jsonl"
                         (directory-namestring *load-truename*)))))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun del-records-from-file (engine input-file)
  (let ((error-recs 0)
        (success-recs 0))
    (with-open-file (in-stream input-file :direction :input)
      (loop for record-to-delete = (read-line in-stream nil nil)
            while record-to-delete
            do (handler-case
                   (let* ((record (yason:parse record-to-delete))
                          (data-source (gethash "DATA_SOURCE" record ""))
                          (record-id (gethash "RECORD_ID" record "")))
                     (delete-record engine data-source record-id)
                     (incf success-recs))
                 (sz-bad-input-error (e)
                   (mock-logger "ERROR" e record-to-delete)
                   (incf error-recs))
                 (sz-retryable-error (e)
                   (mock-logger "WARN" e record-to-delete)
                   (incf error-recs))
                 ((or sz-unrecoverable-error sz-error) (e)
                   (error e)))
               (when (zerop (mod success-recs 100))
                 (format t "Processed ~:D deletes, with ~:D errors~%" success-recs error-recs)
                 (force-output))))
    (format t "~%Successfully deleted ~:D records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (del-records-from-file engine *input-file*)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
