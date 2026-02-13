;;;; add-with-redo.lisp — Load truthset files, then process redo records
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/redo/add-with-redo.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:add-with-redo
  (:use #:cl #:sz-sdk))

(in-package #:add-with-redo)

(defparameter *instance-name* "add-with-redo")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *resources-dir*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (or env
        (merge-pathnames "../../resources/"
                         (directory-namestring *load-truename*)))))

(defparameter *input-files*
  (list (merge-pathnames "data/truthset/customers.jsonl" *resources-dir*)
        (merge-pathnames "data/truthset/reference.jsonl" *resources-dir*)
        (merge-pathnames "data/truthset/watchlist.jsonl" *resources-dir*)))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun add-records-from-file (engine input-file)
  (let ((error-recs 0)
        (success-recs 0))
    (with-open-file (in-stream input-file :direction :input)
      (loop for record-to-add = (read-line in-stream nil nil)
            while record-to-add
            do (handler-case
                   (let* ((record (yason:parse record-to-add))
                          (data-source (gethash "DATA_SOURCE" record ""))
                          (record-id (gethash "RECORD_ID" record "")))
                     (add-record engine data-source record-id record-to-add)
                     (incf success-recs))
                 (sz-bad-input-error (e)
                   (mock-logger "ERROR" e record-to-add)
                   (incf error-recs))
                 (sz-retryable-error (e)
                   (mock-logger "WARN" e record-to-add)
                   (incf error-recs))
                 ((or sz-unrecoverable-error sz-error) (e)
                   (error e)))
               (when (zerop (mod success-recs 100))
                 (format t "Processed ~:D adds, with ~:D errors~%" success-recs error-recs)
                 (force-output))))
    (format t "~%Successfully added ~:D records, with ~:D errors~%" success-recs error-recs)))

(defun process-redo (engine)
  (let ((error-recs 0)
        (success-recs 0))
    (format t "~%Starting to process redo records...~%")
    (loop
      (handler-case
          (let ((response (get-redo-record engine)))
            (when (or (null response) (string= response ""))
              (return))
            (process-redo-record engine response)
            (incf success-recs)
            (when (zerop (mod success-recs 1))
              (format t "Processed ~:D redo records, with ~:D errors~%" success-recs error-recs)))
        (sz-bad-input-error (e)
          (mock-logger "ERROR" e)
          (incf error-recs))
        (sz-retryable-error (e)
          (mock-logger "WARN" e)
          (incf error-recs))
        ((or sz-unrecoverable-error sz-error) (e)
          (mock-logger "CRITICAL" e)
          (error e))))
    (format t "~%Successfully processed ~:D redo records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (dolist (load-file *input-files*)
          (add-records-from-file engine load-file))
        (if (plusp (count-redo-records engine))
            (process-redo engine)
            (format t "~%No redo records to process~%"))))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
