;;;; add-truthset-loop.lisp — Load truthset files one record at a time
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/loading/add-truthset-loop.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:add-truthset-loop
  (:use #:cl #:sz-sdk))

(in-package #:add-truthset-loop)

(defparameter *instance-name* "add-truthset-loop")

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
  (let ((success-recs 0)
        (error-recs 0))
    (format t "~%Adding records from ~A~%" input-file)
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
                 (format t "Processed ~:D adds, with ~:D errors~%" success-recs error-recs))))
    (format t "~%Successfully loaded ~:D records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (dolist (load-file *input-files*)
          (add-records-from-file engine load-file))))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
