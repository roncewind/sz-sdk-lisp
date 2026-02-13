;;;; redo-with-info-continuous.lisp — Continuous redo processing with WITH_INFO
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/redo/redo-with-info-continuous.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:redo-with-info-continuous
  (:use #:cl #:sz-sdk #:sz-sdk.flags))

(in-package #:redo-with-info-continuous)

(defparameter *instance-name* "redo-with-info-continuous")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *output-file*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (if env
        (merge-pathnames "output/redo_with_info_continuous.jsonl" env)
        (merge-pathnames "../../resources/output/redo_with_info_continuous.jsonl"
                         (directory-namestring *load-truename*)))))

;; Install SIGINT handler
(sb-sys:enable-interrupt sb-unix:sigint
  (lambda (signo context info)
    (declare (ignore signo context info))
    (format t "~%Caught ctrl-c, exiting~%")
    (format t "~%With info responses written to ~A~%" *output-file*)
    (sb-ext:exit :code 0)))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun redo-pause (success)
  (format t "No redo records to process, pausing for 30 seconds. Total processed: ~:D (ctrl-c to exit)...~%"
          success)
  (sleep 30))

(defun process-redo (engine output-file)
  (let ((error-recs 0)
        (success-recs 0))
    (ensure-directories-exist output-file)
    (with-open-file (out-stream output-file :direction :output
                                             :if-exists :supersede)
      (loop
        (handler-case
            (let ((redo-record (get-redo-record engine)))
              (when (or (null redo-record) (string= redo-record ""))
                (redo-pause success-recs)
                (go :continue))
              (let ((response (process-redo-record engine redo-record
                                                   :flags +sz-with-info+)))
                (incf success-recs)
                (write-line response out-stream))
              (when (zerop (mod success-recs 100))
                (format t "Processed ~:D redo records, with ~:D errors~%" success-recs error-recs)))
          (sz-bad-input-error (e)
            (mock-logger "ERROR" e)
            (incf error-recs))
          (sz-retryable-error (e)
            (mock-logger "WARN" e)
            (incf error-recs))
          ((or sz-unrecoverable-error sz-error) (e)
            (error e)))
        :continue))))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (process-redo engine *output-file*)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
