;;;; redo-continuous.lisp — Continuous redo processing with SIGINT handler
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/redo/redo-continuous.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:redo-continuous
  (:use #:cl #:sz-sdk))

(in-package #:redo-continuous)

(defparameter *instance-name* "redo-continuous")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

;; Install SIGINT handler
(sb-sys:enable-interrupt sb-unix:sigint
  (lambda (signo context info)
    (declare (ignore signo context info))
    (format t "~%Caught ctrl-c, exiting~%")
    (sb-ext:exit :code 0)))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun process-redo (engine)
  (let ((error-recs 0)
        (success-recs 0))
    (loop
      (handler-case
          (let ((response (get-redo-record engine)))
            (when (or (null response) (string= response ""))
              (format t "No redo records to process, pausing for 30 seconds. Total processed: ~:D (ctrl-c to exit)...~%"
                      success-recs)
              (sleep 30)
              (go :continue))
            (process-redo-record engine response)
            (incf success-recs)
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
      :continue)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (process-redo engine)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
