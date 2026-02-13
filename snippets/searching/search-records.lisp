;;;; search-records.lisp — Search for entities by attributes
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/searching/search-records.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "yason" :silent t)

(defpackage #:search-records
  (:use #:cl #:sz-sdk))

(in-package #:search-records)

(defparameter *instance-name* "search-records")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *search-records*
  '("{\"NAME_FULL\":\"Susan Moony\",\"DATE_OF_BIRTH\":\"15/6/1998\",\"SSN_NUMBER\":\"521212123\"}"
    "{\"NAME_FIRST\":\"Robert\",\"NAME_LAST\":\"Smith\",\"ADDR_FULL\":\"123 Main Street Las Vegas NV 89132\"}"
    "{\"NAME_FIRST\":\"Makio\",\"NAME_LAST\":\"Yamanaka\",\"ADDR_FULL\":\"787 Rotary Drive Rotorville FL 78720\"}"))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun searcher (engine)
  (dolist (record-str *search-records*)
    (handler-case
        (let ((response (search-by-attributes engine record-str)))
          (format t "~%------ Searched: ~A~%" record-str)
          (format t "~%~A~%" response)
          (force-output))
      (sz-bad-input-error (e)
        (mock-logger "ERROR" e record-str))
      (sz-retryable-error (e)
        (mock-logger "WARN" e record-str))
      ((or sz-unrecoverable-error sz-error) (e)
        (mock-logger "CRITICAL" e record-str)
        (error e)))))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (searcher engine)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
