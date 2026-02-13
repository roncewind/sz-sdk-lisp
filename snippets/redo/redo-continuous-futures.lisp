;;;; redo-continuous-futures.lisp — Continuous redo processing with futures
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/redo/redo-continuous-futures.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload "lparallel" :silent t)

(defpackage #:redo-continuous-futures
  (:use #:cl #:sz-sdk))

(in-package #:redo-continuous-futures)

(defparameter *instance-name* "redo-continuous-futures")

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

(defun safe-get-redo-record (engine)
  (handler-case
      (let ((record (get-redo-record engine)))
        (if (or (null record) (string= record ""))
            nil
            record))
    (sz-error (e)
      (mock-logger "CRITICAL" e)
      (error e))))

(defun prime-redo-records (engine quantity)
  (loop repeat quantity
        for record = (safe-get-redo-record engine)
        while record
        collect record))

(defun redo-count (engine)
  (handler-case
      (count-redo-records engine)
    (sz-retryable-error (e)
      (mock-logger "WARN" e)
      0)
    (sz-error (e)
      (mock-logger "CRITICAL" e)
      (error e))))

(defun redo-pause (success)
  (format t "No redo records to process, pausing for 30 seconds. Total processed: ~:D (ctrl-c to exit)...~%"
          success)
  (sleep 30))

(defun futures-redo (engine)
  (let ((error-recs 0)
        (success-recs 0)
        (num-workers (or (parse-integer (or (uiop:getenv "SZ_SNIPPET_WORKERS") "")
                                        :junk-allowed t)
                         8)))
    (setf lparallel:*kernel* (lparallel:make-kernel num-workers))
    (unwind-protect
         (let ((futures (make-hash-table :test 'eq)))
           ;; Wait until we have redo records
           (loop
             (let ((records (prime-redo-records engine num-workers)))
               (when records
                 (dolist (rec records)
                   (let ((r rec))
                     (setf (gethash
                            (lparallel:future
                              (process-redo-record engine r))
                            futures)
                           r)))
                 (return)))
             (redo-pause success-recs))
           ;; Main processing loop
           (loop while (plusp (hash-table-count futures))
                 do (let ((done nil)
                          (redo-paused nil))
                      (loop
                        (maphash (lambda (f rec)
                                   (declare (ignore rec))
                                   (when (lparallel:fulfilledp f)
                                     (push f done)))
                                 futures)
                        (when done (return))
                        (sleep 0.01))
                      (dolist (f done)
                        (let ((record (gethash f futures)))
                          (handler-case
                              (progn
                                (lparallel:force f)
                                (incf success-recs)
                                (when (zerop (mod success-recs 100))
                                  (format t "Processed ~:D redo records, with ~:D errors~%"
                                          success-recs error-recs)))
                            (sz-bad-input-error (e)
                              (mock-logger "ERROR" e record)
                              (incf error-recs))
                            (sz-retryable-error (e)
                              (mock-logger "WARN" e record)
                              (incf error-recs))
                            ((or sz-unrecoverable-error sz-error) (e)
                              (error e))))
                        (remhash f futures)
                        ;; Submit next redo record
                        (let ((record (safe-get-redo-record engine)))
                          (if record
                              (let ((r record))
                                (setf (gethash
                                       (lparallel:future
                                         (process-redo-record engine r))
                                       futures)
                                      r))
                              (setf redo-paused t))))
                      ;; If paused, wait for more redo records
                      (when redo-paused
                        (loop while (zerop (redo-count engine))
                              do (redo-pause success-recs))
                        ;; Refill futures
                        (loop while (< (hash-table-count futures) num-workers)
                              for record = (safe-get-redo-record engine)
                              while record
                              do (let ((r record))
                                   (setf (gethash
                                          (lparallel:future
                                            (process-redo-record engine r))
                                          futures)
                                         r)))))))
      (lparallel:end-kernel))))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (futures-redo engine)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
