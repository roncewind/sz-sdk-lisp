;;;; add-futures.lisp — Concurrent record loading with lparallel futures
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/loading/add-futures.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload '("yason" "lparallel") :silent t)

(defpackage #:add-futures
  (:use #:cl #:sz-sdk))

(in-package #:add-futures)

(defparameter *instance-name* "add-futures")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *input-file*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (if env
        (merge-pathnames "data/load-500.jsonl" env)
        (merge-pathnames "../../resources/data/load-500.jsonl"
                         (directory-namestring *load-truename*)))))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun add-record-from-json (engine record-line)
  (let* ((record (yason:parse record-line))
         (data-source (gethash "DATA_SOURCE" record ""))
         (record-id (gethash "RECORD_ID" record "")))
    (add-record engine data-source record-id record-line)))

(defun futures-add (engine input-file)
  (let ((error-recs 0)
        (success-recs 0)
        (num-workers (or (parse-integer (or (uiop:getenv "SZ_SNIPPET_WORKERS") "")
                                        :junk-allowed t)
                         8)))
    (setf lparallel:*kernel* (lparallel:make-kernel num-workers))
    (unwind-protect
         (with-open-file (in-stream input-file :direction :input)
           (let ((futures (make-hash-table :test 'eq)))
             ;; Prime the initial batch
             (loop repeat num-workers
                   for line = (read-line in-stream nil nil)
                   while line
                   do (let ((l line))
                        (setf (gethash
                               (lparallel:future
                                 (add-record-from-json engine l))
                               futures)
                              l)))
             ;; Process completed futures and submit new ones
             (loop while (plusp (hash-table-count futures))
                   do (let ((done nil))
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
                                    (format t "Processed ~:D adds, with ~:D errors~%"
                                            success-recs error-recs)
                                    (force-output)))
                              (sz-bad-input-error (e)
                                (mock-logger "ERROR" e record)
                                (incf error-recs))
                              (sz-retryable-error (e)
                                (mock-logger "WARN" e record)
                                (incf error-recs))
                              ((or sz-unrecoverable-error sz-error) (e)
                                (error e))))
                          (remhash f futures)
                          (let ((line (read-line in-stream nil nil)))
                            (when line
                              (let ((l line))
                                (setf (gethash
                                       (lparallel:future
                                         (add-record-from-json engine l))
                                       futures)
                                      l)))))))))
      (lparallel:end-kernel))
    (format t "~%Successfully loaded ~:D records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (futures-add engine *input-file*)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
