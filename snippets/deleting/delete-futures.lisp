;;;; delete-futures.lisp — Concurrent record deletion with lparallel futures
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/deleting/delete-futures.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload '("yason" "lparallel") :silent t)

(defpackage #:delete-futures
  (:use #:cl #:sz-sdk))

(in-package #:delete-futures)

(defparameter *instance-name* "delete-futures")

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

(defun delete-record-from-json (engine record-line)
  (let* ((record (yason:parse record-line))
         (data-source (gethash "DATA_SOURCE" record ""))
         (record-id (gethash "RECORD_ID" record "")))
    (delete-record engine data-source record-id)))

(defun futures-del (engine input-file)
  (let ((error-recs 0)
        (success-recs 0)
        (num-workers (or (parse-integer (or (uiop:getenv "SZ_SNIPPET_WORKERS") "")
                                        :junk-allowed t)
                         8)))
    (setf lparallel:*kernel* (lparallel:make-kernel num-workers))
    (unwind-protect
         (with-open-file (in-stream input-file :direction :input)
           (let ((futures (make-hash-table :test 'eq)))
             (loop repeat num-workers
                   for line = (read-line in-stream nil nil)
                   while line
                   do (let ((l line))
                        (setf (gethash
                               (lparallel:future
                                 (delete-record-from-json engine l))
                               futures)
                              l)))
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
                                    (format t "Processed ~:D deletes, with ~:D errors~%"
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
                                         (delete-record-from-json engine l))
                                       futures)
                                      l)))))))))
      (lparallel:end-kernel))
    (format t "~%Successfully deleted ~:D records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (futures-del engine *input-file*)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
