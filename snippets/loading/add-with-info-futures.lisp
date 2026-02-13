;;;; add-with-info-futures.lisp — Concurrent loading with WITH_INFO responses
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/loading/add-with-info-futures.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload '("yason" "lparallel") :silent t)

(defpackage #:add-with-info-futures
  (:use #:cl #:sz-sdk #:sz-sdk.flags))

(in-package #:add-with-info-futures)

(defparameter *instance-name* "add-with-info-futures")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

(defparameter *resources-dir*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (or env
        (merge-pathnames "../../resources/"
                         (directory-namestring *load-truename*)))))

(defparameter *input-file*
  (merge-pathnames "data/load-500-with-errors.jsonl" *resources-dir*))

(defparameter *output-file*
  (merge-pathnames "output/add_file_with_info.jsonl" *resources-dir*))

(defun mock-logger (level error &optional error-record)
  (format *error-output* "~%~A: ~A - ~A~%" level (type-of error) error)
  (when error-record
    (format *error-output* "~A~%" error-record)))

(defun add-record-with-info (engine record-line)
  (let* ((record (yason:parse record-line))
         (data-source (gethash "DATA_SOURCE" record ""))
         (record-id (gethash "RECORD_ID" record "")))
    (add-record engine data-source record-id record-line :flags +sz-with-info+)))

(defun engine-stats (engine)
  (handler-case
      (format t "~%~A~%~%" (get-stats engine))
    (sz-retryable-error (e)
      (mock-logger "WARN" e))
    (sz-error (e)
      (mock-logger "CRITICAL" e)
      (error e))))

(defun futures-add (engine input-file output-file)
  (let ((error-recs 0)
        (success-recs 0)
        (num-workers (or (parse-integer (or (uiop:getenv "SZ_SNIPPET_WORKERS") "")
                                        :junk-allowed t)
                         8)))
    ;; Ensure output directory exists
    (ensure-directories-exist output-file)
    (setf lparallel:*kernel* (lparallel:make-kernel num-workers))
    (unwind-protect
         (with-open-file (out-stream output-file :direction :output
                                                  :if-exists :supersede)
           (with-open-file (in-stream input-file :direction :input)
             (let ((futures (make-hash-table :test 'eq)))
               ;; Prime the initial batch
               (loop repeat num-workers
                     for line = (read-line in-stream nil nil)
                     while line
                     do (let ((l line))
                          (setf (gethash
                                 (lparallel:future
                                   (add-record-with-info engine l))
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
                                  (let ((result (lparallel:force f)))
                                    (write-line result out-stream)
                                    (incf success-recs)
                                    (when (zerop (mod success-recs 100))
                                      (format t "Processed ~:D adds, with ~:D errors~%"
                                              success-recs error-recs)
                                      (force-output))
                                    (when (zerop (mod success-recs 200))
                                      (engine-stats engine)))
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
                                           (add-record-with-info engine l))
                                         futures)
                                        l))))))))))
      (lparallel:end-kernel))
    (format t "~%Successfully loaded ~:D records, with ~:D errors~%" success-recs error-recs)
    (format t "~%With info responses written to ~A~%" output-file)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (futures-add engine *input-file* *output-file*)))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
