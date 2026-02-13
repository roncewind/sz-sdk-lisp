;;;; add-queue.lisp — Producer/consumer loading with lparallel queue
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/loading/add-queue.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(ql:quickload '("yason" "lparallel") :silent t)

(defpackage #:add-queue
  (:use #:cl #:sz-sdk))

(in-package #:add-queue)

(defparameter *instance-name* "add-queue")

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

(defparameter *sentinel* :done)

(defun producer (input-file queue)
  "Read records from file and push onto queue. Sends sentinel when done."
  (with-open-file (in-stream input-file :direction :input)
    (loop for line = (read-line in-stream nil nil)
          while line
          do (lparallel.queue:push-queue line queue)))
  (lparallel.queue:push-queue *sentinel* queue))

(defun consumer (engine queue)
  "Drain the queue using futures for concurrent processing."
  (let ((error-recs 0)
        (success-recs 0)
        (num-workers (or (parse-integer (or (uiop:getenv "SZ_SNIPPET_WORKERS") "")
                                        :junk-allowed t)
                         8))
        (futures (make-hash-table :test 'eq)))
    (setf lparallel:*kernel* (lparallel:make-kernel num-workers))
    (unwind-protect
         (progn
           ;; Prime the initial batch
           (loop repeat num-workers
                 for item = (lparallel.queue:pop-queue queue)
                 while (not (eq item *sentinel*))
                 do (let ((l item))
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
                        ;; Get next from queue if available
                        (unless (lparallel.queue:queue-empty-p queue)
                          (let ((item (lparallel.queue:pop-queue queue)))
                            (unless (eq item *sentinel*)
                              (let ((l item))
                                (setf (gethash
                                       (lparallel:future
                                         (add-record-from-json engine l))
                                       futures)
                                      l)))))))))
      (lparallel:end-kernel))
    (format t "~%Successfully loaded ~:D records, with ~:D errors~%" success-recs error-recs)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory))
            (queue (lparallel.queue:make-queue :fixed-capacity 200)))
        ;; Run producer in a thread, consumer in main thread
        (let ((producer-thread
                (bt:make-thread (lambda () (producer *input-file* queue))
                                :name "producer")))
          (consumer engine queue)
          (bt:join-thread producer-thread))))
  (sz-error (e)
    (mock-logger "CRITICAL" e)))
