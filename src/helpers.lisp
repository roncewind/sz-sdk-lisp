;;;; helpers.lisp — Result checking, C memory freeing, and JSON building
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.helpers)

;;; ---------------------------------------------------------------------------
;;; Exception message retrieval
;;; ---------------------------------------------------------------------------

(defconstant +exception-buffer-size+ 4096
  "Size of the buffer used to retrieve exception messages from the C library.")

(defun get-last-exception-message (get-last-exception-fn clear-last-exception-fn)
  "Retrieve the last exception message from the C library using the provided
   getter/clearer functions, then clear it."
  (let ((buf-size +exception-buffer-size+))
    (cffi:with-foreign-object (buf :char buf-size)
      (funcall get-last-exception-fn buf buf-size)
      (let ((msg (cffi:foreign-string-to-lisp buf :encoding :utf-8)))
        (funcall clear-last-exception-fn)
        msg))))

;;; ---------------------------------------------------------------------------
;;; Result checking — signals appropriate conditions on error
;;; ---------------------------------------------------------------------------

;;; FUTURE WORK: Exception function triple elimination
;;;
;;; Each component package defines *get-exc-fn*, *clear-exc-fn*, and
;;; *get-code-fn* variables that are threaded through every call to
;;; check-result and with-sz-*-result.  A more idiomatic approach would
;;; be a component-aware macro that selects the right exception functions
;;; automatically, e.g.:
;;;
;;;   (defmacro with-sz-check (component &body body)
;;;     (ecase component
;;;       (:engine `(let ((*get-exc-fn* ...)) ,@body))
;;;       ...))
;;;
;;; Or, since each component package already shadows *get-exc-fn* etc.,
;;; the macros could simply close over them at expansion time, removing
;;; the need to pass them explicitly at all.  This would eliminate the
;;; triple from every single method body in the SDK.

(defun check-result (return-code get-last-exception-fn clear-last-exception-fn
                     get-last-exception-code-fn)
  "Check a Senzing return code. If non-zero, retrieve the last exception details
   and signal the appropriate condition."
  (unless (zerop return-code)
    (let* ((error-code (funcall get-last-exception-code-fn))
           (error-message (get-last-exception-message
                           get-last-exception-fn
                           clear-last-exception-fn))
           (condition-class (lookup-exception-class error-code)))
      (error condition-class
             :error-code error-code
             :error-message error-message))))

;;; ---------------------------------------------------------------------------
;;; Macros for safe result handling with automatic memory management
;;; ---------------------------------------------------------------------------

(defmacro with-sz-string-result ((result-var call-form
                                  get-exc-fn clear-exc-fn get-code-fn)
                                 &body body)
  "Execute CALL-FORM which returns a (:struct sz-string-result) plist.
   Bind RESULT-VAR to the response string. Check the return code and free
   the C memory via unwind-protect.

   Within BODY, RESULT-VAR is bound to the Lisp string extracted from the
   C response pointer. BODY is only reached if the return code is zero."
  (let ((result (gensym "RESULT"))
        (ptr (gensym "PTR"))
        (rc (gensym "RC")))
    `(let* ((,result ,call-form)
            (,ptr (getf ,result 'sz-sdk.bindings::response))
            (,rc (getf ,result 'sz-sdk.bindings::return-code)))
       (unwind-protect
            (progn
              (check-result ,rc ,get-exc-fn ,clear-exc-fn ,get-code-fn)
              (let ((,result-var
                      (if (cffi:null-pointer-p ,ptr)
                          ""
                          (cffi:foreign-string-to-lisp ,ptr :encoding :utf-8))))
                ,@body))
         (unless (cffi:null-pointer-p ,ptr)
           (%sz-helper-free ,ptr))))))

(defmacro with-sz-int64-result ((result-var call-form
                                 get-exc-fn clear-exc-fn get-code-fn)
                                &body body)
  "Execute CALL-FORM which returns a (:struct sz-int64-result) plist.
   Bind RESULT-VAR to the int64 response value."
  (let ((result (gensym "RESULT"))
        (rc (gensym "RC")))
    `(let* ((,result ,call-form)
            (,rc (getf ,result 'sz-sdk.bindings::return-code)))
       (check-result ,rc ,get-exc-fn ,clear-exc-fn ,get-code-fn)
       (let ((,result-var (getf ,result 'sz-sdk.bindings::response)))
         ,@body))))

(defmacro with-sz-handle-result ((result-var call-form
                                  get-exc-fn clear-exc-fn get-code-fn)
                                 &body body)
  "Execute CALL-FORM which returns a (:struct sz-handle-result) plist.
   Bind RESULT-VAR to the handle (uintptr) response value."
  (let ((result (gensym "RESULT"))
        (rc (gensym "RC")))
    `(let* ((,result ,call-form)
            (,rc (getf ,result 'sz-sdk.bindings::return-code)))
       (check-result ,rc ,get-exc-fn ,clear-exc-fn ,get-code-fn)
       (let ((,result-var (getf ,result 'sz-sdk.bindings::response)))
         ,@body))))

;;; ---------------------------------------------------------------------------
;;; Destroyed-object guard
;;; ---------------------------------------------------------------------------

(defun check-not-destroyed (destroyedp component-name)
  "Signal SZ-NOT-INITIALIZED-ERROR if DESTROYEDP is true.
   COMPONENT-NAME is a string like \"Engine\" for the error message."
  (when destroyedp
    (error 'sz-not-initialized-error
           :error-code 0
           :error-message (format nil "~A has been destroyed" component-name))))

;;; ---------------------------------------------------------------------------
;;; JSON building helpers
;;; ---------------------------------------------------------------------------

;;; FUTURE WORK: Consider adding yason as an SDK dependency for proper JSON
;;; serialization throughout.  This would eliminate the hand-rolled escaping
;;; below and enable richer JSON manipulation in helpers and components.

(defun %json-escape (string)
  "Escape a string for embedding in a JSON string literal.
   Handles backslash, double-quote, and common control characters."
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (#\Backspace (write-string "\\b" out))
               (#\Page (write-string "\\f" out))
               (otherwise
                (if (< (char-code ch) #x20)
                    (format out "\\u~4,'0X" (char-code ch))
                    (write-char ch out)))))))

(defun build-entities-json (entity-ids)
  "Build a JSON string of the form {\"ENTITIES\":[{\"ENTITY_ID\":1},{\"ENTITY_ID\":2}]}
   from a list of integer entity IDs. Returns \"{}\" for nil."
  (if (null entity-ids)
      "{}"
      (format nil "{\"ENTITIES\":[~{~A~^,~}]}"
              (mapcar (lambda (id) (format nil "{\"ENTITY_ID\":~D}" id))
                      entity-ids))))

(defun build-records-json (record-keys)
  "Build a JSON string of the form
   {\"RECORDS\":[{\"DATA_SOURCE\":\"DS\",\"RECORD_ID\":\"R1\"},...]}
   from a list of (data-source record-id) pairs."
  (if (null record-keys)
      "{}"
      (format nil "{\"RECORDS\":[~{~A~^,~}]}"
              (mapcar (lambda (pair)
                        (format nil "{\"DATA_SOURCE\":\"~A\",\"RECORD_ID\":\"~A\"}"
                                (%json-escape (first pair))
                                (%json-escape (second pair))))
                      record-keys))))

(defun build-data-sources-json (data-source-codes)
  "Build a JSON string of the form {\"DATA_SOURCES\":[\"DS1\",\"DS2\"]}
   from a list of data source code strings."
  (if (null data-source-codes)
      "{}"
      (format nil "{\"DATA_SOURCES\":[~{\"~A\"~^,~}]}"
              (mapcar #'%json-escape data-source-codes))))

(defun build-dsrc-code-json (data-source-code)
  "Build a JSON string of the form {\"DSRC_CODE\":\"DS\"} for a single data source."
  (format nil "{\"DSRC_CODE\":\"~A\"}" (%json-escape data-source-code)))
