;;;; signal-handler.lisp — SIGINT handler demonstration
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/signal-handler.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:signal-handler
  (:use #:cl #:sz-sdk))

(in-package #:signal-handler)

(defparameter *instance-name* "signal-handler")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

;; Install SIGINT handler (SBCL-specific)
(sb-sys:enable-interrupt sb-unix:sigint
  (lambda (signo context info)
    (declare (ignore signo context info))
    (format t "~%Caught ctrl-c, exiting~%")
    (sb-ext:exit :code 0)))

(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (declare (ignore engine))
        ;; Do work...
        (format t "~%Simulating work, press ctrl-c to exit...~%")
        (force-output)
        (loop (sleep 60))))
  (sz-error (e)
    (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e))))
