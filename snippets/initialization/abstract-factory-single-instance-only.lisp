;;;; abstract-factory-single-instance-only.lisp — Demonstrate single-instance behavior
;;;
;;; Creates two factories, destroys the first, shows its engine fails,
;;; then recreates the second factory successfully.
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/abstract-factory-single-instance-only.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:abstract-factory-single-instance-only
  (:use #:cl #:sz-sdk))

(in-package #:abstract-factory-single-instance-only)

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))

;; Try and create 2 abstract factories where any of the arguments differs
(let (factory-1 engine-1)
  (handler-case
      (progn
        (format t "~%Creating first abstract factory...~%")
        (setf factory-1 (make-instance 'sz-abstract-factory
                                        :instance-name "ABSTRACT_FACTORY_1"
                                        :settings *settings*))
        (setf engine-1 (create-engine factory-1))
        (format t "~CFirst abstract factory and engine created~%" #\Tab)
        (format t "~CUsing engine-1: ~A~%" #\Tab (get-active-config-id engine-1))

        (format t "~%Creating second abstract factory...~%")
        (let* ((factory-2 (make-instance 'sz-abstract-factory
                                          :instance-name "ABSTRACT_FACTORY_2"
                                          :settings *settings*))
               (engine-2 (create-engine factory-2)))
          (format t "~CSecond abstract factory and engine created~%" #\Tab)
          (format t "~CUsing engine-2: ~A~%" #\Tab (get-active-config-id engine-2))))
    (sz-error (e)
      (format t "~C~A - ~A~%" #\Tab (type-of e) (sz-error-message e))))

  ;; Destroy the first factory
  (when factory-1
    (destroy-factory factory-1)
    (format t "~%First abstract factory has been destroyed~%"))

  ;; First abstract factory has been destroyed, try and use the engine object it created
  (handler-case
      (progn
        (format t "~%Trying engine-1 from first abstract factory again...~%")
        (format t "~CUsing engine-1: ~A~%" #\Tab (get-active-config-id engine-1)))
    (sz-error (e)
      (format t "~C~A - ~A~%" #\Tab (type-of e) (sz-error-message e))))

  ;; Now abstract factory 1 has been destroyed, try and re-create factory 2 and engine
  (handler-case
      (progn
        (format t "~%Trying second abstract factory again...~%")
        (let* ((factory-2 (make-instance 'sz-abstract-factory
                                          :instance-name "ABSTRACT_FACTORY_2"
                                          :settings *settings*))
               (engine-2 (create-engine factory-2)))
          (format t "~CCreated second abstract factory and engine after first was destroyed~%" #\Tab)
          (format t "~CUsing engine-2: ~A~%" #\Tab (get-active-config-id engine-2))))
    (sz-error (e)
      (format *error-output* "~%~A - ~A~%" (type-of e) (sz-error-message e)))))
