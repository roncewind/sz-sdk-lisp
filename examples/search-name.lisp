;;;; search-name.lisp — Search for a person by name in Senzing
;;;
;;; Usage:
;;;   sbcl --load examples/search-name.lisp "Robert Smith"
;;;
;;; Or from the REPL:
;;;   (load "examples/search-name.lisp")
;;;   (search-name:run "Robert Smith")
;;;
;;; Prerequisites:
;;;   - Senzing installed (libSz.so accessible)
;;;   - SENZING_ENGINE_CONFIGURATION_JSON environment variable set
;;;   - Data already loaded into Senzing (e.g. via szloader or add-record)

;;; --- Bootstrap: load the SDK ---

(require :asdf)
(ql:quickload "cffi-libffi" :silent t)

;; Add the SDK to ASDF's search path (parent of examples/)
(pushnew (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
         asdf:*central-registry* :test #'equal)

(asdf:load-system "sz-sdk" :verbose nil)

;;; --- Example package ---

(defpackage #:search-name
  (:use #:cl #:sz-sdk)
  (:export #:run))

(in-package #:search-name)

;;; --- Configuration ---

(defparameter *instance-name* "sz-lisp-search-example")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "SENZING_ENGINE_CONFIGURATION_JSON environment variable is not set.")))

;;; --- Core search logic ---

(defun format-attributes (name)
  "Build the Senzing search attributes JSON for a person name."
  (format nil "{\"NAME_FULL\": \"~A\"}" name))

(defun run (name)
  "Search Senzing for entities matching NAME and print the results."
  (format t "~%Searching for: ~A~%~%" name)
  (with-sz-factory (factory *instance-name* *settings*)
    (let ((engine (create-engine factory)))
      (let ((result (search-by-attributes engine (format-attributes name))))
        (if (string= result "")
            (format t "No results found.~%")
            (format t "Search results:~%~A~%" result))))))

;;; --- Command-line entry point ---

(defun main ()
  "Entry point when run from the command line."
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((null args)
       (format t "Usage: sbcl --load examples/search-name.lisp \"Name to search\"~%")
       (uiop:quit 1))
      (t
       (handler-case
           (run (format nil "~{~A~^ ~}" args))
         (sz-error (e)
           (format *error-output* "Senzing error ~D: ~A~%"
                   (sz-error-code e) (sz-error-message e))
           (uiop:quit 2))
         (error (e)
           (format *error-output* "Error: ~A~%" e)
           (uiop:quit 3)))
       (uiop:quit 0)))))

;; Run if loaded non-interactively
(unless (find :swank *features*)
  (main))
