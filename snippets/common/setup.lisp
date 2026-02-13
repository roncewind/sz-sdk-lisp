;;;; setup.lisp — Shared bootstrap for Senzing code snippets
;;;
;;; Every snippet loads this file first via:
;;;   (load (merge-pathnames "../common/setup.lisp"
;;;           (directory-namestring *load-truename*)))
;;;
;;; It loads the sz-sdk system and provides the SENZING_ENGINE_CONFIGURATION_JSON
;;; settings string.

;;; --- Load the SDK ---

(require :asdf)
(ql:quickload "cffi-libffi" :silent t)

;; Add the SDK to ASDF's search path (snippets/ -> sz-sdk-lisp/)
(pushnew (truename (merge-pathnames "../../"
                                    (directory-namestring *load-truename*)))
         asdf:*central-registry* :test #'equal)

(asdf:load-system "sz-sdk" :verbose nil)
