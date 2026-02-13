;;;; ini-to-json.lisp — Parse an INI file and print as a JSON-like structure
;;;
;;; Usage:
;;;   sbcl --non-interactive --load snippets/initialization/ini-to-json.lisp

(load (merge-pathnames "../common/setup.lisp"
        (directory-namestring *load-truename*)))

(defpackage #:ini-to-json
  (:use #:cl))

(in-package #:ini-to-json)

(defparameter *ini-file*
  (let ((env (uiop:getenv "SENZING_SNIPPETS_RESOURCES")))
    (if env
        (merge-pathnames "engine_config/sz_engine_config.ini" env)
        (merge-pathnames "../../resources/engine_config/sz_engine_config.ini"
                         (directory-namestring *load-truename*)))))

(defun parse-ini-file (path)
  "Parse an INI file into an alist of (section . ((key . value) ...))."
  (let ((result nil)
        (current-section nil)
        (current-pairs nil))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                 (cond
                   ;; Skip empty lines and comments
                   ((or (zerop (length trimmed))
                        (char= (char trimmed 0) #\;)
                        (char= (char trimmed 0) #\#))
                    nil)
                   ;; Section header
                   ((and (char= (char trimmed 0) #\[)
                         (char= (char trimmed (1- (length trimmed))) #\]))
                    ;; Save previous section
                    (when current-section
                      (push (cons current-section (nreverse current-pairs)) result))
                    (setf current-section (subseq trimmed 1 (1- (length trimmed))))
                    (setf current-pairs nil))
                   ;; Key=value pair
                   (t
                    (let ((pos (position #\= trimmed)))
                      (when pos
                        (push (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
                                    (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos))))
                              current-pairs))))))))
    ;; Save last section
    (when current-section
      (push (cons current-section (nreverse current-pairs)) result))
    (nreverse result)))

(defun print-settings (settings)
  "Print the parsed INI settings in a JSON-like format."
  (format t "{")
  (loop for (section . pairs) in settings
        for first-section = t then nil
        do (unless first-section (format t ", "))
           (format t "'~A': {" section)
           (loop for (key . value) in pairs
                 for first-pair = t then nil
                 do (unless first-pair (format t ", "))
                    (format t "'~A': '~A'" key value))
           (format t "}"))
  (format t "}~%"))

(let ((settings (parse-ini-file *ini-file*)))
  (print-settings settings))
