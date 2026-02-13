;;;; constants.lisp — Convenience constants for Senzing SDK
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0
;;;
;;; Mirrors constants.py from sz-sdk-python.
;;;
;;; Integer constants use defconstant. String constants use defparameter
;;; because CL's defconstant requires EQL equality on redefinition, which
;;; strings don't portably satisfy. The +...+ naming convention signals
;;; intent: treat these as constants.

(in-package #:sz-sdk.constants)

(defconstant +sz-without-info+ 0)
(defconstant +sz-initialize-with-default-configuration+ 0)
(defconstant +sz-no-logging+ 0)
(defconstant +sz-verbose-logging+ 1)

(defparameter +sz-no-attributes+ "")
(defparameter +sz-no-avoidances+ "")
(defparameter +sz-no-info+ "")
(defparameter +sz-no-required-datasources+ "")
(defparameter +sz-no-search-profile+ "")
