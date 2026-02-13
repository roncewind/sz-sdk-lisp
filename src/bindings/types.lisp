;;;; types.lisp — CFFI struct definitions for Senzing helper return types
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; The C helper functions in SzLang_helpers.h return structs by value.
;;; CFFI translates these to plists with slot names as keywords.
;;;
;;; Three struct shapes cover all helper returns:
;;;
;;; 1. sz-string-result  — char* response + int64_t returnCode  (most functions)
;;; 2. sz-int64-result   — int64_t response + int64_t returnCode (getActiveConfigID, etc.)
;;; 3. sz-handle-result  — uintptr_t response + int64_t returnCode (export/config handles)

(defcstruct sz-string-result
  "Result struct for helper functions that return a string response."
  (response :pointer)
  (return-code :int64))

(defcstruct sz-int64-result
  "Result struct for helper functions that return an int64 response."
  (response :int64)
  (return-code :int64))

(defcstruct sz-handle-result
  "Result struct for helper functions that return a handle (uintptr_t) response."
  (response :uintptr)
  (return-code :int64))

;;; Free function for C-allocated strings returned by helpers.

(defcfun ("SzHelper_free" %sz-helper-free) :void
  "Free memory allocated by Senzing helper functions."
  (ptr :pointer))
