;;;; library.lisp — CFFI library loading for Senzing
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

(define-foreign-library libsz
  (:unix (:or "libSz.so" "/opt/senzing/er/lib/libSz.so"))
  (t (:default "libSz")))

(use-foreign-library libsz)
