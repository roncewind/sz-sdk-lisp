;;;; product-bindings.lisp — CFFI bindings for SzProduct
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; Lifecycle

(defcfun ("SzProduct_init" %sz-product-init) :int64
  (instance-name :string)
  (settings :string)
  (verbose-logging :int64))

(defcfun ("SzProduct_destroy" %sz-product-destroy) :int64)

;;; Product info — these return static strings, not helper-allocated memory

(defcfun ("SzProduct_getLicense" %sz-product-get-license) :string)

(defcfun ("SzProduct_getVersion" %sz-product-get-version) :string)

;;; License validation helpers — return sz-string-result structs

(defcfun ("SzProduct_validateLicenseFile_helper"
          %sz-product-validate-license-file-helper)
    (:struct sz-string-result)
  (license-file-path :string))

(defcfun ("SzProduct_validateLicenseStringBase64_helper"
          %sz-product-validate-license-string-base64-helper)
    (:struct sz-string-result)
  (license-string :string))

;;; Exception handling

(defcfun ("SzProduct_getLastException" %sz-product-get-last-exception) :int64
  (buf :pointer)
  (buf-size :size))

(defcfun ("SzProduct_clearLastException" %sz-product-clear-last-exception) :void)

(defcfun ("SzProduct_getLastExceptionCode" %sz-product-get-last-exception-code) :int64)
