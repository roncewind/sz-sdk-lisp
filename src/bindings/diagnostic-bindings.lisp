;;;; diagnostic-bindings.lisp — CFFI bindings for SzDiagnostic
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; Lifecycle

(defcfun ("SzDiagnostic_init" %sz-diagnostic-init) :int64
  (instance-name :string)
  (settings :string)
  (verbose-logging :int64))

(defcfun ("SzDiagnostic_initWithConfigID" %sz-diagnostic-init-with-config-id) :int64
  (instance-name :string)
  (settings :string)
  (config-id :int64)
  (verbose-logging :int64))

(defcfun ("SzDiagnostic_destroy" %sz-diagnostic-destroy) :int64)

(defcfun ("SzDiagnostic_reinit" %sz-diagnostic-reinit) :int64
  (config-id :int64))

;;; Repository operations

(defcfun ("SzDiagnostic_purgeRepository" %sz-diagnostic-purge-repository) :int64)

(defcfun ("SzDiagnostic_checkRepositoryPerformance_helper"
          %sz-diagnostic-check-repository-performance-helper)
    (:struct sz-string-result)
  (seconds-to-run :int64))

(defcfun ("SzDiagnostic_getRepositoryInfo_helper"
          %sz-diagnostic-get-repository-info-helper)
    (:struct sz-string-result))

;;; Feature (unsupported/undocumented)

(defcfun ("SzDiagnostic_getFeature_helper"
          %sz-diagnostic-get-feature-helper)
    (:struct sz-string-result)
  (lib-feat-id :int64))

;;; Exception handling

(defcfun ("SzDiagnostic_getLastException" %sz-diagnostic-get-last-exception) :int64
  (buf :pointer)
  (buf-size :size))

(defcfun ("SzDiagnostic_clearLastException" %sz-diagnostic-clear-last-exception) :void)

(defcfun ("SzDiagnostic_getLastExceptionCode" %sz-diagnostic-get-last-exception-code) :int64)
