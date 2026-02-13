;;;; config-bindings.lisp — CFFI bindings for SzConfig
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; Lifecycle

(defcfun ("SzConfig_init" %sz-config-init) :int64
  (instance-name :string)
  (settings :string)
  (verbose-logging :int64))

(defcfun ("SzConfig_destroy" %sz-config-destroy) :int64)

;;; Config handle management

(defcfun ("SzConfig_create_helper" %sz-config-create-helper)
    (:struct sz-handle-result))

(defcfun ("SzConfig_load_helper" %sz-config-load-helper)
    (:struct sz-handle-result)
  (input-json :string))

(defcfun ("SzConfig_export_helper" %sz-config-export-helper)
    (:struct sz-string-result)
  (config-handle :uintptr))

(defcfun ("SzConfig_close_helper" %sz-config-close-helper) :int64
  (config-handle :uintptr))

;;; Data source operations

(defcfun ("SzConfig_getDataSourceRegistry_helper"
          %sz-config-get-data-source-registry-helper)
    (:struct sz-string-result)
  (config-handle :uintptr))

(defcfun ("SzConfig_registerDataSource_helper"
          %sz-config-register-data-source-helper)
    (:struct sz-string-result)
  (config-handle :uintptr)
  (input-json :string))

(defcfun ("SzConfig_unregisterDataSource_helper"
          %sz-config-unregister-data-source-helper) :int64
  (config-handle :uintptr)
  (input-json :string))

;;; Exception handling

(defcfun ("SzConfig_getLastException" %sz-config-get-last-exception) :int64
  (buf :pointer)
  (buf-size :size))

(defcfun ("SzConfig_clearLastException" %sz-config-clear-last-exception) :void)

(defcfun ("SzConfig_getLastExceptionCode" %sz-config-get-last-exception-code) :int64)
