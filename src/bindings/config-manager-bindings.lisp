;;;; config-manager-bindings.lisp — CFFI bindings for SzConfigMgr
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.bindings)

;;; Lifecycle

(defcfun ("SzConfigMgr_init" %sz-config-mgr-init) :int64
  (instance-name :string)
  (settings :string)
  (verbose-logging :int64))

(defcfun ("SzConfigMgr_destroy" %sz-config-mgr-destroy) :int64)

;;; Config registration — returns int64 config-id + return-code

(defcfun ("SzConfigMgr_registerConfig_helper"
          %sz-config-mgr-register-config-helper)
    (:struct sz-int64-result)
  (config-str :string)
  (config-comments :string))

;;; Config retrieval — returns string + return-code

(defcfun ("SzConfigMgr_getConfig_helper"
          %sz-config-mgr-get-config-helper)
    (:struct sz-string-result)
  (config-id :int64))

(defcfun ("SzConfigMgr_getConfigRegistry_helper"
          %sz-config-mgr-get-config-registry-helper)
    (:struct sz-string-result))

;;; Default config ID — returns int64 + return-code

(defcfun ("SzConfigMgr_getDefaultConfigID_helper"
          %sz-config-mgr-get-default-config-id-helper)
    (:struct sz-int64-result))

(defcfun ("SzConfigMgr_setDefaultConfigID" %sz-config-mgr-set-default-config-id) :int64
  (config-id :int64))

(defcfun ("SzConfigMgr_replaceDefaultConfigID" %sz-config-mgr-replace-default-config-id) :int64
  (current-default-config-id :int64)
  (new-default-config-id :int64))

;;; Exception handling

(defcfun ("SzConfigMgr_getLastException" %sz-config-mgr-get-last-exception) :int64
  (buf :pointer)
  (buf-size :size))

(defcfun ("SzConfigMgr_clearLastException" %sz-config-mgr-clear-last-exception) :void)

(defcfun ("SzConfigMgr_getLastExceptionCode" %sz-config-mgr-get-last-exception-code) :int64)
