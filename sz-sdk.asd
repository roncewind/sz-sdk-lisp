;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: asdf-user -*-
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0

(defsystem "sz-sdk"
  :description "Common Lisp SDK for Senzing entity resolution"
  :version "0.1.0"
  :license "Apache-2.0"
  :depends-on ("cffi" "cffi-libffi")
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:module "bindings"
                :serial t
                :components ((:file "library")
                             (:file "types")
                             (:file "product-bindings")
                             (:file "engine-bindings")
                             (:file "config-bindings")
                             (:file "config-manager-bindings")
                             (:file "diagnostic-bindings")))
               (:file "conditions")
               (:file "flags")
               (:file "constants")
               (:file "helpers")
               (:file "product")
               (:file "engine")
               (:file "config")
               (:file "config-manager")
               (:file "diagnostic")
               (:file "factory"))
  :in-order-to ((test-op (test-op "sz-sdk/test"))))

(defsystem "sz-sdk/test"
  :description "Tests for sz-sdk"
  :depends-on ("sz-sdk" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "test-package")
               (:file "test-suite")
               (:file "test-setup")
               (:file "test-conditions")
               (:file "test-flags")
               (:file "test-helpers")
               (:file "test-product")
               (:file "test-config")
               (:file "test-config-manager")
               (:file "test-engine")
               (:file "test-diagnostic")
               (:file "test-factory"))
  :perform (test-op (o c)
             (symbol-call :fiveam :run!
                          (find-symbol "SZ-SDK-TESTS" :sz-sdk.test))))
