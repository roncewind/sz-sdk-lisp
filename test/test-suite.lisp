;;;; test-suite.lisp — FiveAM test suite definition
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(def-suite sz-sdk-tests
  :description "All tests for sz-sdk")

(def-suite condition-tests
  :description "Tests for condition hierarchy"
  :in sz-sdk-tests)

(def-suite flag-tests
  :description "Tests for engine flags"
  :in sz-sdk-tests)

(def-suite helper-tests
  :description "Tests for JSON building helpers"
  :in sz-sdk-tests)

(def-suite product-tests
  :description "Tests for sz-product (requires Senzing runtime)"
  :in sz-sdk-tests)

(def-suite config-tests
  :description "Tests for sz-config"
  :in sz-sdk-tests)

(def-suite config-manager-tests
  :description "Tests for sz-config-manager"
  :in sz-sdk-tests)

(def-suite engine-tests
  :description "Tests for sz-engine"
  :in sz-sdk-tests)

(def-suite diagnostic-tests
  :description "Tests for sz-diagnostic"
  :in sz-sdk-tests)

(def-suite factory-tests
  :description "Tests for sz-abstract-factory"
  :in sz-sdk-tests)
