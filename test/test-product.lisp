;;;; test-product.lisp — Tests for sz-product (requires Senzing installed)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite product-tests)

;;; ---------------------------------------------------------------------------
;;; Product lifecycle tests
;;; ---------------------------------------------------------------------------

(test product-constructor
  "Can create and initialize an sz-product"
  (let ((product (make-test-product)))
    (unwind-protect
         (is (typep product 'sz-sdk.product:sz-product))
      (sz-sdk.product:destroy-product product))))

(test product-destroy
  "Can destroy an sz-product"
  (let ((product (make-test-product)))
    (finishes (sz-sdk.product:destroy-product product))))

;;; ---------------------------------------------------------------------------
;;; Product information tests
;;; ---------------------------------------------------------------------------

(test product-get-license
  "get-license returns valid JSON with expected keys"
  (let ((product (make-test-product)))
    (unwind-protect
         (let ((license (sz-sdk.product:get-license product)))
           (is (stringp license))
           (is (plusp (length license)))
           (is (search "customer" license))
           (is (search "contract" license))
           (is (search "issueDate" license))
           (is (search "licenseType" license))
           (is (search "expireDate" license)))
      (sz-sdk.product:destroy-product product))))

(test product-get-version
  "get-version returns valid JSON with expected keys"
  (let ((product (make-test-product)))
    (unwind-protect
         (let ((version (sz-sdk.product:get-version product)))
           (is (stringp version))
           (is (plusp (length version)))
           (is (search "PRODUCT_NAME" version))
           (is (search "VERSION" version))
           (is (search "BUILD_VERSION" version))
           (is (search "BUILD_DATE" version)))
      (sz-sdk.product:destroy-product product))))
