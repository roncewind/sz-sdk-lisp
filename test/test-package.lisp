;;;; test-package.lisp — Test package definition
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(defpackage #:sz-sdk.test
  (:use #:cl #:fiveam
        #:sz-sdk.conditions
        #:sz-sdk.flags
        #:sz-sdk.constants)
  (:export #:sz-sdk-tests))
