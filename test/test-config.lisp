;;;; test-config.lisp — Tests for sz-config (requires Senzing installed)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite config-tests)

;;; ---------------------------------------------------------------------------
;;; Config tests
;;; ---------------------------------------------------------------------------

(test config-export
  "config-export returns non-empty JSON string after import-template"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           (sz-sdk.config:import-template config)
           (let ((result (sz-sdk.config:config-export config)))
             (is (stringp result))
             (is (plusp (length result)))
             ;; Config JSON should contain G2_CONFIG structure
             (is (search "G2_CONFIG" result))))
      (sz-sdk.config:destroy-config config))))

(test config-get-data-source-registry
  "get-data-source-registry returns JSON with DATA_SOURCES array"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           (sz-sdk.config:import-template config)
           (let ((result (sz-sdk.config:get-data-source-registry config)))
             (is (stringp result))
             (is (search "DATA_SOURCES" result))))
      (sz-sdk.config:destroy-config config))))

(test config-register-data-source
  "register-data-source adds a new data source and returns response"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           (sz-sdk.config:import-template config)
           (let ((result (sz-sdk.config:register-data-source config "TEST_DS")))
             (is (stringp result))
             (is (search "DSRC_ID" result)))
           ;; Verify it's in the registry
           (let ((registry (sz-sdk.config:get-data-source-registry config)))
             (is (search "TEST_DS" registry))))
      (sz-sdk.config:destroy-config config))))

(test config-unregister-data-source
  "unregister-data-source removes a data source"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           (sz-sdk.config:import-template config)
           ;; Register first
           (sz-sdk.config:register-data-source config "TEMP_DS")
           ;; Verify it's there
           (let ((before (sz-sdk.config:get-data-source-registry config)))
             (is (search "TEMP_DS" before)))
           ;; Unregister
           (finishes (sz-sdk.config:unregister-data-source config "TEMP_DS"))
           ;; Verify it's gone
           (let ((after (sz-sdk.config:get-data-source-registry config)))
             (is (not (search "TEMP_DS" after)))))
      (sz-sdk.config:destroy-config config))))

(test config-import-template
  "import-template loads config successfully"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           (finishes (sz-sdk.config:import-template config))
           ;; After import, export should return non-empty config
           (let ((exported (sz-sdk.config:config-export config)))
             (is (plusp (length exported)))))
      (sz-sdk.config:destroy-config config))))

(test config-import-config-definition
  "import-config-definition imports a JSON string and export matches"
  (let ((config (make-test-config)))
    (unwind-protect
         (progn
           ;; Get a valid config by importing template
           (sz-sdk.config:import-template config)
           (let ((original (sz-sdk.config:config-export config)))
             ;; Import that config string into a fresh config
             (sz-sdk.config:import-config-definition config original)
             (let ((reimported (sz-sdk.config:config-export config)))
               (is (string= original reimported)))))
      (sz-sdk.config:destroy-config config))))
