;;;; test-config-manager.lisp — Tests for sz-config-manager (requires Senzing)
;;;
;;; Copyright (C) 2024-2026 Senzing, Inc. All Rights Reserved.
;;; Licensed under the Apache License, Version 2.0

(in-package #:sz-sdk.test)

(in-suite config-manager-tests)

;;; ---------------------------------------------------------------------------
;;; Config manager tests
;;; ---------------------------------------------------------------------------

(test config-mgr-get-default-config-id
  "get-default-config-id returns positive integer"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let ((config-id (sz-sdk.config-manager:get-default-config-id mgr)))
           (is (integerp config-id))
           (is (plusp config-id)))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-create-config-from-config-id
  "create-config-from-config-id returns sz-config with valid definition"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let* ((config-id (sz-sdk.config-manager:get-default-config-id mgr))
                (config (sz-sdk.config-manager:create-config-from-config-id
                         mgr config-id)))
           (is (typep config 'sz-sdk.config:sz-config))
           (let ((exported (sz-sdk.config:config-export config)))
             (is (plusp (length exported)))
             (is (search "G2_CONFIG" exported)))
           (sz-sdk.config:destroy-config config))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-create-config-from-config-id-bad-id
  "create-config-from-config-id with bad ID signals condition"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.config-manager:create-config-from-config-id mgr 999999999))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-create-config-from-string
  "create-config-from-string returns sz-config from JSON string"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let* ((config-id (sz-sdk.config-manager:get-default-config-id mgr))
                (source-config (sz-sdk.config-manager:create-config-from-config-id
                                mgr config-id))
                (config-def (sz-sdk.config:config-export source-config)))
           (sz-sdk.config:destroy-config source-config)
           (let ((config (sz-sdk.config-manager:create-config-from-string
                          mgr config-def)))
             (is (typep config 'sz-sdk.config:sz-config))
             (sz-sdk.config:destroy-config config)))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-create-config-from-template
  "create-config-from-template returns sz-config from template"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let ((config (sz-sdk.config-manager:create-config-from-template mgr)))
           (is (typep config 'sz-sdk.config:sz-config))
           (let ((exported (sz-sdk.config:config-export config)))
             (is (plusp (length exported))))
           (sz-sdk.config:destroy-config config))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-get-config-registry
  "get-config-registry returns JSON with CONFIGS array"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let ((result (sz-sdk.config-manager:get-config-registry mgr)))
           (is (stringp result))
           (is (search "CONFIGS" result)))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-register-config
  "register-config registers new config and returns config ID"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let* ((config (sz-sdk.config-manager:create-config-from-template mgr))
                (config-def (sz-sdk.config:config-export config)))
           (sz-sdk.config:destroy-config config)
           (let ((new-id (sz-sdk.config-manager:register-config
                          mgr config-def "Test registration")))
             (is (integerp new-id))
             (is (plusp new-id))))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-replace-default-config-id
  "replace-default-config-id atomically replaces default"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let* ((current-id (sz-sdk.config-manager:get-default-config-id mgr))
                (config (sz-sdk.config-manager:create-config-from-template mgr))
                (config-def (sz-sdk.config:config-export config)))
           (sz-sdk.config:destroy-config config)
           (let ((new-id (sz-sdk.config-manager:register-config
                          mgr config-def "Replace test")))
             (finishes
               (sz-sdk.config-manager:replace-default-config-id
                mgr current-id new-id))
             ;; Verify it changed
             (let ((updated-id (sz-sdk.config-manager:get-default-config-id mgr)))
               (is (= new-id updated-id)))))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-replace-default-config-id-bad-id
  "replace-default-config-id with wrong current ID signals condition"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (signals (sz-error)
           (sz-sdk.config-manager:replace-default-config-id mgr 999999999 1))
      (sz-sdk.config-manager:destroy-config-manager mgr))))

(test config-mgr-set-default-config
  "set-default-config registers and sets as default"
  (let ((mgr (make-test-config-manager)))
    (unwind-protect
         (let* ((config (sz-sdk.config-manager:create-config-from-template mgr))
                (config-def (sz-sdk.config:config-export config)))
           (sz-sdk.config:destroy-config config)
           (let ((new-id (sz-sdk.config-manager:set-default-config
                          mgr config-def "Set default test")))
             (is (integerp new-id))
             (is (plusp new-id))
             ;; Verify it's the default
             (let ((default-id (sz-sdk.config-manager:get-default-config-id mgr)))
               (is (= new-id default-id)))))
      (sz-sdk.config-manager:destroy-config-manager mgr))))
