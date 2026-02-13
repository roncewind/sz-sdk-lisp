# sz-sdk — Common Lisp SDK for Senzing

<img src="docs/images/lisplogo_warning2_256.png" align="right" alt="Warning: Built Using Lisp" width="120">

Common Lisp SDK for [Senzing](https://senzing.com/) entity resolution. Wraps
the Senzing C library (`libSz.so`) via CFFI, providing idiomatic CLOS-based
access to all Senzing subsystems.

**License:** Apache-2.0 | **Version:** 0.1.0

<br clear="right">

## Features

- Full coverage of all 5 Senzing components: Engine, Product, Config, Config
  Manager, Diagnostic
- Factory pattern with RAII cleanup (`with-sz-factory`)
- 14-type condition hierarchy mirroring the Python SDK's exceptions
- 68 flag constants with 25 pre-composed defaults
- 279 test checks across 9 FiveAM suites
- 36 standalone code snippets

## Quick Example

```lisp
(defpackage #:my-app (:use #:cl #:sz-sdk))
(in-package #:my-app)

(defparameter *settings*
  (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON"))

(with-sz-factory (factory "my-app" *settings*)
  (let ((engine (create-engine factory)))
    ;; Add a record
    (add-record engine "CUSTOMERS" "1001"
      "{\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\"}")
    ;; Search
    (format t "~A~%"
      (search-by-attributes engine "{\"NAME_FULL\":\"Robert Smith\"}"))))
```

## Prerequisites

- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/)
- [Senzing v4](https://senzing.com/docs/quickstart/quickstart_api/) installed
  at `/opt/senzing/er/`
- A Senzing project (created via `sz_create_project`)

## Installation

```bash
# 1. Create a Senzing project (one time)
/opt/senzing/er/bin/sz_create_project ~/senzing

# 2. Source the project environment before starting SBCL
source ~/senzing/setupEnv
```

```lisp
;; 3. Load the SDK
(ql:quickload "cffi-libffi" :silent t)
(push #p"/path/to/sz-sdk-lisp/" asdf:*central-registry*)
(asdf:load-system "sz-sdk")
```

## Running Tests

```bash
source ~/senzing/setupEnv

sbcl --non-interactive \
  --eval '(ql:quickload "cffi-libffi" :silent t)' \
  --eval '(push #p"/path/to/sz-sdk-lisp/" asdf:*central-registry*)' \
  --eval '(asdf:test-system "sz-sdk")'
```

## Running Snippets

36 standalone snippets in `snippets/` demonstrate every major SDK operation:

```bash
source ~/senzing/setupEnv
sbcl --non-interactive --load snippets/information/get-version.lisp
```

| Directory | Topics |
|-----------|--------|
| `initialization/` | Factory creation, priming, config IDs, debug, signals |
| `loading/` | Adding records: single, loop, futures, queue, with-info |
| `searching/` | Search by attributes, concurrent search |
| `information/` | Version, license, stats, repository info, performance |
| `deleting/` | Delete loop, futures, with-info |
| `redo/` | Redo processing: single, continuous, futures, with-info |
| `stewardship/` | Force resolve, force unresolve |
| `configuration/` | Config registry, data sources, init default config |

## Documentation

- [Getting Started](docs/quickstart.md) — Installation, first search, core
  concepts, common recipes
- [API Reference](docs/api-reference.md) — Every public function, class,
  condition, flag, and constant
- [Architecture Guide](docs/guide.md) — Layer stack, design patterns, testing,
  future work

## Repository Layout

```
sz-sdk-lisp/
  sz-sdk.asd                — ASDF system definition
  src/
    packages.lisp            — All package definitions
    bindings/                — Raw CFFI bindings to libSz.so
    conditions.lisp          — Condition hierarchy (14 types)
    flags.lisp               — 68 flag constants
    constants.lisp           — String/sentinel constants
    helpers.lisp             — Result macros, error checking, JSON builders
    product.lisp             — sz-product (version, license)
    engine.lisp              — sz-engine (30 methods)
    config.lisp              — sz-config (in-memory config editing)
    config-manager.lisp      — sz-config-manager (persistent config registry)
    diagnostic.lisp          — sz-diagnostic (repository maintenance)
    factory.lisp             — sz-abstract-factory + with-sz-factory
  test/                      — FiveAM test suite
  snippets/                  — 36 standalone code snippets
  examples/                  — Example applications
  docs/                      — Documentation
```

## Dependencies

| Dependency | Purpose | Required? |
|------------|---------|-----------|
| `cffi` | Foreign function interface | Yes |
| `cffi-libffi` | Struct-by-value return support | Yes |
| `fiveam` | Test framework | Tests only |
| `yason` | JSON parsing | Snippets only |
| `lparallel` | Concurrent loading | Snippets only |

## License

Copyright 2024-2026 Senzing, Inc. All Rights Reserved.

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
