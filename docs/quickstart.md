# Getting Started with sz-sdk

A hands-on guide to Senzing entity resolution from Common Lisp.

**Audience:** Novice Lisp programmers, Python SDK users migrating to Lisp.

> For the complete API see the [API Reference](api-reference.md). For
> architecture details see the [Architecture Guide](guide.md).

---

## Table of Contents

- [What is Senzing?](#what-is-senzing)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Your First Search](#your-first-search)
- [Core Concepts for Newcomers](#core-concepts-for-newcomers)
  - [Packages](#packages)
  - [CLOS Objects as Handles](#clos-objects-as-handles)
  - [Conditions (Error Handling)](#conditions-error-handling)
  - [Flags](#flags)
  - [Resource Management](#resource-management)
- [Common Recipes](#common-recipes)
  - [Adding Records](#adding-records)
  - [Searching for Entities](#searching-for-entities)
  - [Managing Configuration](#managing-configuration)
  - [Concurrent Loading with lparallel](#concurrent-loading-with-lparallel)
- [Python-to-Lisp Quick Reference](#python-to-lisp-quick-reference)
- [Running the Tests](#running-the-tests)
- [Learning Resources](#learning-resources)

---

## What is Senzing?

[Senzing](https://senzing.com/) is an entity resolution engine that determines
when different records refer to the same real-world entity (person,
organization, etc.). You load records from various data sources, and Senzing
automatically resolves them into entities — finding matches, possible matches,
and relationships across your data.

The `sz-sdk` package is a Common Lisp SDK that wraps Senzing's C library,
giving you idiomatic Lisp access to all Senzing subsystems: the entity
resolution engine, product info, configuration management, and diagnostics.

---

## Prerequisites

Before you begin, you need:

1. **SBCL** — Steel Bank Common Lisp (the SDK is developed and tested on
   SBCL). Download from [sbcl.org](http://www.sbcl.org/).

2. **Quicklisp** — The Common Lisp library manager. Install from
   [quicklisp.org](https://www.quicklisp.org/).

3. **Senzing v4** — The Senzing entity resolution runtime, installed at
   `/opt/senzing/er/`. Follow the
   [Senzing v4 Linux Quickstart Guide](https://senzing.com/docs/quickstart/quickstart_api/)
   for installation and initial setup.

4. **A Senzing project** — After installing Senzing, create a project:

   ```bash
   /opt/senzing/er/bin/sz_create_project ~/senzing
   ```

   This generates a project directory containing a `setupEnv` script and
   configuration files. See the
   [Senzing Quickstart Guide](https://senzing.com/docs/quickstart/quickstart_api/)
   for details.

5. **cffi and cffi-libffi** — CFFI (Common Foreign Function Interface) and its
   libffi backend. These are Quicklisp-installable:

   ```lisp
   (ql:quickload "cffi-libffi")
   ```

6. **Environment variables** — Before starting SBCL, you must set up two
   things:

   **a) Source your Senzing project's `setupEnv` script.** This sets
   `LD_LIBRARY_PATH` and other variables so the system can find `libSz.so`:

   ```bash
   source ~/senzing/setupEnv
   ```

   (Replace `~/senzing` with the path you used when creating your project.)

   **b) Set `SENZING_ENGINE_CONFIGURATION_JSON`.** This tells the engine
   where to find its resources and database. The `setupEnv` script may set
   this for you. If not, export it manually — see the
   [Senzing Engine Configuration](https://senzing.com/docs/tutorials/senzing_engine_config/)
   documentation for the JSON format. A typical SQLite configuration looks
   like:

   ```bash
   export SENZING_ENGINE_CONFIGURATION_JSON='{
     "PIPELINE": {
       "CONFIGPATH": "/etc/opt/senzing",
       "RESOURCEPATH": "/opt/senzing/er/resources",
       "SUPPORTPATH": "/opt/senzing/data"
     },
     "SQL": {
       "CONNECTION": "sqlite3://na:na@/tmp/sqlite/G2C.db"
     }
   }'
   ```

   Adjust the paths and database connection for your environment.

---

## Installation

The SDK is an ASDF system. To load it:

```lisp
;; 1. Load the libffi backend (needed for struct-by-value returns)
(ql:quickload "cffi-libffi" :silent t)

;; 2. Tell ASDF where to find the SDK
(push #p"/path/to/sz-sdk-lisp/" asdf:*central-registry*)

;; 3. Load the SDK
(asdf:load-system "sz-sdk")
```

**Verify it works:**

```lisp
(defpackage #:my-test (:use #:cl #:sz-sdk))
(in-package #:my-test)

(with-sz-factory (factory "test" *settings*)
  (let ((product (create-product factory)))
    (format t "~A~%" (get-version product))))
```

If this prints a JSON string with version info, your installation is working.

**Common errors:**

- **"Unable to load foreign library"** — `libSz.so` is not on the library
  path. Make sure you sourced your Senzing project's `setupEnv` script before
  starting SBCL.
- **"CFFI requires libffi"** — You need to install `libffi-dev` (or
  equivalent) on your system, then `(ql:quickload "cffi-libffi")`.
- **"SENZING_ENGINE_CONFIGURATION_JSON is not set"** — Export the environment
  variable before starting SBCL.

---

## Your First Search

This walkthrough is based on
[`examples/search-name.lisp`](../examples/search-name.lisp) — a standalone
script that searches Senzing for a person by name.

### Step 1: Set Up the Package

```lisp
(defpackage #:my-search
  (:use #:cl #:sz-sdk))

(in-package #:my-search)
```

The `(:use #:sz-sdk)` clause imports all public symbols — factory functions,
engine methods, conditions, flags, and constants.

### Step 2: Configure Settings

```lisp
(defparameter *instance-name* "my-search-app")

(defparameter *settings*
  (or (uiop:getenv "SENZING_ENGINE_CONFIGURATION_JSON")
      (error "Set SENZING_ENGINE_CONFIGURATION_JSON")))
```

`*instance-name*` is an arbitrary label for your application instance.
`*settings*` is the Senzing configuration JSON — typically loaded from an
environment variable.

### Step 3: Create a Factory and Engine

```lisp
(with-sz-factory (factory *instance-name* *settings*)
  (let ((engine (create-engine factory)))
    ;; Use engine here...
    ))
```

`with-sz-factory` creates a factory, runs your code, and automatically
destroys everything when done (even if an error occurs). `create-engine`
initializes the Senzing engine using the factory's settings.

### Step 4: Search

```lisp
(with-sz-factory (factory *instance-name* *settings*)
  (let ((engine (create-engine factory)))
    (let ((result (search-by-attributes engine
                    "{\"NAME_FULL\": \"Robert Smith\"}")))
      (format t "Results: ~A~%" result))))
```

`search-by-attributes` takes a JSON string with search attributes and returns
a JSON string with matching entities.

### Step 5: Handle Errors

```lisp
(handler-case
    (with-sz-factory (factory *instance-name* *settings*)
      (let ((engine (create-engine factory)))
        (let ((result (search-by-attributes engine
                        "{\"NAME_FULL\": \"Robert Smith\"}")))
          (if (string= result "")
              (format t "No results found.~%")
              (format t "Results: ~A~%" result)))))
  (sz-error (e)
    (format *error-output* "Senzing error ~D: ~A~%"
            (sz-error-code e) (sz-error-message e))))
```

`handler-case` is Common Lisp's equivalent of Python's `try/except`. Here we
catch any `sz-error` (the base class for all Senzing errors) and print a
friendly message.

### Running it

Save the complete example as a file and run:

```bash
sbcl --non-interactive --load my-search.lisp
```

Or from the REPL: load the file and call your functions interactively.

---

## Core Concepts for Newcomers

### Packages

Common Lisp packages are namespaces. The SDK defines several, but you only
need the umbrella package:

```lisp
(defpackage #:my-app
  (:use #:cl #:sz-sdk))
```

This imports all public symbols. You can then write `add-record` instead of
`sz-sdk:add-record`.

**Naming conventions:**

| Convention | Meaning | Example |
|------------|---------|---------|
| `+name+` | Constant | `+sz-no-flags+` |
| `*name*` | Special (dynamic) variable | `*settings*` |
| `name` | Function or generic | `add-record` |
| `sz-name` | Class name | `sz-engine` |
| `%name` | Internal (private) | `%sz-helper-free` |

### CLOS Objects as Handles

The SDK uses CLOS (Common Lisp Object System) classes as handles to Senzing
subsystems. You never need to manage their internals — just pass them to the
appropriate functions:

```lisp
(let ((engine (create-engine factory)))     ; Create the handle
  (add-record engine "SRC" "1" "{...}")     ; Use it
  (get-stats engine))                       ; Use it again
;; The factory destroys it automatically
```

Think of `sz-engine`, `sz-product`, etc. as opaque tokens that prove a
subsystem has been initialized. The factory creates them; you use them; the
factory destroys them.

### Conditions (Error Handling)

Common Lisp uses *conditions* instead of exceptions. The concepts map
directly:

| Python | Common Lisp |
|--------|-------------|
| `try:` | `(handler-case` |
| `except SzError as e:` | `(sz-error (e)` |
| `raise` | `(error ...)` |
| `e.message` | `(sz-error-message e)` |

**The four error categories:**

```
sz-error                          ← Catch-all for any Senzing error
├── sz-bad-input-error            ← Your input was wrong
├── sz-general-error              ← Configuration or SDK problem
├── sz-retryable-error            ← Try again (transient failure)
└── sz-unrecoverable-error        ← Fatal — restart needed
```

**When to catch what:**

```lisp
;; Catch everything:
(handler-case (add-record engine ...)
  (sz-error (e) (handle-any-error e)))

;; Catch by category:
(handler-case (add-record engine ...)
  (sz-bad-input-error (e) (log-and-skip e))
  (sz-retryable-error (e) (retry-later e))
  (sz-unrecoverable-error (e) (error e)))  ; re-signal

;; Catch specific condition:
(handler-case (get-entity-by-record-id engine "SRC" "999")
  (sz-not-found-error () (format t "Not found~%")))
```

### Flags

Flags control what information Senzing includes in its responses. They are
64-bit integers composed with `logior` (bitwise OR):

```lisp
;; Use defaults (most common):
(get-entity-by-entity-id engine 1)

;; Add extra detail:
(get-entity-by-entity-id engine 1
  :flags (logior +sz-entity-default-flags+ +sz-include-feature-scores+))

;; Request with-info on add:
(add-record engine "SRC" "1" "{...}"
  :flags (logior +sz-add-record-default-flags+ +sz-with-info+))
```

Every method has a sensible default. You only need to specify flags when you
want something different from the default.

**Key flags to know:**

| Flag | Purpose |
|------|---------|
| `+sz-no-flags+` | No flags (0) |
| `+sz-with-info+` | Get resolution info back from add/delete/reevaluate |
| `+sz-entity-default-flags+` | Standard entity detail level |
| `+sz-search-by-attributes-default-flags+` | Standard search results |

See the [API Reference flags section](api-reference.md#flags) for the complete
list.

### Resource Management

Common Lisp uses `unwind-protect` for cleanup, similar to Python's `finally`
or `with` blocks. The SDK's `with-sz-factory` macro handles this for you:

```lisp
;; GOOD — automatic cleanup:
(with-sz-factory (factory "app" *settings*)
  (let ((engine (create-engine factory)))
    (add-record engine ...)))
;; Factory and all components destroyed here, even on error

;; MANUAL (advanced) — you manage cleanup:
(let ((factory (make-instance 'sz-abstract-factory
                 :instance-name "app"
                 :settings *settings*)))
  (unwind-protect
       (let ((engine (create-engine factory)))
         (add-record engine ...))
    (destroy-factory factory)))
```

Always prefer `with-sz-factory` unless you have a specific reason to manage
the lifecycle manually.

---

## Common Recipes

### Adding Records

Single record:

```lisp
(with-sz-factory (factory "loader" *settings*)
  (let ((engine (create-engine factory)))
    (add-record engine "CUSTOMERS" "1001"
      "{\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\"}")
    (format t "Record added~%")))
```

Multiple records in a loop:

```lisp
(with-sz-factory (factory "loader" *settings*)
  (let ((engine (create-engine factory)))
    (dolist (record *my-records*)
      (let ((ds (getf record :data-source))
            (id (getf record :id))
            (json (getf record :json)))
        (handler-case
            (progn
              (add-record engine ds id json)
              (format t "Added ~A/~A~%" ds id))
          (sz-bad-input-error (e)
            (format *error-output* "Skipping ~A/~A: ~A~%"
                    ds id (sz-error-message e))))))))
```

Add with info (get resolution details back):

```lisp
(let ((info (add-record engine "CUSTOMERS" "1001"
              "{\"PRIMARY_NAME_LAST\":\"Smith\"}"
              :flags (logior +sz-add-record-default-flags+ +sz-with-info+))))
  (format t "Resolution info: ~A~%" info))
```

See [`snippets/loading/add-records.lisp`](../snippets/loading/add-records.lisp)
for a complete example.

### Searching for Entities

Basic search:

```lisp
(let ((result (search-by-attributes engine
                "{\"NAME_FULL\":\"Robert Smith\"}")))
  (format t "~A~%" result))
```

Search with custom flags:

```lisp
(let ((result (search-by-attributes engine
                "{\"NAME_FULL\":\"Robert Smith\"}"
                :flags +sz-search-by-attributes-strong+)))
  (format t "~A~%" result))
```

See [`snippets/searching/search-records.lisp`](../snippets/searching/search-records.lisp)
for more examples.

### Managing Configuration

Register a new data source:

```lisp
(with-sz-factory (factory "config-app" *settings*)
  (let ((config-mgr (create-config-manager factory)))
    ;; Create a config from the template
    (let ((config (create-config-from-template config-mgr)))
      ;; Register a new data source
      (register-data-source config "MY_DATA_SOURCE")
      ;; Save and set as default
      (let ((config-def (config-export config)))
        (set-default-config config-mgr config-def "Added MY_DATA_SOURCE")))))
```

See [`snippets/configuration/register-data-sources.lisp`](../snippets/configuration/register-data-sources.lisp)
for a complete example.

### Concurrent Loading with lparallel

For high-volume loading, use `lparallel` futures:

```lisp
(ql:quickload '("yason" "lparallel") :silent t)

(setf lparallel:*kernel* (lparallel:make-kernel 8))
(unwind-protect
     (with-sz-factory (factory "loader" *settings*)
       (let ((engine (create-engine factory)))
         ;; Submit records as futures
         (let ((futures nil))
           (dolist (line *record-lines*)
             (push (lparallel:future
                     (let* ((record (yason:parse line))
                            (ds (gethash "DATA_SOURCE" record))
                            (id (gethash "RECORD_ID" record)))
                       (add-record engine ds id line)))
                   futures))
           ;; Collect results
           (dolist (f (nreverse futures))
             (handler-case (lparallel:force f)
               (sz-error (e)
                 (format *error-output* "Error: ~A~%" (sz-error-message e))))))))
  (lparallel:end-kernel))
```

See [`snippets/loading/add-futures.lisp`](../snippets/loading/add-futures.lisp)
for a production-ready concurrent loader.

---

## Python-to-Lisp Quick Reference

| Python SDK | Common Lisp SDK | Notes |
|------------|-----------------|-------|
| `from senzing import *` | `(:use #:sz-sdk)` | In `defpackage` |
| `SzAbstractFactory(...)` | `(make-instance 'sz-abstract-factory ...)` | Or use `with-sz-factory` |
| `with SzAbstractFactory(...) as factory:` | `(with-sz-factory (factory ...) ...)` | RAII cleanup |
| `factory.create_engine()` | `(create-engine factory)` | Returns `sz-engine` |
| `engine.add_record(...)` | `(add-record engine ...)` | Generic function |
| `try: ... except SzError as e:` | `(handler-case ... (sz-error (e) ...))` | Condition system |
| `e.message` | `(sz-error-message e)` | Reader function |
| `SzEngineFlags.SZ_WITH_INFO` | `+sz-with-info+` | Constant |
| `flags \| SZ_WITH_INFO` | `(logior flags +sz-with-info+)` | Bitwise OR |
| `json.loads(result)` | `(yason:parse result)` | Needs `yason` loaded |
| `os.environ["VAR"]` | `(uiop:getenv "VAR")` | Returns string or nil |
| `ThreadPoolExecutor` | `lparallel:make-kernel` | Needs `lparallel` loaded |
| `executor.submit(fn)` | `(lparallel:future (fn))` | Returns a future |
| `future.result()` | `(lparallel:force future)` | Blocks until complete |
| `snake_case` | `kebab-case` | Lisp naming convention |
| `CamelCase` | `kebab-case` | Class/method names |
| `UPPER_CASE` | `+earmuff-case+` | Constants |
| `_private` | `%internal` | Internal functions |

---

## Running the Tests

The SDK has a comprehensive test suite (279 checks across 9 suites).

### Setup

```bash
# Source your Senzing project environment (sets LD_LIBRARY_PATH, etc.)
source ~/senzing/setupEnv

# Ensure SENZING_ENGINE_CONFIGURATION_JSON is set (setupEnv may handle this,
# or export it manually — see Prerequisites above)
```

### Run All Tests

```bash
sbcl --non-interactive \
  --eval '(ql:quickload "cffi-libffi" :silent t)' \
  --eval '(push #p"/path/to/sz-sdk-lisp/" asdf:*central-registry*)' \
  --eval '(asdf:test-system "sz-sdk")'
```

The tests automatically:
1. Copy a fresh Senzing template database to `/tmp/sqlite/G2C.db`
2. Register a default configuration
3. Run all 9 test suites

### Run a Single Snippet

```bash
sbcl --non-interactive --load sz-sdk-lisp/snippets/information/get-version.lisp
```

---

## Learning Resources

### Common Lisp

- [Practical Common Lisp](https://gigamonkeys.com/book/) — The best
  introduction to CL for programmers coming from other languages
- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/) — The
  definitive CL language reference
- [Quicklisp](https://www.quicklisp.org/) — Library manager
- [ASDF Manual](https://asdf.common-lisp.dev/) — Build system documentation

### CFFI

- [CFFI Manual](https://cffi.common-lisp.dev/manual/) — Foreign function
  interface documentation (relevant for understanding how the SDK calls C)

### Senzing

- [Senzing Documentation](https://senzing.com/docs/) — Official documentation
- [sz-sdk-python](https://github.com/senzing-garage/sz-sdk-python) — The
  Python SDK (the upstream reference implementation)

### Libraries Used in Snippets

- [lparallel](https://lparallel.org/) — Parallel programming library (used
  for concurrent record loading)
- [FiveAM](https://common-lisp.net/project/fiveam/) — Testing framework
- [yason](https://github.com/phmarek/yason) — JSON parsing/encoding

### The Snippets

36 standalone code snippets in [`snippets/`](../snippets/) demonstrate every
major SDK operation. Each is runnable with
`sbcl --non-interactive --load <file>`. They are organized by task:

| Directory | Snippets | Topics |
|-----------|----------|--------|
| `initialization/` | 10 | Factory creation, priming, config IDs, debug, signals |
| `loading/` | 6 | Adding records: single, loop, truthset, futures, queue, with-info |
| `searching/` | 2 | Search by attributes, concurrent search |
| `information/` | 5 | Version, license, stats, repository info, performance |
| `deleting/` | 3 | Delete loop, futures, with-info |
| `redo/` | 4 | Redo processing: single, continuous, futures, with-info |
| `stewardship/` | 2 | Force resolve, force unresolve |
| `configuration/` | 4 | Config registry, data sources, init default config |

Start with [`snippets/initialization/abstract-factory.lisp`](../snippets/initialization/abstract-factory.lisp) —
it's the simplest complete example.
