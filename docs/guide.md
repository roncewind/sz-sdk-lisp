# Architecture and Design Guide

Comprehensive guide to the sz-sdk Common Lisp SDK architecture, design
rationale, and internals.

**Audience:** Professional Lisp programmers who want to understand the design,
contribute, or extend the SDK.

> For the public API see the [API Reference](api-reference.md). For a
> getting-started walkthrough see the [Quickstart](quickstart.md).

---

## Table of Contents

- [Project Overview](#project-overview)
- [Architectural Layers](#architectural-layers)
  - [Layer 1: CFFI Bindings](#layer-1-cffi-bindings)
  - [Layer 2: Conditions](#layer-2-conditions)
  - [Layer 3: Flags and Constants](#layer-3-flags-and-constants)
  - [Layer 4: Helpers](#layer-4-helpers)
  - [Layer 5: Component Packages](#layer-5-component-packages)
  - [Layer 6: Factory](#layer-6-factory)
  - [Layer 7: Umbrella Package](#layer-7-umbrella-package)
- [Design Patterns Deep Dive](#design-patterns-deep-dive)
  - [Phantom Objects](#phantom-objects)
  - [Per-Component Exception Bindings](#per-component-exception-bindings)
  - [Config Handle Lifecycle](#config-handle-lifecycle)
  - [SDK-Only Flags](#sdk-only-flags)
  - [Concurrency](#concurrency)
- [Package Hierarchy](#package-hierarchy)
- [Testing Architecture](#testing-architecture)
- [Future Work Catalog](#future-work-catalog)

---

## Project Overview

**sz-sdk** is a Common Lisp SDK for [Senzing](https://senzing.com/) entity
resolution. It wraps the Senzing C library (`libSz.so`) via CFFI, providing
idiomatic CLOS-based access to all Senzing subsystems.

| | |
|---|---|
| **Upstream** | [sz-sdk-python](https://github.com/senzing-garage/sz-sdk-python) |
| **Version** | 0.1.0 |
| **License** | Apache-2.0 |
| **ASDF System** | `sz-sdk` (main), `sz-sdk/test` (tests) |
| **Dependencies** | `cffi`, `cffi-libffi` |
| **Test Dependencies** | `fiveam` |
| **Optional** | `yason` (JSON, snippets only), `lparallel` (concurrency, snippets only) |

The SDK mirrors the Python SDK's abstract interface hierarchy but re-expresses
it in Common Lisp idioms: CLOS generic functions instead of abstract methods,
conditions instead of exceptions, `unwind-protect` instead of context managers,
and `logior` instead of bitwise-or.

---

## Architectural Layers

The SDK is organized as a bottom-up stack of seven layers, each building on
the one below. The ASDF system uses `:serial t` to enforce this load order.

```
┌──────────────────────────────────────────────────┐
│  User Code / Snippets                            │
├──────────────────────────────────────────────────┤
│  Layer 7: sz-sdk (umbrella — re-exports API)     │
├──────────────────────────────────────────────────┤
│  Layer 6: sz-sdk.factory                         │
├──────────────────────────────────────────────────┤
│  Layer 5: Component packages                     │
│    sz-sdk.product  sz-sdk.engine  sz-sdk.config  │
│    sz-sdk.config-manager  sz-sdk.diagnostic      │
├──────────────────────────────────────────────────┤
│  Layer 4: sz-sdk.helpers                         │
├──────────────────────────────────────────────────┤
│  Layer 3: sz-sdk.flags + sz-sdk.constants        │
├──────────────────────────────────────────────────┤
│  Layer 2: sz-sdk.conditions                      │
├──────────────────────────────────────────────────┤
│  Layer 1: sz-sdk.bindings                        │
│    library → types → product → engine →          │
│    config → config-manager → diagnostic          │
└──────────────────────────────────────────────────┘
```

### Layer 1: CFFI Bindings

**Package:** `sz-sdk.bindings`
**Source:** [`src/bindings/`](../src/bindings/)

The lowest layer provides raw `defcfun` declarations that bind directly to C
functions in `libSz.so`.

**Library loading** (`library.lisp`):

```lisp
(define-foreign-library libsz
  (:unix (:or "libSz.so" "/opt/senzing/er/lib/libSz.so"))
  (t (:default "libSz")))

(use-foreign-library libsz)
```

**Struct types** (`types.lisp`): Three struct definitions cover all helper
return types:

```lisp
(defcstruct sz-string-result    ; Most common — JSON responses
  (response :pointer)           ; Heap-allocated char*
  (return-code :int64))         ; 0 = success

(defcstruct sz-int64-result     ; Config IDs, counts
  (response :int64)
  (return-code :int64))

(defcstruct sz-handle-result    ; Export and config handles
  (response :uintptr)
  (return-code :int64))
```

CFFI translates these struct-by-value returns into plists with slot names as
keys. This requires `cffi-libffi` because standard CFFI cannot handle
struct-by-value returns.

**Naming convention:** C function names use CamelCase
(`Sz_addRecordWithInfo_helper`); CFFI bindings use `%kebab-case`
(`%sz-add-record-with-info-helper`). The `%` prefix signals internal use — no
user code should call these directly.

**Helper function pattern:** The C API provides two variants for most
operations:

1. **Direct function** — takes output parameters, returns `int64_t` error code
2. **Helper function** — wraps the direct function, returns a struct by value

The SDK binds the helper variants because struct-by-value returns are cleaner
to work with in Lisp than output parameters.

**Per-binding file organization:**

| File | C Component | Functions |
|------|-------------|-----------|
| `product-bindings.lisp` | SzProduct | 9 |
| `engine-bindings.lisp` | Sz (engine) | 40 |
| `config-bindings.lisp` | SzConfig | 12 |
| `config-manager-bindings.lisp` | SzConfigMgr | 10 |
| `diagnostic-bindings.lisp` | SzDiagnostic | 11 |

See the binding source files in [`src/bindings/`](../src/bindings/) for the
complete function mapping.

### Layer 2: Conditions

**Package:** `sz-sdk.conditions`
**Source:** [`src/conditions.lisp`](../src/conditions.lisp)

Defines the condition hierarchy that mirrors the Python SDK's exception
classes. Uses Common Lisp's condition system (`define-condition`) rather than
ad-hoc error values.

**Hierarchy:**

```
sz-error (base — subclass of CL error)
├── sz-bad-input-error
│   ├── sz-not-found-error
│   └── sz-unknown-data-source-error
├── sz-general-error
│   ├── sz-configuration-error
│   ├── sz-replace-conflict-error
│   └── sz-sdk-error
├── sz-retryable-error
│   ├── sz-database-connection-lost-error
│   ├── sz-database-transient-error
│   └── sz-retry-timeout-exceeded-error
└── sz-unrecoverable-error
    ├── sz-database-error
    ├── sz-license-error
    ├── sz-not-initialized-error
    └── sz-unhandled-error
```

**`define-sz-condition` macro** reduces boilerplate:

```lisp
(defmacro define-sz-condition (name parent report-prefix)
  `(define-condition ,name (,parent) ()
     (:report (lambda (c s)
                (format s ,(format nil "~A ~~D: ~~A" report-prefix)
                        (sz-error-code c) (sz-error-message c))))))

;; Usage:
(define-sz-condition sz-not-found-error sz-bad-input-error "SzNotFoundError")
```

**Error code mapping:** The `*engine-exception-map*` hash table maps 200+
Senzing error codes to condition class symbols. `lookup-exception-class`
performs the lookup, defaulting to `sz-error` for unmapped codes:

```lisp
(defun lookup-exception-class (error-code)
  (gethash error-code *engine-exception-map* 'sz-error))
```

This design means new error codes from future Senzing releases are handled
gracefully — they signal the base `sz-error` condition until the map is
updated.

### Layer 3: Flags and Constants

**Packages:** `sz-sdk.flags`, `sz-sdk.constants`
**Source:** [`src/flags.lisp`](../src/flags.lisp),
[`src/constants.lisp`](../src/constants.lisp)

**Flags** are `(unsigned-byte 64)` constants composed with `logior`:

```lisp
(declaim (type (unsigned-byte 64) +sz-no-flags+ ...))

(defconstant +sz-export-include-multi-record-entities+ (ash 1 0))
(defconstant +sz-export-include-possibly-same+         (ash 1 1))

;; Composed defaults:
(defconstant +sz-export-include-all-entities+
  (logior +sz-export-include-multi-record-entities+
          +sz-export-include-single-record-entities+))
```

68 individual flags plus 25 composed defaults — see the
[API Reference flags section](api-reference.md#flags) for the complete list.

**Constants** provide named sentinels for clarity:

```lisp
;; Integer constants — defconstant is fine
(defconstant +sz-no-logging+ 0)
(defconstant +sz-verbose-logging+ 1)

;; String constants — defparameter, not defconstant!
(defparameter +sz-no-attributes+ "")
```

**Why `defparameter` for strings?** Common Lisp's `defconstant` requires the
value to be `eql` on redefinition. Strings are not portably `eql` across
compilations (they're only `equal`), so `(defconstant foo "bar")` may signal
an error when the file is reloaded. Using `defparameter` with the `+...+`
naming convention communicates intent without tripping the `eql` constraint.

### Layer 4: Helpers

**Package:** `sz-sdk.helpers`
**Source:** [`src/helpers.lisp`](../src/helpers.lisp)

The helper layer provides four key abstractions used by every component:

**1. `get-last-exception-message`** — Retrieves the last error message from
the C library using component-specific getter/clearer functions:

```lisp
(defun get-last-exception-message (get-fn clear-fn)
  (cffi:with-foreign-object (buf :char +exception-buffer-size+)
    (funcall get-fn buf +exception-buffer-size+)
    (let ((msg (cffi:foreign-string-to-lisp buf :encoding :utf-8)))
      (funcall clear-fn)
      msg)))
```

**2. `check-result`** — Checks a return code and signals the appropriate
condition:

```lisp
(defun check-result (return-code get-exc-fn clear-exc-fn get-code-fn)
  (unless (zerop return-code)
    (let* ((error-code (funcall get-code-fn))
           (error-message (get-last-exception-message get-exc-fn clear-exc-fn))
           (condition-class (lookup-exception-class error-code)))
      (error condition-class
             :error-code error-code
             :error-message error-message))))
```

**3. Three result macros** — Handle the common pattern of calling a C helper,
extracting the result, checking for errors, and cleaning up memory:

- **`with-sz-string-result`** — For `sz-string-result` returns. Extracts the
  string, checks the return code, and frees the C memory in `unwind-protect`.
- **`with-sz-int64-result`** — For `sz-int64-result` returns. Extracts the
  integer and checks the return code.
- **`with-sz-handle-result`** — For `sz-handle-result` returns. Extracts the
  handle and checks the return code.

Example expansion of `with-sz-string-result`:

```lisp
(with-sz-string-result (result
                        (%sz-stats-helper)
                        *get-exc-fn* *clear-exc-fn* *get-code-fn*)
  result)

;; Expands to approximately:
(let* ((#:result (%sz-stats-helper))
       (#:ptr (getf #:result 'response))
       (#:rc  (getf #:result 'return-code)))
  (unwind-protect
       (progn
         (check-result #:rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)
         (let ((result (cffi:foreign-string-to-lisp #:ptr :encoding :utf-8)))
           result))
    (unless (cffi:null-pointer-p #:ptr)
      (%sz-helper-free #:ptr))))
```

**4. `check-not-destroyed`** — Guards against using destroyed components:

```lisp
(defun check-not-destroyed (destroyedp component-name)
  (when destroyedp
    (error 'sz-not-initialized-error
           :error-code 0
           :error-message (format nil "~A has been destroyed" component-name))))
```

**5. JSON builders** — Construct JSON strings for list-based parameters:

```lisp
(build-entities-json '(1 2 3))
;; → "{\"ENTITIES\":[{\"ENTITY_ID\":1},{\"ENTITY_ID\":2},{\"ENTITY_ID\":3}]}"

(build-records-json '(("CUSTOMERS" "1001") ("REFERENCE" "2001")))
;; → "{\"RECORDS\":[{\"DATA_SOURCE\":\"CUSTOMERS\",\"RECORD_ID\":\"1001\"}, ...]}"
```

### Layer 5: Component Packages

**Packages:** `sz-sdk.product`, `sz-sdk.engine`, `sz-sdk.config`,
`sz-sdk.config-manager`, `sz-sdk.diagnostic`
**Source:** [`src/product.lisp`](../src/product.lisp),
[`src/engine.lisp`](../src/engine.lisp), etc.

Each component follows the same structural pattern:

1. **CLOS class** with a `destroyedp` flag (see [Phantom Objects](#phantom-objects))
2. **Per-component exception bindings** (see [Per-Component Exception Bindings](#per-component-exception-bindings))
3. **Lifecycle generics** — `initialize-*` and `destroy-*`
4. **Public API generics** — the component's exported methods

**Generic functions over `defun`:** All methods are `defgeneric`/`defmethod`
pairs rather than plain `defun`. This mirrors the Python SDK's abstract
interface pattern and enables future pluggable backends (gRPC transport, mock
implementations). If alternative backends are never needed, these could be
simplified to `defun`.

**Component sizes:**

| Component | Public Methods | CFFI Bindings Used |
|-----------|---------------|-------------------|
| Product | 2 | 4 |
| Engine | 30 | 40 |
| Config | 7 | 10 |
| Config Manager | 9 | 8 |
| Diagnostic | 4 | 8 |

### Layer 6: Factory

**Package:** `sz-sdk.factory`
**Source:** [`src/factory.lisp`](../src/factory.lisp)

The factory is the primary user-facing entry point. It manages the lifecycle
of all components:

```lisp
(defclass sz-abstract-factory ()
  ((instance-name ...)
   (settings ...)
   (config-id ...)
   (verbose-logging ...)
   (destroyedp ...)
   (created-objects ...)))  ; Tracks all created components for cleanup
```

**Creation methods** (`create-engine`, `create-product`,
`create-config-manager`, `create-diagnostic`) instantiate and initialize a
component, then push it onto `created-objects` for tracking.

**`destroy-factory`** iterates `created-objects` and calls
`destroy-component` on each, using `ignore-errors` for resilience:

```lisp
(defmethod destroy-factory ((factory sz-abstract-factory))
  (unless (factory-destroyed-p factory)
    (dolist (obj (factory-created-objects factory))
      (ignore-errors (destroy-component obj)))
    (setf (factory-created-objects factory) nil)
    (setf (factory-destroyed-p factory) t)))
```

**`destroy-component`** is an internal generic that dispatches to the
component-specific destroy method:

```lisp
(defmethod destroy-component ((e sz-engine))   (destroy-engine e))
(defmethod destroy-component ((p sz-product))  (destroy-product p))
;; etc.
```

**`reinitialize-factory`** propagates a new config ID to all components that
support it (engine and diagnostic). A default no-op method handles components
that don't support reinitialization:

```lisp
(defgeneric reinitialize-component (component config-id)
  (:method (component config-id)
    (declare (ignore component config-id))
    (values)))  ; Default no-op
```

**`with-sz-factory` macro** provides RAII-style cleanup:

```lisp
(defmacro with-sz-factory ((factory-var instance-name settings
                            &key (config-id 0) (verbose-logging 0))
                           &body body)
  `(let ((,factory-var (make-instance 'sz-abstract-factory ...)))
     (unwind-protect
          (progn ,@body)
       (destroy-factory ,factory-var))))
```

### Layer 7: Umbrella Package

**Package:** `sz-sdk`
**Source:** [`src/packages.lisp`](../src/packages.lisp) (lines 416–569)

The umbrella package `:use`s all component packages and explicitly `:export`s
the public API. This gives users a single package to import:

```lisp
(defpackage #:my-app
  (:use #:cl #:sz-sdk))
```

**Deliberate exclusions:** Lifecycle methods (`initialize-*`, `destroy-*`) are
not exported from the umbrella package. Users should use the factory pattern
(`with-sz-factory`, `create-*`) instead. The lifecycle methods remain
accessible via their component packages for advanced use cases.

**Re-export strategy:** Currently manual — every exported symbol is listed
explicitly in both the component package and the umbrella package. A future
improvement could use `uiop:define-package` with `:reexport`.

---

## Design Patterns Deep Dive

### Phantom Objects

The most distinctive pattern in the SDK. CLOS classes like `sz-engine`,
`sz-product`, and `sz-diagnostic` carry no state beyond a `destroyedp`
boolean flag:

```lisp
(defclass sz-engine ()
  ((destroyedp
    :initform nil
    :accessor sz-engine-destroyed-p)))
```

**Why?** The underlying C library uses global state — there is exactly one
engine instance, one product instance, etc. per process. The CLOS objects
serve as:

1. **Initialization tokens** — proof that `initialize-*` was called
2. **Destroy guards** — prevent use-after-destroy via `check-not-destroyed`
3. **Generic function dispatch targets** — enable the CLOS method protocol

**Trade-offs:**

- **Pro:** Mirrors the Python SDK's abstract interface pattern
- **Pro:** Enables future pluggable backends (gRPC, mocks) via CLOS dispatch
- **Con:** The objects carry no useful state — a boolean flag is their only
  purpose
- **Con:** Users must thread these objects through call sites

**Alternatives considered** (documented in `factory.lisp`):

1. A single `sz-context` object tracking all initialization state
2. Special variables (`*sz-engine-initialized*`) checked by macros
3. The factory itself as the primary handle

Any alternative would be a breaking change to the public API.

### Per-Component Exception Bindings

Each component package defines three `defvar` bindings for its exception
functions:

```lisp
;; In sz-sdk.engine:
(defvar *get-exc-fn* #'%sz-get-last-exception)
(defvar *clear-exc-fn* #'%sz-clear-last-exception)
(defvar *get-code-fn* #'%sz-get-last-exception-code)
```

These are threaded through every call to `check-result` and the result macros.
Each C component (SzProduct, Sz, SzConfig, SzConfigMgr, SzDiagnostic) has its
own exception functions, so each Lisp component package shadows these
variables with the correct bindings.

**The triple-threading pattern:**

```lisp
(defmethod get-stats ((engine sz-engine))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (with-sz-string-result (result
                          (%sz-stats-helper)
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    result))
```

Every method body passes the same three variables. This is verbose but correct
— the variables resolve to different C functions depending on which package
the method is defined in.

**Future simplification:** The macros could close over the package-level
variables at expansion time, eliminating the need to pass them explicitly. See
[Future Work](#future-work-catalog).

### Config Handle Lifecycle

`sz-config` is unique among components because the C API uses ephemeral
in-memory config handles that must be explicitly closed. The Lisp API
abstracts this away:

```lisp
(defmethod get-data-source-registry ((config sz-config))
  ;; 1. Load config from internal JSON → get handle
  (with-sz-handle-result (config-handle
                          (%sz-config-load-helper
                           (sz-config-config-definition config))
                          *get-exc-fn* *clear-exc-fn* *get-code-fn*)
    (unwind-protect
         ;; 2. Use the handle
         (with-sz-string-result (result
                                 (%sz-config-get-data-source-registry-helper
                                  config-handle)
                                 *get-exc-fn* *clear-exc-fn* *get-code-fn*)
           result)
      ;; 3. Always close the handle
      (let ((rc (%sz-config-close-helper config-handle)))
        (check-result rc *get-exc-fn* *clear-exc-fn* *get-code-fn*)))))
```

**Key invariant:** Every `%sz-config-create-helper` or `%sz-config-load-helper`
call is paired with a `%sz-config-close-helper` in `unwind-protect`. This
prevents handle leaks even when errors occur during config operations.

The `register-data-source` method shows the full pattern with nested cleanup:

1. Load config → handle
2. Register data source using handle
3. Export modified config from handle
4. Store exported JSON in `sz-config-config-definition`
5. Close handle (in `unwind-protect`)

### SDK-Only Flags

The `+sz-with-info+` flag (bit 62) is a pure SDK invention — the C library
does not understand it. The SDK uses it to dispatch between two C function
variants:

```lisp
(defmethod add-record ((engine sz-engine) data-source-code record-id
                       record-definition
                       &key (flags +sz-add-record-default-flags+))
  (check-not-destroyed (sz-engine-destroyed-p engine) "Engine")
  (if (logtest flags +sz-with-info+)
      ;; WITH INFO: call the helper variant, return JSON
      (with-sz-string-result (result
                              (%sz-add-record-with-info-helper
                               data-source-code record-id record-definition
                               (%strip-sdk-flags flags))
                              ...)
        result)
      ;; NO INFO: call the simple variant, return ""
      (let ((rc (%sz-add-record data-source-code record-id record-definition)))
        (check-result rc ...)
        "")))
```

**`%strip-sdk-flags`** removes the SDK-only bit before passing to C:

```lisp
(defun %strip-sdk-flags (flags)
  (logand flags (lognot +sz-with-info+)))
```

This pattern consolidates two C functions into one Lisp generic, giving users
a single function with a flag that controls the return behavior.

### Concurrency

The Senzing C library is thread-safe — multiple threads can call engine
functions concurrently. The SDK leverages this in snippets via `lparallel`:

```lisp
;; From snippets/loading/add-futures.lisp
(setf lparallel:*kernel* (lparallel:make-kernel num-workers))
(unwind-protect
     (progn
       ;; Submit futures that call add-record concurrently
       (lparallel:future (add-record-from-json engine line))
       ;; Collect results...
       )
  (lparallel:end-kernel))
```

**Key points:**

- `lparallel` is not an SDK dependency — it's loaded on demand by snippets
- The factory and engine can be shared across threads
- Error handling uses `handler-case` around `lparallel:force` to catch
  conditions from worker threads
- `lparallel:end-kernel` is called in `unwind-protect` for cleanup

---

## Package Hierarchy

Complete dependency diagram:

```
                    sz-sdk (umbrella)
                       │
              ┌────────┼────────────────────────┐
              │        │                        │
              ▼        ▼                        ▼
        sz-sdk.factory                   (conditions, flags,
              │                           constants — re-exported)
    ┌─────────┼──────────────────┐
    │         │         │        │
    ▼         ▼         ▼        ▼
 .product  .engine  .config-  .diagnostic
              │     manager      │
              │        │         │
              └───┬────┘         │
                  │              │
                  ▼              │
             .config ◄───────────┘
                  │
    ┌─────────────┼──────────────┐
    │             │              │
    ▼             ▼              ▼
 .helpers    .conditions    .flags
    │             │
    ▼             │
 .bindings ◄──────┘
    │
    ▼
  CFFI + cffi-libffi
    │
    ▼
  libSz.so
```

All component packages (`.product`, `.engine`, `.config`, `.config-manager`,
`.diagnostic`) depend on `.bindings`, `.conditions`, and `.helpers`. The
`.engine` package additionally depends on `.flags` for the `+sz-with-info+`
flag.

---

## Testing Architecture

**Framework:** [FiveAM](https://common-lisp.net/project/fiveam/)
**ASDF subsystem:** `sz-sdk/test`

### Suite Structure

9 FiveAM suites under a parent suite `sz-sdk-tests`:

| Suite | Tests | File |
|-------|-------|------|
| `condition-tests` | 9 | `test-conditions.lisp` |
| `flag-tests` | 11 | `test-flags.lisp` |
| `helper-tests` | 12 | `test-helpers.lisp` |
| `product-tests` | 4 | `test-product.lisp` |
| `config-tests` | 6 | `test-config.lisp` |
| `config-manager-tests` | 10 | `test-config-manager.lisp` |
| `engine-tests` | 47 | `test-engine.lisp` |
| `diagnostic-tests` | 6 | `test-diagnostic.lisp` |
| `factory-tests` | 8 | `test-factory.lisp` |
| **Total** | **113 tests (279 checks)** | |

### Test Infrastructure

`test-setup.lisp` provides:

1. **`*test-settings*`** — JSON configuration pointing to
   `/tmp/sqlite/G2C.db`
2. **Truthset data** — 16 test records across 3 data sources (CUSTOMERS,
   REFERENCE, WATCHLIST) matching the Python SDK's test data
3. **`ensure-test-database`** — Idempotent setup that:
   - Copies the template DB from `/opt/senzing/er/resources/templates/G2C.db`
   - Registers a default config from template
   - Guards against re-initialization via `*test-database-initialized*`
4. **Component creation helpers** — `make-test-engine`, `make-test-product`,
   etc.
5. **Record management** — `add-truthset-records`, `delete-truthset-records`

### Important Caveat

FiveAM's `:depends-on` option does NOT work correctly in the version used by
this project — it treats the dependency symbol as a CL variable and fails.
Suite ordering is managed by file load order (`:serial t` in the ASDF
definition) rather than explicit dependencies.

### Running Tests

```bash
# Source your Senzing project environment (sets LD_LIBRARY_PATH, etc.)
# See: https://senzing.com/docs/quickstart/quickstart_api/
source ~/senzing/setupEnv

# Ensure SENZING_ENGINE_CONFIGURATION_JSON is set
# (setupEnv may handle this, or export it manually)

sbcl --non-interactive \
  --eval '(ql:quickload "cffi-libffi" :silent t)' \
  --eval '(push #p"/path/to/sz-sdk-lisp/" asdf:*central-registry*)' \
  --eval '(asdf:test-system "sz-sdk")'
```

---

## Future Work Catalog

All `FUTURE WORK` comments from the source, collected and categorized.

### 1. Phantom Object Refactoring

**Source:** [`factory.lisp:12`](../src/factory.lisp) (lines 12–34)
**Impact:** Breaking change to public API

Three alternatives to the current phantom object design:

- **(a)** Single `sz-context` object tracking all initialization state
- **(b)** Special variables checked by macros — eliminates object threading
- **(c)** Factory as primary handle — `(add-record factory ...)`

### 2. Generic-to-Defun Simplification

**Source:** [`packages.lisp:297`](../src/packages.lisp) (lines 290–299)
**Impact:** Breaking change (removes CLOS extensibility)

If alternative backends (gRPC, mocks) are never needed, generic functions
could be collapsed to plain `defun` for simplicity and reduced dispatch
overhead.

### 3. Exception Triple Elimination

**Source:** [`helpers.lisp:29`](../src/helpers.lisp) (lines 29–45)
**Impact:** Internal refactoring only

The `*get-exc-fn*` / `*clear-exc-fn*` / `*get-code-fn*` triple threaded
through every method call could be eliminated by having the result macros
close over the package-level variables at expansion time.

### 4. Yason as SDK Dependency

**Source:** [`helpers.lisp:133`](../src/helpers.lisp) (lines 133–135)
**Impact:** Adds dependency, simplifies internals

Adding `yason` as an SDK dependency would replace the hand-rolled
`%json-escape` and `build-*-json` functions with proper JSON
serialization/parsing.

### 5. Umbrella Package Re-export

**Source:** [`packages.lisp:412`](../src/packages.lisp) (lines 412–413)
**Impact:** Internal simplification only

Using `uiop:define-package` with `:reexport` would reduce the manual
symbol-by-symbol re-export listing in the umbrella package.
