# Python-to-Lisp Translation Methodology

How the Senzing Python SDK was translated into idiomatic Common Lisp.

**Audience:** Maintainers and contributors wanting to understand the
translation process, mapping decisions, and lessons learned.

> For the resulting API see the [API Reference](api-reference.md). For
> architecture details see the [Architecture Guide](guide.md).

---

## Table of Contents

- [Introduction](#introduction)
- [Resources Consulted](#resources-consulted)
- [Translation Methodology](#translation-methodology)
  - [Phase 1: API Surface Analysis](#phase-1-api-surface-analysis)
  - [Phase 2: FFI Binding Layer](#phase-2-ffi-binding-layer)
  - [Phase 3: Error Handling](#phase-3-error-handling)
  - [Phase 4: Core Design Decisions](#phase-4-core-design-decisions)
- [Mapping Decisions and Rationale](#mapping-decisions-and-rationale)
  - [Classes and Objects](#classes-and-objects)
  - [Error Handling](#error-handling)
  - [Concurrency](#concurrency)
  - [JSON](#json)
  - [Flags](#flags-mapping)
  - [Configuration and I/O](#configuration-and-io)
  - [Naming](#naming)
  - [Snippets](#snippets)
- [Lessons Learned](#lessons-learned)

---

## Introduction

The sz-sdk Common Lisp SDK is a re-expression of the
[Senzing Python SDK](https://github.com/senzing-garage/sz-sdk-python) in
Common Lisp idioms. The goal was not mechanical transliteration — it was to
produce an SDK that a Common Lisp programmer would expect, while maintaining
the same API surface, behavior, and error semantics as the Python original.

The translation followed several guiding principles:

1. **Idiomatic CL** — Use CLOS, conditions, generic functions, `unwind-protect`,
   and standard CL naming conventions rather than mimicking Python patterns
2. **Same API surface** — Every Python abstract method has a corresponding
   Lisp generic function with equivalent parameters and behavior
3. **Same error semantics** — The condition hierarchy exactly mirrors the
   Python exception hierarchy, with identical error code mappings
4. **Minimal dependencies** — Only `cffi` and `cffi-libffi` are required;
   `yason` and `lparallel` are optional (snippets only)
5. **Test parity** — The test suite covers the same operations and edge cases
   as the Python tests

---

## Resources Consulted

### Primary Sources

| Resource | Purpose |
|----------|---------|
| [sz-sdk-python](https://github.com/senzing-garage/sz-sdk-python) source | Abstract interfaces, concrete implementations, exception hierarchy, error code map, engine flags, constants |
| Senzing C headers (`SzLang_helpers.h`, etc.) | C function signatures, struct definitions, type mappings |
| [Senzing documentation](https://senzing.com/docs/) | API semantics, parameter meanings, expected behaviors |
| Python code snippets (sz-sdk-python-grpc) | Usage patterns, error handling, concurrency, configuration |

### Common Lisp References

| Resource | Purpose |
|----------|---------|
| [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/) | Language specification — conditions, packages, CLOS, type system |
| [Practical Common Lisp](https://gigamonkeys.com/book/) | Idiomatic patterns, macro design, condition system usage |
| [CFFI Manual](https://cffi.common-lisp.dev/manual/) | Foreign function bindings, struct definitions, memory management |
| cffi-libffi source | Struct-by-value return support |
| [lparallel documentation](https://lparallel.org/) | Futures, kernels, queues for concurrent snippets |
| [FiveAM documentation](https://common-lisp.net/project/fiveam/) | Test framework suites, checks, fixtures |

---

## Translation Methodology

### Phase 1: API Surface Analysis

The first step was cataloging every public method in the Python SDK's abstract
interfaces:

- `SzProductAbstract` → 2 methods
- `SzEngineAbstract` → 30 methods
- `SzConfigAbstract` → 7 methods
- `SzConfigManagerAbstract` → 9 methods
- `SzDiagnosticAbstract` → 4 methods
- `SzAbstractFactoryAbstract` → 7 methods (create/destroy/reinit)

For each method, the following was documented:

1. Python signature (parameter names, types, defaults)
2. Return type (string, int, None)
3. Which C functions it calls
4. Error conditions it can raise
5. Flag behavior (which flags affect the response)

### Phase 2: FFI Binding Layer

With the API surface mapped, the C headers were translated to CFFI bindings:

1. **Library loading** — `define-foreign-library` + `use-foreign-library`
2. **Struct definitions** — Three struct types for the three return patterns
3. **Per-function bindings** — Each C function got a `defcfun` with exact
   type mappings

**Key discovery:** Standard CFFI cannot handle struct-by-value returns. The
Senzing C helper functions return structs (not pointers to structs), which
requires `cffi-libffi` — a CFFI backend that uses `libffi` for proper ABI
compliance. This was not obvious from the CFFI documentation and required
experimentation.

**Type mapping decisions:**

| C Header | CFFI | Rationale |
|----------|------|-----------|
| `int64_t` | `:int64` | Exact match for error codes, entity IDs, config IDs |
| `size_t` | `:size` | Platform-appropriate unsigned integer |
| `uintptr_t` | `:uintptr` | Opaque handles (export, config) |
| `const char*` (input) | `:string` | CFFI auto-converts Lisp strings |
| `char*` (output) | `:pointer` | Manual extraction + free required |
| `char*` (static) | `:string` | Safe for `SzProduct_getLicense` etc. (static lifetime) |

### Phase 3: Error Handling

The Python exception hierarchy was translated to CL conditions:

1. **`szerror.py`** analyzed — base exception, category exceptions, detail
   exceptions
2. **Condition hierarchy** designed — `define-condition` with identical
   inheritance
3. **`define-sz-condition` macro** created — reduces boilerplate for the 14
   condition types
4. **Error code map** translated — 200+ entries from
   `ENGINE_EXCEPTION_MAP` → `*engine-exception-map*` hash table
5. **`check-result` function** created — bridges C return codes to CL
   conditions

### Phase 4: Core Design Decisions

With bindings and errors in place, the higher-level design was established:

1. **CLOS generics over defun** — To mirror Python's abstract interfaces and
   enable future backends
2. **Phantom objects** — To provide method dispatch targets without duplicating
   C's global state
3. **Factory pattern** — To match Python's `SzAbstractFactory` and provide
   RAII via `with-sz-factory`
4. **Per-component exception bindings** — To route each component's errors
   through the correct C exception functions
5. **Umbrella package** — To provide a single import for users, matching
   Python's `from senzing import *`

---

## Mapping Decisions and Rationale

### Classes and Objects

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `class SzAbstractFactoryCore` | `(defclass sz-abstract-factory ...)` | CLOS class with slots for settings |
| `class SzEngine(SzEngineAbstract)` | `(defclass sz-engine ...)` | Phantom object — see [guide](guide.md#phantom-objects) |
| Abstract methods | `(defgeneric ...)` | CLOS generics are CL's abstract methods |
| Concrete methods | `(defmethod ...)` | Single specialization on the CLOS class |
| `__init__(self, ...)` | `(initialize-engine engine ...)` | Explicit init, not constructor |
| `__del__(self)` | `(destroy-engine engine)` | Explicit destroy, not finalizer |
| `with SzAbstractFactory(...) as f:` | `(with-sz-factory (f ...) ...)` | `unwind-protect` replaces `__enter__`/`__exit__` |
| `factory.create_engine()` | `(create-engine factory)` | Generic function, not method call |
| Property access (`self.xxx`) | Reader functions | `(factory-instance-name factory)` |

**Why not use CL finalizers?** The C library's `destroy` functions must be
called deterministically (not at GC time). `unwind-protect` and the factory
macro guarantee timely cleanup.

### Error Handling

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `try: ... except SzError as e:` | `(handler-case ... (sz-error (e) ...))` | CL's condition system |
| `except SzBadInputError:` | `(sz-bad-input-error (e) ...)` | Condition hierarchy maps 1:1 |
| `raise SzNotFoundError(code, msg)` | `(error 'sz-not-found-error :error-code c :error-message m)` | `error` signals a condition |
| `e.message` | `(sz-error-message e)` | Reader function |
| `e.error_code` | `(sz-error-code e)` | Reader function |
| `ENGINE_EXCEPTION_MAP` (dict) | `*engine-exception-map*` (hash-table) | Same data structure, CL equivalent |
| Decorator-based validation | `check-not-destroyed` function | Called at method entry |

**Condition vs. exception terminology:** Common Lisp's condition system is
more general than Python's exceptions (conditions can be non-error signals,
and handlers can resume without unwinding the stack). The SDK only uses the
error-signaling subset, making the mapping straightforward.

### Concurrency

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `ThreadPoolExecutor(max_workers=N)` | `(lparallel:make-kernel N)` | Thread pool |
| `executor.submit(fn, *args)` | `(lparallel:future (fn args))` | Submit work |
| `future.result()` | `(lparallel:force future)` | Block for result |
| `concurrent.futures.as_completed(fs)` | `(lparallel:fulfilledp f)` poll loop | No direct equivalent — poll + sleep |
| `queue.Queue()` | `(lparallel.queue:make-queue)` | Thread-safe queue |
| `queue.put(item)` | `(lparallel.queue:push-queue item q)` | Enqueue |
| `queue.get()` | `(lparallel.queue:pop-queue q)` | Dequeue (blocking) |
| Context manager cleanup | `(unwind-protect ... (lparallel:end-kernel))` | Kernel cleanup |

**Why lparallel?** It's the most mature and well-documented parallel
programming library for CL. Its futures/kernel model maps naturally to
Python's `ThreadPoolExecutor`. It's only loaded by snippets that need it — not
an SDK dependency.

### JSON

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `json.loads(string)` | `(yason:parse string)` | Returns hash-table (like Python dict) |
| `json.dumps(obj)` | `(yason:encode obj stream)` | Or `with-output-to-string` |
| `f'{"key": "{value}"}'` | `(format nil "{\"key\":\"~A\"}" value)` | FORMAT for interpolation |
| `dict` access | `(gethash key ht)` | Hash-table access |

**Why not yason as an SDK dependency?** The SDK's internal JSON needs are
simple (building 3-4 JSON shapes for list parameters). Hand-rolled `FORMAT`
strings with `%json-escape` are sufficient. Adding yason would increase the
dependency footprint for all users when only snippet authors need full JSON
parsing. See [Future Work](guide.md#future-work-catalog) for the yason
consideration.

### Flags Mapping

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `SzEngineFlags.SZ_WITH_INFO` | `+sz-with-info+` | `+earmuffs+` for constants |
| `flags \| SZ_WITH_INFO` | `(logior flags +sz-with-info+)` | CL bitwise OR |
| `flags & SZ_WITH_INFO` | `(logtest flags +sz-with-info+)` | CL bitwise test (returns T/NIL) |
| `class SzEngineFlags:` | `(defconstant ...)` in `sz-sdk.flags` | Package-level constants |
| `SZ_NO_FLAGS = 0` | `(defconstant +sz-no-flags+ 0)` | Same value |
| Flag type annotations | `(declaim (type (unsigned-byte 64) ...))` | Compiler optimization |

**Naming convention:** Python's `SZ_EXPORT_INCLUDE_ALL_ENTITIES` becomes
`+sz-export-include-all-entities+`. The mapping is mechanical:
`UPPER_SNAKE_CASE` → `+lower-kebab-case+`.

### Configuration and I/O

| Python | Common Lisp | Rationale |
|--------|-------------|-----------|
| `os.environ["VAR"]` | `(uiop:getenv "VAR")` | UIOP is universally available |
| `os.environ.get("VAR", default)` | `(or (uiop:getenv "VAR") default)` | `or` for defaults |
| `configparser` (ini files) | Custom `ini-to-json` in snippet | No standard CL ini parser; hand-rolled for one snippet |
| `signal.signal(SIGINT, handler)` | `(sb-sys:enable-interrupt :sigint handler)` | SBCL-specific |
| `pathlib.Path` | `(merge-pathnames ...)` | CL pathname system |
| `shutil.copy2()` | `(uiop:copy-file src dst)` | UIOP file operations |
| `os.makedirs()` | `(ensure-directories-exist path)` | CL standard function |

### Naming

| Python Pattern | CL Pattern | Example |
|----------------|------------|---------|
| `snake_case` functions | `kebab-case` | `add_record` → `add-record` |
| `CamelCase` classes | `kebab-case` | `SzEngine` → `sz-engine` |
| `_private` methods | `%internal` | `_strip_flags` → `%strip-sdk-flags` |
| `UPPER_CASE` constants | `+earmuff-case+` | `SZ_NO_FLAGS` → `+sz-no-flags+` |
| `*dynamic*` variables | `*earmuff-case*` | Not common in Python |
| C `CamelCase` functions | `%kebab-case` bindings | `Sz_addRecord` → `%sz-add-record` |
| Module-level functions | Package-qualified | `szerror.lookup_exception_class` → `sz-sdk.conditions:lookup-exception-class` |

### Snippets

| Python Pattern | CL Pattern | Rationale |
|----------------|------------|-----------|
| `with` blocks | `unwind-protect` | Deterministic cleanup |
| `dict` | alist or hash-table | alists for small configs, hash-tables for JSON |
| `print(...)` | `(format t "~A~%" ...)` | FORMAT for output |
| `f"..."` | `(format nil "..." ...)` | FORMAT directives |
| `argparse` | `(uiop:command-line-arguments)` | UIOP for portable CLI args |
| `if __name__ == "__main__":` | `(unless (find :swank *features*) (main))` | Skip if in SLIME/Sly REPL |
| Module imports | `(defpackage (:use #:sz-sdk))` | Package system |
| `requirements.txt` | `(ql:quickload ...)` | Quicklisp on demand |

Each of the 36 Python code snippets has a corresponding CL snippet in
[`snippets/`](../snippets/). They share a common bootstrap
([`snippets/common/setup.lisp`](../snippets/common/setup.lisp)) that loads
the SDK and makes it available.

---

## Lessons Learned

### 1. cffi-libffi is Required for Struct-by-Value Returns

**Problem:** The Senzing C helper functions return structs by value (not
pointers). Standard CFFI does not support this — it silently produces
incorrect results or crashes.

**Solution:** Use `cffi-libffi`, which leverages `libffi` for proper
struct-by-value ABI support. This requires `libffi-dev` to be installed on the
system.

**Lesson:** When wrapping a C library that returns structs by value, test early
with actual function calls. The CFFI documentation mentions this limitation
but it's easy to miss.

### 2. `defconstant` and String EQL

**Problem:** CL's `defconstant` requires the value to be `eql` on
redefinition. Strings are only `equal`, not `eql`, across compilations. This
causes errors when reloading files:

```lisp
;; This will fail on reload:
(defconstant +foo+ "bar")  ; "bar" not EQL to previous "bar"
```

**Solution:** Use `defparameter` for string constants, with the `+...+`
naming convention to signal intent. Integer constants can safely use
`defconstant`.

**Lesson:** This is a well-known CL gotcha but easily forgotten. The
`+earmuffs+` convention lets you communicate "treat as constant" regardless of
the underlying definition form.

### 3. FiveAM `:depends-on` Bug

**Problem:** FiveAM's `:depends-on` option for test suites does not work
correctly in the version used. It treats the dependency name as a CL variable
and fails at runtime.

**Solution:** Avoid `:depends-on` entirely. Rely on `:serial t` in the ASDF
system definition to ensure tests run in the correct order.

**Lesson:** Test framework features that aren't widely used may have bugs.
When a feature doesn't work, use simpler alternatives rather than debugging
the framework.

### 4. Template Database Has No Registered Config

**Problem:** A fresh copy of Senzing's template database
(`/opt/senzing/er/resources/templates/G2C.db`) has no registered
configuration. The engine and diagnostic subsystems require a registered
default config to initialize. Tests that copied the template DB and
immediately tried to initialize the engine would fail.

**Solution:** `ensure-test-database` in `test-setup.lisp` copies the template
DB and then registers a default configuration from the template before any
engine or diagnostic initialization.

**Lesson:** When translating test infrastructure, don't assume the test
database is pre-configured. The Python tests may have handled this differently
(e.g., through a conftest.py fixture that runs once per session).

### 5. The Condition System is More Powerful Than Needed

**Problem:** CL's condition system supports restarts, handler-bind (non-unwinding
handlers), and interactive debugger integration. The Python SDK only uses
simple raise/except patterns.

**Decision:** Use only `handler-case` (equivalent to `try/except`) and
`error` (equivalent to `raise`) for simplicity. The full condition system
power (restarts, `handler-bind`) is available to users who want it, but the
SDK doesn't require it.

**Lesson:** When translating from a simpler error model, stick to the
equivalent subset of the richer system. Users who know the advanced features
can use them; users coming from Python will find the familiar patterns.

### 6. Generic Functions vs. Plain Functions

**Decision:** Use `defgeneric`/`defmethod` throughout, even though there's
currently only one specialization per generic (on the phantom object class).

**Rationale:** The Python SDK uses abstract base classes to enable pluggable
backends (gRPC transport, mock implementations). CLOS generics are the natural
CL equivalent. Even if alternative backends are never implemented, the
generics cost negligible runtime overhead and maintain API compatibility with
the upstream design.

**Trade-off:** If backends are never needed, the generics add indirection
without benefit. They could be simplified to `defun` in the future without
changing the calling convention (only the definition syntax changes).

### 7. Umbrella Package Design

**Decision:** Create an `sz-sdk` umbrella package that re-exports symbols
from all component packages, excluding lifecycle methods.

**Rationale:**
- Users only need `(:use #:sz-sdk)` — one import for everything
- Lifecycle methods (`initialize-*`, `destroy-*`) are excluded because users
  should use the factory pattern
- Component packages remain accessible for advanced use cases

**Trade-off:** Manual re-export is tedious (every symbol listed twice). A
future improvement could use `uiop:define-package :reexport`.

### 8. Delete-Record Idempotency

**Discovery:** Senzing's `delete-record` does not signal an error when
deleting a non-existent record. This differs from some databases and ORMs
where deleting a missing record raises "not found."

**Implication:** Test cleanup code can safely call `delete-record` without
checking whether records exist. The Lisp SDK preserves this behavior.
