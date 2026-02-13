# Senzing C API Reference

Mapping of the Senzing C library (`libSz.so`) through CFFI bindings to the
public Common Lisp API.

**Audience:** Developers maintaining or extending the CFFI bindings layer.

> For the public Lisp API see the [API Reference](api-reference.md). For
> architectural context see the [Architecture Guide](guide.md).

---

## Table of Contents

- [Overview](#overview)
- [Return Type Structures](#return-type-structures)
- [Memory Management](#memory-management)
- [Type Mapping](#type-mapping)
- [SzProduct](#szproduct) (9 functions)
- [Sz (Engine)](#sz-engine) (40 functions)
- [SzConfig](#szconfig) (12 functions)
- [SzConfigMgr](#szconfigmgr) (10 functions)
- [SzDiagnostic](#szdiagnostic) (11 functions)
- [Engine Flags](#engine-flags)
- [Function Consolidation](#function-consolidation)

---

## Overview

The SDK wraps 82 C functions from `libSz.so` (plus the `SzHelper_free`
utility) through a three-layer mapping:

```
C function (libSz.so)
    ↓ defcfun
CFFI binding (sz-sdk.bindings package, %kebab-case name)
    ↓ result macros + error checking
Public Lisp API (sz-sdk package, kebab-case generic functions)
```

**Naming convention:** C `CamelCase` function names are translated to
`%kebab-case` CFFI binding names (with `%` prefix indicating internal use).
The `%` prefix is the Common Lisp convention for internal/private functions.

**Library loading** (`library.lisp`):

```lisp
(define-foreign-library libsz
  (:unix (:or "libSz.so" "/opt/senzing/er/lib/libSz.so"))
  (t (:default "libSz")))

(use-foreign-library libsz)
```

The library requires `cffi-libffi` because several C helper functions return
structs by value, which standard CFFI cannot handle. The `cffi-libffi` backend
uses `libffi` for proper struct-by-value returns.

---

## Return Type Structures

The C helper layer (`SzLang_helpers.h`) wraps core C functions that return
results through output parameters. Helpers return structs by value containing
both the result and a return code.

### `sz-string-result`

Most common return type. Used by all functions that return JSON strings.

| C Struct Field | C Type | CFFI Type | Description |
|----------------|--------|-----------|-------------|
| `response` | `char*` | `:pointer` | Pointer to heap-allocated UTF-8 string |
| `returnCode` | `int64_t` | `:int64` | 0 = success, non-zero = error code |

```lisp
(defcstruct sz-string-result
  (response :pointer)
  (return-code :int64))
```

**Handling macro:** `with-sz-string-result` — extracts the string, checks the
return code, and frees the C memory via `%sz-helper-free` in `unwind-protect`.

### `sz-int64-result`

Used by functions returning integer values (config IDs, etc.).

| C Struct Field | C Type | CFFI Type | Description |
|----------------|--------|-----------|-------------|
| `response` | `int64_t` | `:int64` | The integer result value |
| `returnCode` | `int64_t` | `:int64` | 0 = success, non-zero = error code |

```lisp
(defcstruct sz-int64-result
  (response :int64)
  (return-code :int64))
```

**Handling macro:** `with-sz-int64-result` — extracts the integer and checks
the return code. No memory freeing needed.

### `sz-handle-result`

Used by functions returning opaque handles (export handles, config handles).

| C Struct Field | C Type | CFFI Type | Description |
|----------------|--------|-----------|-------------|
| `response` | `uintptr_t` | `:uintptr` | Opaque handle value |
| `returnCode` | `int64_t` | `:int64` | 0 = success, non-zero = error code |

```lisp
(defcstruct sz-handle-result
  (response :uintptr)
  (return-code :int64))
```

**Handling macro:** `with-sz-handle-result` — extracts the handle and checks
the return code. Handle must be closed by the caller.

---

## Memory Management

### String Results

C helper functions allocate response strings on the heap. The Lisp SDK must
free them after extraction:

```
1. Call C helper → returns sz-string-result struct
2. Extract response pointer and return-code from plist
3. Convert pointer to Lisp string (cffi:foreign-string-to-lisp)
4. Free the C memory via SzHelper_free (always, in unwind-protect)
5. Check return code, signal condition if non-zero
```

**`SzHelper_free`** / `%sz-helper-free`:

```lisp
(defcfun ("SzHelper_free" %sz-helper-free) :void
  (ptr :pointer))
```

Frees memory allocated by any Senzing helper function. Called in
`unwind-protect` to guarantee cleanup even on error.

### Config Handles

Config handles from `SzConfig_create_helper` / `SzConfig_load_helper` must be
closed with `SzConfig_close_helper`:

```lisp
(with-sz-handle-result (config-handle
                        (%sz-config-create-helper)
                        ...)
  (unwind-protect
       ;; Use config-handle...
       (%sz-config-export-helper config-handle)
    ;; Always close
    (%sz-config-close-helper config-handle)))
```

### Export Handles

Export handles from `Sz_exportCSVEntityReport_helper` /
`Sz_exportJSONEntityReport_helper` must be closed with
`Sz_closeExportReport_helper`.

---

## Type Mapping

| C Type | CFFI Type | CL Type | Notes |
|--------|-----------|---------|-------|
| `int64_t` | `:int64` | `(signed-byte 64)` | Return codes, entity IDs, config IDs |
| `size_t` | `:size` | `(unsigned-byte 64)` | Buffer sizes |
| `uintptr_t` | `:uintptr` | `(unsigned-byte 64)` | Opaque handles (export, config) |
| `const char*` | `:string` | `string` | Input strings (auto-converted by CFFI) |
| `char*` | `:pointer` | — | Output strings (manual extraction + free) |
| `char*` (static) | `:string` | `string` | Static returns like `SzProduct_getLicense` |
| `void` | `:void` | — | Functions with no return value |

---

## SzProduct

9 C functions for product information and license validation.

### Lifecycle

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzProduct_init` | `%sz-product-init` | instance-name `:string`, settings `:string`, verbose-logging `:int64` | `:int64` | `initialize-product` (internal) |
| `SzProduct_destroy` | `%sz-product-destroy` | (none) | `:int64` | `destroy-product` (internal) |

### Product Information

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzProduct_getLicense` | `%sz-product-get-license` | (none) | `:string` (static) | `get-license` |
| `SzProduct_getVersion` | `%sz-product-get-version` | (none) | `:string` (static) | `get-version` |

Note: `getLicense` and `getVersion` return static C strings, not
helper-allocated memory. CFFI's `:string` return type handles the conversion
directly — no freeing needed.

### License Validation

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzProduct_validateLicenseFile_helper` | `%sz-product-validate-license-file-helper` | license-file-path `:string` | `sz-string-result` | (not exposed) |
| `SzProduct_validateLicenseStringBase64_helper` | `%sz-product-validate-license-string-base64-helper` | license-string `:string` | `sz-string-result` | (not exposed) |

### Exception Handling

| C Function | CFFI Binding | Parameters | Return |
|------------|--------------|------------|--------|
| `SzProduct_getLastException` | `%sz-product-get-last-exception` | buf `:pointer`, buf-size `:size` | `:int64` |
| `SzProduct_clearLastException` | `%sz-product-clear-last-exception` | (none) | `:void` |
| `SzProduct_getLastExceptionCode` | `%sz-product-get-last-exception-code` | (none) | `:int64` |

---

## Sz (Engine)

40 C functions for the entity resolution engine — the largest component.

### Lifecycle

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_init` | `%sz-init` | instance-name `:string`, settings `:string`, verbose-logging `:int64` | `:int64` | `initialize-engine` (internal) |
| `Sz_initWithConfigID` | `%sz-init-with-config-id` | instance-name `:string`, settings `:string`, config-id `:int64`, verbose-logging `:int64` | `:int64` | `initialize-engine` with `:config-id` |
| `Sz_destroy` | `%sz-destroy` | (none) | `:int64` | `destroy-engine` (internal) |
| `Sz_primeEngine` | `%sz-prime-engine` | (none) | `:int64` | `prime-engine` |
| `Sz_reinit` | `%sz-reinit` | config-id `:int64` | `:int64` | `reinitialize-engine` (internal) |

### Exception Handling

| C Function | CFFI Binding | Parameters | Return |
|------------|--------------|------------|--------|
| `Sz_getLastException` | `%sz-get-last-exception` | buf `:pointer`, buf-size `:size` | `:int64` |
| `Sz_clearLastException` | `%sz-clear-last-exception` | (none) | `:void` |
| `Sz_getLastExceptionCode` | `%sz-get-last-exception-code` | (none) | `:int64` |

### Record Operations

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_addRecord` | `%sz-add-record` | data-source-code `:string`, record-id `:string`, json-data `:string` | `:int64` | `add-record` (no info) |
| `Sz_addRecordWithInfo_helper` | `%sz-add-record-with-info-helper` | data-source-code `:string`, record-id `:string`, json-data `:string`, flags `:int64` | `sz-string-result` | `add-record` (with `+sz-with-info+`) |
| `Sz_deleteRecord` | `%sz-delete-record` | data-source-code `:string`, record-id `:string` | `:int64` | `delete-record` (no info) |
| `Sz_deleteRecordWithInfo_helper` | `%sz-delete-record-with-info-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `delete-record` (with `+sz-with-info+`) |

### Entity Retrieval

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_getEntityByEntityID_V2_helper` | `%sz-get-entity-by-entity-id-v2-helper` | entity-id `:int64`, flags `:int64` | `sz-string-result` | `get-entity-by-entity-id` |
| `Sz_getEntityByRecordID_V2_helper` | `%sz-get-entity-by-record-id-v2-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `get-entity-by-record-id` |
| `Sz_getRecord_V2_helper` | `%sz-get-record-v2-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `get-record` |
| `Sz_getRecordPreview_helper` | `%sz-get-record-preview-helper` | json-data `:string`, flags `:int64` | `sz-string-result` | `get-record-preview` |
| `Sz_getRedoRecord_helper` | `%sz-get-redo-record-helper` | (none) | `sz-string-result` | `get-redo-record` |
| `Sz_getVirtualEntityByRecordID_V2_helper` | `%sz-get-virtual-entity-by-record-id-v2-helper` | record-list `:string`, flags `:int64` | `sz-string-result` | `get-virtual-entity-by-record-id` |

### Search

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_searchByAttributes_V3_helper` | `%sz-search-by-attributes-v3-helper` | json-data `:string`, profile `:string`, flags `:int64` | `sz-string-result` | `search-by-attributes` |

### Redo Queue

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_countRedoRecords` | `%sz-count-redo-records` | (none) | `:int64` | `count-redo-records` |
| `Sz_processRedoRecord` | `%sz-process-redo-record` | json-data `:string` | `:int64` | `process-redo-record` (no info) |
| `Sz_processRedoRecordWithInfo_helper` | `%sz-process-redo-record-with-info-helper` | json-data `:string` | `sz-string-result` | `process-redo-record` (with `+sz-with-info+`) |

### Reevaluation

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_reevaluateEntity` | `%sz-reevaluate-entity` | entity-id `:int64`, flags `:int64` | `:int64` | `reevaluate-entity` (no info) |
| `Sz_reevaluateEntityWithInfo_helper` | `%sz-reevaluate-entity-with-info-helper` | entity-id `:int64`, flags `:int64` | `sz-string-result` | `reevaluate-entity` (with `+sz-with-info+`) |
| `Sz_reevaluateRecord` | `%sz-reevaluate-record` | data-source-code `:string`, record-id `:string`, flags `:int64` | `:int64` | `reevaluate-record` (no info) |
| `Sz_reevaluateRecordWithInfo_helper` | `%sz-reevaluate-record-with-info-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `reevaluate-record` (with `+sz-with-info+`) |

### Export

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_exportCSVEntityReport_helper` | `%sz-export-csv-entity-report-helper` | csv-column-list `:string`, flags `:int64` | `sz-handle-result` | `export-csv-entity-report` |
| `Sz_exportJSONEntityReport_helper` | `%sz-export-json-entity-report-helper` | flags `:int64` | `sz-handle-result` | `export-json-entity-report` |
| `Sz_fetchNext_helper` | `%sz-fetch-next-helper` | export-handle `:uintptr` | `sz-string-result` | `fetch-next` |
| `Sz_closeExportReport_helper` | `%sz-close-export-report-helper` | export-handle `:uintptr` | `:int64` | `close-export-report` |

### Path Finding

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_findPathByEntityID_V2_helper` | `%sz-find-path-by-entity-id-v2-helper` | entity-id-1 `:int64`, entity-id-2 `:int64`, max-degree `:int64`, flags `:int64` | `sz-string-result` | `find-path-by-entity-id` (simple) |
| `Sz_findPathByEntityIDWithAvoids_V2_helper` | `%sz-find-path-by-entity-id-with-avoids-v2-helper` | entity-id-1 `:int64`, entity-id-2 `:int64`, max-degree `:int64`, avoided-entities `:string`, flags `:int64` | `sz-string-result` | `find-path-by-entity-id` (with avoids) |
| `Sz_findPathByEntityIDIncludingSource_V2_helper` | `%sz-find-path-by-entity-id-including-source-v2-helper` | entity-id-1 `:int64`, entity-id-2 `:int64`, max-degree `:int64`, avoided-entities `:string`, required-dsrcs `:string`, flags `:int64` | `sz-string-result` | `find-path-by-entity-id` (with sources) |
| `Sz_findPathByRecordID_V2_helper` | `%sz-find-path-by-record-id-v2-helper` | dsrc-1 `:string`, rec-1 `:string`, dsrc-2 `:string`, rec-2 `:string`, max-degree `:int64`, flags `:int64` | `sz-string-result` | `find-path-by-record-id` (simple) |
| `Sz_findPathByRecordIDWithAvoids_V2_helper` | `%sz-find-path-by-record-id-with-avoids-v2-helper` | dsrc-1 `:string`, rec-1 `:string`, dsrc-2 `:string`, rec-2 `:string`, max-degree `:int64`, avoided-records `:string`, flags `:int64` | `sz-string-result` | `find-path-by-record-id` (with avoids) |
| `Sz_findPathByRecordIDIncludingSource_V2_helper` | `%sz-find-path-by-record-id-including-source-v2-helper` | dsrc-1 `:string`, rec-1 `:string`, dsrc-2 `:string`, rec-2 `:string`, max-degree `:int64`, avoided-records `:string`, required-dsrcs `:string`, flags `:int64` | `sz-string-result` | `find-path-by-record-id` (with sources) |

### Network Finding

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_findNetworkByEntityID_V2_helper` | `%sz-find-network-by-entity-id-v2-helper` | entity-list `:string`, max-degree `:int64`, build-out-degree `:int64`, max-entities `:int64`, flags `:int64` | `sz-string-result` | `find-network-by-entity-id` |
| `Sz_findNetworkByRecordID_V2_helper` | `%sz-find-network-by-record-id-v2-helper` | record-list `:string`, max-degree `:int64`, build-out-degree `:int64`, max-entities `:int64`, flags `:int64` | `sz-string-result` | `find-network-by-record-id` |

### Interesting Entities

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_findInterestingEntitiesByEntityID_helper` | `%sz-find-interesting-entities-by-entity-id-helper` | entity-id `:int64`, flags `:int64` | `sz-string-result` | `find-interesting-entities-by-entity-id` |
| `Sz_findInterestingEntitiesByRecordID_helper` | `%sz-find-interesting-entities-by-record-id-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `find-interesting-entities-by-record-id` |

### Why / How Analysis

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_whyEntities_V2_helper` | `%sz-why-entities-v2-helper` | entity-id-1 `:int64`, entity-id-2 `:int64`, flags `:int64` | `sz-string-result` | `why-entities` |
| `Sz_whyRecords_V2_helper` | `%sz-why-records-v2-helper` | dsrc-1 `:string`, rec-1 `:string`, dsrc-2 `:string`, rec-2 `:string`, flags `:int64` | `sz-string-result` | `why-records` |
| `Sz_whyRecordInEntity_V2_helper` | `%sz-why-record-in-entity-v2-helper` | data-source-code `:string`, record-id `:string`, flags `:int64` | `sz-string-result` | `why-record-in-entity` |
| `Sz_whySearch_V2_helper` | `%sz-why-search-v2-helper` | json-data `:string`, entity-id `:int64`, search-profile `:string`, flags `:int64` | `sz-string-result` | `why-search` |
| `Sz_howEntityByEntityID_V2_helper` | `%sz-how-entity-by-entity-id-v2-helper` | entity-id `:int64`, flags `:int64` | `sz-string-result` | `how-entity-by-entity-id` |

### Statistics / Config

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `Sz_getActiveConfigID_helper` | `%sz-get-active-config-id-helper` | (none) | `sz-int64-result` | `get-active-config-id` |
| `Sz_stats_helper` | `%sz-stats-helper` | (none) | `sz-string-result` | `get-stats` |

---

## SzConfig

12 C functions for in-memory configuration editing.

### Lifecycle

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfig_init` | `%sz-config-init` | instance-name `:string`, settings `:string`, verbose-logging `:int64` | `:int64` | `initialize-config` (internal) |
| `SzConfig_destroy` | `%sz-config-destroy` | (none) | `:int64` | `destroy-config` (internal) |

### Config Handle Management

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfig_create_helper` | `%sz-config-create-helper` | (none) | `sz-handle-result` | `import-template` (internally) |
| `SzConfig_load_helper` | `%sz-config-load-helper` | input-json `:string` | `sz-handle-result` | Various config methods (internally) |
| `SzConfig_export_helper` | `%sz-config-export-helper` | config-handle `:uintptr` | `sz-string-result` | Various config methods (internally) |
| `SzConfig_close_helper` | `%sz-config-close-helper` | config-handle `:uintptr` | `:int64` | Various config methods (internally) |

Note: Config handle management is fully internal. The public `sz-config` API
abstracts away handle creation/closing with automatic `unwind-protect` cleanup.

### Data Source Operations

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfig_getDataSourceRegistry_helper` | `%sz-config-get-data-source-registry-helper` | config-handle `:uintptr` | `sz-string-result` | `get-data-source-registry` |
| `SzConfig_registerDataSource_helper` | `%sz-config-register-data-source-helper` | config-handle `:uintptr`, input-json `:string` | `sz-string-result` | `register-data-source` |
| `SzConfig_unregisterDataSource_helper` | `%sz-config-unregister-data-source-helper` | config-handle `:uintptr`, input-json `:string` | `:int64` | `unregister-data-source` |

### Exception Handling

| C Function | CFFI Binding | Parameters | Return |
|------------|--------------|------------|--------|
| `SzConfig_getLastException` | `%sz-config-get-last-exception` | buf `:pointer`, buf-size `:size` | `:int64` |
| `SzConfig_clearLastException` | `%sz-config-clear-last-exception` | (none) | `:void` |
| `SzConfig_getLastExceptionCode` | `%sz-config-get-last-exception-code` | (none) | `:int64` |

---

## SzConfigMgr

10 C functions for persistent configuration management.

### Lifecycle

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfigMgr_init` | `%sz-config-mgr-init` | instance-name `:string`, settings `:string`, verbose-logging `:int64` | `:int64` | `initialize-config-manager` (internal) |
| `SzConfigMgr_destroy` | `%sz-config-mgr-destroy` | (none) | `:int64` | `destroy-config-manager` (internal) |

### Config Registration

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfigMgr_registerConfig_helper` | `%sz-config-mgr-register-config-helper` | config-str `:string`, config-comments `:string` | `sz-int64-result` | `register-config` |

### Config Retrieval

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfigMgr_getConfig_helper` | `%sz-config-mgr-get-config-helper` | config-id `:int64` | `sz-string-result` | `create-config-from-config-id` (internally) |
| `SzConfigMgr_getConfigRegistry_helper` | `%sz-config-mgr-get-config-registry-helper` | (none) | `sz-string-result` | `get-config-registry` |

### Default Config ID

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzConfigMgr_getDefaultConfigID_helper` | `%sz-config-mgr-get-default-config-id-helper` | (none) | `sz-int64-result` | `get-default-config-id` |
| `SzConfigMgr_setDefaultConfigID` | `%sz-config-mgr-set-default-config-id` | config-id `:int64` | `:int64` | `set-default-config-id` |
| `SzConfigMgr_replaceDefaultConfigID` | `%sz-config-mgr-replace-default-config-id` | current-default-config-id `:int64`, new-default-config-id `:int64` | `:int64` | `replace-default-config-id` |

### Exception Handling

| C Function | CFFI Binding | Parameters | Return |
|------------|--------------|------------|--------|
| `SzConfigMgr_getLastException` | `%sz-config-mgr-get-last-exception` | buf `:pointer`, buf-size `:size` | `:int64` |
| `SzConfigMgr_clearLastException` | `%sz-config-mgr-clear-last-exception` | (none) | `:void` |
| `SzConfigMgr_getLastExceptionCode` | `%sz-config-mgr-get-last-exception-code` | (none) | `:int64` |

---

## SzDiagnostic

11 C functions for repository diagnostics and maintenance.

### Lifecycle

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzDiagnostic_init` | `%sz-diagnostic-init` | instance-name `:string`, settings `:string`, verbose-logging `:int64` | `:int64` | `initialize-diagnostic` (internal) |
| `SzDiagnostic_initWithConfigID` | `%sz-diagnostic-init-with-config-id` | instance-name `:string`, settings `:string`, config-id `:int64`, verbose-logging `:int64` | `:int64` | `initialize-diagnostic` with `:config-id` |
| `SzDiagnostic_destroy` | `%sz-diagnostic-destroy` | (none) | `:int64` | `destroy-diagnostic` (internal) |
| `SzDiagnostic_reinit` | `%sz-diagnostic-reinit` | config-id `:int64` | `:int64` | `reinitialize-diagnostic` (internal) |

### Repository Operations

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzDiagnostic_purgeRepository` | `%sz-diagnostic-purge-repository` | (none) | `:int64` | `purge-repository` |
| `SzDiagnostic_checkRepositoryPerformance_helper` | `%sz-diagnostic-check-repository-performance-helper` | seconds-to-run `:int64` | `sz-string-result` | `check-repository-performance` |
| `SzDiagnostic_getRepositoryInfo_helper` | `%sz-diagnostic-get-repository-info-helper` | (none) | `sz-string-result` | `get-repository-info` |

### Feature (Undocumented)

| C Function | CFFI Binding | Parameters | Return | Public API |
|------------|--------------|------------|--------|------------|
| `SzDiagnostic_getFeature_helper` | `%sz-diagnostic-get-feature-helper` | lib-feat-id `:int64` | `sz-string-result` | `get-feature` |

### Exception Handling

| C Function | CFFI Binding | Parameters | Return |
|------------|--------------|------------|--------|
| `SzDiagnostic_getLastException` | `%sz-diagnostic-get-last-exception` | buf `:pointer`, buf-size `:size` | `:int64` |
| `SzDiagnostic_clearLastException` | `%sz-diagnostic-clear-last-exception` | (none) | `:void` |
| `SzDiagnostic_getLastExceptionCode` | `%sz-diagnostic-get-last-exception-code` | (none) | `:int64` |

---

## Engine Flags

All flags are `(unsigned-byte 64)` constants defined in
[`src/flags.lisp`](../src/flags.lisp). The C library uses the same bit
positions.

| C Name | Lisp Name | Bit | Value |
|--------|-----------|-----|-------|
| `SZ_NO_FLAGS` | `+sz-no-flags+` | — | `0` |
| `SZ_INCLUDE_FEATURE_SCORES` | `+sz-include-feature-scores+` | 26 | `67108864` |
| `SZ_INCLUDE_MATCH_KEY_DETAILS` | `+sz-include-match-key-details+` | 34 | `17179869184` |
| `SZ_EXPORT_INCLUDE_MULTI_RECORD_ENTITIES` | `+sz-export-include-multi-record-entities+` | 0 | `1` |
| `SZ_EXPORT_INCLUDE_POSSIBLY_SAME` | `+sz-export-include-possibly-same+` | 1 | `2` |
| `SZ_EXPORT_INCLUDE_POSSIBLY_RELATED` | `+sz-export-include-possibly-related+` | 2 | `4` |
| `SZ_EXPORT_INCLUDE_NAME_ONLY` | `+sz-export-include-name-only+` | 3 | `8` |
| `SZ_EXPORT_INCLUDE_DISCLOSED` | `+sz-export-include-disclosed+` | 4 | `16` |
| `SZ_EXPORT_INCLUDE_SINGLE_RECORD_ENTITIES` | `+sz-export-include-single-record-entities+` | 5 | `32` |
| `SZ_ENTITY_INCLUDE_POSSIBLY_SAME_RELATIONS` | `+sz-entity-include-possibly-same-relations+` | 6 | `64` |
| `SZ_ENTITY_INCLUDE_POSSIBLY_RELATED_RELATIONS` | `+sz-entity-include-possibly-related-relations+` | 7 | `128` |
| `SZ_ENTITY_INCLUDE_NAME_ONLY_RELATIONS` | `+sz-entity-include-name-only-relations+` | 8 | `256` |
| `SZ_ENTITY_INCLUDE_DISCLOSED_RELATIONS` | `+sz-entity-include-disclosed-relations+` | 9 | `512` |
| `SZ_ENTITY_INCLUDE_ALL_FEATURES` | `+sz-entity-include-all-features+` | 10 | `1024` |
| `SZ_ENTITY_INCLUDE_REPRESENTATIVE_FEATURES` | `+sz-entity-include-representative-features+` | 11 | `2048` |
| `SZ_ENTITY_INCLUDE_ENTITY_NAME` | `+sz-entity-include-entity-name+` | 12 | `4096` |
| `SZ_ENTITY_INCLUDE_RECORD_SUMMARY` | `+sz-entity-include-record-summary+` | 13 | `8192` |
| `SZ_ENTITY_INCLUDE_RECORD_DATA` | `+sz-entity-include-record-data+` | 14 | `16384` |
| `SZ_ENTITY_INCLUDE_RECORD_MATCHING_INFO` | `+sz-entity-include-record-matching-info+` | 15 | `32768` |
| `SZ_ENTITY_INCLUDE_RECORD_JSON_DATA` | `+sz-entity-include-record-json-data+` | 16 | `65536` |
| `SZ_ENTITY_INCLUDE_RECORD_FEATURES` | `+sz-entity-include-record-features+` | 18 | `262144` |
| `SZ_ENTITY_INCLUDE_RELATED_ENTITY_NAME` | `+sz-entity-include-related-entity-name+` | 19 | `524288` |
| `SZ_ENTITY_INCLUDE_RELATED_MATCHING_INFO` | `+sz-entity-include-related-matching-info+` | 20 | `1048576` |
| `SZ_ENTITY_INCLUDE_RELATED_RECORD_SUMMARY` | `+sz-entity-include-related-record-summary+` | 21 | `2097152` |
| `SZ_ENTITY_INCLUDE_RELATED_RECORD_DATA` | `+sz-entity-include-related-record-data+` | 22 | `4194304` |
| `SZ_ENTITY_INCLUDE_INTERNAL_FEATURES` | `+sz-entity-include-internal-features+` | 23 | `8388608` |
| `SZ_ENTITY_INCLUDE_FEATURE_STATS` | `+sz-entity-include-feature-stats+` | 24 | `16777216` |
| `SZ_FIND_PATH_STRICT_AVOID` | `+sz-find-path-strict-avoid+` | 25 | `33554432` |
| `SZ_SEARCH_INCLUDE_STATS` | `+sz-search-include-stats+` | 27 | `134217728` |
| `SZ_ENTITY_INCLUDE_RECORD_TYPES` | `+sz-entity-include-record-types+` | 28 | `268435456` |
| `SZ_ENTITY_INCLUDE_RELATED_RECORD_TYPES` | `+sz-entity-include-related-record-types+` | 29 | `536870912` |
| `SZ_FIND_PATH_INCLUDE_MATCHING_INFO` | `+sz-find-path-include-matching-info+` | 30 | `1073741824` |
| `SZ_ENTITY_INCLUDE_RECORD_UNMAPPED_DATA` | `+sz-entity-include-record-unmapped-data+` | 31 | `2147483648` |
| `SZ_SEARCH_INCLUDE_ALL_CANDIDATES` | `+sz-search-include-all-candidates+` | 32 | `4294967296` |
| `SZ_FIND_NETWORK_INCLUDE_MATCHING_INFO` | `+sz-find-network-include-matching-info+` | 33 | `8589934592` |
| `SZ_ENTITY_INCLUDE_RECORD_FEATURE_DETAILS` | `+sz-entity-include-record-feature-details+` | 35 | `34359738368` |
| `SZ_ENTITY_INCLUDE_RECORD_FEATURE_STATS` | `+sz-entity-include-record-feature-stats+` | 36 | `68719476736` |
| `SZ_SEARCH_INCLUDE_REQUEST` | `+sz-search-include-request+` | 37 | `137438953472` |
| `SZ_SEARCH_INCLUDE_REQUEST_DETAILS` | `+sz-search-include-request-details+` | 38 | `274877906944` |
| `SZ_ENTITY_INCLUDE_RECORD_DATES` | `+sz-entity-include-record-dates+` | 39 | `549755813888` |
| `SZ_WITH_INFO` (SDK-only) | `+sz-with-info+` | 62 | `4611686018427387904` |

---

## Function Consolidation

The Lisp API consolidates multiple C functions into single generic functions
with keyword arguments, reducing the public API surface.

### `add-record` → 2 C functions

```
Sz_addRecord          ← flags does NOT include +sz-with-info+
Sz_addRecordWithInfo  ← flags includes +sz-with-info+
```

The method tests `(logtest flags +sz-with-info+)` and dispatches accordingly.
The `+sz-with-info+` bit is stripped via `%strip-sdk-flags` before passing to C.

### `delete-record` → 2 C functions

Same pattern as `add-record`:
```
Sz_deleteRecord          ← no info
Sz_deleteRecordWithInfo  ← with info
```

### `process-redo-record` → 2 C functions

```
Sz_processRedoRecord          ← no info
Sz_processRedoRecordWithInfo  ← with info
```

### `reevaluate-entity` → 2 C functions

```
Sz_reevaluateEntity          ← no info
Sz_reevaluateEntityWithInfo  ← with info
```

### `reevaluate-record` → 2 C functions

```
Sz_reevaluateRecord          ← no info
Sz_reevaluateRecordWithInfo  ← with info
```

### `find-path-by-entity-id` → 3 C functions

Dispatches via `cond` based on which keyword arguments are provided:

```
Sz_findPathByEntityID_V2                   ← no avoid-entity-ids, no required-data-sources
Sz_findPathByEntityIDWithAvoids_V2         ← avoid-entity-ids provided
Sz_findPathByEntityIDIncludingSource_V2    ← required-data-sources provided
```

### `find-path-by-record-id` → 3 C functions

Same pattern:
```
Sz_findPathByRecordID_V2                   ← simple
Sz_findPathByRecordIDWithAvoids_V2         ← with avoids
Sz_findPathByRecordIDIncludingSource_V2    ← with required sources
```

### `initialize-engine` → 2 C functions

```
Sz_init              ← config-id is 0 (default)
Sz_initWithConfigID  ← config-id is non-zero
```

### `initialize-diagnostic` → 2 C functions

```
SzDiagnostic_init              ← config-id is 0 (default)
SzDiagnostic_initWithConfigID  ← config-id is non-zero
```

### Summary

| Public Lisp Function | C Functions Consolidated |
|---------------------|------------------------|
| `add-record` | 2 |
| `delete-record` | 2 |
| `process-redo-record` | 2 |
| `reevaluate-entity` | 2 |
| `reevaluate-record` | 2 |
| `find-path-by-entity-id` | 3 |
| `find-path-by-record-id` | 3 |
| `initialize-engine` | 2 |
| `initialize-diagnostic` | 2 |
| **Total** | **20 C → 9 Lisp** |
