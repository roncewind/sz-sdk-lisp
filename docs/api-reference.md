# sz-sdk API Reference

Common Lisp SDK for Senzing Entity Resolution — Public API Reference

**Version:** 0.1.0 &middot; **License:** Apache-2.0

> This document covers every public symbol exported from the `sz-sdk` umbrella
> package. For architectural details see the [Architecture Guide](guide.md).

---

## Table of Contents

- [Factory](#factory)
- [Product](#product)
- [Engine](#engine)
  - [Record Operations](#record-operations)
  - [Entity Retrieval](#entity-retrieval)
  - [Search](#search)
  - [Redo Queue](#redo-queue)
  - [Reevaluation](#reevaluation)
  - [Export](#export)
  - [Path Finding](#path-finding)
  - [Network Finding](#network-finding)
  - [Interesting Entities](#interesting-entities)
  - [Why / How Analysis](#why--how-analysis)
  - [Statistics / Config](#statistics--config)
- [Config](#config)
- [Config Manager](#config-manager)
- [Diagnostic](#diagnostic)
- [Conditions](#conditions)
- [Flags](#flags)
  - [Base](#base-flags)
  - [Feature Inclusion](#feature-inclusion-flags)
  - [Export Flags](#export-flags)
  - [Entity Relation Flags](#entity-relation-flags)
  - [Entity Feature Flags](#entity-feature-flags)
  - [Entity Info Flags](#entity-info-flags)
  - [Internal / Stats Flags](#internal--stats-flags)
  - [Path / Network Flags](#path--network-flags)
  - [Search Flags](#search-flags)
  - [Recommended Defaults](#recommended-defaults)
  - [With-Info Flag](#with-info-flag)
- [Constants](#constants)

---

## Factory

The factory is the primary entry point for using the SDK. It creates, tracks,
and destroys all Senzing subsystem components.

### Class: `sz-abstract-factory`

```lisp
(make-instance 'sz-abstract-factory
  :instance-name "my-app"
  :settings settings-json
  :config-id 0              ; optional, default 0
  :verbose-logging 0)       ; optional, default 0
```

**Slots:**

| Slot | Initarg | Default | Reader | Description |
|------|---------|---------|--------|-------------|
| `instance-name` | `:instance-name` | `""` | `factory-instance-name` | Application instance name |
| `settings` | `:settings` | `""` | `factory-settings` | Senzing engine configuration JSON |
| `config-id` | `:config-id` | `0` | `factory-config-id` | Configuration ID (0 = default) |
| `verbose-logging` | `:verbose-logging` | `0` | `factory-verbose-logging` | Logging level (0 = off, 1 = verbose) |

### Macro: `with-sz-factory`

```lisp
(with-sz-factory (factory-var instance-name settings
                  &key (config-id 0) (verbose-logging 0))
  &body body)
```

Creates an `sz-abstract-factory`, binds it to `factory-var`, executes `body`,
and guarantees `destroy-factory` is called via `unwind-protect`. This is the
recommended way to use the SDK.

**Example:**

```lisp
(with-sz-factory (factory "my-app" *settings*)
  (let ((engine (create-engine factory)))
    (format t "~A~%" (get-stats engine))))
```

### Generic: `create-engine`

```lisp
(create-engine factory) → sz-engine
```

Create and initialize an `sz-engine` using the factory's settings.

### Generic: `create-product`

```lisp
(create-product factory) → sz-product
```

Create and initialize an `sz-product` using the factory's settings.

### Generic: `create-config-manager`

```lisp
(create-config-manager factory) → sz-config-manager
```

Create and initialize an `sz-config-manager` using the factory's settings.

### Generic: `create-diagnostic`

```lisp
(create-diagnostic factory) → sz-diagnostic
```

Create and initialize an `sz-diagnostic` using the factory's settings.

### Generic: `destroy-factory`

```lisp
(destroy-factory factory) → (values)
```

Destroy all components created by this factory, then mark the factory as
destroyed. Uses `ignore-errors` per component for resilience — partial
failures do not prevent other components from being cleaned up.

### Generic: `reinitialize-factory`

```lisp
(reinitialize-factory factory config-id) → (values)
```

Reinitialize all components that support it (engine, diagnostic) with a new
configuration ID. Components that do not support reinitialization (product,
config-manager) are silently skipped.

**Example — Full factory lifecycle:**

```lisp
(with-sz-factory (factory "my-app" *settings*)
  (let ((engine (create-engine factory))
        (product (create-product factory))
        (config-manager (create-config-manager factory))
        (diagnostic (create-diagnostic factory)))
    ;; Use components...
    (format t "Version: ~A~%" (get-version product))
    (format t "Stats: ~A~%" (get-stats engine))))
;; All components automatically destroyed here
```

---

## Product

Product information interface — version and license queries.

### Class: `sz-product`

Phantom object created via `create-product`. Carries only a `destroyedp` flag.

### Generic: `get-license`

```lisp
(get-license product) → string
```

Return license information as a JSON string.

### Generic: `get-version`

```lisp
(get-version product) → string
```

Return version and build information as a JSON string.

**Example:**

```lisp
(with-sz-factory (factory "my-app" *settings*)
  (let ((product (create-product factory)))
    (format t "~A~%" (get-version product))))
```

---

## Engine

The entity resolution engine — the largest and most-used component. All
methods accept an `sz-engine` instance as the first argument.

### Class: `sz-engine`

Phantom object created via `create-engine`. Carries only a `destroyedp` flag.

### Generic: `prime-engine`

```lisp
(prime-engine engine) → (values)
```

Prime the engine for faster initial operations. Call this after creation if
you want to pre-warm internal caches.

---

### Record Operations

#### Generic: `add-record`

```lisp
(add-record engine data-source-code record-id record-definition
            &key (flags +sz-add-record-default-flags+))
  → string
```

Add a record to the repository.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name (e.g. `"CUSTOMERS"`) |
| `record-id` | `string` | Unique record identifier within the data source |
| `record-definition` | `string` | JSON document containing the record data |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-add-record-default-flags+` (0) |

**Returns:** Empty string normally. If `+sz-with-info+` is included in flags
(via `logior`), returns a JSON string containing resolution info (affected
entities, interesting entities, etc.).

**Example:**

```lisp
;; Simple add
(add-record engine "CUSTOMERS" "1001"
  "{\"PRIMARY_NAME_LAST\":\"Smith\",\"PRIMARY_NAME_FIRST\":\"Robert\"}")

;; Add with info
(let ((info (add-record engine "CUSTOMERS" "1001"
              "{\"PRIMARY_NAME_LAST\":\"Smith\"}"
              :flags (logior +sz-add-record-default-flags+ +sz-with-info+))))
  (format t "Info: ~A~%" info))
```

#### Generic: `delete-record`

```lisp
(delete-record engine data-source-code record-id
               &key (flags +sz-delete-record-default-flags+))
  → string
```

Delete a record from the repository. Idempotent — deleting a non-existent
record does not signal an error.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-delete-record-default-flags+` (0) |

**Returns:** Empty string normally; JSON with-info string if `+sz-with-info+`
is set.

---

### Entity Retrieval

#### Generic: `get-entity-by-entity-id`

```lisp
(get-entity-by-entity-id engine entity-id
                         &key (flags +sz-entity-default-flags+))
  → string
```

Get entity data by entity ID. Returns a JSON string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-id` | `integer` | The entity ID |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-entity-default-flags+` |

#### Generic: `get-entity-by-record-id`

```lisp
(get-entity-by-record-id engine data-source-code record-id
                         &key (flags +sz-entity-default-flags+))
  → string
```

Get entity data by data source and record ID. Returns a JSON string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-entity-default-flags+` |

#### Generic: `get-record`

```lisp
(get-record engine data-source-code record-id
            &key (flags +sz-record-default-flags+))
  → string
```

Get a specific record. Returns a JSON string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-record-default-flags+` |

#### Generic: `get-record-preview`

```lisp
(get-record-preview engine record-definition
                    &key (flags +sz-record-preview-default-flags+))
  → string
```

Preview how a record would be processed without adding it. Returns a JSON
string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `record-definition` | `string` | JSON record document |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-record-preview-default-flags+` |

#### Generic: `get-redo-record`

```lisp
(get-redo-record engine) → string
```

Get the next redo record from the redo queue. Returns a JSON string.

#### Generic: `get-virtual-entity-by-record-id`

```lisp
(get-virtual-entity-by-record-id engine record-keys
                                  &key (flags +sz-virtual-entity-default-flags+))
  → string
```

Get a virtual entity composed of the given records.

| Parameter | Type | Description |
|-----------|------|-------------|
| `record-keys` | `list` | List of `(data-source-code record-id)` pairs |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-virtual-entity-default-flags+` |

**Example:**

```lisp
(get-virtual-entity-by-record-id engine
  '(("CUSTOMERS" "1001") ("CUSTOMERS" "1002")))
```

---

### Search

#### Generic: `search-by-attributes`

```lisp
(search-by-attributes engine attributes
                      &key (flags +sz-search-by-attributes-default-flags+)
                           (search-profile ""))
  → string
```

Search for entities matching the given attributes. Returns a JSON string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `attributes` | `string` | JSON search attributes |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-search-by-attributes-default-flags+` |
| `search-profile` | `string` | Search profile name (empty = default) |

**Example:**

```lisp
(search-by-attributes engine
  "{\"NAME_FULL\":\"Robert Smith\"}")
```

---

### Redo Queue

#### Generic: `count-redo-records`

```lisp
(count-redo-records engine) → integer
```

Return the number of redo records in the queue.

#### Generic: `process-redo-record`

```lisp
(process-redo-record engine redo-record
                     &key (flags +sz-redo-default-flags+))
  → string
```

Process a redo record. Returns empty string normally; JSON with-info if
`+sz-with-info+` is set in flags.

| Parameter | Type | Description |
|-----------|------|-------------|
| `redo-record` | `string` | Redo record JSON (from `get-redo-record`) |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-redo-default-flags+` (0) |

---

### Reevaluation

#### Generic: `reevaluate-entity`

```lisp
(reevaluate-entity engine entity-id
                   &key (flags +sz-reevaluate-entity-default-flags+))
  → string
```

Reevaluate an entity. Returns empty string normally; JSON with-info if
`+sz-with-info+` is set.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-id` | `integer` | Entity ID to reevaluate |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-reevaluate-entity-default-flags+` (0) |

#### Generic: `reevaluate-record`

```lisp
(reevaluate-record engine data-source-code record-id
                   &key (flags +sz-reevaluate-record-default-flags+))
  → string
```

Reevaluate a record. Returns empty string normally; JSON with-info if
`+sz-with-info+` is set.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-reevaluate-record-default-flags+` (0) |

---

### Export

Export functions produce bulk entity reports. The workflow is:
1. Start an export (CSV or JSON) — returns an export handle
2. Call `fetch-next` in a loop until it returns an empty string
3. Call `close-export-report` to release the handle

#### Generic: `export-csv-entity-report`

```lisp
(export-csv-entity-report engine csv-column-list
                          &key (flags +sz-export-default-flags+))
  → integer
```

Start a CSV entity report export. Returns an export handle.

| Parameter | Type | Description |
|-----------|------|-------------|
| `csv-column-list` | `string` | Comma-separated list of CSV column names |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-export-default-flags+` |

#### Generic: `export-json-entity-report`

```lisp
(export-json-entity-report engine &key (flags +sz-export-default-flags+))
  → integer
```

Start a JSON entity report export. Returns an export handle.

| Parameter | Type | Description |
|-----------|------|-------------|
| `flags` | `(unsigned-byte 64)` | Default: `+sz-export-default-flags+` |

#### Generic: `fetch-next`

```lisp
(fetch-next engine export-handle) → string
```

Fetch the next chunk of an export report. Returns an empty string when the
export is complete.

| Parameter | Type | Description |
|-----------|------|-------------|
| `export-handle` | `integer` | Handle from `export-csv-entity-report` or `export-json-entity-report` |

#### Generic: `close-export-report`

```lisp
(close-export-report engine export-handle) → (values)
```

Close an export report handle and release associated resources.

**Example:**

```lisp
(let ((handle (export-json-entity-report engine)))
  (unwind-protect
       (loop for chunk = (fetch-next engine handle)
             until (string= chunk "")
             do (write-string chunk))
    (close-export-report engine handle)))
```

---

### Path Finding

#### Generic: `find-path-by-entity-id`

```lisp
(find-path-by-entity-id engine start-entity-id end-entity-id max-degrees
                        &key avoid-entity-ids required-data-sources
                             (flags +sz-find-path-default-flags+))
  → string
```

Find a relationship path between two entities by entity ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `start-entity-id` | `integer` | Starting entity ID |
| `end-entity-id` | `integer` | Ending entity ID |
| `max-degrees` | `integer` | Maximum degrees of separation |
| `avoid-entity-ids` | `list` or `nil` | List of integer entity IDs to avoid |
| `required-data-sources` | `list` or `nil` | List of data source code strings required along the path |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-path-default-flags+` |

Dispatches to one of three C functions based on which optional parameters are
provided:
- No avoids or required sources → simple path
- `avoid-entity-ids` only → path with avoidances
- `required-data-sources` → path with avoidances and required data sources

#### Generic: `find-path-by-record-id`

```lisp
(find-path-by-record-id engine
                        start-data-source-code start-record-id
                        end-data-source-code end-record-id
                        max-degrees
                        &key avoid-record-keys required-data-sources
                             (flags +sz-find-path-default-flags+))
  → string
```

Find a relationship path between two entities by record ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `start-data-source-code` | `string` | Starting record's data source |
| `start-record-id` | `string` | Starting record's ID |
| `end-data-source-code` | `string` | Ending record's data source |
| `end-record-id` | `string` | Ending record's ID |
| `max-degrees` | `integer` | Maximum degrees of separation |
| `avoid-record-keys` | `list` or `nil` | List of `(data-source-code record-id)` pairs to avoid |
| `required-data-sources` | `list` or `nil` | List of required data source code strings |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-path-default-flags+` |

---

### Network Finding

#### Generic: `find-network-by-entity-id`

```lisp
(find-network-by-entity-id engine entity-ids max-degrees
                           build-out-degrees build-out-max-entities
                           &key (flags +sz-find-network-default-flags+))
  → string
```

Find a network among a set of entities by entity ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-ids` | `list` | List of integer entity IDs |
| `max-degrees` | `integer` | Maximum degrees of separation between listed entities |
| `build-out-degrees` | `integer` | Degrees to build out from the network |
| `build-out-max-entities` | `integer` | Maximum entities in the build-out |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-network-default-flags+` |

#### Generic: `find-network-by-record-id`

```lisp
(find-network-by-record-id engine record-keys max-degrees
                           build-out-degrees build-out-max-entities
                           &key (flags +sz-find-network-default-flags+))
  → string
```

Find a network among a set of entities by record ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `record-keys` | `list` | List of `(data-source-code record-id)` pairs |
| `max-degrees` | `integer` | Maximum degrees of separation |
| `build-out-degrees` | `integer` | Degrees to build out |
| `build-out-max-entities` | `integer` | Maximum build-out entities |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-network-default-flags+` |

---

### Interesting Entities

#### Generic: `find-interesting-entities-by-entity-id`

```lisp
(find-interesting-entities-by-entity-id engine entity-id
                                        &key (flags +sz-find-interesting-entities-default-flags+))
  → string
```

Find interesting entities related to the given entity.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-id` | `integer` | Entity ID |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-interesting-entities-default-flags+` (0) |

#### Generic: `find-interesting-entities-by-record-id`

```lisp
(find-interesting-entities-by-record-id engine data-source-code record-id
                                        &key (flags +sz-find-interesting-entities-default-flags+))
  → string
```

Find interesting entities related to the entity containing the given record.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-find-interesting-entities-default-flags+` (0) |

---

### Why / How Analysis

#### Generic: `why-entities`

```lisp
(why-entities engine entity-id-1 entity-id-2
              &key (flags +sz-why-entities-default-flags+))
  → string
```

Determine why two entities resolved together or are related.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-id-1` | `integer` | First entity ID |
| `entity-id-2` | `integer` | Second entity ID |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-why-entities-default-flags+` |

#### Generic: `why-records`

```lisp
(why-records engine data-source-code-1 record-id-1
             data-source-code-2 record-id-2
             &key (flags +sz-why-records-default-flags+))
  → string
```

Determine why two records resolved together or are related.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code-1` | `string` | First record's data source |
| `record-id-1` | `string` | First record's ID |
| `data-source-code-2` | `string` | Second record's data source |
| `record-id-2` | `string` | Second record's ID |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-why-records-default-flags+` |

#### Generic: `why-record-in-entity`

```lisp
(why-record-in-entity engine data-source-code record-id
                      &key (flags +sz-why-record-in-entity-default-flags+))
  → string
```

Determine why a specific record is in its resolved entity.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Data source name |
| `record-id` | `string` | Record identifier |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-why-record-in-entity-default-flags+` |

#### Generic: `why-search`

```lisp
(why-search engine attributes entity-id
            &key (flags +sz-why-search-default-flags+)
                 (search-profile ""))
  → string
```

Determine why an entity matches a search.

| Parameter | Type | Description |
|-----------|------|-------------|
| `attributes` | `string` | JSON search attributes |
| `entity-id` | `integer` | Entity ID that matched |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-why-search-default-flags+` |
| `search-profile` | `string` | Search profile name (empty = default) |

#### Generic: `how-entity-by-entity-id`

```lisp
(how-entity-by-entity-id engine entity-id
                         &key (flags +sz-how-entity-default-flags+))
  → string
```

Determine how an entity was constructed — the step-by-step resolution process.

| Parameter | Type | Description |
|-----------|------|-------------|
| `entity-id` | `integer` | Entity ID |
| `flags` | `(unsigned-byte 64)` | Default: `+sz-how-entity-default-flags+` |

---

### Statistics / Config

#### Generic: `get-active-config-id`

```lisp
(get-active-config-id engine) → integer
```

Return the active configuration ID currently in use by the engine.

#### Generic: `get-stats`

```lisp
(get-stats engine) → string
```

Return engine statistics as a JSON string. Useful for monitoring and
diagnostics.

---

## Config

In-memory configuration editing interface. `sz-config` objects are created via
the config manager (not the factory). They hold a configuration JSON document
in memory for inspection and modification.

### Class: `sz-config`

| Slot | Accessor | Description |
|------|----------|-------------|
| `config-definition` | `sz-config-config-definition` | The in-memory JSON config document |

### Generic: `config-export`

```lisp
(config-export config) → string
```

Return the current configuration definition as a JSON string.

### Generic: `import-config-definition`

```lisp
(import-config-definition config config-definition) → (values)
```

Set the internal configuration from a JSON string.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-definition` | `string` | JSON configuration document |

### Generic: `import-template`

```lisp
(import-template config) → (values)
```

Load the default Senzing configuration template into the config object. This
creates an in-memory config handle from the template, exports it to JSON, and
stores it internally.

### Generic: `verify-config-definition`

```lisp
(verify-config-definition config config-definition) → (values)
```

Verify that a configuration JSON document is valid by loading and re-exporting
it. Signals an `sz-error` condition on invalid input.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-definition` | `string` | JSON configuration document to verify |

### Generic: `get-data-source-registry`

```lisp
(get-data-source-registry config) → string
```

Return the data source registry from the current configuration as a JSON
string.

### Generic: `register-data-source`

```lisp
(register-data-source config data-source-code) → string
```

Register a new data source in the configuration. Returns the registration
response as a JSON string. The internal configuration is updated with the new
data source.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Name for the new data source |

### Generic: `unregister-data-source`

```lisp
(unregister-data-source config data-source-code) → (values)
```

Remove a data source from the configuration.

| Parameter | Type | Description |
|-----------|------|-------------|
| `data-source-code` | `string` | Name of the data source to remove |

**Example — Config workflow:**

```lisp
(with-sz-factory (factory "my-app" *settings*)
  (let* ((config-mgr (create-config-manager factory))
         (config (create-config-from-template config-mgr)))
    (register-data-source config "MY_DATA_SOURCE")
    (let ((config-def (config-export config)))
      (set-default-config config-mgr config-def "Added MY_DATA_SOURCE"))))
```

---

## Config Manager

Configuration registration and management interface. Manages the persistent
configuration registry stored in the Senzing repository.

### Class: `sz-config-manager`

Tracks instance-name, settings, and verbose-logging for creating `sz-config`
objects.

### Generic: `create-config-from-config-id`

```lisp
(create-config-from-config-id config-manager config-id) → sz-config
```

Create an `sz-config` object loaded with the configuration identified by
`config-id` from the repository.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-id` | `integer` | Registered configuration ID |

### Generic: `create-config-from-string`

```lisp
(create-config-from-string config-manager config-definition) → sz-config
```

Create an `sz-config` object from a JSON configuration string. The string is
verified before being imported.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-definition` | `string` | JSON configuration document |

### Generic: `create-config-from-template`

```lisp
(create-config-from-template config-manager) → sz-config
```

Create an `sz-config` object loaded with the default Senzing template
configuration.

### Generic: `get-config-registry`

```lisp
(get-config-registry config-manager) → string
```

Return the configuration registry as a JSON string listing all registered
configurations.

### Generic: `get-default-config-id`

```lisp
(get-default-config-id config-manager) → integer
```

Return the default configuration ID.

### Generic: `register-config`

```lisp
(register-config config-manager config-definition config-comment) → integer
```

Register a new configuration. Returns the new configuration ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-definition` | `string` | JSON configuration document |
| `config-comment` | `string` | Description of the configuration |

### Generic: `replace-default-config-id`

```lisp
(replace-default-config-id config-manager
                           current-default-config-id
                           new-default-config-id)
  → (values)
```

Atomically replace the default configuration ID. Signals
`sz-replace-conflict-error` if `current-default-config-id` does not match the
actual current default (optimistic locking).

| Parameter | Type | Description |
|-----------|------|-------------|
| `current-default-config-id` | `integer` | Expected current default config ID |
| `new-default-config-id` | `integer` | New default config ID to set |

### Generic: `set-default-config`

```lisp
(set-default-config config-manager config-definition config-comment) → integer
```

Register a configuration and set it as the default in one step. Returns the
new configuration ID.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-definition` | `string` | JSON configuration document |
| `config-comment` | `string` | Description of the configuration |

### Generic: `set-default-config-id`

```lisp
(set-default-config-id config-manager config-id) → (values)
```

Set the default configuration ID directly.

| Parameter | Type | Description |
|-----------|------|-------------|
| `config-id` | `integer` | Configuration ID to set as default |

---

## Diagnostic

Repository diagnostic and maintenance interface.

### Class: `sz-diagnostic`

Phantom object created via `create-diagnostic`. Carries only a `destroyedp`
flag.

### Generic: `check-repository-performance`

```lisp
(check-repository-performance diagnostic seconds-to-run) → string
```

Run a performance test for the specified duration. Returns a JSON string with
performance metrics.

| Parameter | Type | Description |
|-----------|------|-------------|
| `seconds-to-run` | `integer` | Duration of the test in seconds |

### Generic: `get-repository-info`

```lisp
(get-repository-info diagnostic) → string
```

Return repository information as a JSON string.

### Generic: `get-feature`

```lisp
(get-feature diagnostic feature-id) → string
```

Get a feature by ID. Returns a JSON string. This function is unsupported and
undocumented — it exists for internal diagnostic use.

| Parameter | Type | Description |
|-----------|------|-------------|
| `feature-id` | `integer` | Feature ID |

### Generic: `purge-repository`

```lisp
(purge-repository diagnostic) → (values)
```

**DESTRUCTIVE.** Purge all data from the repository. This removes all loaded
records and resolved entities.

---

## Conditions

The SDK uses Common Lisp's condition system for error handling. All SDK errors
are subclasses of `sz-error`, which is itself a subclass of CL's `error`.

### Base Condition: `sz-error`

```lisp
(define-condition sz-error (error)
  ((error-code    :reader sz-error-code)
   (error-message :reader sz-error-message)))
```

| Reader | Type | Description |
|--------|------|-------------|
| `sz-error-code` | `integer` | Senzing numeric error code |
| `sz-error-message` | `string` | Human-readable error description |

### Condition Hierarchy

```
sz-error
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

### Category Conditions

| Condition | Parent | When to Catch |
|-----------|--------|---------------|
| `sz-bad-input-error` | `sz-error` | Input validation failures (bad record, missing data source) |
| `sz-general-error` | `sz-error` | Configuration problems, SDK internal errors |
| `sz-retryable-error` | `sz-error` | Transient failures worth retrying |
| `sz-unrecoverable-error` | `sz-error` | Fatal errors requiring restart |

### Detail Conditions

| Condition | Parent | Description |
|-----------|--------|-------------|
| `sz-not-found-error` | `sz-bad-input-error` | Entity or record not found |
| `sz-unknown-data-source-error` | `sz-bad-input-error` | Unregistered data source referenced |
| `sz-configuration-error` | `sz-general-error` | Invalid or missing configuration |
| `sz-replace-conflict-error` | `sz-general-error` | Optimistic locking conflict in `replace-default-config-id` |
| `sz-sdk-error` | `sz-general-error` | SDK-level internal error |
| `sz-database-connection-lost-error` | `sz-retryable-error` | Database connection dropped |
| `sz-database-transient-error` | `sz-retryable-error` | Temporary database issue |
| `sz-retry-timeout-exceeded-error` | `sz-retryable-error` | Retry budget exhausted |
| `sz-database-error` | `sz-unrecoverable-error` | Permanent database failure |
| `sz-license-error` | `sz-unrecoverable-error` | License validation failure |
| `sz-not-initialized-error` | `sz-unrecoverable-error` | Component not initialized or already destroyed |
| `sz-unhandled-error` | `sz-unrecoverable-error` | Unexpected internal error |

### Error Handling Patterns

**Catch all SDK errors:**

```lisp
(handler-case
    (add-record engine "SRC" "1" "{}")
  (sz-error (e)
    (format t "Error ~D: ~A~%" (sz-error-code e) (sz-error-message e))))
```

**Catch by category:**

```lisp
(handler-case
    (add-record engine "SRC" "1" record-json)
  (sz-bad-input-error (e)
    (format t "Bad input: ~A~%" (sz-error-message e)))
  (sz-retryable-error (e)
    (format t "Retryable: ~A~%" (sz-error-message e)))
  (sz-unrecoverable-error (e)
    (error e)))  ; re-signal fatal errors
```

**Catch specific condition:**

```lisp
(handler-case
    (get-entity-by-record-id engine "SRC" "999")
  (sz-not-found-error ()
    (format t "Record not found~%")))
```

---

## Flags

All flags are `(unsigned-byte 64)` constants. Compose them with `logior`:

```lisp
(logior +sz-entity-default-flags+ +sz-include-feature-scores+)
```

Test whether a flag is set with `logtest`:

```lisp
(logtest flags +sz-with-info+)  ; → T or NIL
```

### Base Flags

| Flag | Value | Description |
|------|-------|-------------|
| `+sz-no-flags+` | `0` | No flags set |

### Feature Inclusion Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-include-feature-scores+` | 26 | Include feature scores in results |
| `+sz-include-match-key-details+` | 34 | Include match key details |

### Export Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-export-include-multi-record-entities+` | 0 | Include entities with multiple records |
| `+sz-export-include-possibly-same+` | 1 | Include possibly-same relationships |
| `+sz-export-include-possibly-related+` | 2 | Include possibly-related relationships |
| `+sz-export-include-name-only+` | 3 | Include name-only relationships |
| `+sz-export-include-disclosed+` | 4 | Include disclosed relationships |
| `+sz-export-include-single-record-entities+` | 5 | Include single-record entities |
| `+sz-export-include-all-entities+` | composite | Multi-record + single-record |
| `+sz-export-include-all-having-relationships+` | composite | Possibly-same + possibly-related + name-only + disclosed |

### Entity Relation Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-entity-include-possibly-same-relations+` | 6 | Include possibly-same relations |
| `+sz-entity-include-possibly-related-relations+` | 7 | Include possibly-related relations |
| `+sz-entity-include-name-only-relations+` | 8 | Include name-only relations |
| `+sz-entity-include-disclosed-relations+` | 9 | Include disclosed relations |
| `+sz-entity-include-all-relations+` | composite | All four relation types |

### Entity Feature Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-entity-include-all-features+` | 10 | Include all features |
| `+sz-entity-include-representative-features+` | 11 | Include representative features only |

### Entity Info Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-entity-include-entity-name+` | 12 | Include entity name |
| `+sz-entity-include-record-summary+` | 13 | Include record summary |
| `+sz-entity-include-record-types+` | 28 | Include record types |
| `+sz-entity-include-record-data+` | 14 | Include record data |
| `+sz-entity-include-record-matching-info+` | 15 | Include record matching info |
| `+sz-entity-include-record-dates+` | 39 | Include record dates |
| `+sz-entity-include-record-json-data+` | 16 | Include record JSON data |
| `+sz-entity-include-record-unmapped-data+` | 31 | Include record unmapped data |
| `+sz-entity-include-record-features+` | 18 | Include record features |
| `+sz-entity-include-record-feature-details+` | 35 | Include record feature details |
| `+sz-entity-include-record-feature-stats+` | 36 | Include record feature statistics |
| `+sz-entity-include-related-entity-name+` | 19 | Include related entity name |
| `+sz-entity-include-related-matching-info+` | 20 | Include related matching info |
| `+sz-entity-include-related-record-summary+` | 21 | Include related record summary |
| `+sz-entity-include-related-record-types+` | 29 | Include related record types |
| `+sz-entity-include-related-record-data+` | 22 | Include related record data |

### Internal / Stats Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-entity-include-internal-features+` | 23 | Include internal features |
| `+sz-entity-include-feature-stats+` | 24 | Include feature statistics |

### Path / Network Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-find-path-strict-avoid+` | 25 | Strictly avoid listed entities (fail rather than route through) |
| `+sz-find-path-include-matching-info+` | 30 | Include matching info in path results |
| `+sz-find-network-include-matching-info+` | 33 | Include matching info in network results |

### Search Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-search-include-stats+` | 27 | Include search statistics |
| `+sz-search-include-resolved+` | 0 | Include resolved matches (aliases `+sz-export-include-multi-record-entities+`) |
| `+sz-search-include-possibly-same+` | 1 | Include possibly-same matches |
| `+sz-search-include-possibly-related+` | 2 | Include possibly-related matches |
| `+sz-search-include-name-only+` | 3 | Include name-only matches |
| `+sz-search-include-all-entities+` | composite | All search match types |
| `+sz-search-include-all-candidates+` | 32 | Include all candidates |
| `+sz-search-include-request+` | 37 | Include the search request in results |
| `+sz-search-include-request-details+` | 38 | Include detailed search request info |

### Recommended Defaults

Pre-composed flag combinations used as method defaults:

| Flag | Composition | Used By |
|------|-------------|---------|
| `+sz-record-default-flags+` | `+sz-entity-include-record-json-data+` | `get-record` |
| `+sz-entity-core-flags+` | representative-features + entity-name + record-summary + record-data + record-matching-info | (base for other defaults) |
| `+sz-entity-default-flags+` | core + all-relations + related-entity-name + related-record-summary + related-matching-info | `get-entity-by-*` |
| `+sz-entity-brief-default-flags+` | record-matching-info + all-relations + related-matching-info | (brief entity view) |
| `+sz-export-default-flags+` | all-entities + entity-default | `export-*-entity-report` |
| `+sz-find-path-default-flags+` | path-matching-info + entity-name + record-summary | `find-path-by-*` |
| `+sz-find-network-default-flags+` | network-matching-info + entity-name + record-summary | `find-network-by-*` |
| `+sz-why-entities-default-flags+` | `+sz-include-feature-scores+` | `why-entities` |
| `+sz-why-records-default-flags+` | `+sz-include-feature-scores+` | `why-records` |
| `+sz-why-record-in-entity-default-flags+` | `+sz-include-feature-scores+` | `why-record-in-entity` |
| `+sz-why-search-default-flags+` | feature-scores + request-details + stats | `why-search` |
| `+sz-how-entity-default-flags+` | `+sz-include-feature-scores+` | `how-entity-by-entity-id` |
| `+sz-virtual-entity-default-flags+` | `+sz-entity-core-flags+` | `get-virtual-entity-by-record-id` |
| `+sz-add-record-default-flags+` | `0` | `add-record` |
| `+sz-delete-record-default-flags+` | `0` | `delete-record` |
| `+sz-record-preview-default-flags+` | `+sz-entity-include-record-feature-details+` | `get-record-preview` |
| `+sz-redo-default-flags+` | `0` | `process-redo-record` |
| `+sz-reevaluate-record-default-flags+` | `0` | `reevaluate-record` |
| `+sz-reevaluate-entity-default-flags+` | `0` | `reevaluate-entity` |
| `+sz-find-interesting-entities-default-flags+` | `0` | `find-interesting-entities-by-*` |
| `+sz-search-by-attributes-all+` | all-entities + representative + entity-name + record-summary + feature-scores + stats | `search-by-attributes` default |
| `+sz-search-by-attributes-strong+` | resolved + possibly-same + representative + entity-name + record-summary + feature-scores + stats | (strong matches only) |
| `+sz-search-by-attributes-minimal-all+` | all-entities + stats | (minimal all) |
| `+sz-search-by-attributes-minimal-strong+` | resolved + possibly-same + stats | (minimal strong) |
| `+sz-search-by-attributes-default-flags+` | `+sz-search-by-attributes-all+` | `search-by-attributes` |

### With-Info Flag

| Flag | Bit | Description |
|------|-----|-------------|
| `+sz-with-info+` | 62 | **SDK-only flag.** When included via `logior`, methods that support it (`add-record`, `delete-record`, `process-redo-record`, `reevaluate-entity`, `reevaluate-record`) return resolution info JSON instead of an empty string. This flag is stripped before passing to the C library. |

---

## Constants

Convenience constants for clarity and self-documenting code. Integer constants
use `defconstant`; string constants use `defparameter` (because
`defconstant` requires `eql` equality on redefinition, which strings do not
portably satisfy).

| Constant | Type | Value | Description |
|----------|------|-------|-------------|
| `+sz-without-info+` | `integer` | `0` | Explicit "no with-info" flag value |
| `+sz-initialize-with-default-configuration+` | `integer` | `0` | Use default configuration on init |
| `+sz-no-logging+` | `integer` | `0` | Disable verbose logging |
| `+sz-verbose-logging+` | `integer` | `1` | Enable verbose logging |
| `+sz-no-attributes+` | `string` | `""` | Empty attributes placeholder |
| `+sz-no-avoidances+` | `string` | `""` | Empty avoidances placeholder |
| `+sz-no-info+` | `string` | `""` | Empty info placeholder |
| `+sz-no-required-datasources+` | `string` | `""` | Empty required data sources placeholder |
| `+sz-no-search-profile+` | `string` | `""` | Empty search profile placeholder |
