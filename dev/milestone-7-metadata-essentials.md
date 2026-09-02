# Milestone 7 Slices 1 and 1B: metadata explorer foundation

Status: implementation candidate awaiting independent review and owner final
check. This document does not authorize commit, deployment, dependency changes,
or a later Milestone 7 slice.

## Scope and dependency order

Slice 1 and its approved hierarchy-neighbourhood extension establish the
smallest useful read-only vertical slice:

1. retain immutable source metadata at the existing GLC inspection/import
   boundary;
2. bind a typed, versioned LightLogWeb overlay to the combined source
   fingerprint without mutating the source;
3. build one serializable in-memory hierarchy/search index;
4. carry the bundle through the existing dataset record, session recovery,
   undo, and append seams;
5. expose a discoverable, responsive, keyboard-ready explorer in production,
   including item properties and exact structural neighbourhood navigation;
6. verify complete, partial, absent, nested, unknown-field, and appended-source
   behavior with offline fixtures and explicit performance budgets.

The slice does not implement semantic relationship extraction, metadata-driven changes
to the library/dashboard/table, overlay editing, overlay export, edited GLC
package generation, preprocessing, reproducibility bundles, or live
measurement transfer.

Milestone 6 remains responsible for metadata/data transport and recoverable
task state. The metadata explorer consumes only a retained dataset record. It
does not reopen a GLC package, retry transport, or deserialize the raw dataset
payload. Portable `.llwproj` save/reload remains a later project-bundle concern.

## Baseline and seam audit

- Branch: `dev`
- Exact baseline: `b704d24ee6eab066163bb88e7125faf19e09543d`
- Baseline relation: eight commits ahead of `origin/dev`
- Unrelated untracked paths preserved: `dev/brand/candidates/` and
  `man/man.Rproj`
- glcdp package version: 1.0.0.9000
- glcdp exact revision:
  `e49fcf153a114916ce2680f8b40eb475e694e10d`
- DESCRIPTION and renv.lock already pin that exact revision. Repinning is not
  needed and was not performed.

The existing dataset record already separates `factual_metadata`,
`analysis_settings`, recipes, immutable raw payload, prepared payload,
provenance, revision, draft, and history. Slice 1 adds `metadata_bundle` to
that same record. Snapshot, draft preview, apply, reset, undo, session event,
and serialization paths carry it without adding a second recovery store.

GLC inspection already owns the long-running package boundary. Slice 1 adds a
truthful `metadata_index` progress phase there, before the live package handle
is discarded. Projection and full-import handoffs retain the serializable
source snapshot. Ordinary explorer interactions use only that snapshot's
index.

Append already preserves `llw_source_dataset_id` in the combined data and
source-specific manifests/provenance. Slice 1 adds one top-level metadata
namespace per exact immediate source dataset ID. Each namespace retains its
original nested metadata bundle and factual summary. Cached nested indexes are
not recursively duplicated in the appended search projection, and no fields
are flattened or reconciled.

## Exported glcdp metadata API audit

Only exported functions at the exact pinned revision were audited and used.

| Function | Exact formals | Public return/behavior |
|---|---|---|
| `glc_resources` | `x` | Tibble with one row per declared resource path. Observed columns: `resource`, `path`, `core`, `directory`, `format`, `media_type`, `profile`, `schema_path`, `delimiter`, `decimal_mark`. |
| `glc_metadata` | `x, resources = NULL` | Named list by requested resource. Tabular resources are tibbles and directory resources are named nested lists. `NULL` selects declared core metadata. |
| `glc_search_metadata` | `x, query, resources = NULL, fields = NULL, fixed = TRUE, ignore_case = TRUE, search_in = c("values", "fields", "both")` | Leaf-level tibble. Observed columns: `resource`, integer `record`, `field`, `value`, `context`. Repeated scalar values produce repeated rows. |
| `extract_metadata` | `dataset, metadata, fields, by = "file_group_id", resource = NULL` | One row per unique key in first-occurrence order. It preserves key class and dplyr grouping. Package-backed default traversal follows file-group, dataset, participant, study, and device relationships. |
| `add_metadata` | `dataset, metadata, fields, by = "file_group_id", resource = NULL, overwrite = FALSE` | Derived table with metadata columns added. It preserves row count, row order, identifiers, and dplyr grouping. It does not mutate the input dataset. |

The explorer uses its retained LightLogWeb index for user-local interaction.
It does not call `glc_search_metadata` repeatedly because doing so would reopen
or retain a package transport boundary. Later relationship work will delegate
relationship semantics to `extract_metadata`, and will use `add_metadata` only
on a derived analysis table.

Ambiguity and conflict behavior is retained rather than reimplemented:

- duplicate metadata identifiers raise `glcdp_metadata_id_error`;
- an existing output column raises `glcdp_metadata_column_conflict` unless
  `overwrite = TRUE` is explicit;
- partial identifier matches retain unmatched identifiers with missing values
  and a warning;
- missing linked participant, study, or device records remain missing with a
  warning;
- fields that cannot resolve to exactly one connected resource are errors;
- dataset-level device fields error when one dataset links to multiple devices;
- `add_metadata` preserves input rows, order, grouping, and identifiers.

Metadata inventory, loading, and search are metadata-only operations. The
fixture generator is the sole Slice 1 action that performed owner-approved,
bounded public measurement reads.

## Source-versus-overlay contract

The contract version is 1.0.0 and contains:

- `llw_source_metadata`: immutable namespace identity, exact
  `source_dataset_id`, state, records, declared resources, schema/provenance,
  and SHA-256 fingerprint;
- `llw_metadata_overlay`: revision, bound combined-source fingerprint, five
  typed tables, and preserved unknown fields;
- `llw_metadata_index` version 1.3.1: stable hierarchy nodes with cached
  parent/count facts and exact retained record identities, exact leaf entries,
  and semantic field groups with lower-case search text and precomputed stable
  sort ranks;
- `llw_metadata_bundle`: named source namespaces, one overlay, combined source
  fingerprint, and the prebuilt index;
- `llw_glc_source_metadata_snapshot`: exact GLC source provenance plus the
  bundle.

Overlay tables cover datasets, participants, devices, sites, and variables.
Validation enforces exact columns and primitive types, stable non-empty keys,
unique identities, paired finite coordinates within geographic bounds, and
known IANA source timezones. Unknown overlay fields remain versioned and
searchable. Overlay rows use separate badges and detail language. They never
alter a source fingerprint or imply source-package validation.

Metadata-free records receive an absent source namespace and remain valid.
Display helpers return the raw variable name and `unit not specified` when
optional labels or units do not exist.

## Three-source fixture contract

The authoritative machine-readable contract is
`tests/testthat/fixtures/milestone-7/manifest.json`.

| Fixture | Exact source | Selection | Retained output shape | Metadata state |
|---|---|---|---:|---|
| TUM MEDI-only | `tscnlab/melidos-tum-glc-dataset@1187d19b7f6145b73b8d4dfee13af711c1cb6180` | S001 through S005 glasses files, explicit `MEDI` | 160 rows, 6 columns | Complete |
| IZTECH subset | `tscnlab/melidos-iztech-glc-dataset@abc456bdb418e6250b9c973813a190dc062c6e76` | S001 through S005 glasses files, with two S004 devices, explicit `LIGHT`, `MEDI` | 192 rows, 7 columns | Partial |
| UCR all variables | `tscnlab/melidos-ucr-glc-dataset@1d1309c969b9ca754a5c9696382c39d4b58ccdfd` | S001 through S005 wrist files, all 29 source variables | 160 rows, 31 columns | Complete |

All three sources are published, publicly accessible, Schema 3.0.2 packages
under CC-BY-4.0. Each selected file read is capped at 32 rows with
`problems = "error"` and `standardize = "lightlogr"`. Only exact selected
files were read. Exact bounded public values are retained under the owner's
explicit no-filtering instruction. The snapshots are not complete published
datasets and are not fresh or live measurement evidence.

Each fixture retains metadata only for the represented import relationships:
one selected study when available, five selected participants, the exact
selected files, their linked devices and datasheets, and variables actually
present in the imported result. Study-to-dataset and file-to-device links are
preserved. IZTECH S004 deliberately retains two files from two distinct
devices. Participant age, sex, gender, characteristics, device metadata, and
measurement values remain exact public values. This scoping prevents
participants, files, devices, or questionnaire-variable definitions unrelated
to the represented measurements from appearing as results.

The IZTECH `study` and `participant_characteristics` objects exist in the Git
revision but returned HTTP 400 from GitHub raw transport through exported
`glc_metadata()` on 2026-08-31. The fixture records both exclusions and remains
partial. Slice 1 does not copy those objects around glcdp or reinterpret the
source package.

The five-participant UCR fixture uses the S001 through S005 wrist files at the
same pinned package revision. It keeps the genuine 29-variable all-variable
request, compatibility ID
`glcc_a6aa68bac79e3236d9f1d46788dbd6bcaec6c4994fa4737cbf22a0f21f8ede12`,
and unit ID
`glcu_f6d259c843347e3d10d784cd1d26e4b74d8651d21529d39222213c59bbd3e853`.

Final fixture total: 159,228 bytes. Routine tests are offline and never reopen
the public sources.

The same manifest also freezes one generated-data-free review state,
`tum-iztech-appended`. At showcase startup, LightLogWeb constructs it in
memory from the TUM and IZTECH RDS fixtures through the existing append
contract. TUM `MEDI`, IZTECH `MEDI`, and IZTECH `LIGHT` remain separate as
`tum_MEDI`, `iztech_MEDI`, and `iztech_LIGHT`. Every one of its 352 retained
rows preserves the stable immediate-source identity in
`llw_source_dataset_id`. The metadata bundle contains two exact source
namespaces and a source-fingerprint-bound revision 1 review overlay. The
manifest repeats both source citations, repositories, exact revisions,
licenses, package dataset IDs, and Schema 3.0.2 versions. It creates no fourth
fixture data file, performs no package reopen, and reads no public
measurement.

## Performance contract and evidence

Budgets were fixed before implementation:

- ordinary navigation, search, filtering, sorting, and selection design target:
  p95 at or below 50 ms;
- hard interaction gate: p95 at or below 100 ms;
- retained bundle: at or below 32 MiB each;
- all three fixture bundles: at or below 64 MiB combined;
- 50,000-entry scaled index: at or below 64 MiB;
- one complete 50,000-entry item-neighbourhood and hierarchy-focus operation:
  at or below 10 MiB temporary allocation;
- one complete expanded textual hierarchy-panel render: at or below 10 MiB
  temporary allocation and subject to the same p95 interaction gates.

The first scaled run exposed path sorting at p95 107 ms. Stable integer sort
ranks are now computed once during the truthful background indexing phase.
The current rerun on R 4.6.1 reported:

| Inventory | Entries | Field groups | Search p95 | Select p95 | Item p95 | Hierarchy p95 | Hierarchy allocation |
|---|---:|---:|---:|---:|---:|---:|---:|
| TUM | 1,428 | 354 | 1 ms | 1 ms | 1 ms | 10 ms | 244,320 B |
| IZTECH | 1,144 | 326 | 1 ms | 1 ms | 1 ms | 9 ms | 162,256 B |
| UCR all-variable | 4,874 | 723 | 1 ms | 1 ms | 1 ms | 9 ms | 705,832 B |
| TUM plus IZTECH append | 2,690 | 740 | 1 ms | 1 ms | 1 ms | 10 ms | 313,976 B |
| Scaled UCR | 50,000 | 7,683 | 4 ms | 1 ms | 2 ms | 15 ms | 7,444,392 B |

The reordered hierarchy shell and its expanded panel reported:

| Inventory | Collapsed shell p95 | Expanded panel p95 | Panel allocation |
|---|---:|---:|---:|
| TUM | 6.25 ms | 47 ms | 646,088 B |
| IZTECH | 3 ms | 33 ms | 394,392 B |
| UCR all-variable | 3 ms | 48 ms | 1,095,576 B |
| TUM plus IZTECH append | 4 ms | 33 ms | 567,536 B |
| Scaled UCR | 3 ms | 46.65 ms | 7,850,760 B |

Navigation, filtering, and sorting also remain at or below 7 ms p95 in every
case.
The three bundles occupy 6,225,672 bytes combined. The scaled index occupies
35,702,368 bytes, and the appended review bundle occupies 5,036,512 bytes.
All design, hard timing, and memory gates pass. Benchmarked selection returns
one semantic field group plus all exact retained entries in that group. The
neighbourhood benchmark adds same-item properties, the exact ancestor path,
children, and structural branch shortcuts. Benchmark operations accept only
the prebuilt index, so package reopen and record deserialization counts are
zero. The acceptance artifact also asserts the six exact appended IZTECH file
groups, their device ids, and the two distinct S004 MEDI instances. Exact
machine-readable results from 2026-09-01T06:32:53Z are in
`dev/milestone-7-metadata-benchmark.json`.

## Production interaction contract

The production navbar contains a Metadata destination. The explorer provides:

- a selected-dataset context card with a derived-fixture label, exact public
  package/revision, variable contract, and bounded-fixture disclaimer when
  those facts are present;
- clear source and LightLogWeb overlay cards;
- complete, partial, absent, empty-result, and no-selection states;
- fixed-text raw and plain-language field-name/value search with an obvious
  clear action; source labels, record labels, and full paths are not repeated
  into every field's search text because dedicated filters and hierarchy
  controls already expose that context;
- native section and sort controls whose menus are not clipped by their card;
- compact filter placement whose absent source/layer controls reserve no empty
  grid tracks at desktop, tablet, or mobile widths;
- a source-dataset filter and source labels only when an appended record has
  multiple source namespaces, plus a distinct metadata-layer filter only when
  overlay values exist;
- stable location, field, value, and conditional source sorting with reverse
  order;
- a bounded 40-field result list with each matching field directly selectable
  by mouse or keyboard;
- one result per semantic field location, with repeated record values grouped
  rather than duplicated in search results;
- a selectable same-item property panel. For example, `VLSQ8` exposes the
  retained participant, name, value, unit, and description fields together,
  while concrete `MEDI` variable records expose their label, type,
  description, and `lx` unit;
- a focus-and-context hierarchy navigator with a selectable ancestor path,
  contained records, fields at the focused level, and nearby structural
  branches;
- one collapsed-by-default `Explore metadata structure` accordion immediately
  above `Selected metadata field`. Its summary always names the current field
  and hierarchy focus. Expanding it reveals the same-item and hierarchy
  selectors plus explicit direction text. Branch choices navigate within the
  panel, concrete field choices replace only the detail below, and search,
  filters, sorting, and the matching-result list above stay unchanged;
- an exact-instance selector when a semantic item is repeated, so grouped
  values remain compact while a participant, dataset, or file-group path can
  still be inspected individually;
- recursive retained file-group lookup inside an appended source namespace,
  so the values table, exact-instance selector, hierarchy ancestors, and
  expanded focus identity all preserve `MELIDOS_IZTECH_S004:19` and `:20`
  instead of falling back to their shared parent dataset id;
- explicit language that shared containment is not semantic equivalence. The
  `melanopic_edi` term record retains only its source term and label, while the
  concrete `MEDI` record retains its unit. Navigation connects their source
  locations without copying or inferring a unit for the term;
- a separate, plain-language section/subsection/field hierarchy and all exact
  retained values and paths in the selected-field detail;
- parent dataset or file-group identity retained separately from semantic item
  and value, so file-variable and file-device relationships remain readable;
- explicit source or overlay provenance and bound fingerprints;
- responsive single-column behavior at narrow widths.

No bespoke JavaScript is present. Standard Shiny and bslib input dependencies
remain in use. No interactive visualization dependency is included in this
reorder gate.

The offline review route in `dev/milestone-7-metadata-app.R` loads the actual
TUM MEDI-only, IZTECH LIGHT plus MEDI, and UCR all-variable RDS fixtures. It
also constructs the manifest-defined TUM plus IZTECH append and adds one
metadata-absent fallback record, for five selectable review states. It never
opens a package. Each frozen snapshot fingerprint is validated first, then
its immutable sources and overlay receive one current in-memory search index
at showcase startup. This leaves the serialized snapshot untouched and avoids
reindexing during ordinary interactions. Exact startup and retained-selection
timings are regenerated in `dev/milestone-7-metadata-benchmark.json` for every
frozen build. The current 20-run construction p95 is 4,739.6 ms, before a
development user session exists, and retained fixture selection is p95 0 ms
across 200 runs. This
startup-only fixture construction is not present in the production app and is
not an ordinary interaction gate. A dataset change clears the prior query and
hierarchy filters. Zero-match searches clear the authoritative selected
detail, and an empty underlying index uses a metadata-absent message rather
than suggesting an impossible filter recovery.

## Current verification evidence

- The focused metadata-explorer test file passes after Air formatting, and
  the complete `devtools::test()` suite passes with no failures. Six
  daemon-only tests are skipped in this runtime and two expected recovery
  warnings are exercised.
- `R CMD check` completes with 0 errors, 1 warning, and 1 note. The warning is
  the baseline's undeclared test-only `jsonlite::` use, which is already
  present at commit `b704d24ee6eab066163bb88e7125faf19e09543d`; changing
  dependency metadata is outside this approved reorder boundary. The note is
  the offline clock-verification note.
- Browser checks at 1,440, 768, 390, and 320 CSS pixels confirm that the
  hierarchy panel is folded initially, sits before the selected-field detail,
  stays open while its focus and selected field update, and has no horizontal
  overflow. Result selectors retain a 16 px left inset, expanded exact paths
  retain 32 px list padding, and the dataset menu has no clipping ancestor.
- Browser flows cover `VLSQ8` same-item value selection, melanopic EDI
  datasheet-unit discovery, the two exact IZTECH S004 device records,
  zero-match recovery, fixture changes, and the metadata-absent fallback.
  The browser console reports no warnings or errors. Controls remain standard
  semantic buttons, radio groups, checkboxes, and native/selectize inputs, so
  no bespoke keyboard or pointer implementation is introduced.

## Review boundary

The frozen Slice 1B review packet must include this document, exact Git status
and diff, fixture and benchmark manifests, focused/full/package checks, and
wide/tablet/mobile browser evidence. Reviewer acceptance authorizes only the
owner final check. It does not authorize commit or Slice 2.
