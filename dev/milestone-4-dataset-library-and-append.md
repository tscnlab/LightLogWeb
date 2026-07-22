# Milestone 4 — dataset library and safe append merging

Date: 2026-07-22

Status: complete; accepted by the owner on 2026-07-22. This evidence is
development-only and is excluded from package builds.

The owner accepted the completed milestone after the final mapping-card layout
and safe re-append review. The complete test suite passed, the source-package
check completed with 0 errors, 0 warnings, and 0 notes, the reviewed browser
state had no console errors, and the local showcase was stopped at closeout.

## Compatibility basis

- R 4.5.0
- LightLogR 0.10.3
- Shiny 1.14.0
- bslib 0.11.0
- testthat 3.3.2
- shinytest2 0.5.1

The implementation uses the existing immutable `llw_dataset_record` boundary.
It does not infer scientific equivalence from a shared column name, unit, or
device label. It does not interpolate, fill gaps, replace missing values with
zero, or discard rows during append.

## Ready-to-use dataset library

The main dataset sidebar exposes the available entries from
`dataset_example_catalog()`. In the reviewed development checkout all four are
available:

| Key | Display choice | Rows × columns | Participants | Source zone | Device |
|---|---|---:|---:|---|---|
| `sample` | LightLogR test data (small) | 69,120 × 3 | 2 | Europe/Berlin | ActLumus |
| `actlumus_synthetic` | ActLumus synthetic device file (small) | 3 × 35 | 1 | UTC | ActLumus |
| `actlumus_4789` | ActLumus 4789 provided device file | 4,686 × 27 | 1 | Europe/Berlin | ActLumus |
| `iztech` | IZTECH light glasses (151,200 rows) | 151,200 × 37 | 17 | Europe/Istanbul | Unknown |

The small package sample and pinned IZTECH snapshot use their existing reviewed
record builders. The two ActLumus files use the same validated LightLogR import
boundary as an interactive upload, but their device, version, and source zone
are already configured. They are staged through a private temporary copy, so a
development path is never retained in durable provenance. This provides a
one-click session copy without requiring the import form on every Milestone 4
test.

Development-only choices disappear safely when their payload is absent, such
as in a built source package. The catalog still reports their unavailability;
the UI only offers usable entries. “Start new import” remains next to the
ready-to-use selector and continues to open the complete Milestone 3 workflow.
The configured fixture zones are documented in `dev/fixtures/README.md` and
are fixture decisions, not timestamp inference.

## Library actions and inventory

- Dataset identity uses the stable record ID. A display-name edit changes a
  label, never a storage key.
- Display names are unique within each session after trimming surrounding
  whitespace and case folding. The session model enforces the invariant for
  every add or replace, covering import, ready examples, append, duplicate,
  rename, and future loaders without relying on individual screens.
- Duplicate creates a new stable ID and independent record while preserving the
  canonical payload and recording `duplicated_from` provenance. Duplicate and
  rename use a reusable naming dialog with inline conflict feedback; a reusable
  outer conflict dialog catches workflow races, accepts a replacement name, and
  retries the original event. Conflicts never overwrite or silently suffix an
  existing dataset.
- Reset restores prepared data to the immutable canonical raw payload and
  clears draft/history state.
- Delete and reset require confirmation. Switching and immediate duplication
  are explicit stable-ID events.
- The dashboard reports source type, filenames, available source-file bytes,
  canonical payload size, device, IDs, absolute UTC span, source and recorded
  datetime zones, dominant sampling interval, primary variable and unit,
  calibration evidence, recipe revision, recipe/draft/history state, and
  warnings. Missing values render as `Unknown`.

## Safe append contract

`new_append_spec()` is the serializable source of truth. Every selected source
requires an explicit ID column, POSIXct main datetime column, numeric primary
measurement, measurement target, source prefix when applicable, and optional
source-to-target mappings. The specification also records the measurement
layout, output zone, and time-alignment rule. Existing within-source duplicates
are always retained and flagged. A cross-source overlap policy is required
only when IDs are preserved; unique source prefixes make that policy
inapplicable.

The preview performs the following checks before creation:

1. Validate required columns, usable IDs, complete POSIXct values,
   numeric primary measurements, output-column collisions, output dataset-name
   conflicts with the current session, and optional-column coercions.
2. Compare rows, unique IDs, source and recorded datetime zones, dominant
   sampling, mapped types, primary units, devices, and calibration evidence.
   Missing metadata remains visibly `Unknown`.
3. Apply one explicit time rule to the main datetime and every retained
   POSIXct column. The safe default preserves written local clock labels in a
   UTC output zone with `force_tz`; the alternative preserves absolute instants
   with `with_tz`; a no-adjustment choice is available only when the mapped
   sources share one zone. Ambiguous or nonexistent daylight-saving labels are
   blockers. Each output row retains source dataset ID/name/row, source zone,
   the original local timestamp with offset, and source device. User-facing
   tables format every POSIXct value with its clock label, numeric UTC offset,
   and IANA zone rather than relying on the table widget's UTC serialization.
4. Build a union of mapped columns. The safe default keeps source measurements
   separate. Explicit combining requires confirmation of the intended
   quantity and output unit, acknowledgement of unknown units when present,
   and acknowledgement of distinct or unknown devices. A third constrained
   strategy keeps one shared numeric column name and unit, and is enabled only
   when every source has such a candidate. `lx` and `lux` are treated as label
   aliases and recorded as a warning; the warning names only recorded labels
   that differ from the output label. Genuinely differing labels need an
   explicit same-numeric-scale confirmation and remain a warning; no unit
   conversion is attempted.
5. Retain and flag rows that already share an ID/timestamp within a source.
   Prefixing with `prefix_ID` is the safe default and prevents cross-source
   collisions when prefixes are unique. When IDs are preserved, the user
   either blocks or retains and flags cross-source overlaps; the result also
   receives a regular `Source` column containing dataset display names for
   later grouping.
6. Bound the browser row preview to 100 rows while computing diagnostics from
   the complete mapped union.

Create rechecks a fingerprint of every source checksum/revision and the complete
append specification. Any changed source, mapping, policy, confirmation, or
output label makes the preview visibly stale and blocks creation. A successful
preview is single-use, preventing a second click from creating an accidental
duplicate record.

The committed result is a new immutable `append_merge` record. Its source
manifest contains every source ID, name, checksum, revision, original
manifest, explicit mapping, policy, confirmation, compatibility result, and
quality count. Source records and their checksums remain unchanged. Append
provenance columns are excluded from analysis-variable eligibility. They remain
stored for reproducibility but are hidden from the user-facing row sample and
development inspector; ordinary output fields such as `Source` remain visible.
They are also removed from every later append column-choice pool, and mapping
validation rejects them even if a stale or hand-built request names one.

## Acceptance matrix

| Case | Expected contract |
|---|---|
| Matching schemas | Explicit mapping, separate measurements by default, constrained identical-column strategy available, all rows retained |
| Differing schemas/types | Union of columns; conflicting shared targets require separate names or explicit character coercion |
| Equal ID labels | Source prefixes by default; exact preservation adds a grouping-ready `Source` column |
| Different source zones | Default UTC output preserves local clock labels; optional instant preservation converts clock displays; source/local context remains |
| Same source/output zone | The plan reports both clock time and instant preserved; the optional no-adjustment path retains a shared source zone; incompatible optional POSIXct zones still block |
| Retained POSIXct columns | The selected `force_tz`/`with_tz` rule is applied to every retained datetime column |
| Datetime table rendering | Clock label, UTC offset, and IANA zone are explicit, so preserved-clock and preserved-instant results are visually distinguishable |
| DST ambiguity/nonexistence | Preview blocks and asks for source-time correction or instant-preserving alignment; it never guesses |
| `lx` and `lux` | Warning lists recorded units and calls out only the alias label differing from the output; no numeric conversion |
| Other differing units | Explicit same-scale confirmation changes the blocker to a recorded warning; no numeric conversion |
| Unknown unit/device/calibration | Visible uncertainty; separate append remains usable; combined append requires acknowledgement |
| Within-source duplicate | Always retained, counted, and flagged as a pre-existing source property |
| Cross-source overlap | Inapplicable under unique prefixes; explicit block or keep-and-flag choice for preserved IDs |
| Missing optional metadata | Remains `Unknown`; does not imply compatibility |
| Existing dataset display name | Add/replace is rejected after trim and case folding; current session state remains unchanged and the user must choose another name |
| Changed source/specification after preview | Visible stale state; creation blocked until a new preview |
| Repeated Create | Consumed preview blocks an accidental second record |
| Successful Create | New immutable record; complete provenance; source records unchanged |
| Re-append an appended result | Internal provenance remains stored but cannot appear in or pass any source-column mapping |

## Automated verification

The main coverage is in:

- `tests/testthat/test-append-merge.R`
- `tests/testthat/test-example-library.R`
- `tests/testthat/test-module-contracts.R`
- `tests/testthat/test-app-smoke.R`
- `tests/testthat/test-raw-import-quality.R`

Run from the repository root with:

```sh
RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e \
  'devtools::test(reporter = "summary", stop_on_failure = TRUE)'

RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e \
  'devtools::check(document = FALSE, manual = FALSE, error_on = "never")'
```

The complete checkout test suite finished with 0 failures, 3 warnings, and 2
skips. The only skips were the two existing
mirai-daemon cases because that backend is unavailable in this runtime; the
three warnings are the existing Shiny build-version warning and two tests that
intentionally exercise rejected background work. The built source-package
check completed with 0 errors, 0 warnings, and 0 notes. Optional development
fixture tests skip when the excluded payloads are absent from the tarball.

The local R library reports two unrelated incomplete installation directories
and `renv` reports that the project is out of sync. Tests and checks were run
with the renv sandbox disabled because the managed environment cannot create a
lock in the user cache; neither environment condition changed project files or
the clean package-check result.

## Browser verification

The integrated `LightLogWeb()` app showed all four ready-to-use choices. The
browser loaded the 69,120-row LightLogR sample and the three-row synthetic
ActLumus device file directly, then opened Append with both sources selected.
“Start new import” remained available throughout.

The revised append showcase also verified:

- a four-step Sources & IDs → Time & measurements → Output time → Review &
  create flow with later steps gated until their preceding decisions were
  reviewed;
- prefix controls and the overlap policy appearing only under their relevant ID
  choices;
- a real multi-select optional-column control with a select-all choice and no
  exposed configuration text, plus select menus that escape scrollable cards
  and accordions instead of being clipped;
- source mapping cards stretched to the same height within each two-column row,
  with core mapping controls aligned from the top and nested Advanced panels
  using the same corner geometry as their parent cards;
- an inline minimum-two-sources warning and temporary validation notifications
  that dismiss automatically;
- an identical-column strategy that is disabled for incompatible fixtures and
  enabled for the matching `MEDI`/`lux` fixture, plus automatic `MEDI` and
  `lux` output defaults for explicit combine;
- a nine-row differing-schema preview with two sources, zero duplicates, zero
  overlaps, UTC and America/New_York source zones, and explicit `lux`/`lx`
  labels;
- an alias warning that names only `lx` as differing from output `lux`;
- a regular `Source` result column containing both dataset display names when
  IDs were preserved;
- a compact four-value diagnostic strip and two readable source profile cards;
- a dominant initial Preview action, a disabled Create action before preview,
  and a visible stale state with a dominant Update preview action after changing
  the time-alignment rule;
- a UTC output-time plan for every source with clock-label, instant-preserving,
  and shared-zone explanations, including tooltip examples and row-specific
  confirmation that both clock time and instant remain preserved when the
  current and output zones already match;
- adjustment of both the main datetime and an optional `DiaryTime` POSIXct
  column under the instant-preserving rule;
- successful creation and selection of exactly one new record after a refreshed
  preview;
- a consumed preview disabling repeated creation;
- a development inspector that can show each fixture or a newly created result;
- user-facing row samples and the development inspector hiding internal append
  provenance columns while retaining ordinary analysis/grouping columns, with
  explicit clock labels, UTC offsets, and IANA zones for datetime values;
- a newly appended result offering only ordinary user columns when selected as
  a later append source, while its internal provenance remains stored;
- blocking preview messages offering a direct action to the earliest wizard
  step that can resolve the problem;
- radio choices whose wrapped labels align under their first line rather than
  under the radio marker;
- duplicate and rename naming dialogs rejecting whitespace/case variants of an
  existing name without changing the dataset list;
- usable layouts at 390 × 844, 1024 × 900, and 1440 × 900 without horizontal
  page overflow; and
- no application console errors.

## Requirement traceability

| Plan item | Direct evidence |
|---|---|
| 1. Library actions | Stable-ID manager events, confirmation modals, independent duplicate records, raw reset, normalized session-wide name uniqueness, reusable name/conflict dialogs, and module tests |
| 2. Provenance/inventory | `dataset_record_inventory()`, dashboard status table, explicit unknown rendering, and catalog/browser checks |
| 3. Explicit append mappings | `new_append_source_mapping()`, `new_append_spec()`, four-step wizard cards, select-all optional mapping, and differing-schema tests |
| 4. Compatibility comparison | Source profiles, readable comparison cards, optional-target comparison, and collision/coercion tests |
| 5. Explicit time semantics/local context | `force_tz`/`with_tz` regressions, shared-zone path, DST blocker, all-retained-POSIXct test, time-plan UI, and per-row source/local provenance |
| 6. Union and measurement safety | Separate default, constrained identical-column path, automatic equal-name/unit defaults, union tests, precise `lx`/`lux` alias warning, differing-unit confirmation, device warnings, and unknown acknowledgements |
| 7. Duplicate/overlap handling | Automatic within-source retain-and-flag, conditional preserved-ID overlap policy, prefix non-overlap tests, and grouping-ready `Source` column |
| 8. Immutable append result | Fingerprint guard, single-use Create, complete source manifests, checksum regressions, and unchanged-source tests |
