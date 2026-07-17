# LightLogWeb legacy implementation audit

Status: Milestone 0 evidence

Audited: 2026-07-16 on branch dev

## 1. Audit rule

The existing repository is a partial implementation from an earlier
development period. It is not a baseline or source of truth. Every part is
classified as:

- **Salvage:** the underlying behavior remains useful and can be retained after
  focused tests and naming/style cleanup.
- **Adapt:** the user concept is useful, but the implementation must be changed
  to satisfy the new contracts.
- **Replace:** retaining the implementation would carry forward incorrect
  scientific behavior, unsafe state, poor failure containment, or an
  incompatible architecture.

No classification authorizes feature work before Milestone 0 acceptance.

## 2. Repository-level findings

- Branch dev is the only development source considered. mockup is ignored.
- The package has one exported launcher, LightLogWeb, and a small modular Shiny
  skeleton for import, dataset selection, metadata, preprocessing, dashboard,
  summaries, and table views.
- There are no substantive automated tests.
- The historical renv.lock and manifest contain obsolete package versions and
  cannot be treated as a compatibility baseline.
- DESCRIPTION still contains a placeholder description and an R minimum older
  than current LightLogR requires.
- The app launch currently fails in the available environment because legacy
  direct dependencies leaflet and shinycssloaders are not installed.
- A package archive can be built from source, but the initial package check
  stops at missing dependencies.
- Existing state uses names as storage keys and nests mutable reactiveValues.
- Raw, prepared, factual metadata, analysis settings, and cached summaries are
  mixed in the same mutable object.
- Existing code contains a scientifically unsafe automatic force_tz operation
  when saving metadata. This can change absolute instants merely because a user
  edits metadata.
- Existing preprocessing aggregates the already processed dataset. Repeated
  changes therefore do not guarantee recomputation from raw data.
- Long work is synchronous and has no stale-result protection.

## 3. File and feature disposition

| Area/files | Disposition | Useful material | Required change |
|---|---|---|---|
| R/LightLogWeb-app.R | Adapt | Exported launcher name; packaged shinyApp pattern; navbar prototype | Implement explicit profile/max_upload_mb/workers API; 200 MB default; validated session model; resource cleanup; new workflow navigation |
| R/LightLogWeb-package.R and NAMESPACE | Adapt | Package documentation/roxygen structure | Narrow imports, document new API, regenerate namespace from current dependencies |
| R/m_import.R | Adapt substantially | Stepwise import concept; device/version choices from LightLogR; Id preview; overview/table idea | Separate pure preflight/import; stage unchanged bytes; task execution; typed errors; eligible numeric variable selection; explicit checks and source manifest |
| R/get_versions.R | Salvage after tests | Public supported_versions-based lookup | Handle empty/invalid device and future column/API changes with contract tests |
| R/set_Id_preview.R and R/helper.R | Adapt | Previewing automatic/manual/regex Id mapping | Return validated mapping object; handle duplicates, missing matches, extensions, Unicode, and hostile filenames; add unit tests |
| R/rename_files.R | Replace | Need to expose original filename to parsers | Never rename/move upload temp paths in place; copy/stage safely and store original name separately |
| R/import_data.R | Replace orchestration | LightLogR import_Dataset remains the parser; VEET modality and Id arguments are useful requirements | Pure adapter with explicit arguments; structured diagnostics; no captured exception object printed as success text; no direct input object dependency |
| R/import_notification.R and R/import_modal.R | Replace presentation | Need progress, success summary, and recoverable errors | Common task/error states and concise safe messages |
| R/build_metadata_hull.R | Replace | Field inventory provides migration hints | Immutable validated dataset record; separate metadata/settings/recipe; stable ID and revision |
| R/adding_dataset.R | Replace | Need post-import quality summary and add-to-library event | Pure record constructor plus session-store event; no name-key mutation or eager nested reactive summaries |
| R/m_dataset_manager.R | Adapt substantially | Switch/rename/delete/import affordances; delete confirmation | Stable storage IDs; duplicate/reset; status summaries; explicit events; merge wizard later; names remain labels |
| R/collect_dataset_names.R, R/dataset_lens.R, R/rename_dataset.R, R/delete_dataset.R | Replace | Behavioral requirements for selection and confirmation | Operate on validated records by stable ID; test transitions; prevent state mutation through nested references |
| R/load_testdata.R | Adapt | Correct small LightLogR fixture and useful location/variable metadata seed | Construct immutable record/source manifest; derive current metadata truth; add larger melidosData path separately |
| R/m_metadata.R | Replace | Useful field concepts and optional coordinate-map affordance | Separate factual metadata and settings; typed hierarchy; no data mutation on metadata save; with_tz display semantics; calibration as recipe; optional map must not force a heavy core dependency |
| R/m_preprocessing.R | Replace | Explicit Apply button and aggregation control are valid interaction ideas | Ordered recipe from raw; draft/preview/apply/reset/undo; full filter/gap/aggregation scope; task and stale protection |
| R/m_dataset_dashboard.R | Replace orchestration; salvage output ideas | Dashboard, plot, summary, and table destinations | Scalable adaptive views; explicit specs; server-side table; no unconditional full-data gg_days/summary computation |
| R/m_summaries.R | Adapt | Participant, epoch, coverage, gap, and irregularity overview concepts | Pure quality adapter; correct per-window definitions; robust empty states; no direct nested state reads |
| R/prepare_quality_metrics.R | Replace | Quality categories | Return validated pure value object; distinguish raw/prepared basis; cover duplicates/DST/missingness |
| R/UI_overall.R and R/no_dataset_modal.R | Replace | Funding/footer and empty-state requirements | Verified final wording, accessible layout, design-system components, intentional empty state |
| R/tooltip.R | Salvage after accessibility review | Reusable tooltip wrapper | Ensure every tooltip has a non-hover path and accessible trigger/label |
| app.R | Adapt | Local development launcher | Keep build-ignored; use explicit development loading compatible with refreshed environment |

## 4. Scientific hazards that must not survive refactoring

### Automatic timezone reinterpretation

R/m_metadata.R applies force_tz to all POSIXct fields when a metadata timezone
differs from the current Datetime timezone. That changes instants without a
scientific correction workflow. It must be removed. Display/analysis timezone
uses with_tz semantics; wrong-label reinterpretation becomes an advanced,
previewed recipe step.

### Mutation of canonical and processed data

Metadata saving overwrites dataset$data, then selects only Id, Datetime, and the
primary variable into data_processed and applies factor/offset in place. This
mixes factual metadata, analysis settings, calibration, column selection, and
preprocessing. It can discard source columns. The new model keeps raw data
immutable and transformations explicit.

### Chained aggregation

R/m_preprocessing.R aggregates data_processed rather than evaluating a complete
recipe from raw. Changing resolution after a prior aggregation can therefore
produce a different result from a fresh calculation. Replace it with recipe
evaluation and regression tests.

### Silent or premature duplicate/DST choices

The current import surface lets users request duplicate removal and DST
adjustment before presenting detailed diagnostics. Both can alter observations
or time. Preflight and preview must explain consequences, and no default can
silently discard or double-correct.

### Names as keys

Dataset names currently serve as reactiveValues keys. Rename is implemented by
copying to a new key and deleting the old key. References and task results can
become stale or ambiguous. Stable IDs and revisions replace this.

### Synchronous expensive work

Import, overview plotting, summaries, gap checks, and aggregation run in the
main reactive process. Large data can freeze the session and failures can
invalidate outputs. The shared task layer and bounded previews are required.

## 5. Salvage sequence

When the relevant milestone begins:

1. Write a behavioral characterization test for any legacy helper proposed for
   salvage.
2. Compare it with the current LightLogR public API and the approved
   analysis-principles rule.
3. Extract pure computation from Shiny inputs/session/state.
4. Retain it only if the adapted code is clearer and lower risk than a small
   replacement.
5. Delete obsolete paths only within the accepted milestone and only after
   replacement tests pass.

This is intentionally a conservative salvage policy: existing UI concepts can
accelerate implementation, but legacy architecture and scientific side effects
do not.
