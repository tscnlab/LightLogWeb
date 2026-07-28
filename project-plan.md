# LightLogWeb full implementation plan

Last updated: 2026-07-28

This is the governing implementation plan for LightLogWeb. It is tracked in
version control but excluded from package builds by the exact
.Rbuildignore rule:

    ^project-plan\.md$

Internal milestone evidence and standalone module showcases live under dev/,
which is also excluded from package builds.

## 1. Product goal

LightLogWeb will be a packaged, modular Shiny interface for the complete
LightLogR workflow:

    import → inspect → prepare → group → visualize, summarize, and calculate
    metrics → export reproducibly

The app must make scientifically consequential decisions visible, preserve the
unchanged source data, remain useful without optional metadata, and fail with
helpful messages rather than ending a Shiny session.

Whenever scientific precision is preserved, the app should communicate in
simple, concise, and comprehensible language.

## 2. Governing decisions

### 2.1 Sources of truth

The existing repository is a partial legacy implementation. It is material to
audit and selectively salvage, not the product specification and not the
scientific ground truth.

The precedence order is:

1. Approved owner requirements and later owner decisions.
2. The approved LightLogWeb analysis-principles specification.
3. Current public LightLogR documentation, articles, webinar workflows, and
   public APIs for the supported compatibility baseline.
4. The documented public glcdp API and supported GLC package schemas for GLC
   discovery, access, metadata, and import behavior.
5. Explicit LightLogWeb contracts and tests.
6. Existing LightLogWeb code, only where it still satisfies the preceding
   sources.

Existing code is retained only when its behavior is scientifically correct,
compatible with the intended architecture, testable, and cheaper to adapt than
replace. Visual appearance, mutable state structures, implicit defaults, and
old dependency choices are not inherited by default.

### 2.2 Development and scope

- Develop only from dev. Ignore the mockup branch completely.
- Keep project-plan.md under version control and out of package archives.
- The owner opened the GLC/glcdp implementation gate on 2026-07-17. GLC source
  support is part of v1 and is implemented after the raw-import foundation,
  source-agnostic dataset management, and scalable dashboard. It precedes
  metadata integration and preprocessing.
- Treat time-aligned multi-stream merging and multi-input metrics as a
  post-v1 expansion. Row-wise append and the curated single-measurement-variable
  metric set remain in v1.
- Use native Shiny and bslib with custom Sass/CSS. Bespoke JavaScript requires
  separate approval.
- Keep LightLogWeb as an MIT-licensed R package.
- Do not deploy, publish, change a registry, or create external resources
  without explicit authorization.

### 2.3 GLC/glcdp integration basis

- The reviewed integration target is glcdp 1.0.0 at public Git commit
  `76b532a7167edb212059734234ff6ed6fe10f9e2`, verified against the GitHub
  origin on 2026-07-28. The existing dependency files remain at the historical
  0.90.0 Milestone 0 baseline until the production upgrade at Milestone 6.
  glcdp 1.0.0 was not available from CRAN at review time, so the exact-commit
  policy remains in force.
- glcdp is the sole boundary for GLC registry discovery, immutable validated
  revision selection, Git/Git-LFS transport, schema-normalized inventories,
  metadata reads/search, data import, compatible collection, subset download,
  and the glcdp reproducibility manifest. LightLogWeb must not duplicate those
  responsibilities.
- The LightLogWeb adapter uses exported, documented glcdp functions and return
  contracts only. It must not call glcdp internals or couple feature modules to
  the mutable internals of a glc_package handle.
- glcdp 1.0.0 preserves the names and formals of all 14 lower-level public
  functions in the reviewed 0.90.0 adapter contract. It additionally exports
  extract_metadata and add_metadata for relationship-aware metadata joins and
  glc_explore as a package-owned interactive explorer. LightLogWeb wraps the
  metadata helpers when Milestone 7 begins; it does not embed glc_explore or
  depend on its internal Shiny modules.
- Registry-backed sources default to latest_pass. The resolved repository,
  exact commit, validation/attestation state, schema version, registry
  generation time, and every selection are captured in LightLogWeb provenance.
- Schema 3.0.2 is the final current schema and primary metadata-driven import
  contract. Schemas 3.0.0 and 3.0.1 remain stable compatible predecessors for
  reproducibility and are enabled after parity tests. Schemas 1.0.0 and 2.0.0
  are limited legacy paths: expose only capabilities covered by fixtures,
  identify the limitations visibly, and do not imply the full typed Schema 3
  contract. Unsupported future schemas are rejected; any future schema that
  glc_schema_versions reports as experimental requires a visible warning,
  explicit acknowledgement, and its own compatibility fixture.
- The canonical Schema 3.0.2 bundle is released with GLC metadata validator
  0.5.2 at commit `19360dce2b2d5967809cbc68e45774c1725d39b7`.
  Its metadata-builder copy is identical. Relative to 3.0.1, 3.0.2 changes
  only device-datasheet validation: datasheet_channel must contain at least
  one entry when present. New packages should use 3.0.2.
- glcdp 1.0.0 provides read, search, import, metadata extraction/join, and
  subset-download APIs, but no public schema-aware metadata writer or validator
  API. GLC metadata are therefore explored unchanged and any LightLogWeb edits
  are stored as a separate overlay. Standards-compliant edited-package
  generation remains capability-gated until glcdp exposes a public
  write-and-validate contract.
- Hosted v1 supports public registered packages. Browser entry of private
  repository tokens is out of scope unless separately designed and approved.

### 2.4 Dependency and compatibility policy

- The historical renv.lock is not a baseline.
- At Milestone 0, resolve the project against current CRAN releases, using the
  current CRAN LightLogR release as the first analysis contract.
- Until glcdp reaches CRAN, pin its exact GitHub commit in the development
  lockfile and deployment manifest; never install it from a moving branch.
  When a CRAN release becomes available, move to the current CRAN release only
  after adapter parity tests pass, then remove the GitHub-specific resolution.
- Add glcdp to Imports when the production adapter begins. Do not attempt a
  CRAN release of LightLogWeb while a required glcdp release is unavailable on
  CRAN.
- The refreshed lockfile is an output of that resolution and records the
  reproducible environment; it must not be used to force obsolete versions.
- The initial runtime target remains R 4.5.0. DESCRIPTION must declare at least
  the minimum R version required by the resolved LightLogR release.
- Record exact resolved versions and the resolution date in the lockfile and
  Milestone 0 evidence.
- Later upgrades are intentional maintenance events: update from current CRAN,
  run compatibility and parity tests, review behavior changes, refresh the
  lockfile and deployment manifest, and document the result.
- Remove unused legacy dependencies. Add a dependency only when package code,
  tests, documentation, or an approved deployment path needs it.

### 2.5 Data and session guarantees

- Preserve uploaded source bytes unchanged within the session.
- Store imported LightLogR data as an immutable canonical base.
- Represent preparation as an ordered, versioned recipe recomputed from that
  base.
- Keep hosted data session-only: no accounts, server persistence, or
  cross-session storage.
- Offer downloadable and reloadable project bundles for continuity.
- Never perform arbitrary R evaluation, silent interpolation, silent timezone
  reinterpretation, silent deduplication, or silent data sampling.
- A 200 MiB limit applies to the total hosted upload request for one import
  action. The local default is the same, but an explicitly resourced local
  profile may raise it.
- GLC network materialization is not an upload, but it is subject to the same
  hosted memory, disk, and task budgets. glcdp caches and downloads must use a
  session-scoped directory unless the user explicitly downloads an artifact.
- Provide resource-conscious hosted behavior and a higher-resource local
  profile.

### 2.6 Interaction and delivery

- Use guided defaults with progressively disclosed advanced arguments.
- Use plain language in interface labels, guidance, warnings, errors, reports,
  and documentation whenever this does not reduce scientific precision. Keep
  necessary scientific terms, explain them at first use, and do not replace
  them with simpler but ambiguous wording.
- Draft edits feed previews; Apply or Calculate is required before expensive or
  consequential work is committed.
- Every reduction, aggregation, exclusion, or sample shown as a preview is
  labelled.
- Every feature milestone ends with automated tests, a build-ignored internal
  module showcase, browser inspection, and owner acceptance before the next
  milestone starts.

## 3. Public and internal contracts

### 3.1 Public launcher

The sole required exported app API for v1 is:

~~~r
LightLogWeb(
  profile = c("auto", "hosted", "local"),
  max_upload_mb = 200,
  workers = NULL
)
~~~

- auto respects LIGHTLOGWEB_PROFILE; otherwise it chooses local for an
  interactive package launch and hosted-safe defaults for deployed or
  non-interactive launches.
- workers = NULL resolves to one background worker when hosted and at most two
  locally, bounded by available CPUs.
- workers = 0 is a synchronous fallback.
- Generated handover scripts use public LightLogR calls, not private
  LightLogWeb helpers.

### 3.2 Versioned session model

Use validated internal value objects rather than nested mutable
reactiveValues.

- **Dataset record:** stable ID, display name, immutable canonical raw data,
  source manifest, factual metadata, analysis settings, committed recipe,
  revision, and provenance.
- **Source manifest:** source type, original filenames, hashes,
  device/version, import arguments, source timezone, and import timestamp. A
  GLC source additionally records registry URL/generation time, repository,
  immutable commit, validation/attestation state, schema and glcdp versions,
  selected dataset/file-group/file/resource/variable/term identifiers,
  standardization mode, and the difference between preview and committed read.
- **Factual metadata:** optional study, participant, device, site, location,
  timezone, and variable descriptions.
- **Analysis settings:** primary variable, display scale, analysis timezone,
  plot defaults, missingness policy, and active grouping specifications.
- **Recipe:** ordered, versioned preprocessing steps with parameters and
  enabled state.
- **Metric instance:** stable ID, metric, label, parameters, grouping
  reference, eligibility, result, warnings, and LightLogR version.
- **Plot specification:** plot type, variable, participant/date selection,
  grouping/faceting, scale, styling, and data-reduction notice.
- **Project manifest:** schema version, package versions, checksums, dataset
  records, recipes, analysis specifications, and optional cached results.

Raw data are never mutated. Prepared data are materialized from raw data plus
the committed recipe and cached by dataset ID, revision, and recipe hash.
Dataset display names are unique within a session after trimming surrounding
whitespace and case folding. Every add or replace operation enforces this at
the session-model boundary, so imports, examples, appends, duplicates, renames,
and future loaders share one rule even when a workflow omits an early UI check.
Stable dataset IDs remain the storage and selection keys.

### 3.3 Portable project contract

Use a ZIP-based .llwproj format containing:

- A versioned JSON manifest and checksums.
- Canonical raw data as RDS for type- and timezone-safe reload.
- Metadata, settings, recipes, plot specifications, and metric specifications.
- Generated analysis.R, package/session information, and a README.
- Optional original device files and optional result artifacts.
- For GLC sources, the serializable source specification and, when selected,
  the glc_download package subset including glcdp-manifest.json.

Loading validates archive paths and size, checksums, schema version, and package
compatibility before creating session state.

### 3.4 Active GLC deep-link contract

On launch, inspect the raw query string and accept at most one repo parameter.
Decode it exactly once, require a conservative owner/repository form, and
reject repeated values, full URLs, whitespace, extra path components, dot
segments, control characters, and malformed percent encoding.

A valid value is matched exactly against the current glc_packages registry,
then opened through glc_open with ref = "latest_pass". It preselects the GLC
source screen and starts discovery/preview only. It never downloads data,
creates a dataset, accepts an unvalidated fallback revision, or bypasses an
Apply confirmation.

### 3.5 glcdp adapter contract

Feature modules depend on a small LightLogWeb adapter, not directly on
glc_package objects. The adapter owns three serializable value objects:

- **GLC source specification:** registry, repository, requested revision
  policy, resolved immutable revision, verification state, schema version,
  glcdp version, and session cache location.
- **GLC selection:** dataset ids, file-group ids, files, resources, source
  variables, semantic terms, primary-only flag, preview row budget, and import
  problem policy.
- **GLC preview:** package summary plus dataset, file, variable, resource, and
  metadata inventories; declared transfer size; availability; compatibility
  partitions; warnings; and explicit estimate uncertainty.

The adapter is implemented only with glc_packages, glc_search_packages,
glc_open, glc_schema_versions, glc_summary, glc_datasets, glc_files,
glc_variables, glc_resources, glc_metadata, glc_search_metadata, glc_read,
glc_collect, glc_download, extract_metadata, and add_metadata. The metadata
helpers enter the production adapter with Milestone 7. If exact provenance or
future write support cannot be obtained through documented outputs, adding the
required public accessor to glcdp is a prerequisite; LightLogWeb must not reach
into package internals. glc_explore is a package-owned reference application,
not an embedded LightLogWeb dependency.

Background tasks receive a source specification and reopen the immutable
package revision inside the worker. They do not serialize a live glc_package
handle or retain a user-specific transport environment in reactive state.
glcdp's own interactive progress is disabled inside the app; LightLogWeb owns
phase progress and cancellation/staleness messaging.

## 4. Milestones

### Milestone 0 — Isolated plan, scientific specification, and fresh baseline

Status: complete; accepted by the owner on 2026-07-17

Acceptance result: the owner approved the analysis principles and Milestone 0
baseline, authorized the refreshed package versions, and opened GLC/glcdp
integration. The post-acceptance dependency reconciliation pins the reviewed
glcdp GitHub revision and resolves all other locked packages from current CRAN.
The paused Milestone 1 work already present in the shared tree is deliberately
kept separate from this closeout.

1. Track this plan at the repository root, add the exact .Rbuildignore rule,
   and prove that package archives exclude it.
2. Review every current LightLogR article:
   - The whole game
   - Import and cleaning
   - Log transformation
   - Metrics
   - Photoperiod
   - Light spectrum
   - Durations, states, and clusters
   - Visualizations
3. Complete the beginner webinar and all advanced cases:
   - A Day in Daylight
   - High light sensitivity
   - Therapy lamps
   - Visual experience beyond light
4. Produce and approve dev/analysis-principles.md. For each principle record:
   - LightLogR function or established workflow
   - Safe app default
   - Required data or metadata
   - Warning versus blocking behavior
   - User-facing explanation
   - Automated acceptance scenario
5. Explicitly cover tidy Id/Datetime structure, absolute versus local time,
   mixed timezones, DST, implicit gaps, irregular sampling, zero inflation,
   symlog display, aggregation, partial groups, photoperiod,
   grouping-dependent metrics, device comparability, state data, and
   spectral/multimodal data.
6. Audit every legacy module as salvage, adapt, or replace. Legacy behavior
   does not become a requirement merely because it exists.
7. Replace the old dependency baseline with a fresh current-CRAN resolution.
   Align DESCRIPTION, renv.lock, and manifest.json; remove unused dependencies;
   and add development dependencies under the correct fields.
8. Capture the pre-refactor package build/check and app smoke-test state.

Acceptance gate:

- The analysis-principles specification is owner-approved.
- The legacy audit is reviewable.
- Dependencies restore reproducibly from the refreshed lockfile.
- The package plan is absent from the built archive.
- Baseline build/check and smoke evidence are recorded.
- No functional refactor begins before acceptance.

Milestone evidence:

- dev/analysis-principles.md
- dev/legacy-audit.md
- dev/milestone-0-baseline.md

### Milestone 1 — Core architecture, task handling, and failure containment

Status: complete; accepted by the owner on 2026-07-17

Acceptance result: the owner confirmed the draft, preview, and apply status
flow and all long-task success, contained-failure/retry, warning, stale-result,
and cancellation/retry flows. The automated acceptance gates, real mirai
worker path, package tests, source build, and package check were rerun at
closeout.

1. Replace nested mutable dataset state with the validated session model and
   stable dataset IDs.
2. Separate pure computation from Shiny orchestration. Modules receive explicit
   reactive inputs and return explicit values or events.
3. Implement draft, preview, apply, reset, and undo semantics.
4. Add a common long-task layer using Shiny ExtendedTask, promises, and mirai
   for raw import, GLC discovery/read/download, preparation, append merging,
   metrics, and report rendering.
5. Reject stale task results if the dataset revision changed while work ran.
6. Define task states: idle, queued, running, finalizing, complete, warning,
   error, and cancelled/stale.
7. Implement the glcdp adapter seam and serializable GLC source/selection
   value objects before building its feature module. Reopen immutable sources
   inside workers rather than passing live glc_package handles across process
   boundaries.
8. Define typed errors for import, validation, resources, preparation,
   grouping, metrics, export, network, and unavailable features. Map documented
   glcdp conditions into recoverable discovery, validation, transport, LFS,
   import, compatibility, and disk/resource messages while retaining private
   diagnostics.
9. Establish hosted and local resource profiles, bounded caches, one task per
   hosted session initially, and complete session-temp cleanup.

Acceptance gate: two sessions remain isolated; reset reproduces raw data
exactly; stale tasks cannot overwrite newer revisions; live package handles do
not cross worker boundaries; and a task failure leaves the app usable.

Milestone evidence:

- dev/milestone-1-core-architecture.md
- dev/milestone-1-core-app.R

### Milestone 2 — Design guide and LightLogWeb identity

Status: complete; accepted by the owner on 2026-07-20

Acceptance result: the owner approved the implemented design guide, component
gallery, Measured Day Arc logo, responsive expandable-plot pattern, and the
requested clipping, alignment, and control refinements. Automated
design-system tests, live gallery and production-browser checks, responsive
checks at 320, 768, 1321 × 1324, and 1440 px, a source build and package check
with 0 errors, 0 warnings, and 0 notes, and the preliminary collision scan are
recorded in the milestone evidence.

1. Define color, typography, spacing, responsive breakpoints, cards,
   navigation, tables, plots, forms, focus styles, and all system states.
2. Use a minimalist visual language with restrained flourishes derived from
   light, time, and circadian cycles.
3. Design a unique LightLogWeb hex logo related to, but distinguishable from,
   the MIT-licensed LightLogR logo.
4. Keep one editable vector source and derive header, pkgdown, favicon,
   light/dark, monochrome, and high-resolution PNG assets.
5. Meet WCAG 2.2 AA contrast and visible keyboard-focus requirements; never use
   color alone for status.
6. Build a bslib component-gallery showcase without prematurely fixing the
   final app information architecture.

Acceptance gate: owner approval of design guide, gallery, and logo.

Milestone evidence:

- dev/milestone-2-design-guide.md
- dev/component-gallery-app.R
- dev/brand/lightlogweb-logo-master.svg
- dev/brand/brand-provenance.md

### Milestone 3 — Robust raw import and test datasets

Status: complete; owner accepted on 2026-07-21

Verification result: the raw-import contract, deterministic fixtures, pinned
IZTECH development snapshot, package tests, clean-worker import, and generated
near-200 MiB boundary case all pass. `R CMD check` completes with 0 errors,
0 warnings, and 0 notes. Browser acceptance confirms the valid ActLumus flow,
the explicit analysis-focus return, both immediate example datasets, no
horizontal overflow at 320, 768, or 1440 px, the signal-profile and structural
eligibility guidance, and no browser-console errors. The follow-up review also
confirms that LightLogR receives original safe basenames and produces the final
participant IDs, participant-specific missing-time-point counts remain correct
for mixed sampling epochs, and the overview is based on `gg_overview()` with
compact, derived long-interval overlays. Those overlays compare the lag between
consecutive observations with each participant's dominant epoch without
expanding or filling the time series. The detailed review is a four-panel
accordion that keeps its parent review open and shows one detail panel at a
time. A session-local progress snapshot lets the browser show the currently
running import phase while the background worker is still processing data.
Malformed, wrong-timezone, unsupported, missing-column, retry, and over-limit
paths are covered by typed-condition tests and remain recoverable.
Plain and ZIP-wrapped VEET fixtures from both the original and current export
formats produce equivalent imported payloads, with safe worker-side extraction
and the original archive retained for source provenance.

Production-presentation addendum (2026-07-21): after owner review in the
isolated showcase, the four-step Source, Details & IDs, Check & import, and
Review wizard became the production `LightLogWeb()` import view. It retains the
accepted importer, validation, progress, quality, and returned-dataset
contracts; step completion now reflects actual input readiness and Review is
gated on a completed import. The legacy accordion remains available only as a
development comparison. `GENEActiv_GGIR` remains listed for discoverability,
but the app explains that this route is currently provided only by LightLogR,
clears the unsupported selection, and restores the full device list. This UI
promotion does not impute observations or change the separation of explicit
missing values, implicit gaps, and off-grid observations. All available tests
passed after promotion; the two clean-mirai-daemon checks were skipped in this
runtime. `R CMD check` completed with 0 errors, 0 warnings, and one
environment-only note because it could not verify the current time.

1. Salvage useful current import behavior only where it passes the new
   contracts; replace temporary-file renaming and mutable metadata coupling.
2. Enforce a 200 MiB per-action ceiling for the hosted profile and document
   reverse-proxy/host requirements. Keep 200 MiB as the local default while
   allowing an explicitly higher local limit when disk, parsing time, and
   memory are provisioned accordingly.
3. Stage unchanged bytes in a session directory, hash them, and keep original
   names separate from server paths.
4. Populate devices and versions dynamically through LightLogR public APIs.
5. Preflight size, extension, device/version, timezone, filename-to-ID mapping,
   missing mappings, repeated filenames, readability, and supported
   combinations. Allow several files to map to one participant while retaining
   each source filename and reporting overlapping times after import.
6. Preview filename-to-participant mapping.
   Accept validated single-payload VEET ZIP exports, extracting their CSV/TXT
   member only inside the background worker while retaining the archive as the
   source-provenance object.
7. Run large imports in a background worker with visible validation, import,
   normalization, quality, and preview phases. Retain a documented synchronous
   fallback for constrained runtimes.
8. Validate Id, POSIXct Datetime, names/types, ordering, duplicates, timezone,
   date range, participant count, epoch, explicit missingness, implicit gaps,
   irregularity, DST transitions, and per-variable missing/zero/negative/range
   profiles.
9. Show a post-import overview and let the user choose an initial analysis-focus
   variable from scalar numeric columns with finite observations. Explain that
   the focus can be changed later, give reasons for non-selectability, and warn
   that structural selectability does not establish units, calibration,
   placement, or cross-device comparability.
10. Offer:
    - Immediate LightLogR sample.data.environment with useful basic metadata.
    - For development purposes, keep an instance of the current CRAN melidosData IZTECH
      light_glasses_1minute dataset as a "large" example
11. Keep network access out of routine tests by using deterministic fixtures.

Acceptance gate: valid, malformed, wrong-timezone, unsupported, missing-column,
and generated near-200 MiB hosted cases produce a usable dataset or recoverable
message, never an app crash.

Milestone evidence:

- dev/milestone-3-raw-import.md
- dev/fixtures/README.md
- dev/generate-milestone-3-fixtures.R
- dev/run-milestone-3-large-file-acceptance.R
- dev/run-testdevice-import-acceptance.R

### Milestone 4 — Dataset library and safe append merging

Status: complete; accepted by the owner on 2026-07-22

Acceptance result: the owner approved the dataset library, four-step safe
append workflow, immutable result/provenance contract, explicit time-zone and
measurement semantics, session-wide unique naming, and the final layout and
mapping refinements. The complete package tests and source-package check were
rerun at closeout; `R CMD check` completed with 0 errors, 0 warnings, and
0 notes, and the accepted local showcase was stopped.

Sequencing boundary: implement this milestone with canonical raw imports and
test datasets. GLC-specific source integration and metadata-derived labels,
units, and crosslinks are deferred to Milestones 6 and 7. Missing metadata must
remain visible as unknown and must never be treated as evidence of
compatibility.

Implementation status (completed and reviewed 2026-07-22): the integrated app
now provides stable-ID dataset actions, complete available/unknown inventory
fields, four ready-to-use development choices, and a fingerprinted four-step
append wizard. Append keeps source measurements separate by default, defaults
to preserving local clock labels in an explicit UTC output zone, offers
preservation of absolute instants or an already-shared source zone, retains
source/local context, makes relevant compatibility decisions explicit, creates
one new immutable record, and leaves every source unchanged. Dataset names are
also unique within a session across import, example loading, append, duplicate,
and rename paths; conflicts keep the existing state unchanged and require a
different name.

1. Switch, rename, delete with confirmation, duplicate, and reset datasets.
   Display names are labels, not storage keys, but must be unique within the
   session after trimming and case folding. Use one reusable naming dialog for
   rename/duplicate entry and one reusable conflict dialog at the central
   session boundary that accepts a replacement name and retries the original
   operation; never silently suffix or overwrite a conflicting name.
2. Show available source provenance, size, participants, span, recipe revision,
   warning state, and explicit unknowns.
3. Implement a row-wise append preview wizard with explicit mappings for ID,
   datetime, primary measurement, and optional columns. Guide the user through
   Sources & IDs, Time & measurements, Output time, and Review & create.
4. Compare available types, units, devices, source timezones, sampling,
   overlaps, duplicates, missing columns, and coercions.
5. Make time-zone semantics explicit. Default to a UTC output zone with local
   clock labels preserved (`force_tz`), offer preservation of absolute instants
   (`with_tz`), and offer no adjustment when every source already shares one
   zone. Apply the selected rule to every retained POSIXct column while keeping
   source-zone and original local-time context. Block ambiguous or nonexistent
   daylight-saving clock labels instead of guessing. In the adjustment plan,
   report both clock time and instant as preserved whenever a source already
   uses the selected output zone. Format browser-table datetimes with their
   clock label, UTC offset, and IANA zone so widget serialization cannot make
   `force_tz` and `with_tz` appear reversed.
6. Use a union of columns. Keep source measurements separate by default.
   Combine measurements only after explicit compatible-quantity and unit
   mapping, or use the constrained identical-column path when every source has
   the same numeric column name and recorded unit. Treat `lx` and `lux` as
   recorded label aliases without numeric conversion, and mention only labels
   that differ from the selected output label in the warning. Unknown or
   genuinely differing units require an explicit user decision and visible
   warning; they are not assumed compatible.
7. Retain and flag duplicates that already exist within a source. Ask for a
   cross-source overlap policy only when IDs are preserved; unique prefixes
   joined to IDs with `_` make that policy inapplicable. When IDs are
   preserved, add a regular source-name column for later grouping.
8. Create a new immutable dataset with complete source/mapping provenance and
   leave sources unchanged. Keep internal append-provenance fields stored for
   reproducibility, but exclude them from later ID, datetime, measurement, and
   optional-column mapping choices and reject hand-built mappings that name
   them.

Acceptance gate: append handles matching and differing schemas, participants,
timezones, overlaps, devices, and absent optional metadata without losing
source information or assuming comparability. No workflow can add or replace a
dataset under a display name already used in the session. Appending an appended
dataset cannot expose or remap internal provenance fields.

Milestone evidence:

- dev/milestone-4-dataset-library-and-append.md
- dev/fixtures/README.md

### Milestone 5 — Scalable dashboard and raw/prepared-data table

Sequencing boundary: implement this milestone for canonical imports with empty
recipe and grouping states and without requiring GLC or optional metadata.
GLC-specific provenance panels and metadata-driven labels, units, and
crosslinks are added in Milestones 6 and 7 without changing the generic
dashboard and table contracts.

1. Show available provenance, participant/date span, sampling, variable
   inventory, missingness, gaps, irregularity, duplicates, DST, daily coverage,
   active recipe, and grouping. Empty or unavailable states must be explicit.
2. Choose detailed timelines for small/short data and overview/availability
   displays for many participants or long spans.
3. Add participant, date/window, and facet-page controls with explicit
   show-all warnings.
4. Bound preview plot data and label reductions.
5. Add a server-side DT table for the active data with search, sort,
   visibility, pagination, and type-aware formatting. Before preprocessing is
   available, the immutable canonical base is the active prepared result and
   the empty recipe is shown explicitly.
6. Keep raw and prepared views visually distinct and show the recipe revision,
   including the unchanged empty-recipe state.

Acceptance gate: the small sample and a synthetic 10-participant,
one-month, one-minute dataset remain interpretable without repeatedly sending
all data to the browser and without requiring GLC or optional metadata.

### Milestone 6 — GLC discovery, selective import, and package download

1. Upgrade the historical optional glcdp 0.90.0 dependency to glcdp 1.0.0 at
   exact commit `76b532a7167edb212059734234ff6ed6fe10f9e2` and promote it to
   Imports at the start of this milestone. Refresh the lock and deployment
   manifest only after adapter parity tests pass. Make any later CRAN
   transition a tested dependency update rather than an automatic source
   switch.
2. Build registry discovery with glc_packages and glc_search_packages. Show
   repository, registry id, current validation status, latest passing revision,
   attestation state, validation time, and whether a passing revision exists.
   Refresh is explicit and failures preserve the last successful session view.
3. Implement the active repo deep link exactly as specified in section 3.4.
   An exact valid match selects the package; no query parameter starts a read
   or download.
4. Open remote sources through glc_open(ref = "latest_pass") in a background
   task and immediately reduce the live handle to the serializable source
   specification and public preview outputs. Record the exact passing commit,
   not the moving registry label alone.
5. Preview glc_summary, glc_datasets, glc_files, glc_variables,
   glc_resources, and requested glc_metadata resources before materialization.
   Include study/dataset/participant/device context, schema status, modalities,
   timezones, data states, roles, temporal resolution, primary variables,
   stable file-group ids, file-specific encodings, declared types, labels,
   descriptions, units, ordered factor values/labels/descriptions, file
   availability, storage type, and declared bytes.
6. Select datasets, file groups, files, source variables, semantic terms, and
   primary-only mode using stable identifiers. Support documented role,
   modality, availability, term, and primary filters through glc_files and
   glc_variables. Participant choices are translated through glc_datasets to
   dataset ids. Resource selection applies to metadata/subset download, not
   silently to measurement interpretation.
7. Show declared transfer bytes and file counts before Apply, with hosted disk
   and memory warnings. Row and memory estimates are explicitly approximate;
   missing expected sizes are reported as unknown, never treated as zero.
8. Create bounded previews with glc_read(n_max = ..., progress = FALSE).
   Because n_max applies per file, derive and display a per-file allocation
   from a total preview budget. Let glcdp apply Schema 3's declared column
   types, boolean representations, ordered factor levels, datetime metadata,
   and per-file encodings. Default metadata/import discrepancies to
   problems = "error"; offer the warning policy only as an explained advanced
   inspection mode.
9. Preserve collection-level file-group, participant, device, timezone,
   modality, role, data-state, datetime, and file provenance before using
   glc_collect(standardize = "lightlogr") for canonical analysis data. Never
   reconstruct glcdp's LightLogR mapping in LightLogWeb. Respect its identity
   contract: Id is the GLC dataset id, file_group_id is the stable source group,
   and participant_Id is the linked participant id; expose all applicable
   identities and never silently substitute one for another.
10. Preview glc_collect compatibility before Apply. If selected file groups
    cannot be collected together, explain whether columns, declared types,
    factor values/labels/order, timezones, modalities, roles, data states, or
    relationship consistency differ and offer compatible partitions as
    separate new LightLogWeb datasets. Do not coerce across the glcdp
    compatibility boundary; the established append workflow remains explicit.
11. Require Apply for the committed full read. Preview rows are never promoted
    silently to a complete dataset. Date-window filtering occurs from the
    immutable imported base: the current glcdp API cannot push a date filter
    into a monolithic remote file, so the UI must state when a requested date
    range will not reduce network transfer. Add pushdown only when glcdp exposes
    it publicly.
12. Integrate committed GLC datasets with the existing dataset library and
    dashboard. Add GLC-specific source, validation, revision, and provenance
    views without changing their source-agnostic module contracts.
13. Offer download of the selected source package subset through glc_download,
    including its metadata closure, hashes, immutable revision, and
    glcdp-manifest.json. Clarify that this preserves the validated source
    metadata and does not incorporate a LightLogWeb metadata overlay.
14. Map no-passing-revision, non-passing revision, limited legacy schema,
    root/profile or package/dataset schema declaration mismatch, unsupported
    future schema, malformed registry, missing resource/path, unsupported
    format, parsing, timezone, incompatible collection, HTTP, Git-LFS,
    quota/auth, checksum, existing file, disk, cancellation, and stale-result
    conditions to recoverable app states. Schema 3.0.2 itself is not presented
    as experimental.
15. Use LightLogWeb-owned local Schema 3.0.2 fixtures and local registry JSON
    in routine tests, plus compatibility fixtures for 3.0.0 and 3.0.1 and
    limited legacy-message fixtures for 1.0.0 and 2.0.0. Mock transport where
    required. Keep a validated Schema 3.0.2 IZTECH public-registry journey
    opt-in and do not depend on glcdp's unexported test fixtures.

Acceptance gate: normal discovery, registry refresh failure, the validated
Schema 3.0.2 IZTECH deep link, malformed/repeated queries, no passing revision,
offline access, metadata preview, bounded typed preview, partial and full
import, incompatible selections, Git-LFS failure, subset download, and
dataset-library/dashboard integration are safe and reproducible; no data
transfer or dataset creation occurs without the relevant confirmation.

### Milestone 7 — Metadata essentials and settings separation

1. Separate factual metadata from analysis/view settings and transformations.
2. Maintain two factual-metadata layers for GLC sources: the immutable metadata
   loaded through glc_metadata and a clearly labelled LightLogWeb overlay.
   Never imply that overlay edits have changed or revalidated the source data
   package.
3. Build the hierarchical explorer from glc_resources and glc_metadata,
   covering Schema 3.0.2's required study, participants, datasets, devices, and
   device-datasheet resources; its optional participant-characteristics
   resource; and additional declared resources. Preserve resource names,
   nested paths, record identity, profile/schema version, and unknown fields.
4. Add field/value search through glc_search_metadata and crosslinks from
   dataset, participant, device, and variable views to their metadata context.
5. Delegate immutable source-metadata relationship traversal to
   extract_metadata. Use file_group_id as the default anchor across linked
   dataset, participant, study, and device records; use by = "Id" only for
   explicitly dataset-level extraction. Use add_metadata only on a derived
   analysis table, with overwrite disabled by default; never add metadata
   columns to the immutable canonical base or implement competing join rules.
6. Provide typed overlay forms for the analysis-essential normalized fields:
   dataset title, participant labels, device make/model, site/country, source
   timezone, coordinates, and variable labels/units. Align overlapping input
   types and controlled values with Schema 3.0.2, but store the result in the
   versioned LightLogWeb overlay. Show unmodeled fields read-only rather than
   silently rewriting them.
7. Store import timezone in provenance even without optional metadata. Move
   primary-variable choice, calibration, analysis timezone, missingness,
   scales, view windows, and grouping into settings or recipes.
8. Permit core analyses with no optional metadata, using raw names and “unit
   not specified.” Use factual labels/units in outputs when present, and make
   overlay precedence visible.
9. Add metadata-driven labels, units, participant/device links, and metadata
   crosslinks to the existing dataset library, dashboard, and table. Preserve
   the raw-name and “unit not specified” fallbacks, and invalidate only outputs
   that actually depend on changed metadata.
10. Validate overlay coordinates, IANA timezones, units, references, and
   participant IDs inline without changing the immutable source layer.
11. Export the overlay as LightLogWeb JSON or flattened CSV. Keep generation
    of an edited standards-compliant GLC package disabled with a capability
    message until glcdp provides a public write-and-validate API. Schema 3.0.2
    being final does not authorize LightLogWeb to recreate schema serialization
    or treat the metadata builder's soft validation as authoritative.

Acceptance gate: complete, partial, and absent optional metadata all permit
valid analysis; source metadata and overlays remain distinguishable; unknown
fields survive project save/reload; file-group and explicit dataset-level
metadata extraction preserve row count, order, grouping, and identifiers;
ambiguous relationships and column conflicts are recoverable; display-only
edits do not mutate data or invalidate unrelated results; and the earlier
source-agnostic library and dashboard remain valid when metadata are absent.

### Milestone 8 — Recipe-based preprocessing

1. Support ordered steps for participant selection, date/datetime ranges,
   recurring cross-midnight clock windows, typed conditions, numeric range
   filtering or invalidation, visible calibration, gap expansion, DST
   diagnostics/correction, minimum coverage/duration, and datetime aggregation.
2. Never evaluate arbitrary user R.
3. Never interpolate silently; any future interpolation must be advanced and
   retain an imputation mask.
4. Recompute every draft preview from immutable raw data.
5. Compare rows, participants, span, epoch, missingness, gaps, coverage,
   removed/invalidated proportion, memory/runtime estimates, and bounded
   before/after plots.
6. Require Apply, with extra confirmation for expensive, lossy,
   timezone-reinterpreting, or high-removal recipes.
7. Provide reset, undo, and step enable/disable.
8. Preserve the exact recipe for project reload and script generation.

Acceptance gate: switching from one-hour to fifteen-minute aggregation exactly
matches a fresh fifteen-minute calculation from raw data.

### Milestone 9 — Explicit grouping system

1. Keep separate metric and visualization grouping specifications, linked by
   default through a visible control.
2. Support participant, date, weekday/weekend, photoperiod, photoperiod
   counter, cross-midnight clock windows, numeric bins, categorical conditions,
   and attached states.
3. Generate photoperiod after aggregation and require timezone plus
   coordinates. Default solar depression is 6 degrees and configurable.
4. Preview group count, rows, observed duration, range/median size, coverage,
   regularity, and example labels using durations and related checks.
5. Warn below six hours without blocking solely on that general threshold.
6. Offer an explicit ungrouped setting per metric instance.
7. Derive grouping columns from prepared data without mutating raw data.

Acceptance gate: previews remain correct across midnight, DST, missing solar
metadata, mixed source timezones, empty groups, and single-observation groups.

### Milestone 10 — Visualization workbench

1. Use a registry for 24-hour/day plots, multi-day timelines, double plots,
   heatmaps, overview/availability, gap/irregularity plots, distributions,
   participant comparisons, and photoperiod/state overlays.
2. Share variable, participant, window, grouping, faceting, dimensions, labels,
   palette, scale, limits, and timezone controls.
3. Default light displays to a documented symlog-friendly scale while keeping
   linear and zero-aware log alternatives.
4. Prefer reliable static ggplot/LightLogR output. Allow Plotly only below
   tested row/trace limits and explain fallbacks.
5. Store reproducible plot specifications.
6. Make metadata labels optional and transparent.

Acceptance gate: every plot has a useful empty state, size guard, reproducible
specification, and legible small/large defaults.

### Milestone 11 — Summary interface

1. Wrap summary_table through a pure adapter accepting prepared data, primary
   variable, grouping, missing-data policy, labels, and units.
2. Pair the table with a context-matched overview or distribution plot.
3. Show preparation revision, grouping, exclusions, and warnings.
4. Persist table and plot specifications for projects, reports, and exports.
5. Treat per-group failures as warnings without failing the entire summary.

Acceptance gate: deterministic LightLogWeb results match direct LightLogR
calls across representative grouping and timezone cases.

### Milestone 12 — Single-measurement-variable metric engine

1. Use a curated, versioned registry rather than runtime form reflection.
2. Initial registry:
   - barroso_lighting_metrics
   - bright_dark_period
   - centroidLE
   - disparity_index
   - dose
   - duration_above_threshold
   - exponential_moving_average
   - frequency_crossing_threshold
   - interdaily_stability
   - intradaily_variability
   - midpointCE
   - period_above_threshold
   - pulses_above_threshold
   - threshold_for_duration
   - timing_above_threshold
3. Record parameter schemas, units, output type, grouping and minimum-duration
   requirements, regularity/gap requirements, eligible variable types, and
   documentation links.
4. Disable ineligible metrics with a specific explanation.
5. Permit repeated independently labelled instances with separate parameters
   and grouping.
6. Provide a version-matched summary_metrics typical-metrics preset.
7. Require Calculate; run batches in the background; retain successes if
   another metric fails.
8. Present tidy-long and readable-wide results with parameters, groups, units,
   warnings, and package versions.
9. Store and present warnings and messages for individual metrics alongside those metrics outputs (e.g., when hours are missing for interdaily stability)

Acceptance gate: every registry result matches a direct LightLogR call, and
repeated configurations survive display, export, project reload, and generated
code as distinct instances.

### Milestone 13 — Reproducible projects and exports

1. Implement .llwproj save/reload first because hosted sessions are ephemeral.
2. Generate readable analysis.R scripts from source manifests and recipes using
   public LightLogR calls. For GLC sources, regenerate acquisition through
   public glcdp calls at the recorded exact commit and recorded selection, then
   assert the recorded package/glcdp compatibility before analysis.
3. Keep shinymeta out of the core dependency graph. Re-evaluate it only as an
   optional experiment; the explicit recipe/specification model remains the
   source of truth.
4. Export metrics as CSV/XLSX; plots as PNG/JPEG/PDF/SVG where supported;
   metadata as nested LightLogWeb JSON and flattened CSV; prepared data as CSV
   and RDS; and projects with optional original files.
5. For GLC sources, let users include the serializable source specification,
   original metadata, LightLogWeb overlay, and optional glc_download subset
   with glcdp-manifest.json in the project. Keep the immutable source package
   and overlay as separate artifacts.
6. Provide one parameterized Quarto report with selectable provenance,
   metadata, quality, preparation, grouping, dashboard, summary, metrics,
   plots, and session sections.
7. Support HTML, PDF, and Word through runtime capability checks, isolated
   temporary rendering, and recoverable errors.
8. Default to HTML; explain missing PDF tooling precisely.
9. Run generated scripts in clean R processes and compare data, metrics, and
   plot data with the interactive session.
10. Offer validated-source GLC subset export through glc_download. Do not label
    a metadata-overlay export as a GLC package or fold it into the downloaded
    source subset until glcdp exposes and validates a public writer workflow.

Acceptance gate: project reload is equivalent; generated scripts reproduce
core outputs outside Shiny; and report failure leaves the analysis intact.

### Milestone 14 — Cohesive application experience

1. Apply the approved design system consistently.
2. Build a landing page with raw import, small test data, large test data,
   project reload, GLC registry discovery, and validated repo deep links.
3. Guide Import → Check → Prepare → Group → Analyze → Export.
4. Show completion/warning state and corrective links per stage.
5. Cross-link app controls, LightLogWeb docs, LightLogR references, and
   analysis-principles explanations.
6. Add About, Funding, Citation, License, Privacy, and Data Handling pages with
   verified, owner-approved wording.
7. Preserve dataset/workflow context during navigation.
8. Complete responsive behavior, focus management, screen-reader labels,
   reduced motion, keyboard use, and high-contrast states.
9. Run task-based usability passes for a newcomer and an experienced LightLogR
   user.

Acceptance gate: both profiles complete the principal workflow without hidden
steps, and all empty/loading/error states are intentional.

### Milestone 15 — Documentation, hardening, and v1 release

1. Build pkgdown reference, changelog, installation, hosted-app, and contextual
   app-help links.
2. Add vignettes for getting started, principles, imports/quality, datasets and
   timezone-aware append, GLC registry/deep-link/selective import, metadata and
   overlays, recipes, grouping, visualization, summaries, metrics,
   reproducibility/projects/reports, local versus hosted deployment, and
   troubleshooting/privacy/resources.
3. Add CI for package checks, unit/module tests, selected shinytest2 journeys,
   pkgdown, and link checking.
4. Verify dependency and asset licenses, including LightLogR-derived visuals
   and melidosData.
5. Write hosted/local runbooks covering upload limits, workers, Quarto,
   temporary storage, cleanup, and capability differences.
6. Run hosted and local deployment smoke tests without publishing or changing
   an external service unless separately authorized.

Acceptance gate: checks have no errors or warnings; pkgdown builds; critical
journeys pass; accessibility and browser targets pass; and authorized
deployment smoke tests succeed.

### Milestone 16 — Post-v1 time-aligned streams and multi-input metrics

This work begins only after v1 acceptance.

1. Add exact/nearest/previous/next time alignment, participant mapping,
   tolerance, join type, sampling harmonization, conflict suffixes, and
   variable mapping.
2. Preview matched, unmatched, duplicated, and many-to-many observations.
3. Preserve stream-specific devices, units, and provenance.
4. Add state/interval attachment with relevant LightLogR helpers.
5. Extend the metric registry to nvRC outputs, nvRD and cumulative response,
   spectral integration, and required spectral reconstruction.
6. Enable advanced workflows only when all inputs, units, time structure, and
   metadata are present.

Acceptance gate: advanced outputs match direct LightLogR workflows, and every
incompatibility names the missing stream, variable, unit, or temporal
requirement.

## 5. Cross-cutting verification

- Unit-test recipes, model validation, metadata/settings separation, timezone
  handling, merging, grouping, eligibility, serialization, code generation,
  and error mapping.
- Use testServer for every module contract and state transition.
- Keep a build-ignored standalone showcase for every feature module.
- Use shinytest2 for small-data end-to-end work, recoverable failed import,
  switching, preview/apply/reset, repeated metrics, project reload,
  report/export, and GLC registry/deep-link/preview/apply flows.
- Test the glcdp adapter with LightLogWeb-owned local packages and registry
  fixtures. Mock remote transport in routine CI; run live registry/Git-LFS
  smoke tests only when explicitly enabled.
- Generate large fixtures in temporary storage rather than committing them.
- On the initial 8 GB/2 CPU hosted target, a representative near-200 MB import
  must remain below 75% peak memory; optimize copying/caching before release if
  it does not.
- Keep navigation and unrelated controls responsive during long tasks.
- Enforce explicit row/trace budgets and label samples or aggregations.
- Test current Chrome, Firefox, Edge, and Safari at desktop/tablet widths and
  essential states at mobile width.
- Meet WCAG 2.2 AA for contrast, labels, focus, keyboard navigation, and
  non-color status cues.
- Review user-facing language for clarity. A person new to LightLogR should be
  able to understand the requested action and its consequence; terminology
  that is necessary for scientific precision should be retained and explained.
- Test malformed files, unsupported devices, invalid and mixed timezones, DST,
  duplicates, empty filters, absent metadata, small groups, unavailable
  optional tools, malformed/repeated repo queries, registry revisions with no
  passing validation, current Schema 3.0.2 packages, stable 3.0.0/3.0.1
  predecessors, limited legacy 1.0.0/2.0.0 paths, root/profile and
  package/dataset schema declaration mismatches, unsupported future schemas,
  offline and Git-LFS failures, worker failure, disconnect, corrupt archives,
  disk limits, and hostile filenames/metadata text.
- Verify cleanup, cross-session isolation, and exclusion of project-plan.md
  from every package archive.

## 6. Version-one defaults

- mockup is ignored.
- Hosted uploads are private to the session and total at most 200 MB per
  import action.
- Hosted-safe defaults target Posit Connect Cloud provisionally; benchmark
  results may revise worker/cache values, not silently reduce the upload
  contract.
- Current CRAN packages resolved at Milestone 0 define the initial
  compatibility baseline; the lockfile records that result. Until glcdp is on
  CRAN, its one explicit exception is an immutable reviewed GitHub commit.
- Single-measurement-variable metrics and safe row-wise append are v1.
- Time-aligned streams and multi-input metrics are post-v1.
- GLC registry discovery, validated-revision preview, selective import, source
  subset download, and metadata exploration are v1.
- Schema 3.0.2 is the primary GLC contract for v1. Stable 3.0.0 and 3.0.1
  packages remain reproducible compatibility paths; 1.0.0 and 2.0.0 receive
  only explicitly tested legacy support.
- Public registered GLC packages are supported in hosted v1; private-token
  entry is not.
- Metadata overlays enhance analysis without altering validated GLC source
  metadata. Edited GLC package generation waits for a public glcdp writer and
  validator API.
- Reports target HTML, PDF, and Word subject to runtime capability checks.
- Core analysis works without optional metadata.
