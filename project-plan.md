# LightLogWeb full implementation plan

Last updated: 2026-11-17

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

- The current integration target is glcdp 1.1.0 at exact Git commit
  `58a16400dc6d30ec2686ebf1898f938d3721eb2c`, as pinned in DESCRIPTION,
  renv.lock and manifest.json after the 2026-09-27 registry upgrade. The
  default registry is https://registry.globallightcommons.org/registry.json.
  Earlier milestone records retain their historical pins and evidence.
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
- The current reproducible development runtime is R 4.6.1. DESCRIPTION must
  declare at least the minimum R version required by the resolved LightLogR
  release; it need not require R 4.6.1 unless package code depends on it.
- The 2026-08-01 R 4.6.1 dependency refresh is recorded in
  dev/dependency-refresh-r-4.6.1.md.
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
- Owner direction, 2026-11-11: the independent review chat must review every
  revision before it is handed to the owner, including layout and rapid
  refinement iterations. Programmer checks alone do not satisfy this gate.
  Resolve blocking review findings and obtain acceptance of the exact candidate
  before owner handoff; keep the review proportional to the changed surface.

### 2.7 Vision-first delivery from Milestone 9

Owner direction, 2026-11-11: every future milestone starts with **Slice 1:
Vision and app-context prototype**. This rule begins with Milestone 9. It does
not restart, renumber, redesign or change the approval gates of the current
Milestone 8, which the owner is directing separately. The owner has invited the
Milestone 8 programmer to decide how much of the vision-first, design-led and
real-world-data guidance is useful for the current task/slice, without a
mandatory restart. The capability lists in Section 4 remain the scope, not a
mandatory implementation order.

Start by showing the complete intended user experience, not a collection of
isolated controls or a technical foundation. The vision is a useful direction,
not a fixed final layout. A short read-only capability and feasibility audit
may inform it, but must not turn into substantial implementation before the
owner can see and assess the direction.

**The vision slice is design-led.** Its primary deliverable is a clear,
usable concept for the whole workflow, not technical completeness. Established
UI, interaction-design and UX principles must guide the structure before
implementation convenience determines it. Scientific meaning and safety remain
constraints throughout. These principles also apply to later slices, but the
vision slice is where the main design decisions receive the most attention:

- Organize the interface around user goals and familiar concepts, not internal
  modules or a form exposing every function argument.
- Establish clear information hierarchy, a visible next action and enough
  nearby context to make each choice understandable. Prefer recognition over
  remembering settings from another screen.
- Keep the common path short. Reveal advanced choices when relevant without
  hiding scientifically consequential assumptions, scope or defaults.
- Make current state, feedback, error prevention, recovery and reversibility
  part of the proposed flow, not details added at the end.
- Design keyboard access, focus, readable contrast and responsive behaviour
  from the beginning. Use consistent language, spacing and controls from the
  existing app design system rather than introducing a separate visual style.
- Show useful results and the effect of choices close to the controls when
  that helps understanding. Keep dense detail available without overwhelming
  the main task.

Resolve workflow and information structure before fine visual polish. Where a
major interaction choice is unclear, compare a small number of lightweight
alternatives and explain their tradeoffs. The vision brief records the main
design decisions and why they suit LightLogWeb's users. A first-time-user
walkthrough must test whether the intended task, current state, next action,
result and recovery route are clear; attractive screenshots alone are not
sufficient. Do not add design machinery or extra controls merely to satisfy a
generic checklist.

The vision slice contains three connected deliverables:

1. **A short vision brief.** Explain:
   - the user's goal, what a successful result looks like, and what is outside
     the milestone;
   - the complete journey from entry to useful output, including one ordinary
     case and one more demanding case;
   - where the feature belongs in the app, what it takes from earlier work,
     what it passes to later work, and how the user returns without losing
     context;
   - how complexity is managed: a simple default path, advanced choices shown
     when needed, clear prerequisites, and helpful incomplete/error states;
   - the moving parts in plain language: inputs, settings, draft and committed
     state, operations, outputs and dependencies, including which changes
     invalidate results and which do not;
   - the intended straightforward R/tidyverse/public LightLogR workflow, and
     which interactions should be immediate versus explicit background work.
     Keep app safety and task handling separate from the scientific steps.
2. **A lightweight app-context prototype.** Prefer a runnable, clickable
   Shiny demonstration using the existing design system and real-world data or
   documented participant subsets under Section 2.8. Show it in the current app
   shell or a clearly identified development shell that preserves the relevant
   navigation and neighbouring sections. The owner must be able to understand
   the intended end-to-end workflow before detailed implementation. Use
   recorded results from those data or clearly labelled unimplemented UI
   placeholders where needed; distinguish working, simulated and undecided
   interactions. Do not invent measurements to make the main journey look
   complete or build the whole backend just to make the prototype appear
   finished. No new live-data access or production integration is implied.
3. **An implementation-slice map.** Derive the remaining slices from that
   overall journey. For each slice, name the useful outcome, dependencies,
   important risks and acceptance checks. Show how the slices build toward
   the proposed whole rather than presenting unrelated technical tasks.

The independent reviewer first checks whether the vision is understandable,
fits the app, makes the main components and boundaries clear, and handles
complexity sensibly, using the design priorities above in concrete user
journeys. The owner then approves or revises that direction before
the first implementation slice begins. Vision acceptance is not proof of
scientific correctness, performance or production readiness; simulated output
cannot count as functional evidence. Later slices still require the normal
R tests, performance checks, frozen-build review and owner acceptance.

Keep the vision visible as development progresses. Every later showcase must
locate the completed work within the full intended workflow and state what is
working, still provisional or not yet implemented. Record meaningful changes
to the vision and their reasons. Seek owner agreement when they change the
user workflow, scientific meaning or milestone scope; do not turn the initial
prototype into an inflexible layout requirement. Full production integration
still occurs at its explicitly agreed gate.

### 2.8 Real-world data as the milestone default

Owner direction, 2026-11-11: use the available real-world data for milestone
design, prototypes, ordinary development journeys, demonstrations and
acceptance. Synthetic data are an exception for proving a specific point or
testing an edge case, not the default material for building the feature.

- Reuse existing authorized local datasets and retained public-data snapshots.
  Offline real-world fixtures are suitable; fresh network access is not a
  prerequisite. Keep their provenance and known limitations visible.
- When computation would be too expensive, use a reproducible subset of a few
  participants. Retain enough of their recordings to exercise the actual
  workflow, including relevant days, windows and source variation. A few
  initial rows per file cannot establish full-day or long-duration behaviour.
- Preserve the real values and relevant characteristics, including missing
  values, gaps and irregularity. Do not silently clean away the features that
  make the example realistic. Document the source/version, participant and
  file selection, retained variables and any time restriction. Perform data
  subsetting and verification in R.
- Every synthetic fixture must have a named purpose, such as an exact boundary,
  a rare DST case, controlled failure or a defined scaling experiment. Label
  it as synthetic and state the point it proves. It supplements, rather than
  replaces, the ordinary real-data journey. Focused unit tests may use these
  small constructed cases.
- The independent reviewer exercises the primary journey with real-world data
  and suitable variation. Synthetic edge-case passes alone do not establish
  milestone usability or readiness. Label subsets as subsets, and do not
  present their runtime as evidence for a complete dataset or larger workload.
- Data choice remains subject to existing access, privacy, licence and resource
  limits. Prefer the available retained data; obtain separate authorization
  when an additional live transfer or other new data access is needed.

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
helpers enter the production adapter with Milestone 7. glc_download enters the
user-facing production workflow only through Milestone 13's shared
reproducibility, export, project-bundle, report, and LightLogR-handoff
workflow. If exact provenance or future write support cannot be obtained
through documented outputs, adding the required public accessor to glcdp is a
prerequisite; LightLogWeb must not reach into package internals. glc_explore is
a package-owned reference application, not an embedded LightLogWeb dependency.

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

### Milestone 5 — Scalable dashboard and source/pre-processed-data table

Status: complete; accepted by the owner on 2026-07-31

Acceptance result: the owner approved the scalable real-data dashboard,
source/pre-processed tables, focus-metric quality and missingness semantics,
scientific axis controls, and the final responsive two-axis Explorer
navigation. The focused dashboard data, navigation, and refinement suites
passed at closeout, the accepted local showcase was stopped, and the complete
Milestone 5 implementation was committed and pushed from the development
branch.

Sequencing boundary: implement this milestone for source imports with empty
recipe and grouping states and without requiring GLC or optional metadata.
GLC-specific provenance panels and metadata-driven labels, units, and
crosslinks are added in Milestones 6 and 7 without changing the generic
dashboard and table contracts.

1. Show available provenance, participant/date span, sampling, variable
   inventory, focus-metric missingness and percentage under three explicit
   denominators (recorded timestamps, the regular first-to-last recording
   span, or full participant-local days), explicit missing focus epochs,
   implicit gaps, timing jitter, DST, focus-metric daily coverage, active
   recipe, and grouping. Empty or unavailable states must be explicit. Keep
   the focus-metric selector in the compact summary-box row and explain its
   analytical scope through the box icon's tooltip.
2. Let the user choose the numeric focus metric and whether Explore uses source
   or pre-processed data. Offer symlog, linear, and logarithmic display scales;
   log views must disclose omitted exact zeros separately from negative values.
   Let users set optional y-axis minimum and maximum values without deleting
   out-of-range observations. Keep the symlog linear-range control with the
   optional minimum and maximum controls in the Y-range popover. Symlog views
   label zero, the signed threshold, and outer powers of ten and keep the
   central half-range and each outer ×10 step equally spaced on the axis. Add
   unlabeled raw-value tenth axis ticks between adjacent major breaks so
   spacing is regular in the linear center and visibly compressed across the
   logarithmic outer regions.
3. Keep absolute calendar scope distinct from participant measurement duration.
   Let users view each participant on their own recorded dates, align everyone
   by days from first measurement, use one shared date window, or navigate
   calendar weeks by weekday. Participant changes refit the relevant scope to
   those recordings. Label every absolute-time axis `Local Date/Time` with its
   display timezone; reserve `Days from first measurement` for the elapsed
   view.
4. Choose detailed timelines through seven available focus-metric days per
   participant and focus-coverage overviews for longer actual participant
   spans. A long global calendar interval containing only short, staggered
   recordings must remain eligible for the detailed view. Users can always
   override Automatic with Detailed timeline or Coverage overview.
5. Provide one lean, inspectable recommendation contract for the initial view:
   own recorded dates, the full selected measurement duration, seven-day time
   pages, four participants per page, Automatic detail/coverage from actual
   focus-metric span, and the dataset's recorded scale preference. Keep one
   slim Recommended view action visible, disable it while the recommendation
   is active, preserve user adjustments, and show the recommendation and
   rationale in a corner notification when it is applied. Do not infer
   elapsed or weekday intent from the data.
6. Use independent, accessible two-axis navigation: participant pages move
   through two full-height, half-plot up/down controls beside the plot; time
   pages move left/right through compact arrow-and-calendar controls below it.
   Let users set the maximum participants per page and select either axis
   directly. Keep the participant-page selector inside the resizable sidebar
   and the time selector in the visualization footer. Add one clickable
   participant-by-time navigator grid: filled
   rectangles contain focus measurements, hollow rectangles are valid empty
   pages, and the active rectangle uses a signal color. Disable boundary
   buttons, split oversized scopes into bounded pages, replace the participant
   selector with a compact all-participants scope while Show all is active,
   and render an explicit empty state when a page has no focus measurements.
7. Bound preview plot data and label reductions. Build detailed participant
   facets with LightLogR `gg_days()` so recordings from different dates can be
   viewed simultaneously with participant-specific, complete day-aligned date
   windows or aligned elapsed-time axes. Keep participant labels bold, retain
   empty facet slots when the final participant page is short, and let the
   plot grow with the number of visible participant slots while preserving a
   useful minimum height. Put concise scope, reduction, scale, and
   manual-range context in the plot title/subtitle and targeted tooltips or
   popovers; do not add a persistent scope disclosure below the plot. Bounded
   display sampling is not itself a quality warning.
8. Use compact, icon-bearing summary and quality boxes with definitions in
   tooltips. A positive quality outcome is green; an issue or unavailable
   result is orange.
9. Use all available dashboard width; keep controls and tables responsive.
   Explore is one flat workbench inside the tab boundary: a grouped
   what/who/when/how sidebar, a descriptive visualization canvas, full-height
   participant turns beside the plot, a sidebar-owned participant-page
   selector, and one compact time/page-map footer below the canvas.
   Do not add a second plot card or persistent explanatory prose around the
   visualization.
10. Add a server-side DT table for the active data with search, sort,
   pagination fixed at 10 rows, externally controlled column visibility, and
   type-aware formatting. Keep the column selector with the main-columns
   switch rather than inside the horizontally scrolling table. The variable
   inventory and participant-day coverage use fixed 10-row pages in cards tall
   enough to show a complete page without vertical scrolling. Both retain
   global search and sorting but omit per-column filters and page-size
   selectors.
11. Keep source and pre-processed tables visually distinct. Let users toggle
   between main columns (grouping/Id, Datetime, focus metric) and the full
   horizontally scrollable schema. Show the recipe revision and unchanged
   empty-recipe state explicitly.
12. Keep source provenance and folded source import checks with the Source data
    tab, rendering pass/warning/information/error states with colored icons and
    text. Keep recipe/grouping state and a distinct post-recipe integrity
    summary with the Pre-processed data tab; it checks active rows, identity,
    timestamps, focus values, and recipe traceability without pretending to be
    a second source-import validation. For appended datasets, recompute source
    checks on the combined source table while retaining source-specific
    manifests and diagnostics in append provenance.

Acceptance gate: the deterministic scale fixture and every reviewed real
showcase source remain interpretable without sending full tables to the
browser. The visible showcase contains installed LightLogR testdata, pinned
IZTECH data, all 20 provided ActLumus files, all eight Speccy files grouped into
ID01/ID02/ID04 with `^(ID[0-9]{2})`, and `02_VEET_L` imported separately as
ambient-light (ALS) and spectral (PHO) sensor data. Large records load lazily
and are cached only for the current session. VEET phase shifts remain explicit
timing diagnostics but do not inflate missing epochs through one globally
anchored timestamp grid.

Milestone evidence:

- dev/milestone-5-scalable-dashboard.md
- dev/run-milestone-5-scale-acceptance.R

### Milestone 6: GLC discovery, selective import, and library integration

1. Upgrade the historical optional glcdp 0.90.0 dependency and promote it to
   Imports. The implemented dependency is now glcdp 1.0.0.9000 at exact commit
   `e49fcf153a114916ce2680f8b40eb475e694e10d`. Keep adapter parity tests as the
   feature gate. Make any later CRAN transition a tested dependency update
   rather than an automatic source switch.
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
13. Defer validated-source subset export to Milestone 13. Do not add a
    standalone GLC-only download surface in Milestone 6. Milestone 13 owns the
    public glc_download workflow, including metadata closure, hashes, immutable
    revision, glcdp-manifest.json, and separation from any LightLogWeb metadata
    overlay.
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
import, incompatible selections, Git-LFS failure, and dataset-library/dashboard
integration are safe and reproducible; no data transfer or dataset creation
occurs without the relevant confirmation. Validated-source subset export is a
Milestone 13 acceptance item.

### Milestone 7 — Metadata essentials and settings separation

Status: complete; accepted by the owner on 2026-09-27, including final
integration, Dashboard quick metadata editing, the R58/R60/R61 corrections and
the visible "Dataset details" wording. The owner authorized milestone closeout,
commit and push. The dated checkpoints below preserve the review history;
their former pending gates are superseded by this acceptance. Final closeout
evidence is recorded in `dev/milestone-7-overlay-authoring.md`.

Remaining limitations are explicit: R59 is a deferred, nonblocking mobile
table-layout improvement; the 50 ms design target is not uniformly met despite
passing hard gates; wide-mutation allocation has narrow headroom. Standalone
overlay export was removed from scope, not passed. Edited GLC generation,
Milestone 8 preprocessing and Milestone 13 project/reproducibility outputs are
not included in this completion.

Milestone 7 is divided into three independently reviewable vertical slices:

1. A read-only explorer foundation and versioned source-versus-overlay value
   contract, including exact GLC snapshot retention, append namespaces,
   complete/partial/absent states, offline public-derived fixtures, and
   in-memory performance gates.
2. Relationship-aware metadata context and source-agnostic integration using
   exported extract_metadata/add_metadata behavior, including crosslinks and
   metadata-driven labels/units in the library, dashboard, and tables. This
   slice also completes the factual-metadata versus analysis-settings moves.
3. Typed overlay authoring, precedence and invalidation behavior, inline
   validation, and protected undo in the LightLogWeb overlay summary.

The owner approved Slice 3 on 2026-09-21, adding batch editing as the default.
Its scope is the current library dataset, including appended sources, never
other library entries. All matching items and Single item only remain explicit
choices. Variable batches match exact source names across retained file-group
occurrences; participant batches count each exact source-qualified participant
once even when linked to several files or devices. Every proposed target and
old/new value is inspectable before one atomic Apply. Search or pagination in
that preview cannot restrict the transaction. No-op, stale, invalid and
ambiguous assignments cannot silently change valid committed state.

The overlay-specific version-2 contract adds source-qualified identities and
participant sex without changing immutable source snapshots or the existing
bundle contract. Version-1 assignments upgrade only when their target is
unambiguous; unresolved and unknown content stays preserved. Participant sex
is free text under the retained schema, separate from gender. Unit and source
timezone edits are annotations, never measurement conversion or timestamp
reinterpretation. Drafts stay bound to their original library record and
revision. Apply/remove/undo reuse the existing session history and a narrow
metadata-only record update, with no post-initialization measurement decoding.

On 2026-09-23 the owner removed standalone overlay JSON/CSV export from the
Milestone 7 scope and code base. Remove its controls, download handlers,
serialization helpers and export-only tests/performance probes, not just its
visible buttons. The separate Edit LightLogWeb metadata card is removed;
Undo latest metadata edit and its result/error feedback belong inside the
existing LightLogWeb overlay summary. Source-versus-overlay separation and
Milestone 6 record/session recovery remain intact. This does not remove project
serialization, fixture manifests, or Milestone 13 outputs. No new dependency,
installation or repin is authorized.

The owner's subsequent toolbar refinement places Edit selected field and Add
missing metadata beside One level up inside the Metadata explorer. Remove the
separate selection-status paragraph and action row. Edit stays selection-bound;
Add missing metadata remains library-dataset-wide, including appended sources.
The toolbar stays available for metadata-absent datasets and mounted during
updates so modal dismissal can return focus. These presentation changes go
directly to the owner, with no separate external review.

For Add missing metadata and the standalone editor, the additional source-name
selector appears only for Variable label or Variable unit and is labelled
Source variable. Other fields proceed directly to Edit scope. Apply to still
selects all matching items or one exact target; cohort identities and validation
are unchanged. Contextual editing continues to preselect the explorer field.

On 2026-09-23 the owner accepted the accumulated Slice 3 production-module
showcase and authorized wrap-up and commit. This includes the consolidation,
export removal, toolbar and source-variable selector refinements, with no new
external review as requested. The accepted scope is the module and its runnable
showcase, not full-app integration or completion of the whole milestone.
Final verification is recorded in `dev/milestone-7-overlay-authoring.md`:
1,418 focused and 4,863 full-suite assertions pass; package check is Status OK.
Report 2.10.0 at `2026-09-23T12:13:57Z` passes all hard timing, memory and
identity gates. Ordinary maximum p95 is 76.6/100 ms, approved stress removal
68/150 ms, and initial/bulk index work 190.95/500 ms. The 50 ms design target is
not uniformly met; mutation allocation retains limited headroom below 64 MiB.
That verification did not authorize full-app wiring, later slices, push or deployment.

The owner subsequently approved `APPROVE M7 FINAL INTEGRATION CONTRACT` on
2026-09-23. This final slice wires the accepted editor into the actual app,
resolves effective labels/units per exact occurrence, and refreshes presentation
without rebuilding measurement-derived dashboard, library or append state.
The approved boundary and baseline audit are recorded in
`dev/milestone-7-overlay-authoring.md`. Frozen-build independent review, owner
final acceptance and a separate commit gate remain open. Milestone 7 is not yet
declared complete. No new export, dependency, JavaScript or live read is authorized.

The owner also approved the narrow final-integration overlay performance
correction on 2026-09-23. `R/metadata-overlay.R` is added to that integration
allowlist; all earlier boundaries remain in force. Conservative bundle-size
accounting retains an exact fallback at the 32 MiB limit, and shared session
accounting avoids repeated sizing of unchanged history in dashboard refreshes.
The 100/150/500 ms timing gates and 64 MiB transaction-allocation gate are not
relaxed. Local final verification now passes: 5,240 full-suite assertions with
zero failures/errors, package check Status OK, and report 2.11.0 at
`2026-09-23T16:38:08Z` passes every hard timing/memory/invariant gate. Ordinary
maximum p95 is 93.3/100 ms; approved stress removal is 80.65/150 ms and maximum
initial/bulk work is 236.55/500 ms. The 50 ms design target is not uniformly met.
The widest integrated batch allocates 65,884,576 of 67,108,864 allowed bytes.
All seven integrated cases have zero post-startup measurement decodes and retain
source, numerical data and valid context. Main-app browser checks cover the
three fixtures, appended/absent cases, six viewport widths and stale append
preview recovery. Existing datepicker locale warnings remain documented.
The frozen local packet is `/private/tmp/llw-m7-final-integration-review-gyOZEO`.
The owner subsequently resumed normal review transmission. Independent review
found M7-UX-R58: metadata-only Apply/Undo reset DT table search and paging. The
owner approved the table-state correction on 2026-09-24. Native, validated,
session-only DT view restoration is now implemented for both table stages,
scoped to numerical data/schema identity and raw column names. It neither adds
browser persistence nor new JavaScript. Focused tests pass 896 assertions and
the full suite passes 5,320 with no failures/errors. Package check is Status OK
(5,239 installed-test passes). Report 2.12.1 passes unchanged hard gates: ordinary
maximum 88/100 ms, stress removal 79.65/150 ms, bulk maximum 215.6/500 ms, and
65,884,576/67,108,864 bytes maximum transaction allocation. The 50 ms design
target is not uniformly met. The new frozen handoff is
`/private/tmp/llw-m7-r58-review-Zas6Ij`. Details and the exact correction
boundary are in `dev/milestone-7-overlay-authoring.md`. Independent acceptance,
owner final check and a separate commit gate remain open. This is not milestone
completion.

On 2026-09-27 the owner accepted the independently reviewed R58 build and
approved `APPROVE M7 DASHBOARD QUICK METADATA CONTRACT`. This follow-up adds a
Dashboard-only two-field Label/Unit editor with Adjust/Create, exact mapped
current-library-dataset scope, atomic single-revision Save/Undo and unchanged
source/measurement/settings/table state. It also corrects the small example's
false unit conflict by using factual metadata before the same-source manifest
fallback. It does not reconcile different retained sources or convert units.
The narrow file boundary and unchanged performance budgets are recorded in
`dev/milestone-7-overlay-authoring.md`. The candidate passes 1,950 focused and
5,409 full-suite assertions (zero failures/errors) and package check Status OK.
Benchmark 2.13.0 passes all hard timing/memory/integrity gates: ordinary
integrated p95 maximum 98/100 ms, wide removal 93/150 ms and explicit bulk
maximum 245.7/500 ms. Wide two-field Save allocates 66,283,528/67,108,864 bytes,
with limited remaining headroom; the 50 ms design target is not uniformly met.
Post-initialization payload decoding and package reopening remain zero. The
frozen candidate is `/private/tmp/llw-m7-quick-review-u9vfBx`. Independent
review, owner final check and a separate commit gate remain open. No owner
showcase is left running. R59 remains outside this boundary, and standalone
metadata export remains removed.

Slice 3 was accepted as a production-module showcase. Its ordinary 100 ms p95 hard gate and 64 MiB
commit/index-patch allocation limit remain unchanged. The owner subsequently
approved a separate 500 ms p95 limit for explicit large-inventory commits,
initial index attachment and bulk transactions, with immediate busy feedback,
duplicate blocking and atomic recovery. Warm single-item updates and all
ordinary navigation/preview retain the 100 ms limit. The existing local browser
helper may also return focus without scrolling when the overlay dialog closes.
That focus correction now passes real-browser Apply/Discard checks. On
2026-09-22 the owner accepted the standalone editor refinement and a narrowly
extended 150 ms p95 gate for warm single removal in the 5,000-variable-occurrence
inventory. This exception does not relax navigation, selection, preview, other
warm single edits, memory or zero-reopen/deserialization gates. The earlier
100 ms failure remains historical evidence. The full 2026-09-22 matrix passes
its explicitly recorded operation-specific timing and memory limits.

The same owner request adds contextual editing to the metadata explorer.
Edit selected field resolves the current source-qualified field and fixes it
in the modal; the first form choice is Edit scope, followed by Apply to,
current values and Change. The contextual form has no field or matching-field
selector. All matching items still means the current library dataset, including
appended sources. Single item only starts with the focused exact item and
offers the other exact targets in that cohort. The existing typed allowlist is
unchanged. IDs, relationships, calibration, ambiguous mappings and unmodeled
fields are read-only with an explanation. Core variable labels/units can alias
the corresponding inventory assertion only when exact source scope, file-group
identity, raw variable name and retained value agree. No source record is
overwritten or revalidated. Current effective values are shown separately.
Overlay-only Apply/Undo retains the search, selected field and hierarchy focus;
switching records or refreshing source context still clears stale selections.

This extension is first exercised in a combined explorer/editor showcase.
Independent review and owner acceptance precede full application wiring.
The exact file boundary,
implementation evidence and open gates are recorded in
`dev/milestone-7-overlay-authoring.md`.

On 2026-09-22 the owner approved the R48-R54 correction contract following
independent rejection of the prior contextual candidate. It adds consistent
placeholder-unit validation, layer-accurate guidance, a missing-annotation
route bounded by existing identities, stable keyboard modal return, effective
values beside immutable originals, and clean/dirty Escape dismissal. The
source-qualified presentation is separate from source index facts. The exact
typed authoring allowlist, budgets and file boundary remain unchanged. Saved
JSON/CSV browser-artifact verification (R52) is explicitly deferred to the
owner final check, not recorded as passed. A new frozen revision and independent
acceptance are required before owner final check; full-app integration and
commit remain separately gated.

The R48-R54 correction candidate passed 1,191 focused assertions, 4,636 full-suite
assertions and `R CMD check --no-manual` (Status OK). The final offline matrix
at `2026-09-22T10:34:53Z` passes unchanged hard timing and memory gates; effective
presentation reaches 74.65 ms p95 in the stress inventory, above the 50 ms design
target but below 100 ms. The exact revised packet is
`/private/tmp/llw-m7-r48-r54-review-njSxcT`. Independent acceptance is pending;
this verification does not approve full-app integration or commit.

The independent review of that candidate required revision: R48-R51 and R53
closed within the report's stated coverage, R52 remained owner-deferred (not
passed), and R54 still lost keyboard focus after Review. The owner approved
`APPROVE M7 SLICE 3 R54 REVIEW-FOCUS CORRECTION`. The narrow seven-file boundary
adds an after-flush focus request for successful, repeated, unchanged and invalid
reviews, with the existing browser helper recovering only lost focus after the
native busy button resets. Escape also finds the visible owned editor when
focus has fallen to the page. Dropdown-first Escape, Keep/Discard, independent
full-path ownership, explicit Apply and all existing budgets remain unchanged.
Typed metadata, fixture, transaction and export contracts are untouched. A new
frozen packet and independent acceptance are required before owner final check;
full-app integration, commit and push remain separately gated.

The R54 review-focus candidate passes 1,219 focused assertions, 4,664 full-suite
assertions and `R CMD check --no-manual` (Status OK). Targeted pointer and native
keyboard checks cover success, repeated/unchanged Review, validation feedback,
Keep/Discard, dropdown-first Escape, missing annotations and full-path ownership
at 1024, 390 and 320 px. The refreshed offline matrix at
`2026-09-22T11:32:39Z` passes unchanged hard timing and memory gates. Stress
effective presentation reaches 73 ms p95, above the 50 ms design target but below
100 ms; mutation allocation remains within 64 MiB with limited headroom.
The frozen packet is `/private/tmp/llw-m7-r54-review-17QJZ3`. Independent
acceptance and the explicit R52 owner saved-file check remain outstanding.

The independent R54 candidate review closed R54 but required revision for R55:
keyboard full-path Close returned focus to an off-screen opener after a scroll
jump. The owner approved `APPROVE M7 SLICE 3 R55 FULL-PATH CLOSE CORRECTION`.
The seven-file correction boundary is recorded in the overlay authoring note.
A native fade trial failed and was reverted. The existing helper now preserves
the invoking viewport on Close/Escape/backdrop dismissal, while path selection
retains intentional destination navigation. It yields to new user navigation,
keeps R54 and native controls unchanged, and adds no server work, dependency,
source or overlay edits. All budgets and subsequent acceptance/owner/integration/
commit gates remain in force; R52 remains owner-deferred, not passed.

The R55 candidate passes 1,239 focused assertions, 4,684 full-suite assertions
and `R CMD check --no-manual` (Status OK). Targeted native keyboard and pointer
checks cover dismissal, intentional ancestor navigation, new user gestures and
protected R54 focus/Escape behavior across 1440, 1200, 1024, 768, 390 and 320 px.
The fresh offline matrix at `2026-09-22T12:42:47Z` passes all unchanged hard
timing and memory gates. Ordinary p95 reaches 77.65 ms, above the 50 ms design
target but below 100 ms; the approved stress single removal is 65.65 ms against
150 ms. Mutation allocation remains below 64 MiB with limited headroom.
The exact frozen packet is `/private/tmp/llw-m7-r55-review-X2yYmu`. Independent
acceptance is pending; no full-app integration, commit or push is authorized.

The independent R55 review closes R55 and protects R54, but finds medium R56:
sustained filter alternation after the appended single-unit Apply/source-switch/
Undo/full-path Close sequence. The owner approved
`APPROVE M7 SLICE 3 R56 FILTER-STABILITY CORRECTION CONTRACT` on 2026-09-22.
The exact seven-file boundary is recorded in the overlay authoring note. Native
source/layer/section/sort controls now stay mounted; choices update only when
changed, without rebuilding or echoing the source/layer inputs. Native
conditional panels preserve relevance and a scoped responsive grid preserves
layout. Clear changes only the query; invalid sections normalize once and new
source identities reset once. Same-source Apply/Undo retains valid context.
No JavaScript, overlay/source schema, fixture, dependency or integration change.

R56 passes 1,294 focused and 4,739 full-suite assertions with no failures.
Package check is Status OK (4,658 installed assertions). Existing recovery
warnings and environment/optional-fixture skips are recorded, not hidden.
The original browser sequence settles without oscillation for 115.631 seconds;
a fresh repeat stays stable for 72.423 seconds. Query-only clear, empty-state
recovery, valid filters, R54/R55 keyboard paths and six responsive widths are
checked. An over-batched immediate-post-dismissal Clear that did not take effect
is recorded and excluded from successful route evidence. All timing/memory
budgets remain fixed; filter-choice preparation and an eight-choice burst are
added to the offline matrix. Independent review and owner final check still
precede full-app integration or commit. R52 saved-file verification remains
owner-deferred, not passed.

The refreshed R56 matrix at `2026-09-22T14:01:48Z` passes every hard timing and
memory gate. Filter-choice preparation reaches 9.65 ms p95 and the eight-choice
burst 30 ms on 50,000 entries, allocating at most 7,327,552 bytes against 10 MiB.
The broader ordinary maximum is 74.3 ms, so the 50 ms design target is still not
uniformly met; all unchanged hard gates pass. Approved stress single removal is
64/150 ms, initial/bulk work 238/500 ms and CSV 80.65/1000 ms. Mutation allocation
stays below 64 MiB with limited headroom. Package reopen/deserialization counts
are zero. Exact packet: `/private/tmp/llw-m7-r56-review-wIRlAo`. Independent
acceptance is pending and authorizes owner final check only.

The independent R56 report closes the sustained-refresh symptom but opens
medium R57: a valid rapid Source-to-Section selection could be visibly accepted
then overwritten by a delayed native Section update. The owner approved the
R57 dependent-selection contract, then the separate
`APPROVE M7 SLICE 3 R57 GUARDED SECTION UPDATE JAVASCRIPT` gate after a read-only
native-first A-to-B-to-A counterexample. The model is not a browser reproduction.
The exact eight-file correction boundary and protocol are recorded in
`dev/milestone-7-overlay-authoring.md`; a one-line asset-version test extension
outside that boundary was subsequently approved by the owner on 2026-09-23.
The ninth file changes only that assertion from 1.2.4 to 1.2.5.

Section remains a native mounted selector. A namespaced per-module generation
immediately makes it unavailable with truthful pending feedback on parent
changes, and only the current epoch/generation reply can populate it once.
Ready selections cannot be overwritten by old or duplicate responses. Dataset
identity resets once; Apply/Undo retains valid context, invalid sections fall
back once, and Clear remains query-only. A scoped retry/reconnect path and
native keyboard semantics preserve recovery. R validates requests against a
small cached choice inventory, without reopening packages or deserializing
records. Source namespaces, overlay semantics, R54/R55 handlers, fixtures,
dependencies and full-app integration remain unchanged.

R57 programmer verification is complete, not independent acceptance. Programmer
browser checks cover the original route, parent reversals, protected R56/R54/R55,
complete/partial/widest/absent/appended states and six responsive tiers. The
original Apply/Undo/filter burst stays stable for 74.596 seconds without input.
Deterministic tests additionally exercise delayed and duplicate replies,
multiple modules and reconnect; no real-browser reconnect claim is made. The
fresh benchmark includes guarded reply serialization and charges the small
choice inventory to the existing section-index memory budget. All established
budgets remain unchanged. The matrix at `2026-09-23T07:58:07Z`, report 2.9.0,
passes all hard timing/memory/identity gates: guarded replies 1 ms p95,
eight-request bursts 11-13 ms, and maximum burst allocation 3,543,352 bytes.
The broader ordinary maximum is 83.3 ms, above the 50 ms design target but below
100 ms. Stress removal is 70.65/150 ms, initial/bulk 208.65/500 ms and CSV
81/1000 ms. The existing limited mutation-memory headroom remains documented.
After the owner-approved one-line version assertion update, final focused
verification has 1,391 passes, the full suite 4,836, and package check is Status
OK with 4,755 installed-test passes. Existing recovery warnings, unavailable
mirai/optional-fixture skips and unavailable repository-index lookups are
recorded. A 31-file comparison confirms exactly the approved nine-file correction
and unchanged fixture hashes, HEAD and glcdp pin. Checked runtime/test sources
match the candidate. Exact packet: `/private/tmp/llw-m7-r57-review-MG8nrU`.
Independent acceptance still precedes owner final check.
R52 remains owner-deferred, not passed; no later slice, integration, commit or
push is authorized by this correction.

Owner refinement, 2026-09-23: R57 was subsequently accepted by the independent
reviewer for owner final check only. During that check the owner requested a
borderless explorer help icon, removal of the separate editor summary card,
relocation of Undo to LightLogWeb overlay, and deletion of standalone overlay
export. These refinements receive local regression/browser checks without a
separate external review, as requested. R52 is now superseded by feature removal,
not passed by a saved-file test. The prior benchmark and frozen reviewer packet
remain historical evidence, not verification of this later refinement. Full-app
integration, commit and push still require their owner gates.

Owner final-check consistency correction, 2026-09-27: the accepted Dashboard
quick-edit build correctly prefills imported factual label/unit values and treats
unchanged Save as a no-op, but its Focus variable metadata crosslink omitted
those already-indexed factual fields. The owner authorized a scoped correction.
Exact retained factual variable paths now supply a source-qualified fallback
focus target without creating an overlay, reconciling sources or rebuilding the
index. Inventory/overlay routes retain precedence; ambiguous or unmapped source
ownership remains unavailable. Initial navigation, unchanged Save, genuine edit
and Undo are regression-tested together. Independent acceptance, owner final
check and commit are still separate gates; this does not declare M7 complete.
Local verification: 1,988 focused and 5,447 full assertions, package check Status
OK with 5,366 installed passes and only documented warnings/skips. The full
benchmark at `2026-09-27T12:07:08Z` passes unchanged hard gates (ordinary maximum
87 ms p95; exact factual focus 2 ms). Source/index, payload and lineage remain
unchanged. Frozen follow-up packet:
`/private/tmp/llw-m7-factual-focus-review-TZjk2b`; independent acceptance pending.

The subsequent independent pass closed that factual-navigation defect as R60
and required R61: clear stale successful crosslink feedback when Undo or clearing
removes its field. The owner approved the narrow R61 correction contract on
2026-09-27. The existing explorer observer now validates the requested group
against the retained in-memory index on overlay revisions, without changing
valid exploration context or data. Focused regressions pass 954 assertions;
full tests pass 5,531 assertions with two expected warnings and six mirai skips.
Package check is Status OK (5,450 passes, two expected warnings and nine skips).
Browser checks verify Undo/clear feedback and protected source-backed navigation.
Complete status refresh takes p95 62.65/81.95 ms for absent/actual-small cases,
below 100 ms with no measurement decoding. The refreshed full matrix passes all
hard gates: integrated ordinary maximum 84/100 ms, wide single removal
79.3/150 ms, wide bulk 176.6/500 ms; mutation allocation headroom remains
825,336 bytes under 64 MiB. The 50 ms design goal is not uniformly met. Frozen
revision `/private/tmp/llw-m7-r61-review-uFEw8S` was subsequently accepted by the
independent reviewer: R61 closed, R60 remained closed and R58 stayed protected.
The owner then accepted the showcase and authorized milestone closeout, commit
and push on 2026-09-27. R59 stays deferred and nonblocking.

The owner-approved "Dataset details" label replaces "Factual Metadata" in
explorer paths and matches the section selector. It is display wording only:
the `factual_metadata` key, exact paths, source fingerprints, provenance and
overlay distinction remain unchanged. The heading does not imply independent
verification of imported facts.

The owner-approved Slice 1B extends only the read-only explorer foundation.
Selecting a result exposes every sibling property belonging to the same exact
source item, then permits focus-based traversal through retained ancestors,
children, and meaningful top-level metadata areas. Shared containment is
displayed as containment and never as inferred semantic equivalence. This
extension does not pull relationship-aware library/dashboard integration
forward from Slice 2.

The owner-approved Slice 2A navigation correction keeps the matching-field
list inside the Find metadata card. The owner's subsequent compact-path
correction replaces the collapsed panel with an always-visible, one-row path
inside one Metadata explorer card: at most four blocky hierarchy levels, or the current level on
narrow phones with compact, accessibly named navigation controls.
It starts at meaningful source areas such as Core metadata, Dataset inventory,
and Variable inventory, omitting duplicate resource wrappers while preserving
their exact retained paths. If earlier levels are hidden, a left ellipsis opens
a Shiny-native full-path modal with ancestor jumps and nothing else. There is
no Browse control or hidden navigation menu. The blocks use the former Where
this field lives styling, including friendly names and exact raw node labels;
that duplicate sequence is removed from selected-field detail. There is no
redundant Fields at this level list. Nearby structural branches and the
separate exact-item selector stay absent; exact node and record identities
remain in the in-memory routing contract.

The contents in that same Metadata explorer card follow the path and always
reflect the current hierarchy focus. The single visible contents list merges
contained child branches and direct field properties. Arrow actions enter
children and extend the path; radio buttons select a field without changing
the path or available choices. In-place field selection keeps the contents
list mounted and does not request scrolling or move keyboard focus; native
radio updates synchronize selections from other entry points. Search results
and deliberate hierarchy/dashboard navigation still land at the explorer.
The retained-item count shares the One level up toolbar, wrapping only when
the narrow viewport cannot fit both controls. One level up moves to exactly
one meaningful
parent, including ancestors hidden from the compact path. From a top-level
area it returns to the area overview, where it is disabled. The overview
exposes the meaningful top-level areas in this same list. A keyboard- and
touch-accessible header help button opens native bslib popup help with Escape,
Close, and focus return. There are no redundant inner headings or persistent
help paragraph. Branch names and chevrons retain dark text in hover, focus,
and active states, with a visible keyboard-focus outline. Structural nodes with no
direct fields explain that their contained items can be opened. Selected-field
detail remains stable until a field is explicitly chosen, and its Values in
this field section precedes About this field. Dataset state, identity,
source/overlay provenance, and
the selected-item notice appear at the bottom of the Metadata tab, after field
detail. Dashboard metadata context follows the dashboard workbench. A small
local browser helper scrolls and transfers keyboard focus to Metadata explorer
after explicit result selection, hierarchy jumps, or dashboard routes.
It waits for the matching rendered selection, coalesces superseded requests,
and does not change data, query/filter state, or route identities. Native Shiny
controls handle all selections and modals. No visualization dependency is
introduced. The owner has authorized custom JavaScript where no Shiny-native
equivalent exists. The owner requested reviewer subagents but subsequently
authorized the existing external reviewer for actual visual inspection when
those subagents lacked browser access. The same responsibilities and owner
gates apply; neither route may substitute programmer screenshots for an
independent visual pass.

The explorer's Section control is derived from grouped field paths rather than
transport resource names. It exposes logical Core Metadata, Dataset Inventory,
Variable Inventory, and retained unknown top-level areas within the selected
source scope. The filter is applied to the complete grouped in-memory index
before the 40-row display cap. It is omitted when fewer than two specific
sections exist.

Every enabled dashboard metadata crosslink is an activation event, including
a second activation of the same participant or device after the user has
navigated elsewhere in the hierarchy. The app-level delivery bridge reapplies
the exact request without changing its schema or stable request ID. Query,
logical section, selected field, hierarchy focus, and the focus-driven
comparison therefore return to one consistent destination. Deterministic
dashboard action identities, stale-event rejection, and record-refresh clearing
remain unchanged.

Within one parent location and exact source namespace, repeated same-level
nodes with the same label, kind, resource, and layer are one logical navigation
branch. Counts, sibling fields, retained records, and the exact-path disclosure
cover every grouped peer. Thus a repeated variable such as Melanopic Edi is
listed once under Variable Inventory and opens the fields from all included
file-group instances. This display grouping never crosses a source namespace
and does not flatten or reconcile source metadata. A record refresh also clears
any stale checked matching-field control together with the server selection.

The owner-approved Slice 2 relationship and integration contract retains one
typed, versioned `llw-metadata-relationships` context per exact GLC source. It
is built during the existing inspection while the package is already open,
using only exported `extract_metadata()`. File-group traversal uses
`file_group_id`; explicitly dataset-level traversal uses `by = "Id"`.
Ambiguous participant identity is resolved through the exported API with the
explicit `participants` resource. The retained relationship context is
fingerprinted and serializable, but it is a LightLogWeb integration artifact,
not another package metadata resource. It is therefore excluded from source
metadata search results while the exact source records remain fully indexed.
Projection scopes both records and relationships to the selected datasets,
participants, devices, datasheets, studies, file groups, and variables.
For appended dashboard navigation, a disposable relationship view may project
only exact dataset, participant, device, study, and file-group identities
across immediate sources. Every projected row remains qualified by
`llw_source_dataset_id`, and each selected entity carries its one exact source
namespace into the metadata crosslink. Source relationship contexts and
descriptive metadata remain separate and unchanged.

Slice 2 may use exported `add_metadata()` only on a disposable derived key or
analysis table with `overwrite = FALSE`. LightLogWeb asserts row membership,
order, key values and types, and dplyr grouping before showing the result. The
canonical source and prepared payloads are never passed to this enrichment
boundary and remain byte-identical. Missing or ambiguous links, multi-device
datasets, incomplete append lineage, unavailable APIs, and column conflicts
are recoverable states. In particular, IZTECH S004 must retain its distinct
AL03 and AL04 devices and file groups without reconciliation.

Metadata display resolution is also owner-approved for Slice 2. Raw variable
identities remain authoritative. Display label and unit precedence is the
exact matching LightLogWeb overlay row, then the exact retained source
namespace, then imported source details, followed by the raw name and the
literal `unit not specified` fallback. Precedence is resolved separately for
each property, so an overlay label or unit never hides immutable source
calibration evidence. Appended variables resolve only through their exact
source mapping and `llw_source_dataset_id`; conflicting sources are never
flattened or reconciled.

The dataset library and dashboard expose these labels, units, and provenance,
plus exact metadata crosslinks for source, dataset, focus variable, selected
participant, related devices, and a selected variable-inventory row. The
metadata explorer resolves each crosslink against its existing in-memory
index and retains the valid current selection when a target is absent or
ambiguous. Source timezone and source calibration remain factual provenance.
Analysis timezone, focus choice, view scale, missingness denominator, and
other display choices remain analysis or view settings. The dashboard source
view shows the source timezone rather than the display timezone.

Dynamic participant and device actions use deterministic identities derived
from their exact dataset, namespace, field, path, and record identity, never
their visible list position. Up to four participants and up to four related
devices are shown as individual actions. Above either limit, that entity group
shows a reduction-required state and no partial or arbitrarily truncated list.
A participant refresh baselines only identities in the new scope.
Consequently, a briefly retained browser button cannot silently route its old
label to a different newly ordered entity.

For Slice 2, label and unit resolution, relationship selection, crosslink
resolution, and ordinary dashboard metadata context must meet the existing
50 ms p95 design target and 100 ms p95 hard gate in memory. A 50,000-row
disposable `add_metadata()` operation must meet the 100 ms p95 hard gate and
allocate at most 64 MiB temporarily. It must preserve row order, keys, and
grouping. The three fixtures, widest UCR inventory, appended review state,
metadata-absent fallback, and 50,000-row derived case are all benchmarked
offline. Routine use and tests never reopen a published package or
deserialize a record for an ordinary metadata interaction.

Milestone 6 remains the owner of data/metadata transport, task recovery, and
session record recovery. Milestone 7 carries its metadata bundle through those
existing seams and does not introduce a parallel recovery mechanism. Portable
project save/reload remains within the later project-bundle milestone.

For Slice 1, ordinary hierarchy navigation, fixed-text search, filtering,
sorting, and selection use only the retained in-memory index. The design gate
is p95 at or below 50 ms and the hard gate is p95 at or below 100 ms. Each
retained bundle must remain at or below 32 MiB, the three approved fixture
bundles together at or below 64 MiB, and a 50,000-entry scaled index at or
below 64 MiB. One complete neighbourhood and hierarchy-focus operation on the
scaled index must allocate at most 10 MiB temporarily. Work outside those
bounds must use the existing truthful task progress, duplicate blocking,
recovery, and retained-context behavior. Constructing and rendering the
expanded textual hierarchy panel must also allocate at most 10 MiB
temporarily and meet the same interaction timing gates.

Slice 1 uses three bounded derived fixtures from different CC-BY-4.0 public
packages at exact revisions: TUM MEDI-only at
`1187d19b7f6145b73b8d4dfee13af711c1cb6180`, IZTECH LIGHT plus MEDI at
`abc456bdb418e6250b9c973813a190dc062c6e76`, and all variables from the UCR
S001 through S005 wrist files at
`1d1309c969b9ca754a5c9696382c39d4b58ccdfd`. Each fixture represents five
participants. TUM retains five files, IZTECH retains six files because S004
has separate AL03 and AL04 device files, and UCR retains five files.
IZTECH remains explicitly partial because two declared metadata resources
returned HTTP 400 through exported glc_metadata at generation. Routine tests
are offline and never reopen these public packages. The fixture manifest
labels every snapshot as derived, bounded, and not current or complete
published evidence. Under the owner's no-filtering instruction, exact bounded
public source values are retained and no privacy transform is applied.
Fixture schema 2.1.0 adds only the source-scoped relationship context from
exported `extract_metadata()` calls. Its successful metadata-only augmentation
at `2026-09-02T09:11:00Z` reused the existing measurements, made zero public
measurement reads, and wrote all three fixtures transactionally. The Slice 2A
acceptance run at `2026-09-02T19:06:14Z` passes the 100 ms p95 hard timing
gate, memory gate, navigation-correction gate, repeated-crosslink gate,
relationship-integrity gate, append-lineage gate, no-reopen gate, and
no-deserialization gate. Repeated delivery of the same exact IZTECH S004
participant and AL03 device routes takes 2 ms p95 and retains stable request
IDs. Every realistic fixture and appended-state interaction remains within the
50 ms p95 design target. The artificial 50,000-entry hierarchy-panel render is
73 ms p95, below the hard gate but above the design target; its ordinary
navigation, search, filtering, sorting, selection, path, and focus-comparison
operations remain at or below 23.65 ms p95. This includes the realistic
fixtures, appended view, synthetic 50,000-entry stress inventory, 50,000-row
derived enrichment, logical-section filtering before the display cap, stable
participant/device action identity, and idempotent repeated route activation.

The Slice 1 offline review route also constructs one deterministic appended
state in memory from the frozen TUM and IZTECH fixtures. Its 352 rows retain
stable `llw_source_dataset_id` lineage, its two metadata namespaces remain
separate, and the TUM and IZTECH measurements use distinct result columns.
One source-bound revision 1 overlay value makes layer separation reviewable.
The fixture manifest records both source citations, revisions, licenses,
package dataset IDs, mappings, and the no-reopen policy. No additional package
data file or public measurement read is required for this review state.

The Slice 1 explorer presents one selectable result per semantic item and
field location rather than one row per repeated record value. Repeated values
for the same item across datasets or participants stay together. Sibling
items, such as photopic and melanopic variables in the same variable table,
remain separate selectable results. Selecting a result shows every exact
retained record value and path. Parent dataset or file-group identity remains
separate from the semantic item and leaf value, preserving readable file to
device and file to variable relationships. The always-visible Metadata explorer
starts with a compact path through meaningful source areas, then shows direct
fields and contained metadata in one list. A left ellipsis reveals the full
path when ancestors are hidden. One level up selects the immediate meaningful
parent; native popup help explains branch arrows and field radios.
Duplicate source/resource wrappers, nearby-branch shortcuts, and a separate
exact-record selector are not shown. Exact record node identities remain
available internally for crosslink routing and the current exact focus path.
Repeated same-level structural nodes are collapsed into one logical branch
only when parent, label, kind, source namespace, layer, and resource all match;
all exact peers and paths remain disclosed below that branch.
The same explorer exposes same-item properties as selectable fields across
the included records. When a
source is carried through append, file-group lookup recurses only through that
source namespace's retained immutable metadata bundle. This preserves exact
file-group identities in values, hierarchy nodes, and focus paths. For example,
IZTECH participant S004 retains separate `:19` and `:20` records linked to AL03
and AL04 instead of falling back to the shared parent dataset id. Separately,
the source term `melanopic_edi` remains distinct from a concrete `MEDI`
variable whose retained unit is `lx`; the explorer permits navigation between
their containing branches without assigning the concrete unit to the term.
Single-source records omit a source selector and source labels from matching
results; appended records keep their source selector and provenance. Native
section, layer, source, and sort controls are used so their menus are not
clipped by the search card.
Free-text search covers raw and plain-language field names plus retained
values. It does not copy source labels, record labels, or whole paths into
every field's search text, so direct variable matches remain visible within
the bounded result list while source and hierarchy context remain available
through their dedicated controls.
The offline review loader verifies each frozen source snapshot fingerprint
before building one current derived search index from its immutable sources
and overlay. It does not change the serialized snapshot and never rebuilds the
index during ordinary explorer interactions.

The public-derived fixtures and the production GLC snapshot seam retain only
study, participant, file, device, datasheet, and variable metadata connected
to the represented import selection. Unrelated participants, devices, files,
and questionnaire-variable definitions are not copied into the selected
metadata context. The fixture values remain exact public source values under
the owner's explicit no-filtering instruction. Tests remain offline and never
treat the bounded snapshots as current evidence.

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
11. Preserve the overlay through the existing record/session recovery contract.
    Standalone overlay JSON/CSV export was removed by the owner on 2026-09-23.
    Generation of an edited standards-compliant GLC package remains out of scope
    until glcdp provides a public write-and-validate API. Schema 3.0.2 being final
    does not authorize LightLogWeb to recreate schema serialization or treat the
    metadata builder's soft validation as authoritative.

Acceptance gate: complete, partial, and absent optional metadata all permit
valid analysis; source metadata and overlays remain distinguishable; unknown
fields survive project save/reload; file-group and explicit dataset-level
metadata extraction preserve row count, order, grouping, and identifiers;
ambiguous relationships and column conflicts are recoverable; display-only
edits do not mutate data or invalidate unrelated results; and the earlier
source-agnostic library and dashboard remain valid when metadata are absent.

Slice 1 evidence:

- dev/milestone-7-metadata-essentials.md
- dev/run-milestone-7-metadata-acceptance.R
- dev/milestone-7-metadata-benchmark.json
- tests/testthat/fixtures/milestone-7/manifest.json

### Milestone 8 — Recipe-based preprocessing

Closed, 2026-11-17: the owner accepted Slice 5 R14 and authorized committing and
pushing the completed milestone. All five slices are accepted. The final
candidate passed 1,328 focused assertions, 44 independent metadata contracts
and independent responsive/browser review. The accepted frozen source index is
`d0d71ba567501bcef3fef479a1ca1eae50bc4c5a3af35b6fa3adce209b7ebdc0`.
Deferred narrow-layout notes remain nonblocking. Detailed evidence and the
acceptance record are in `dev/milestone-8-recipe-preparation.md`.

The owner also accepted the redesigned Prepare workspace on 2026-11-17 after
independent review of R11. The application now opens Prepare with a recipe
sidebar, an operation picker and a preview alongside the editor on wide
screens. The [workspace closeout](dev/prepare-flow-experiment.md) records the
accepted changes to Variability and Aggregate and the browser verification.

Historical review progression:

Subsequent owner retest, 2026-11-17: R13 fixes hidden date-control resets that
cancelled automatic preview after the duration/range/variability sequence, and
adds independent metadata editing for Prepare-created variables. Assertions are
scoped to the dataset and creator; source metadata and history remain intact.
Independent review confirmed both reported flows. R14 corrects its nonlinear
parent-unit inheritance follow-up and adds the owner's bounded presentation-only
Prepare cleanup. Inputs, defaults and outputs are structurally unchanged. Final
independent checks and owner retesting remain required; Slice 5 stays open.

R14 handoff: independent review accepts the frozen candidate for owner final
review. The consolidated focused suite passes 1,328 assertions and the reviewer
passes 44 independent metadata contracts plus the six-width light/dark browser
checks. The showcase is `http://127.0.0.1:7455/`; only that instance remains running.
The 221-file source index is
`d0d71ba567501bcef3fef479a1ca1eae50bc4c5a3af35b6fa3adce209b7ebdc0`.
Dense 1024-pixel wrapping and earlier owner-deferred mobile notes remain
nonblocking. The report and sealed evidence are retained in the independent
review chat's `m8-s5-integration-20261117-r14-review` folder. The two presentation
files have a separate patch so that the owner can assess that bounded proposal
independently of the fixes. Slice 5 remains open until the owner's acceptance.

Current owner review, 2026-11-17: Slice 5 remains open. The owner reviewed R7 and
authorized the accumulated feedback for R8: elapsed 24-hour days by default,
preview-queue recovery, non-dirty operation browsing, an explicit Save-and-preview
action, visible busy status, taller plots, bottom legends, Only-step comparisons,
and optional split plots with each stage's time zone. Apply synchronizes the
Dashboard's derived focus and time zone. Zero-inflated log/inverse operations
use LightLogR with recorded base and offset. Long axis labels wrap and the
missingness denominator choices use the owner's wording. Independent review
and the revised showcase precede the owner's retest; this is not slice closeout.
Final review corrections in R9 include automatic axes covering transformed
values, nearest-ancestor focus after removing a derived branch, and clean editor
restoration after Dashboard focus changes while retaining genuine unsaved edits.
Independent R9 review cleared these functional corrections. R12 aligns the
time-zone fields when their labels wrap, separates plot caption lines and
clarifies Save-and-preview guidance. Independent presentation review passed R12
at the six required widths with light/dark samples. The final owner showcase is
`http://127.0.0.1:7453/`; obsolete runtimes are closed. The 320-pixel empty-stage
subtitle note remains nonblocking. Slice 5 is open pending the owner's retest.

Latest gate, 2026-11-16: the owner accepted Slice 4 r6 and authorized Slice 5,
including independent review before handoff. Slice 4 is closed. Slice 5 now
covers full-app Prepare/library/dashboard wiring, shared focus and plot settings,
effective metadata labels/units, retained recipe drafts, task recovery and
performance/responsive verification. Project/script export stays in Milestone 13.
Independent R3 review closed derived Dashboard focus/labels, summary-card layout
and stale-draft restoration after Reset. It identified R37: reordering took
171.6 ms p95 when required browser acknowledgement work was included. R4 retains
those updates but avoids unchanged control/catalogue and date-widget rebuilds.
Its complete local matrix reached 96.8 ms p95, but independent R4 review measured
108 ms at 250k and retained R37. The affected functional and layout checks passed.
R5 keeps clean editor acknowledgements from redrawing unchanged dirty-state
outputs. Saved-step reordering can replace an outstanding clean editor update;
unsaved edits remain protected and Save/Apply still wait for acknowledgement.
Independent R5 review retained R37 at 102.4 ms and opened R38 for unexpected user
input lost during a pending control update. R6 merges that input into the draft
transaction, including across dataset switches, and batches stable UI state.
It reclaims temporary objects after full refreshes, avoiding major-GC pauses in
subsequent ordinary edits at the cost of cleanup time at refresh completion.
Independent R6 review passes the complete performance gate at a maximum p95
of 83.8 ms, but retains R38 for returning an acknowledged field to its former
value while another field is pending. R7 retires old values per field after
acknowledgement or a captured user edit. All 11 full-parent draft-preservation
checks now pass. Its complete local 25k/250k control matrix has a maximum p95
of 67.6 ms, including acknowledgements, with no source-payload decoding or
Dashboard rebuilds. The R7 consolidated suite passes 3,964 assertions in 257
cases, with three expected cancellation warnings and no failures, errors or
skips. Independent R7 review accepts the candidate for owner final review:
R38 closes with 14 state checks and the browser Save/reorder/Apply flow; the
complete timing gate passes with a maximum p95 of 72.2 ms. The source-package
R3 check was Status: OK. The owner deferred their review until later and asked
for all instances closed. Servers 7433/7437/7440/7449 and showcase tabs are
stopped; frozen R7, its evidence and restart instructions are saved. Slice 5
owner acceptance remains pending.
No commit, push or deployment is authorized.

Historical Slice 4 handoff, 2026-11-16: Slice 3 was accepted and closed, including the final
conditional gap-legend refinement independently accepted in
`m8-s3-gap-marks-20261116-r2`. The owner authorized Slice 4 aggregation.
The owner chose to keep low-coverage aggregates and report coverage separately,
and clarified that simple controls must still allow multiple aggregation steps.
Slice 4 uses public LightLogR aggregation with explicit numeric and NA policies,
actual local bin windows, bounded coverage reports, and compact source-to-bin
provenance. Fresh-from-source one-hour-to-fifteen-minute parity is a required
acceptance check. Candidate `m8-s4-aggregation-20261116-r6` passed independent
review and awaits owner acceptance at `http://127.0.0.1:7440/`. R31, R32 and R33
are closed. The owner's follow-up refinements add prominent failure alerts,
simple categorical/logical summaries, numeric sum and an optional row-based
missingness limit. The limit counts NA among existing readings; the UI explains
that Gaps must precede Aggregate to count absent timestamps. Inactive invalid
limits do not block another missing-data policy. Existing defaults and multiple
aggregation steps remain supported. Consolidated affected verification contains
1,804 passing assertions across 128 cases, with no failures/errors/skips and
three expected cancellation warnings. Independent review adds 125 refinement
checks, 24 final correction checks, browser recovery and six responsive widths.
The existing 320 px summary-number alignment note remains deferred. The detailed
contract and durable evidence are in `dev/milestone-8-recipe-preparation.md` and
the frozen r6 artifact. Superseded runtimes 7438/7439 are stopped; owner drafts
7433/7437 remain untouched. Slice 4 is open for owner acceptance; full-app
integration remains Slice 5.

Prior progression: the owner accepted independently reviewed Slice 1 r9b,
closed Slice 1 and authorized Slice 2 (measurement values). Slice 2 r3 has now
passed independent review, including the owner-requested operation choices
and coverage duration thresholds. The owner's further request for derived
measurement variables, editable collision-free defaults, inherited metadata and
transformation history has passed independent review on frozen build
`m8-s2-r7-bcedbce71b7f`. Recovery issues R13-R15 are closed. Three nonblocking
layout notes remain at 320-336 pixels. The owner then requested a two-input
Target variable / Write to editor, connected variable chains across edits and
reordering, globally centered dropdowns and a taller plot using dashboard axis
settings. That refinement passed independent review on frozen build
`m8-s2-r10-dcc985cff4e9` at port 7416. Full-app
axis wiring remains part of Slice 5; the modules now expose the reactive contract.
The owner explicitly accepted this boundary and requires the integrated preview
to inherit both the dashboard focus variable and plot settings. Participant/day
selection remains local; other relevant display preferences will be assessed at
integration. Frozen candidate `m8-s2-r8-14de6805ddc1` passes 1,663 assertions in
130 affected test blocks. Independent review found R17, an exact-category editor
loading failure after reorder. The follow-up preserves saved categories that are
absent from the new input catalog. R9 passes 333 focused assertions, and its
browser recheck preserves categories, completes editor loading and restores the
original recipe on Undo. R17 and R18 (compact dashboard selector wrapping) are
closed. R10 passes focused dashboard checks at desktop/390/320 pixels and a
derived-variable Prepare journey through Apply. Independent R verification
covers 20 chain scenarios and 12 plot-setting combinations. Minor R16 totals
alignment at 336 pixels remains nonblocking. The M8 note records the frozen
identity, acceptance report hash and runtime cleanup. Owner acceptance is pending.
The owner's r11 follow-up corrects inactive variable-tool defaults when reopening
a later saved step: use the preceding recipe context while retaining explicitly
saved selections. The same defaults now refresh after direct Save as new step
(r11 review finding R20). Recipe controls text is also enlarged. The owner added
a Variability operation: elapsed-time SD or CV, separate pinned source channel,
target/output lineage, adjustable 80% window coverage, and reported unevaluable
windows that leave target values unchanged. This follow-up passes independent
review on frozen `m8-s2-r13-1946f73d90c0` at port 7420: 46 independent numerical
fixtures, real-data SD/CV browser parity, source fallback and recovery, and phone
layout checks. The programmer's r13 focused suite passes 108 assertions; the
964-assertion r12 suite remains supporting evidence. Long Measures diagnostic
labels still need horizontal scrolling, a nonblocking layout note. Owner 7416
is preserved; released review runtimes are stopped. The owner's subsequent
create-then-switch issue passes independent review on frozen r14,
`m8-s2-r14-4cd689ffc532` at port 7421: saving a new-variable creator
now prepares a next-step draft with that output selected, while Edit restores
the creator's original inputs. A separate read-only slider performance/outcome
comparison passed independent methodological review with documented threshold,
adapter and run-provenance limitations. It changes no production engine or
dependency. The r14 focused suite passes 493 assertions in 46 blocks; browser
review confirms defaults, saved creator inputs, collision refusal, Undo/Apply
recovery and the 320-pixel layout.
Owner acceptance is pending. Every
slice requires independent acceptance followed by owner acceptance before the
next slice begins. Full application wiring is Slice 5. No commit, push or
deployment is authorized by this milestone gate.

On 13 November the owner authorized the vector-storage optimization and requested
independent review before the updated showcase. R15 changes only rolling result
storage, preserving the numerical operations and return contract. Exact parity,
fresh fingerprinted performance evidence and a focused browser check are the
acceptance requirements. R15 passed independent review as the storage-only
baseline, with exact parity and the source/derived-variable, Apply and Reset
journeys verified. The owner then requested an explicit unevaluable-window policy:
keep target unchanged by default, or set target to NA. R16 adds this choice with
versioned recipe/history support and passed focused independent review as the
intermediate baseline. The owner also requested independent numeric/categorical condition
sources for Valid range. R17 adds pinned check sources, lineage fallback,
matching/non-matching conditions and explicit missing-source handling, preserving
legacy range recipes and the optimized rolling engine. The combined candidate
`m8-s2-r17-f3ebdb5e9888` at port 7424 passed independent review, including 64
complete-output condition cases, source fallback and visible keyboard/Edit/Apply/
Reset/Transform journeys. The focused suite passes 921 assertions in 64 blocks.
Acceptance report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r17-review/review-acceptance.md`,
SHA-256 `9cc026bccd6e31da77dcfcc8b6ad948d5396e68ac5e1422019a4525c99479074`.
Released review runtimes 7422/7423 are closed; owner draft sessions are preserved.
The owner accepted Slice 2 on 2026-11-13. The reviewed source remains frozen;
closeout changes only the acceptance records and manifest. No slider dependency
or algorithm replacement was introduced.

The owner additionally requires simple, learnable R: short explicit functions,
existing tidyverse components and documented public LightLogR calls. Each step
must have a readable plain-R equivalent and a versioned public-call mapping
with explicit arguments. Complete-recipe parity tests use independently
written R pipelines. Safety checks, provenance, resource control and reactive
orchestration remain separate from the scientific pipeline. Do not introduce a
generic workflow framework, expression language, arbitrary R evaluation, a
whole-tidyverse dependency or unapproved dependency changes. Milestone 13 owns
script/project export UI.

1. Recipe foundation and selection: ordered add/edit/reorder/enable/disable/
   remove, participant selection, date/datetime ranges, recurring cross-midnight
   clock windows, Preview, Apply, Reset and Undo.
2. Typed conditions, numeric filtering/invalidation and explicit calibration.
3. Time integrity and gaps: DST diagnosis/correction, timezone semantics,
   marked gap expansion and resource estimates.
4. Aggregation, intended-window coverage and observed-duration rules, including
   fresh-from-raw one-hour versus fifteen-minute parity.
5. Full-app integration and hardening: dashboard/library, metadata compatibility,
   task recovery, performance/responsiveness and retained serializable recipe.

The bounded Slice 1 contract, startup baseline and verification record are in
`dev/milestone-8-recipe-preparation.md`. Independent r1 review requested three
corrections: action-local feedback, settled cancellation feedback, and truthful
selection scope/temporal diagnostics when file identity is absent. Revision r2
addresses these within Prepare and has 174 passing focused assertions. The
independent r2 re-review accepted build `m8-s1-r2-ca277661989a` for the owner's
final check and closed R01-R03 and R05. Owner acceptance is pending. The packaged
suite retains its known GLC retry failure. No later slice is authorized by these
checks or by independent acceptance.
Owner feedback on the r2 showcase accepts the broad functionality but requests
bounded dashboard-style date pickers, native date/clock selection, separate
date/time/native-or-Olson-zone controls for both datetime endpoints, a clearer
editing/preview layout, and duration selection from either end with overall or
per-dataset scope. Slice 1 revision r3 implemented these requests; its independent
review passed the controls and requested one date-recovery correction, R06.
Revision r4 adds field-specific available-range guidance and an eligible month
when a cleared picker opens, without substituting a date. Focused r4 checks pass
257 assertions. The independent r4 review accepted build `m8-s1-r4-20456c80a1c9`
and closed R06 after both date-recovery paths, mouse Save and endpoint Edit
passed. Owner acceptance of the controls and provisional layout is pending.
Ordinary edit p95 is 51 ms, above the 50 ms target and below the 100 ms hard gate.
R2's independent acceptance applies only to its preserved snapshot. Duration now
offers elapsed units and whole local calendar days, from either end, with
explicit overall, source-dataset, participant and retained-identity scopes.
Source-dataset scope requires complete retained source identifiers. Explicit
`with_tz` (preserve instants) and `force_tz` (preserve wall-clock readings)
operations are recorded for Slice 3 rather than silently altering timestamps.
The owner's earlier mockups are selective inspiration. Prepare changes the
observations; Group defines analysis units in Milestone 9. The clock control
now explicitly describes row filtering. Effective variable labels/units remain
an open, nonblocking reviewer requirement to carry into Slice 5 integration.
On 2026-11-11 the owner requested a further layout iteration before moving on.
The isolated showcase now uses numbered selection and recipe-review cards with
an adjacent impact preview, and defaults to four participants from the standard
IZTECH fixture. This exposed a grouped-input clock-mask defect, corrected within
the existing selection pipeline with independent R parity checks. Focused checks
pass 263 assertions; browser checks cover the revised flow, Apply/Undo and narrow
stacking. This workspace iteration is provisional and is not covered by r4's
independent acceptance. The owner subsequently required independent review before
every handoff, including layout iterations. Frozen r5 build
`m8-s1-r5-91bbda0c35f1` has now passed that independent review: no new functional
or alignment blocker across six widths, protected R01-R03/R05/R06, and independent
R parity for the real grouped-data daytime and overnight selections. Existing
narrow-table readability and R04 labels/units remain nonblocking integration
notes. All 200 frozen file hashes matched before and after review. The M8 note
records the report and evidence. Owner acceptance, Slice 2 and full-app
integration remain gated.
The owner then completed another feedback batch authorizing the one-recipe
layout and automatic-preview revision, plus typed variable/NA, photoperiod and
daily coverage filters. Frozen r6 build `m8-s1-r6-fcb0da9ce616` has passed
independent review for owner design feedback. It moves
the editor to the full left column and the recipe and overlaid day preview to
the right, keeps Apply/Reset visible, removes the data-loss acknowledgement,
and shows per-step removal and applied/preview status. Repeated Apply starts
from the original source. The other transformation slices remain deferred;
no full-app integration is authorized. The independent pass found no new
acceptance or alignment defect across six widths and verified new-filter,
source-row and per-step-denominator parity in R. All 204 frozen files remained
unchanged; r5 is preserved. Focused checks pass 307 assertions. The real-worker
automatic-preview benchmark passes the 100 ms p95 limit at 73 ms for 250k rows
and 84 ms for 1m, while missing the 50 ms target. Narrow sample-table readability
is resolved in the exercised views; R04 metadata integration remains nonblocking.
The M8 note records the exact report and evidence. Owner design acceptance
is pending.
The subsequent r7 owner refinement moves the recipe below a natural-height
editor and places impact at the top right, with compact toolbar and plot controls
so common editor controls and the full plot fit together on a laptop viewport.
It reuses dashboard symlog axis behavior with threshold 1 for this showcase.
Focused R, Sass and responsive checks plus independent review are required;
the full regression suite and resource benchmark are deferred during this
bounded layout iteration. R6 remains frozen and preserved.
The independent review prompted two bounded follow-ups: a shorter plot clears
the fixed footer at 1366 by 768, and an unchanged preview no longer leaves the
participant/day controls waiting indefinitely for browser acknowledgement.
The latter has a module regression and a focused preparation test rerun,
passing 311 assertions in 33 blocks. Frozen r7c (`m8-s1-r7c-66cfb8c68b5f`)
passed independent re-review, closing M8-S1-UX-R07 and R08. The complete plot
and both save buttons fit at actual CSS viewports 1280 by 800 and 1366 by 768;
manual selectors recover after refresh, disable and Undo. Owner design feedback
is next, with full-app integration still pending. The M8 note records the exact
source index, review report and preserved evidence.
The owner's subsequent r8 feedback clarifies the duration scope as "Per recording"
with plain-language help and expands the recipe action buttons across each row.
The combined revision also adds whole-group coverage with or without gap
handling, using the current dataset groups as the owner explicitly requested,
and a default-on automatic informative-day checkbox that switches off when
the participant or date is manually selected. Frozen r8b
(`m8-s1-r8b-c4ab4fd965e5`) passed independent review, with 18 independent
coverage comparisons and 401 focused test assertions passing. Responsive
action rows and automatic/pinned behavior through recipe edits, Apply and
dataset reset passed. The exact source and report hashes are recorded in the
M8 note. The owner superseded the
earlier close/advance instruction: Slice 1 stays open for one final comprehensive
view, and Slice 2 remains deferred. Sections 2.7 and 2.8 are adopted here through
the existing compact design and the four-participant IZTECH primary journey,
with synthetic fixtures limited to named edge cases. M8 is not restarted or
expanded with a new vision slice.
Further r9 owner feedback adds an explicit current-group duration scope,
renames the existing recording scope to "Per participant and file", centers
filter button contents, and keeps removed applied steps visible as pending
removals until Apply. Group definition controls remain in Milestone 9.
R9b passed independent review and was accepted by the owner on 2026-11-12.
Slice 1 is closed. The accepted build is `m8-s1-r9b-6eb6a5f6c7dd` at port 7403.
The M8 note records source and report hashes, 473 passing focused assertions,
40 independent duration comparisons and browser acceptance. The owner
explicitly authorized the `sortable` dependency for
drag-and-drop ordering, with Earlier/Later keyboard controls retained and
reordering connected to automatic preview and recipe Undo.
Slice 2 now adds range invalidation and explicit multiplier/offset calibration
in Measurement values, plus transformed-value previews and counts. Typed
filtering is already covered by accepted Slice 1. Build
`m8-s2-r2-e360c61a912d` passed independent review at port 7406, with 555 focused
assertions and six supplied independent IZTECH pipelines passing. It also
implements the owner's equal-height editor/preview cards, full-width recipe
below, and recipe/navigation split with Apply/Reset inside the card. R09
worker error guidance and R10 badge alignment are closed; R11 narrow-caption
fit and the known R04 metadata integration item remain nonblocking. The M8
note records the frozen identity and evidence. Later slices remain gated.
The owner then requested additional mathematical transformations and duration
coverage thresholds. Revision `m8-s2-r3-761164f70f3f` at port 7407 passed
independent review: 712 focused assertions in 52 blocks, 22 independent
transformation pipelines and 63 independent coverage comparisons. Coverage
failure now explicitly removes the entire current group or local group/day;
minimum available and maximum missing duration are supported. Scale/offset,
logarithm, root, power, exponential and absolute value use typed arguments and
explicit invalid-result handling. Two minor 320 px fit issues are recorded in
the M8 note. Owner acceptance remains pending; later slices are gated.
The following original requirements
continue to govern the complete milestone.

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
5. Within the shared reproducibility, export, project-bundle, report, and
   LightLogR-handoff workflow, let users include the GLC serializable source
   specification, original metadata, LightLogWeb overlay, and optional
   glc_download subset with glcdp-manifest.json. Keep the immutable source
   package and overlay as separate artifacts.
6. Provide one parameterized Quarto report with selectable provenance,
   metadata, quality, preparation, grouping, dashboard, summary, metrics,
   plots, and session sections.
7. Support HTML, PDF, and Word through runtime capability checks, isolated
   temporary rendering, and recoverable errors.
8. Default to HTML; explain missing PDF tooling precisely.
9. Run generated scripts in clean R processes and compare data, metrics, and
   plot data with the interactive session.
10. Implement the validated-source GLC subset export deferred from Milestone 6
    through glc_download. Integrate it into the shared workflow rather than a
    separate GLC-only surface. Include exact revision and selection, metadata
    closure, hashes, and glcdp-manifest.json. Do not label a metadata-overlay
    export as a GLC package or fold it into the downloaded source subset until
    glcdp exposes and validates a public writer workflow.

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
- GLC registry discovery, validated-revision preview, selective import,
  metadata exploration, and Milestone 13's integrated validated-source subset
  export are v1.
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
