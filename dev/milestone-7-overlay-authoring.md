# Milestone 7 Slice 3: overlay authoring

Owner approved the Slice 3 contract on 2026-09-21, adding batch editing as the
default and a single-item option. The owner explicitly resolved batch scope to
the current library dataset, including appended sources. No other library entry
may be changed. Baseline: `00ecd229f174503996aa91256120074f2b2faf37`.

Status: the owner accepted the Slice 3 production module and authorized wrap-up
and commit on 2026-09-23. This includes the owner-requested UI consolidation,
standalone export removal, toolbar placement and source-variable selector
refinement. R57 was accepted by the independent reviewer for owner final check
in `/private/tmp/llw-m7-r57-review-MG8nrU`; the owner explicitly waived another
external review for the subsequent refinements and has now accepted them.
R52 is superseded by removal of the export feature, not passed by saved-file
verification. Earlier checkpoints below are historical; the final verification
section records the accumulated accepted build.
The slice is not integrated into the full app. Full application wiring, later
slices, push and deployment remain separately gated.

## User contract

- Supported factual fields: dataset title; participant label and sex; device
  manufacturer/model; site, country, source-timezone annotation and coordinates;
  variable label/unit. Raw IDs, calibration and unmodeled fields remain read-only.
- The editor shows immutable source facts separately from the LightLogWeb overlay.
  Passing overlay validation is not source-package validation.
- All matching items is the default scope. Single item only selects one exact
  source-qualified target. Variable batches match the exact source variable name,
  not arbitrary variables or inferred semantic equivalents. Participant batches
  deduplicate repeated file/device references to the same exact participant.
- Show affected count, exact identities, original/effective and proposed values
  before Apply. Display pagination and search caps must not silently limit targets.
- Batch Apply is atomic. Stale, ambiguous or invalid targets prevent every write.
  Unchanged submissions are no-ops. A successful batch increments the overlay
  revision once. Undo protects intervening changes and treats a batch as one action.
- Explicit Apply, Discard and Remove override actions. Protect dirty drafts and
  retain valid context after failures. Source changes cannot redirect a draft.
- Preserve search, hierarchy selection and viewport when their identities survive.
- Never change source fingerprints, measurements, IDs, timestamps, calibration,
  recipes or analysis settings. A unit annotation is not a conversion; a source
  timezone annotation never reinterprets stored timestamps or import provenance.

## Versioning and recovery

Overlay version 2 adds source-qualified keys and participant sex. Immutable source
and bundle contracts remain unchanged. Existing version-1 rows are upgraded only
when their exact target is unambiguous. Other legacy rows remain preserved,
visible and unapplied pending explicit resolution. Unknown fields remain intact.

The owner removed standalone overlay JSON/CSV export from both the interface
and implementation on 2026-09-23. Existing record/session serialization still
preserves committed typed overlays, unknown values and source-qualified
identities. This is not edited-GLC generation or a project-loader feature.
The already installed and locked jsonlite package remains a declared runtime
dependency for exact source-qualified identities; no installation or repin is
authorized.

## Schema evidence

Schema 3.0.2 at the already-documented metadata-builder revision
`a8a312c58b91ae2b725168a3b5debe63030a82ad` was checked read-only. Participant sex
is an optional string, with no enumeration. The editor must not invent a binary
list, recode existing values, or conflate sex with gender. Units permit UCUM or
precise custom text, reject placeholders, and distinguish numeric/integer from
categorical types. These are input constraints, not authoritative GLC validation.

- https://raw.githubusercontent.com/tscnlab/glc-metadata-builder/a8a312c58b91ae2b725168a3b5debe63030a82ad/schemas/3.0.2/participants.schema.json
- https://raw.githubusercontent.com/tscnlab/glc-metadata-builder/a8a312c58b91ae2b725168a3b5debe63030a82ad/schemas/3.0.2/dataset.schema.json

## Fixed acceptance gates

- Ordinary form choice, validation, preview, warm one-target Apply/remove/undo
  and explorer interactions: p95 <= 50 ms design target, <= 100 ms hard gate.
  Exception approved on 2026-09-22: warm single removal in the populated
  5,000-variable-occurrence inventory has a p95 <= 150 ms gate. Other warm
  single operations retain 100 ms.
- Batch targeting/preview uses the same interaction gate. Measure atomic Apply
  separately for all three realistic fixtures and 5,000-field stress inventories;
  owner-approved explicit large-inventory commit/index creation and bulk
  transactions have a p95 <= 500 ms gate. This includes initial index attachment
  for one assertion in a 50,000-entry source inventory. Immediate busy feedback,
  duplicate blocking and retained valid context are required. This is not a
  relaxation for ordinary browsing, preview or warm single-item updates.
- Ordinary interactions allocate <= 10 MiB temporarily. Commit/index-patch
  operations allocate <= 64 MiB additionally, independent of measurement row count.
- Existing bundle/index limits remain: 32 MiB per bundle, 64 MiB combined for
  the three fixtures, and 64 MiB for the 50,000-entry index.
- Zero ordinary measurement deserializations or package reopens after startup.
- Keep frozen fixtures and their manifest byte-identical. All testing is offline.
- Test absent/partial/nested/unknown metadata, colliding independent source IDs,
  IZTECH S004's two devices, no-op/repeated/stale batches, atomic failure and undo.
- Focused/full/package checks and typed record/session recovery checks;
  real-browser mouse/keyboard tests at 1440, 1200, 1024, 768, 390 and 320 pixels.
  Protect accepted R39-R47 explorer behavior.
- Freeze exact build before external independent review. The owner waived a
  separate external review for the 2026-09-23 consolidation/removal refinement.
  Disposable offline overlay editing remains an authorized test action.
  No source writes or live reads.
- Reviewer acceptance permits owner final check only. Stop unused task-owned
  instances after each development step; leave unrelated processes alone.

## Approved boundary

The 25 existing and six new files below are the approved allowlist, not a
requirement to edit every file. No GLC adapter/import code, frozen fixture,
lockfile or generated namespace/manual change is authorized. Browser JavaScript
is limited to the owner-approved focus/viewport return and safe Escape
dismissal extensions in the existing helper.
A boundary expansion or unmet hard budget requires a new owner gate. Full
integration follows owner acceptance of the isolated production module, not
automatic continuation.

Existing files:

- `R/metadata-model.R`
- `R/session-model.R`
- `R/LightLogWeb-app.R`
- `R/m_metadata_explorer.R`
- `R/m_dataset_dashboard.R`
- `R/dashboard-data.R`
- `R/m_dataset_manager.R`
- `R/append-merge.R`
- `inst/app/scss/lightlogweb.scss`
- `inst/app/www/js/metadata-navigation.js`
- `DESCRIPTION`
- `project-plan.md`
- `dev/run-milestone-7-metadata-acceptance.R`
- `dev/milestone-7-metadata-benchmark.json`
- `tests/testthat/helper-milestone-7.R`
- `tests/testthat/test-metadata-model.R`
- `tests/testthat/test-metadata-explorer.R`
- `tests/testthat/test-session-model.R`
- `tests/testthat/test-dashboard-data.R`
- `tests/testthat/test-dashboard-navigation.R`
- `tests/testthat/test-dashboard-refinements.R`
- `tests/testthat/test-append-merge.R`
- `tests/testthat/test-metadata-fixtures.R`
- `tests/testthat/test-module-contracts.R`
- `tests/testthat/test-app-smoke.R`

New files:

- `R/metadata-overlay.R`
- `R/m_metadata_overlay.R`
- `tests/testthat/test-metadata-overlay.R`
- `tests/testthat/test-metadata-overlay-module.R`
- `dev/milestone-7-overlay-authoring.md`
- `dev/milestone-7-overlay-app.R`

## Implementation checkpoint

The isolated editor, version-2 validation, exact-target catalog, atomic preview,
apply/remove/undo, committed-only export and session transaction path are
implemented. The Shiny and R development skills guided the isolated-module gate,
native forms, narrow session write boundary, deterministic tests and package
verification. This is not a claim that the full Slice 3 integration is complete.
The later contextual-editing section records the owner-requested combined
explorer/editor showcase. Source-qualified effective presentation in the full
dashboard, cache invalidation and full-app R39-R47 regression checks remain
behind the showcase acceptance gate.

Source inventories are cached per record and source fingerprint. Ordinary edits
use retained identities and the source-only index; tests replace measurement
deserialization with an error after initialization. Metadata-absent records can
be annotated through observed imported identities without invented devices or
relationships. Unknown and ambiguous legacy values stay serialized and visible;
explicit resolution preserves the previous assignment in the export record.

Native task buttons expose busy feedback and block duplicate starts. Native
inputs flush on blur before Review or Apply. Browser checks caught the prior
default-input timing race; the corrected form previews the latest typed value
and refuses Apply when a previously previewed value has just changed.

At the current checkpoint, 322 focused assertions pass with no failures,
warnings or skips. The current package check finishes with `Status: OK`; its
complete packaged suite reports 4,081 passes, zero failures, two test warnings
and nine skips. Six skips concern unavailable mirai workers; the other three
are optional development-only ActLumus/snapshot fixtures absent from the package
archive. The two warning scenarios were previously traced to deliberate failed
task and malformed-import recovery checks. No dependency was installed.
Namespace/Rd regeneration on a disposable package copy matched the existing
generated files, leaving the author tree untouched. Local logs are in
`/private/tmp/llw-m7-slice3-work-NYoXPO/package-check-latest.log` and the adjacent
`LightLogWeb.Rcheck/tests/testthat.Rout`.

These are checkpoint checks, not a frozen or accepted build. The exceptional
performance/focus gates, complete final mutation matrix, controlled browser
downloads and independent visual acceptance remain required. Checks must be
rerun as appropriate after approved corrections and before a review freeze.

Programmer browser checks exercised default batch scope, single-item keyboard
selection, validation and recovery, stale-preview refusal, Apply, removal/undo,
discard and committed-export preparation. IZTECH exposes both S004 file-group
occurrences separately. Switching records clears the prior success notice.
The form and preview were visually checked at 1440, 1200, 1024, 768, 390 and
320 pixels. No document-level horizontal overflow was observed; the exact-target
table has its own horizontal scroll. No browser warnings or errors were captured.
These checks do not replace independent reviewer inspection. Temporary viewport
overrides were reset and the task-owned showcase was stopped.

### Owner-approved keyboard-focus correction

After Apply or Discard, native Shiny modal removal leaves focus on the page body
instead of returning to Edit metadata. The owner approved extending the existing
scoped focus-return handler in `inst/app/www/js/metadata-navigation.js`, adding
that one file to this slice's allowlist. The helper remembers the overlay opener and
return focus without scrolling only when this editor closes. Native Shiny would
still own all form events, validation and transactions. No new dependency,
measurement access or general navigation behavior is proposed.

The Shiny-native alternative is an inline editor, which changes the approved
dialog workflow. The existing native modal has no corresponding return-focus
argument. The JavaScript extension was explicitly approved before implementation.
It would have no data payload or external communication. Verification must
exercise mouse and keyboard Apply/Discard, repeated opening, removed openers,
unchanged viewport and the existing breadcrumb-dialog focus behavior. The
maintenance surface is one additional scoped opener/dialog pair in the existing
helper, not a general focus manager.
Owner approval: `APPROVE M7 SLICE 3 FOCUS RETURN CORRECTION`.

### Prior performance checkpoint and approved exceptional commit gate

The formal offline run at `2026-09-21T16:36:29Z` used 60 measured iterations after
10 warm-ups per operation. The committed benchmark report retains prior-slice
timestamps separately. R-side elapsed p95 is not a browser end-to-end or
concurrent-capacity guarantee. No other task-owned R/test/showcase process was
running during this run.

| Case | Measured p95 |
| --- | ---: |
| TUM fixture, slowest tested operation | 16.65 ms |
| IZTECH fixture, slowest tested operation | 15 ms |
| UCR fixture, slowest tested operation | 24 ms |
| Appended TUM/IZTECH, slowest tested operation | 27 ms |
| Metadata-absent record, slowest tested operation | 11 ms |
| Apply 5,000 participant-field assertions atomically | 152.4 ms |
| Initial index attachment, 50,000 source entries and one overlay assertion | 152.65 ms |
| Initial index attachment, 50,000 source entries and 5,000 overlay assertions | 293.5 ms |
| Update one assertion in that already populated wide index | 44 ms |

All measured allocation gates pass. The widest initial index attachment allocates
61.89 MiB against the unchanged 64 MiB limit. The two initial-attachment tests are
index-only stress probes, not library records exceeding the 32 MiB bundle limit.
The full acceptance runner includes the overlay timing and memory gates in its
overall result. That pre-approval run correctly exited unsuccessfully: three
operations exceeded the original 100 ms hard timing gate.

The owner has now approved a separate p95 <= 500 ms gate for
explicit large-inventory commit/index creation and bulk transactions, tested up
to 50,000 source entries plus 5,000 overlay assertions. This includes the first
single assertion when it requires that wide initial index attachment; it is not
presented as a batch-only cost. Keep ordinary navigation, draft input, validation,
preview and warm single-item updates at <= 100 ms, with <= 50 ms as the design
target. Keep the 64 MiB allocation limit and zero post-initialization measurement
decoding/package reopen requirements unchanged. Native busy feedback and duplicate
blocking must appear immediately; atomic failure retains the valid committed
state and retryable draft. The remaining complete mutation matrix still needs
to be measured before any frozen-build acceptance claim.

Owner approval: `APPROVE M7 SLICE 3 LARGE-INVENTORY COMMIT BUDGET`. Each measured
operation records its actual gate class and numeric limit; warm single-item
updates cannot inherit the exceptional bulk/initial-index gate.

The frozen-fixture manifest hash remains
`c228fa293b7c50da120a1416e5e1c2362e48e29806a32945e0bb7da906c61890`.
The fixture, lockfile and generated namespace/manual paths have no tracked
changes. Lintr and spelling checks are unavailable locally;
neither tool was installed. Air formatting and focused tests were run.

### Correction checkpoint: 2026-09-21, not frozen

The approved focus correction is implemented and verified in the real browser.
Keyboard Apply and Discard both return focus to `overlay-edit` without changing
document scrollY (24 before and after). A separate offline handler test covers
repeated dialogs, distinct openers, unrelated dialogs, and disconnected, hidden
or disabled openers. The existing breadcrumb-dialog regression still needs its
final real-browser recheck before freezing.

Index mutations now avoid data-frame row-name repair and unnecessary full table
copies. Source-qualified row keys are cached per context; a unique key component
is only a lookup aid and every qualified key is verified before matching.
Compacting a typed overlay table reuses existing path-order slots only when
source paths are disjoint. Revision-label sorting is reused only when source
labels cannot cross that ordering. Otherwise the conservative full sort remains.
Undo reuses the protected snapshot index, while preserving monotonic revisions.
Regression tests compare incremental edits, removals and undo with a full index
rebuild, including lexical row ordering across row 10 and fallback ordering.

Current focused result: **438 passes, zero failures/errors/warnings/skips**.
Air and `git diff --check` pass. The earlier package/full-suite checkpoint above
predates these final index optimizations and is not final-build evidence.

The complete formal run at `2026-09-21T17:13:00Z` passed memory, navigation,
repeated-crosslink, relationship, lineage and zero-reopen/deserialization gates.
All exceptional large commits met 500 ms; all allocation gates passed (largest
overlay-case allocation 63.62 MiB). It correctly exited unsuccessfully because
single-item removal in the 5,000-variable-occurrence case took 126.65 ms p95,
above its unchanged ordinary 100 ms gate. This run predates the last path-order,
key-match and undo optimizations. Its exact report remains in
`dev/milestone-7-metadata-benchmark.json`; it must not be called a passing freeze.

After those optimizations, the targeted 60-sample, ten-warm-up R check gives
56 ms median and **104.85 ms p95** for that same large-inventory single removal.
Earlier targeted checks of the intermediate candidates measured 101.35 to
119.4 ms p95. The remaining gap is small but the approved hard gate is still
unmet. No limit has been changed or silently reclassified. Freeze, final full
checks, independent acceptance and full application wiring remain paused for
the owner's performance decision. The expanded runner also now explicitly
measures undo of single-item updates and removals; the final full matrix remains
to be rerun.

JSON and CSV preparation was verified in the browser. Their exact observed local
HTTP responses were saved only to
`/private/tmp/llw-m7-overlay-download-test-9f8FNE` and verified in R: revision 1,
five source-qualified participant sex assertions, correct attachment names and
content types, no source records or measurement payloads. JSON is 14,124 bytes;
CSV is 5,834 bytes. This is an HTTP transfer check, **not** proof of a saved
browser download. The browser interface exposes no save-destination method;
native Codex app access was safety-blocked and no workaround was attempted.
Saved-browser-download behavior remains an explicit reviewer/owner check.

The task-owned showcase is stopped. No commit, push, dependency change, fixture
modification, live read or full-app integration was performed. Unrelated user
processes and untracked files remain untouched.

### Owner-requested editor refinement: 2026-09-22, provisional

The owner requested Matching field above Edit scope, a separate selectize target
control, the wording Single item only, and current values before Change. This is
a refinement of the isolated Slice 3 module, not acceptance of the pending
performance exception or authorization for full-app integration.

The form now follows Metadata field, Matching field, Edit scope, Apply to,
Current values, then Change. Matching field identifies the exact variable-name
cohort independently of scope. The default all-items scope has one option with
the affected identities. Single item only offers individual source-qualified
participants, devices, datasets or variable occurrences within that cohort.
Native Shiny server-side selectize supplies both lists; no bespoke JavaScript
or dependency changes were needed. Selection changes invalidate a review.
Empty or obsolete selections cannot be applied and prompt an actionable error.

The information box summarizes current and retained-source values separately,
with occurrence counts, the number of committed overrides and the draft's
committed overlay revision. Missing units say Unit not specified; other missing
values and empty text are distinguishable. Unapplied input is not presented as a
current value. Refresh updates the snapshot without clearing input. An initially
collapsed, paginated table exposes every exact target, current/source value and
provenance. The all-items option lists up to 20 identities, covering the complete
realistic fixture cohorts. Wider synthetic inventories explicitly show the
remaining count and point to the full table; display limits never reduce scope.
Appended-source numbers are consistent between the selector and detailed table.

Changed files for this refinement: `R/m_metadata_overlay.R`,
`inst/app/scss/lightlogweb.scss`,
`tests/testthat/test-metadata-overlay-module.R`, and this note.

Focused checks cover independent cohort/item selection, batch/single switching,
stale preview refusal, empty selections, mixed overrides, refresh, missing and
empty values, source collisions, both IZTECH devices, paired coordinates and
5,000-item lists. At this checkpoint, 496 assertions pass with no failures,
errors, warnings or skips. Air and `git diff --check` pass. The frozen fixture
manifest remains byte-identical.

Programmer browser checks cover keyboard search/selection, the five-item MEDI
batch, a single-participant Apply followed by mixed current/source display,
expanded per-item provenance, repeated opening, and source-qualified IZTECH
AL03/AL04 selection in the appended record. The dropdown overlays the content
without clipping. Checks at the normal desktop viewport, 390 px and 320 px
found no document-level horizontal overflow; the wide details table scrolls
within its container. These are focused correction checks, not independent
acceptance or a complete final browser matrix.

A focused R-side check of scope-choice and current-value helpers used 60 timed
iterations after 10 warm-ups, across all three fixtures, appended and absent
cases, and the populated 5,000-variable-occurrence inventory. The realistic
cases measured 1 ms p95; the widest case measured 53.3 ms p95 and 3.30 MiB of
temporary allocation, within the unchanged 100 ms / 10 MiB gates. This is not an
end-to-end browser or transaction benchmark. The reproducible temporary script
is `/private/tmp/llw-m7-slice3-work-NYoXPO/editor-refinement-timing.R`.
R 4.6.1, shiny 1.14.0, bslib 0.11.0, DT 0.34.0 and testthat 3.3.2 were used.

The earlier warm single-removal timing failure remains unresolved. Full matrix,
package and independent acceptance checks remain pending; this change does not
relax their gates. The showcase is left running for owner feedback, with obsolete
task-owned instances stopped. No commit, push, live reads, fixture modification
or full-app integration was performed.

### Contextual explorer editing: 2026-09-22

The owner accepted the preceding standalone refinement and approved 150 ms p95
for the previously reported warm single removal in the 5,000-variable-occurrence
inventory. Ordinary interactions retain 100 ms, with 50 ms as the design target.
Explicit initial/bulk commits retain their approved 500 ms limit. Memory limits
and zero measurement decoding/package reopening are unchanged. A targeted
60-sample check after ten warm-ups measured 104.15 ms p95, 55 ms median and
123 ms maximum for the newly excepted removal. The runner now records that
exception by operation, and includes contextual source/overlay selection probes.
The earlier failed report is historical, not evidence of current acceptance.
The full rerun below supersedes it as current performance evidence without
erasing the earlier adverse measurements from this log.

The owner additionally requested that the explorer supply the field to edit.
The combined showcase composes the production explorer and overlay modules
through an explicit selection reactive. Edit selected field sits immediately
after the explorer, before selected-field detail. Its stable native button uses
the already approved dialog focus-return helper. No JavaScript was added.
First-time target preparation uses native Shiny progress; subsequent selections
reuse the in-memory context. The modal displays the fixed field, exact source-variable name where relevant,
and raw field name, then starts with Edit scope. It omits Metadata field and
Matching field. The draft snapshots its selection, so navigation or browser
inputs cannot silently retarget an open draft. Single item only defaults to the
focused exact item; all-items scope still covers the matching cohort throughout
the current library dataset, including appended sources.

Mapping is cached with the source index. Canonical retained paths and current
overlay rows resolve to exact typed target identities. Core variable label/unit
aliases require a unique source-qualified file-group/raw-variable join and
agreement of retained values. Ambiguous, conflicting, calibration, identity,
relationship and unmodeled fields remain read-only with an explanation. No
semantic term, similar display label, or overlay row position becomes an entity
identity. Longitude opens the existing paired-coordinate editor. Source values
remain unchanged in the explorer, while the edit toolbar summarizes current
effective values and committed override counts separately.

Overlay-only record revisions now preserve the query, active field and hierarchy
focus, including a sibling selected after a search result. Ordinary record
refreshes and dataset switches still clear stale context. Tests cover exact
source identity across all three public-derived fixtures, nested/appended
sources, same-name collisions, the two IZTECH S004 file groups, conflicting
aliases, protected fields, single/batch scope, forged field inputs, source
immutability and Apply/Undo context retention.

This request changes `R/metadata-overlay.R`, `R/m_metadata_overlay.R`,
`R/m_metadata_explorer.R`, `inst/app/scss/lightlogweb.scss`,
`dev/milestone-7-overlay-app.R`, the three overlay/explorer test files,
`dev/run-milestone-7-metadata-acceptance.R`, its generated evidence JSON,
`project-plan.md` and this note, all within the approved Slice 3 boundary.
Full app/dashboard wiring, dependencies, fixtures and generated documentation
remain unchanged. The active develop-shiny-apps and develop-r-code skills guide
the module seam, source-qualified tests and package checks.

Focused tests: 1,137 assertions, no failures, errors, warnings or skips.
The full local suite: 4,411 assertions, zero failures/errors, two expected
recovery-path warnings and six unavailable-mirai-runtime skips. Package checking
found one unqualified `head()` call in the earlier selector helper; it has been
qualified as `utils::head()` and the next complete package check reported OK
with 4,330 installed-package assertions, two expected warnings and nine
environment/optional-fixture skips. The final complete check, including native
first-time progress, also reports OK with those same counts. Generated namespace
and Rd files are unchanged when regenerated in a disposable package copy.

Programmer browser checks verify: participant sex opens as a fixed field;
TUM_S004-only preview/Apply changes one overlay assertion; query, field and
hierarchy focus survive Apply/Undo; Apply returns focus to the persistent Edit
button with document scrollY unchanged at 950 px; protected participant IDs
disable editing with a visible explanation; an IZTECH core MEDI unit opens the
eleven-occurrence appended-source cohort; single mode starts in IZTECH and
exposes S004:19 and S004:20 separately. Keyboard Discard restores Edit-button
focus. At 390 px and 320 px there is no document-level horizontal overflow.
The browser console has no warnings/errors. These are programmer checks, not
independent acceptance. No commit or full-app wiring is implied.

The full offline matrix at `2026-09-22T08:02:00Z` passes every hard timing,
memory, navigation, repeated-crosslink, relationship and lineage gate. Contextual
source/overlay selection is at most 1 ms p95 in the three public-derived fixtures
and appended case, and at most 27 ms p95 in the two 5,000-occurrence stress
inventories. Its maximum allocation is 6,606,416 bytes, below 10 MiB. Ordinary
overlay operations reach 60.65 ms p95; the explicitly excepted 5,000-variable
single removal measures 64.65 ms in this run, below both 100 and 150 ms. The
earlier targeted 104.15 ms result remains documented above because these are
local timing samples, not deterministic latency guarantees. Large initial/index
and bulk commits reach 201.65 ms p95 against 500 ms. Largest overlay-operation
allocation is 66,125,800 bytes (63.06 MiB), below 64 MiB.

The 50 ms design target is not universal: IZTECH's widest contents render is
52 ms p95, and the explicitly listed overlay commit cases exceed it. All their
unchanged or owner-approved hard gates pass. Ordinary core index actions reach
9 ms p95, hierarchy navigation 23 ms, and full-path rendering 29 ms. Bundle totals
and scaled-index memory remain 6,286,168 and 35,702,368 bytes. Ordinary operations
report zero package reopens and zero complete record deserializations. These are
R-side local measurements, not browser end-to-end or concurrent-user guarantees.
Unrelated Quarto computation was observed during this run and was left untouched.

Final browser checks also cover selecting Participant sex as a hierarchy sibling
after searching for participant_internal_id, keyboard batch Apply, and Undo.
The search remains participant_internal_id while the selected field remains
Participant sex. Native first-time progress was observed. The seven-level
full-path modal opens correctly and Close returns focus to its own ellipsis
button, independently of the overlay dialog opener. Test edits were undone.

The frozen packet is `/private/tmp/llw-m7-contextual-review-8khHPy`, with a
31-file SHA-256 manifest, exact source copies, benchmark report, package logs,
review instructions and explicit remaining checks. The source baseline is
unchanged at `00ecd229f174503996aa91256120074f2b2faf37`; fixtures, dependency
lockfile and generated paths have no tracked changes. Air and
`git diff --check` pass. One task-owned showcase remains on localhost 7381;
obsolete task showcase instances were stopped. No commit, push, dependency
change, live read or full-app integration was performed. Independent visual
acceptance, saved-browser-download verification and owner final check remain
required before a later integration/commit gate.

### Owner-approved R48-R54 correction, 2026-09-22

The independent report for the preceding frozen candidate remains unchanged at
`/private/tmp/llw-m7-contextual-review-8khHPy/reviewer-findings-2026-09-22.md`
(SHA-256 `0ae8313866634afd1c31ea4b4df268f0b218672afe8eabb8022633355b755ff4`).
That candidate was rejected, not accepted. The owner approved the combined
`APPROVE M7 SLICE 3 R48–R54 CORRECTION CONTRACT` before this implementation.

- R48 rejects explicit missingness placeholders, including `not specified` and
  whitespace/case/hyphen/underscore variants, before preview or Apply. Valid
  UCUM and precise custom units remain allowed. Removal restores the source;
  no numerical conversion occurs.
- R49 names the actual inspected layer. Preserved unresolved legacy content
  is identified as unapplied and unbound, not described as a source field. No
  new legacy-resolution workflow is introduced.
- R50 provides a secondary **Add missing annotation** action. Its typed catalog
  offers only fields with no selectable retained-source binding, using existing
  imported identities. It does not map study titles or contributor countries to
  dataset fields, create entities or infer relationships. The snapshotted set
  of eligible targets bounds both preview and atomic Apply. Existing source
  fields remain on the contextual Edit selected field route. Coordinates stay
  paired; if either source coordinate exists, its contextual route handles the
  pair rather than offering a half-pair as a missing annotation.
- R51 uses Bootstrap's scrollable modal layout. The existing, scoped helper
  also remembers the opening page and explorer-list positions, restores them
  after close and returns focus without scrolling. Full-path dialogs keep
  separate ownership and their intentional navigation-to-destination behavior.
- R52 is explicitly **owner-deferred saved-browser-file verification**, not an
  application defect and not a pass. The owner final check must actually save
  the prepared JSON and CSV and verify the saved filenames, revision and
  payloads. Automated typed/unknown-field/CSV round-trip tests remain required.
  No native-app safety workaround, alternative HTTP substitute or additional
  download capability is introduced by this disposition.
- R53 shows effective values as primary in selected source-field rows and
  matching/hierarchy previews. Each changed row names Overlay or Source and
  displays the original source value beside the current one. A sparse,
  source-qualified presentation is passed explicitly from editor to explorer;
  source snapshots, source index facts, search identities and hierarchy IDs
  remain unchanged. Search still searches retained facts and the separately
  indexed committed overlay. Single-occurrence overrides do not propagate to
  siblings or other source scopes. No label-based semantic reconciliation occurs.
- R54 permits Escape. Clean editors close without applying. Dirty input or a
  reviewed draft offers **Discard changes / Keep editing**, preserving values
  and focus on Keep. An open selectize menu consumes its first Escape normally.
  A namespaced browser event requests dismissal only; it cannot request Apply.
  The server continues to own draft state, validation and every transaction.

Native modal behavior was checked against installed Shiny 1.14.0 and bslib
0.11.0 and the official [Shiny modal reference](https://shiny.posit.co/r/reference/shiny/latest/modaldialog.html)
and [Bootstrap modal guidance](https://getbootstrap.com/docs/5.3/components/modal/).
Native `easyClose` alone neither protects unblurred draft text nor clears the
server-side draft. The approved browser helper addresses only that gap and
focus/viewport restoration; no dependency was added or repinned.

All previously fixed budgets remain in force. The complete matrix now measures
effective-value presentation and missing-target discovery as ordinary 100 ms /
10 MiB interactions for realistic and wide inventories. No full-app wiring,
fixture changes, commit, push, deployment or live reads are authorized.

#### Correction verification

The final focused run passes 1,191 assertions with no failures, errors,
warnings or skips. The full local run passes 4,636 assertions with zero
failures/errors, two intentional recovery-path warnings and six unavailable
mirai-daemon skips. The warnings exercise failed ExtendedTask retry and malformed
import recovery. The final `R CMD check --no-manual` is **Status: OK**; installed
tests pass 4,555 assertions with the same two warnings and nine skips (six
daemon skips plus three unavailable optional development fixtures). Namespace
and Rd regeneration on a disposable package copy produces no source changes.
The checked package's changed runtime and test files match the final sources.
Air, JavaScript syntax validation and `git diff --check` pass. Lintr and spelling
are unavailable and were not installed.

Programmer browser checks cover all three public-derived fixtures, the appended
eleven-occurrence MEDI scope, metadata absence and unresolved legacy content.
TUM's batch label edit updates the five primary current values and hierarchy
preview while preserving original labels. Appended single S004:19 editing leaves
S004:20 and other source occurrences unchanged. Invalid unit placeholders retain
the draft. Missing annotations stay within existing imported identities.
Clean Escape closes; dirty Escape offers Keep/Discard and Keep retains unblurred
input. Widths inspected are 1440, 1200, 1024, 768, 390 and 320 px, without observed
page-level horizontal overflow in the inspected states. Browser warnings/errors
were empty at the final targeted check; temporary viewport overrides were reset.

The final navigation helper is version 1.2.2. A bounded observer retains the
page and actual scrolling list through asynchronously replaced output, and
immediately yields to a new user navigation gesture. At 390 by 900 px, the
measured keyboard-open/Apply and clean-Escape routes preserve page scrollY 2028
and inner-list scrollTop 245, returning focus to Edit selected field. The
independent full-path dialog retains its own focus-return behavior. These are
targeted browser observations, not browser p95 or concurrency guarantees.
All three fixture-file hashes match the unchanged manifest.

The final offline matrix is dated `2026-09-22T10:34:53Z` and passes all hard
timing, allocation, navigation, relationship and appended-identity gates.
Effective-value presentation is at most 2 ms p95 for the three fixtures and
appended case, 62.65 ms for 5,000 participant assertions and 74.65 ms for the
5,000-variable inventory. Its largest allocation is 7,054,640 bytes, below
10 MiB. Missing-target discovery reaches 4 ms p95 in stress cases. These stress
presentations do not meet the 50 ms design target; the unchanged 100 ms hard
gate passes. The narrow 5,000-variable single-removal exception measures
68.65 ms p95 against its approved 150 ms gate. Large initial/index and bulk
commits reach 200 ms p95 against 500 ms. The largest mutation allocation is
66,125,824 bytes, below 64 MiB but with limited headroom, which remains a scaling
risk rather than permission to raise the budget. CSV export reaches 81.65 ms
p95 against 1 second.

Core navigation/search/filter/sort/select operations reach 9 ms p95 on the
50,000-entry index; hierarchy navigation reaches 22.65 ms and full-path rendering
29 ms. Combined fixture bundles remain 6,286,168 bytes and the scaled index
35,702,368 bytes. Ordinary operations record zero package reopens and zero full
record deserializations. These are local R-side measurements, not browser
end-to-end latency or multi-user guarantees; earlier samples remain above.

The revised frozen review packet is
`/private/tmp/llw-m7-r48-r54-review-njSxcT`, containing the exact 31-file source
boundary, SHA-256 manifest, checks, benchmark, programmer browser evidence and
R48-R54 review instructions. Twelve files differ from the preceding candidate,
all inside the existing boundary. The baseline and fixture manifest are
unchanged. Independent visual acceptance and the explicit R52 owner saved-file
check remain outstanding; no integration, commit or push has occurred.

### Owner-approved R54 review-focus correction, 2026-09-22

The independent report for the preceding frozen candidate is preserved at
`/private/tmp/llw-m7-r48-r54-review-njSxcT/reviewer-findings-2026-09-22.md`
(SHA-256 `fa9dacfee671396de26fbf7ce40d540a6d5cc9ead1e98c9ee5287c64583771ee`).
Its decision is **REVISION REQUIRED**, not acceptance. R48, R49, R50, R51 and
R53 were visibly closed within the report's stated coverage. R52 remains
owner-deferred actual saved-file verification, **not passed**. R54 remains open:
successful Review released focus to BODY, making native Escape ineffective
until a modal control was focused manually. The owner subsequently approved
`APPROVE M7 SLICE 3 R54 REVIEW-FOCUS CORRECTION`.

The exact correction boundary is seven files: `R/m_metadata_overlay.R`,
`inst/app/www/js/metadata-navigation.js`, `R/m_metadata_explorer.R` (dependency
version only), `tests/testthat/test-metadata-overlay-module.R`, this note,
`dev/milestone-7-metadata-benchmark.json` and `project-plan.md`. The full reviewed
31-file context remains available in the next frozen packet, but changes from
the preceding packet must be confined to these seven files. No typed metadata,
fixture, transaction, export format, source-binding or integration contract
changes are included.

The installed bslib 0.11.0 task button intentionally becomes disabled while
busy and resets after reactive processing, consistent with the official
[task-button contract](https://rstudio.github.io/bslib/reference/input_task_button.html).
Disabling it can release browser focus to BODY. Native progress and duplicate
blocking are preserved. Review now schedules an owned focus request after the
server flush, on success, repeated/unchanged previews and validation failure.
The existing helper waits for the enabled Review button and settled Shiny state.
It never supersedes a newer confirmation request or a user's different focused
control in the modal. Requests expire after one second; dismissal clears them.

Escape locates the active visible owned editor even when focus has fallen to
the page. It still ignores unrelated modals and lets an open selectize menu
consume the first Escape. The server alone decides clean dismissal versus
Keep/Discard; browser events cannot Apply. Keep restores a valid draft control,
including a fallback for page-focus loss. The helper version is 1.2.3. Existing
full-path ownership and viewport-return behavior are unchanged.

The unchanged performance budgets apply: ordinary p95 100 ms with a 50 ms
design target, the explicitly approved 150 ms warm single-removal exception,
500 ms initial/bulk work, 10 MiB ordinary presentation allocation and 64 MiB
mutation/export allocation. Browser focus recovery is separately verified by
native keyboard interaction, not claimed as an R-side timing result.

#### Review-focus verification

Focused tests pass 1,219 assertions without failures, errors, warnings or skips.
The full local suite passes 4,664 assertions, with zero failures/errors, two
intentional recovery-path warnings and six unavailable mirai-daemon skips.
`R CMD check --no-manual` is **Status: OK**; installed tests pass 4,583 assertions
with the same two warnings and nine skips (six mirai and three optional-fixture
skips). Namespace and Rd regeneration on a disposable package copy is unchanged.
The checked runtime/test sources match the candidate. Air, JavaScript syntax and
`git diff --check` pass. No unavailable linting tools were installed.

The programmer first reproduced the previous candidate's BODY focus and failed
native Escape at 1024 x 900, then tested the correction with the identical
IZTECH MEDI unit, single-item, custom-unit route. Pointer Review and native
Tab/Return Review now return focus to Review when ready. Native Escape, without
intervening refocus, opens confirmation with focus on Keep editing. Repeated,
unchanged and invalid-unit Review preserve usable focus. Keep retains draft
text and preview; second Escape resumes the draft; explicit Discard leaves the
committed revision unchanged. An open selectize menu consumes only its first
Escape. These assertions also cover 390 px batch editing and 320 px missing
annotations, with no observed page/modal horizontal overflow. Explicit Apply
updates the effective values beside their retained originals, and Undo restores
them. The independent full-path dialog still closes to its own opener.

The browser warning/error log was empty at the final targeted check. Temporary
viewport overrides and test tabs were cleared, and the temporary 7382 process
was stopped. These are targeted programmer observations, not independent
acceptance, universal viewport invariance, browser p95 or concurrency evidence.

The refreshed offline matrix is dated `2026-09-22T11:32:39Z`. All hard timing,
allocation, relationship, navigation and appended-identity gates pass. The
three public-derived fixtures and appended inventory present effective values
within 2 ms p95. Stress presentation reaches 70.65 ms for 5,000 participant
assertions and 73 ms for 5,000 variables, above the 50 ms design target but
within the unchanged 100 ms hard gate. Maximum presentation allocation remains
7,054,640 bytes, below 10 MiB. Missing-target discovery reaches 4 ms p95.
The approved 150 ms single-removal case measures 69 ms; initial/bulk work
reaches 203 ms against 500 ms. Maximum mutation allocation remains 66,125,824
bytes against 64 MiB (67,108,864 bytes), leaving limited scaling headroom.
CSV export reaches 80 ms p95 against one second. No budget was increased.

Core navigation/search/filter/sort/select operations reach 9 ms p95 on the
50,000-entry inventory, hierarchy navigation 23 ms and full-path rendering
29 ms. Combined fixture bundles remain 6,286,168 bytes and the scaled index
35,702,368 bytes. Ordinary operations record zero package reopens and zero
full-record deserializations. Fixture hashes and the pinned glcdp SHA are
unchanged. The matrix is local R-side evidence, not browser or multi-user p95.

The new exact packet is `/private/tmp/llw-m7-r54-review-17QJZ3`, including its
31-file SHA-256 manifest, the seven-file change comparison, unchanged fixture
checks, complete logs, benchmark and native keyboard/browser observations.
Independent acceptance is pending. R52 remains explicitly owner-deferred,
not passed. Full-app integration, commit and push are still separately gated.

### Owner-approved R55 full-path Close correction, 2026-09-22

The independent report at
`/private/tmp/llw-m7-r54-review-17QJZ3/reviewer-findings-2026-09-22.md`
(SHA-256 `d1deb0ed86f3058096470ed9eab2cd4af59f16a0b79675de3286ae1a06dc1aef`)
closes R54 on visible original-path and adjacent-state evidence, but requires
revision for the separate R55 finding. At 1024 px, native keyboard activation of
the full-path dialog's Close returned focus to an off-screen ellipsis and moved
the page below the explorer. It reproduced in a fresh UCR session and at 390 px.
Its introduction point is not established. Pointer Close and Escape worked in
the reviewed states. The report and previous frozen packet remain unchanged.
The owner approved `APPROVE M7 SLICE 3 R55 FULL-PATH CLOSE CORRECTION`.

The exact seven-file boundary is `R/m_metadata_explorer.R`,
`inst/app/www/js/metadata-navigation.js`,
`tests/testthat/test-metadata-explorer.R`,
`tests/testthat/test-metadata-overlay-module.R` (dependency-version assertion
only), this note, `dev/milestone-7-metadata-benchmark.json` and `project-plan.md`.
No overlay editor R logic, typed allowlist, source/overlay binding, metadata,
measurement, fixture, export, integration or dependency contract changes.

The native-first trial enabled Shiny's default modal fade, as documented by the
[official modal reference](https://shiny.posit.co/r/reference/shiny/latest/modaldialog.html)
and confirmed in installed Shiny 1.14.0. It did not correct the observed scroll
jump and was reverted. Final native Close, Escape, modal arguments and the
server's validated hierarchy-navigation behavior are unchanged. The only final
explorer R change is helper dependency version 1.2.4.

The existing helper now distinguishes dismissal from choosing a path level.
Close, Escape and backdrop dismissal retain the invoking page/list viewport
through the existing bounded settling period. Native Close is recognized from
its initiating pointer/keyboard gesture because Bootstrap can hide the modal
before a later click listener runs. A path radio change clears that dismissal
intent, so the server's validated ancestor jump continues to position and focus
the destination without restoring the old viewport. No new R/browser message,
server round trip or metadata indexing is added.

Restoration yields to a new navigation gesture and releases its temporary
observer/listeners after settling, with a one-second upper bound. A native
closing keydown already in flight does not cancel its own restoration; its
creation timestamp is compared with the restoration start, following the
documented [Event timeStamp time-origin contract](https://developer.mozilla.org/en-US/docs/Web/API/Event/timeStamp).
This is transient client positioning, not a busy or blocked user operation.
All existing R54 focus/Escape semantics and independent modal ownership remain.

Existing budgets are unchanged: ordinary p95 100 ms, design target 50 ms;
approved stress single-removal exception 150 ms; initial/bulk 500 ms; export
one second; ordinary presentation allocation 10 MiB and mutations/exports
64 MiB. The correction must add no package reopening or full-record
deserialization. Browser positioning is tested separately and is not claimed
as an R benchmark, browser p95 or multi-user guarantee.

#### Full-path Close verification

Focused tests pass 1,239 assertions with no failures, errors, warnings or skips.
The full local suite passes 4,684 assertions with zero failures/errors, two
intentional recovery-path warnings and six unavailable mirai-daemon skips.
`R CMD check --no-manual` is **Status: OK**; installed tests pass 4,603 assertions
with the same two warnings and nine skips (six mirai and three optional-fixture
skips). Namespace and Rd regeneration on a disposable copy is unchanged.
Air, JavaScript syntax and `git diff --check` pass. No dependencies or unavailable
linting tools were installed. The exact runtime and test files are checked
against the successful package build before freeze.

The programmer reproduced the reported off-screen return on the previous
candidate before testing the correction. At 1024 px, the fresh UCR core MEDI
label route now retains the invoking viewport and visible ellipsis focus after
native Shift+Tab to Close and Return, including repeated keyboard-only opening
and closing. Pointer Close, native Escape and backdrop dismissal preserve the
same context. Short and long paths were exercised at 1440, 1200, 1024, 768, 390
and 320 px without observed page-level horizontal overflow. Choosing an actual
ancestor still navigates and focuses the intended explorer destination. A
deliberate native End after Close is not undone by the settling callback.

The protected IZTECH single MEDI-unit R54 route retains Review focus,
Escape/Keep/Discard behavior and draft text; subsequent full-path dismissal
returns to its own ellipsis. Final browser warnings/errors were empty. These
are targeted programmer observations, not independent acceptance or universal
viewport invariance. One early automated attempt sent keys before the modal
was ready and is explicitly excluded; final routes verify the visible ready
dialog and Close focus before activation. Temporary tabs and viewport overrides
were cleared, and superseded task-owned showcase processes were stopped.

The fresh offline matrix is dated `2026-09-22T12:42:47Z`. All hard timing,
allocation, relationship, navigation and appended-identity gates pass. The
three public-derived fixtures and appended inventory present effective values
within 2 ms p95. Stress presentation reaches 66 ms for 5,000 participant
assertions and 77.65 ms for 5,000 variables. The widest IZTECH contents render
reaches 52 ms. These and some large warm mutations exceed the 50 ms design
target; the unchanged ordinary 100 ms hard gate passes. Maximum presentation
allocation remains 7,054,640 bytes, below 10 MiB. Missing-target discovery
reaches 4 ms p95.

The approved 150 ms stress single-removal case measures 65.65 ms p95. Large
initial/index and bulk work reaches 200.65 ms against 500 ms. Maximum mutation
allocation is 66,125,824 bytes against 64 MiB (67,108,864 bytes), retaining the
limited scaling headroom noted in earlier candidates. CSV export reaches
83.3 ms against one second. No budget was increased and no unfavorable sample
was replaced. The full matrix, including maximum observed timings, is retained.

Core navigation/search/filter/sort/select operations reach 10 ms p95 on the
50,000-entry inventory, hierarchy navigation 23 ms and full-path rendering
29 ms. Combined fixture bundles remain 6,286,168 bytes and the scaled index
35,702,368 bytes. Ordinary operations record zero package reopens and zero
full-record deserializations. Fixture hashes and the glcdp pin are unchanged.
These are local R-side measurements, not browser end-to-end or multi-user p95.

The new exact packet is `/private/tmp/llw-m7-r55-review-X2yYmu`. It contains the
31-file source snapshot, SHA-256 manifest and seven-file change comparison,
unchanged fixture checks, focused/full/package evidence, fresh benchmark and
targeted native keyboard/browser observations. Independent R55 acceptance is
pending. Existing finding closures retain their reported scope, and R52 actual
saved-browser JSON/CSV verification remains owner-deferred, **not passed**.
Full-app integration, later slices, commit and push remain separately gated.

### Owner-approved R56 filter-stability correction, 2026-09-22

The independent R55 report at
`/private/tmp/llw-m7-r55-review-X2yYmu/reviewer-findings-2026-09-22.md`
(SHA-256 `43c3cb26bf3e70c98f16028e859f35969533d402ea4b76f724c2dc8cc89e10bd`)
closes R55 and protects R54, but requires revision for medium R56. In the
appended fixture, after single S004:19 MEDI-unit Apply, a TUM source switch and
Undo, return to IZTECH and full-path Close, rapid All sources / overlay layer /
Clear search produced sustained 3/741-field alternation and faded redraw.
The reviewer observed 90 seconds and another fresh run of at least 36 seconds.
No crash, corruption, introduction point or simpler minimal trigger is claimed.
The owner approved `APPROVE M7 SLICE 3 R56 FILTER-STABILITY CORRECTION CONTRACT`.

Exact boundary: `R/m_metadata_explorer.R`,
`inst/app/scss/lightlogweb.scss`, `tests/testthat/test-metadata-explorer.R`,
`dev/run-milestone-7-metadata-acceptance.R`,
`dev/milestone-7-metadata-benchmark.json`, this note and `project-plan.md`.
Comparison is against the preceding 31-file frozen candidate, not HEAD, because
the uncommitted Slice 3 work predates this correction. No JavaScript, overlay
transaction/schema, source record, fixture, dependency or full-app wiring change.

Read-only triage established that the old filter row's renderUI depended on its
own source and layer selections and recreated those same inputs. A bounded
server-message model showed stale replacement rows sustaining alternating
responses. This was a model, not a browser reproduction or proof of exact
delivery order. The final correction permanently mounts the four native
selectors and uses targeted updateSelectInput messages only when a choice set
changes or a new library/source identity requires a reset. Ordinary source and
layer selections never reconstruct or write back those same controls. The
existing cached section index supplies dependent choices. Native conditionalPanel
owns relevance visibility, and a scoped CSS grid preserves responsive layout.
No bespoke browser code is added; the R54/R55 helper stays byte-identical.

Clear search changes only the query. Valid source, layer, section and sort
choices survive it. An invalid dependent section falls back once to All.
Library/source identity changes reset context once, while same-source overlay
Apply/Undo retains valid filters and selection. Stable source-qualified target
identity and immutable source/overlay separation are unchanged.

New deterministic tests model rapid selections and delayed echoes, assert that
source/layer/sort controls are not rewritten, cover query-only clearing,
irrelevant-section fallback, exact appended Apply/Undo and dataset reset.
An initial browser test caught inline display:none overriding Shiny 1.14's shown
class; it was removed before final verification and guarded by a regression
assertion. Native visibility now works for complete, partial, absent and
appended inventories. Official native-control guidance remains
[dynamic UI](https://shiny.posit.co/r/articles/build/dynamic-ui/) and
[input updates](https://shiny.posit.co/r/reference/shiny/latest/updateselectinput.html).

#### Filter-stability verification

Focused tests pass 1,294 assertions without failures, warnings or skips. The
full suite passes 4,739 assertions with zero failures/errors, two intentional
recovery-path warnings and six unavailable mirai-daemon skips. Package check is
**Status: OK**, with 4,658 installed-test assertions, the same two warnings and
nine skips (six mirai and three optional-fixture cases). Namespace and Rd
regeneration on a disposable package copy is unchanged. Air, JavaScript syntax
and git diff whitespace checks pass. No unavailable tools were installed.

At 1200 x 900 the programmer's original R56 route settles to revision 3, all
sources, overlay layer, empty query and three results. With no further input,
observations at 32.323, 85.396 and 115.631 seconds show unchanged state, no busy
class and zero recalculating outputs. A fresh-session repeat after verified
full-path Close stays stable for another 72.423 seconds. Repeated bursts,
query-only clear, valid-section retention, invalid-section recovery and
empty-state recovery also pass. One over-batched automation sent filter actions
in the same call as modal dismissal without verifying dismissal first; its Clear
did not take effect, and that attempt is excluded from successful route evidence.
The packet records this limitation rather than inferring a new product cause.

Protected R54 keyboard Review/Return/Escape reaches ready preview and focused
Keep editing confirmation at 1024 px. Discard leaves the revision unchanged.
Protected R55 keyboard full-path Close returns to its visible opener for
appended IZTECH at 1200 px and UCR at 390 px; intentional ancestor selection
still reaches Datasets/Core Metadata. Complete TUM, partial IZTECH, all-variable
UCR, appended and absent states retain appropriate controls and fallbacks.
The persistent filter grid fits 1440, 1200, 1024, 768, 390 and 320 px with
appropriate wrapping/stacking and no observed page-level horizontal overflow.
Final browser warnings/errors are empty; temporary tabs, viewport overrides and
the test server were cleaned up. These are targeted programmer checks, not
independent acceptance or universal viewport/concurrency guarantees.

All existing budgets remain unchanged: ordinary p95 <=100 ms, design <=50 ms;
approved stress warm single removal <=150 ms; initial/bulk <=500 ms;
exports <=1 second; ordinary allocation <=10 MiB; mutation/export <=64 MiB.
The refreshed driver adds single filter-choice preparation and a bounded
eight-choice burst for all three fixtures, appended and scaled 50,000-entry
inventories. The burst allocation is checked against the existing 10 MiB
ordinary budget, without granting any filter timing exception.

The fresh matrix is dated `2026-09-22T14:01:48Z` (report 2.8.0). All hard timing,
memory, navigation, relationship and appended-identity gates pass. Single filter
choice preparation is <=1 ms p95 on all three fixtures/appended and 9.65 ms on
the scaled inventory. Eight-choice bursts are 2 ms TUM, 4 ms IZTECH, 9 ms UCR,
4 ms appended and 30 ms scaled. Maximum burst allocation is 7,327,552 bytes,
below 10 MiB. This measures local R-side preparation, not browser delivery or
end-to-end latency. No ordinary package reopening or record deserialization.

The broader ordinary maximum is 74.3 ms p95 for 5,000-variable effective
presentation; participant presentation is 63.3 ms and widest IZTECH contents
51 ms. Some large warm mutations also exceed the 50 ms design target, without
breaching 100 ms. The approved stress single removal is 64 ms against 150 ms;
initial/bulk work reaches 238 ms against 500 ms, and CSV export 80.65 ms against
one second. Maximum overlay presentation allocation remains 7,054,640 bytes.
Mutation allocation is 66,125,824 bytes against 64 MiB (67,108,864 bytes), with
limited scaling headroom. Combined bundles remain 6,286,168 bytes and the scaled
index 35,702,368 bytes. Full samples including slower maxima are retained; no
budget was increased or unfavorable sample replaced.

The exact packet is `/private/tmp/llw-m7-r56-review-wIRlAo`, with a 31-file frozen
snapshot, SHA-256 manifest and exact seven-file comparison, unchanged fixture
hashes, complete tests/package/benchmark evidence and targeted browser notes.
The checked package's runtime/test sources match the candidate. Independent
acceptance is pending and permits owner final check only. R52 remains explicitly
owner-deferred, not passed. No full-app integration, later slice, commit or push.

### Owner-approved R57 guarded Section delivery, 2026-09-23

The independent R56 report at
`/private/tmp/llw-m7-r56-review-wIRlAo/reviewer-findings-2026-09-22.md`
closes the original sustained-refresh symptom and protects R54/R55. It opens
medium R57: in the fresh appended fixture, immediately choosing IZTECH and then
Core Metadata could visibly accept Core and subsequently replace it with All
sections. The source's valid Core choice persisted when selected after the
source response settled. No corruption or introduction point was established.

The owner first approved `APPROVE M7 SLICE 3 R57 DEPENDENT-SELECTION STABILITY
CONTRACT`. The read-only native-first audit is retained at
`/private/tmp/llw-m7-r57-native-audit-OPoTmn`. Its deterministic server-message
model shows that a context-equality acknowledgment is insufficient for an
A-to-B-to-A ordering. It is not a browser reproduction or a claim that every
native design is impossible. A stable complete Section menu would avoid the
replacement race, but change the approved relevance and normalization contract.
The audit stopped without production edits for the separate browser-code gate.
The owner then approved `APPROVE M7 SLICE 3 R57 GUARDED SECTION UPDATE JAVASCRIPT`.

The eight-file boundary is `R/m_metadata_explorer.R`,
`inst/app/scss/lightlogweb.scss`, `tests/testthat/test-metadata-explorer.R`,
`dev/run-milestone-7-metadata-acceptance.R`,
`dev/milestone-7-metadata-benchmark.json`, this note, `project-plan.md`, and
`inst/app/www/js/metadata-navigation.js`. On 2026-09-23 the owner additionally
approved exactly one line in `tests/testthat/test-metadata-overlay-module.R`:
the dependency-version assertion changes from 1.2.4 to 1.2.5. This is the ninth
and only additional file. No other line in that test file changes from R56.

The native Section selector remains mounted. Each relevant browser Source or
layer change immediately disables it and announces Updating sections, while
leaving the parent controls usable. A per-module monotonic generation identifies
the interaction even when the parent values return to the same context. R
validates the namespaced request against the current in-memory choice inventory
and returns the dataset epoch, generation, choices and retained valid selection.
Only the current pending reply can update the options, once, before enabling
Section. Old or duplicate replies cannot overwrite a later ready choice.

Library/source identity resets establish a new epoch once. Same-source overlay
Apply/Undo preserves a valid section; invalid sections normalize once to All.
Clear remains query-only, and sort/reverse preferences remain native. Crosslink
selection uses the same guarded Section path and normalizes hidden single-source
parent controls to their available options. No hierarchy, overlay transaction,
source record, fixture, dependency pin, export or full-app wiring is changed.
The browser asset version is 1.2.5 for cache invalidation, not a new dependency.

The protocol uses native disabled semantics, a separate live-status region,
escaped option text and a scoped Retry sections button. A retry that still owns
focus returns to the ready Section, or a visible parent if Section is irrelevant;
it does not steal focus after the user has moved elsewhere. Disconnect disables
Section truthfully and reconnect requests an epoch resynchronization. There is
no fixed delay, external script, shared cross-session state or unguarded native
Section update. The existing R54/R55 focus/viewport handlers are unchanged apart
from registering this independent scoped handler.

New pure and testServer checks cover validation, generations, epoch changes,
invalid-section recovery, hidden controls, duplicate replies, appended identities
and the absence of unguarded Section messages. The shipped JavaScript is also
executed in a deterministic temporary Node DOM/message harness for A-to-B-to-A,
late/duplicate replies, reset, error/retry, disconnect/resync, independent module
instances and escaped labels. Node uses an existing optional test runtime; no
runtime app dependency or package installation is introduced. This is protocol
unit evidence, not real-browser reconnect, latency or assistive-technology proof.

The cached filter inventory contains only distinct namespace/layer/section
choice rows. It does not flatten source metadata or reconcile any values. The
benchmark now includes guarded reply preparation and JSON serialization in its
eight-request burst across the three fixtures, appended inventory and scaled
50,000-entry inventory. Its retained size is counted with the existing section
index against the unchanged 16 MiB limit. Ordinary p95 remains 100 ms, design
target 50 ms and allocation 10 MiB. Existing 150 ms stress-removal, 500 ms
initial/bulk, one-second export and 64 MiB mutation/export gates are unchanged
and do not authorize a filter exception. Ordinary package reopening and full
record deserialization remain prohibited.

#### Guarded Section verification

Programmer browser notes are retained at
`/private/tmp/llw-m7-r57-work-jQiIyw/browser-checks.md`. The original rapid
Source-to-Section route now exposes native pending unavailability and accepts
Core only when ready, retaining 253 IZTECH fields. A-to-B-to-A and layer reversal
samples settle without a ready choice being overwritten. The protected R56
single S004:19 Apply/TUM switch/Undo/full-path Close/filter burst retained the
correct source-qualified values and stayed stable without input for 74.596
seconds. R54 keyboard Review/Escape/Keep/Discard and R55 keyboard Close/Escape
plus intentional ancestor selection were sampled. Complete, partial, widest,
absent and appended states, query-only Clear and six responsive widths were
checked without observed page-level horizontal overflow. Final console warnings
and errors were empty. Temporary browser tabs and viewport overrides were
cleared, and superseded task-owned servers were stopped before benchmarking.

The fresh complete matrix is dated `2026-09-23T07:58:07Z`, report 2.9.0.
Every hard timing, memory, navigation, relationship and appended-identity gate
passes. Single choice preparation and guarded reply preparation each reach
1 ms p95 across all five inventories. Eight-request bursts, now including
guarded replies and serialization, reach 11/11/13/11/12 ms for TUM/IZTECH/UCR/
appended/scaled. Maximum burst allocation, conservatively including choice
inventory construction, is 3,543,352 bytes against 10 MiB. The largest retained
section index plus choice inventory is 1,663,440 bytes against 16 MiB.

The broader ordinary maximum is 83.3 ms p95 for 5,000-variable effective
presentation; 5,000-participant presentation is 67 ms, and widest IZTECH contents
52 ms. Some large warm mutations also exceed the 50 ms design target, but all
ordinary hard gates remain below 100 ms. The approved stress single removal is
70.65 ms against 150 ms. Large initial/index and bulk work reaches 208.65 ms
against 500 ms, and CSV export 81 ms against one second. Maximum presentation
allocation is 7,054,640 bytes; mutation allocation remains 66,125,824 bytes
against 64 MiB (67,108,864 bytes), with limited scaling headroom. Combined
fixture bundles are 6,286,168 bytes and the scaled index 35,702,368 bytes.
Ordinary package reopening and full-record deserialization counts are zero.
This is local R-side evidence, not browser or multi-user p95. Slower individual
maxima remain in the report; no budget was changed or unfavorable run replaced.

Before the one-line boundary extension, focused tests recorded 1,390 passes
and one stale-version assertion failure; a preliminary full run recorded 4,835
passes and the same failure. The assertion was not skipped or silently changed.
After the owner's approval, final focused tests pass 1,391 assertions with no
failures, errors, warnings or skips. The final full suite passes 4,836 assertions
with no failures/errors, two intentional recovery-path warnings and six
unavailable mirai-daemon skips. `R CMD check --no-manual` is **Status: OK**;
installed tests pass 4,755 assertions with the same two warnings and nine skips
(six mirai and three optional-development-fixture cases). Repository-index
lookups were unavailable in the check environment and are recorded in the log;
no dependency installation, repin or live package/measurement read was performed.
Namespace and Rd regeneration on a disposable package copy is unchanged.

Air checks of touched R files, JavaScript syntax and git diff whitespace checks
pass. A 31-file comparison against the R56 manifest confirms exactly the nine
approved files changed. HEAD, the fixture manifest and all three payload hashes,
and the glcdp pin remain unchanged. The ninth file differs by the approved
version assertion only. Runtime/test sources match the successful checked
package before freeze. Unavailable lintr and spelling tools were not installed.
All superseded task-owned servers were stopped; one exact showcase is retained
for independent review. The unrelated renv watchdog is not touched.

The exact packet is `/private/tmp/llw-m7-r57-review-MG8nrU`: 31-file source
snapshot, SHA-256 manifest, exact nine-file comparison, checked-source and fixture
identity checks, complete logs, fresh benchmark, native-first audit and targeted
browser evidence. The permission reviewer initially blocked a working-status
notification; after explicit owner approval, that exact status message was
delivered to the existing independent reviewer. No alternate channel was used.
Independent acceptance of this new candidate is pending and permits owner final
check only. R52 saved-browser JSON/CSV remains owner-deferred, not passed.
Integration, later slices, commit and push remain separately gated.

## Owner consolidation and export removal, 2026-09-23

Following R57 acceptance for owner final check, the owner requested:

- A borderless help icon, retaining its accessible name, 44-pixel target,
  keyboard focus and native popover behavior.
- Removal of the separate Edit LightLogWeb metadata card.
- Placement of Undo latest metadata edit and its feedback inside the existing
  LightLogWeb overlay card.
- Removal of standalone overlay JSON/CSV export from the code base, not merely
  hiding its controls. No separate external review for these refinements.

The editor UI now supplies a namespaced controls fragment through an explicit
overlay-actions slot. It does not reach into another module's reactive state.
Undo retains its existing latest-history guard and atomic callback. Empty or
unrelated history does not offer Undo. Both the combined and standalone module
showcases use the shared source/overlay summary rather than a second editor card.

The export observers, state, download handlers, filenames, JSON/CSV serializers,
export-only type encoder, and performance probes are removed. Obsolete export
tests are removed or replaced with absence checks and exact typed session-record
recovery checks. JSON use for source-qualified identities and fixture/tool
manifests remains required. Immutable sources, measurements, overlay versioning,
legacy/unknown value retention and Milestone 6 recovery are unchanged. R52 is
superseded by feature removal, not recorded as a successful browser-download test.

Local verification:

- Focused overlay tests pass. A new placement assertion initially used the
  whole tag tree instead of the selected card; the corrected assertion passes.
- The full test suite passes without failures. Six existing mirai-daemon tests
  are skipped, and two expected recovery-path warnings remain.
- Formatting and whitespace checks pass. The modified acceptance driver parses.
- In-browser TUM batch Apply and keyboard Undo update the overlay summary and
  restore source values. Undo and feedback are inside the overlay card; the
  redundant card and export controls are absent. Dataset switching clears old
  feedback and preserves the appended/metadata-absent cases. Desktop and
  390-pixel mobile checks show no horizontal overflow; no browser warnings or
  errors were captured. Test assertions were undone and the viewport reset.
- Focused R summary-render measurements cover all three fixtures, appended and
  metadata-absent cases. p95 is 8-9 ms and allocation is at most 61,632 bytes,
  within the unchanged 50 ms design / 100 ms hard and 10 MiB allocation gates.
  This is server-render evidence, not browser latency or a new full benchmark.

Evidence: `/private/tmp/llw-m7-owner-consolidation-bX71WK`. Package check and the
full benchmark matrix are not rerun for this refinement; earlier packets and
benchmark timestamps remain historical. No JavaScript, dependency, fixture,
generated namespace/manual, commit or push change is included.

The owner session on port 7381 is retained unchanged because restarting it would
discard its current demo edits. A separate updated preview on port 7382 is kept
for owner feedback. No other new Shiny instance is needed. Replacing the old
showcase, full-app integration and commit still require their respective gates.

## Owner toolbar refinement, 2026-09-23

The owner approved the clearer Add missing metadata name and requested both
authoring buttons in the explorer's One level up toolbar, rather than a
selection-status row between the explorer and field detail. This change goes
directly to the owner without external review.

The existing field-actions slot now places both namespaced controls in the
toolbar. The authoring shell stays mounted, retaining its opener elements for
keyboard focus return after Apply or dismissal. The read-only explorer keeps
its existing conditional shell. With authoring enabled, metadata-absent datasets
still show the toolbar and an honest empty state; Add missing metadata remains
available for existing imported identities. No identities or source facts are
invented. Native tooltips distinguish selection-bound editing from the
library-dataset-wide missing-metadata route. The separate selection-status
output and its duplicate value-summary computation are removed.

Focused explorer and overlay-module tests pass without failures, warnings or
skips. New assertions cover toolbar placement, unique namespaced controls,
absence of the old status row and the metadata-absent context. Air and whitespace
checks pass. Browser checks on port 7383 confirm the three controls share one
desktop row, wrap at 390 pixels without horizontal overflow, and have 47-pixel
button heights. Tab shows a visible focus indicator. Contextual editing still
preselects participant sex, batch Apply updates all five intended occurrences,
and focus returns to the moved Edit button. Undo restores the disposable test
edit. In the metadata-absent fixture, the renamed Add action opens without a
selected source field and Escape returns focus to that button. No console
warning or error was captured. The temporary viewport override was reset.

The narrow toolbar-shell render probe uses 10 warmups and 100 repetitions:
p95 6 ms, maximum 25 ms, 113,216 allocated bytes, within the unchanged 50 ms
design / 100 ms hard and 10 MiB allocation budgets. This is R-side presentation
evidence, not a new full-inventory or browser benchmark. Broader suite, package
and full performance checks remain deferred during owner-directed rapid layout
iteration, as required by the active development skill.

Owner evidence is `/private/tmp/llw-m7-toolbar-owner-hZskl3`; the preview is
`http://127.0.0.1:7383/`. Older ports 7381 and 7382 retain the owner's edited
sessions pending an explicit cleanup decision. The failed sandbox startup
exited; only the successfully authorized fresh preview is newly running.
No bespoke JavaScript, dependency, fixture, serialization or transaction
semantics changed. Full-app integration, commit and push remain separately gated.

## Owner source-variable selector refinement, 2026-09-23

The owner agreed to remove the redundant visible Matching field choice for
non-variable metadata. Its only distinct role is choosing the exact source
variable when authoring Variable label or Variable unit. The visible label is
now Source variable and its choices use the exact raw source names.

A native namespaced Shiny conditional panel hides that control for all other
fields. Its existing internal anchor and server validation stay intact, so
there is no change to cohort membership, appended-source identities, the
all/single scope, stale-input guards, or contextual explorer editing. The
validation message no longer refers to the removed Matching field label.
No bespoke JavaScript was added.

Focused overlay-module tests pass without warnings or skips, including variable
MEDI/LIGHT separation, exact-item changes and preview invalidation. The new test
initially expected six IZTECH country targets instead of five distinct datasets
and inspected a nonexistent selectize options payload; those test assumptions
were corrected, without changing runtime targeting logic.

Browser checks verify Country has no additional selector, keyboard Tab proceeds
directly to Edit scope, and Single item only previews exactly MELIDOS_TUM_S003.
That disposable preview was discarded without Apply. The metadata-absent case
retains Source variable for Variable label/unit (MEDI), hides it again for
Country/Participant sex, and retains correct current-value/target context after
repeated switching. A 390 x 844 check has no horizontal overflow; viewport was
reset. Escape returns to the toolbar opener and no browser warnings/errors were
captured. An automation click immediately after viewport restoration failed;
the settled page was inspected and the native keyboard action succeeded.

The owner explicitly approved restarting port 7383 and clearing its temporary
demo edits. It now serves the updated build (PID 58996). The old PID 55825 and
temporary test listener on 7384 (PID 58823) were stopped after exact-process
verification. Older edited sessions on 7381 and 7382 remain untouched as agreed.
The Country editor is left open for the owner's check, with no test commits.

Evidence: `/private/tmp/llw-m7-source-variable-owner-d8xG2P`. This is another
owner-directed rapid refinement, with no external review. Full suite, package
checks and full benchmarks are deferred; previous measurements are historical.
Performance budgets and data/index work are unchanged, and no new latency claim
is made. Full-app integration, commit and push remain separately gated.

## Owner acceptance and final verification, 2026-09-23

The owner accepted the current showcase and requested: "approved. wrap up the
slice and commit". This closes the production-module Slice 3 owner check and
authorizes its commit, including the accumulated R48-R57 corrections and the
later owner-only refinements. No further external review was requested or sent.
The active development skills required leaving rapid-refinement mode and running
the deferred quality gates once for the accumulated change set.

Final evidence: `/private/tmp/llw-m7-slice3-final-e6rfX4`. Its manifest freezes
the 16 commit files, unchanged fixture hashes, checked runtime/test source
identity, package versions, exact glcdp pin and the preceding commit
`00ecd229f174503996aa91256120074f2b2faf37`. The package was built from a disposable
copy containing tracked files plus the six authorized new files. Unrelated
`dev/brand/candidates/**` and `man/man.Rproj` were neither included nor changed.

### Accepted scope and file boundary

The accepted module provides typed version-2, source-qualified overlay editing;
current-library-dataset batch and exact single-item targeting; atomic Review /
Apply / Remove / Undo; effective-versus-source values; contextual explorer
editing; the independent Add missing metadata route; and the corrected native
navigation/filter/focus behavior. Undo is inside LightLogWeb overlay, authoring
buttons are inside the explorer toolbar, and only variable label/unit editing
shows the distinct Source variable selector. Standalone overlay JSON/CSV export
is removed. Record/session recovery and immutable source metadata are preserved.

The commit boundary is `DESCRIPTION`; `R/metadata-model.R`,
`R/metadata-overlay.R`, `R/m_metadata_overlay.R`, `R/m_metadata_explorer.R`,
`R/session-model.R`; `inst/app/scss/lightlogweb.scss` and
`inst/app/www/js/metadata-navigation.js`; the three explorer/overlay test files;
`dev/milestone-7-overlay-app.R`, this note, the acceptance driver and its JSON
report; and `project-plan.md`. No frozen fixture, dependency pin, generated
namespace/manual, full-app wiring or source-import code changed.

### Tests and browser evidence

- Focused explorer/overlay checks: 1,418 passes, no failures, warnings or skips.
- Full suite: 4,863 passes, no failures, two expected failure/recovery warnings
  and six unavailable-mirai-runtime skips. The warnings exercise the private
  worker failure and malformed `P01_bad.txt` recovery paths.
- `R CMD check --no-manual`: `Status: OK`; 4,782 installed-test passes, the same
  two recovery warnings and nine skips (the six mirai skips plus three optional
  development fixtures omitted from the built package). Unavailable repository
  index lookups are retained in the log; no dependencies were installed.
- Namespace and Rd regeneration on a disposable copy is byte-identical. Air,
  JavaScript syntax and diff-whitespace checks pass. Lintr and spelling checks
  remain unavailable locally; no tool installation was performed.
- Current owner-only browser evidence is in the consolidation, toolbar and
  source-variable packets cited above. It covers Apply/Undo, current versus
  source values, exact single targeting, missing metadata, native keyboard and
  Escape/focus return, desktop and 390-pixel layouts with no horizontal overflow
  or captured console errors. No runtime code changed after those checks.
  The earlier six-tier R57 independent review remains historical evidence, not
  a new independent review of the owner-only refinements.

### Final performance evidence and remaining gates

The full offline report is version 2.10.0, measured at
`2026-09-23T12:13:57Z` after the test/check processes finished. It includes all
three public-derived fixtures, the widest UCR inventory, appended/absent/nested
and unknown metadata, 50,000 source entries and 5,000-field overlay inventories.
All operation-specific hard timing, allocation, relationship and identity gates
pass. Ordinary navigation/search/filter/sort/selection performs zero package
reopens and zero measurement deserializations after initialization.

The maximum ordinary p95 is 76.6 ms against 100 ms. Thus the 50 ms design target
is not uniformly met across overlay presentation and edits, even though the
explorer-only design gate passes. Stress single removal is 68/150 ms; largest
initial/bulk index work is 190.95/500 ms. Guarded section replies are 1 ms p95,
and eight-choice bursts reach 10.65 ms. Largest overlay presentation allocation
is 7,054,640 bytes against 10 MiB; maximum mutation allocation is 66,125,824
bytes against 67,108,864 bytes, retaining limited headroom. The three fixture
bundles total 6,286,168 bytes and the scaled index 35,702,368 bytes. These are
local R-side measurements, not concurrent-capacity or end-to-end browser claims.

Full-app authoring wiring and its integration regressions remain the next owner
gate. This acceptance does not mark the whole Milestone 7 complete and does not
authorize a later slice, push, deployment, dependency installation/repin or new
live reads. No additional test server was started during final verification;
the approved 7383 showcase and the owner's older edited sessions remain intact.
