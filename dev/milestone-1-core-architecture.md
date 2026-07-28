# Milestone 1 core architecture and verification record

Status: complete; accepted by the owner on 2026-07-17

Recorded: 2026-07-17

Branch: dev

## 1. Delivered architecture

Milestone 1 replaces the active legacy dataset graph with session-scoped,
validated value objects and a single reducer-backed store. Dataset records use
stable storage IDs that do not change when display names change. Canonical raw
data are serialized privately, carry a SHA-256 integrity checksum, and are
never returned by reference. Prepared data, metadata, analysis settings,
versioned recipes, drafts, previews, revisions, provenance, and bounded undo
history have explicit fields and validation boundaries.

Recipes now contain ordered stable step objects with a type, serializable
parameters, and an enabled flag. Consequential edits follow this lifecycle:

    draft -> preview -> apply

Apply records an undo snapshot and increments the dataset revision. Reset
materializes prepared data directly from the unchanged canonical payload. Undo
restores the previous applied snapshot while advancing the revision so any
older background result is stale.

The active import, dataset-manager, and dashboard modules receive explicit
reactive inputs and return explicit events or values. The parent app alone
reduces those events into session state. Obsolete package-loaded helpers and
modules that mutated nested `reactiveValues` were removed after their active
replacements passed regression tests. No `reactiveValues` dataset graph remains
under `R/`.

## 2. Long-task and failure contract

The common controller is built around Shiny `ExtendedTask`, promises, mirai,
and the per-session runtime. It supports these workload types:

- raw import;
- GLC discovery, read, and download;
- preparation;
- append merging;
- metrics; and
- report rendering.

Its visible states are idle, queued, running, finalizing, complete, warning,
error, cancelled, and stale. Hosted sessions allow one active task. Local
sessions use at most two workers, bounded by available physical cores. Setting
`workers = 0` selects the synchronous promise fallback. If mirai daemons cannot
start, only that session falls back synchronously and receives a recoverable
warning.

Task payloads are validated as process-serializable values before submission.
Dataset tasks snapshot the stable ID and revision. Finalization looks up the
current revision and discards a mismatched result before calling its apply
callback. Worker, submission, worker-start, finalization, timeout, and
cancellation failures are contained as task state, leaving other outputs and
session data usable. A failed or cancelled controller can be invoked again.

Typed condition domains cover import, validation, resource, preparation,
grouping, metric, export, network, and unavailable-feature failures. Public
messages omit private worker details; diagnostic IDs, original classes,
messages, stack information when available, and parent conditions remain on
the private condition object.

## 3. Source and resource boundaries

Uploads are copied rather than moved from Shiny's temporary path. Generated
storage names prevent client filenames from becoming paths. The staged bytes
must match both the source size and source SHA-256 fingerprint. The staged
fingerprint is revalidated when the import request is created, so a same-size
modification after staging is rejected before worker submission. Original
names, hashes, byte sizes, import arguments, and session paths are captured
separately in provenance. The configured upload limit applies to the complete
import action.

Every session owns a marked temporary tree with separate upload, cache, and
task directories. Hosted and local profiles set upload, cache, total temporary
storage, timeout, worker, and concurrency limits. The RDS cache evicts the
least-recently used files before finalizing a new item, rejects an item larger
than the complete cache, and uses generated temporary files plus guarded cache
keys. Upload staging checks both the request and total session limits. Session
initialization refuses an already-existing caller-supplied root rather than
marking it for ownership. Session end cancels work, stops only that session's
mirai compute profile, and removes the complete marked tree.

The glcdp seam matches the reviewed 0.90.0 public export contract at commit
`21e806112a0032261d65180c3cc3ed72af38ea8d`. Source specifications, selections,
previews, and task payloads contain serializable data only. A worker receives
the repository plus exact 40-character revision and reopens it with
`glc_open()` inside the worker. A `glc_package` object, its transport
environment, and other live handles are explicitly rejected from task
payloads. Downloads are constrained to the source's session cache directory.
The task-payload and worker boundaries rebuild and revalidate source and
selection objects, so changing a resolved SHA or selection after construction
is rejected before `glc_open()` is called.
Documented glcdp discovery, validation, transport, Git LFS, import,
compatibility, and disk/resource conditions map to recoverable LightLogWeb
conditions while retaining private diagnostics.

The refreshed lock and deployment manifest contain the same 163 records: 162
CRAN records and the exact glcdp GitHub revision. Retiring the legacy map and
widget modules removed `leaflet`, `leaflet.providers`, `raster`, `sp`, `terra`,
`lattice`, and `shinyWidgets` from the closure. The deployment manifest now
contains only the 25 runtime source files plus `renv.lock`; it no longer lists
the internal `dev/` workspace, tests, build artifacts, or retired R files.

## 4. Requirement and acceptance-gate evidence

| Plan item | Authoritative implementation and verification |
|---|---|
| 1. Validated session model and stable IDs | `R/session-model.R` owns dataset/session constructors, validators, reducer, and store; `test-session-model.R` covers integrity, stable-ID rename, and independent sessions. |
| 2. Pure computation and explicit module contracts | Lifecycle and reducer operations are ordinary functions. `m_import.R`, `m_dataset_manager.R`, and `m_dataset_dashboard.R` accept explicit inputs and return values/events; `test-module-contracts.R` exercises those returns. No `reactiveValues` dataset graph remains under `R/`. |
| 3. Draft, preview, apply, reset, and undo | The complete revisioned lifecycle is implemented in `R/session-model.R`, covered by pure tests, and exercised in the isolated browser showcase. |
| 4. Common ExtendedTask/promises/mirai layer | `R/long-task.R` and `R/resource-profile.R` provide one controller/runtime for all eight named workload types; `test-long-task.R` covers synchronous and real-daemon execution. |
| 5. Stale-result rejection | `finalize_task_result()` compares the snapshotted revision before the apply callback. Unit, controller, real-worker browser, and revision-advance evidence all show that stale results are not applied. |
| 6. Explicit task states | `task_states()`, `allowed_task_transitions()`, and transition tests cover idle, queued, running, finalizing, complete, warning, error, cancelled, and stale. |
| 7. Serializable glcdp seam | `R/glcdp-adapter.R` owns source, selection, preview, exact-SHA reopen, and worker execution. Boundary tests reject live handles and modified value objects before `glc_open()`. |
| 8. Typed recoverable errors | `R/conditions.R` defines all nine LightLogWeb domains and documented glcdp category mappings; `test-conditions.R` and `test-glcdp-adapter.R` verify classes, safe public messages, and retained diagnostics. |
| 9. Hosted/local resource profiles and cleanup | `R/resource-profile.R` owns worker/concurrency limits, timeouts, capacity checks, bounded cache, marked session trees, and cleanup; resource/import tests cover limits, eviction, fallback, safe ownership, and session-end removal. |

| Gate | Evidence |
|---|---|
| Two sessions remain isolated | `test-session-model.R` creates independent `testServer()` sessions; the second begins empty after the first adds a record. |
| Reset reproduces raw exactly | Pure lifecycle tests compare serialized payloads and reconstructed data; the browser showcase returns from 3 prepared rows to all 6 raw rows while the raw checksum is unchanged. |
| Stale tasks cannot overwrite newer revisions | Unit and controller tests prove the apply callback is not called; the real-worker showcase advances revision 0 to 1 while running and finishes `stale` with prepared data still equal to raw. |
| Live package handles do not cross workers | Serialization tests reject a fake `glc_package` with a transport environment; the adapter test reopens the exact SHA in the worker seam and returns a serializable preview. |
| A task failure leaves the app usable | ExtendedTask tests fail and retry the same controller. In the browser, a private worker failure renders only the public preparation message, then a successful retry completes. The integrated app also loads a sample dataset after an invalid import request. |

Additional automated coverage verifies all task states and transition rules,
queued and running cancellation with retry, warning capture, real process
execution, finalization-failure recovery, runtime-start failure fallback,
cache eviction, request/session resource limits, cleanup on session end,
refusal to take ownership of existing directories, typed error mappings,
upload byte and hash preservation, rejection of modified staged files, module
returns, stable-ID rename/remove events, exact glcdp exports and worker-side
value revalidation, launcher arguments, and construction of every retained
module development app.

## 5. Verification results

- Air formatting check: passed for every touched R, test, and showcase file.
- `devtools::load_all()`: passed.
- Full `devtools::test()`: passed. The sandbox run skips only the test that
  requires mirai IPC sockets. Its two warnings are intentional evidence: the
  installed Shiny build-version notice and the deliberately raised private
  worker failure.
- Real-worker `test-long-task.R` outside the IPC sandbox: passed with no skips;
  it proves a different worker PID and asynchronous warning capture.
- `R CMD check` through `devtools::check(document = FALSE, manual = FALSE)`:
  `0 errors | 0 warnings | 0 notes`.
- Source archive: 39 files and 8 test files; `project-plan.md`, all of `dev/`,
  `manifest.json`, `renv.lock`, test replay artifacts, and every retired legacy
  module are absent.
- Lock/manifest parity: 163 packages in each, no version mismatches, exact
  glcdp SHA in both, no non-CRAN repository record other than glcdp, and all 26
  manifest file checksums match the current runtime allowlist.

## 6. Browser verification

The isolated `core_architecture_app()` and integrated `LightLogWeb(profile =
"local", workers = 1)` were exercised in a real browser with an actual mirai
daemon.

The isolated journey verified idle, running, complete, warning, error,
cancelled, and stale task states; retry after error and cancellation; and the
complete draft/preview/apply/reset/undo lifecycle. Error text did not expose
the private worker message. The status region has `role = "status"` and
`aria-live = "polite"`.

The integrated journey verified the dashboard empty state and return-to-import
action, sample loading, the stable ID remaining unchanged across rename while
revision advanced from 0 to 1, removal back to the empty state, a recoverable
invalid-import message, and successful sample loading after that error.

At 390 x 844 and 1280 x 800, neither app produced horizontal document
overflow. Buttons, form labels, tabs, dialogs, status regions, and the sidebar
resize separator exposed semantic browser roles and accessible names. The core
showcase produced no browser warnings or errors. The integrated app produced
no browser errors; five deprecation warnings originate in Shiny's bundled
Bootstrap Datepicker locale code rather than LightLogWeb code.

## 7. Local environment notices

The active system library contains two orphaned failed-install directories
named `file46ba324ac7` and `file46badbbe3fe`, so renv prints a startup notice.
They are outside this repository and were not removed. The current project
library also records some packages as installed from Posit Package Manager,
whereas the portable lock deliberately standardizes released packages to CRAN.
The package build, tests, lock/manifest parity checks, and source-package check
are unaffected.

## 8. Owner acceptance and regression script

The owner confirmed on 2026-07-17 that the draft, preview, and apply statuses
were correct and that all presented long-task success, contained-failure and
retry, warning, stale-result, cancellation, and post-cancellation retry flows
behaved as described. Reset and undo fidelity, session isolation, worker
serialization, cleanup, and package integrity remain covered by the automated
and browser evidence above.

The showcase is launched by `dev/milestone-1-core-app.R`, or after
`devtools::load_all()` with `shiny::runApp(core_architecture_app())`.

1. Click **Create draft**, **Preview**, then **Apply**. Draft and Preview become
   true before Apply; Apply advances revision and reduces prepared rows from 6
   to 3 without changing raw rows.
2. Click **Reset to raw**, then **Undo**. Reset restores 6 prepared rows; Undo
   restores the prior 3-row applied result while revisions continue upward.
3. Click **Failed task**, then **Successful task**. The first ends in error with
   a recovery-safe message; the second completes in the same session.
4. Click **Task with warning**. It ends in warning without losing its result.
5. Click **Stale task**. The record revision changes while the worker runs and
   the result ends stale rather than applying.
6. Start **Successful task** and immediately click **Cancel current task**. It
   ends cancelled; another successful task can then complete.

This milestone intentionally provides the core contracts and adapter seam, not
the later GLC feature UI, full preparation recipe editor, merge wizard, metric
workbench, or report interface.

## 9. 2026-07-28 glcdp 1.0.0 compatibility addendum

This is a later compatibility review, not a rewrite of the accepted Milestone 1
evidence. The core seam above was implemented and verified against glcdp
0.90.0 at `21e806112a0032261d65180c3cc3ed72af38ea8d`; those historical claims
and results remain unchanged.

The review loaded glcdp 1.0.0 from commit
`76b532a7167edb212059734234ff6ed6fe10f9e2` under R 4.5.0 and compared the
exported functions and formals with the installed 0.90.0 package. Every one of
the 14 lower-level functions named by `glcdp_export_contract()` remains
available with the same formal arguments. The existing source, selection,
preview, exact-SHA reopen, worker-serialization, and recoverable-condition
architecture therefore remains a valid seam for the Milestone 6 upgrade.

Version 1.0.0 adds extract_metadata, add_metadata, and glc_explore. The first
two enter the LightLogWeb adapter with Milestone 7 so relationship traversal
and row-preserving metadata joins remain owned by glcdp. glc_explore remains a
package-owned reference application and is not embedded or coupled to
LightLogWeb. The existing source specification already records the exact
package revision, schema version, glcdp version, registry state, and session
cache, so no value-object redesign is required.

The same review confirmed Schema 3.0.2 as the final current contract, with
3.0.0 and 3.0.1 retained as stable predecessors and 1.0.0/2.0.0 reclassified
as limited legacy paths. Schema 3.0.2 is no longer an experimental condition.
Milestone 6 must replace that obsolete path with current, stable-predecessor,
legacy, declaration-mismatch, and unsupported-future-schema coverage.

The opt-in live R check opened the attestation-verified latest passing IZTECH
revision `9353a0c4287d44cb400d30f45d7dbcf9910f9bde` through glcdp 1.0.0 as
Schema 3.0.2 after validator 0.5.2 reported zero errors and zero warnings. This
establishes the current integration target but does not upgrade the installed
LightLogWeb dependency or its Milestone 1 fixtures. DESCRIPTION, renv.lock,
manifest.json, adapter code, and tests remain unchanged until Milestone 6.
