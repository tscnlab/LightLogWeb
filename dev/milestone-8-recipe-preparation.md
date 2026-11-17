# Milestone 8: recipe-based preparation

## Closed: owner acceptance, 2026-11-17

The owner accepted Slice 5 R14 and asked to close, commit and push Milestone 8.
All five slices are accepted. The frozen accepted full-app showcase is
`m8-s5-integration-20261117-r14`, source-index SHA256
`d0d71ba567501bcef3fef479a1ca1eae50bc4c5a3af35b6fa3adce209b7ebdc0`.
Its final gate includes 1,328 focused assertions, 44 independent metadata
contracts and independent responsive/browser review. Previously deferred narrow
layout notes remain nonblocking; no data-processing issue remains open.

The owner separately requested an experimental redesign of the Prepare user
journey. That work starts after the accepted milestone commit, on a separate
branch. It does not reopen the preparation engine, change operation semantics
or replace the accepted interface without a further review. The R14 showcase
remains available for comparison. No deployment is requested.

The following development and review history is retained as evidence. This
closeout supersedes earlier pending-acceptance and no-commit/no-push statements.

## Development and review history

Updated: 2026-11-17. Slices 3 and 4 are accepted and closed. For Slice 3, the owner accepted the
slice and authorized Slice 4, subject to the final conditional-legend correction.
Independent review accepted that correction in `m8-s3-gap-marks-20261116-r2`:
no legend for no marks, and only the labels present for amber, blue or mixed marks.
The source index is `7411fae20c62e850f3aff348f093914178b711b1de095e890f128a7400079437`.
Its durable report is in the reviewer's `m8-s3-gap-legend-20261116-review` folder.
This status supersedes historical owner-review-pending entries below.

The owner accepted Slice 4 r6 on 2026-11-16 and authorized Slice 5 with an
independent review before handoff. Slice 4 is closed. Slice 5 integrates the
accepted Prepare module into the full app: dataset library selection and atomic
Apply/Reset, dashboard focus and axis settings, effective metadata labels/units,
retained per-dataset recipe drafts, task recovery and bounded resource use.
Applied recipes remain serializable in dataset records; project/script export
UI remains Milestone 13. The review will exercise dataset switches during work,
metadata changes, source-based reapplication and dashboard refresh, plus browser
and performance checks. No commit, push or deployment is included.
The historical Slice 4 handoff record below remains as evidence.

## Slice 5: accepted full-app integration

The subsequent owner retest found selective automatic-preview failures after a
four-day duration step and a blocked metadata editor for Prepare-created
variables. R13 corrects the save transaction: inactive date controls move into
the shortened entering span before their calendars redraw, so their empty
out-of-range acknowledgements cannot be mistaken for new edits. Real parameter
edits still invalidate the preview. Derived metadata assertions have a separate
dataset/creator identity; editing their label or unit leaves the parent,
immutable source and transformation history intact. Removed-creator assertions
remain retained without being assigned to a new creator reusing the same name.
Independent review confirmed both automatic saves and the metadata editor,
including cached context, reapply, Undo and Reset. Its R48 follow-up is corrected
in R14: linear or masking children retain a transformed parent's declared unit;
only an additional transformation needs a new derived-unit qualification.

R14 also contains the owner's bounded presentation pass, isolated to the Prepare
UI and scoped SCSS. It removes doubled tab/input spacing, groups Target and
Write to with their hint, gathers measurement explanations and diagnostic counts
under How this step works, and separates editor feedback/actions with a light
divider. One redundant preview instruction is removed. An R HTML contract check
confirms unchanged inputs, defaults, choices, button states and output IDs.
The functional regression set and final independent presentation review precede
handoff. The owner subsequently accepted this presentation and the fixes.

Final R14 gate: independent review accepts the candidate for owner retesting.
The consolidated focused suite passes 1,328 assertions with zero failures,
errors, skips or test warnings; the reviewer separately passes 44 metadata
contracts and the six-width browser check with light/dark samples. The exact
single-save R46 reproduction is protected by R13; R14 accepted saves also
complete automatically, with rapid automation-input limitations retained in the
review report. Dense wrapping at 1024 pixels and earlier deferred mobile notes
remain nonblocking. R14 is running at `http://127.0.0.1:7455/`; obsolete showcase
servers are closed. Its 221-file source-index SHA256 is
`d0d71ba567501bcef3fef479a1ca1eae50bc4c5a3af35b6fa3adce209b7ebdc0`.
The independent report is in the review chat's
`m8-s5-integration-20261117-r14-review/review-r14.md`. The frozen candidate includes
an isolated two-file `presentation.patch`, the screenshot and verification
records. Source files remain unchanged after review. Owner acceptance on
2026-11-17 closes Slice 5 and Milestone 8.

The owner reviewed R7 on 17 November and requested a revision before retesting.
R8 addresses the duration default, pending-preview recovery, operation browsing,
explicit Save-and-preview action, clearer busy feedback, taller plots and bottom
legends. Only-step comparisons show the input and output of that step while
retaining earlier recipe dependencies. Overlay remains the default; Split view
uses each stage's own time zone and exposes dates observed in either zone.
Long measurement labels wrap on the plot axis.

Apply now follows the selected measurement's derived-variable chain in the
Dashboard and displays the prepared timestamps' time zone. Existing selected
branches stay selected; Reset falls back through their recorded ancestry.
The denominator labels now distinguish observations, observed time spans and
full observed days. Zero-inflated log and inverse operations call the installed
LightLogR functions with recorded base and offset, using mapping version 3 while
preserving existing transform versions. Documentation links point to those
functions.

The reported duration-plus-range loop was reproduced with a deferred worker:
an obsolete pending request survived an input-generation change, then failed
task validation repeatedly. Obsolete requests now clear before dispatch, and
startup rejection releases the pending state. Operation browsing alone no
longer changes the generation; actual parameter edits still invalidate stale
work. The existing form-acknowledgement transaction remains in place.

The initial R8 control check exceeded the 100 ms p95 limit for reordering at
250,000 rows (111.2 ms). Profiling identified unnecessary busy/idle rendering
while saved recipe edits awaited control acknowledgements. Queued feedback now
stays visible until the replacement request is ready, and Cancel also clears
that automatic-preview intent. The stale-generation check remains authoritative.
The same complete-action check, including all browser acknowledgements, now
measures p95 60.6/38.0/51.2 ms for edit/toggle/reorder at 25,000 rows and
50.6/39.0/70.8 ms at 250,000 rows. All 25 consecutive samples per action are
retained. No source payload was decoded in the main process and Dashboard
snapshots were not rebuilt. These are local synthetic checks, not hosting-load
estimates; the initial failed run is retained with the revised evidence.

The final review corrections are in R9. Automatic shared axes now include the
full before/after comparison, including negative transformed values. Bounds are
calculated in the worker and remain stable across selected days and Split panels;
explicit manual limits remain authoritative. Removing a selected derived branch
returns to its nearest surviving ancestor without following a sibling branch.
Refreshing Prepare after a Dashboard focus change uses the intended restored
control selections, avoiding a false unsaved state while preserving real edits.

Independent R9 review cleared the functional corrections and completed the
browser checks. R12 adds a scoped alignment correction for the time-zone fields
when the entering-zone label wraps, separate lines for Split titles and plot
captions, and guidance matching the combined Save-and-preview action. Independent
review passed the final presentation checks at 1440/1200/1024/768/390/320 CSS
pixels, including light and dark samples. R12 is ready for owner retesting at
`http://127.0.0.1:7453/`; obsolete showcase runtimes are closed. Source-index SHA256:
`735b4a154885a68cf63f4923ce312e59513d9932721074b8ae2344a978b808f0`.
The durable report is in the independent review chat's
`m8-s5-integration-20261117-r12-review` folder. A minor empty-stage subtitle clip
at 320 pixels remains documented as nonblocking; the complete context is in the
adjacent controls. The R7 acceptance below is historical. Slice 5 remains open.

The production parent now includes Prepare between Dashboard and Metadata. The
offline integration launcher uses that same parent with the existing synthetic
and local fixtures. Apply and Reset use a guarded store transaction for the
selected dataset; source payloads remain immutable. Saved draft recipes live in
dataset records as serializable settings, separate from the applied recipe and
measurements. Up to 20 small unsaved editor states are retained during this
browser session; evicting a form never removes its saved recipe.

The preview follows the dashboard's focus metric, scale, symlog threshold, and
axis range. Participant and day selection remains local to Prepare. A derived
dashboard focus falls back to its recorded source where it does not yet exist in
the recipe. Effective metadata labels and units appear beside raw variable names.
Metadata edits and undo retain recipe drafts, refresh their metadata context,
and invalidate older previews before Apply. Aggregation now refreshes dashboard
sampling and coverage from the prepared timestamps, while import diagnostics
retain source quality.

R tests cover serialization, navigation and dataset switches, Apply/Reset,
dashboard focus and scale, metadata edits during a draft, and stale commit
rejection. The existing task cancellation/recovery tests remain part of the
affected suite. An R benchmark of five-minute aggregation measured 0.952 seconds
for 25,000 rows and 3.729 seconds for 250,000 rows; dashboard snapshot construction
took 0.331 and 0.513 seconds respectively. Draft staging had a 1 ms p95 over 25
repetitions at both sizes. These local synthetic timings are not hosting-load
estimates. The benchmark records code hashes, inputs, command and sessionInfo.

Independent review accepts frozen Slice 5 R7 for owner final review. The owner
will review later and requested all running instances closed. All showcase
servers and browser tabs are stopped; the frozen source, review evidence,
preview screenshot and restart instructions remain saved. Owner acceptance is
pending. No project export UI, commit, push or deployment is included.

The r2 candidate corrected prepared-quality validation when the display timezone
differs from the timestamp timezone. It also keeps committed record views stable
while only recipe drafts change, avoiding Dashboard, library and metadata
redraws. Consolidated affected tests pass 3,848 assertions in 251 cases, with no
failures, errors or skips and three expected cancellation warnings. Later reruns
replace matching earlier cases; their results are not double-counted.

A full-parent testServer stress loop with automatic preview cancellation and
completion measured p95 180 ms at 25,000 rows and 190.8 ms at 250,000 rows after
the redraw fix, versus about 764 and 880 ms in r1. It performs two mock-session
flushes and evaluates registered outputs including hidden tabs. No payloads
were decoded in the main process and Dashboard snapshots were not rebuilt.
This broader loop is not claimed to meet the earlier 100 ms ordinary-control
gate. The independent review will assess the remaining cost alongside actual
browser responsiveness. The pure draft-stage timing above measures a narrower
operation and must not substitute for the full-parent result.

R3 corrects the independent review's R34/R04, R35 and R36 findings. Applied
numeric derived variables are available as Dashboard focuses, with the inherited
label, a single raw-name reference and the effective unit. Source views explicitly
fall back to the recorded source variable. Dashboard date summaries wrap between
complete date endpoints, and stacked summary cards have aligned edges. Reset
cannot repersist a stale editor recipe after the store restores the source.
Regression checks exercise derived focus through both screens and Reset followed
by tab switches. Prepare's applied-record and editor display contexts remain
stable when their contents have not changed; Append also consumes committed
records. Computational recipe contexts still update immediately.

An enabled-only change to the edited step no longer reloads all of its controls.
Changes to preceding steps or the edited parameters still refresh the editor.
Automatic previews coalesce quick saved edits for 150 ms before dispatch; pending
feedback is immediate and manual Preview dispatch is immediate. Replaced or
cancelled requests cannot run from an older timer. The existing installed `later`
package moves from Suggests to Imports for this runtime use, with no installation
or lockfile change.

The consolidated R3 affected suite passes 3,886 assertions in 254 cases with no
failures, errors or skips and three expected cancellation warnings. Later runs
replace earlier matching cases, including the initially failing Reset regression
and a timer test that omitted the browser's initial input acknowledgement.
The timer checks cover replacement, Cancel and immediate manual Preview.

The R3 direct-action benchmark omitted the required follow-up acknowledgement
from its timer. Independent review closed R34/R04, R35 and R36 but opened R37:
complete reordering at 250k rows reached 171.6 ms p95. Its original timing, raw
samples, DOM visibility evidence and unrestricted Mock stress run remain as
historical evidence; the direct-action result does not establish acceptance.

R4 retains every required date/time update and measures the action plus all
immediate input acknowledgements. Unchanged selections and choice catalogues
are not rebuilt. Calendar state is batched, with date widgets replaced only
when their displayed value or bounds change. Calendar-bound conversion keeps
already-typed endpoints intact. Documentation depends only on the operation
and its relevant settings. Apply remains blocked while controls are loading.
No early-preview queue change or bespoke browser JavaScript is included.

The complete full-parent matrix has 25 consecutive saved edits, toggles and
reorders at each size. Their p95 values are 78.8/51.2/61 ms at 25k rows and
85.2/45.8/96.8 ms at 250k. The changed inactive endpoint and its reactive flush
remain inside the timer. All required live outputs and outputs in other modules
remain active; only outputs hidden in the observed clock-form DOM are suspended.
No complete source payload decode or Dashboard snapshot rebuild occurred. This
passes the 100 ms ordinary-control p95 gate locally, with limited headroom for
reordering. Earlier diagnostic attempts, including unsuccessful optimizations,
are retained. Independent R4 review measured 108 ms p95 for complete reordering
at 250k rows, so R37 remained open. Calendar bounds, dropdown catalogues,
restored drafts, cancellation/retry and the affected desktop/mobile layouts
passed. The frozen R4 source and failed timing gate remain preserved.

Package checking prompted Unicode escapes in seven preparation files and an
explicit `.data` import. R verified that every parsed expression in those seven
files is identical before and after escaping. Documentation was regenerated.
The final source-package build and `R CMD check --no-manual --no-tests` finish
with Status: OK; the affected test suite is recorded separately above. The
pre-existing untracked `man/man.Rproj` is excluded automatically by R CMD build
and was left untouched. The pre-existing renv out-of-sync startup note remains.
Independent R3 review is complete; R37 is the sole open acceptance finding.
R4 passes 3,916 consolidated assertions in 255 cases, with no failures, errors
or skips and three expected cancellation warnings. Affected module, time/DST
and integration checks were rerun; the added checks cover transition-date
bounds and the actual rendered calendars.

R5 treats server-issued editor updates as one form transaction. Clean saved
settings stay clean while browser acknowledgements arrive, and unchanged dirty
state no longer redraws every dependent output. Reordering operates on the
saved recipe and may replace an outstanding clean editor update. Unsaved forms,
including restored drafts still loading their controls, block reordering.
Save and Apply still wait for the browser's complete acknowledgement. No input
update, validation, required output or acknowledgement is removed from the gate.

The R5 complete action-plus-acknowledgement matrix measures p95
70.0/49.6/65.8 ms at 25k rows and 66.6/49.6/82.0 ms at 250k, for saved edits,
toggles and reorders respectively. All 25 changed-endpoint acknowledgement
batches are included at 250k. There are no main-process source-payload decodes
or Dashboard snapshot rebuilds. Independent R5 review measured 102.4 ms p95
for complete 250k reordering and retained R37. It also identified R38: an
unexpected edit during a clean control update could be lost on navigation.
Frozen R5 and its failed gates remain retained; no favorable rerun of unchanged
code replaces those results.

The consolidated R5 suite passes 3,934 assertions in 256 cases with no failures,
errors or skips and three expected cancellation warnings. Its module, time/DST
and integration rerun replaces matching earlier cases. New checks cover clean
acknowledgements without dirty-state invalidations, rapid saved-step reordering,
Save/Apply guards during loading and restored unsaved drafts before and after
their browser acknowledgement.

R6 merges unexpected user input into the intended editor transaction while
allowing old and new values from issued control updates to settle. Such edits
are immediately marked unsaved, block recipe changes, survive dataset draft
caching, and remain available to save after the remaining controls acknowledge.
Presentation flags and button messages are published together; identical visible
values no longer trigger repeated status/output work. Required controls, live
outputs and acknowledgements remain included in verification. Independent R6
review is required before owner handoff.

The first R6 local matrix reduced complete 250k reordering to 63.8 ms p95 but
failed saved edits at 167.2 ms because of large intermittent pauses. An R
diagnostic associated such pauses with major garbage collection. R6 now reclaims
temporary objects after full inspect/preview rendering. This adds cleanup time
at the full-refresh boundary and can briefly occupy the main R process there;
it avoids leaving that cleanup to interrupt a subsequent ordinary edit. It is
not run for individual edits, reorders or day-view changes.

The final R6 matrix measures p95 42.8/40.2/51.2 ms at 25k rows and 41/33/65 ms
at 250k for saved edits, toggles and reorders. All 25 endpoint acknowledgement
batches at 250k remain included; no source payload decode or Dashboard rebuild
occurs during the actions. The earlier failed matrix and GC diagnostic are
retained separately and are not acceptance results.

Independent R6 review passes R37: its highest complete-action p95 is 83.8 ms
for 250k reordering, with all required acknowledgements included. The initial
R38 sequence and dataset restoration also pass, but returning an acknowledged
field to its former value while another field is pending still loses that edit.
R38 remains open on frozen R6. R7 retires old values separately for each field
after acknowledgement or a captured user edit, so subsequent reverts are new
edits. Pure transaction and module regressions cover that sequence before the
next independent review.

The R7 consolidated affected suite passes 3,964 assertions in 257 cases, with
no failures, errors or skips and three expected cancellation warnings. Its
full-parent transaction reproduction passes all 11 draft-preservation checks.
The second sequence acknowledges the observed clock-end update while leaving
the observed clock-start update pending, so the boundary does not depend on
asynchronous hidden-calendar updates. The first harness attempt stopped at
that missing-pending-control precondition and is retained separately.

R7's one complete local control matrix measures p95 43.2/37.8/44.2 ms at 25k
rows and 41.6/33/67.6 ms at 250k for saved edits, toggles and reorders. All
25 required endpoint acknowledgement batches at 250k are included. No complete
source payload is decoded in the main process and Dashboard is not rebuilt.
The R6 cleanup tradeoff is unchanged. Frozen R7 requires independent acceptance
before owner handoff; its scripts, raw samples, source hashes and R/package
versions are retained with the candidate.

Independent R7 verification closes R38 with 14 passing state checks, including
the original pending-input sequence, an old-value return after partial
acknowledgement, dataset restoration, Save and successful later reordering.
The visible browser check also preserves the unsaved value across tabs and
datasets, blocks reordering until Save, and successfully applies the reordered
recipe. No in-scope acceptance blocker remains. The one independent complete
timing run has a highest p95 of 72.2 ms; 250k reordering is 64 ms with all 25
acknowledgement batches included. Its 180 ms individual sample is retained;
the agreed gate is p95, not maximum or end-to-end browser latency.

The accepted frozen candidate is `m8-s5-integration-20261116-r7`, with 220
source files and index SHA-256
`452d5135a1c0f006b5a2a70f65f773ef1757509f8949b1b6d3f878423a576874`.
The owner then requested shutdown for later review. Runtimes 7433, 7437, 7440
and 7449 and their showcase tabs are closed. `RESTART.md`, `launch.R`,
`prepare-preview.jpg` and the sealed independent report are retained outside
the immutable source directory. Restart creates a fresh browser session;
session-only editor state is not claimed to survive server shutdown.

## Historical Slice 4 handoff

Slice 4, aggregation, was handed over for owner final review on frozen candidate
`m8-s4-aggregation-20261116-r6` at `http://127.0.0.1:7440/`. Independent review
accepted the owner's alert, common-type summary and missingness refinements and
closed R33. Inactive invalid or cleared limits no longer block another NA policy;
active invalid limits remain visible errors. The 217-file source-index SHA-256 is
`c3d4b1adc8d0de8536d81496e9151d258c263053a26e930b0c471d90bc372086`.
Consolidated affected verification passes 1,804 assertions across 128 cases with
no failures, errors or skips and three expected cancellation warnings. This
combines unchanged r4 cases, the corrected r5 module rerun and the final r6
aggregation suite (280 assertions). Independent R verification adds 125 checks
for the refinements and 24 for the final threshold correction. Browser review
covers recovery, saved settings, keyboard operation and six responsive widths;
the earlier R16 summary-number alignment observation at 320 px remains deferred.
The durable review is in the reviewer's
`m8-s4-owner-feedback-20261116-review/review-acceptance.md`, copied with its
evidence into the r6 build folder. Superseded test runtimes 7438 and 7439 are
stopped; owner drafts on 7433 and 7437 were preserved. The subsequent owner
acceptance closes Slice 4 and authorizes Slice 5 full-app integration.

## Slice 4: aggregation

The owner requested simple, robust controls within each step and explicitly
allows multiple aggregation steps in a recipe. Each step uses the data entering
it; replay always begins from immutable source. The owner chose to keep
low-coverage aggregates and report coverage without masking them.

- A short interval list and five numeric summaries (mean, median, min, max, sum).
  Numeric NA policies propagate any NA, use available readings, or apply an
  optional maximum missing-readings percentage. The original default is unchanged.
  The limit applies separately to each numeric column, including Duration:
  strictly above the limit gives NA; equality uses available readings, and an
  all-missing bin stays NA even at a 100% limit. NA/NaN rows form the numerator;
  all entering rows, including duplicates, form the denominator. Absent timestamps
  do not count. The owner explicitly chose this denominator and requested visible
  advice to add Gaps first when absent timestamps should count. Complete-local-day
  Gaps includes recording-edge gaps. Time coverage remains a separate description.
  No custom expressions or new dependencies.
- Public `LightLogR::aggregate_Datetime(type = "floor")` supplies calendar-aligned,
  start-labelled bins. Current groups and retained participant/file identities
  remain separate. Intervals finer than the entering native cadence are refused;
  aggregation never upsamples or inserts empty bins.
- A collapsed Other data types section offers most frequent/first/last for
  text and factors, and majority/any/all for logical readings. The original
  LightLogR defaults remain most frequent (including NA, first table category
  wins ties) and majority (TRUE at a tie). First/last use timestamp order and
  original row order for duplicate instants. A missing logical reading yields
  NA under every logical policy. First/last preserve factor levels and ordering;
  most frequent retains LightLogR's factor handling. Other time types use the
  existing public means.
  Duration columns follow the selected numeric method and NA policy. The adapter
  summarises their seconds and restores their Duration type consistently, avoiding
  the upstream odd/even median and missing-bin class mismatch.
  Unsupported types and reserved internal column names stop rather than silently
  dropping source columns. `is.implicit` is true only when all inputs were
  synthetic gap rows, so a majority of gaps cannot relabel real observations.
- Coverage reports the focused measurement entering each aggregation. Finite
  readings contribute at most one native epoch forward, capped at the next
  timestamp and bin boundary. Duplicate timestamps count once for duration;
  all readings still contribute to the selected LightLogR summary. Full bins,
  including partial recording edges and empty interior intervals, define the
  intended window. Actual local bin boundaries handle DST. Unknown cadence gives
  unavailable coverage. After preceding aggregation, coverage refers to available
  input bins, using their actual elapsed bounds across DST, and does not claim
  to recover within-bin source coverage. If a later time correction changes the
  bins' instants, their support is unknown and coverage is unavailable until the
  next aggregation establishes new bounds. Display-only timezone changes retain
  their support.
- Multiple aggregates are deliberately sequential. Means of prior bins give each
  input bin equal weight; they are not weighted by its original row count.
- Compact source-to-bin provenance separates combining rows from filtering them.
  Filters after aggregation exclude all source rows represented by a removed bin.
  Per-step counts remain available; the final preview does not invent a one-to-one
  source-value difference after aggregation. The plot overlays actual bin values
  and keeps the accepted bottom gap marks and conditional legend.
- Measures contains per-step observed duration and coverage plus a selectable,
  bounded first-100-bin report, now also showing the focus variable's row-based
  missing percentage and whether its selected limit was exceeded. The scientific
  output is never sampled.
- Preview failures use a prominent alert with a heading, icon, cause and recovery
  guidance in one place. Settings validation uses the same visible treatment.
  Normal progress stays quiet. Older four-field aggregation recipes are supported
  by additive defaults; the mapping version remains compatible.
- Source-based hourly-to-15-minute replay, explicit public-pipeline parity,
  multiple aggregation, NA/gaps/duplicates, 23/25-hour and half-hour DST,
  source lineage, save/edit/apply/reset and resource guards are verified in R.

Earlier candidate: `m8-s4-aggregation-20261116-r3`, independently accepted for
owner review at `http://127.0.0.1:7437/` on 2026-11-16. Its 217-file index SHA-256
is `abe8baa393e7201c1b00f02d737a3a360740ce3b93bd0e106c963a3065790413`.
R2 fixes independent finding R31 by clipping each reading at its own output-bin
edge, retains actual input-bin bounds for sequential aggregation across DST,
and corrects the showcase title. Frozen r1 remains unchanged.
R3 addresses R32 by stabilising Duration output and correcting its explanation;
clarifies that the minimum interval concerns data entering the step; and stacks
Apply/Reset below 400 px so Reset remains readable. R2 passes 1,686 assertions,
with no failures/errors/skips and three expected worker-cancellation warnings.
The final focused aggregation suite passes 195 assertions with no failures,
errors, skips or warnings. Independent review adds 109 passing R checks and
browser evidence for sequential aggregation, fresh-source replay, filter ordering,
Apply/Reset/Undo, keyboard interaction, cancellation/retry, failure recovery and
six responsive widths. The existing 320 px summary-number baseline observation
remains deferred. No new blockers remain.

Durable review: `/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s4-aggregation-20261116-review/review-acceptance.md`.
The build folder `m8-s4-aggregation-20261116-r3` retains source, source index,
launch script, verification scripts/results, review evidence and a screenshot.
Runtimes 7435 and 7436 are stopped. The owner's existing 7433 draft is preserved.
The owner subsequently requested larger failure messages, simple common-type
handlers and an optional missingness limit. These refinements are independently
accepted in r6, as recorded above. The r3 owner draft on 7437 is preserved.
Slice 4 remains open for owner acceptance; Slice 5 has not started.

### Owner review refinements, 2026-11-16

- The timezone editor places the zone entering the step beside **Change to**.
  Short method-specific text explains clock changes, unchanged or reinterpreted
  instants, and skipped/repeated local times during DST.
- Two clearly labelled synthetic examples cover the Berlin spring transition
  on 29 March 2026 and autumn transition on 25 October 2026. Each has one
  recording, 289 observations and simple visible peaks. The Device DST adapter
  shifts 181 observations by +1 hour in spring and -1 hour in autumn, preserving
  all rows and measurements. Existing duplicate/gap stress fixtures remain.
- Collapsed **Function documentation** sections link only to a central method.
  Custom windowing, duration bounds, full-day gap boundaries and the extended
  DST adapter are identified where the linked function is a method reference.
  No generic data-manipulation links are added.
- **Preview details** now explicitly distinguishes row filtering, missing-value
  assignment, and plot-only line breaks. Independent R comparison of the owner's
  IZTECH example confirms 34,560 source rows become 34,339 rows, with no MEDI NAs
  inserted. The prepared data are identical to direct row subsetting.

Independent finding R30 exposed hidden date controls becoming invalid when an
earlier timezone step was disabled while a later timezone editor stayed open.
Clean saved editors now reload their saved parameters and refreshed defaults
whenever their preceding recipe changes. Blank editors also refresh their defaults.
A new reactive regression fails on the old implementation and passes with the fix,
including both editor states, repeated toggles and Apply.

Timezone/DST refinement candidate: `m8-s3-timezone-ui-20261116-r5`, 214 indexed files,
source index SHA256
`5e964e4807a57cfdcc6f4d7b529627e4c4be89b1a9fed53afdebceac9fb559a9`.
The candidate and focused checks are preserved under
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/m8-s3-timezone-ui-20261116-r5/`.
The final preparation/module verification passes 1,486 assertions with no
failures, errors or skips and the three expected cancellation warnings. Two
testServer reorder cases now acknowledge refreshed editor controls before waiting
for preview, matching the browser. The updated derived-variable test and final
passing rerun are retained under `verification/`, outside the immutable source
snapshot. The prior broader dashboard/metadata/preparation run passed 2,290
assertions. Independent browser checks close R30 for saved and blank editors,
including repeated toggles, drag reordering and Apply. Six-width layout checks
found no new blocking issue. At that handoff the updated showcase was on port 7432; obsolete
review servers and the previous owner-review instance were stopped. Owner
acceptance remains required before Slice 4. The independent report and screenshots
are retained in the candidate's `review-evidence/` directory. The existing minor
320-pixel summary-number alignment note remains outside this refinement.

### Bottom gap marks, 2026-11-16

The owner replaced the proposed shading design with bottom marks. Candidate
`m8-s3-gap-marks-20261116-r1` uses the existing amber for source timestamps absent
from the preview and blue for present preview readings that are NA. Adding a
Gaps step changes restored timestamps from amber to blue, including added rows
whose provenance flag is `is.implicit = TRUE`. No filtering, value or gap-expansion
operation changed. Existing source gaps and pure retiming are not labelled as
removals. Matching respects recording identity and current groups, and a retained
duplicate counts as a present timestamp. All observations contribute marks before
line sampling. A two-colour legend, Preview details and plot alt text explain the
states. When different recordings overlap, a taller amber mark leaves both colours
visible. Marks for removed rows refer to their original source instants.

The frozen 215-file source index has SHA256
`a70e34b2fe3d9ee4083762681b1dc41b91674d652199150b2428ccb98d1d14d2`.
The build and evidence are retained under
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/m8-s3-gap-marks-20261116-r1/`.
732 affected assertions pass, with no failures, errors, warnings or skips after
rerunning the real-worker cases with local process access. The exact IZTECH
example has 221 removed rows and 221 explicit gap rows after expansion. For the
selected IZTECH_S004 day, 60 marks change colour at identical instants.
Independent focused review passed: 31 independent R checks, the amber/blue
switch and recipe-prefix comparison, and a 390px legend fit check. The report and
its evidence are copied into `review-evidence/`; no new finding was raised.
Only the updated showcase on port 7433 remains running. The superseded 7432
server is stopped. Owner acceptance is pending.
This is a bounded refinement; broader package and responsive review are deferred.

## Slice 3: time integrity and gaps

The owner authorized this slice on 2026-11-13 and requested independent review
before the showcase handoff. Time & sampling now offers three recipe operations:

- **Time zone:** choose an Olson name and either keep the instant (`with_tz`)
  or keep the clock (`force_tz`). Reinterpreting a skipped local time is refused.
  Repeated local times require an explicit earlier/later choice; the default
  refuses ambiguity. Later native-date and clock filters use the zone entering
  that step. Explicit absolute datetime bounds retain their saved instants.
- **Device DST:** diagnose offset transitions, duplicate instants and repeated
  local clocks, and explicitly correct a device that kept its starting UTC
  offset. The first timestamp in each current group and participant/file
  recording is assumed correct. This is never inferred from a transition alone.
  The adapter matches `LightLogR::dst_change_handler()` for one-hour transitions
  and additionally handles half-hour transitions using actual offset changes.
- **Gaps:** use each recording's dominant interval or a custom elapsed interval.
  Expand within its observed timestamps or complete local days. Added rows have
  `is.implicit = TRUE` and missing measurements. Existing readings, duplicate
  and off-grid timestamps, prior implicit markers, and grouping are preserved.
  Single-observation recordings stay unchanged. No values are interpolated.

Expansion is preflighted without allocating the full grid. A conservative
working-memory estimate and a five-million-row ceiling reject oversized work
with a useful coarser-interval/shorter-window recovery message. Complete days
respect 23/25-hour DST days and include the day of a final midnight observation.
Observed-span expansion uses the public `LightLogR::gap_handler()`; a small
`seq`/`full_join` adapter supplies the explicit complete-day boundary contract.
Both preserve source data. Applying repeatedly always replays from source.

Time-aware previews use the actual source and preview instants on a common
preview-zone axis. Added gaps have bottom marks. Summaries distinguish removed
source rows, added gaps and shifted timestamps. Saving, editing, reordering,
Undo and Reset use the accepted single-recipe behavior. The showcase starts
with four IZTECH participants and includes synthetic spring and autumn DST
fixtures; diagnostics do not assert that these synthetic devices were faulty.

Validation: the revised preparation/module suite passes 1,453 checks with no
failures or skips and the three expected real-worker cancellation warnings.
The suite covers empty results, fractional day grids, native date/clock replay,
absolute endpoint editing after timezone changes, source-only plot layers and
Undo across changed date bounds. Evidence is in
`/private/tmp/llw-m8-s3-r3-tests.log` and the matching `.rds` file.
The module drag test acknowledges worker-updated hidden controls, matching the
browser rather than leaving client input updates pending in `testServer()`.
Browser checks pass the exact DST Apply, full-day gaps, disable/Undo and
reorder/Undo paths, restoring the 444-row preview with a clean editor and Apply
available. The revised frozen candidate passed independent acceptance; owner review remains pending.

### Slice 3 review follow-up

Frozen r1 is `m8-s3-r1-7476f179d3ba`, source
`/private/tmp/llw-m8-s3-r1-68a5phak/source`, index SHA256
`7476f179d3baab3ce3352f8f06d5315fad6bfd0a00fd7fadb82aa0b80290a782`.
The independent review found R27: a native datetime endpoint created after a
timezone change was validated against the original source zone and refused.
The correction preserves the saved offset-bearing instant; native clock input
is validated when saving the form. Editing renders that instant in the zone
entering the step. Explicit custom endpoint zones remain checked.

New date and recurring-clock filters use a version-2 native-time mapping so
they continue to follow the zone entering their step after a timezone step is
disabled, removed or reordered. Version-1 filters retain their explicit zones.
Absolute datetime endpoints retain their saved instants through reordering.
These rules are exercised through pure replay and reactive Save/Edit checks.
The original r1 source remains frozen while corrections are prepared.

The r2 review extended R27: reordering the unchanged absolute-endpoint step
changed its entering zone without reloading its date/time fields. The next
revision reloads a clean endpoint editor whenever its preceding recipe changes,
including Earlier/Later, drag order and preceding-step toggles. Saved absolute
instants stay unchanged. Reactive and actual IZTECH browser checks verify both
reorder directions and saving unchanged controls without losing rows.


R28 identified grey source traces interrupted by preview-only missing rows in
the retimed overlay. The source line and point layers now receive only their
source-stage rows. Genuine source missing values and gaps still break lines;
the preview remains overlaid at its own instants. A plot-layer regression
checks both complete continuous traces independently.

R29 identified stale hidden date defaults after Undo when DST correction changed
the available date span. Preview completion now refreshes inactive date controls
before their bounded inputs redraw, while preserving the active step's fields.
Empty-prefix results retain valid source-derived date bounds for editing.
The independent review's exact recovery sequence passes in the internal browser;
reactive tests also cover toggling, reordering, Undo and subsequent Apply.

### Slice 3 independent acceptance and deferred owner review

Frozen r3 `m8-s3-r3-eda5f34fbbe5` passed independent review. R27, R28 and R29
are closed. All 31 final independent R checks passed, alongside 1,453 supplied
regression checks and six-width browser alignment checks. A previously known
minor summary baseline issue at 320 px remains outside the acceptance blockers.
The owner requested no handoff today and all instances closed. Six remaining
LightLogWeb Shiny servers and their associated workers were stopped; all showcase
tabs were closed. Owner approval remains pending, so Slice 4 has not started.

Durable restart package:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/m8-s3-r3-eda5f34fbbe5/README.md`.
It preserves all 214 indexed source files, the launcher, regression evidence and
independent review artifacts. Source index SHA256:
`eda5f34fbbe5a6ac847939385c5159e7a92af5963f894412808256f488b64ee3`.
Review report SHA256:
`89ccdc810dc7e4814e54172f22cf8229cd4dd4de1b56faa7b5e9c1e391f26aaa`.
Only this status documentation, project-plan status and the corresponding
manifest checksum change after the reviewed source was frozen.

Prior progression: the owner accepted r9b and closed Slice 1 after
independent acceptance. Slice 2 r3 passed independent review after the owner requested
transformation and coverage refinements. The derived-variable refinement passed
independent review on r7. The owner's subsequent target/destination, chain and plot
refinement passed independent acceptance on r10. The owner's follow-up on
variable defaults, Recipe controls text and the new Variability step passed
independent review on r13. The owner's remaining create-then-switch default
issue passes independent review on r14. The owner authorized the vector-storage
optimization in r15, which passed independent review as the storage-only baseline.
The owner then requested a choice for unevaluable windows; r16 adds that choice
and passed focused independent review as the intermediate baseline.
The owner's further Valid range request adds numeric or categorical check sources
in r17. The combined candidate passed independent review and the owner's final
showcase check on 2026-11-13.
Full-app integration remains Slice 5. The accepted Slice 1
snapshots remain preserved.

## Slice 2: measurement values

Owner authorization: "looks good. Close this slice and move on to the next
one." Typed categorical/numeric filtering and explicit NA policies were
already delivered in Slice 1. This slice adds numeric range invalidation and
explicit measurement transformations in the existing Measurement values tab.
The owner subsequently requested log/root operations and coverage durations.

- Range invalidation keeps both inclusive bounds and replaces values outside
  them with NA. Existing missing values, timestamps, rows and unrelated
  variables remain unchanged.
- Calibration uses `new = old * multiplier + offset`, with finite explicit
  coefficients and an optional reference note. It applies to one numeric
  measurement column and all rows reaching the step. Missing values stay
  missing. Nonfinite input or arithmetic overflow produces an actionable
  error instead of a partially applied result. Units are not inferred or
  converted.
- Identifier, provenance, timestamp and grouping columns cannot be targets.
  The steps have versioned plain-R mappings and no expression evaluation.
- Saving automatically previews the whole ordered recipe from immutable
  source data. Apply, Reset, Undo, dragging and pending removals retain the
  accepted Slice 1 contract.
- The day plot overlays the transformed measurement on the source, breaks
  at invalidated values, and selects informative days using both removed rows
  and changed values. Step and whole-recipe summaries distinguish row loss
  from value changes. Selecting a measurement step shows its target variable.
- Checks cover independent R pipeline parity, order-sensitive compositions,
  missing/nonfinite values, invalid inputs, source/group preservation,
  repeated Apply, reactive state and the real four-person IZTECH journey.
  Independent review precedes owner handoff. Time/gap changes, aggregation
  and full-app wiring remain later slices.

The owner additionally requested the Slice 2 layout: equal-height editor and
impact cards side by side, with one full-width recipe card below. The recipe
list sits beside its navigation controls, including Apply and Reset, replacing
the fixed bottom bar. Automatic-day and one-off day selection share a centered
row. Plot explanations and diagnostics are collapsed under Preview details;
the manual preview/retry control is hidden whenever the preview is current.

### Slice 2 target, destination and chain refinement

The current editor combines Target variable and Write to. Write to starts with
the selected target's name and resets when the target changes. Keeping that name
updates the selected column; another unused name creates a column. Other existing
columns cannot be overwritten. The latest enabled created column is the default
for a new measurement, variable-filter or coverage step.

Editing a measurement step's output retargets later consumers of that output,
including branches. Reordering resolves a missing derived input through its known
parents. An in-place measurement operation moved before its creator becomes the
creator; the former creator then continues from that result. For example, moving
an add-10 operation before a multiply-by-2 creator changes `2*x + 10` into
`(x + 10)*2` in the same derived column. The raw column stays intact. Filters moved
before a creator use the available ancestor and keep their literal conditions;
an absent category simply matches no rows. These rewritten inputs are explicit in
the recipe. Undo restores both the order and all changed inputs/destinations.
Disabling or removing a still-needed producer continues to require resolving its
dependents first. No unrelated variable is substituted.

The page lists every adjusted input/destination after a chain change. Save and
reorder update the preview only; Apply still evaluates the complete recipe from
the original source and updates prepared data atomically. Mapping versions and
metadata-history generation remain compatible with saved recipes.

Global app selectize styling centers selected values while retaining search and
multi-selection. The plot is 300 pixels high. Prepare accepts the dashboard's
reactive plot settings, including linear/log/symlog, threshold, shared/page domain
and manual bounds. Dataset settings are the fallback, with symlog/1 when absent.
Dashboard now exposes this return contract; full-app parent wiring remains Slice 5.
Separate showcase sessions do not share another session's dashboard controls.
The owner explicitly accepted this boundary and clarified the Slice 5 contract:
Prepare's preview inherits the dashboard focus variable and plot settings, with
other relevant display preferences considered at integration. Participant/day
selection remains local to the preview.

Frozen candidate: `m8-s2-r8-14de6805ddc1`, `http://127.0.0.1:7413/`, source
`/private/tmp/llw-m8-s2-r8-zwonzwsz/source`. Its 208-file index SHA-256 is
`14de6805ddc166165ec99b610ccea0a6be7b7d4e4b2e009bc792de0f03b32998`.
The affected suite passes 1,663 assertions in 130 blocks with no failures,
errors or skips and three expected cancellation warnings. Command:
`RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'testthat::test_local(filter="preparation|prepare-module|metadata-model|dashboard-data", reporter="summary")'`.
Evidence: `/private/tmp/llw-m8-s2-chain-release-tests.log`,
`/private/tmp/llw-m8-s2-chain-release-results.rds` and
`/private/tmp/llw-m8-s2-chain-session-info.txt`. Intermediate mock-browser tests
were updated to acknowledge worker-derived form defaults and the saved reference
field. Actual browser checks passed new-variable defaults, chained reorder,
renaming, target reset, collision refusal and centered dropdown text. The old
7407 and temporary 7412 runtimes are stopped; owner session 7411 is preserved.
The r8 review found R17: moving an exact-category filter before a derived-variable
creator correctly retargeted its input, but editor loading dropped categories
absent from the new input catalog. The browser then could not acknowledge the saved
form. The follow-up preserves the union of available and saved literal categories
when loading a form. A real-worker regression inspects the browser update payload,
completes the load, previews the expected empty selection and saves again. R8 stays
frozen; a corrected candidate requires independent recheck.

R9 (`m8-s2-r9-6d2b850d3c87`) passes 333 assertions in 32 focused derived/module
blocks, with no failures, errors or skips and the three expected cancellation
warnings. Evidence: `/private/tmp/llw-m8-s2-r9-focused-tests.log` and
`/private/tmp/llw-m8-s2-r9-focused-results.rds`.
The dashboard browser check also found R18: global wrapped labels overrode the
compact 32-pixel selectors. The follow-up restores single-line ellipsis and
compact padding/input height for dashboard KPI selectors, preserving the full
accessible/dropdown labels. Ordinary selectors retain centered wrapped text.
This styling-only follow-up requires a focused browser recheck. R8's 320-pixel
plot-caption and coverage-arrow notes were cleared; the 336-pixel summary-total
alignment remains a small, nonblocking observation.

### R10 independent acceptance

Accepted for owner review on 2026-11-12: `m8-s2-r10-dcc985cff4e9` at
`http://127.0.0.1:7416/`, frozen source
`/private/tmp/llw-m8-s2-r10-tcl2lipx/source`. The 208-file index SHA-256 is
`dcc985cff4e900347e2cc4e4cca347dbc950f08f08bb0802d7e37001334e74ba`.
All indexed files matched before and after review. R code and tests match r9;
the only runtime change is the scoped compact-selector CSS correction.

The independent reviewer closed R17 (saved categories, editor recovery,
preview/Apply/reopen and Undo) and R18 (compact labels at desktop, 390 and
320 pixels, full-label dropdown and keyboard selection). Ordinary wrapped
coverage labels and participant chips remain readable at 320 pixels. A real
IZTECH two-step derived-variable journey passed through Apply. Independent R
evidence covers 20 chain/filter/source-preservation scenarios, 12 plot-setting
combinations and four exact browser-filter reproductions. The r8 and r9 suite
results above were inspected separately, not counted as reviewer reruns.

R16 remains a minor, nonblocking 336-pixel totals alignment issue. R04 remains
the existing labels/units integration item. The owner-accepted Slice 5 boundary
includes inheritance of dashboard focus variable and plot settings. No browser
console errors were found; Prepare retains five existing datepicker warnings.

Acceptance report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r10-review/review-acceptance.md`,
SHA-256 `ff2145a07c0b25d36323c847fbcc1bd5581b1ef2d422bce665941a1968366b2f`.
Its directory holds screenshots, geometry, console logs and source checksums.
The programmer's final integrity record is
`/private/tmp/llw-m8-s2-r10-tcl2lipx/programmer-final-integrity.json`.
Released review runtimes 7413, 7414, 7415 and 7417 are stopped. Owner draft
7411 is preserved; 7416 holds an unapplied two-step example for owner review.
Only documentation/manifest handoff records are updated after freezing. Slice 2
remains open for owner acceptance; no later slice, commit, push or deployment.

### R11 owner follow-up

The owner found Coverage still defaulting to MEDI while editing a later Duration
step after MEDI_processed had been created. Reopening a saved step initialized
inactive tools from the raw dataset context. Defaults now come from the recipe
prefix reaching that step, including the latest enabled derived variable and
its cached catalog. The saved operation then restores its explicit settings.
Consequently a saved raw-variable selection stays raw, and a variable created by
a later step cannot become an earlier step's default. A regression covers
Duration-to-Coverage, the measurement and variable-filter defaults, saving the
replacement, an explicit raw selection, and the original creator's editor.

Recipe controls now use larger text for the heading, buttons and Apply/Reset
explanation. This remains a bounded refinement of Slice 2 and requires a focused
independent recheck before owner handoff. The current 7416 owner draft is preserved.

### R12 variability and direct-save defaults

The independent r11 review reproduced R20: switching tools after directly saving
a new Duration step still retained inactive raw-variable defaults. Saving now
refreshes only unused selectors from the preceding recipe context. Explicit
choices in the saved operation are preserved. R11 is superseded, not accepted.

The owner authorized a Variability measurement operation, and confirmed sample
SD, percentage as CV (100 × SD / |window mean|), and unchanged target values for
unevaluable windows. Minimum window coverage defaults to 80% and is adjustable.
The Target variable and Write to contract is shared with other measurement steps.
A separate numeric source drives the check and is recorded in transformation history.
It stays pinned through target changes. Only an unavailable source falls back
through recorded lineage, with the adjustment displayed and recipe undo available.

Window length is elapsed seconds, minutes or hours. Centred windows are
`[t - length/2, t + length/2)`; trailing windows are `(t - length, t]`.
Sample SD uses finite source readings inside those bounds. Threshold comparison
is strict; equality leaves the target unchanged. CV uses the absolute mean;
zero or nonfinite means are unevaluable. Windows remain within the intersection
of current groups and retained participant/file identities. Original timestamps
provide each stream's dominant epoch, unchanged by earlier filtering. Observed
time coverage assigns each finite observation at most one centred epoch, clips
overlaps at adjacent timestamp midpoints and intersects those intervals with the
full requested window. Missing values and gaps contribute no time; no values are
interpolated. Recording edges can therefore have lower coverage. Windows need
at least two finite source observations, sufficient coverage, no duplicate
timestamps, and a known epoch. Unevaluable windows leave targets unchanged and
are counted by reason in Measures and as a total beside the recipe step.

Focused verification includes direct R SD oracles, large-offset numerical
stability, explicit versus implicit gaps, undefined CV, threshold equality,
duplicate and singular timestamps, file/group separation, elapsed-time DST
windows, metadata inheritance, source fallback independent of target promotion,
and save/preview/edit/apply module paths. No dependency or browser-script change.
Owner 7416 remains running. Obsolete 7411 and released review 7418 are stopped.
Independent review is required before presenting this candidate to the owner.

R12 verification: 964 assertions in 79 test blocks pass, with no failures,
errors or skips and three expected cancellation warnings. Evidence is in
`/private/tmp/llw-m8-s2-r12-results.rds`, `-tests.log` and `-session-info.txt`.
The programmer's separate browser session confirms the direct-save default fix,
PIM-driven new-variable masking, auto-preview, diagnostic counts, Apply and
recipe Undo. The matching IZTECH R check records 13,503 newly missing values,
24 unevaluable windows and 34,560 retained rows, in 0.713 seconds for evaluation.
Its script is `/private/tmp/llw-m8-s2-r12-iztech-check.R`.

R13 corrects numerical edge cases found in the programmer and independent
checks: exact constant windows now produce exactly zero SD; subtraction
cancellation triggers a fresh local calculation instead of erasing small
variance; near-zero means are recomputed from actual readings for CV. Borderline
SD/CV comparisons are re-evaluated with `stats::sd()` and `base::mean()` before
strict above/below testing, so a rounding residual cannot mask equality with the
threshold. The recomputation tolerance changes no comparison semantics.
Regression tests include true tiny variability and truly nonzero near-zero means.
The measurement helper now says Target to distinguish it from the variability
check source, and phone layouts use two operation columns to avoid splitting
Transform/Variability labels. The independent r13 recheck passes.

R13 focused verification passes 108 assertions in 11 blocks, with no failures,
errors, skips or warnings. Command:
`RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'testthat::test_local(filter="preparation-variability", reporter="summary")'`.
Evidence: `/private/tmp/llw-m8-s2-r13-variability-results.rds`,
`/private/tmp/llw-m8-s2-r13-variability-tests.log` and
`/private/tmp/llw-m8-s2-r13-session-info.txt`. The earlier 964-assertion r12
suite is supporting evidence, not a full r13 rerun. R 4.6.1 uses shiny 1.14.0,
bslib 0.11.0, LightLogR 0.10.3, dplyr 1.2.1 and testthat 3.3.2.

The frozen r13 IZTECH check and browser agree: a centred 10-minute PIM SD below
0.01, at 80% minimum coverage, writes MEDI_processed with 13,503 newly missing
values and 24 unevaluable windows, retaining all 34,560 rows and source columns.
The R evaluation took 1.073 seconds; script, results and versions are under
`/private/tmp/llw-m8-s2-r13-iztech-check.R`, `-check.rds`, `-check.log` and
`/private/tmp/llw-m8-s2-r13-iztech-session-info.txt`. This is a UI verification
example, not a recommended scientific threshold. It remains unapplied in the
programmer's final showcase session.

### R13 independent acceptance

Accepted for the owner's final check on 2026-11-12: `m8-s2-r13-1946f73d90c0`
at `http://127.0.0.1:7420/`, frozen source
`/private/tmp/llw-m8-s2-r13-szgkpq24/source`. The 210-file index SHA-256 is
`1946f73d90c08e62de0a31ac216a7794a296c1e9800e7a5ec8833059dcd40f5a`.
The programmer verified all indexed files unchanged and all 207 non-handoff
workspace files matching that snapshot. The integrity record is
`/private/tmp/llw-m8-s2-r13-szgkpq24/programmer-final-integrity.json`.

Independent verification passes 46 numerical fixtures, including the corrected
boundary cases and small-value checks. Browser checks cover the real-data SD and
trailing CV paths, source fallback, Undo, Apply and Reset. Operation labels remain
on one line at 320 and 390 pixels, with aligned card edges and no page overflow.
A nonblocking Measures layout issue remains: long diagnostic labels require
horizontal scrolling to see Preview values. The earlier narrow-screen totals
alignment observation and Slice 5 dashboard integration boundary remain open.

Review report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r13-review/review-acceptance.md`,
SHA-256 `1c6e191d4cccabe80056dbd3333f8fc165a801d8138077a99f01fcb163ba9251`.
R21-R24 are closed and R20 remains closed. The remaining presentation notes
are R25 (Measures label wrapping) and R16 (narrow totals baselines); R04 remains
the full-app labels/units integration note. The reviewer verified all 210 frozen
files before and after review and closed its private 7420 tab.

The reviewer released port 7419, and its tab and runtime are closed. The older
7416 owner draft is preserved. Port 7420 holds the unapplied variability example.
Only handoff documentation and its manifest checksum change after freezing.
Slice 2 remains open for owner acceptance; no later slice, commit, push or
deployment is included.

### R17 independent condition sources

The owner requested a source selector for Valid range that follows the same pinned
source and lineage fallback contract as Variability. Numeric intervals and exact
categories can now mask the target, or write the masked target into a derived
column. The user chooses matching or non-matching source values, plus a separate
Keep/NA policy for missing source values. Factor levels use their labels, not their
underlying integer codes. Numeric bounds are inclusive.

New condition steps use invalidation mapping version 3. Existing version 1/2
invalidation recipes retain their original target-based range behavior. The source
is independent of target retargeting; reordering/removal follows only a known
source lineage when that source becomes unavailable. Metadata inherits from the
target and records the check source, predicate and missing-source choice. Rows,
timestamps, groups and unrelated columns are retained. Saved categorical choices
remain literal if an earlier step removes their levels. The first 500 distinct
categories are offered, with existing selections retained and a visible note when
the catalog is incomplete.

The r15 optimized rolling engine and r16 unevaluable-window policy are preserved.
The focused values, derived-variable, variability and module suite passes 921
assertions in 64 blocks, with no failures, errors or skips and three expected
cancellation warnings. The numeric/category matrix checks both matching
directions, missing-source policies and output destinations against explicit R
expectations. It also covers legacy mappings, pinned source fallback, metadata,
saved literal categories and a real-worker Edit/Apply journey. An intermediate
test exposed inactive condition controls being reset during a Transform save;
the correction updates those defaults only when the default source changes.
Evidence: `/private/tmp/llw-m8-s2-r17-evidence/`, with the durable copy at
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/condition-source-r17/`.
An additional programmer R oracle passes 24 complete-output cases against the
real IZTECH and synthetic factor fixtures, including legacy parity.

Combined independent acceptance: `m8-s2-r17-f3ebdb5e9888`, served at
`http://127.0.0.1:7424/` from `/private/tmp/llw-m8-s2-r17-a6lhjgx3/source`.
All 210 files match source-index SHA-256
`f3ebdb5e9888d014b77096321bc0b1c5e1e2a219a4e855e7af3e83cba28a75a0`.
The reviewer independently verified 64 complete-output condition cases, legacy
recipes, history, source/target lineage, invalid recipes and the exact browser
chain in R. Browser checks passed numeric and categorical sources, keyboard
selection, missing-source policies, Edit/Apply/Reset, producer reorder/removal
fallback and the adjacent Transform flow. Six-width alignment checks passed.
The two existing minor presentation notes, narrow preview totals and long
diagnostic labels, remain nonblocking; full-app settings/focus wiring is Slice 5.

Acceptance report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r17-review/review-acceptance.md`,
SHA-256 `9cc026bccd6e31da77dcfcc8b6ad948d5396e68ac5e1422019a4525c99479074`.
It incorporates the accepted r15 optimization and r16 policy checks. The rolling
file is byte-identical to r16; no additional performance claim or benchmark is
introduced. The programmer's final showcase contains an unapplied categorical
PIM = 0 condition writing MEDI_processed, with 17,083 newly missing values and
all 34,560 source rows retained. The numeric [0, 0.01] variant was also exercised.
Only handoff documentation and manifest records change after freezing. The owner
accepted this exact candidate on 2026-11-13 with "slice accepted", closing Slice 2.
No application source, commit, push or deployment is part of this closeout.

### R16 unevaluable-window policy

The owner requested a choice between keeping the target unchanged and setting it
to NA when a variability window cannot be assessed. Keep unchanged remains the
default, including for existing version 1/2 recipes. Newly saved variability
steps use mapping version 3 with an explicit `unevaluable` policy. Both in-place
and derived outputs are supported; variable history and recipe labels record the
choice. Reordering and source fallback retain the policy.

The NA policy masks every unevaluable target window, including insufficient
coverage, fewer than two finite readings, duplicate timestamps, unknown cadence,
missing timestamps and undefined CV. Evaluability diagnostics and their reasons
are unchanged; source columns, groups and timestamps are retained. The policy can
therefore introduce more missing values near recording edges or gaps, by explicit
user choice. The rolling calculation and r15 vector storage are unchanged.
Frozen candidate `m8-s2-r16-04598cb55a1a` is at port 7423, source
`/private/tmp/llw-m8-s2-r16-o3vudz7p/source`, with 210-file index SHA-256
`04598cb55a1a9fd0b4d22bf34bbcc5c2b1b8239ffc1d4f4c9fd5d80c709e7240`.
The affected preparation suite passes 1,117 assertions in 85 blocks, with no
failures, errors or skips and three expected cancellation warnings. All 98 prior
fixtures exactly retain legacy/default output; the explicit NA policy masks only
unevaluable positions and leaves diagnostics unchanged. Real IZTECH missing-value
counts are 13,503 versus 13,527 for SD (24 unevaluable), and 16,937 versus 30,472 for
CV (13,535 unevaluable). The programmer's browser confirms the selected NA policy,
13,527 missing derived values, 24 labelled unevaluable windows and saved-form
restoration. Evidence is under
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/unevaluable-policy-r16/`.
Independent review accepted this intermediate baseline, including all 98 fixtures,
real SD/CV vectors, legacy recipes, history, source fallback and Apply/Reset.
Browser checks covered keyboard selection, saved-form restoration and six widths
from 320 to 1440 CSS pixels without a new alignment issue or page overflow.
Report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r16-review/review-acceptance.md`,
SHA-256 `93ac4ad4bd43d9257831bfa4394d4723eedb0c7c3d1eb3ee45f441e312f45469`.
The reviewer released ports 7422/7423. Their own programmer tabs and runtimes
are closed; the owner's 7416/7420/7421 sessions remain untouched. The combined
r17 acceptance above incorporates this intermediate review.

### R15 vector-storage optimization

The owner authorized promoting the vector-storage experiment on 13 November.
The bounded change replaces repeated result-data-frame cell assignments in
`preparation_rolling_variability()` with preallocated typed vectors and one final
data-frame assembly. The rolling arithmetic, half-open time windows, coverage,
source grouping, numerical fallback checks and masking wrapper stay unchanged.
The return columns and their types remain the same, including empty inputs and
invalid-cadence early returns. No dependency or UI behavior is added.

Verification compares exact outputs and all diagnostics with frozen r14, including
the earlier 98-case fixture set, 70 exact threshold stress cases, real IZTECH SD/CV
paths and empty/invalid-cadence/subprecision boundaries. Fresh paired timings used
a pre-run source, script and input fingerprint and separate allocation checks.

Candidate `m8-s2-r15-d511d86d3e51` runs at `http://127.0.0.1:7422/`, with
source `/private/tmp/llw-m8-s2-r15-g75mtcoy/source`. Its 210-file index SHA-256
is `d511d86d3e5187a815279943788e48d1de0ef502cb9fe677417fb7dde9badff9`.
All indexed files remain unchanged after verification. The focused suite passes
505 assertions in 48 blocks with no failures, errors or skips and three expected
cancellation warnings. The initial sandboxed run skipped worker checks; the
subsequent local-worker run completed all of them.

Exact r14 parity passes for all 98 fixtures (output data, diagnostics and complete
raw kernel results), 70 lossless threshold stress cases, 90 boundary combinations
and both real IZTECH paths. Fresh masking-operation medians are 1.020 to 0.077
seconds for IZTECH, 14.392 to 0.287 seconds for 100,000 minute-spaced rows, and
13.732 to 0.285 seconds for 100,000 second-spaced rows with a 60-minute window.
R15 handles one million rows in 2.229 seconds; r14's warmup was interrupted at
30.146 seconds by the cooperative limit, so no r14 median or ratio is claimed.
Separate cumulative IZTECH allocation is 9,732.878 MB versus 35.065 MB, not peak
RAM. Three repeats, observed ranges, raw rows and the pre-run fingerprints are
recorded in `/private/tmp/llw-vector-storage-r15-20261113/` and its durable copy:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/vector-storage-r15/`.
Independent review accepted r15 as the storage-only baseline. Report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r15-review/review-acceptance.md`,
SHA-256 `d571c18eaba61d0754c6c55b845efa20460cab45eaf355bd017ab038300d1e24`.
The reviewer independently verified numerical parity and timing evidence, then
completed SD/CV, derived defaults, Apply and Reset browser journeys. No new
blocking issue was found. Final handoff awaits the r16/r17 refinements and review.
The programmer's own r15 browser session verifies
the PIM-driven MEDI_processed preview, including 13,503 newly missing values,
24 unevaluable windows, and the preserved new-variable defaults.

Potential downsides: parallel result vectors require keeping the return schema
aligned; storage still grows with the number of observations; existing custom
floating-point behavior is retained rather than improved; and measured operation
speedups do not directly predict full preview or browser latency. Timing results
are specific to the tested workloads and machine. The previous candidate remains
available for rollback and the owner's draft is preserved.

### R14 creator-to-next-step defaults and slider comparison

The owner reproduced a distinct default problem on r13: after saving a
new-variable creator, the editor remained attached to that creator. Its input
context necessarily excluded its own output, so switching from Variability to
Transform or Coverage retained MEDI. Save as new now finishes a creator edit and
opens a next-step draft with the newly created variable selected in Target,
Write to, variability source, variable filtering and Coverage. Operation settings
remain available in the draft. The saved creator keeps its original input and
source; selecting its Edit control restores those exact settings. Ordinary edits
and saves that do not create a variable retain their previous behavior. The
recipe remains unapplied until Apply. Regression checks cover the owner's
Valid range -> Variability creator -> Transform/Coverage route and re-editing.
Independent review passed before owner handoff, including immediate Transform,
Variable filter and Coverage defaults, creator re-editing, collision refusal,
Undo/Apply recovery and the 320-pixel layout. Existing nonblocking R16 and R25
layout notes remain. Evidence directory:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r14-review/`.
Its `review-acceptance.md` closes R26 and has SHA-256
`ebd695afd283b5e5c40409aad887e344b5018ec8be3d9f19f721622373d2f1f3`.

Frozen candidate: `m8-s2-r14-4cd689ffc532` at `http://127.0.0.1:7421/`,
source `/private/tmp/llw-m8-s2-r14-2ppmorcx/source`. All 210 indexed files match
SHA-256 `4cd689ffc5322305171b20bf0655d575f7749783830606721f1161731ceb74d3`.
The focused derived-variable, variability and module suite passes 493 assertions
in 46 blocks, with no failures, errors or skips and three expected cancellation
warnings. Evidence: `/private/tmp/llw-m8-s2-r14-release-tests.log`,
`/private/tmp/llw-m8-s2-r14-focused-results.rds` and
`/private/tmp/llw-m8-s2-r14-session-info.txt`. The actual browser reproduces the
owner's Valid range, Variability creator, Transform and Coverage route with
MEDI_processed selected for the next step. The numerical engine is unchanged.

The owner separately requested a read-only comparison with slider. The
experiment uses frozen r13 and installed slider 0.3.3, with matched half-open
elapsed-time windows and unchanged grouping, coverage and masking policies.
All 98 preset fixture outputs and real IZTECH SD/CV decisions agree. Additional
cutoffs chosen between floating-point results expose differences, so parity is
not claimed for every possible threshold. A local-vector diagnostic preserves
the custom arithmetic and removes repeated data-frame copying; this reverses
the speed comparison in its measured cases. Neither experimental implementation
is installed in the app. Scripts, timing runs, outcomes, environment and report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e4-b062-7122-96c7-577c3b4c170e/variability-slider-comparison/`.
The comparison and its audit supplement passed independent methodological review
with the explicitly documented numerical and provenance limitations.
Verdict: `/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-slider-independent-audit-20261112/methodological-verdict.md`,
SHA-256 `cb419c7147148523b200d101d16294dfc847fc6a7f0235ec577a3ae81ba57244`.
The audit supplement preserves exact threshold cutoffs and statistics in RDS
and hexadecimal CSV, and replays all 70 targeted cases without changes to their
recorded decisions. Decimal CSV remains a display format. A positive window
shorter than timestamp precision makes the experimental slider adapter error;
the custom implementations leave those windows unevaluable and preserve the
target. The report also states the cooperative warmup timeouts, centered-SD
timing scope and retrospective provenance limit of the interrupted initial run.

### Slice 2 derived-variable refinement (r4-r7)

Owner requests: choose replacement or a new variable for measurement operations,
inherit metadata and retain transformation history, suggest an editable collision-free
name such as MEDI_processed, and make informative-day selection a regular button.
The approved short whole-group coverage hint is included.

Measurement mapping version 2 adds an explicit output column; version 1 replacement
steps remain compatible. All source column names and names reserved by other recipe
steps are protected, including disabled steps. Defaults add _2, _3 and so on when
needed. New targets remain numeric measurements available to later steps. Reordering,
disabling or removing their producer is rejected while an enabled dependent step
still needs it. Empty filtered results retain the derived-column schema.

The applied recipe supplies a derived metadata layer referencing immutable source
metadata. It retains the inherited label, unit, calibration, factual fields and
metadata provenance, plus each contributing measurement operation and parameters
in order. Branches copy the history at creation, excluding later parent changes.
For nonlinear transformations the source unit is retained explicitly, while display
metadata identifies transformed values. Apply, recipe Undo and Reset restore the
corresponding data and history together. The Variable history tab exposes this
information. Source/preview plots compare the root source with the derived target.

Verification uses R and the locked project library. The synthetic derived-variable
tests exercise independent expected vectors, chaining, metadata inheritance,
name conflicts, dependency order, empty results and real-worker form round trips.
Independent review remains required before owner handoff. The owner's port 7407
session is preserved. Older 7403/7406 Shiny processes and leftover syntax-check
processes were stopped at the owner's request.

Frozen derived-variable candidate: `m8-s2-r4-a09a9206db9c`, served at
`http://127.0.0.1:7408/` from `/private/tmp/llw-m8-s2-r4-zobr_oli/source`.
The 208-file index SHA-256 is
`a09a9206db9c74420090f568eca7b22fb21133a094091d1da329ef3bc5c052b0`.
The frozen files matched the tested workspace after the test run. Verification:
`RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'testthat::test_local(filter="preparation|prepare-module|metadata-model|dashboard-data", reporter="summary")'`.
All 1,583 assertions in 120 blocks passed, with no failures, errors or skips;
three warnings are the existing expected cancellation checks. R 4.6.1,
LightLogR 0.10.3, shiny 1.14.0, bslib 0.11.0, dplyr 1.2.1, testthat 3.3.2,
mirai 2.7.2. Log: `/private/tmp/llw-m8-s2-derived-tests.log`;
results: `/private/tmp/llw-m8-s2-derived-test-results.rds`.
The new derived fixture is synthetic; the browser used the four-person IZTECH
fixture. Browser checks confirmed the default and _2 naming, collision feedback,
custom target field, new-column range preview, Apply, visible history and
later-step column selection. Independent acceptance is pending.

The independent r4 review confirmed R14: a disabled consumer with a renamed
input could become stuck while loading its editor. The follow-up preserves that
saved input as a labelled unavailable option so the form loads and can be
retargeted. Orphaned category selections likewise remain editable. History list
numbering was also simplified. A focused recovery run covers preparation-derived
and prepare-module; the corrected frozen build still requires independent recheck.

R15 was also found during independent r4 review: exact-category choices for a
derived variable disappeared when form edits invalidated the preview. The follow-up
keeps worker-computed per-prefix catalogs separately from the applyable preview,
bound to the dataset ID, raw checksum and exact unchanged recipe prefix. Dataset
switches clear them. Inspecting an applied derived recipe rebuilds the catalogs in
the worker. Category choices and missingness now describe the data entering the
step, and changing an earlier step prevents reuse of its stale catalog. Real-worker
tests cover opening an applied derived recipe, choosing exact categories while the
form is dirty, saving, applying and raw-column preservation. Independent recheck
of the combined corrected candidate remains required.

The combined r6 recovery suite passed 266 assertions in 24 blocks, with no
failures, errors or skips and the three expected cancellation warnings. Evidence:
`/private/tmp/llw-m8-s2-derived-categories-tests.log` and
`/private/tmp/llw-m8-s2-derived-categories-results.rds`. A final catalog correction
excludes the temporary row-tracking column from variable choices, while retaining
an original column with the same name. Its focused derived-variable run passed
84 assertions in nine blocks with no warnings, failures, errors or skips:
`/private/tmp/llw-m8-s2-derived-marker-tests.log` and
`/private/tmp/llw-m8-s2-derived-marker-results.rds`. The command was
`RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'testthat::test_local(filter="preparation-derived", reporter="summary")'`.
This final delta passed the independent browser check on r7.

Final candidate: `m8-s2-r7-bcedbce71b7f` at `http://127.0.0.1:7411/`,
frozen source `/private/tmp/llw-m8-s2-r7-9bw94zde/source`, 208-file index
SHA-256 `bcedbce71b7f83857990c04f29c3c0a79f027b68c2a44da808f308158c9f41a5`.
The only runtime difference from r6 is exclusion of the generated row marker
from variable catalogs. Frozen source hashes still match after browser checks.
The handoff example previews `MEDI_processed = MEDI * 2 + 1` on the four-person
IZTECH fixture and has not been applied. The owner's edited r3 session on 7407
is preserved. Superseded 7408, 7409 and 7410 review runtimes were stopped after
the reviewer released them, in addition to the earlier 7403/7406 cleanup.

The independent reviewer accepted r7 for owner inspection on 2026-11-12.
R13 (history numbering), R14 (disabled-consumer editor recovery) and R15
(derived categories during form edits) are closed. Three minor, nonblocking
layout notes remain: plot-caption clipping and the coverage dropdown at 320
pixels, and uneven totals at 336 pixels. The default viewport was restored.
No later slice or full-app integration is opened by this review result.
Report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r7-review/review-acceptance.md`,
SHA-256 `ad898a0c9159ca19315afcc9dd54774993b6d105300a3518099d4c0cf81635d6`.
Independent R checks reproduced derived values and category selection, verified
metadata/source preservation and catalog invalidation, and confirmed generated
marker exclusion. All 208 frozen source hashes matched before and after review.
Only this acceptance record, the project plan and its manifest checksum changed
in the workspace after freezing; runtime, tests and fixtures are unchanged.

### Slice 2 r3 independent acceptance

The Transform editor retains scale/offset and adds logarithm (base), root
(degree), power (exponent), exponential (base), and absolute value. Each
operation has typed arguments and a readable formula. Undefined or nonfinite
results stop with guidance by default; the user can explicitly mark them NA.
Existing missing values stay missing. Odd roots allow negative readings;
even roots do not. Preview axes and visible notes identify nonlinear values
as transformed and do not claim the original measurement unit. The original
source and metadata are retained. Existing affine recipe steps remain valid.

Coverage now offers minimum available percentage, minimum available duration,
and maximum missing duration. Durations accept seconds, minutes, hours or
24-hour days. Minimum available duration must be positive; maximum missing
may be zero. Equality passes. A failing unit removes its entire current group
or local group/day, as stated beside the controls. Calculation delegates to
`LightLogR::remove_partial_data()`, with negative Duration for minimum available
and nonnegative Duration for maximum missing. Existing percentage mappings
retain versions 1/2; duration mappings use version 3. Current grouping and all
four observed/gap-aware bases remain supported.

R checks cover explicit mathematical values, domain failures and recovery,
compositions, prefix plots, immutable source, form round trips, real workers,
and parity with public duration coverage across grouping/basis combinations.
Candidate `m8-s2-r3-761164f70f3f` is served on port 7407 from
`/private/tmp/llw-m8-s2-r3-3wbzea_1/source`. Its 206-file source-index SHA-256 is
`761164f70f3faad7d16e19cd8c02adc870b8bd44c93d4a9af9f6dddacab78825`.
The frozen focused suite passed 712 assertions across 52 blocks with zero
failures, errors or skips and three expected cancellation warnings. Logs and
`test-results.rds` are beside the frozen source. R 4.6.1, LightLogR 0.10.3,
Shiny 1.14.0 and dplyr 1.2.1 were used through the existing project library.
The root browser check confirmed duration auto-preview, a logarithm domain
failure, explicit NA recovery, unit wording and Apply from source.
The independent reviewer accepted this revision for owner review on
2026-11-12. Independent verification passed 22 transformation pipelines and
63 coverage comparisons, plus real-browser validation, saved-form restoration,
Apply/Reset, keyboard flows and six responsive widths. There were no blocking
findings. Two minor 320 px fit issues remain: the existing plot-caption clipping
and overlap of the long coverage-basis label with its dropdown arrow. The
known R04 metadata integration item remains assigned to full-app integration.

The frozen candidate remains unchanged. At acceptance, only this note, the
project plan and manifest metadata were updated after review. The prior owner session
at port 7406 remains available. The reviewed showcase is open at port 7407,
with a combined transformation and duration-coverage example. Its result was
also checked against direct R arithmetic followed by `remove_partial_data()`.
Owner acceptance is still required before another slice begins.

Independent report: `/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r3-review/review-acceptance.md`.
Report SHA-256: `563495eea176a81c51b0533920c8c1600cbf4ff9d10b3b5c92e59b341b5244b4`.

Owner copy follow-up: the whole-group hint now reads "The entire current
group is removed if it fails the threshold." The independent reviewer
confirmed the exact one-string change and matching manifest checksum. R parse
passed; no functional retest was needed. This workspace-only copy update will
ship in the next showcase build. The owner's unsaved port-7407 session and
frozen r3 remain untouched.

### Slice 2 r2 independent acceptance

The reviewer accepted `m8-s2-r2-e360c61a912d` on 2026-11-12 for owner review.
The showcase is at `http://127.0.0.1:7406/`, served from
`/private/tmp/llw-m8-s2-r2-_knyx5dp/source`. All 206 frozen indexed files matched;
the source-index SHA-256 is
`e360c61a912d2e1a7adae6268ac6c7a88f8d32a9103a3c9bd62aee17c973060a`.

R1 review required R09: background execution replaced an actionable
calibration-overflow error with generic retry guidance. R2 preserves the safe
public error through a preparation-specific worker response and tells the
user to edit and save the measurement step. Applied data stay unchanged and
Apply remains unavailable. Real-worker tests cover both overflow and infinite
input, followed by successful recovery. Unexpected failures retain the common
sanitized error path. R10, stretched badges beside wrapped titles, is also
closed by keeping badges at their intrinsic height.

The frozen focused suite passed 555 assertions across 46 blocks, with no
failures, errors or skips and three expected cancellation warnings. Six
independent explicit IZTECH pipelines and Sass compilation passed. The
reviewer independently verified the transformations, lineage, prefix plots,
real browser flows, worker recovery and responsive layout. Full-package
integration and resource benchmarks were not repeated for this slice.

The independent report is
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s2-r2-review/review-acceptance.md`,
SHA-256 `2ed500fd5e92b4f9fef76be348be84ce7fe1ef5426fde58d362e1bef09960ccd`.
It records 15 exact independent pipelines, 28 prefix views and 11 expected
rejections, plus 82 passing focused measurement assertions with real worker
sockets available and no skips. Browser checks covered 320, 390, 768, 1024,
1200 and 1440 CSS px, with no page-wide horizontal overflow or console errors.
The five existing datepicker deprecation warnings remain unchanged.

R11 remains a nonblocking visual improvement: at 320 CSS px the plot caption
clips the end of the time-zone name, while the full zone remains visible in
the editor. R04 labels/units remains the known full-app integration item.
The complete r1 and r2 packets, tests and browser evidence are preserved.
Owner acceptance is still required before Slice 3; integration, commit, push
and deployment are not authorized by this independent acceptance.

## R9 duration groups, step ordering and pending removals

Duration offers both "Per current group" and "Per participant and file".
The former evaluates the actual incoming `dplyr::group_vars()` and uses one
group when the data are ungrouped. The latter retains the existing `identity`
recipe mapping, using participant and retained source/file identifiers.
Existing recipes therefore keep their meaning. Each scope anchors on the
first or last observation reaching that step, after earlier filters. The
editor names the current grouping columns. Defining new analysis groups
remains Milestone 9.

Filter button icons and labels are centered. Removing an enabled applied
step leaves a red "Removal pending" entry showing its applied definition.
It is excluded from the draft preview, while the prepared data retain its
effect until Apply. The entry is derived from the applied and draft recipes,
so Undo, Apply, Reset and dataset changes cannot leave a separate stale list.
Steps that were never applied disappear immediately when removed.

The owner explicitly authorized the new `sortable` dependency. Version 0.6.0
and its dependencies are recorded in the project lockfile and manifest.
The standard R widget and capture helper add drag grips to the existing
radio selection list. No bespoke browser code is added. Earlier/Later
controls remain available for keyboard ordering. R validates that every drag
is an exact permutation of the current stable step IDs, rejects stale or
invalid lists and unsaved form edits, and updates the automatic preview and
recipe Undo history. Widget initialization publishes the current order so
the same drag can be repeated after Undo. Pending removals are outside the
draggable list. The selected step remains selected when the order changes.

Verification covers differing current-group, participant and file anchors,
ungrouped behavior, prior filters, preserved source/grouping, pending removals
through Undo/Apply/Reset, drag ordering and repeated Undo, and a real
four-participant IZTECH browser journey.
The full project suite and resource benchmark are not repeated for this
bounded revision.

### R9b independent acceptance

The reviewer accepted build `m8-s1-r9b-6eb6a5f6c7dd` on 2026-11-12 for owner
design feedback. The showcase is at `http://127.0.0.1:7403/`, served from
`/private/tmp/llw-m8-s1-r9b-ptkqikho/source`. All 204 indexed files matched
before and after independent review. The source-index SHA-256 is
`6eb6a5f6c7ddda1d602ee93a0f765e855d7d0eb479f0d853295173c19c398444`.

Focused verification passed 473 assertions across 39 blocks, with no
failures, errors or skips and three expected existing cancellation warnings.
Eight supplied real-data duration comparisons and Sass compilation passed.
The reviewer independently passed 40 exact duration comparisons against
public `LightLogR::filter_Datetime()`, including group definitions that differ
from participant/file streams, and reproduced five browser-recipe counts.

Visible internal-browser checks passed real dragging in both directions,
repeated dragging after Undo, keyboard Earlier/Later, selected-step retention,
automatic preview, both duration scopes, pending-removal state through
Undo/Apply/Reset, and automatic or pinned participant/day selection. Checks
at 320, 390, 1024, 1280, 1366 and 1422 CSS px found no horizontal overflow.
The full plot and both save buttons clear the footer at 1366 by 768 and
1280 by 800. No browser console errors were recorded. The five existing
datepicker warnings caused no observed failure.

The acceptance report is
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s1-r9b-review/review-acceptance.md`,
SHA-256 `2d9b4d50a796f6d77061e762a2b4ccf384a6567268ea2c81f75dbf1c996c9868`.
Its directory holds the independent R script, results, screenshots and
checksums. No new acceptance finding was opened. R04 remains the known
nonblocking labels/units requirement for full-app integration. This decision
does not close Slice 1 or authorize Slice 2, integration, commit or push.

## R8 combined owner refinements

The owner found the duration option "Per retained participant/source identity"
unclear. It now reads "Per recording" in the selector and saved-step summary.
The help explains separate counting for each participant in each source file,
with an example under "How duration is counted". Missing file information is
still explained explicitly. Other scope descriptions use start/end wording
instead of "anchor". The stored scope keys and duration calculations are unchanged.

The Edit, Earlier, Later, Enable/disable and Remove buttons grow to fill each
row, including wrapped rows on narrow screens.

Coverage now offers complete local days, observed days, whole groups including
gaps, and whole observed groups. New coverage steps use mapping version 2 and
preserve the incoming dataset's actual `dplyr::group_vars()`, as the owner
specified. An ungrouped dataset is one group. The current grouping columns are
shown in the editor. All four choices call public
`LightLogR::remove_partial_data()` with explicit `by.date` and `handle.gaps`.
Gap handling uses that function's full-day outer boundaries. The returned data
contain only exact original rows, with original grouping retained. Version 1
daily coverage recipes retain their historical participant/source/file grouping
until edited, so stored recipes do not silently change meaning.

The preview has an "Automatically choose a day" checkbox, checked initially.
Saved recipe updates reassess the informative day while checked. Manually
selecting a participant or date clears it and holds that view across updates.
Checking it resumes automatic selection immediately; the existing one-off
button chooses again without changing the checkbox. Dataset changes reset the
choice. Apply continues to rebuild one recipe from the immutable source.

These coverage and reactive changes require the focused preparation suite,
direct public-function parity checks, and browser checks before independent
review. Rendering and Sass checks cover the wording and button refinements.
The full project suite and resource benchmark are not repeated.

For the new vision-first and real-world-data guidance in project-plan.md
Sections 2.7 and 2.8, this bounded revision keeps the existing compact editor,
adjacent impact preview and single-source recipe model. The primary browser
journey uses all retained observations of the first four sorted IZTECH
participants, with subset provenance preserved by the showcase constructor.
Synthetic cases supplement it for named grouping, gaps, singleton and reactive
state tests. A new vision slice is not retrofitted into M8; the new milestone
sequence applies from M9. No broader data-access or implementation scope is
added here.

### R8b independent acceptance

Accepted build: `m8-s1-r8b-c4ab4fd965e5`, running at
`http://127.0.0.1:7401/`. Frozen source:
`/private/tmp/llw-m8-s1-r8b-aoe9um57/source`. All 204 indexed files matched
before and after review; source-index SHA-256:
`c4ab4fd965e5e9a24213019f0650270167b439b4b989accc19a92be408cc1c60`.

The independent report is
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s1-r8b-review/review-acceptance.md`.
Report SHA-256:
`128c0c1fffc7b4bc2edc49526a97f31d2dcba6b90245bc116bd4babe04bfb8dd`.
It accepts the combined revision for owner feedback without a new acceptance
finding. All 18 independent R coverage comparisons passed, including actual
groups, ungrouped data, gaps, missing values and legacy mappings. The focused
preparation suite passed 401 assertions in 36 blocks, with zero failures,
errors or skips and three expected worker-cancellation warnings.

Browser review confirmed duration wording, all four coverage choices,
automatic versus pinned selection through saved edits and recipe actions,
one-off selection, keyboard re-enabling and dataset reset. Recipe buttons fill
their rows at six tested widths. The representative clock form and full plot
still fit above the footer at 1280 by 800 and 1366 by 768. Longer forms and
expanded help use ordinary vertical scrolling. The tightly batched numeric
input timing observation did not reproduce as a new acceptance defect in
ordinary interactions; the existing stale-result protection and recovery held.
R04 full-app labels/units integration remains nonblocking. Earlier performance
and full-project findings are unchanged. No later slice has started.

## R7 compact layout and dashboard scale

The owner requested the editor and full preview plot alongside one another
without initial desktop scrolling. The editor now has natural height, with the
recipe directly below it, and impact begins at the top of the right column.
Toolbar, introduction, dataset context and plot selectors use compact rows.
The editor title moves into its card heading. Apply and Reset stay visible.
Narrow screens retain normal stacking and vertical scrolling.

The first r7 review found eight pixels of the plot behind the footer at a
1366 by 768 CSS-pixel viewport. R7b reduces the plot height from 240 to 220
pixels, retaining the existing control sizes and axis behavior. The original
r7 source remains frozen for comparison.

The same review exposed a preview-selector lock after refreshing an unchanged
view. R7c only waits for browser input updates when a selection actually changes.
A new module regression reproduces the previous lock, then checks that selecting
another participant updates the plotted timestamps and values without altering
source data. The preparation test files are rerun for this reactive correction.

The pure `preparation_preview_plot()` renderer reuses `dashboard_symlog_axis()`
and `LightLogR::symlog_trans()` with threshold and scale equal to 1, as requested
for this showcase. It shares the dashboard's axis breaks, labels, limits and
minor ticks. Plot text and alternative text name symlog. Source values, recipe
evaluation, common overlay samples, gaps and row counts are unchanged. Live
dashboard preference wiring remains part of later full-app integration.

This bounded refinement uses focused R plot/source checks, Sass compilation,
browser layout checks and the required independent review. The complete project
test suite and resource benchmark are not rerun for this bounded change.
R6's numerical/filter and performance evidence remains historical, without
claiming a new full regression pass. No dependencies or custom JavaScript.

### R7c independent acceptance

Accepted build: `m8-s1-r7c-66cfb8c68b5f`, running at
`http://127.0.0.1:7399/`. Frozen source:
`/private/tmp/llw-m8-s1-r7c-zg4dlbmb/source`. All 204 indexed files matched before
and after review; index SHA-256:
`66cfb8c68b5fdb16f223b3db4072a80f2ba0bdea2cf23bdc3304d7a6434991e8`.

The independent report is
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s1-r7-review/review-acceptance.md`.
It closes M8-S1-UX-R07 (footer overlap) and R08 (selector/plot mismatch).
Both save buttons and the complete plot fit at actual CSS viewports 1280 by
800 and 1366 by 768, with readable input sizes retained. Tablet and phone
stacking, long forms, and selector recovery after refresh, disable and Undo
passed. The plot clears the footer by 11.87 pixels at 1366 by 768.

The preparation rerun passes 311 assertions in 33 blocks, with no failures,
errors or skips and three expected real-worker cancellation warnings.
Independent R checks confirm dashboard symlog parity, signed/zero behavior,
inverse transformation and unchanged source and retained values. Frozen r7,
r7b and r6 remain preserved. R04 labels/units integration remains nonblocking;
no open acceptance issue remains in the reviewed r7c surface.

## R6 owner-requested revision

The editor occupies the left column. The right column contains the ordered
recipe and impact preview. Filtering is the working tab, with compact icon
choices for participants, combined date/time selection, duration, day/night,
variable conditions and daily coverage. Measurement-value and time/sampling
tabs explain the later operations; those transformations are not implemented
in this revision. Grouping remains a separate milestone.

Adjust current step and Save as new step have equal widths. Saving, reordering,
toggling or removing a step automatically previews the complete recipe. Apply
and Reset to source stay visible at the bottom. Apply needs a current, nonempty
preview, but no removal-acknowledgement checkbox. Large previews retain their
resource acknowledgement and hard memory limit. Undo recipe edit reverses
recipe edits, with a bounded 30-edit history. The UI does not expose stacked
applications. Every evaluation starts from immutable source rows. Apply keeps
the editable recipe and its preview; a later edit and Apply rebuilds from source.

Each step shows removed rows, the percentage of rows entering that step, the
percentage of original source rows, and cumulative source removal. Applied
status requires an unchanged prefix of the applied recipe. Changing an earlier
step therefore marks affected downstream steps Preview. Disabled steps are
labelled separately.

The plot uses a single participant and native calendar day, with grey source
and teal retained data on the same axes. Automatic selection favours a partly
changed day closest to 51% of its source rows removed. This is an observation
count fraction, not a claim of 24-hour coverage. Fully removed days are a
fallback when no partly changed day exists; an observed unchanged day is used
when nothing changes. Participant, available date and cumulative recipe step
can be selected explicitly. Counts always use all rows. The plot uses at most
2,000 evenly spaced rows from that source day, with the identical subset used
for its retained overlay. Segments break at removed observations, missing
values and recording gaps; the caption names the sampling and fallback.

Variable filters use exact categories or an inclusive numeric interval, with
inside/outside and keep/drop/only-NA choices. Numeric intervals create temporary
inside/outside categories, without modifying source columns. Choices are bounded
to the first 500 distinct source values and the UI states this limit.
Day/night can use existing photoperiod.state or explicit latitude, longitude
and solar depression via LightLogR::add_photoperiod. Unknown classification has
an explicit NA policy. Coordinates apply to all rows reaching that step.
Coverage delegates to LightLogR::remove_partial_data, separately per retained
identity and native local day. It offers complete-day gap handling or observed
span, a percentage threshold, and a chosen availability variable. Temporary
gap expansion has a conservative bound; only original rows are returned.
Coverage after other filters assesses the rows reaching that step. No
interpolation, calibration, invalidation, aggregation or time-zone conversion
is introduced.

The default remains the exact four-person offline IZTECH subset, 34,560 rows,
with original columns, grouping and Europe/Istanbul timestamps. The fixture
itself is unchanged. The candidate uses native Shiny/bslib controls and scoped
Sass. No bespoke browser JavaScript or dependencies were added.

The owner requires independent review before every handoff, including layout
refinements. The exact r6 candidate has passed that gate, as recorded below.

## R6 independent acceptance

The reviewer accepted build `m8-s1-r6-fcb0da9ce616` on 2026-11-11 for owner
design feedback. Source-index SHA-256:
`fcb0da9ce61663c6eb53395704e60295d6f4708f634f63bce486a43bc6bd232e`.
All 204 frozen r6 files and all 200 preserved r5 files matched before and after
review. The reviewed showcase runs at `http://127.0.0.1:7395/`, sourced from
`/private/tmp/llw-m8-s1-r6-ULtEw1/source`.

Report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s1-r6-review/review-acceptance.md`.
Report SHA-256:
`adbed18918491576eb1f93892c7070d94f2fcf6d737ea6a84de975adf295f92d`.

The independent pass found no new acceptance or alignment defect at 1440,
1200, 1024, 768, 390 and 320 px widths. It checked auto-preview, one-recipe
editing, repeated Apply restoring source rows, per-step denominators and
badges, the new filters, overlay selection, keyboard controls, missing-date
recovery, empty results, cancellation, failure/retry and source switching.
Independent R references matched the exact IZTECH subset, grouped clock
filters, typed/NA filters, photoperiod, coverage and overlay masks. Readable
table cells and local scrolling resolve the prior narrow-table note. R01-R03,
R05 and R06 remain protected. R04 has verified label/unit improvements but
remains a nonblocking full-app integration requirement.

Frozen focused checks pass 307 assertions in 32 blocks, with no failures,
errors or skips and three expected cancellation warnings. The exact frozen
benchmark passes the 100 ms ordinary-control p95 limit: 73 ms for hosted 250k
rows and 84 ms for local 1m rows. Both miss the 50 ms design target. Full
previews took 1.188 s and 3.069 s, with zero main-session full-payload decodes
during controls. These sequential one-worker R probes include automatic
preview; they do not establish browser or concurrent-deployment performance.
The separate warning audit classified all 122 stress warnings as intentional
ExtendedTask cancellation, with no other warning type.

Only acceptance documentation, the project-plan manifest checksum and the
workspace benchmark evidence change after review. The frozen source remains
untouched. The packet's `owner-handoff-integrity.txt` records the comparison.
The owner-facing session starts with an unapplied 06:00-18:00 clock filter on
the four-person IZTECH fixture, retaining 17,280 of 34,560 rows in preview.
Owner design acceptance remains required before later transformation slices
or full-app integration.

## R5 independent acceptance

The reviewer accepted build `m8-s1-r5-91bbda0c35f1` on 2026-11-11 for the
owner's final feedback. Source-index SHA-256:
`91bbda0c35f131a3256ca9d2baa0934dd16d83cfe5e3cbdc346b222298191aab`.
All 200 frozen files matched before and after review. The frozen reviewed app
remains at `http://127.0.0.1:7393/`, sourced from
`/private/tmp/llw-m8-s1-r5-ahrKw1/source`.

Report:
`/Users/zauner/.codex/visualizations/2026/09/28/01a0e6e3-2a52-7a11-a322-1c2dc553b423/m8-s1-r5-review/review-acceptance.md`.
Report SHA-256:
`8089a84cbbbabc76b7cb34288b57b761c8eb91d2b38a3f2f2d67501b1c891ecb`.

The independent pass found no new functional or alignment blocker across 1440,
1200, 1024, 768, 390 and 320 px widths. It exercised the real-data selection,
recipe, Preview, acknowledgement, Apply and Undo journey, date recovery,
separate endpoint zones, duration controls, cancellation/retry and retained-source
warnings. Separate independent R checks confirmed the exact subset and grouped
daytime/overnight parity. R01-R03, R05 and R06 remain closed/protected. R04 labels
and units and the existing narrow sample-table wrapping remain nonblocking
integration notes. Owner design acceptance is still required before Slice 2.

The frozen-source test rerun passes 263 assertions with the same three expected
cancellation warnings. Refreshed synthetic resource probes pass the 50 ms target
and 100 ms hard gate: ordinary-control p95 47 ms for hosted 250k and 48.05 ms
for local 1m, with no full-payload decodes during controls. Full previews took
0.895 s and 2.431 s. These do not establish browser/concurrency or full-real-data
performance. Evidence is in `/private/tmp/llw-m8-s1-r5-ahrKw1`.

Only acceptance documentation and its manifest checksum change after review.
The frozen candidate remains untouched; `owner-handoff-integrity.txt` records
that runtime, tests and fixture bytes still match the reviewed candidate.

## Authorization and bounded files

The owner approved the five-slice plan recorded in project-plan.md and requires
simple, learnable R using existing tidyverse packages and public LightLogR APIs.
Slice 1, its independent-review corrections and the three filters explicitly requested in the completed r6 feedback are authorized. Other later slices,
full-app wiring, dependency changes, commit, push and deployment remain gated.
The owner explicitly invoked develop-shiny-apps in this task and directly
authorized ongoing messages with the coordinator and dedicated reviewer.
The owner requires independent review before every handoff, including rapid
layout iterations. Resolve acceptance findings in a new candidate, re-review
the changed surface, and obtain acceptance before returning it to the owner.
No second skill or implementation subagent is implied by that authorization.

After reading the existing sources, the Slice 1 implementation boundary is:

- New `R/preparation.R`: small explicit selection pipeline, step/public-call
  mapping, recipe validation and background transaction worker.
- New `R/preparation-summary.R`: source-qualified selection context, bounded
  comparisons and resource accounting, kept separate from the pipeline.
- New `R/m_prepare.R`: isolated Prepare server and retained `prepare_app()`.
- New `R/preparation-ui.R`, `R/preparation-filters.R` and `R/preparation-preview.R`: scoped UI, typed filter helpers and source-day comparison.
- Scoped Prepare presentation rules in `inst/app/scss/lightlogweb.scss`, added
  for the owner's layout refinement while preserving the shared app styles.
- New `tests/testthat/helper-milestone-8.R`, `test-preparation.R` and
  `test-prepare-module.R`: synthetic fixtures, independent R pipeline parity,
  identity/lifecycle and module regression checks.
- New `dev/milestone-8-prepare-app.R`,
  `dev/run-milestone-8-acceptance.R` and
  `dev/milestone-8-preparation-benchmark.json`: offline showcase and measured
  operation evidence.
- This note, `project-plan.md` and `manifest.json` for current plan/evidence and
  exact runtime allowlist regeneration.

No existing scientific, metadata, app, session or shared task implementation
file is in this boundary. If a shared repair becomes necessary, document the
specific blocker before expanding it. Preserve unrelated `dev/brand/candidates/`
and `man/man.Rproj` exactly.

## Startup audit

Baseline: `dev`, HEAD `7fd3d2d8b244a6a90c4cb85581b4c9403544480e`, tracked
worktree clean. R 4.6.1, LightLogR 0.10.3, glcdp 1.1.0 at
`58a16400dc6d30ec2686ebf1898f938d3721eb2c`, shiny 1.14.0, bslib 0.11.0,
Bootstrap 5, dplyr 1.2.1, lubridate 1.9.5, mirai 2.7.2, promises 1.5.0,
testthat 3.3.2 and shinytest2 0.5.1. Lock and manifest contain 173 packages;
the initial manifest has 435 files. No installation or repin was performed.

The project activates with `RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript`; this
uses its configured renv library without the restricted sandbox-lock hang.
The current renv usage scan reports additional unrecorded/unused development
dependencies. The exact status is retained rather than silently snapshotting
them. The first API probe used a nonexistent bslib export; the corrected probe
used `bslib::theme_version()` and verified Bootstrap 5.

Audit code, versions, inputs and logs: `/private/tmp/llw-m8-audit-KNEvBB`.
Commands: `RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript baseline.R` and
`RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript glc-baseline.R` from that evidence
directory, with the project root set in each script. The same baseline was
rerun with local worker sockets enabled after the restricted run skipped its
real-worker test. Session, resource and common long-task tests pass with real
workers and one deliberately exercised recovery warning. The isolated GLC
bounded-preview immediate retry test reproduces its disclosed timeout. The
separate invoked-inspection startup-deadline retry passes in this run, which
does not resolve its earlier intermittent failure. No GLC repair is included.

The existing versioned `new_recipe()` / `new_recipe_step()`, immutable payload,
draft/preview/apply/reset/undo functions and `new_long_task()` are reusable.
Full record validation decodes payloads and must stay out of routine control
work. Draft editing therefore updates only the small recipe. Inspection and
complete preview preparation run in the session worker. Apply may reuse only
the exact full worker result, bound to dataset ID, revision, immutable input
and recipe fingerprint. The parent owns record publication.

## Slice 1 scientific and interaction contract

Supported selections are participant/source identity selection, inclusive
calendar dates, half-open absolute datetime ranges, half-open recurring clock
windows, and length from either end with an explicit scope. Most operations use
mapping version 1; new datetime endpoint controls use mapping version 2 while
retaining legacy version-1 explicit-offset recipes. Every step stores its
dataset ID, canonical checksum, operation
version and explicit arguments. Participant choices retain exact `Id` plus
available appended-source/file-group/file identities; labels and positions are
never selection keys. Filtering preserves row order, duplicates, every retained
column, values and absolute timestamps. Each evaluation starts from raw.
An identity is the exact combination of fields retained in the record. It is
not called an acquisition stream when that finer identity is unavailable.

Date and datetime selection call public `LightLogR::filter_Date()` and
`LightLogR::filter_Datetime()` with explicit parameters. Installed 0.10.3
`filter_Time()` uses inclusive endpoints, adds a Time column and applies an AND
condition that returns no rows for 22:00 to 06:00. A short dplyr condition using
lubridate clock components is therefore needed for consistent half-open
overnight windows. This is a plain-R predicate, not an expression interpreter.
The public reference is https://tscnlab.github.io/LightLogR/reference/filter_Datetime.html;
the installed API/source inspection is authoritative for this locked runtime.
Clock filters record their timezone and start-date display convention. They
keep observations inside the interval and discard the others. Grouping defines
metric analysis units separately in Milestone 9. Equal clock bounds are rejected as
ambiguous. Datetime controls require an explicit UTC offset, avoiding guesses
about repeated or nonexistent local clock labels. Metadata annotations never
convert values or times.

Preview compares immutable raw with the complete candidate. Displays are
explicitly bounded; they are never used as prepared data. Lossy selection of
any number of rows requires an additional acknowledgement before Apply, so
there is no arbitrary universal high-removal threshold. A zero-row preview is
inspectable but cannot be applied; edit, disable or remove its steps. Invalid
forms retain the prior applied data. Reset restores exact raw and an empty
recipe; Undo restores the previous applied snapshot. Both follow the existing
20-snapshot history contract. Repeating Apply without a new preview is blocked.

Interval, gap and duplicate summaries require a retained file-group or file-name
identifier for each observation in the compared dataset. They describe each
exact retained identity's observed span. Where file identity is absent or
partially missing, these temporal diagnostics are unavailable with an explicit
reason. Rows, retained identity counts, timestamps and measurement missingness
remain available. No source identities are inferred or reconstructed.
An intended-window coverage percentage is not invented: Slice 4 owns that
definition and its rules. Slice 1 explicitly displays coverage as not assessed
without an intended window. Filtering does not become an implicit missing-value,
gap-fill, calibration, DST-correction or aggregation operation.

## Operation and resource budgets

Ordinary recipe/form/navigation work: p95 target 50 ms, hard gate 100 ms,
with no complete payload decode, rescan or network access. Inspect, Preview,
Reset and Undo use the session-local preparation task. Duplicate requests are
blocked, cancellation remains draining until the worker releases, and retry
waits for that release. Dataset/revision/draft changes reject stale completions.

The Slice 1 resource policy reuses the profile's 256 MiB hosted / 1 GiB
local cache allowance as its per-record preparation budget. Preflight estimates
64 MiB plus sixteen serialized raw payload equivalents; the worker also checks
64 MiB plus the retained record and eight decoded-data equivalents, using the
larger estimate. The initial eight-payload estimate proved too low compared with
the first observed R heap peaks and was revised before review; that initial
measurement is preserved in the audit directory as `benchmark-initial.json`.
These are conservative working-memory estimates, not process RSS guarantees.
An estimate above half the profile budget requires explicit acknowledgement
before Preview. This is a resource policy, not a scientific removal threshold.
Over-budget work is rejected before evaluation,
with an actionable reduce-input/use-local message. Retain one full preview per
module, show at most 100 rows/8 columns and 1,000 plot points per stage, and
bound recipes to 30 steps. Existing profile worker/time/storage limits remain
in force. Measured synthetic 250,000-row and local 1,000,000-row evidence will
record actual runtime and allocations; R timings do not establish browser or
concurrency latency. No hardware-independent runtime prediction is asserted.

## Candidate r1 verification

Final focused run: 131 passing assertions, zero failures/errors/skips, two
expected cancellation warnings from the existing ExtendedTask controller.
Evidence: `focused-handoff.log` and `focused-handoff.rds` in the audit directory.
Command: `RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e
'testthat::test_local(filter="^(preparation|prepare-module)$")'`.

`R CMD build` succeeded. The first `R CMD check --no-manual` ran the complete
packaged suite: 5,657 passes, one failure, four test warnings and three skips.
Its only failure is the already reproduced GLC bounded-preview immediate-retry
timeout at `test-glc-preview.R:4501`. The three skips are the development-only
ActLumus inputs and optional development snapshot excluded from the package.
The new Slice 1 tests passed. This is not a clean full-suite result.
The check also caught an unqualified `tail()` and a misleading local reactive
name `library`; both were corrected. The final package check with `--no-tests`
is `Status: OK`, and the changed Slice 1 surface was retested separately.
Logs: `package-check-with-tests.log`, `full-tests.Rout.fail`, `build-handoff.log`
and `package-check.log`. The temporary `build-check.R` records the exact commands
and package inputs. No baseline test was skipped, relaxed or repaired for Slice 1.

The frozen r1 source-hash-bound benchmark is in
`/private/tmp/llw-m8-s1-r1-RR7M3G/source/dev/milestone-8-preparation-benchmark.json`,
report version 1.1.0. In sequential
local R execution, the 250,000-row hosted-budget case took 0.781 s for the full
preview and had 30 ms worst ordinary-control p95. The 1,000,000-row local-budget
case took 1.671 s and had 30 ms worst control p95. Both have zero full-payload
decodes during ordinary edits/toggles/actual reorders and exact independent-R
pipeline equality. Maximum observed R vector heaps were 115.5 and 236.5 MiB;
total transient allocations were about 464 MB and 1.75 GB, not simultaneous RSS.
Both 50 ms design targets and 100 ms hard gates pass on this machine.

Browser checks exercised first keyboard Save/Preview/Apply, loss acknowledgement,
Undo, New-step focus, rapid date edit/save, empty-result recovery, all five
offline schema variants, cancel/immediate retry, dataset switching while running,
synthetic worker failure/retry, truly empty source and the 250,000-row fixture.
Native `textInput(updateOn="blur")` prevents delayed text edits arriving after
Save. Save/New/Apply remain mounted to preserve focus. The heading retains
prepared/source counts after publication. Source/preview plot order is explicit,
the plot has an accessible description, and no-removal previews do not ask for
a removal acknowledgement. Hosted preflight refusal retains actionable resource
feedback without a stale loading message, including on repeated Preview. A
smaller source recovers normally. No custom JavaScript was added.

Widths 1440/1200/1024/768/390/320 had no page-wide horizontal overflow. At 320 px
the table is dense and wraps heavily; this is a severe-failure check, not a
claim of polished mobile presentation. Native control focus is visible and
console error/warning output was empty. Screenshots are in the audit directory.
Some in-app full-page captures contain stitching artifacts, so viewport captures
and read-only DOM geometry were used to distinguish capture from page layout.
Final cosmetic adjustments receive an additional frozen-build smoke check.

The runtime manifest adds only the three preparation R files. Strict dependency
resolution confirms the same 173 package descriptions. Repository lookup was
unavailable during generation, so exact approved package source/provenance
records are retained rather than accepting null repository replacements.
The final manifest contains 438 files; every runtime checksum is verified.

The closed r1 review snapshot and launchers are preserved in
`/private/tmp/llw-m8-s1-r1-RR7M3G`. Its main offline route used port 7391, local
profile with one worker. A second route on 7392 uses the same frozen code,
hosted profile and synthetic 500,000/1,000,000-row sources to expose resource
acknowledgement and preflight refusal. These launchers only build offline R
fixtures and temporary session state. The review packet records exact hashes.

## Independent r1 findings and r2 disposition

The independent r1 pass closed with revision required. The report is
`/private/tmp/llw-m8-s1-r1-independent-KGEmPv/review-findings.md`, SHA-256
`a15960d50b73d005d413019a83ae6758cafc7058bfa019d7fba645dc64784922`.
All 474 frozen files matched before and after review. No author source changed
during that pass. The original snapshot remains unchanged.

- M8-S1-UX-R01: lifecycle, failure, refusal and Apply validation messages now
  appear beside the Preview/Apply controls. Running work no longer advises
  starting a preview. Button labels reflect busy/unavailable/acknowledgement
  states while keeping controls mounted and focusable for explanations.
  An empty source asks for a dataset containing observations; a filtered empty
  result asks for changes to the recipe. No custom JavaScript was added.
- M8-S1-UX-R02: cancellation shows draining while the existing controller is
  busy and a completed message after release. An idle Cancel says no work is
  running. Applied data remain unchanged. Immediate retry retains the existing
  queue-until-release behavior.
- M8-S1-UX-R03: choices, saved steps and comparisons now describe retained
  participant/source identities. Missing file identity is visible beside the
  selector, and file-specific temporal diagnostics are withheld. Exact file
  choices and diagnostics remain available for standalone/synthetic inputs.
  Shared append code and its fixtures remain unchanged.
- M8-S1-UX-R04: effective variable labels, units, raw-name references and an
  unspecified-unit fallback remain an open integration requirement for Slice 5.
  This is a nonblocking reviewer improvement, not an owner-approved deferral or
  a claim that metadata integration already passed.
- M8-S1-UX-R05: the operation is now labelled "Filter by clock time". Its help
  states that it keeps observations inside the interval and does not define
  metric groups. The overnight start-date label is explicitly a preview label.

The owner's earlier mockups are selective inspiration, not a required redesign,
new scientific defaults or a Group implementation gate. Prepare changes the
observations, and Group defines analysis units. Narrow sample-table polish
remains a nonblocking observation and does not reopen M7 R59.

Read-only R diagnosis of R03 is preserved in
`/private/tmp/llw-m8-audit-KNEvBB/stream-audit-concise.R` and `.log`, including
input paths, the command in the review report and complete package versions.
Two 32-row IZTECH S004 files were combined under one retained appended identity.
The r1 diagnostic reported 14,172 implicit gap epochs; exact original file
identities restored only in that audit yielded zero. The product correction
withholds the unsupported diagnostic and preserves all selected source rows.

R2 evidence is retained in `/private/tmp/llw-m8-s1-r2-1LVo8c`. The synthetic scale
fixture now includes explicit file-group identifiers so large-data benchmarks
continue to exercise temporal diagnostics. This only changes synthetic
development data. Frozen public-derived and appended fixtures are unchanged.

Final focused checks: 174 passes, zero failures/errors/skips, three expected
cancellation warnings. Logs/results: `focused-release.log` and
`focused-release.rds`. Command: `RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e
'testthat::test_local(filter="^(preparation|prepare-module)$")'`.
The new R03 test compares the appended selection with an independent direct R
row subset, including preserved row names, and verifies that temporal outputs
are unavailable. Missing, blank and partially retained file identities are
covered. Real-worker tests verify cancellation does not announce availability
before capacity is released, immediate retry, and stale-result rejection.

Focused browser checks confirm running/failure/acknowledgement messages beside
the controls, mounted-button keyboard focus, cancellation after an applied
144-row result, retry, empty-source guidance, appended S004's explicit limitation
and separate standalone S004 file choices. At 320 pixels, the acknowledgement
message and Apply remain reachable and the page has no horizontal overflow;
the existing dense sample-table limitation remains. Hosted 500,000-row work
fits the revised synthetic-data estimate of 216.6 MiB after acknowledgement;
1,000,000 rows is refused at 369.2 MiB against the 256 MiB profile. This is a
local policy simulation. Screenshots are retained alongside the R2 evidence.
Final readiness-label and resource-guidance changes receive a frozen-build
smoke check before handoff. No broader acceptance is inferred from these checks.

The r2 source-bound benchmark is preserved in the r2 frozen source at
`dev/milestone-8-preparation-benchmark.json`, version 1.1.0. Final sequential R
timings were 0.859 s for 250,000 rows and 2.517 s for 1,000,000 rows. Worst
ordinary-control p95 was 40 ms and 38.05 ms respectively, with zero full-payload
decodes during edits/toggles/reorders and exact independent pipeline equality.
Both 50 ms targets and 100 ms hard gates pass. Maximum observed vector heaps
were 113.1 and 266.8 MiB; total transient allocations were 527,310,896 and
1,994,754,520 bytes, not simultaneous RSS. The final command/output is retained
in `benchmark-release.log` under the R2 evidence directory.

Package build/check, strict manifest regeneration, exact hashes and the final
frozen-route smoke checks are recorded in the separate R2 `review-packet.md`
and `integrity.txt`. The existing complete-suite GLC timeout remains disclosed;
the focused verification does not imply a green full package test suite.

## Independent review and regression surface

### Owner control revision after the r2 handoff

R3 reuses the dashboard's bounded native date-picker pattern and the import
module's searchable `OlsonNames()` choices. Calendar-date and recurring-clock
forms take their zone from the immutable timestamps' native `tzone`, not the
dashboard analysis/display preference. Date-and-time selection has separate
date, time, and native/custom zone controls for each endpoint. The custom zone
adjusts that endpoint's available date bounds. R's explicit local-time parser
rejects skipped DST times and requires an offset for repeated times rather than
guessing. New datetime steps use public-call mapping version 2 to retain both
endpoint zone choices; existing version-1 explicit-offset recipes still execute.
Selection leaves source instants unchanged. The controls and impact preview are
now adjacent at desktop widths. These are provisional owner-directed changes,
not a final visual-design acceptance or a new independent review pass.

Duration selection offers elapsed seconds/minutes/hours/24-hour days and whole
local calendar days. Start/end anchoring and overall/source-dataset/participant/
retained-identity scopes are explicit options, so the unanswered terminology
question does not impose a hidden grouping choice. Overall is the initial
scope. Source-dataset scope is offered only with complete retained source IDs.
Each anchor uses observations reaching that step, so prior selections matter.
Gaps count toward length; this does not implement minimum observed-duration or
coverage rules. The scientific implementation calls public
`LightLogR::filter_Datetime()` on a grouped indexed view, then subsets the
unchanged input rows. From start excludes the upper boundary; from end includes
the lower boundary, following that public API. A calendar-day boundary falling
in a skipped/repeated local hour is rejected with recovery guidance.
Reference: https://tscnlab.github.io/LightLogR/reference/filter_Datetime.html.
The separate timezone-transform operation is reserved for Slice 3: `with_tz`
preserves instants and changes displayed clock values; `force_tz` preserves
clock values and changes instants. Both require explicit recorded choices and
DST handling. No transform is implemented by the new selection controls.

R3 work and focused evidence are in `/private/tmp/llw-m8-s1-r3-PYqKM6`.
The final focused run passes 230 assertions, with no failures/skips and the same
three expected cancellation warnings. Command: `RENV_CONFIG_SANDBOX_ENABLED=FALSE
Rscript -e 'testthat::test_local(filter="^(preparation|prepare-module)$")'`.
Results/log: `focused-release-final.rds` and `focused-release-final.log`; R and
package versions: `session-info.txt`. Inputs are the synthetic fixtures in
`tests/testthat/helper-milestone-8.R`, additional explicit synthetic records in
`test-preparation.R`, and the unchanged offline M7 fixtures. Tests distinguish
staggered source/participant/file anchors, elapsed and calendar durations across
DST, preceding-step effects, endpoint zones, missing source identity, strict
recipe versions and transactional Apply/Undo.

Initial checks exposed empty-source date-bound warnings, corrected before the
191-pass repeat. The first duration test run exposed a reference row-name
difference after the existing public date filter and a missing initial browser
echo in the new test setup; those reference/setup issues were corrected. Browser
verification then caught numeric-input type differences and a delayed numeric
Save. Canonical numeric form values and native `updateOn = "blur"` resolve them.
Invalid step feedback now appears beside Save as well as Preview/Apply, keeping
date/time errors visible in the stacked layout. Editing or discarding controls
clears that feedback. No custom JavaScript was added.

The r3 source-bound benchmark and package/check records are included in its
frozen review packet. R2 evidence remains historical. The known complete-suite
GLC retry timeout is not repaired or concealed by these focused checks.

### R4: recover cleared date controls

The independent r3 pass is closed with one acceptance correction, R06.
Report: `/private/tmp/llw-m8-s1-r3-independent-4rmy8d09/review-findings.md`,
SHA-256 `69dffaa43420e149a904dd73c5f641debda32bf951f675dd00c4f2b9fd24bbc2`.
It passed the revised endpoint and duration controls, scope restrictions,
Apply/Undo, adjacent cancellation/stale-result behavior and targeted responsive
states. R01-R03 and R05 remain closed. R04 remains nonblocking for integration.

R4 gives missing or unavailable dates field-specific guidance for First date,
Last date, Start date and End date, including the eligible range and interpreted
time zone. Calendar inputs use the bundled datepicker's native
`data-date-default-view-date` option so a cleared picker opens at the recording
month. Its empty initial value is explicit; the app never substitutes a date.
Only the date control is rendered when its context, form-loading state or endpoint
zone changes. Ordinary date/time typing does not redraw it. No bespoke
JavaScript, new dependency or scientific selection change was added.

The final focused R run passes 257 assertions, no failures/skips, with three
expected cancellation warnings. Evidence, commands and versions are in
`/private/tmp/llw-m8-s1-r4-sJo8NO/focused-release.log`, `focused.R`, `focused.rds`
and `session-info.txt`. New regression checks cover all four cleared fields,
custom-zone bounds, preserving the saved draft/applied record after refusal,
and successful correction with independent endpoint choices.

Intermediate checks caught a date-conversion warning and a duplicated HTML
initial-date attribute during the recovery refinement. Both were corrected;
fresh browser startup reached a ready form with the native dates present.
Browser checks then reproduced the two R06 paths, opened each cleared picker
in March 2026, selected an eligible date, and completed Save/Preview/Edit with
the native Start and New York End preserved and no false dirty state. The final
source-bound performance run and frozen identity are recorded in the r4 packet.
R3's successful package build and check remain earlier supporting evidence;
they are not presented as a new r4 package check. The known full-suite GLC retry
failure remains disclosed.

The independent r4 pass is closed with acceptance for the owner's final check.
Report: `/private/tmp/llw-m8-s1-r4-independent-gxx8mshn/review-acceptance.md`,
SHA-256 `4144eff698adb5193349045f40bbb7de1203a9625e3fc894eb6abfbda5402b80`.
R06 is closed. Fresh reviewer evidence covers both missing-date paths, mouse
Save, valid-date retention, corrected Preview/Edit and narrow recovery. The
earlier R01-R03 and R05 closures remain intact; R04 remains nonblocking.
The accepted build is `m8-s1-r4-20456c80a1c9`, source-index SHA-256
`20456c80a1c999fd387e708bcbed6c37fc7e35f4d2b9e47032a8572c64ebf17b`.
Its 474 indexed source files remain frozen at
`/private/tmp/llw-m8-s1-r4-sJo8NO/source`.

R4's ordinary-edit p95 is 51 ms for both sizes, above the 50 ms design target
and below the 100 ms hard gate. Full previews took 1.082 s at 250k rows and
2.862 s at local 1m. Both hard gates pass; the target miss remains explicit.
Only acceptance documentation and the plan's manifest checksum were updated
after the review closed. `owner-handoff-integrity.txt` in the r4 evidence
directory verifies unchanged runtime, tests, fixtures and benchmark bytes.

At the r4 handoff, the owner showcase at `http://127.0.0.1:7391/` had a saved native Berlin
Start of 28 March 2026 10:00 and New York End of 30 March 2026 02:00. Its
269-of-431-row preview was unapplied. Acknowledge and Apply, then Undo to restore
431 source rows. The independent acceptance does not settle the provisional
layout or open Slice 2. Owner feedback remains the next gate.

### Preserved r2 acceptance

The independent r2 review is closed with acceptance for the owner's final check.
Report: `/private/tmp/llw-m8-s1-r2-independent-y8lf6ufb/review-acceptance.md`,
SHA-256 `b9ab896c9a772d88e724685b81ea89b8beb0f38ae93a60b753b495613fd22cc1`.
R01-R03 and R05 are closed by visible evidence. R04 remains an open,
nonblocking Slice 5 integration requirement; narrow-table polish remains a
nonblocking observation. The reviewer verified all 474 frozen source hashes
before and after the pass and preserved the r1 snapshot.

The accepted build is `m8-s1-r2-ca277661989a`, with source-index SHA-256
`ca277661989a30a9aa1ba43d07470a6d7d95805279cd7917dfde8df918ae2714`.
Its source remains frozen at `/private/tmp/llw-m8-s1-r2-1LVo8c/source`.
The main showcase remains at `http://127.0.0.1:7391/`; the local hosted-policy
simulation remains at `http://127.0.0.1:7392/`. This acceptance status was added
after the review closed. At the r2 owner handoff, only this note,
`project-plan.md` and its regenerated manifest checksum differed from the
accepted snapshot; runtime, tests, fixtures and benchmark were unchanged.
Subsequent r3 edits are described above. The r2 post-review integrity record is
`/private/tmp/llw-m8-s1-r2-1LVo8c/owner-handoff-integrity.txt`.

At the r2 owner handoff, the programmer's main session had a saved 22:00-06:00
Europe/Berlin clock filter and an unapplied 143-of-431-row preview. The current
r4 handoff example is described above. The appended S004 choice additionally demonstrates
the explicit missing-file-identity warning and unavailable temporal diagnostics.
Independent acceptance does not replace the owner gate before Slice 2.

The dedicated reviewer read the one-time M7 handoff and prepared
`/private/tmp/llw-m8-s1-ux-startup-d9vCl9/slice-1-review-checklist.md` (SHA-256
`b8806b75d425b067bf13cab95dfedac8e4b38c55b831cb8c7f479131ae683baa`). Its
handoff source is `/private/tmp/llw-m7-m8-handoff-tWS9rD/milestone-7-ux-handoff.md`.
Carry forward first-action, late-update, focus, repeated redraw and stale-success
lessons. R58 metadata-only table-state retention, R60 factual fallback and R61
removed-target feedback remain protected in the unchanged main app. R59 stays
deferred; removed export R52 is not passed or reinstated.

Use unchanged TUM MEDI, IZTECH LIGHT+MEDI and UCR all-variable fixtures plus
appended and metadata-absent records. Their 32 rows per file are derived offline
fixtures, not fresh published measurements. Add clearly synthetic multi-day,
DST, overnight, gaps, duplicates, irregular sampling, NA, boundaries and scale
cases in R. No live package opening or measurement transfer.

Review one frozen candidate at a time with build/hash/URL/profile, source copy,
file boundary, visible fixture routes, addressed IDs, limitations, automated
evidence, budgets and action authority. No source mutations during review.
Production widths are 1440/1200/1024, 768 requires usable core, 390/320 are severe
failure checks. Keyboard and state recovery need fresh evidence. Independent
acceptance opens only the owner final check. Full-app integration stays Slice 5.
