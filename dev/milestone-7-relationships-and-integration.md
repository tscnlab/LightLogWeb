# Milestone 7 Slice 2: relationships and metadata integration

Status: unified-explorer candidate 1 requires revision for R43, the independent
reviewer's branch hover/focus contrast finding. The owner approved the combined
R43-R45 correction, including one merged Metadata explorer card and immediate
parent navigation. Candidate 2 is implemented, verified, and frozen for a new
independent review. The
preceding compact-path candidate 3 was accepted for
owner final check by the existing external reviewer, including independent
visual closure of R42. That acceptance applies to the preceding contract,
not this new behavior. R39-R42 remain protected regression targets. This document
does not authorize owner final acceptance, commit, deployment, dependency
changes, overlay authoring/export, or a later slice.

## Scope

Slice 2 adds one source-agnostic integration layer on top of the accepted
Slices 1 and 1B explorer:

1. capture exported glcdp relationship traversal while the exact source is
   already open;
2. retain a typed, versioned, serializable relationship context without
   indexing it as original source metadata;
3. scope source records and relationship rows to the accepted import unit;
4. enrich only a disposable derived key table through exported
   `add_metadata()`;
5. resolve variable labels, units, calibration, and provenance without
   changing raw variable identity;
6. add exact metadata crosslinks from the library and dashboard;
7. keep source timezone and calibration separate from analysis and display
   settings; and
8. correct metadata navigation order and apply the same bounded participant
   and device action policy.

Milestone 6 remains responsible for metadata and data transport, task
recovery, and session record recovery. Slice 2 adds no parallel recovery
mechanism and does not reopen a package during ordinary app use.

Explicitly excluded are overlay forms and export, edited GLC packages,
preprocessing recipes, reproducibility bundles, subset export, reports,
LightLogR handoff, live measurement transfer, silent cross-source
reconciliation, and the rejected visual-hierarchy prototype.

## Baseline and public API boundary

- Slice 2 baseline: `e02abf15dfb7349964467b9cbd31f757b91d6431`
- Original Milestone 7 audit baseline:
  `b704d24ee6eab066163bb88e7125faf19e09543d`
- glcdp version: 1.0.0.9000
- glcdp exact revision:
  `e49fcf153a114916ce2680f8b40eb475e694e10d`
- No dependency repin or installation is required.

Only the audited exports `extract_metadata()` and `add_metadata()` enter this
slice. The relationship adapter does not use unexported glcdp functions and
does not read measurement contents. The future full fixture generator and the
one-purpose augmentation script use the same production adapter.

The first TUM metadata-only augmentation attempt opened the exact package,
then stopped before producing relationship output because
`participant_internal_id` exists in both `participants` and
`participant_characteristics`. No fixture file or manifest was replaced, and
IZTECH and UCR were not opened. The corrected adapter asks exported
`extract_metadata()` for all unambiguous fields through connected-resource
traversal, then asks for `participant_internal_id` with
`resource = "participants"`. It combines the fragments only after proving
that their keys and first-occurrence order are identical. The owner authorized
one corrective TUM retry. The successful transactional run opened TUM, IZTECH,
and UCR once each, read no measurement contents, and replaced no fixture until
all three relationship extractions and integrity assertions passed. Across the
development history, TUM therefore had one failed no-output metadata open and
one authorized successful metadata-only retry.

## Retained relationship contract

`llw-metadata-relationships` version 1.0.0 contains:

- a unique `file_group_id` table for default relationship traversal;
- a unique `Id` table for explicitly dataset-level traversal;
- requested and actually resolved fields for each anchor;
- retained warnings;
- exported-API, anchor, ambiguity-resolution, no-measurement-read, and exact
  fixture provenance;
- a SHA-256 fingerprint covering the complete contract.

The context is stored inside the exact source snapshot so it survives dataset
records, session operations, and serialization. It is a LightLogWeb-derived
integration artifact, not a copied package metadata resource. The explorer
therefore excludes it from source metadata search and hierarchy indexing.
Original `core_metadata`, dataset inventory, variable inventory, unknown
fields, resources, namespace, revision, and source provenance remain indexed
and displayed unchanged.

Projection scopes the context by exact selected file-group and dataset IDs.
The same selected IDs scope participants, participant characteristics,
datasets, devices, device datasheets, and study records. No relationship is
inferred by LightLogWeb. Appended records retain one context under each
immediate source namespace. For dashboard navigation, LightLogWeb may build a
disposable row-level identity projection containing only exact dataset,
participant, device, study, and file-group identifiers. Every projected row
remains qualified by `llw_source_dataset_id`, and each returned entity carries
its one exact source namespace into the metadata crosslink. This projection
does not merge retained source records or copy descriptive values between
namespaces. The original per-source relationship contexts remain unchanged.

## Derived enrichment boundary

The dashboard reduces its already prepared in-memory data to a unique
`Id`/`file_group_id` key table. It passes only this disposable table and the
retained relationship table to exported `glcdp::add_metadata()` with
`overwrite = FALSE`. It then asserts:

- unchanged row count and first-occurrence order;
- unchanged `Id` and `file_group_id` values and classes;
- unchanged grouping variables and `.drop` behavior;
- no mutation of the canonical raw or prepared payload.

Missing or ambiguous lineage, unavailable glcdp, and exported column conflicts
remain visible recoverable states. The relationship table is shown separately
from source and prepared measurements and is explicitly described as
disposable.

## Display resolution and settings separation

Every measurement keeps its raw column name. Display properties resolve per
field in this order:

1. an exact overlay dataset, file-group, and source-name match;
2. the exact retained source namespace and source-name match;
3. imported factual source details retained by earlier records;
4. raw variable name plus `unit not specified`.

Per-field resolution matters. For example, an overlay label and unit can take
display precedence while immutable source calibration remains visible with
its own provenance. Multiple conflicting values are reported as varying and
the raw name remains the safe label. Appended target columns resolve through
their exact append mapping; rows from a different source with the same source
variable name are not candidates.

Resolved labels, units, and provenance appear in the selected library summary,
dashboard focus control, plot axis, exploration context, variable inventory,
source/pre-processed display-table headers, and the dashboard metadata-context
card below the workbench.
Raw names remain visible in brackets wherever a label differs.

The dashboard metadata-context card links to exact source or study, dataset,
focus-variable, selected-participant, and related-device fields. Selecting a
variable-inventory row adds a direct variable crosslink. Crosslinks carry a
versioned request with the dataset, namespace, field, retained path fragment,
record identity, and stable request ID. The explorer resolves it against the
existing in-memory index. Missing or ambiguous targets leave the current valid
selection unchanged and explain the fallback.

Each dynamic participant and device button is keyed by a deterministic SHA-256
action identity derived from the validated dataset, namespace, field, retained
path, and record identity. It is never keyed by its ordinal display position.
The dashboard shows individual actions for up to four participants and up to
four related devices. If either entity count exceeds four, that entity group
shows its complete count and a reduction instruction, with no arbitrary subset
of buttons. When a participant selection changes, the observer reads and
baselines only the action identities belonging to the new relationship scope.
A button that is still visible while the browser applies the refreshed UI
therefore either opens the exact identity shown at click time or has no effect.
It cannot be rebound to the next participant or device in the refreshed list.

The source view now reports `source_manifest$source_timezone`; the prepared
view continues to report the analysis/display timezone. Source calibration is
factual metadata. Focus variable, display timezone, scale, view range,
missingness denominator, and grouping remain settings or derived view state.
No label, unit, timezone, or calibration display claims to mutate or
revalidate a source package.

## Slice 2A navigation correction

The matching-field summary and selectable rows are inside the Find metadata
card. The next region is an always-visible Path to this focus card. It
starts at meaningful source areas, including Core metadata, Dataset inventory,
Variable inventory, and retained unknown top-level areas. A source resource
wrapper and its same-path child are represented once in this view, while the
exact retained source path remains available in the disclosure below.

The path is one row with the last four levels, reduced to one readable current
level and a compact, accessibly named Browse icon on narrow phones. A left
ellipsis appears when earlier levels are hidden and
opens a native Shiny modal containing the full path and ancestor jumps. A
Browse action opens the same dialog with metadata-area and contained-metadata
choices. The redundant Fields at this level list is removed. The page does not
show Nearby structural branches or a separate exact-item-instance selector. Exact
node IDs and record identities remain available in memory and continue to
resolve external dashboard routes. Choosing a structural node changes only the
path focus. Choosing a field changes the selected field below. Search text,
filters, result order, and the matching list remain stable.

The Section control is derived from the grouped field paths, not from the
transport resource list. Its logical choices are Core Metadata, Dataset
Inventory, Variable Inventory, and retained unknown top-level areas when they
exist in the selected source scope. Filtering is applied to the complete
in-memory grouped index before the 40-row display cap. The control is omitted
when fewer than two specific sections are available, so an inert selector is
never shown.

Repeated same-level nodes with the same label, kind, source namespace, layer,
resource, and parent location appear as one logical branch. Its retained value
and record counts cover every exact peer node. For example, the six IZTECH
file-group instances of Melanopic Edi appear once under Variable Inventory;
opening that branch exposes all twelve retained fields across all six exact
instances, including Unit. A disclosure lists every exact source node and path.
This is presentation grouping within one retained structural location, not
cross-source reconciliation. Record refreshes clear both server focus and any
visually checked stale result before a new field is chosen.

A separate Explore this metadata item across datasets card follows the path
panel. It groups repeated instances of the selected semantic item and exposes
their sibling properties, without reconciling source namespaces. The selected
field detail remains downstream of both exploration controls. Its contents are
always derived from the current hierarchy focus. A structural focus with no
direct fields shows a truthful empty state instead of repeating properties from
the previously selected entity. The selected field changes only after the user
chooses a field at the current focus. Dataset state, identity, source-versus-
overlay context, and selected-item status follow field detail at the bottom of
the Metadata tab. Dashboard metadata context is below the dashboard workbench.

Explicit matched-field selection, hierarchy jumps, and dashboard crosslinks
scroll to the path card and transfer keyboard focus there. A local
`metadata-navigation.js` helper performs only this DOM scroll/focus operation
and restores the trigger focus when a navigation modal is dismissed. It waits
for the requested group to render and layout to settle, cancels superseded
requests, and never changes metadata, filters, route identities, or selection.
Shiny-native radio/action controls and modal dialogs retain all selection and
navigation state. This narrowly scoped helper uses the owner's authorization
for custom JavaScript when no Shiny-native alternative exists. There is no new
visualization or other package dependency.

Every enabled dashboard metadata crosslink activation reapplies its complete
destination, including query, logical section, selected field, hierarchy focus,
and focus-driven comparison. This also applies when the participant or device
request is identical to one activated earlier in the session. The app bridge
creates a fresh reactive delivery edge without changing the request schema or
its stable request ID. Dashboard action identities, stale-DOM rejection, and
dataset-refresh clearing remain unchanged.

## Fixtures and offline policy

The accepted three sources and measurement selections remain unchanged:

| Fixture | Exact source | Participants/files | Variable request |
|---|---|---:|---|
| TUM | `tscnlab/melidos-tum-glc-dataset@1187d19b7f6145b73b8d4dfee13af711c1cb6180` | 5/5 | `MEDI` |
| IZTECH | `tscnlab/melidos-iztech-glc-dataset@abc456bdb418e6250b9c973813a190dc062c6e76` | 5/6 | `LIGHT`, `MEDI` |
| UCR | `tscnlab/melidos-ucr-glc-dataset@1d1309c969b9ca754a5c9696382c39d4b58ccdfd` | 5/5 | all compatible variables |

The successful metadata-only augmentation is frozen at
`2026-09-02T09:11:00Z` in fixture schema 2.1.0:

| Fixture file | Bytes | SHA-256 |
|---|---:|---|
| `tum-medi-only.rds` | 43,044 | `664660fc59204eae7298adc8f83ac934bb7024d1f3d50f6a2bdad57011f56e04` |
| `iztech-light-subset.rds` | 38,784 | `6f85478a1c83774a3d26a48142b0fb3599bd354aaec635928daef1f7271bd77c` |
| `ucr-all-light-variables.rds` | 81,496 | `754c7ad5e028807c11821aec4b98cd6ad682a74d2223871e3fc0b310a3ab9581` |

The augmentation reuses the existing bounded measurement snapshots
byte-for-byte. It opens each exact package only for exported metadata
relationship traversal, records zero public measurement reads, writes all
outputs to a temporary staging directory, and replaces fixture files only
after all three pass. The manifest records the exact generation timestamp,
glcdp revision, extraction fields, anchors, warnings, row counts, relationship
fingerprints, package-open count, hashes, and reuse policy. Offline tests and
review runs never reopen a source package.

IZTECH S004 is a required ambiguity case. Its relationship context must map
`MELIDOS_IZTECH_S004:19` to `IZTECH_AL03` and
`MELIDOS_IZTECH_S004:20` to `IZTECH_AL04`. These stay two rows and two device
crosslinks under one selected dataset and participant.

## Performance gates

Budgets were fixed before implementation:

- ordinary label/unit resolution, relationship selection, exact crosslink
  resolution, and dashboard metadata context: p95 at or below 50 ms design
  target and 100 ms hard gate;
- all existing explorer operations: the same 50/100 ms targets;
- each retained bundle: at most 32 MiB;
- all three fixture bundles: at most 64 MiB combined;
- widest scaled inventory: at least 50,000 entries and at most 64 MiB;
- one 50,000-row disposable exported `add_metadata()` operation: p95 at or
  below 100 ms and at most 64 MiB temporary allocation;
- ordinary interactions: zero package reopens and zero record payload
  deserializations after startup.

The acceptance script covers all three fixtures, the widest UCR inventory,
the TUM plus IZTECH appended state, metadata-absent fallback, and the scaled
50,000-entry and 50,000-row cases. The frozen report is
`dev/milestone-7-metadata-benchmark.json`, generated at
`2026-09-14T08:18:44Z` under R 4.6.1 (report version 2.5.0). The 100 ms p95 hard timing gate, memory
gate, navigation-correction gate, repeated-crosslink gate,
relationship-integrity gate, append-lineage gate, no-reopen gate, and
no-deserialization gate pass. Every realistic fixture and appended-state
interaction meets the 50 ms p95 design target. Repeated delivery of the exact
IZTECH S004 participant and AL03 device requests retains each request's stable
ID. The overall design-target flag is false only because the deliberately
artificial 50,000-entry full-path modal takes 61 ms p95. Its compact path renders
at 25 ms p95, and ordinary navigation, search, filtering, sorting, selection,
path, and focus-comparison operations remain at or below 22 ms p95.

| Case | Highest ordinary-operation p95 | Compact path p95 | Full-path modal p95 | Index bytes | Bundle bytes |
|---|---:|---:|---:|---:|---:|
| TUM, 1,428 entries | 8.65 ms | 12 ms | 19 ms | 1,192,480 | 1,366,008 |
| IZTECH, 1,144 entries | 6.90 ms | 12 ms | 16 ms | 996,512 | 1,167,024 |
| UCR, 4,874 entries | 7.25 ms | 13 ms | 19 ms | 3,419,272 | 3,753,136 |
| TUM + IZTECH append, 2,690 entries | 7.25 ms | 11 ms | 15 ms | 2,454,056 | 5,089,384 |
| Artificial scaled UCR, 50,000 entries | 22 ms | 25 ms | 61 ms | 35,702,368 | not applicable |

Single-source and appended relationship selection are 1 ms p95. The appended
mapped-focus dashboard view is 31 ms p95. Entity-reference resolution is
1 ms p95; rendering four participant actions, the five-participant reduction
state, and four device actions takes 6.25, 1, and 6.25 ms p95 respectively. The
50,000-row disposable `add_metadata()` case is 19.15 ms p95 and allocates
15,670,096 bytes. Combined retained fixture bundles occupy 6,286,168 bytes.
The artificial 50,000-entry hierarchy context, compact path, and full-path
modal allocate 7,541,312, 7,197,104, and 7,590,880 bytes respectively, all below
their 10 MiB limits.
Ordinary review-route package reopens and record payload deserializations are
both zero.

## Earlier R39-R41 verification

- Focused app-smoke, dashboard-data, metadata-explorer, and module-contract
  suites pass 66, 540, 326, and 174 expectations respectively. The module suite
  retains one intentional malformed-import recovery warning.
- The complete local suite passes 3,770 expectations with zero failures, two
  intentional recovery warnings, and six expected skips because mirai daemons
  are unavailable. A fresh disposable source archive passes `R CMD check` with
  `Status: OK`; its installed-package tests pass 3,689 expectations with zero
  failures, two intentional warnings, and nine expected environment or
  optional-fixture skips.
- The corrected offline acceptance benchmark passes every hard timing, memory,
  relationship, append-lineage, no-reopen, and no-deserialization gate. Its
  one design-target exception is the artificial 50,000-entry expanded panel
  described above.
- Production browser checks cover all three source fixtures, the appended
  state, metadata-absent fallback, exact participant and device routes,
  same-level grouped branches, VLSQ8 sibling values, melanopic units, and the
  four-entity reduction policy. The corrected record-refresh state shows no
  stale checked field.
- The complex hierarchy has no page-level horizontal overflow at 320, 390,
  768, 1,024, 1,200, or 1,440 px. Long exact source paths wrap inside their
  card, while wide value tables retain their intentional local scroll region.
  Matching-field, accordion, root, child, and field choices are keyboard
  operable. The browser console reports no application warning or error.
- The R39 browser reproductions pass for IZTECH S004 and UCR Melanopic Edi.
  Moving from participant metadata to Dataset Inventory first shows the
  truthful no-fields state, then the selected IZTECH dataset exposes
  `device_ids` without participant properties. Moving from a UCR participant
  characteristic to Variable Inventory and Melanopic Edi exposes the exact
  name, label, and `lx` unit without participant-characteristic properties.
  In both paths, the prior selected field remains unchanged until a current
  focus field is explicitly selected.
- The R40 browser and unit checks expose Core Metadata, Dataset Inventory, and
  Variable Inventory for each of the three fixtures. On UCR, Dataset Inventory
  returns all 13 grouped fields before the display cap, Variable Inventory
  returns 189 matches with 40 displayed, and All sections returns 723 matches
  with 40 displayed. Keyboard selection works, and the Section control is
  absent for the metadata-absent fallback.
- The R41 browser reproductions pass for repeated IZTECH participant S004 and
  device AL03 links. After navigating either route to Dataset Inventory and
  MELIDOS IZTECH S004, activating the same dashboard link again restores its
  exact Core Metadata field, hierarchy focus, and participant or device
  comparison. Stable request and action identities are retained, and browser
  warning/error logs remain empty.
- Independent review remains pending for this corrected build.
- The exact stale-route regression passes: UCR participant S002 resolves to
  device AL06, its device link selects `device_internal_id`, and switching
  UCR to TUM to UCR clears the query, selected field, and hierarchy focus.
  Returning to the dashboard does not replay the old metadata event.
- The hierarchy first-click regression passes. Routing IZTECH S004 dataset
  metadata selects `dataset_internal_id`. Moving through Dataset File to exact
  item `MELIDOS IZTECH S004:20` changes only hierarchy focus and keeps that
  selected field, with no implicit S001 or unrelated device-field selection.
- The device-refresh race regression passes twice in a real browser. After
  selecting S003 and S004, removing S003 leaves the old AL05 button visible
  briefly. Clicking it 285 and 286 ms after removal produces no navigation and
  never opens AL03. Once S004-only relationships settle, the AL04 action still
  opens query and hierarchy focus `IZTECH_AL04`. Unit tests also prove that
  equal device identities have equal action IDs, different devices have
  different IDs, removed IDs disappear from refreshed HTML, stale events are
  ignored, and a current event retains its identity.
- IZTECH S004 still exposes exactly `IZTECH_AL03` and `IZTECH_AL04`. The
  metadata-absent record remains usable with raw `MEDI` and the literal
  `unit not specified`. Partial metadata, immutable source provenance, and the
  separate LightLogWeb overlay remain explicit. A metadata result can be
  selected with keyboard focus and Space. No application error was observed.
  The independent reviewer observed five nonblocking bootstrap-datepicker
  vendor deprecation warnings already present on the preceding build; they
  are not application warnings and are unchanged by this slice.

## Review boundary

The frozen Slice 2 packet will include this document, the exact Git status and
diff, manifest and fixture hashes, benchmark JSON, focused/full/package checks,
and desktop/tablet/mobile keyboard and crosslink evidence. The independent UX
reviewer was moved to a subagent at the owner's request. Because those
subagents lacked browser access, the owner subsequently authorized the
existing external reviewer for visual inspection, with the same
responsibilities and stable finding IDs. Independent reviewer
acceptance authorizes only the owner final check. It does not authorize commit
or Slice 3.

## Compact-path candidate 3 verification, 2026-09-14

The current owner-directed correction changes only the metadata module, the
dashboard context placement, the app's redundant metadata heading, the
metadata SCSS, and one local DOM-only scroll/focus helper, plus their tests,
acceptance benchmark, and planning notes. It does not change the three offline
fixture snapshots, dependencies, exported glcdp API boundary, source metadata,
overlay values, or measurement payloads. Earlier uncommitted Slice 2 changes
and unrelated owner files are preserved.

- Full local suite: 3,832 passed expectations, zero failures or errors, two
  intentional recovery warnings, and six expected mirai-environment skips.
- Fresh source archive: `R CMD check --no-manual` reports `Status: OK`.
  Installed-package tests pass 3,751 expectations with zero failures, two
  intentional warnings, and nine expected environment/optional-fixture skips.
- The benchmark evidence and its one 61 ms synthetic-modal design exception
  are recorded above. These are R-side operation timings, not instrumented
  browser end-to-end latency measurements. `git diff --check` is clean.
- Programmer browser checks cover Find metadata first, bottom dataset/state/
  provenance context, dashboard context below the workbench, a fixed-height
  breadcrumb row, complete native-modal ancestor jumps, and removal of the
  redundant Fields at this level UI. A deep UCR path exposes four trailing
  levels on desktop, and all five levels in the modal without a duplicate
  resource wrapper. Narrow phones retain one readable current level and use
  the ellipsis for earlier levels.
- Explicit field selection and dashboard crosslinks land at Path to this
  focus and transfer keyboard focus there. Tab, Enter, Space, Escape, modal
  trigger-focus return, and ancestor/contained-item jumps were exercised.
  Filtering/sorting does not issue the explicit navigation scroll request.
- The independent reviewer directed mediated browser scenarios for repeated
  IZTECH S004 participant and AL03 device routes, AL04 identity, UCR VLSQ8
  values, melanopic EDI units, source switching, metadata-absent fallback,
  appended provenance, and logical-section filtering. The programmer executed
  them through the actual production module and supplied visible AX/action
  evidence. R39-R41 behavior remains consistent with that evidence.
- Responsive checks used 320, 390, 768, 1,024, 1,200, and 1,440 px viewports.
  Candidate 1 exposed blank phone breadcrumb labels at 320 px, recorded as
  M7-UX-R42. Candidate 3 programmer screenshots show a readable current
  Melanopic Edi label at 320 and 390 px, working ellipsis/Browse controls,
  a readable full-path modal, and the settled four-block desktop row. The
  viewport override was reset after testing. Current browser warning/error
  logs were empty.
- R42 is fixed in programmer evidence but is not independently closed.
  Both independent subagents lack the in-app browser surface, and inherited
  context does not expose the current native tool screenshots to them. The
  supported in-app content export is unavailable. This is a review-evidence
  blocker, not an additional application finding. No independent visual pass,
  owner-final acceptance, or commit authorization is inferred.

The frozen review packet is
`/private/tmp/llw-m7-compact-final-B2InSB`. It contains the source archive and
package-check output, full-test results, final SHA-256 manifest, Git status and
diff, R session information, and the current review summary. The benchmark
and three-source fixture manifest are also covered by the SHA-256 manifest.
The showcase wrapper is
`/private/tmp/llw-m7-slice2a-r41-showcase.R`, with the visible build label
`MILESTONE 7 SLICE 2A COMPACT PATH CANDIDATE 3`. It serves the production module
using only the existing offline fixtures at `http://127.0.0.1:7381`.

## Unified explorer owner correction, 2026-09-14

The owner requested this correction after using compact-path candidate 3.
Its independent acceptance remains in that build's immutable packet addendum
`independent-visual-acceptance.md`; it is not transferred to this candidate.

The new production-bound contract is:

- Path to this focus uses the former Where this field lives block styling,
  with friendly names, raw identities, and current-level emphasis. The
  duplicate location sequence is no longer rendered in selected-field detail.
- The path stays on one row, with at most four trailing levels. Narrow phones
  keep a readable current block. A left ellipsis appears only when earlier
  levels are hidden and opens a Shiny-native full-path popup. The popup has
  every meaningful ancestor, jump controls, and Close, but no Browse,
  metadata-area menu, contained-item list, or exact-instance selector.
- Explore this metadata item across datasets contains both child branches
  and direct fields in one visible, scrollable list. Chevron actions enter a
  branch and extend the path; field radios update the selected field while
  preserving the focus, available choices, search, filters, and ordering.
- Metadata areas is a visible return action in the explorer. It displays
  Core Metadata, Dataset Inventory, Variable Inventory, and retained unknown
  areas in that same list. Structural nodes explain when their children
  must be opened to reach fields. The previously selected field remains
  unchanged until another field is chosen.
- Values in this field, including every retained record identity and its
  exact-path disclosure, precedes About this field. Grouped location and
  immutable-source/overlay provenance remain available below the values.
- One bounded observer uses stable node-specific Shiny action IDs. Only
  currently visible branches can be entered; remounted counters are
  rebaselined, removed events are ignored, and exact namespace/layer checks
  remain in force. There is no per-node observer accumulation.
- The existing DOM-only helper still handles scroll/focus after explicit
  navigation. This correction only removes its obsolete Browse trigger.
  Native Shiny controls continue to handle selection, branching, and modals.

Exact file boundary for this correction: `R/m_metadata_explorer.R`,
`inst/app/scss/lightlogweb.scss`,
`inst/app/www/js/metadata-navigation.js`,
`tests/testthat/test-metadata-explorer.R`,
`dev/run-milestone-7-metadata-acceptance.R`,
`dev/milestone-7-metadata-benchmark.json`, `project-plan.md`, and this document.
The showcase wrapper and verification evidence live only under
`/private/tmp/llw-m7-unified-explorer-tzG2Rk`. No fixture, dependency, measurement
payload, source metadata, overlay value, or dashboard contract changes are
part of this correction. Existing unrelated and broader Slice 2 changes are
preserved.

Budgets remain p95 at or below 50 ms as the design target and 100 ms as the
hard gate. The visible-contents render is now explicitly benchmarked for the
ordinary focus and widest retained child inventory in each of the three
fixtures, the appended case, and the 50,000-entry scaled index. Both renders
have a 10 MiB transient-allocation gate. Existing retained-bundle, index,
relationship, no-reopen, and no-redeserialization gates remain unchanged.

### Unified explorer candidate 1 verification

- Focused explorer regression tests pass. The full local suite passes 3,868
  expectations with zero failures or errors, two intentional recovery
  warnings, and six expected mirai-environment skips.
- The checked source archive reports `R CMD check --no-manual: Status: OK`.
  Installed-package tests pass 3,787 expectations with zero failures, two
  intentional warnings, and nine environment/optional-fixture skips. No
  dependency was installed or repinned. Air and `git diff --check` pass.
- The final benchmark, run at `2026-09-14T20:12:49Z` after the full and package
  checks completed, passes every hard timing, memory, relationship,
  namespace, and zero-I/O gate. Ordinary navigation/search/filter/sort/
  selection p95 is at most 9 ms across the three fixtures, appended case,
  and 50,000-entry scaled index. Visible focus-contents rendering is at most
  19 ms p95, compact-path rendering 24 ms, and full-path modal rendering
  27 ms. The widest IZTECH list of 33 source channels takes 60 ms p95:
  this remains an explicit 50 ms design-target exception, not a changed
  100 ms hard gate. Individual sampled operations can exceed 100 ms.
  These are R-side timings, not browser end-to-end measurements.
- The three retained fixture bundles total 6,286,168 bytes. The scaled index
  uses 35,702,368 bytes, below its 64 MiB gate. Every measured navigation,
  modal, and visible-contents allocation is below its 10 MiB gate. Ordinary
  operations reopen no packages and deserialize no complete records.
- Programmer browser checks cover mixed child/field contents, stable leaf
  selection, first-click and repeated branch entry, IZTECH S004's two file
  groups, UCR VLSQ8 values, melanopic EDI units, and source-change reset.
  Values precede About this field, and the duplicate location sequence is
  absent. Keyboard activation, full-path ancestor jumps, Escape, and modal
  focus return work in the checked journeys.
- Screenshot checks cover 320, 390, 768, 1,024, 1,200, and 1,440 px widths.
  The left ellipsis opens the complete path without a Browse menu. A
  browser-found inherited minimum-height issue was corrected before the
  final server launch and rechecked. Current browser warning/error logs
  are empty. Programmer checks are not independent visual acceptance.

The exact frozen review packet is
`/private/tmp/llw-m7-unified-explorer-tzG2Rk`. It contains the hash manifest,
build identity, checked archive, complete test/check outputs, benchmark
summary, Git diff/status, R session information, and programmer browser
notes. The visible label is `MILESTONE 7 UNIFIED EXPLORER CANDIDATE 1` at
`http://127.0.0.1:7381/`. Only the existing external reviewer's acceptance of
this exact candidate can advance it to the owner final check. No owner final
acceptance or commit is inferred.

## Owner-approved R43-R45 correction, 2026-09-14

Candidate 1 received REVISION REQUIRED for R43: branch labels and chevrons
lost contrast on hover/focus. The independent reviewer protected the existing
R39-R42 passes. The owner then approved the combined R43-R45 contract.

- R43: branch-specific Bootstrap text tokens keep names and chevrons dark
  in resting, hover, focus, and active states. The keyboard-focus outline
  remains distinct from the field-radio selection styling.
- R44: one Metadata explorer card contains the compact path, parent action,
  and mixed branch/field list. There are no duplicate inner headings,
  nested cards, or persistent instructions. The header help button opens
  a native `bslib::popover()` with click/keyboard activation, Escape, Close,
  and focus return. This supplies the requested popup-tooltip interaction
  without new JavaScript. The installed bslib 0.11.0 implementation was
  checked against the [official popover contract](https://rstudio.github.io/bslib/reference/popover.html).
- R45: One level up follows the full meaningful path, not only its visible
  four-level tail. It moves exactly one parent, returns from a root area to
  the overview, and is disabled at the overview or before a valid focus.
  Exact source namespace/layer identities, field details, query/filter/sort,
  and the existing scroll/focus behavior remain intact. The static Shiny
  action button avoids a remounted counter during repeated navigation.

The approved eight-file boundary is unchanged. Seven files change relative
to candidate 1: the explorer module, SCSS, explorer tests, benchmark script,
benchmark JSON, project plan, and this note. The existing DOM-only navigation
helper requires no edit. Fixtures, dependencies, source payloads, overlays,
dashboard behavior, and unrelated work remain unchanged.

Candidate 2 evidence lives under `/private/tmp/llw-m7-r43-r45-AmWjw9`. Its
visible label is `MILESTONE 7 UNIFIED EXPLORER CANDIDATE 2`. The full local
suite passes 3,968 expectations with zero failures/errors, two intentional
recovery warnings, and six expected mirai-environment skips. The checked
archive reports `R CMD check --no-manual: Status: OK`; installed-package tests
pass 3,887 expectations with zero failures, two intentional warnings, and
nine environment/optional-fixture skips. Remote repository index checks were
unavailable in the restricted runtime; dependency installation and repinning
were neither attempted nor needed. Air and `git diff --check` pass.

Programmer browser checks cover all six existing responsive widths, native
help activation and dismissal, full-path popup readability, a deep path's
hidden ancestors, overview disabling, first-click re-entry, stable field
selection, VLSQ8 values, and the IZTECH S004:19/AL03 and S004:20/AL04 split.
The dashboard focus-variable crosslink lands and focuses the merged card.
These checks do not substitute for the existing external reviewer's visual
acceptance. Every new freeze must be sent to that reviewer before the owner
final check. No commit, later slice, or final acceptance is authorized here.

### R43-R45 candidate 2 performance

Benchmark report 2.7.0, started at `2026-09-14T21:03:05Z` after the full and
package checks completed, passes all 100 ms p95 hard gates and memory gates.
Across the three fixtures, appended case, and 50,000-entry inventory, ordinary
search/filter/sort/navigation/selection p95 is at most 9.65 ms. Immediate
parent navigation is at most 21 ms, compact-path rendering 26 ms, full-path
popup rendering 28 ms, and focus-contents rendering 16.65 ms. The widest
33-child IZTECH list is reported at 50 ms p95. Its pre-serialization design
gate remains false, so it is conservatively retained as a 50 ms target
exception rather than treating the rounded JSON value as a pass. The largest
individual explorer sample is 135 ms; the hard gate is p95, not every sample.
These are R-side timings, not browser end-to-end measurements.

Retained bundle totals remain 6,286,168 bytes and the scaled index remains
35,702,368 bytes. Every measured navigation, path, popup, and contents
allocation is below 10 MiB. Ordinary operations reopen no source package and
deserialize no complete record. The current showcase is the only task Shiny
instance left running. The immutable temporary packet records the exact
candidate hashes, checks, and independent-review status separately.

## Owner-requested R46-R47 corrections, 2026-09-21

The owner approved candidate 2 with one bounded exception: lower-list field
selection must not jump the viewport if a small correction is possible. The
owner additionally requested the retained-item count beside One level up.
These refinements do not authorize a commit or a later slice.

- R46 removes the explicit scroll request from in-place field selection.
  The field/branch list rerenders only when the retained index or hierarchy
  focus changes. A selection-only native Shiny radio update preserves the
  mounted list, its scroll position, and keyboard focus. Selected-field
  values still update immediately. Search-result, Up, breadcrumb, branch,
  and dashboard-crosslink navigation retain their deliberate explorer
  landing. The supported
  [Shiny event binding](https://shiny.posit.co/r/reference/shiny/latest/bindevent.html)
  and [radio updater](https://shiny.posit.co/r/reference/shiny/latest/updateradiobuttons.html)
  supply this behavior without new bespoke JavaScript.
- R47 moves the existing retained-item badge into the One level up toolbar.
  It is right-aligned on the same row when space permits; at the narrowest
  phone width it wraps without overlap or horizontal overflow. Counting and
  exact source identity semantics are unchanged.

The correction boundary comprises the explorer module, scoped SCSS, explorer
tests, benchmark evidence, project plan, and this note. The benchmark script,
DOM navigation helper, fixtures, source/license manifest, dependencies,
overlays, dashboard behavior, and unrelated changes remain unchanged.
Evidence and the candidate 3 wrapper are contained in
`/private/tmp/llw-m7-r46-r47-2dsd12`. The showcase label is
`MILESTONE 7 STABLE-SELECTION CANDIDATE 3` on the existing local port 7381.
The old task showcase was stopped before the replacement was started.

Focused tests pass 566 expectations with zero failures, errors, warnings, or
skips. The new three-fixture regression verifies unchanged list markup and
hierarchy context during selection, native selected-value updates without
replacement choices, updated field detail, preserved query/sort/source
fingerprints, and normal rebuilding after hierarchy navigation.
Programmer browser checks reproduce the original page/list reset, then
verify actual mouse selection and consecutive keyboard selections with
unchanged page scroll (1265.5 px) and list scroll (1371.5 px). These are
specific observed positions, not universal scroll guarantees. Programmatic
test-locator auto-scrolling is excluded from those measurements. Responsive
toolbar checks cover all six agreed widths. Independent acceptance remains
required for this changed candidate.

The full local suite passes 4,011 expectations with zero failures/errors,
two intentional recovery warnings, and six unavailable-mirai skips. The
checked source archive reports `R CMD check --no-manual: Status: OK`, with
3,930 installed-package expectations passing, zero failures, two intentional
warnings, and nine environment/optional-fixture skips. Repository-index
lookups remain unavailable, without installing or repinning any dependency.
Air and `git diff --check` pass.

A new R-side field-selection round-trip check includes field-detail rendering,
native input updates, a stable-list assertion, and the returned status. For
100 alternating selections per fixture, isolated p95 is 58.15 ms (TUM),
71.15 ms (IZTECH), and 72.10 ms (UCR). The accepted candidate 2 measures
65.20, 69.00, and 85.05 ms respectively under the comparable check. All pass
the unchanged 100 ms hard gate; this broader round-trip metric has explicit
50 ms design-target exceptions on both builds. Allocation is at most
7,200,216 bytes, below 10 MiB. A first run concurrent with the full/package
checks reached 123.20 ms p95 for UCR and is retained as adverse evidence,
not discarded or represented as a pass. These timings are not browser
end-to-end latency or a concurrent-user capacity claim. The independent
review packet also holds the current full metadata benchmark, leaving the
older repository benchmark report explicitly historical and unchanged.

The fresh full benchmark passes all existing hard timing, memory,
relationship, lineage, and zero-I/O gates across the three fixtures,
appended-source case, and 50,000-entry inventory. Ordinary operations reach
at most 11 ms p95, hierarchy navigation 25 ms, compact-path rendering 30 ms,
full-path rendering 35.30 ms, and focus-contents rendering 20 ms. The widest
33-child IZTECH list reaches 60 ms p95, retaining its explicit 50 ms design
exception. Individual samples exceed 100 ms; the hard gate is p95, not every
sample. Bundle totals and scaled-index memory remain 6,286,168 and
35,702,368 bytes respectively. No source package is reopened and no complete
record is deserialized during ordinary interactions. The temporary benchmark
script is verified identical to the repository script except for its output
location. Its initial incomplete temporary copy failed parsing before any
benchmark ran and was corrected before the final evidence run.
