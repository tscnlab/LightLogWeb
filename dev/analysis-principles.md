# LightLogWeb analysis-principles specification

Status: awaiting owner approval

Reviewed: 2026-07-16

Initial compatibility target: current CRAN LightLogR 0.10.3

## 1. Purpose

This document translates the scientific and practical analysis guidance in the
current LightLogR articles and course material into enforceable LightLogWeb
behavior. It is the analysis contract for feature work. Existing LightLogWeb
code is not evidence that a behavior is correct.

Each principle records:

- the established LightLogR workflow or function;
- the safe LightLogWeb default;
- required data or metadata;
- whether a problem warns or blocks;
- concise user-facing wording; and
- an automated acceptance scenario.

The app may provide additional advanced choices, but it must not weaken these
guarantees without an explicit owner decision and corresponding tests.

## 2. Reviewed source inventory

### LightLogR articles

1. [The whole game](https://tscnlab.github.io/LightLogR/articles/Day.html)
2. [Import and cleaning](https://tscnlab.github.io/LightLogR/articles/Import.html)
3. [Log transformation](https://tscnlab.github.io/LightLogR/articles/log.html)
4. [Metrics](https://tscnlab.github.io/LightLogR/articles/Metrics.html)
5. [Photoperiod](https://tscnlab.github.io/LightLogR/articles/photoperiod.html)
6. [Light spectrum](https://tscnlab.github.io/LightLogR/articles/spectrum.html)
7. [Durations, states, and clusters](https://tscnlab.github.io/LightLogR/articles/states.html)
8. [Visualizations](https://tscnlab.github.io/LightLogR/articles/Visualizations.html)

### LightLogR webinar and course

1. [Beginner course](https://tscnlab.github.io/LightLogR_webinar/beginner.html)
2. [Advanced case: A Day in Daylight](https://tscnlab.github.io/LightLogR_webinar/advanced_01_a_day_in_daylight.html)
3. [Advanced case: High light sensitivity](https://tscnlab.github.io/LightLogR_webinar/advanced_02_case_light_sensitivity.html)
4. [Advanced case: Therapy lamps](https://tscnlab.github.io/LightLogR_webinar/advanced_03_light_therapy.html)
5. [Advanced case: Visual experience beyond light](https://tscnlab.github.io/LightLogR_webinar/advanced_04_visual_experience.html)

The package reference for the resolved LightLogR release remains authoritative
for exact arguments and return types. The articles and course establish the
analysis intent and safe workflow.

## 3. Governing scientific model

LightLogWeb uses this conceptual sequence:

    unchanged source bytes
        → canonical imported data
        → quality diagnosis
        → draft preparation recipe
        → preview
        → committed prepared data
        → explicit visualization and metric grouping
        → plots, summaries, and metric instances
        → reproducible project, script, report, and result exports

Canonical data are never overwritten. Display transformations do not alter
metric inputs. Metadata can improve labels and eligibility but optional
metadata are not a prerequisite for basic analysis.

## 4. Principles and app rules

### AP-01 — Use tidy longitudinal data with explicit identity and time

**Established workflow**

LightLogR analyses expect observations in rows, variables in columns, a POSIXct
Datetime column, and participant or recording identity in Id. Imported data are
typically grouped by Id.

**Safe app default**

Normalize a successful device import to a canonical table containing Id and
POSIXct Datetime without dropping other source columns. Retain import grouping
as provenance, but make every later grouping specification visible.

**Required inputs**

- A parseable datetime for every retained observation.
- A non-missing stable Id or an explicitly previewed filename-to-Id mapping.

**Warn or block**

- Block creation of a canonical dataset when Id or POSIXct Datetime cannot be
  produced.
- Warn about missing Id values, duplicated Id/Datetime pairs, unsorted rows,
  or unusual types before Apply.

**User-facing wording**

“LightLogR needs one participant/recording identifier and one valid timestamp
per observation. Review the highlighted mapping before adding this dataset.”

**Acceptance scenario**

A valid grouped LightLogR table imports; a table with no resolvable Datetime or
Id remains on the import screen with field-specific guidance and no session
error.

### AP-02 — Preserve source bytes, canonical data, and provenance

**Established workflow**

Reproducible LightLogR work begins with known source files and explicit import
arguments.

**Safe app default**

Hash and retain unchanged uploaded bytes in session storage. Store original
names separately from safe server paths. Treat the first valid LightLogR result
as immutable canonical raw data. Record device, format version, timezone, Id
mapping, DST option, duplicate policy, package version, and import time.

**Required inputs**

Successful preflight and import.

**Warn or block**

- Block if staged bytes cannot be verified or canonical data fail validation.
- Warn if original files are omitted from a downloaded project bundle.

**User-facing wording**

“Your original files are unchanged. LightLogWeb will always rebuild preparation
steps from the imported base.”

**Acceptance scenario**

After several preparation changes and reset, object-level equality and the
source checksum match the original canonical record.

### AP-03 — Batch-import only compatible device exports

**Established workflow**

The beginner course recommends batching files only when they share device,
export structure, and collection timezone. Device parsing belongs to
LightLogR import_Dataset and its supported device/version registry.

**Safe app default**

One import action has one selected device, format version, and source timezone.
Preview file-to-Id mapping and basic structural compatibility before import.
Import different devices or source timezones as separate datasets and combine
them only through an explicit merge workflow.

**Required inputs**

Files, device, version, timezone, and Id mapping.

**Warn or block**

- Block unsupported device/version combinations and incompatible extensions.
- Warn when headers or structures differ and let LightLogR remain the parser.
- Block a mixed-timezone batch unless each source timezone can be represented
  explicitly.

**User-facing wording**

“Import together only files from the same device/export format and source time
zone. Other files can be imported separately and combined with a reviewed
mapping.”

**Acceptance scenario**

Mixed device selections cannot reach import. Structurally inconsistent files
return a recoverable import diagnosis without losing staged files.

### AP-04 — Distinguish an instant from its local clock representation

**Established workflow**

The A Day in Daylight case demonstrates the key difference:

- lubridate with_tz changes how the same instant is displayed.
- lubridate force_tz reinterprets the recorded clock label and therefore
  changes the underlying instant.

**Safe app default**

Use with_tz semantics for analysis/display timezone changes. Preserve a
canonical UTC instant plus source timezone/local context when combining
sources. Offer force_tz semantics only in an advanced correction explicitly
labelled “timestamps were recorded with the wrong timezone label,” preview the
shift, and require confirmation.

**Required inputs**

Valid IANA timezone. Advanced reinterpretation additionally requires an
explicit correction reason.

**Warn or block**

- Block invalid IANA zones.
- Warn on ambiguous/nonexistent DST clock times.
- Require extra confirmation before any instant-changing reinterpretation.

**User-facing wording**

“Changing the analysis time zone changes the displayed local clock only. Use
timestamp correction only when the original clock labels themselves were
wrong.”

**Acceptance scenario**

Changing Europe/Berlin to UTC leaves numeric POSIXct instants unchanged.
Explicit reinterpretation changes instants by the previewed offset and is
captured as a recipe step.

### AP-05 — Treat DST as a data-quality condition, not a cosmetic offset

**Established workflow**

LightLogR import can account for DST jumps for devices that do not already
handle them. DST transitions can also create repeated or absent local clock
times.

**Safe app default**

Require source timezone, detect transitions over the observed span, and explain
whether the selected device/import option is expected to handle them. Preserve
absolute instants and inspect repeated/missing local times per Id.

**Required inputs**

Source timezone, device/version, and observed timestamps.

**Warn or block**

- Warn when data cross a DST transition or contain a suspicious one-hour jump.
- Block only when timestamps cannot be resolved without an owner choice.

**User-facing wording**

“This recording crosses a daylight-saving transition. Review whether the
device already corrected its clock before applying an additional adjustment.”

**Acceptance scenario**

Fixtures spanning spring-forward and fall-back preserve chronological instants,
surface repeated/absent local times, and never apply a second correction
silently.

### AP-06 — Diagnose epoch, gaps, irregularity, and duplicates separately

**Established workflow**

LightLogR dominant_epoch, has_gaps, has_irregulars, gap_handler, gap_table, and
gg_gaps distinguish expected sampling from implicit missing timestamps,
off-grid observations, and explicit missing values.

**Safe app default**

Report dominant interval per Id, explicit NA, implicit gaps, irregular
intervals, duplicated Id/Datetime pairs, and sort order as separate quality
signals. Keep original observations. Gap expansion is an explicit recipe step.

**Required inputs**

Id, Datetime, and selected variable for variable-specific missingness.

**Warn or block**

- Warn on gaps, irregularity, duplicates, or differing dominant epochs.
- Block a downstream metric only when its own requirements are violated.
- Require a duplicate policy; never silently remove rows.

**User-facing wording**

“Missing values, missing timestamps, off-schedule samples, and duplicate
timestamps are different issues. Review each before choosing a preparation
step.”

**Acceptance scenario**

A fixture containing all four conditions reports four independent diagnostics.
Expanding gaps adds explicit NA rows without changing observed values.

### AP-07 — Never interpolate or manufacture exposure silently

**Established workflow**

gap_handler can make implicit gaps explicit; it does not justify automatic
imputation. Light exposure can change rapidly, so filling values is a
scientific decision.

**Safe app default**

Represent gaps as missing. Do not interpolate in v1. If interpolation is added
later, make it advanced, show its method and affected duration, and retain an
imputation flag for every generated value.

**Required inputs**

Regular target epoch for explicit gap expansion.

**Warn or block**

Warn about memory expansion and affected duration before materializing gaps.

**User-facing wording**

“LightLogWeb can make missing timestamps explicit, but it will not estimate
unobserved light values.”

**Acceptance scenario**

No recipe or metric path replaces missing observations with numeric values
unless a future explicit imputation step and mask are present.

### AP-08 — Evaluate missingness against the intended analysis window

**Established workflow**

remove_partial_data and summary workflows often assess full days, but the
therapy-lamp case shows that a short protocol should not be rejected for
lacking a full 24-hour day. Partial first/last days are common in monitoring
data.

**Safe app default**

Show calendar-day coverage as a diagnostic, not a universal validity rule.
Bind every minimum-coverage decision to a visible target window: calendar day,
clock window, study interval, photoperiod, or metric group. Preview affected
groups before removal.

**Required inputs**

Target group/window, expected epoch when meaningful, selected variable, and
coverage threshold.

**Warn or block**

- Warn on partial boundary groups and low coverage.
- Block only metrics whose explicit window requirement is unmet.

**User-facing wording**

“Coverage is calculated for the selected analysis window. A short protocol is
not expected to contain a complete calendar day.”

**Acceptance scenario**

A complete 2.7-hour therapy session is not labelled invalid solely because it
occupies part of a day; a 24-hour metric remains ineligible with a clear
explanation.

### AP-09 — Recompute preparation from raw data

**Established workflow**

Aggregation and filtering choices change the dataset used by later LightLogR
operations. They must be reproducible and reversible.

**Safe app default**

Store an ordered recipe. Draft preview and Apply both evaluate the complete
recipe from immutable canonical data. Never aggregate an already aggregated
result merely because it is currently displayed.

**Required inputs**

Valid recipe steps and canonical data.

**Warn or block**

Block invalid step ordering or parameters. Warn about lossy, expensive, or
high-removal recipes and require extra confirmation.

**User-facing wording**

“This preview is rebuilt from the imported data. Applying it will create a new
revision; the source remains unchanged.”

**Acceptance scenario**

One-hour aggregation followed by a change to fifteen minutes equals a fresh
fifteen-minute calculation from raw data.

### AP-10 — Make aggregation choices scientific and explicit

**Established workflow**

LightLogR aggregate_Datetime changes epoch and requires an interval and summary
function. Light exposure is often skewed and zero-inflated; mean and median can
answer different questions.

**Safe app default**

Require interval, alignment/origin, numeric summary, and missing-value policy.
Offer documented guided defaults without claiming universal correctness.
Perform photoperiod derivation after aggregation so solar boundaries are not
averaged as measurements.

**Required inputs**

Datetime, target interval, compatible summary function, and selected columns.

**Warn or block**

- Block aggregation finer than unsupported source resolution or invalid
  intervals.
- Warn on mixed source epochs, incomplete bins, and large changes in row count.

**User-facing wording**

“Aggregation changes the scientific resolution. Review interval alignment,
summary function, incomplete bins, and the before/after comparison.”

**Acceptance scenario**

Mean and median recipes produce independently reproducible results and retain
their parameters in projects and generated scripts.

### AP-11 — Keep display scaling separate from analysis values

**Established workflow**

Light values are commonly zero-inflated and span several orders of magnitude.
LightLogR provides symlog and zero-aware log approaches to make such data
visible.

**Safe app default**

Use a documented symlog-friendly scale for light plots when appropriate.
Retain linear and zero-aware log views. Plot scales transform coordinates only;
metric inputs remain on the prepared measurement scale unless the user adds a
separate explicit data transformation recipe.

**Required inputs**

Numeric variable and valid scale parameters.

**Warn or block**

- Warn when ordinary log cannot display zero/negative values.
- Block invalid scale limits or transformation parameters.

**User-facing wording**

“This scale changes how the plot is displayed, not the values used for
summaries or metrics.”

**Acceptance scenario**

Switching a plot between linear and symlog leaves prepared data and metric
results object-identical.

### AP-12 — Treat grouping as part of the analysis definition

**Established workflow**

LightLogR data are commonly grouped by Id, while daily, photoperiod,
sleep/wake, and other metrics derive meaning from additional grouping.
durations helps assess group size.

**Safe app default**

Maintain visible visualization and metric grouping specifications, linked by
default but separable. Never silently ungroup or reuse a stale group. Preview
group counts, rows, observed duration, coverage, regularity, and example labels.

**Required inputs**

Prepared data and all variables/metadata required by the chosen grouping.

**Warn or block**

- Warn for groups under six observed hours as a general indicator.
- Do not block solely on six hours; metric-specific rules can be stricter.
- Warn or block empty and single-observation groups according to the requested
  operation.

**User-facing wording**

“Grouping changes what one result represents. The preview shows the groups that
will be passed to plots or metrics.”

**Acceptance scenario**

Changing plot grouping does not change metric grouping when linkage is off.
Every result records the exact grouping reference.

### AP-13 — Define clock windows correctly across midnight

**Established workflow**

Evening, night, sleep, and therapy windows can cross calendar midnight; simple
same-date comparisons misclassify them.

**Safe app default**

Represent recurring clock windows with start, end, timezone, boundary
inclusion, and an anchor-date rule. Preview representative assignments around
midnight and DST.

**Required inputs**

Valid timezone and clock bounds.

**Warn or block**

Warn when a transition produces an unusual observed duration. Block ambiguous
window definitions.

**User-facing wording**

“This window crosses midnight. Results are labelled by the selected anchor
date, and the preview shows how boundary observations are assigned.”

**Acceptance scenario**

A 22:00–06:00 window includes 23:00 and 05:00, excludes noon, assigns a stable
anchor date, and remains correct across DST fixtures.

### AP-14 — Derive photoperiod only with sufficient context

**Established workflow**

LightLogR photoperiod and add_photoperiod use time, timezone, coordinates, and
a solar-depression definition. The articles use a configurable depression
angle, commonly 6 degrees for civil twilight context.

**Safe app default**

Require valid coordinates and timezone. Default solar depression to 6 degrees
but display it. Derive photoperiod from prepared timestamps after aggregation.
Keep both photoperiod label and consecutive counter as derived grouping data.

**Required inputs**

Datetime, IANA timezone, latitude, longitude, and solar-depression parameter.

**Warn or block**

- Block photoperiod derivation without valid context.
- Do not block unrelated analyses when metadata are absent.
- Warn for polar periods or groups without a transition.

**User-facing wording**

“Photoperiod needs a time zone and coordinates. Other analyses remain
available without them.”

**Acceptance scenario**

Missing coordinates disable photoperiod with a specific explanation but leave
participant/date grouping usable. Aggregation never averages an existing
photoperiod label.

### AP-15 — Validate metric eligibility before calculation

**Established workflow**

LightLogR metrics differ in required duration, regular sampling, gap tolerance,
grouping, variable type, and parameters. summary_metrics represents a common
version-specific preset, not the only valid analysis.

**Safe app default**

Use a curated registry for the supported LightLogR version. Eligibility is
calculated per metric instance and group from explicit requirements. Allow
multiple instances with different thresholds and labels. Require Calculate.

**Required inputs**

Eligible numeric variable, parameter values, grouping, and each metric’s
documented time/regularity conditions.

**Warn or block**

- Disable an ineligible metric instance with its exact unmet conditions.
- Retain successful group/metric results if another fails.
- Warn about borderline coverage without hiding results.

**User-facing wording**

“This metric cannot be calculated for the current selection because: [specific
requirements]. Change preparation/grouping or choose another metric.”

**Acceptance scenario**

Every registry entry matches a direct LightLogR call. Two
duration_above_threshold instances with different thresholds remain distinct
through calculation, export, reload, and generated code.

### AP-16 — Use visualization types appropriate to data scale

**Established workflow**

The course recommends detailed day/timeline views for a handful of participants
and short spans, and overview/heatmap views for longer or larger data. gg_day,
gg_days, gg_overview, gg_heatmap, and gap plots answer different questions.

**Safe app default**

- Prefer gg_days for a small participant selection over roughly one to two
  weeks.
- Prefer gg_day for a few days, normally one to four and at most about a week.
- Prefer gg_overview or an availability heatmap for many participants or long
  spans.
- Require explicit selection, facet paging, aggregation, or labelled sampling
  before rendering beyond tested plot budgets.

These are guided defaults, not hard scientific cutoffs.

**Required inputs**

Selected participants, date window, variable, and plot specification.

**Warn or block**

Warn before “show all” on large data. Block only when a render would exceed a
tested safety budget and offer a corrective control.

**User-facing wording**

“This view is too dense to interpret safely. Narrow the selection, page the
facets, or use an overview. No data have been removed from your dataset.”

**Acceptance scenario**

A 10-participant, one-month, one-minute fixture defaults to an overview and
never sends all rows/traces to the browser without explicit bounded handling.

### AP-17 — Do not infer comparability from a name or unit

**Established workflow**

The A Day in Daylight case highlights that devices measuring nominally similar
quantities can differ in response, calibration, placement, and quality.

**Safe app default**

Preserve device and source per observation after append. Use a union of columns.
Combine measurement columns only after explicit quantity/unit mapping and show
that equal units do not establish device equivalence. Calibration is a visible
recipe step.

**Required inputs**

Source/device provenance and explicit mappings.

**Warn or block**

- Block automatic coalescing based only on column name or unit.
- Warn whenever multiple devices feed one analysis variable.

**User-facing wording**

“These columns use compatible labels/units, but the devices may not be
interchangeable. Confirm the mapping and any calibration.”

**Acceptance scenario**

Appending two devices keeps device/source columns. Similarly named variables
remain separate until explicitly mapped.

### AP-18 — Model states and intervals explicitly

**Established workflow**

The High light sensitivity case imports state changes, converts them to
intervals with sc2interval, attaches them with add_states, and groups analyses
by sleep/wake or other meaningful states. interval2state, extract_states, and
state plots support inspection.

**Safe app default**

Treat state/annotation streams as their own provenance-bearing data. Preview
coverage, overlaps, gaps, label mapping, and attachment before Apply. Calendar
days do not silently replace sleep/wake cycles.

**Required inputs**

Compatible Id mapping, interval/state timestamps, timezone, and label mapping.

**Warn or block**

- Warn on incomplete state coverage and overlapping intervals.
- Block attachment when identity/time alignment is unresolved.

**User-facing wording**

“State labels define analysis periods such as sleep and wake. Review unmatched
time and overlaps before attaching them.”

**Acceptance scenario**

An incomplete sleep log attaches only reviewed intervals, reports unmatched
measurement time, and never silently labels it as wake.

### AP-19 — Make duration and cluster parameters visible

**Established workflow**

LightLogR durations, add_clusters, extract_clusters, and state helpers show that
minimum duration and tolerated interruption change the scientific result.

**Safe app default**

Expose duration threshold, interruption tolerance, boundary handling, and
grouping in every state/cluster specification. Store those values in result and
project provenance.

**Required inputs**

Regular or otherwise eligible time structure, grouping, and explicit
parameters.

**Warn or block**

Warn when irregular sampling makes inferred durations unreliable. Block
parameters that are impossible relative to epoch or window.

**User-facing wording**

“Cluster duration and allowed interruptions change which exposure episodes
count. These settings will be included with the result.”

**Acceptance scenario**

Changing interruption tolerance changes the preview and produces distinct,
fully labelled result instances.

### AP-20 — Keep short protocols and merging assumptions visible

**Established workflow**

The Therapy lamps case demonstrates that short laboratory protocols need
protocol-specific windows and that log merges can lose data when names, IDs, or
times do not match exactly.

**Safe app default**

Let the user define protocol intervals independently of calendar days. All
merges show matched/unmatched counts, key normalization, and duplicate
relationships before Apply. Row-wise append is v1; time-aligned stream joins
are post-v1.

**Required inputs**

Protocol window or explicit merge keys/mappings.

**Warn or block**

Block merge Apply until overlap and duplicate policies are chosen. Warn about
normalization that changes keys.

**User-facing wording**

“The preview shows observations that would not match. LightLogWeb will not
discard them or normalize keys without your approval.”

**Acceptance scenario**

Mismatched participant labels and timestamp precision are reported before any
new dataset is created; source datasets remain unchanged.

### AP-21 — Gate spectral and multimodal analysis on calibration

**Established workflow**

The spectrum article and Visual experience case use spectral reconstruction,
spectral integration, device-specific calibration, and multimodal streams.

**Safe app default**

Preserve spectral channels, wavelengths, calibration identifiers, devices, and
units. Do not treat spectra as a single ordinary numeric variable. Place
spectral reconstruction, integration, and multi-input response models in the
post-v1 advanced milestone.

**Required inputs**

Required spectral channels/wavelengths, compatible units, calibration data,
and the metric’s documented time structure.

**Warn or block**

Block spectral or multi-input calculations when any required channel,
calibration, unit, or alignment is absent. Keep other analyses available.

**User-facing wording**

“This calculation needs calibrated spectral inputs. The missing or incompatible
requirements are listed below.”

**Acceptance scenario**

Incomplete spectral data disable the advanced operation with channel-level
reasons and do not prevent raw inspection or single-variable workflows.

### AP-22 — Let metadata enhance, not gate, core analysis

**Established workflow**

LightLogR can work with a tidy data frame and explicit function arguments.
Coordinates, labels, units, devices, and study context improve interpretation
or enable specific functions such as photoperiod.

**Safe app default**

Allow import, quality inspection, basic preparation, grouping that needs no
metadata, visualization, summaries, and eligible metrics without optional
metadata. Fall back to raw column names and “unit not specified.” Keep factual
metadata separate from analysis settings.

**Required inputs**

Only operation-specific essentials.

**Warn or block**

Block only the operation requiring missing metadata; never the whole dataset.

**User-facing wording**

“Optional metadata can improve labels and enable location-dependent analyses.
You can continue without it.”

**Acceptance scenario**

Complete, partial, and absent optional metadata produce the same numerical
result when no metadata-dependent operation is requested.

### AP-23 — Preview and confirm consequential computation

**Established workflow**

LightLogR operations can be computationally expensive, and preparation choices
change downstream results.

**Safe app default**

Draft controls never mutate committed state. Show numerical and graphical
impact plus runtime/memory estimate. Require Apply or Calculate. Use an extra
confirmation for timezone reinterpretation, lossy/high-removal steps,
large gap expansion, and expensive work.

**Required inputs**

Valid draft specification and stable dataset revision.

**Warn or block**

Block stale task results from committing. Map errors to recoverable states.

**User-facing wording**

“Review the preview, then apply this change. If the dataset changes while the
task runs, the old result will not overwrite your newer work.”

**Acceptance scenario**

A long preview finishing after a newer revision is marked stale and cannot
replace current prepared data.

### AP-24 — Make every result reproducible and self-describing

**Established workflow**

The course is code-based and emphasizes inspectable LightLogR calls and
parameters.

**Safe app default**

Store source manifest, recipe, grouping, result/plot specification, package
versions, warnings, and unit/label context. Generate scripts with public
LightLogR calls and verify them in a clean R process.

**Required inputs**

Validated specification and source provenance.

**Warn or block**

Warn when an export omits original source files or optional cached artifacts.
Block project load on unsafe paths, bad checksums, or unsupported schema.

**User-facing wording**

“This output includes the data revision, preparation, grouping, parameters,
and package versions needed to reproduce it.”

**Acceptance scenario**

Project reload and generated analysis.R reproduce prepared data and core
results independently of Shiny.

## 5. Cross-principle acceptance matrix

| Case | Expected outcome |
|---|---|
| Missing Id | Import remains recoverable; user receives mapping guidance |
| Non-POSIXct or unparseable Datetime | Canonical dataset creation is blocked |
| Duplicate Id/Datetime | Diagnosed separately; explicit policy required |
| Implicit gaps plus explicit NA | Both are reported; no interpolation |
| Irregular samples | Warning and metric-specific eligibility |
| Spring-forward and fall-back | Absolute instants preserved; local anomalies explained |
| Display timezone change | POSIXct numeric values unchanged |
| Explicit wrong-label correction | Shift previewed, confirmed, and recorded |
| Mixed source timezones | Separate provenance plus canonical UTC instants |
| One-hour then fifteen-minute aggregation | Fifteen-minute result rebuilt from raw |
| Symlog versus linear plot | Data and metrics unchanged |
| Missing coordinates | Photoperiod unavailable; other workflows continue |
| Less than six-hour group | General warning, not universal block |
| Short laboratory protocol | Evaluated against protocol, not full-day coverage |
| Two devices with same units | No automatic coalescing |
| State log with gaps | Unmatched time shown; no inferred wake state |
| Repeated metric thresholds | Unique labelled instances persist everywhere |
| Large overview | Bounded, labelled rendering; no silent sampling |
| Missing optional metadata | Core eligible analyses remain available |
| Stale background result | Cannot overwrite current revision |
| Corrupt project archive | Load rejected without affecting current session |

## 6. Decisions requiring owner approval at this gate

Approval of this specification confirms:

1. The listed safe defaults are appropriate starting behavior.
2. The six-hour grouping threshold is a warning indicator, never a universal
   scientific validity cutoff.
3. force_tz-style reinterpretation is advanced, exceptional, previewed, and
   separately confirmed.
4. No silent interpolation or deduplication is allowed.
5. Plot scaling is separate from metric data transformation.
6. Short protocols use protocol-specific coverage.
7. Spectral/multimodal and time-aligned stream work is post-v1.
8. Optional metadata enhance the app but do not gate basic analysis.
