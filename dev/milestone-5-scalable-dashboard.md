# Milestone 5 — Scalable dashboard and source/pre-processed-data table

Initial implementation: 2026-07-22
Owner-review revision: 2026-07-23
UI/UX and scientific-axis revision: 2026-07-24
Two-axis view-control revision: 2026-07-24
Current owner-feedback refinement: 2026-07-30
Owner acceptance: 2026-07-31

## Owner acceptance

The owner accepted Milestone 5 on 2026-07-31 after exercising the real-data
showcase, focus-metric and missingness controls, source/pre-processed views,
scales and y-range behavior, participant/time scope controls, and independent
participant/time pagination. The accepted implementation and its regression
coverage are closed by the project-plan acceptance record and development
branch commit.

## 2026-07-30 owner-feedback refinement (accepted implementation)

This section supersedes conflicting July 24 descriptions of missingness,
recommendation controls, paging placement, plot limits, table controls, and
the placement of source/pre-processed support information.

- **Fresh Explorer workbench:** Explore is rebuilt as one flat workbench inside
  the existing tab boundary rather than a plot card nested inside the tab card.
  Its compact sidebar groups display, data, time, and participant controls; the
  plot header is descriptive only. Targeted help lives in tooltips and
  popovers, without a persistent scope disclosure beneath the plot. The
  sidebar and visualization canvas share one boundary and use the complete
  available width without an artificial page maximum.
- **Three explicit missingness denominators:** the focus-metric value box shows
  a count and percentage for one selected scope. *Within recorded time series*
  uses unique recorded timestamps and treats a duplicate timestamp as observed
  when any duplicate has a non-missing focus value. *Within regular series*
  adds only complete dominant-epoch intervals strictly between consecutive
  timestamps. *Within full local days* is the default and additionally pads
  the first and last recording boundaries to participant-local day boundaries.
  Explicit missing values, implicit gaps, and phase/jitter diagnostics remain
  separate.
- **VEET phase/jitter handling:** ordinary timestamp phase changes are retained
  and reported as irregular intervals but no longer manufacture gaps merely
  because later observations do not align with one globally anchored grid.
  This keeps an actually recorded VEET observation credited while making the
  selected denominator inspectable.
- **Compact, aligned controls:** icon-bearing summary and quality boxes remain
  approximately 76–79 px high at the verified desktop sizes. The focus metric
  is the first compact summary-box control and its bullseye tooltip explains
  its analytical scope. Tabs, select contents, plot-header controls, and page
  controls use optical centering. The focus-missingness scope is contained in
  its value box, and all value-box explanations remain available through
  tooltips.
- **Exact plot scope:** participant-specific timelines use complete,
  day-aligned windows of equal duration for every facet, with zero x expansion.
  This avoids both the former extra trailing day and visually misaligned
  participant days. Participant labels are bold, empty slots remain on a short
  final participant page, and the plot grows from a tested minimum height as
  more participant slots are shown. Pages longer than seven days pass a
  ggplot2 waiver rather than `NULL` for minor date breaks, covering the former
  14-day `date_minor_breaks` error.
- **Scale inspection:** logarithmic mode surfaces the total omitted-value count
  beside the scale control; its tooltip distinguishes exact zeros from negative
  values and states that neither is changed in storage. The Y-range popover
  contains the symlog linear-range selector plus optional finite y-axis minimum
  and maximum fields. The latter apply a display-only `coord_cartesian()` zoom;
  invalid, reversed, or non-positive logarithmic limits are rejected without
  changing data. Symlog adds unlabeled raw-value tenth axis ticks between
  adjacent major breaks without adding minor gridlines. They are evenly spaced
  in its linear center and visibly compress in its logarithmic outer regions.
- **View hierarchy and scope information:** Explore now uses its tab content
  only for the view controls and plot. Concise scope, bounded-display, scale,
  and manual-range context lives in the plot title/subtitle and targeted help,
  not a box below the plot. Source provenance and source import diagnostics
  live with Source data. Recipe and grouping state live with Pre-processed
  data beside a distinct post-recipe integrity summary.
- **Recommendation and two-dimensional paging:** the former large
  recommendation callout is a persistent slim **Recommended view** action. It
  becomes a focusable inactive status with an “already the recommended view”
  tooltip when the recommendation is active; otherwise it reapplies the
  recommendation and explains the reason in a corner notification. Four
  participants per page is part of that fixed recommendation target and any
  different choice correctly marks the view as adjusted. Users can choose the
  maximum participants per page. Participant movement uses two full-height,
  half-plot up/down controls beside the plot, while time movement uses compact
  arrow-and-calendar controls below it. A clickable
  participant-by-time navigator shows focus-bearing pages as filled,
  focus-empty but valid pages as hollow, and the active page in the signal
  color. Its atomic page state changes both independent axes together without
  leaving an intermediate selection. The navigator has no visible label or
  internal scrollbar; its rectangles shrink to fit the available columns.
  Participant and time selectors are bounded, and page-turn buttons retain
  clear action backgrounds. Show all replaces the participant selector with
  one compact all-participants scope summary.
- **Tables and import checks:** the pre-processed variable inventory shows its
  fixed 10-row pages in a card tall enough to show a complete page without
  vertical scrolling. Source and pre-processed data retain fixed 10-row pages
  and horizontal scrolling without a vertically scrolling table card. Their
  compact/full-schema switch and external **Choose columns** control stay
  visible above the scrolling region. Participant-day coverage likewise uses
  fixed 10-row pages and a contained horizontal scroll area with non-wrapping
  headers. Source diagnostics remain folded in the Source data tab and use
  status-specific pass, warning, and information icons.

Targeted regression coverage in
`tests/testthat/test-dashboard-refinements.R` comprises 152 passing expectations
with no failures or errors. It covers the three denominators, duplicate and
explicit-NA accounting, regular gaps, full-day padding, phase-shifted
timestamps, y-limit validation and plot attributes, 14-day construction, exact
participant x ranges, log omissions, coverage labels, UI hooks, table paging,
diagnostic icons, scope-help placement, tab placement, alignment hooks, and
pre-processed integrity. The full package test suite also passes; its two
skips are the documented mirai-daemon checks that are unavailable in this test
runtime.

The fresh workbench and recommendation/page-size contracts add 99 passing
expectations in `tests/testthat/test-dashboard-navigation.R`. They preserve all
existing reactive input/output IDs, assert that the former nested plot-card and
persistent navigation-help structures are absent, verify the compact pager
markup and inline arrow/calendar icons, verify that persistent scope
disclosures remain absent, and verify that initialization and Recommended view
restore four participants per page. Pure recommendation tests also cover
non-finite-only focus spans while
retaining finite zero and negative observations.

The final fresh-workbench browser pass used the complete 839,607-row ActLumus
record at a 1265 px desktop viewport. Show all produced five participant pages
by two time pages and ten directly selectable navigator rectangles. The active
page was orange, populated pages were filled, and the valid empty page was
hollow. The navigator, its options group, the Explorer workbench, and the
document all had equal client and scroll widths; no horizontal scrollbar or
document overflow was present. Participant and time selectors stayed on one
row, the time selector remained bounded, the next-page control retained its
filled action background, and both full-height participant-page buttons were
present beside the plot. The complete package suite passed with no failures or
errors and the same two documented mirai skips.

Current in-app-browser evidence covers the default sample, VEET ALS, and All
ActLumus at 991 × 1324 and 1600 × 1200. No document-level horizontal overflow
or visible Shiny output error occurred, and summary boxes measured about
76–79 px high. For VEET ALS, the live selector reported Recorded
0/173,007 missing (0.00%), Regular 18,283/191,290 (9.56%), and Full days
86,028/259,035 (33.21%). The All ActLumus 14-day option was also exercised in
the live UI: one plot rendered successfully with no visible Shiny output error
and no `date_minor_breaks` message. Logarithmic mode reported 7,105 omitted
zeros, and a manual 1–1,000 lux display range applied without a Shiny output
error. Both 839,607-row source and pre-processed tables showed ten rows without
vertical table scrolling. The browser console contained no errors; its five
warnings were bootstrap-datepicker locale-deprecation notices from the
dependency bundle, and the R server emitted no warning during this pass.

The final owner-feedback browser pass used 1280 × 720 and 1600 × 1200
viewports. The Explore sidebar and plot surface shared the same top, bottom,
and height after removing the inset inner-card geometry; document-level
horizontal overflow remained zero. All visible plot-header controls shared one
baseline at wide width. The y-range inputs remained inside their popover, the
missingness menu extended beyond its compact value box without clipping, and
the participant checkbox was vertically centered. No persistent scope
disclosure remained beneath the plot. Participant-day coverage showed ten rows,
non-wrapping headers, one contained horizontal scrollbar, and no vertical
scrollbar. Source provenance and folded source checks were visible only under
Source data; the five post-recipe integrity checks were visible only under
Pre-processed data. The final browser console and R server emitted no warning
or error.

The final symlog and pager pass loaded the full 839,607-row ActLumus dataset in
the isolated module at 1280 × 720. The detailed plot showed nine subtle minor
axis ticks between every adjacent pair of major y breaks. The linear center
therefore retained even subdivisions while outer raw-value tenths visibly
compressed after transformation. The rebuilt single-row dock aligned its
participant selector, page navigator, time selector, and page buttons on one
vertical centerline and retained a measured 36 px navigator-to-time-control
gutter. The focused dashboard data, navigation, and refinement suites completed
successfully; the only warning reported that the installed Shiny binary was
built under R 4.5.2 while the verification process used R 4.5.0.

The final paging refinement was then exercised against the complete
839,607-row ActLumus record. Show all produced five participant pages by two
time pages; the navigator correctly rendered the participant 17–20 ×
measurement-days 8–12 cell as hollow. Selecting that cell atomically changed
the status to participant page 5 and time page 2, retained the complete
dashboard, and rendered the explicit no-focus-measurements state. Returning to
participant page 1 and time page 1 restored the populated coverage view.
Changing the maximum from four to six participants recomputed the compact
scope to four participant pages and rebuilt the navigator without a server
warning or Shiny output error.

## Owner-review and UI/UX revision (2026-07-23/24; superseded where noted)

The 2026-07-23 revision replaces the initial showcase contract where owner
feedback found scientific ambiguity or inefficient layout:

- **Real sources:** the lazy-loaded showcase now contains LightLogR
  `sample.data.environment`, IZTECH, all 20 ActLumus files (839,607 rows), all
  eight Speccy files (121,060 rows) grouped into ID01/ID02/ID04 with
  `^(ID[0-9]{2})`, and `02_VEET_L` imported separately as ALS (173,007 rows)
  and PHO (173,013 rows). No large record is loaded before selection.
- **Focus metric:** every eligible numeric source variable can be selected.
  Focus missingness reports count and percentage for that metric only.
  Participant-day coverage counts non-missing focus values; exact zero counts
  as observed. Explicit missing focus epochs and absent implicit-gap epochs are
  separate.
- **Explore state:** the plot states and can toggle source versus pre-processed
  data. Absolute calendar scope is separate from participant measurement
  duration. Users can retain each participant's own recorded dates, align
  recordings by days from first measurement, share an absolute date window, or
  page through calendar weeks by weekday. Participant selection refits the
  relevant window to the selected recordings. Detailed absolute-time views use
  the explicit `Local Date/Time (timezone)` axis title; the elapsed view alone
  uses `Days from first measurement`.
- **View resolution:** Automatic evaluates each selected participant's actual
  available focus-metric span, not the distance between the earliest and latest
  dates in the group. Staggered three-day recordings therefore remain detailed
  even when their combined calendar envelope is several months. Detailed and
  Coverage overview remain explicit manual choices at every scope.
- **Recommendation contract:** one pure recommendation object supplies the
  initial time reference, full measurement-duration scope, page length,
  Automatic resolution, scale, explanation, and reset target. The neutral
  start is Own recording dates + full duration + seven-day pages. It avoids
  inventing shared calendar or elapsed-study-day intent. The sidebar says
  “Recommended starting view” while controls match and “View adjusted” after
  any relevant user change; adjustments remain untouched until the user
  explicitly applies the recommendation.
- **Single source of defaults:** `dashboard_view_defaults()` feeds both the UI
  controls and `dashboard_view_recommendation()`. Scale and symlog preferences
  remain session-scoped, survive reactive re-rendering, and reset from the
  recommendation only on an explicit action or new dataset context.
- **Two-dimensional paging:** participant pages move up/down and time pages
  move earlier/later. The participant-page selector is a true child of the
  resizable Explorer sidebar; the page map and time controls are a footer of
  the visualization canvas. Time-page size is selectable, and the two page
  states are independent. Boundary buttons are disabled.
- **Detailed rendering:** LightLogR `gg_days()` supplies participant facets.
  Participant-recording mode uses free date axes so people recorded at
  different dates remain simultaneously visible; elapsed mode aligns them on
  measurement day 1. This follows the official
  [`gg_days()` reference](https://tscnlab.github.io/LightLogR/reference/gg_days.html)
  and its treatment of grouped facets and free x scales, together with the
  [LightLogR visualization guide](https://tscnlab.github.io/LightLogR/articles/Visualizations.html).
- **Scale:** detailed plots offer LightLogR symlog, linear, and log scaling.
  Symlog provides adjustable ±10 through ±0.001 linear thresholds in the
  Y-range popover. Its major ticks explicitly show zero, the threshold, and
  outer powers of ten. Unlabelled raw-value tenths between those breaks make
  the center's linear spacing and the outer regions' logarithmic compression
  visible. The LightLogR transform scale follows the selected threshold so the
  central half-range and each outer ×10 step occupy equal axis space. The log
  notice reports how many non-positive values are omitted from that display
  only.
- **Quality:** compact icon-bearing value boxes have tooltips. Zero detected
  issues are green and say “None detected”; an issue or unavailable result is
  orange and says “Review”. The boxes report explicit missing focus epochs,
  implicit gaps, irregular timestamps, and DST transitions rather than
  duplicating timestamp diagnostics.
- **Tables:** all tables use server-side search, sorting, fixed 10-row pages,
  and no per-column filter row or page-size selector. Source and pre-processed
  tables default to grouping/Id, Datetime, and the focus metric; the full schema
  remains available with column visibility and horizontal scrolling. Visible
  timestamps use the dataset display timezone, exact numeric zero is shown as
  `0`, and the timezone is repeated in each data-view header and caption.
- **Layout and language:** the dashboard has no maximum width, controls are
  compact, metric cards remain 76 px tall at desktop and mobile widths, and
  visible wording uses “source” and “pre-processed” rather than “canonical”
  and “prepared.”
- **Import checks:** checks are collapsed below participant-day coverage. For
  appended data they describe the combined source table, while per-source
  manifests and diagnostics remain in append provenance.
- **Empty scope:** a coverage page with no focus measurements renders a labelled
  empty-state plot. A detailed page reports that no focus measurements occur on
  that participant/time page and directs the user to either axis.

The revised 432,000-row scale acceptance completed with three participant
pages, five seven-day time pages, a 7.5 KiB / 28-cell current-page overview,
10-row server-side table pages, the recommendation-active invariant, and the
symlog path. Regression tests construct and build `gg_days()` plots for
participant-specific, elapsed, and shared-date axes. The complete testthat
suite passed; development `R CMD check` finished with 0 errors, 0 warnings,
and 0 notes.

### Final real-browser verification

The 2026-07-24 pass exercised the real showcase at the desktop viewport and
390 × 844:

- LightLogR testdata: 69,120 rows and two participants/series;
- IZTECH: 151,200 rows and 17 participants;
- all ActLumus files: 839,607 rows and 20 extracted IDs;
- all Speccy files: 121,060 rows and the regex-derived IDs ID01, ID02, and ID04;
- `02_VEET_L` ALS: 173,007 rows with Lux as the default focus metric; and
- `02_VEET_L` PHO: 173,013 rows with selectable spectral-channel metrics.

There was no document-level horizontal overflow or visible Shiny output error
while switching datasets, source/pre-processed state, focus metrics, or scale.
The 27-column ActLumus and 92-column Speccy tables retained horizontal scrolling
inside their cards. ActLumus supplied the semantic outcome-state check:
232,458 implicit gaps and 64,065 irregular timestamps rendered orange with
“Review”, while zero issue counts rendered green with “None detected”.
The VEET ALS sub-lux view verified readable symlog ticks at 0, 0.001, 0.01,
0.1, 1, and outer decades.

### Recommendation-system completion audit

The recommendation revision was checked against the goal as a separate,
inspectable contract:

| Requirement | Current evidence |
| --- | --- |
| One lean source of initial states | `dashboard_view_defaults()` supplies Own recording dates, Automatic, seven days per page, symlog, and a ±1 linear range to both UI and recommendation code. |
| Context-sensitive but non-prescriptive behavior | `dashboard_view_recommendation()` uses the selected focus view and participants to include the complete measurement duration and resolve Automatic from actual per-participant focus span. It does not infer elapsed or weekday intent. |
| User adjustment | `dashboard_recommendation_state()` distinguishes meaningful view changes from ordinary participant/time page navigation. The module shows “View adjusted” and retains the current values. |
| Explicit reset | A recording `MockShinySession` verifies the exact native Shiny reset messages for `time_basis = participant`, `view_mode = auto`, `days_per_page = 7`, `plot_scale = symlog`, and `symlog_threshold = 1`. |
| Recommended initialization for a new dataset | The same message-level test verifies pre-processed data, show-all off, Own recording dates, Automatic, seven-day pages, symlog, and ±1 are emitted when the dataset context initializes. |
| Empty focus data | An all-missing focus fixture recommends Coverage overview so the labelled empty coverage state is visible instead of an empty detailed plot. |
| Reviewed real sources | Tests assert the recommendation for every showcase record: testdata, IZTECH, ALS, and PHO resolve Detailed; ActLumus and Speccy resolve Coverage. All retain the same neutral, adjustable contract. |
| Scale and package limits | The 432,000-row acceptance keeps three participant pages, five time pages, and 28 current-page cells. The full suite passes and `R CMD check` completes 0/0/0. |

No bespoke JavaScript was added. The recommendation panel and reset use native
Shiny/bslib controls and session-scoped reactive values. The in-app browser's
localhost policy left its previously open tabs on the stale connection-error
page, so the recommendation revision was not re-screenshot or console-inspected
through that browser surface. The isolated real-data showcase was started
successfully at `http://127.0.0.1:8124/`; the exact update-message contract,
rendered callout HTML, normal/adjusted module states, real datasets, Sass, and
package behavior are covered automatically.

### Symlog axis rationale

The implementation follows the official
[LightLogR `symlog_trans()` reference](https://tscnlab.github.io/LightLogR/reference/symlog_trans.html)
and its
[visualization guidance](https://tscnlab.github.io/LightLogR/articles/Visualizations.html):
exact zero remains visible, values inside the threshold are linear, and values
outside it progress logarithmically. The dashboard additionally uses the
documented `scale` parameter to keep threshold changes geometrically coherent
and supplies explicit breaks and labels through
[ggplot2 continuous scales](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
Raw-value tenth axis ticks are added between adjacent major breaks. This
keeps ten equal steps in the linear center while their transformed spacing
compresses non-uniformly between outer powers of ten. Together with the
explicit major labels, this avoids stacked automatic ticks and makes the
chosen threshold inspectable instead of treating symlog as an unexplained
visual effect.

## Initial implementation record (superseded where the revision differs)

## Scope completed

Milestone 5 is implemented for canonical LightLogR imports with an empty
preparation recipe and no active grouping. The dashboard does not require GLC
state or optional factual metadata. It exposes source-agnostic contracts that
later milestones can enrich without changing the raw/prepared, plot-preview,
coverage, or server-side-table boundaries.

The production dashboard now provides:

- available source provenance with explicit unavailable values;
- participant count, participant-local date span, dominant epoch inventory,
  variable types and roles, missingness, exact measured zeros, raw import
  quality, and participant-day coverage;
- distinct counts for implicit gaps, irregular timestamps, duplicate timestamp
  rows, and daylight-saving transitions;
- an explicit empty-recipe state, dataset/recipe revision, canonical/prepared
  equality, and explicit no-grouping state;
- participant, local-date-window, show-all, and facet-page controls;
- an automatically selected detailed timeline for at most four requested
  participants and seven local days, otherwise a daily availability overview;
- a 12,000-row detailed-plot budget and 2,000-cell availability budget, with
  deterministic even sampling or participant-day binning and an on-screen
  reduction notice;
- separate prepared and immutable canonical-raw panels with different visual
  treatments and state explanations; and
- server-side DT tables for prepared, raw, inventory, and coverage data with
  global and per-column search, sorting, column visibility, pagination, and
  type-aware numeric/temporal presentation.

No bespoke JavaScript was added. The implementation uses Shiny, bslib, DT,
ggplot2, the existing LightLogWeb design system, and pure R helpers.

## Scientific and data contracts

The dashboard keeps three statements separate:

1. the canonical base contains recorded and imported values;
2. import-quality provenance diagnoses the canonical base; and
3. the active prepared result is derived from the canonical base and committed
   recipe.

For the current empty recipe, the prepared payload must be byte-identical to
the canonical payload. The interface states this rather than implying that an
unrecorded cleaning step has occurred. Missing measurements, exact zero,
implicit gaps, irregular timestamps, duplicates, non-wear, and sleep are not
collapsed into one state. Gap counts do not expand or impute the time series.

Participant-day coverage is descriptive and imposes no completeness threshold.
It counts unique on-grid timestamps against each participant's dominant epoch
over the full participant-local calendar day. Therefore partial first and last
days remain visible. DST days retain their actual 23-, 24-, or 25-hour absolute
duration. Off-grid observations and duplicate rows are reported separately and
do not inflate regular-epoch coverage. When no dominant epoch can be estimated,
coverage is explicitly unavailable.

Timestamps are retained as absolute POSIXct instants. Local dates are derived
with `with_tz()` semantics from a validated analysis/source/Datetime timezone;
the dashboard never reinterprets a clock label with `force_tz()`.

## Reusable implementation boundaries

- `dashboard_dataset_snapshot()` validates and materializes one memoized
  dashboard snapshot when the selected dataset changes. It contains raw and
  prepared data, provenance, variable inventory, quality, local dates, coverage,
  recipe, and grouping state.
- `dashboard_daily_coverage()` is a pure participant-day coverage calculation.
- `dashboard_plot_selection()` resolves participant/date scope, facet pages,
  show-all warnings, and the detailed-versus-availability strategy.
- `dashboard_plot_preview()` is the only path from full prepared data into a
  plot. It enforces the row/cell budgets and returns the reduction notice.
- `dashboard_table_contract()` records the server-side DT invariant.
  `datasetDashboardServer()` passes `server = TRUE` explicitly for every table.
- `dashboard_datatable()` centralizes search, sort, pagination, column
  visibility, type classes, integer formatting, and six-significant-digit
  formatting for continuous numeric variables.
- `dataset_dashboard_app()` remains a runnable isolated showcase, but its
  visible choices are reviewed real records: installed LightLogR testdata, the
  pinned IZTECH snapshot when available, the repository-provided ActLumus 4789
  file, and the recoverable empty state. Synthetic records remain available
  only to deterministic unit and scale-acceptance tests.

All state is session-scoped. The helpers do not mutate source data, write files,
cache across sessions, or contact external services.

## Acceptance evidence

### Exact scale fixture

`dev/run-milestone-5-scale-acceptance.R` generates, in memory, the required
10-participant × 30-day × one-minute fixture (432,000 rows) without optional
metadata or GLC state. On the local acceptance run:

- canonical data size: 8.2 MiB;
- raw quality audit: 0.344 seconds elapsed;
- dashboard snapshot, including 300 participant-days: 0.938 seconds elapsed;
- automatic mode: daily availability overview;
- facet pages: three pages of at most four participants;
- plot payload: 120 participant-day cells, 12.3 KiB;
- table page length: 25 rows, server-side; and
- a two-participant, two-day selection switched to a complete 5,760-row
  detailed timeline without reduction.

The timings describe this local run, not a production capacity guarantee. The
script asserts structure and budgets rather than fragile elapsed-time limits.

### Automated checks

The full testthat suite passed with no failures, including focused dashboard,
module-contract, app-smoke, and design-system coverage. The dashboard checks
cover:

- empty recipe and grouping states;
- full-day coverage, one missing epoch, an irregular timestamp, a duplicate,
  and exact zero as separate cases;
- detailed/availability switching and show-all facet pagination;
- both plot budgets and their displayed notices;
- light/dark plot construction;
- server-side DT configuration, search, ordering, pagination, Buttons column
  visibility, and type formatting;
- prepared/raw UI distinction;
- module status for small and scalable fixtures;
- a real-source catalog that excludes the synthetic ActLumus fixture; and
- exact real-record row counts, participant/series counts, automatic plot
  modes, canonical/prepared equality, and empty-recipe state.

Two unrelated long-task cases were skipped because mirai daemons are unavailable
in this verification runtime. The suite's three warnings were the environment
warning that the locked Shiny binary had been built under R 4.5.2 while the
local executable is R 4.5.0, plus intentional notifications from two existing
background-task failure-path tests. All warning-producing tests passed.

A development package check (`cran = FALSE`, manuals and vignettes disabled)
completed with 0 errors, 0 warnings, and 0 notes.

### Real-browser checks

The isolated showcase was inspected in the Codex in-app browser at desktop and
390 × 844 mobile viewports. The following paths were exercised:

1. small fixture → detailed timeline with all 384 rows and no reduction;
2. scalable fixture → 30-day availability overview;
3. show all → explicit 10-participant, three-page warning;
4. facet page 2 → four of ten participants and 120 plot cells;
5. date end changed to 2026-01-07 → automatic switch to a 672-row detailed
   timeline for four participants;
6. quality tab → variable inventory, canonical diagnostics, daily coverage,
   search, page-size, sorting affordances, and column-visibility menu;
7. prepared table → 7,200 rows, revision r0, empty-recipe state, ISO timestamp
   display, and six-significant-digit numeric values;
8. global search `P10` → 720 matching rows with 25 shown on page 1 of 29;
9. canonical raw and prepared panels → visibly distinct orange and teal states;
10. dark mode and mobile layout → no horizontal document overflow; and
11. no selected dataset → recoverable modal with a Go to import action.

No browser-console errors were recorded. Five warnings came from Shiny's bundled
Bootstrap datepicker language aliases and filename deprecations; none originated
in LightLogWeb code.

### Owner follow-up: reviewed real datasets

After the owner requested real data in the visible showcase, the fixture
selector was replaced with the same immutable records used by the dataset
library. No showcase row is synthesized, filled, or imputed:

| Visible source | Shape | Scope and automatic view | Explicit limitations |
| --- | ---: | --- | --- |
| LightLogR `sample.data.environment` | 69,120 × 3 | Two recorded series, Europe/Berlin, mixed 10/30-second dominant epochs; detailed timeline bounded to 12,000 evenly sampled plot rows | None of the canonical values are changed |
| IZTECH light glasses | 151,200 × 37 | 17 anonymous participants, Europe/Istanbul, 60-second dominant epoch; daily availability overview | 1,140,480 missing cells across eight variables; measurement units and absent metadata remain unknown |
| Provided ActLumus 4789 export | 4,686 × 27 | One participant, Europe/Berlin, 60-second dominant epoch; complete detailed timeline | Calibration remains unknown |

All three views were exercised in the in-app browser. The ActLumus file also
exercised the 27-column prepared table and exposed a real-only layout defect:
the global accessible wrapping rule compressed long DataTables headers. The
dashboard table override now restores normal overflow wrapping and keeps
headers on one line, leaving the table horizontally scrollable rather than
stacking header letters. Plot and top-of-dashboard screenshots were retained
for each source. The browser's localhost policy prevented reopening the preview
after the final stylesheet restart, so the final header rule is additionally
covered by Sass compilation and package verification rather than a second
browser capture.

## Version and boundary record

The implementation baseline was checked against:

- R 4.5.0;
- LightLogR 0.10.3;
- Shiny 1.14.0 from the project lockfile;
- bslib 0.11.0;
- DT 0.34.0;
- testthat 3.3.2; and
- shinytest2 0.5.1 in the lockfile.

The active project `renv` bootstrap was lock-contended by earlier R processes
during this work. Verification therefore used the exact cached locked Shiny and
testthat packages with `Rscript --vanilla`, plus the locally installed versions
of the remaining locked dependencies. No package was installed or updated.

GLC-specific revision/provenance panels, metadata-driven labels and units,
preprocessing editors, and grouping editors remain intentionally assigned to
Milestones 6–9. Milestone 5 shows raw variable names and “Unit not specified”
when optional metadata are absent.

## Owner test script

Run `dataset_dashboard_app()` after `devtools::load_all()` and:

1. keep **LightLogR testdata - sample.data.environment** selected and confirm
   69,120 rows, two series, the Automatic → Detailed badge, the 12,000-row
   bounded preview, source provenance, empty recipe, and no active grouping;
2. choose **IZTECH light glasses - 151,200 rows** and confirm 17 participants,
   Europe/Istanbul, participant-specific dates, two time pages for the longest
   selected recording, and a detailed automatic view based on the actual
   per-participant focus duration rather than the group calendar envelope;
3. choose **All ActLumus files** and confirm 839,607 rows, 20 IDs, and orange
   “Review” states for detected gaps and irregular timestamps. Include all
   participants, vary **Max. participants per page**, and confirm the compact
   all-participants scope and participant-by-time navigator update together.
   Filled cells contain focus data; hollow cells remain clickable and show an
   explicit empty page. Confirm the full-height Up/Down participant controls
   and compact arrow/calendar time controls remain independent. Set **Max.
   days per time page** to **14 days** and confirm the plot renders without a
   minor-date-break error;
4. choose **All Speccy files** and confirm 121,060 rows, IDs ID01/ID02/ID04,
   and contained horizontal scrolling through the 92-column full schema;
5. inspect both `02_VEET_L` choices. For ALS, switch missingness among
   **Recorded times**, **Regular span**, and **Full days** and confirm the
   displayed count and denominator change without turning phase shifts into
   gaps. For PHO, select a spectral channel such as `s480`;
6. exercise source/pre-processed, symlog, linear, and logarithmic views. In ALS
   symlog mode choose **±0.001** and confirm explicit 0, 0.001, 0.01, 0.1, 1,
   and outer-decade ticks. In logarithmic mode confirm the total omitted-value
   badge and its zero/negative tooltip, then apply and clear a valid manual
   y-axis range;
7. switch **Time reference** among Own recording dates, Days from first
   measurement, Shared calendar dates, and the weekday-aligned calendar-week
   choice.
   Confirm participant changes refit the scope and only relevant page axes
   appear. Adjust a view setting, use the persistent **Recommended view**
   action, and confirm its rationale appears in a corner notification;
8. force Detailed timeline and Coverage overview over the same scope and
   confirm neither choice changes or aggregates the stored data. Open the
   folded detailed-scope information and confirm its page, display-reduction,
   and scale bullets;
9. open **Quality & coverage** for each source. Confirm the variable inventory
   and participant-day coverage both retain fixed 10-row pages, each complete
   page is visible without vertical card scrolling, and the non-wrapping
   coverage columns scroll horizontally;
10. compare **Pre-processed data** and **Source data**, including main-column
    and full-schema modes, the external **Choose columns** control, fixed
    10-row pages, and no vertical table scrollbar. Confirm recipe/grouping and
    post-recipe integrity under Pre-processed data, and provenance plus folded
    colored import checks under Source data; and
11. choose **No selected dataset** and confirm the recoverable modal.
