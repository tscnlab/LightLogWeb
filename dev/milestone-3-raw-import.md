# Milestone 3 — robust raw import and test datasets

Date: 2026-07-21

Status: implemented and ready for owner acceptance after automated and browser
verification. This evidence is development-only and excluded from package
builds.

## Compatibility basis

- R 4.5.0
- LightLogR 0.10.3
- Shiny 1.14.0
- bslib 0.11.0
- mirai 2.5.1 or newer
- melidosData 1.0.6 for the pinned development snapshot

Devices and format versions come from the installed LightLogR public
`supported_devices()` and `supported_versions()` APIs. Imports use
`import_Dataset()`, epoch estimation uses `dominant_epoch()`, and the recording
overview uses `gg_overview()`. The relevant
public contracts are the [LightLogR import reference](https://tscnlab.github.io/LightLogR/reference/import_Dataset.html)
and [import and cleaning article](https://tscnlab.github.io/LightLogR/articles/Import.html).

LightLogR 0.10.3 does not expose a machine-readable file-extension registry.
LightLogWeb therefore has one narrow, tested adapter based on the exported
`ll_import_expr()` readers and documented examples. A device newly returned by
LightLogR but absent from this adapter fails closed. `GENEActiv_GGIR` is listed
but unavailable through browser upload because its public contract requires a
parent directory containing `meta/basic`; a browser file selection cannot
preserve that directory contract.

## Import boundary

1. Shiny and the session runtime independently enforce the selected total
   request limit. The hosted profile has a hard 200 MiB safety ceiling. The
   local profile defaults to 200 MiB but accepts an explicitly higher limit
   when the operator has provisioned disk, parsing time, and memory.
2. Every upload is copied byte-for-byte into its own generated private
   subdirectory while retaining its safe original basename. This lets
   LightLogR apply its documented filename-ID behavior without permitting
   uploads to overwrite one another. Session and per-action directories use
   mode `0700` where the platform supports Unix permissions. SHA-256 and byte
   size are checked on both sides of the copy and rechecked in the worker.
3. Original basenames, private paths, hashes, and sizes remain separate. Worker
   requests contain staged paths; durable source manifests contain original
   names and hashes but never session paths.
4. ZIP uploads must contain exactly one device-export member after harmless
   macOS metadata entries are ignored. All member paths are checked for
   traversal, the data-member extension must match the selected device, and
   the total expanded size must remain within the request limit. LightLogR
   0.10.3 reads VEET exports as plain text, so LightLogWeb extracts a validated
   VEET CSV/TXT inside the background worker and passes only that private,
   derived copy to LightLogR. The original ZIP name, bytes, size, and SHA-256
   remain the recorded source provenance; the derived path is never persisted.
5. Filename-to-participant mapping is previewed before import. Several files
   may map to one participant because separate exports can collectively form
   that participant's record. Each source filename remains available for
   provenance, and overlapping participant timestamps are reported in the
   post-import quality review. LightLogR creates the final `Id`: an embedded
   `Id` remains authoritative, while files without one use the previewed
   `manual.id` or `auto.id` fallback against the original basename. LightLogWeb
   never rewrites that result after import.
6. Device, version, source IANA time zone, extensions, readability, compatible
   per-file names/types, and consequential duplicate/DST options are validated
   before a complete import.

The worker runs five named phases: validation, LightLogR import, normalization,
full-data quality audit, and bounded preview. Clean mirai daemons load the
installed LightLogWeb namespace, or the current `pkgload` source namespace in
development, before resolving an internal worker by name. Failed worker
startup falls back to the existing synchronous profile with a visible warning.

## Scientific and time-series contract

- `Id` must be non-missing and usable; `Datetime` must be complete `POSIXct`.
- The imported timestamp display zone must exactly match the explicitly chosen
  source IANA zone. There is no silent UTC default in the form.
- DST adjustment and identical-row removal are opt-in import decisions.
- DST diagnostics compare local-clock labels on a copy; absolute instants are
  never reinterpreted by the audit.
- Ordering, duplicate `Id`/`Datetime` rows, participant spans, participant
  count, dominant epoch, explicit missingness, implicit gap epochs and
  episodes, off-grid observations, and DST transitions are computed from the
  complete imported data.
- Implicit missing-time-point counts use each participant's LightLogR dominant
  epoch and vectorized position arithmetic without materializing a gap-filled
  table. This keeps mixed-epoch participants separate and avoids expensive
  interval extraction during import. The same sorted timestamps provide a
  compact derived table of consecutive-observation lags that exceed that
  participant's dominant epoch; it is used only for the overview overlay.
  Missing timestamps and missing sensor values are never imputed, and missing
  values are never replaced with zero.
- Exact zeros, negative values, finite minima/maxima, quartiles, means, medians,
  and missing values are profiled separately and shown in the post-import review.
- The recording overview is LightLogR's public `gg_overview()` with the compact
  derived long-interval table supplied as its gap overlay. Recording spans,
  starts, ends, and long intervals retain distinct accessible LightLogWeb
  styling. A multi-line caption states the lag rule, gives the source time zone,
  and names the summary and detailed panels that report missing-time-point
  counts.
- Each import-warning bullet and detailed quality check has a short tooltip
  explaining the condition and scientifically cautious options before and
  after import.
- The row preview is capped at 100 deterministic first/last rows and explicitly
  states that the quality audit used the complete dataset.
- The detailed review is a four-panel accordion for checks, numeric variables,
  participant-level quality, and row preview. Its parent review remains open,
  while opening one detail panel closes the previous detail panel.
- Only scalar numeric columns with finite observations are structurally
  selectable as the initial analysis focus. Matrix/array, nested, structural,
  temporal, non-numeric, all-missing, and infinite-valued columns are excluded.
  The combined variable table marks eligibility with a labeled green check or
  red cross, gives a reason only when a column is unavailable, and places the
  scientific caveat once above the table. The UI states that the focus can be
  changed later and that selectability is not evidence of valid units,
  calibration, placement, or comparability.

## Test datasets

`LightLogR::sample.data.environment` is immediately available in the main app
with device, site, MEDI label/unit, source-zone, package-version, quality, and
eligibility provenance.

The development-only IZTECH fixture is documented in
`dev/fixtures/README.md`. It is a no-row/no-column-change RDS serialization of
the current CRAN melidosData 1.0.6 `light_glasses_1minute`/IZTECH result:
151,200 rows, 37 columns, 17 anonymous participants, and
`Europe/Istanbul`. It is pinned to source commit
`1f83aa1563603610a4f43afe2a0fbe30f805191c`, Git blob
`8a6aef8c7c0e842eaa167d22fef634848b57508a`, and snapshot SHA-256
`939bea8c78578db3b9840e20a505b0366e3c3ee347c1d838f2c6781844a65ab8`.
The runtime loader verifies this checksum before deserialization and then
checks the exact dimensions, names, participant count, source time zone, and
primary variable before constructing the immutable dataset record.
The source dataset is [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
and is attributed to [Didikoglu et al. (2025), dataset version 1.0.1](https://doi.org/10.5281/zenodo.16568109).

Routine tests never access the network. Small fictional ActLumus files are
generated in temporary directories; the official compressed ActLumus example
comes from the installed LightLogR package; and the IZTECH snapshot is local
and excluded from package archives.

The optional repository-local `testdevices/` folder is also excluded from
package builds. It can hold large development exports without becoming runtime
data. `dev/run-testdevice-import-acceptance.R` currently pairs plain and zipped
VEET exports for the original LightLogR format (`initial`) and current format
(`default`, presently 2.1.7), imports the ALS modality, and requires identical
row counts and payload hashes within each pair. Run it from the repository root
when those local files are available.

On 2026-07-21, that acceptance imported 304,193 ALS rows from both the plain
and zipped `01_VEET_L` export with `version = "initial"`, and 173,007 ALS rows
from both forms of `02_VEET_L` with the current default. Each plain/ZIP pair
had the same serialized measurement-payload SHA-256 after excluding only the
source-derived `Id` and `file.name` labels. The validated expanded sizes were
255.5 MiB and 152.6 MiB, confirming that the explicitly configured 1 GiB local
showcase limit covers these development cases while the hosted 200 MiB ceiling
remains unchanged.

## Hosting a 200 MiB request

The launcher sets `shiny.maxRequestSize` to the selected runtime limit and
restores the previous option when the app stops. The reverse proxy must permit
a request slightly larger than the app limit to accommodate multipart headers,
or leave body-size enforcement to Shiny. Posit's current troubleshooting guide
notes that proxy rejection appears as HTTP 413 and gives
`client_max_body_size 0` for NGINX and `LimitRequestBody 0` for Apache as
unlimited examples: [Posit Connect troubleshooting](https://docs.posit.co/connect/admin/appendix/troubleshooting/).
WebSocket forwarding must also follow the host's proxy guidance:
[Posit Connect proxy configuration](https://docs.posit.co/connect/admin/proxy/).

For a bounded production configuration, set the proxy request-body allowance
above 200 MiB rather than exactly 200 MiB, keep the Shiny limit at 200 MiB, and
retain the hosted session-temp allowance of at least 1 GiB. Capacity planning
must include the web server's temporary upload, LightLogWeb's unchanged staged
copy, the parsed R object, and worker overhead. Do not expose session temporary
directories through the web server.

An explicitly local profile may set `max_upload_mb` above 200. That changes the
Shiny and runtime request boundary but not the physics of the operation: the
unchanged staged copy, decompressed/parsed object, full-data quality audit, and
worker overhead must all fit the local disk and memory budget. The session
temporary-storage guard remains authoritative and rejects an action before
copying when capacity is insufficient.

## Large-file acceptance

`dev/run-milestone-3-large-file-acceptance.R` generates sparse temporary files
and performs no network access. On 2026-07-20, a 209,649,664-byte (199.94 MiB)
malformed ActLumus upload staged byte-for-byte with SHA-256
`51132bb5964f5d0b115be4a256ee8a11d01b3d43d6fe1dba64b054c0f5c5ce06`,
then returned a file-specific `llw_import_error`. A 209,715,201-byte upload
returned an `llw_resource_error` before staging. `/usr/bin/time -l` reported
311,738,368 bytes (297.3 MiB) maximum resident set size, about 3.63% of an
8 GiB host and well below the 75% acceptance ceiling. The peak memory footprint
was 262,407,272 bytes (250.3 MiB). The accepted-path run took 2.49 seconds;
total process wall time was 5.09 seconds.

Run the acceptance again from the repository root with:

```sh
/usr/bin/time -l Rscript dev/run-milestone-3-large-file-acceptance.R
```

## Acceptance matrix

| Case | Expected contract |
|---|---|
| Valid fictional ActLumus export | Complete typed result with LightLogR-produced ID, original filename, full quality audit, and bounded preview |
| Official `.txt.zip` ActLumus example | Safe ZIP preflight and usable offline dataset |
| Malformed export | File-specific recoverable `llw_import_error`; retry remains available |
| Invalid or mismatched source zone | Recoverable validation/import error; no dataset record |
| Unsupported device/version/extension or directory-only format | Recoverable validation/unavailable-feature message |
| Missing `Id`, `Datetime`, or non-POSIXct timestamp | Recoverable canonical-contract error |
| Several files with one proposed participant ID | Allowed and combined under that participant; source filenames are retained and overlapping timestamps are reported |
| Embedded ID differs from filename fallback | LightLogR's embedded value is preserved and never rewritten after import |
| Identical observations | Blocked unless duplicate removal was explicitly enabled |
| Hosted near/over 200 MiB | Near-limit bytes stage and fail recoverably if malformed; over-limit bytes reject before copy |
| Explicit local limit over 200 MiB | Accepted by the local profile and still bounded by session storage and actual resources |
| Background worker | Serializable request completes in a clean mirai daemon |
| Immediate and IZTECH examples | Immutable dataset records with hashes, quality, eligibility, and source provenance |

## Requirement traceability

| Plan item | Direct evidence |
|---|---|
| 1. Replace unsafe legacy coupling | `stage_import_files()`, `raw_import_manifest_arguments()`, the explicit `importServer()` return, and import-boundary/module-contract tests |
| 2. Request and host limits | Hosted/local checks in `resolve_runtime_profile()`, Shiny `maxRequestSize`, runtime staging checks, this hosting section, and the generated large-file runner |
| 3. Unchanged staged bytes | Safe original basenames in isolated generated `0700` directories, size/SHA-256 verification before and inside the worker, and post-request tampering tests |
| 4. Dynamic devices and versions | `supported_devices()`/`supported_versions()` adapters and registry contract tests |
| 5. Complete preflight | Typed checks for size, extension/ZIP safety, device/version, IANA zone, mapping, duplicates, readability, and compatible preview schemas |
| 6. Mapping preview | `llw_filename_id_mapping`, the namespaced fallback preview, actual LightLogR `manual.id`/`auto.id` arguments, multi-file tests, and embedded-ID preservation regression |
| 7. Five visible phases | Timed pipeline phase records, task-state UI, clean mirai execution, cancellation/retry infrastructure, and phase snapshot tests |
| 8. Full-data quality audit | Canonical validation plus participant, gap, irregularity, DST, signal-profile, ordering, and bounded-preview tests |
| 9. Reviewed analysis focus | Combined numeric-summary/focus card, scalar numeric selectability, one shared interpretation caveat, explicit confirmation event, module and browser checks |
| 10. Two example datasets | `sample_dataset_record()` plus the checksum/schema-enforced IZTECH record, fixture generator, provenance README, and record tests |
| 11. Offline routine tests | Generated temporary ActLumus data, installed LightLogR ZIP, local pinned RDS, static source audit, and no network calls in the test suite |

## Verification commands

```sh
Rscript -e 'pkgload::load_all(quiet = TRUE); devtools::test()'
Rscript -e 'devtools::check(document = FALSE, error_on = "never")'
Rscript -e 'pkgload::load_all(quiet = TRUE); shiny::runApp(LightLogWeb:::import_app())'
```

## Verification result

The current owner-review build was verified on 2026-07-21:

- `devtools::test(reporter = "summary", stop_on_failure = TRUE)` passed the
  complete test suite, including valid, malformed/retry, timezone, unsupported,
  canonical-column, clean-worker, and request-boundary contracts.
- `devtools::check(document = FALSE, error_on = "never")` completed with
  0 errors, 0 warnings, and 0 notes.
- The browser imported the valid fictional ActLumus fixture in UTC, showed the
  full-data quality summary, selected `MEDI`, and returned a dataset record
  with 3 rows, the original source filename, UTC, and no quality warnings.
- The browser loaded both the LightLogR sample (69,120 rows, 2 participants,
  `Europe/Berlin`) and the pinned IZTECH snapshot (151,200 rows,
  17 participants, `Europe/Istanbul`).
- The detailed review rendered four independent accordion panels: data checks,
  numeric variables/analysis focus, participant-level quality, and row preview.
  Opening a detail panel kept the parent review open and closed the previously
  selected detail panel. The numeric panel visibly marks eligible and
  ineligible focus variables and retains the shared caveat for units,
  calibration, placement, and comparability.
- A pipeline regression confirms that LightLogR's console summary and returned
  data both use the original-basename participant ID rather than a generated
  staging key. Embedded IDs are preserved without post-import rewriting.
- The overview is created by `LightLogR::gg_overview()` with compact derived
  overlays for consecutive-observation lags longer than each participant's
  dominant epoch. A mixed-epoch regression confirms both the full-data counts
  and overlay thresholds remain attached to the correct participant, while the
  compact caption keeps the source time zone and dominant-epoch definition
  visible. Participant coverage is mentioned only when the plot is capped.
- On the 20-file, 839,607-row ActLumus review dataset, the revised quality phase
  completed in 1.081 seconds locally (previous reviewed build: 30.476 seconds)
  and retained the same 66,234 missing-time-point count. LightLogR import itself
  took 5.319 seconds in that measurement.
- The version tooltip is forced above its field, regular-expression examples
  retain visible inline code, and import status/progress have explicit spacing.
- The showcase uses one local background worker so queued/running state and the
  phase summary can repaint while LightLogR reads and checks the files. Each
  phase writes a compact session-local snapshot that the browser polls while
  the task is active; no imported observations are copied into that channel. A
  synchronous worker remains available when explicitly requested.
- Responsive checks at 320, 768, and 1440 px found no horizontal overflow;
  the core import controls remained visible and the browser console contained
  no errors.

Milestone 3 was accepted by the owner on 2026-07-21.

## Production presentation addendum — 2026-07-21

After a separate owner-reviewed showcase, the guided four-step import was
promoted to the production `LightLogWeb()` entry point. The production flow is
now Source, Details & IDs, Check & import, and Review. Steps become complete
only when their required inputs are ready, and Review remains unavailable until
an import finishes. The readiness summary, import controls, live phase report,
final status, quality review, and analysis-focus choice all use the same
accepted `importServer()` contract as the earlier presentation.

This is a presentation change, not a change to the scientific import contract:
LightLogR still creates or preserves participant IDs, timestamps and sensor
values are not imputed, and implicit gaps, off-grid observations, and explicit
missing values remain separate diagnostics. The older accordion presentation
is retained in the development showcase through
`import_app(presentation = "accordion")` for regression comparison. The
`GENEActiv_GGIR` choice remains visible for discoverability, but selecting it
now shows that this route is currently available only through LightLogR and
restores the complete device list without retaining an invalid selection.

Post-promotion verification ran every available test successfully. The two
clean-mirai-daemon checks were skipped because that daemon backend is not
available in this test runtime. `R CMD check` completed with 0 errors and
0 warnings; its only note was the environment-level message
`unable to verify current time`.
