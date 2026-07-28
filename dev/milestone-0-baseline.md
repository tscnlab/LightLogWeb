# Milestone 0 baseline and verification record

Status: complete; accepted by the owner

Recorded: 2026-07-16

Owner acceptance and postprocess: 2026-07-17

Branch: dev

Pre-refactor commit: f9772b0

## 1. Scope boundary

Milestone 0 established the governing plan, scientific analysis rules, legacy
salvage audit, and a fresh reproducible dependency baseline. It did not begin
the session-model, task-system, UI, import, preprocessing, or other functional
refactors.

The owner accepted this milestone on 2026-07-17. At postprocess time, a paused
parallel Milestone 1 task was already present in the shared worktree. This
record therefore keeps the clean pre-refactor evidence below and separately
records a combined-tree handoff check. The Milestone 1 implementation and test
files were preserved byte-for-byte during the postprocess; only shared package
dependency metadata was reconciled.

The only R-code changes were package-hygiene declarations needed for static
checking and the corresponding rlang import. The legacy app remains available
as a baseline, but its architecture and behavior are not approved by this
milestone.

## 2. Initial state

Before the refresh:

- R was 4.5.0.
- renv.lock recorded an older environment, including LightLogR 0.10.0,
  Shiny 1.11.1, and bslib 0.9.0.
- The system library had LightLogR 0.10.2 and Shiny 1.13.0, which did not match
  the lockfile.
- DESCRIPTION contained a placeholder description, declared R 4.1.0, omitted
  directly used scales and tidyselect, and listed unused svglite.
- leaflet and shinycssloaders were unavailable through the active baseline
  library, so package loading/checking failed before code tests.
- A source archive could be built.
- The initial R CMD check stopped with one dependency error for missing
  leaflet and shinycssloaders.
- No substantive automated tests existed.

These observations are preserved as the pre-refactor baseline; they are not
the compatibility target.

## 3. Dependency policy applied

The historical lockfile was discarded as an input baseline.

The replacement procedure:

1. Corrected DESCRIPTION and declared LightLogWeb’s direct runtime and
   development dependencies.
2. Set the minimum R version to 4.3.0 because current LightLogR requires it,
   while retaining R 4.5.0 as the initial tested runtime.
3. Resolved the complete strong dependency closure from the live CRAN index on
   2026-07-16.
4. Installed the current version of every package in that closure into the
   isolated LightLogWeb renv library.
5. Snapshotted runtime plus explicitly declared development dependencies.
6. Standardized all 167 lockfile records to the CRAN repository.
7. Regenerated manifest.json from the resulting lockfile.
8. Compared every locked version with the live CRAN index.
9. Restored the lockfile into a temporary clean library and loaded key runtime
   packages.

After the owner opened the GLC/glcdp gate and accepted the milestone, the
postprocess also:

10. Pinned glcdp 0.90.0 to reviewed commit
    `21e806112a0032261d65180c3cc3ed72af38ea8d` from `tscnlab/glc-dp-r`.
11. Resolved the async dependencies introduced by the paused Milestone 1 work
    to current CRAN: mirai 2.7.1 and nanonext 1.10.1.
12. Kept glcdp in Suggests while Milestone 1 provides only an optional adapter
    seam. The production GLC feature milestone must promote it to Imports when
    the package becomes an unconditional runtime dependency.
13. Regenerated manifest.json from the reconciled lockfile.

Global recursive Suggests traversal was deliberately not used: it would pull
unrelated optional dependencies of dependencies. LightLogWeb’s own development
tools are included explicitly, while transitive traversal uses Depends,
Imports, and LinkingTo.

## 4. Key resolved versions

| Component | Resolved version |
|---|---:|
| R runtime | 4.5.0 |
| LightLogR | 0.10.3 |
| glcdp | 0.90.0 at `21e806112a0032261d65180c3cc3ed72af38ea8d` |
| shiny | 1.14.0 |
| bslib | 0.11.0 |
| dplyr | 1.2.1 |
| DT | 0.34.0 |
| gt | 1.3.0 |
| leaflet | 2.2.3 |
| melidosData | 1.0.6 |
| mirai | 2.7.1 |
| nanonext | 1.10.1 |
| testthat | 3.3.2 |
| shinytest2 | 0.5.1 |
| pkgdown | 2.2.1 |
| roxygen2 | 8.0.0 |
| quarto R package | 1.5.1 |
| rsconnect | 1.10.1 |
| renv | 1.2.3 |

Final accepted lock: 170 records. Of these, 169 are current CRAN packages and
one is the immutable glcdp GitHub revision above.

Final live-index comparison:

    CURRENT_CRAN_LOCK_OK packages=169

Final renv status:

    No issues found -- the project is in a consistent state.

## 5. DESCRIPTION and tooling corrections

- Replaced the placeholder title and description.
- Added project and issue URLs.
- Raised Depends from R 4.1.0 to R 4.3.0.
- Added key compatibility floors for LightLogR 0.10.3, Shiny 1.14.0, and
  bslib 0.11.0.
- Added directly used scales and tidyselect.
- Removed unused direct svglite.
- Added current test, documentation, test-data, and deployment-development
  packages under Suggests.
- Declared the optional glcdp adapter dependency under Suggests with an exact
  GitHub Remotes reference.
- Added the paused Milestone 1 async runtime declarations without otherwise
  modifying its implementation.
- Configured testthat edition 3 and roxygen2 8.0.0.
- Changed renv snapshots from implicit to explicit and recorded the R 4.5.0
  target.
- Updated renv activation to 1.2.3.
- Regenerated package documentation and NAMESPACE.
- Regenerated manifest.json with current dependency descriptions.

## 6. Automated and live smoke verification

The new test suite contains a package-level app-construction smoke test.

Result:

    app-smoke: ...
    DONE

The combined post-acceptance test suite, including the paused Milestone 1
tests, also completed without failures. Its two warnings are expected test
signals: one package-build version warning from shiny and one deliberately
raised private worker error used to test failure containment.

The legacy launcher was also run as a real local Shiny server. The root request
returned:

    HTTP/1.1 200 OK
    Content-Length: 154670

The served HTML contained the LightLogWeb title and the Import and Dashboard
navigation entries. The temporary server was stopped immediately afterward.

The post-acceptance combined-tree launcher was also served with
`profile = "local"` and `workers = 0`. It returned `HTTP/1.1 200 OK` with a
content length of 86306 bytes and retained the same title and navigation
markers. The server stopped cleanly.

This proves only baseline construction and initial-page serving. It does not
approve legacy interaction behavior or replace milestone-specific browser
journeys.

## 7. Package build and archive isolation

Initial pre-refactor source archive:

    /private/tmp/LightLogWeb_0.0.0.9000.tar.gz

The archive contains the automated smoke test and excludes both:

- project-plan.md
- the complete dev/ directory

Verification result:

    ARCHIVE_ISOLATION_OK
    archive_files=45
    tests_present=TRUE

The post-acceptance combined tree was rebuilt from source at:

    /private/tmp/llw-m0-final.cLpKw5/LightLogWeb_0.0.0.9000.tar.gz

It retains the same isolation guarantees while including the paused Milestone
1 tests:

    ARCHIVE_ISOLATION_OK
    archive_files=58
    test_files=5

## 8. Package check

Command class: R CMD check of the built source archive with the PDF manual
disabled for the baseline environment.

Clean pre-refactor baseline result:

    Status: OK

- Errors: 0
- Warnings: 0
- Notes: 0
- Package installation: passed
- Namespace load/unload: passed
- Dependency-code checks: passed
- Examples: passed
- Automated tests: passed

The sandboxed run could not query remote CRAN/Bioconductor indexes during the
dependency-check phase, but locally declared dependencies were found and the
check status remained OK. A separate authorized live CRAN comparison verified
all 167 locked versions.

## 9. Clean restore

renv.lock was restored into a newly created temporary library. The restore
completed and the temporary library was removed. Key verified versions:

    packages=170
    LightLogR=0.10.3
    shiny=1.14.0
    bslib=0.11.0
    glcdp=0.90.0
    glcdp_sha=21e806112a0032261d65180c3cc3ed72af38ea8d
    mirai=2.7.1
    nanonext=1.10.1
    testthat=3.3.2
    shinytest2=0.5.1
    pkgdown=2.2.1
    melidosData=1.0.6
    roxygen2=8.0.0

Result:

    CLEAN_RESTORE_OK

## 10. Milestone 0 acceptance

The owner accepted Milestone 0 on 2026-07-17, including project-plan.md,
dev/analysis-principles.md, dev/legacy-audit.md, and the current-CRAN dependency
policy. The GLC/glcdp gate is open. Milestone 1 may resume after its existing
paused changes are reviewed against the combined-tree handoff diagnostics
recorded below.

## 11. Combined-tree Milestone 1 handoff

The final post-acceptance source-archive check installed the package, ran its
examples, and passed every test. Its status was `2 WARNINGs, 1 NOTE`, all in
the paused Milestone 1 change set rather than the Milestone 0 postprocess:

1. Non-ASCII code text in R/dataset-record-builders.R and
   R/m_core_architecture.R.
2. The LightLogWeb launcher now accepts `profile`, `max_upload_mb`, and
   `workers`, while man/LightLogWeb.Rd still documents the pre-Milestone 1
   `...` signature.
3. Static analysis reports `payload` as a possible global binding inside
   `new_long_task()` in R/long-task.R.

The earlier unused-import note for glcdp is resolved by declaring the optional
adapter under Suggests. Repository-index warnings in the sandbox were
independently covered by the successful live CRAN comparison. These remaining
diagnostics are the first cleanup items for the resumed Milestone 1 task; they
were intentionally not fixed across the paused task boundary.

SHA-256 comparison confirmed that all 19 protected Milestone 1 implementation,
showcase, helper, and test files were byte-identical before and after the
Milestone 0 postprocess. DESCRIPTION was excluded from that comparison because
it is shared package metadata and was intentionally reconciled.

## 12. 2026-07-28 glcdp and Schema 3.0.2 compatibility addendum

This addendum records a later current-target review. It does not rewrite the
accepted Milestone 0 environment, commands, package counts, or the verified
glcdp 0.90.0 pin above.

The review used R 4.5.0 and the public source repositories:

- glcdp 1.0.0 at commit
  `76b532a7167edb212059734234ff6ed6fe10f9e2`. The commit matched the GitHub
  origin's `HEAD` and `main` on 2026-07-28. A live CRAN index query on the same
  date confirmed that glcdp was not on CRAN.
- GLC metadata validator 0.5.2 at tag and commit
  `19360dce2b2d5967809cbc68e45774c1725d39b7`, with released multi-platform
  container digest
  `sha256:d66c9d705e2a59967c5699af55d872b2012d18fb3c9dc611a9837d1c05af6160`.
- The metadata builder at commit
  `a8a312c58b91ae2b725168a3b5debe63030a82ad`. Recursive comparison found no
  differences between its Schema 3.0.2 bundle and the validator's canonical
  bundle.

Loading the glcdp 1.0.0 source with pkgload 1.5.2 confirmed that all 14 public
functions used by the Milestone 1 adapter contract retain the same names and
formals as 0.90.0. Version 1.0.0 adds three exports:

- extract_metadata for relationship-aware metadata extraction;
- add_metadata for row-preserving metadata joins; and
- glc_explore for the package-owned interactive explorer.

glc_schema_versions now reports Schema 3.0.2 as the current default and primary
stable metadata-driven import contract. Schemas 3.0.0 and 3.0.1 are stable
compatible predecessors. Schemas 1.0.0 and 2.0.0 are barebones legacy paths,
not equivalents of the typed Schema 3 contract. The canonical 3.0.2 bundle is
a corrective patch over 3.0.1: when datasheet_channel is present, it must
contain at least one entry. The broader metadata and import contract is
unchanged.

A read-only live R check used the official registry generated at
`2026-07-28T13:13:47.465808+00:00`. The latest passing
`tscnlab/melidos-iztech-glc-dataset` revision was
`9353a0c4287d44cb400d30f45d7dbcf9910f9bde`: it was attestation-verified,
validated by validator 0.5.2 with zero errors and zero warnings, opened through
glcdp 1.0.0 as Schema 3.0.2, and exposed the documented dataset, file,
variable, resource, typed-import, and stable file-group inventories.

glcdp 1.0.0 still exposes no public schema-aware metadata writer or validator
API. LightLogWeb therefore retains immutable source metadata plus a separate
overlay and does not implement GLC package serialization.

This task updates design documentation only. DESCRIPTION, renv.lock,
manifest.json, the adapter, and its tests remain at the accepted 0.90.0
implementation baseline until Milestone 6 performs the exact-commit dependency
upgrade and parity checks.
