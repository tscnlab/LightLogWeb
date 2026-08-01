# R 4.6.1 dependency refresh

Status: complete on 2026-08-01

## Scope

The development runtime was upgraded from R 4.5.0 to R 4.6.1. The R 4.6
project library was empty except for renv, so the environment was rebuilt from
DESCRIPTION rather than restored from the older lockfile.

- All CRAN packages were resolved from the live source index at
  https://cloud.r-project.org.
- glcdp 1.0.0 was installed from the submitted source at exact commit
  `bf97cb30555569886f08db0bdb25421c546ccf0a`. It was not installed from the
  moving default branch.
- glcdp remains in Suggests for this dependency-only commit. Milestone 6 will
  promote it to Imports together with production code that calls its public
  API. This avoids an unused-Imports note in the intermediate package state.
- renv uses the R 4.6.1 project library and an `all` snapshot. This is
  intentional: the library was created from scratch with only LightLogWeb's
  declared runtime and development dependencies, and the lockfile must retain
  the tools in Suggests needed for tests, documentation, deployment, and
  showcases.
- manifest.json was regenerated with strict dependency resolution. It records
  R 4.6.1, 173 packages, and 33 deployment files.

## Direct package versions

| Field | Package | Version |
|---|---|---:|
| Imports | bsicons | 0.1.2 |
| Imports | bslib | 0.11.0 |
| Imports | digest | 0.6.39 |
| Imports | dplyr | 1.2.1 |
| Imports | DT | 0.34.0 |
| Imports | ggplot2 | 4.0.3 |
| Imports | ggtext | 0.1.2 |
| Imports | gt | 1.3.0 |
| Imports | htmltools | 0.5.9 |
| Imports | LightLogR | 0.10.3 |
| Imports | lubridate | 1.9.5 |
| Imports | mirai | 2.7.2 |
| Imports | promises | 1.5.0 |
| Imports | rlang | 1.3.0 |
| Imports | sass | 0.4.10 |
| Imports | shiny | 1.14.0 |
| Imports | shinycssloaders | 1.1.0 |
| Imports | stringr | 1.6.0 |
| Suggests | glcdp | 1.0.0 |
| Suggests | knitr | 1.51 |
| Suggests | later | 1.4.8 |
| Suggests | melidosData | 1.0.6 |
| Suggests | pkgdown | 2.2.1 |
| Suggests | pkgload | 1.5.3 |
| Suggests | quarto | 1.5.1 |
| Suggests | rmarkdown | 2.31 |
| Suggests | roxygen2 | 8.0.0 |
| Suggests | rsconnect | 1.10.1 |
| Suggests | shinytest2 | 0.5.1 |
| Suggests | testthat | 3.3.2 |
| Environment | renv | 1.2.3 |

The project library also overrides the older copies bundled with R 4.6.1 with
the current CRAN releases of MASS 7.3-66, Matrix 1.7-6, cluster 2.1.8.3,
nlme 3.1-170, and survival 3.8-9.

## Verification

The checks used R 4.6.1 on aarch64-apple-darwin23.

- `renv::status()` reported: `No issues found -- the project is in a
  consistent state.`
- A live, unfiltered CRAN comparison checked 172 repository packages in the
  lockfile and found zero version differences.
- All 30 direct Imports and Suggests were installed at the current CRAN version
  or, for glcdp, at the approved exact commit.
- `pkgload::load_all()` and the complete testthat suite passed. The suite
  retained 11 existing expected warnings from plot-removal diagnostics and
  deliberately failed background-task/import scenarios.
- The source archive built successfully. Neither project-plan.md nor the
  unrelated untracked `man/man.Rproj` file was present in the archive.
- `R CMD check --no-manual --no-build-vignettes` completed with no errors and
  no notes. It retained one pre-existing warning for non-ASCII characters in
  unchanged file `R/m_dataset_dashboard.R`.

## Rebuild outline

1. Query the current CRAN source index in R.
2. Install exact current versions of all packages declared in Imports and
   Suggests into the isolated project library.
3. Install glcdp from the exact submitted Git commit.
4. Install newer CRAN copies of required recommended packages when R's bundled
   copy is older.
5. Run `renv::snapshot(type = "all")` from an activated project session.
6. Confirm `renv::status()`, compare the lockfile with the live CRAN index, and
   regenerate manifest.json with strict dependency resolution.
7. Run the complete test suite, source build, archive-isolation check, and
   package check.
