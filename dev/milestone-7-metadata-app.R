# Run the isolated Milestone 7 metadata explorer showcase.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required for the metadata showcase.")
}

pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
fixture_directory <- file.path(
  "tests",
  "testthat",
  "fixtures",
  "milestone-7"
)
records <- metadata_explorer_fixture_records(fixture_directory)
metadata_explorer_app(records)
