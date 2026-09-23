# Offline isolated production-module showcase. No published package is opened.
pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
records <- metadata_explorer_fixture_records(
  "tests/testthat/fixtures/milestone-7"
)
metadata_overlay_app(records, with_explorer = TRUE)
