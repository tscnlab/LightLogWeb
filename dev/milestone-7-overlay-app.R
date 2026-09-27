# Offline full-app integration showcase. No published package is opened.
pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
records <- metadata_explorer_fixture_records(
  "tests/testthat/fixtures/milestone-7"
)
lightlogweb_app(
  profile = "local",
  workers = 0L,
  initial_records = records,
  registry_loader = function(registry_url, refresh = FALSE) {
    abort_llw(
      "Registry access is disabled in this offline integration showcase.",
      type = "unavailable_feature"
    )
  }
)
