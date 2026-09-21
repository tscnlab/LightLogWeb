m7_fixture_directory <- function() {
  testthat::test_path("fixtures", "milestone-7")
}

m7_fixture_manifest <- function() {
  jsonlite::read_json(
    file.path(m7_fixture_directory(), "manifest.json"),
    simplifyVector = FALSE
  )
}

m7_fixture_manifest_entry <- function(fixture_id) {
  entries <- m7_fixture_manifest()$fixtures
  ids <- vapply(entries, `[[`, character(1), "fixture_id")
  entry <- entries[[match(fixture_id, ids)]]
  if (is.null(entry)) stop("Unknown Milestone 7 fixture: ", fixture_id)
  entry
}

m7_fixture <- function(fixture_id) {
  entry <- m7_fixture_manifest_entry(fixture_id)
  fixture <- readRDS(file.path(m7_fixture_directory(), entry$file))
  fixture$metadata_snapshot <- validate_glc_source_metadata_snapshot(
    fixture$metadata_snapshot
  )
  fixture
}

m7_fixture_record <- function(
  fixture_id = "tum-medi-only",
  display_name = NULL
) {
  fixture <- m7_fixture(fixture_id)
  entry <- m7_fixture_manifest_entry(fixture_id)
  if (is.null(display_name)) display_name <- fixture$label
  new_dataset_record(
    raw_data = fixture$data,
    display_name = display_name,
    source_manifest = new_source_manifest(
      source_type = "derived_glc_fixture",
      original_filenames = entry$file,
      hashes = entry$file_sha256,
      source_timezone = lubridate::tz(fixture$data$Datetime),
      imported_at = as.POSIXct(
        fixture$generation$generated_at,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      details = list(
        derived_fixture = TRUE,
        fixture_id = fixture$fixture_id,
        source = fixture$source,
        selection = fixture$selection
      )
    ),
    factual_metadata = list(
      derived_fixture = fixture$label,
      source_dataset_ids = fixture$selection$dataset_ids
    ),
    analysis_settings = list(
      primary_variable = if ("MEDI" %in% names(fixture$data)) {
        "MEDI"
      } else {
        names(fixture$data)[[1L]]
      }
    ),
    metadata_bundle = fixture$metadata_snapshot$bundle,
    id = metadata_fixture_dataset_record_id(fixture$fixture_id)
  )
}
