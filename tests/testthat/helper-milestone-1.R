m1_fixture_data <- function() {
  data.frame(
    Id = factor(c("P01", "P01", "P02")),
    Datetime = as.POSIXct(
      c(
        "2026-01-01 08:00:00",
        "2026-01-01 08:01:00",
        "2026-01-01 08:00:00"
      ),
      tz = "UTC"
    ),
    MEDI = c(10, 20, 30),
    check.names = FALSE
  )
}

m1_source_manifest <- function() {
  new_source_manifest(
    source_type = "test_fixture",
    original_filenames = "fixture.csv",
    hashes = paste0("sha256:", strrep("a", 64L)),
    source_timezone = "UTC"
  )
}

m1_record <- function(name = "Fixture") {
  new_dataset_record(
    raw_data = m1_fixture_data(),
    display_name = name,
    source_manifest = m1_source_manifest(),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

flush_m1_promises <- function(session, times = 20L) {
  for (index in seq_len(times)) {
    session$flushReact()
    later::run_now(timeoutSecs = 0.01)
  }
  invisible(NULL)
}
