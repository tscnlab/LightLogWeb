# Run the full app with offline datasets and Prepare selected.
source("dev/milestone-8-prepare-app.R", local = TRUE)

prepare_flow_aggregation_record <- function() {
  readings <- c(1:5, 1:4, NA, 1:3, NA, NA, rep(NA_real_, 5))
  raw <- data.frame(
    Id = "Aggregation_demo", file_group_id = "aggregation_demo",
    Datetime = as.POSIXct("2026-01-01 12:00:00", tz = "UTC") + 0:19 * 60,
    MEDI = readings,
    Category = factor(ifelse(is.na(readings), NA, ifelse(readings > 2, "active", "rest")),
      levels = c("rest", "active")),
    Worn = ifelse(is.na(readings), NA, readings > 2)
  )
  new_dataset_record(
    raw, "Aggregation: numeric, categories and logical with missing readings",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_flowaggregation"
  )
}

prepare_flow_app <- function(workers = 1L) {
  lightlogweb_app(
    profile = "local",
    workers = workers,
    initial_records = c(m8_showcase_records(), list(prepare_flow_aggregation_record())),
    initial_panel = "prepare",
    registry_loader = function(...)
      stop("Live GLC discovery is disabled in this offline showcase."),
    preparation_worker = m8_showcase_worker
  )
}
