if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required for this development acceptance run.")
}

pkgload::load_all(".", quiet = TRUE)

participant_ids <- sprintf("P%02d", seq_len(10L))
minutes <- 30L * 24L * 60L
instants <- seq(
  as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  by = "1 min",
  length.out = minutes
)
minute_of_day <- (seq_along(instants) - 1L) %% (24L * 60L)
base_signal <- pmax(
  0,
  300 * sin((minute_of_day - 360) / (24 * 60) * 2 * pi)
)
data <- data.frame(
  Id = rep(participant_ids, each = minutes),
  Datetime = rep(instants, times = length(participant_ids)),
  MEDI = unlist(lapply(seq_along(participant_ids), function(index) {
    base_signal + index
  })),
  stringsAsFactors = FALSE
)

quality_time <- system.time({
  quality <- summarize_raw_import_quality(data, "UTC")
})
record <- new_dataset_record(
  raw_data = data,
  display_name = "10 participants x 30 days x 1 minute",
  source_manifest = new_source_manifest(
    source_type = "milestone_5_scale_acceptance",
    source_timezone = "UTC",
    details = list(epoch_seconds = 60)
  ),
  analysis_settings = list(
    primary_variable = "MEDI",
    analysis_timezone = "UTC"
  ),
  provenance = list(
    LightLogR_version = installed_package_version("LightLogR"),
    raw_import_quality = quality,
    primary_variable_eligibility = quality$eligibility
  )
)

snapshot_time <- system.time({
  snapshot <- dashboard_dataset_snapshot(record)
})
selection <- dashboard_plot_selection(
  snapshot,
  show_all = TRUE,
  facet_page = 1L,
  time_basis = "participant",
  measurement_window = c(1L, 30L),
  time_page = 1L,
  days_per_page = 7L
)
preview <- dashboard_plot_preview(snapshot, selection)
focus_view <- dashboard_focus_view(snapshot, "MEDI", "preprocessed")
recommendation <- dashboard_view_recommendation(
  snapshot,
  focus_view = focus_view,
  participants = participant_ids
)
recommendation_state <- dashboard_recommendation_state(
  selection,
  recommendation
)
table_contract <- dashboard_table_contract()
table_options <- dashboard_table_options(snapshot$prepared_data)

stopifnot(
  nrow(data) == 432000L,
  length(snapshot$participants) == 10L,
  nrow(snapshot$coverage) == 300L,
  all(abs(snapshot$coverage[["Focus coverage (%)"]] - 100) < 1e-8),
  identical(focus_view$focus_variable, "MEDI"),
  identical(focus_view$data_stage, "preprocessed"),
  identical(recommendation$time_basis, "participant"),
  identical(recommendation$measurement_window, c(1L, 30L)),
  identical(recommendation$resolved_mode, "availability"),
  isTRUE(recommendation_state$active),
  identical(selection$mode, "availability"),
  selection$participant_pages == 3L,
  selection$time_pages == 5L,
  identical(selection$time_page_label, "Measurement days 1\u20137"),
  length(selection$page_participants) == 4L,
  !is.null(selection$show_all_warning),
  nrow(preview$data) <= dashboard_plot_limits()$max_overview_cells,
  identical(snapshot$recipe$state, "empty_unchanged"),
  identical(snapshot$grouping$state, "empty"),
  isTRUE(table_contract$server),
  isTRUE(table_contract$search),
  isTRUE(table_contract$sort),
  isTRUE(table_contract$column_visibility),
  identical(table_contract$page_length, 10L),
  isTRUE(table_options$processing),
  isTRUE(table_options$deferRender),
  identical(table_options$buttons[[1L]]$extend, "colvis")
)

small_selection <- dashboard_plot_selection(
  snapshot,
  participants = c("P01", "P02"),
  time_basis = "participant",
  measurement_window = c(1L, 2L)
)
small_preview <- dashboard_plot_preview(snapshot, small_selection)
small_plot <- plot_dashboard_timeline(
  snapshot,
  small_preview,
  scale = "symlog"
)
stopifnot(
  identical(small_selection$mode, "detailed"),
  nrow(small_preview$data) == 2L * 2L * 24L * 60L,
  !small_preview$reduced,
  identical(attr(small_plot, "llw_dashboard_scale"), "symlog"),
  identical(
    dashboard_default_date_window(snapshot),
    as.Date(c("2026-01-01", "2026-01-07"))
  )
)

format_size <- function(value) {
  format(utils::object.size(value), units = "auto", standard = "IEC")
}

cat("Milestone 5 scale acceptance passed.\n")
cat("Rows:", format(nrow(data), big.mark = ","), "\n")
cat("Source data size:", format_size(data), "\n")
cat("Bounded overview size:", format_size(preview$data), "\n")
cat("Quality audit elapsed:", quality_time[["elapsed"]], "seconds\n")
cat("Dashboard snapshot elapsed:", snapshot_time[["elapsed"]], "seconds\n")
cat("Overview cells sent to the plot:", nrow(preview$data), "\n")
cat("Participant pages:", selection$participant_pages, "\n")
cat("Time pages:", selection$time_pages, "\n")
cat("Recommended start:", recommendation$summary, "\n")
cat("Server-side table page length:", table_contract$page_length, "\n")
