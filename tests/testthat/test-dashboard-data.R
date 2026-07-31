test_that("dashboard snapshot keeps source and pre-processed states explicit", {
  record <- dashboard_showcase_record()
  snapshot <- dashboard_dataset_snapshot(record)

  expect_s3_class(snapshot, "llw_dashboard_snapshot")
  expect_identical(snapshot$raw_data, snapshot$prepared_data)
  expect_identical(snapshot$recipe$state, "empty_unchanged")
  expect_true(snapshot$recipe$unchanged)
  expect_identical(snapshot$recipe$step_count, 0L)
  expect_identical(snapshot$grouping$state, "empty")
  expect_identical(snapshot$primary_unit, "Unit not specified")
  expect_true("MEDI" %in% snapshot$focus_variables)
  expect_identical(snapshot$display_timezone, "UTC")
  expect_identical(nrow(snapshot$coverage), 4L)
  expect_equal(snapshot$coverage[["Focus coverage (%)"]], rep(100, 4L))

  state <- dashboard_state_table(snapshot)
  expect_match(
    state$Value[state$Field == "Pre-processed-data state"],
    "Empty recipe"
  )
  expect_identical(
    state$Value[state$Field == "Grouping state"],
    "No active grouping"
  )
})

test_that("focus metric and data stage drive dashboard semantics", {
  record <- dashboard_showcase_record(
    participants = 1L,
    days = 10L,
    epoch_seconds = 3600
  )
  source <- dataset_raw_data(record)
  source$Photopic <- source$MEDI * 2
  source$Photopic[[4L]] <- NA_real_
  record <- new_dataset_record(
    raw_data = source,
    display_name = "Two-metric fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  snapshot <- dashboard_dataset_snapshot(record)
  view <- dashboard_focus_view(snapshot, "Photopic", "source")

  expect_identical(view$focus_variable, "Photopic")
  expect_identical(view$data_stage, "source")
  expect_identical(view$missing_count, 1L)
  expect_equal(view$missing_percent, 100 / nrow(source))
  expect_identical(
    sum(view$coverage[["Explicit missing focus epochs"]]),
    1L
  )
  expect_identical(
    dashboard_default_date_window(snapshot),
    as.Date(c("2026-01-01", "2026-01-07"))
  )

  selection <- dashboard_plot_selection(
    snapshot,
    date_window = dashboard_default_date_window(snapshot)
  )
  expect_identical(selection$mode, "detailed")
  expect_identical(selection$day_count, 7L)
})

test_that("daily coverage separates gaps, irregular timestamps, and duplicates", {
  full_day <- seq(
    as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    by = "1 min",
    length.out = 1440L
  )
  datetimes <- c(
    full_day[-61L],
    full_day[[121L]],
    as.POSIXct("2026-01-01 00:00:30", tz = "UTC")
  )
  data <- data.frame(
    Id = "P01",
    Datetime = datetimes,
    MEDI = c(rep(10, length(datetimes) - 1L), 0),
    stringsAsFactors = FALSE
  )
  data$MEDI[data$Datetime == full_day[[11L]]] <- NA_real_
  quality <- summarize_raw_import_quality(data, "UTC")
  coverage <- dashboard_daily_coverage(data, quality, "UTC", "MEDI")

  expect_s3_class(coverage, "llw_dashboard_coverage")
  expect_identical(nrow(coverage), 1L)
  expect_identical(coverage[["Expected epochs"]], 1441L)
  expect_identical(coverage[["Observed timestamp epochs"]], 1440L)
  expect_identical(coverage[["Observed focus epochs"]], 1439L)
  expect_identical(coverage[["Explicit missing focus epochs"]], 1L)
  expect_identical(coverage[["Implicit gap epochs"]], 1L)
  expect_identical(coverage[["Irregular timestamps"]], 2L)
  expect_identical(coverage[["Duplicate rows"]], 1L)
  expect_equal(coverage[["Focus coverage (%)"]], 100 * 1439 / 1441)
  expect_identical(data$MEDI[[length(datetimes)]], 0)
})

test_that("daily coverage preserves 23- and 25-hour local DST days", {
  timezone <- "Europe/Berlin"
  coverage_for_day <- function(day) {
    date <- as.Date(day)
    start <- as.POSIXct(
      paste(day, "00:00:00"),
      tz = timezone,
      format = "%Y-%m-%d %H:%M:%S"
    )
    end <- as.POSIXct(
      paste(format(date + 1L, "%Y-%m-%d"), "00:00:00"),
      tz = timezone,
      format = "%Y-%m-%d %H:%M:%S"
    )
    instants <- seq(start, end - 60, by = "1 min")
    data <- data.frame(
      Id = "P01",
      Datetime = instants,
      MEDI = rep(10, length(instants)),
      stringsAsFactors = FALSE
    )
    quality <- summarize_raw_import_quality(data, timezone)
    dashboard_daily_coverage(data, quality, timezone, "MEDI")
  }

  spring <- coverage_for_day("2026-03-29")
  autumn <- coverage_for_day("2026-10-25")

  expect_identical(spring[["Day length (h)"]], 23)
  expect_identical(spring[["Expected epochs"]], 1380L)
  expect_identical(spring[["Focus coverage (%)"]], 100)
  expect_identical(autumn[["Day length (h)"]], 25)
  expect_identical(autumn[["Expected epochs"]], 1500L)
  expect_identical(autumn[["Focus coverage (%)"]], 100)
})

test_that("dashboard chooses detailed and availability views from scope", {
  record <- dashboard_showcase_record(
    participants = 10L,
    days = 30L,
    epoch_seconds = 3600,
    display_name = "Scale fixture"
  )
  snapshot <- dashboard_dataset_snapshot(record)

  all_selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    facet_page = 2L
  )
  expect_identical(all_selection$mode, "availability")
  expect_identical(all_selection$facet_pages, 3L)
  expect_identical(all_selection$facet_page, 2L)
  expect_length(all_selection$page_participants, 4L)
  expect_match(
    all_selection$show_all_warning,
    "Only one bounded row and time page"
  )

  overview <- dashboard_plot_preview(snapshot, all_selection)
  expect_identical(overview$mode, "availability")
  expect_lte(nrow(overview$data), 4L * 30L)
  expect_match(overview$notice, "full participant-local calendar day")
  expect_match(overview$notice, "non-missing focus measurements")

  empty_coverage <- snapshot$coverage[0, , drop = FALSE]
  expect_no_error({
    reduced_empty <- dashboard_reduce_coverage(
      empty_coverage,
      max_cells = 100L
    )
  })
  expect_identical(nrow(reduced_empty$data), 0L)
  empty_overview <- overview
  empty_overview$data <- reduced_empty$data
  expect_no_error({
    empty_plot <- plot_dashboard_availability(
      snapshot,
      empty_overview,
      mode = "light"
    )
  })
  expect_identical(
    attr(empty_plot, "llw_dashboard_plot"),
    "daily_availability"
  )

  detailed_selection <- dashboard_plot_selection(
    snapshot,
    participants = c("P01", "P02"),
    date_window = as.Date(c("2026-01-01", "2026-01-02"))
  )
  expect_identical(detailed_selection$mode, "detailed")
  detailed <- dashboard_plot_preview(snapshot, detailed_selection)
  expect_identical(detailed$mode, "detailed")
  expect_setequal(unique(detailed$data$Id), c("P01", "P02"))
  expect_false(detailed$reduced)
})

test_that("participant recordings and calendar scope remain distinct", {
  participant_data <- function(id, first_date) {
    instants <- seq(
      as.POSIXct(paste(first_date, "00:00:00"), tz = "UTC"),
      by = "1 hour",
      length.out = 72L
    )
    data.frame(
      Id = id,
      Datetime = instants,
      MEDI = seq_along(instants),
      stringsAsFactors = FALSE
    )
  }
  data <- rbind(
    participant_data("P01", "2026-01-05"),
    participant_data("P02", "2026-06-15")
  )
  record <- new_dataset_record(
    raw_data = data,
    display_name = "Staggered recordings",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  snapshot <- dashboard_dataset_snapshot(record)

  expect_identical(
    dashboard_participant_date_window(snapshot, "P02"),
    as.Date(c("2026-06-15", "2026-06-17"))
  )
  expect_identical(
    dashboard_participant_date_window(snapshot, c("P01", "P02")),
    as.Date(c("2026-01-05", "2026-06-17"))
  )

  calendar <- dashboard_plot_selection(
    snapshot,
    participants = c("P01", "P02"),
    date_window = as.Date(c("2026-01-05", "2026-06-17")),
    time_basis = "calendar",
    view_mode = "auto"
  )
  expect_identical(calendar$available_span_days, 3L)
  expect_identical(calendar$mode, "detailed")
  expect_gt(calendar$time_pages, 20L)

  participant <- dashboard_plot_selection(
    snapshot,
    participants = c("P01", "P02"),
    time_basis = "participant",
    measurement_window = c(1L, 3L),
    view_mode = "auto"
  )
  expect_identical(participant$time_pages, 1L)
  expect_identical(participant$time_page_label, "Measurement days 1\u20133")
  expect_identical(
    participant$participant_windows$`Window start`,
    as.Date(c("2026-01-05", "2026-06-15"))
  )

  participant_preview <- dashboard_plot_preview(snapshot, participant)
  expect_setequal(unique(participant_preview$data$Id), c("P01", "P02"))
  participant_starts <- tapply(
    as.numeric(participant_preview$data$`Plot time`),
    participant_preview$data$Id,
    min
  )
  expect_equal(
    as.POSIXct(
      as.vector(participant_starts),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    as.POSIXct(
      c("2026-01-05 00:00:00", "2026-06-15 00:00:00"),
      tz = "UTC"
    )
  )

  elapsed <- dashboard_plot_selection(
    snapshot,
    participants = c("P01", "P02"),
    time_basis = "elapsed",
    measurement_window = c(1L, 3L)
  )
  elapsed_preview <- dashboard_plot_preview(snapshot, elapsed)
  elapsed_starts <- tapply(
    elapsed_preview$data$`Plot time`,
    elapsed_preview$data$Id,
    min
  )
  expect_identical(length(unique(as.numeric(elapsed_starts))), 1L)
  expect_identical(
    format(
      as.POSIXct(elapsed_starts[[1L]], origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d",
      tz = "UTC"
    ),
    "2000-01-01"
  )
})

test_that("view mode and two-dimensional pages are independently controlled", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 10L,
    days = 30L,
    epoch_seconds = 3600
  ))

  automatic <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    facet_page = 2L,
    time_basis = "participant",
    measurement_window = c(1L, 30L),
    time_page = 3L,
    days_per_page = 7L
  )
  expect_identical(automatic$mode, "availability")
  expect_identical(automatic$participant_pages, 3L)
  expect_identical(automatic$participant_page, 2L)
  expect_identical(automatic$time_pages, 5L)
  expect_identical(automatic$time_page, 3L)
  expect_identical(automatic$page_participants, sprintf("P%02d", 5:8))
  expect_identical(automatic$time_page_label, "Measurement days 15\u201321")
  expect_identical(
    automatic$participant_windows$`Window start`,
    rep(as.Date("2026-01-15"), 4L)
  )

  forced_detailed <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    time_basis = "participant",
    measurement_window = c(1L, 30L),
    view_mode = "detailed"
  )
  expect_identical(forced_detailed$mode, "detailed")

  forced_coverage <- dashboard_plot_selection(
    snapshot,
    participants = "P01",
    time_basis = "participant",
    measurement_window = c(1L, 2L),
    view_mode = "availability"
  )
  expect_identical(forced_coverage$mode, "availability")
})

test_that("dashboard recommendations are neutral, explicit, and adjustable", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 10L,
    days = 30L,
    epoch_seconds = 3600
  ))
  focus_view <- dashboard_focus_view(snapshot)
  recommendation <- dashboard_view_recommendation(
    snapshot,
    focus_view = focus_view,
    participants = sprintf("P%02d", 1:8)
  )

  expect_s3_class(recommendation, "llw_dashboard_recommendation")
  expect_length(recommendation$participants, 8L)
  expect_identical(recommendation$time_basis, "participant")
  expect_identical(recommendation$measurement_window, c(1L, 30L))
  expect_identical(recommendation$view_mode, "auto")
  expect_identical(recommendation$resolved_mode, "availability")
  expect_identical(recommendation$days_per_page, 7L)
  expect_identical(recommendation$participants_per_page, 4L)
  expect_identical(recommendation$plot_scale, "symlog")
  expect_identical(recommendation$symlog_threshold, 1)
  expect_match(recommendation$summary, "Own recording dates", fixed = TRUE)
  expect_match(recommendation$summary, "4 participants/page", fixed = TRUE)
  expect_match(recommendation$summary, "Automatic \u2192 Coverage overview")
  expect_match(recommendation$summary, "Symlog", fixed = TRUE)
  expect_match(
    paste(recommendation$reasons, collapse = " "),
    "paged instead of truncated",
    fixed = TRUE
  )

  recommended_selection <- dashboard_plot_selection(
    snapshot,
    participants = recommendation$participants,
    time_basis = recommendation$time_basis,
    measurement_window = recommendation$measurement_window,
    view_mode = recommendation$view_mode,
    days_per_page = recommendation$days_per_page,
    participants_per_page = recommendation$participants_per_page,
    time_page = 3L,
    focus_view = focus_view
  )
  active <- dashboard_recommendation_state(
    recommended_selection,
    recommendation,
    plot_scale = recommendation$plot_scale,
    symlog_threshold = recommendation$symlog_threshold
  )
  expect_true(active$active)
  expect_length(active$differences, 0L)

  narrowed <- dashboard_plot_selection(
    snapshot,
    participants = recommendation$participants,
    time_basis = "participant",
    measurement_window = c(1L, 7L),
    view_mode = "detailed",
    days_per_page = 3L,
    focus_view = focus_view
  )
  adjusted <- dashboard_recommendation_state(
    narrowed,
    recommendation,
    plot_scale = "linear"
  )
  expect_false(adjusted$active)
  expect_setequal(
    adjusted$differences,
    c(
      "measurement-duration window",
      "view choice",
      "days per time page"
    )
  )

  short_snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record())
  short_recommendation <- dashboard_view_recommendation(short_snapshot)
  short_selection <- dashboard_plot_selection(
    short_snapshot,
    time_basis = short_recommendation$time_basis,
    measurement_window = short_recommendation$measurement_window,
    view_mode = short_recommendation$view_mode,
    days_per_page = short_recommendation$days_per_page
  )
  scale_adjusted <- dashboard_recommendation_state(
    short_selection,
    short_recommendation,
    plot_scale = "linear"
  )
  expect_identical(scale_adjusted$differences, "display scale")

  wider_selection <- dashboard_plot_selection(
    snapshot,
    participants = recommendation$participants,
    time_basis = recommendation$time_basis,
    measurement_window = recommendation$measurement_window,
    view_mode = recommendation$view_mode,
    days_per_page = recommendation$days_per_page,
    participants_per_page = 8L,
    focus_view = focus_view
  )
  page_size_adjusted <- dashboard_recommendation_state(
    wider_selection,
    recommendation,
    plot_scale = recommendation$plot_scale,
    symlog_threshold = recommendation$symlog_threshold
  )
  expect_identical(
    page_size_adjusted$differences,
    "participants per page"
  )
})

test_that("recommendation normalization is deterministic and safe", {
  defaults <- dashboard_view_defaults()
  expect_s3_class(defaults, "llw_dashboard_view_defaults")
  expect_identical(
    unclass(defaults)[c(
      "time_basis",
      "view_mode",
      "days_per_page",
      "participants_per_page"
    )],
    list(
      time_basis = "participant",
      view_mode = "auto",
      days_per_page = 7L,
      participants_per_page = 4L
    )
  )
  custom_defaults <- dashboard_view_defaults(
    dashboard_plot_limits(facets_per_page = 6L)
  )
  expect_identical(custom_defaults$participants_per_page, 6L)
  expect_identical(dashboard_plot_scale_value("log"), "log")
  expect_identical(dashboard_plot_scale_value("not-a-scale"), "symlog")
  expect_identical(
    dashboard_plot_scale_value("not-a-scale", fallback = "linear"),
    "linear"
  )
  expect_identical(dashboard_symlog_threshold_value(0.08), 0.1)
  expect_identical(dashboard_symlog_threshold_value(0), 1)
  expect_identical(dashboard_symlog_threshold_value(100), 10)

  record <- dashboard_showcase_record(
    participants = 1L,
    days = 2L,
    epoch_seconds = 3600
  )
  data <- dataset_raw_data(record)
  data$MEDI <- NA_real_
  missing_record <- new_dataset_record(
    raw_data = data,
    display_name = "Missing focus fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  missing_snapshot <- dashboard_dataset_snapshot(missing_record)
  missing_recommendation <- dashboard_view_recommendation(missing_snapshot)
  expect_identical(missing_recommendation$available_span_days, 0L)
  expect_identical(missing_recommendation$resolved_mode, "availability")
  expect_match(
    paste(missing_recommendation$reasons, collapse = " "),
    "explicit empty state",
    fixed = TRUE
  )

  data$MEDI <- Inf
  infinite_record <- new_dataset_record(
    raw_data = data,
    display_name = "Infinite focus fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  infinite_snapshot <- dashboard_dataset_snapshot(infinite_record)
  infinite_recommendation <- dashboard_view_recommendation(infinite_snapshot)
  expect_identical(infinite_recommendation$available_span_days, 0L)
  expect_identical(infinite_recommendation$resolved_mode, "availability")
})

test_that("time pages retain width without exceeding the justified span", {
  measurement_chunks <- dashboard_measurement_chunks(
    start_day = 1L,
    end_day = 9L,
    days_per_page = 7L
  )
  expect_identical(measurement_chunks$Start, c(1L, 8L))
  expect_identical(measurement_chunks$End, c(7L, 14L))
  expect_identical(
    measurement_chunks$Label,
    c(
      "Measurement days 1\u20137",
      "Measurement days 8\u201314"
    )
  )
  capped_measurement_chunks <- dashboard_measurement_chunks(
    start_day = 1L,
    end_day = 9L,
    days_per_page = 14L
  )
  expect_identical(capped_measurement_chunks$Start, 1L)
  expect_identical(capped_measurement_chunks$End, 9L)
  expect_identical(
    capped_measurement_chunks$Label,
    "Measurement days 1\u20139"
  )

  calendar_chunks <- dashboard_date_chunks(
    as.Date("2026-01-01"),
    as.Date("2026-01-09"),
    time_basis = "calendar",
    days_per_page = 7L
  )
  expect_identical(
    calendar_chunks$End,
    as.Date(c("2026-01-07", "2026-01-14"))
  )
  capped_calendar_chunks <- dashboard_date_chunks(
    as.Date("2026-01-01"),
    as.Date("2026-01-09"),
    time_basis = "calendar",
    days_per_page = 14L
  )
  expect_identical(
    capped_calendar_chunks$End,
    as.Date("2026-01-09")
  )

  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 1L,
    days = 9L,
    epoch_seconds = 3600
  ))
  selection <- dashboard_plot_selection(
    snapshot,
    participants = "P01",
    time_basis = "elapsed",
    measurement_window = c(1L, 9L),
    view_mode = "availability",
    time_page = 2L,
    days_per_page = 7L
  )
  expect_identical(selection$time_page_label, "Measurement days 8\u201314")
  expect_identical(selection$day_count, 7L)

  preview <- dashboard_plot_preview(snapshot, selection)
  plot <- plot_dashboard_availability(snapshot, preview)
  x_limits <- plot$scales$get_scales("x")$limits
  expect_identical(
    as.Date(x_limits, origin = "1970-01-01"),
    as.Date(c("2000-01-08", "2000-01-14"))
  )

  capped_selection <- dashboard_plot_selection(
    snapshot,
    participants = "P01",
    time_basis = "elapsed",
    measurement_window = c(1L, 9L),
    view_mode = "availability",
    days_per_page = 14L
  )
  expect_identical(capped_selection$time_pages, 1L)
  expect_identical(
    capped_selection$time_page_label,
    "Measurement days 1\u20139"
  )
  expect_identical(capped_selection$day_count, 9L)
})

test_that("timeline y domains stay shared across pages unless requested", {
  data <- dataset_raw_data(dashboard_showcase_record(
    participants = 1L,
    days = 9L,
    epoch_seconds = 3600
  ))
  local_date <- as.Date(data$Datetime, tz = "UTC")
  measurement_day <- as.integer(local_date - min(local_date)) + 1L
  data$MEDI <- ifelse(
    measurement_day <= 7L,
    measurement_day,
    measurement_day * 1000
  )
  record <- new_dataset_record(
    raw_data = data,
    display_name = "Shared timeline domain fixture",
    source_manifest = new_source_manifest(
      source_type = "dashboard_test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  snapshot <- dashboard_dataset_snapshot(record)
  preview_for_page <- function(page) {
    selection <- dashboard_plot_selection(
      snapshot,
      participants = "P01",
      time_basis = "elapsed",
      measurement_window = c(1L, 9L),
      view_mode = "detailed",
      time_page = page,
      days_per_page = 7L
    )
    dashboard_plot_preview(snapshot, selection)
  }
  previews <- lapply(1:2, preview_for_page)

  expect_identical(previews[[1L]]$focus_domain, c(1, 9000))
  expect_identical(
    previews[[1L]]$focus_domain,
    previews[[2L]]$focus_domain
  )
  for (scale in c("linear", "log", "symlog")) {
    shared_plots <- lapply(previews, function(preview) {
      plot_dashboard_timeline(
        snapshot,
        preview,
        scale = scale,
        y_scope = "shared"
      )
    })
    expect_identical(
      shared_plots[[1L]]$scales$get_scales("y")$limits,
      shared_plots[[2L]]$scales$get_scales("y")$limits,
      info = scale
    )
    expect_identical(attr(shared_plots[[1L]], "llw_y_scope"), "shared")
  }

  page_plots <- lapply(previews, function(preview) {
    plot_dashboard_timeline(
      snapshot,
      preview,
      scale = "linear",
      y_scope = "page"
    )
  })
  page_ranges <- lapply(page_plots, function(plot) {
    ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]$y.range
  })
  expect_false(isTRUE(all.equal(page_ranges[[1L]], page_ranges[[2L]])))
  expect_identical(attr(page_plots[[1L]], "llw_y_scope"), "page")
})

test_that("calendar-cycle pages align to weeks, months, and years", {
  week <- dashboard_date_chunks(
    as.Date("2026-01-07"),
    as.Date("2026-01-20"),
    time_basis = "week",
    days_per_page = 7L
  )
  expect_identical(
    week$Start,
    as.Date(c("2026-01-07", "2026-01-12", "2026-01-19"))
  )
  expect_identical(
    week$End,
    as.Date(c("2026-01-11", "2026-01-18", "2026-01-20"))
  )
  expect_true(all(grepl("^Week ", week$Label)))

  month <- dashboard_date_chunks(
    as.Date("2026-01-29"),
    as.Date("2026-02-03"),
    time_basis = "month",
    days_per_page = 31L
  )
  expect_identical(
    month$Start,
    as.Date(c("2026-01-29", "2026-02-01"))
  )
  expect_identical(
    month$End,
    as.Date(c("2026-01-31", "2026-02-03"))
  )
  expect_match(month$Label[[1L]], "January 2026")
  expect_match(month$Label[[2L]], "February 2026")

  year <- dashboard_date_chunks(
    as.Date("2025-12-30"),
    as.Date("2026-01-02"),
    time_basis = "year",
    days_per_page = 365L
  )
  expect_identical(
    year$Start,
    as.Date(c("2025-12-30", "2026-01-01"))
  )
  expect_identical(
    year$End,
    as.Date(c("2025-12-31", "2026-01-02"))
  )
})

test_that("dashboard previews enforce row and availability-cell budgets", {
  detailed_record <- dashboard_showcase_record(
    participants = 2L,
    days = 2L,
    epoch_seconds = 60
  )
  detailed_snapshot <- dashboard_dataset_snapshot(detailed_record)
  detailed_limits <- dashboard_plot_limits(max_plot_rows = 100L)
  detailed_selection <- dashboard_plot_selection(
    detailed_snapshot,
    participants = c("P01", "P02"),
    limits = detailed_limits
  )
  detailed <- dashboard_plot_preview(
    detailed_snapshot,
    detailed_selection,
    limits = detailed_limits
  )
  expect_true(detailed$reduced)
  expect_lte(nrow(detailed$data), 100L)
  expect_setequal(unique(detailed$data$Id), c("P01", "P02"))
  expect_match(detailed$notice, "Evenly spaced display sample")

  overview_record <- dashboard_showcase_record(
    participants = 10L,
    days = 30L,
    epoch_seconds = 3600
  )
  overview_snapshot <- dashboard_dataset_snapshot(overview_record)
  overview_limits <- dashboard_plot_limits(max_overview_cells = 20L)
  overview_selection <- dashboard_plot_selection(
    overview_snapshot,
    show_all = TRUE,
    limits = overview_limits
  )
  overview <- dashboard_plot_preview(
    overview_snapshot,
    overview_selection,
    limits = overview_limits
  )
  expect_true(overview$reduced)
  expect_lte(nrow(overview$data), 20L)
  expect_match(overview$notice, "aggregated into bins")
})

test_that("dashboard plot helpers retain explicit display provenance", {
  detailed_snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record())
  detailed_selection <- dashboard_plot_selection(detailed_snapshot)
  detailed_preview <- dashboard_plot_preview(
    detailed_snapshot,
    detailed_selection
  )
  detailed_plot <- plot_dashboard_timeline(
    detailed_snapshot,
    detailed_preview,
    mode = "light",
    scale = "symlog"
  )
  expect_s3_class(detailed_plot, "ggplot")
  expect_identical(
    attr(detailed_plot, "llw_dashboard_plot"),
    "detailed_timeline"
  )
  expect_identical(detailed_plot$labels$y, "MEDI \u00b7 symlog")
  expect_identical(attr(detailed_plot, "llw_dashboard_scale"), "symlog")
  expect_identical(attr(detailed_plot, "llw_symlog_threshold"), 1)
  expect_s3_class(
    detailed_plot$theme$panel.grid.minor.y,
    "element_blank"
  )
  expect_s3_class(
    detailed_plot$theme$axis.minor.ticks.y.left,
    "element_line"
  )
  expect_equal(
    detailed_plot$theme$axis.minor.ticks.y.left$linewidth,
    0.25
  )
  expect_equal(
    detailed_plot$theme$axis.minor.ticks.length.y.left,
    grid::unit(0.07, "cm")
  )
  expect_true(
    detailed_plot$scales$get_scales("y")$guide$params$minor.ticks
  )
  expect_s3_class(
    detailed_plot$theme$panel.grid.minor.x,
    "element_blank"
  )
  participant_selection <- dashboard_plot_selection(
    detailed_snapshot,
    time_basis = "participant",
    measurement_window = c(1L, 2L)
  )
  participant_preview <- dashboard_plot_preview(
    detailed_snapshot,
    participant_selection
  )
  participant_plot <- plot_dashboard_timeline(
    detailed_snapshot,
    participant_preview,
    mode = "light",
    scale = "linear"
  )
  expect_true(participant_plot$facet$params$free$x)
  expect_identical(
    participant_plot$labels$x,
    paste0(
      "Local Date/Time (",
      detailed_snapshot$display_timezone,
      ")"
    )
  )
  expect_gte(
    as.numeric(participant_plot$theme$plot.margin)[[2L]],
    30
  )
  expect_identical(
    participant_plot$theme$strip.text.y.left$face,
    "bold"
  )

  elapsed_selection <- dashboard_plot_selection(
    detailed_snapshot,
    time_basis = "elapsed",
    measurement_window = c(1L, 2L)
  )
  elapsed_preview <- dashboard_plot_preview(
    detailed_snapshot,
    elapsed_selection
  )
  elapsed_plot <- plot_dashboard_timeline(
    detailed_snapshot,
    elapsed_preview,
    mode = "light",
    scale = "linear"
  )
  expect_false(elapsed_plot$facet$params$free$x)
  expect_identical(
    elapsed_plot$labels$x,
    "Days from first measurement"
  )
  expect_match(
    dashboard_scale_notice(detailed_preview, "log"),
    "non-positive point"
  )

  overview_snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 10L,
    days = 30L,
    epoch_seconds = 3600
  ))
  overview_selection <- dashboard_plot_selection(
    overview_snapshot,
    show_all = TRUE
  )
  overview_preview <- dashboard_plot_preview(
    overview_snapshot,
    overview_selection
  )
  overview_plot <- plot_dashboard_availability(
    overview_snapshot,
    overview_preview,
    mode = "dark"
  )
  expect_s3_class(overview_plot, "ggplot")
  expect_identical(
    attr(overview_plot, "llw_dashboard_plot"),
    "daily_availability"
  )
  expect_identical(overview_plot$theme$axis.text.y$face, "bold")
})

test_that("sparse participant pages retain plot-only facet slots", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 5L,
    days = 2L,
    epoch_seconds = 3600
  ))
  detailed_selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    facet_page = 2L,
    participants_per_page = 4L,
    view_mode = "detailed"
  )
  expect_identical(detailed_selection$page_participants, "P05")
  expect_identical(nrow(detailed_selection$facet_slots), 4L)
  expect_identical(
    detailed_selection$facet_slots$Label,
    c("P05", "", "", "")
  )

  detailed_preview <- dashboard_plot_preview(
    snapshot,
    detailed_selection
  )
  detailed_plot <- plot_dashboard_timeline(
    snapshot,
    detailed_preview,
    scale = "linear"
  )
  expect_identical(
    levels(detailed_plot$data$Id),
    detailed_selection$facet_slots$Id
  )
  expect_false(detailed_plot$facet$params$drop)
  expect_identical(
    attr(detailed_plot, "llw_facet_slots"),
    detailed_selection$facet_slots
  )
  expect_no_error(ggplot2::ggplot_build(detailed_plot))

  coverage_selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    facet_page = 2L,
    participants_per_page = 4L,
    view_mode = "availability"
  )
  coverage_preview <- dashboard_plot_preview(
    snapshot,
    coverage_selection
  )
  coverage_plot <- plot_dashboard_availability(
    snapshot,
    coverage_preview
  )
  expect_setequal(
    levels(coverage_plot$data$Id),
    coverage_selection$facet_slots$Id
  )
  expect_false(coverage_plot$scales$get_scales("y")$drop)
  expect_identical(
    attr(coverage_plot, "llw_facet_slots"),
    coverage_selection$facet_slots
  )
  expect_no_error(ggplot2::ggplot_build(coverage_plot))
})

test_that("participant timelines keep empty facets without losing POSIXct scales", {
  record <- dashboard_showcase_record(
    participants = 4L,
    days = 12L,
    epoch_seconds = 3600
  )
  source <- dataset_raw_data(record)
  cutoff <- min(source$Datetime) + 7 * 86400
  source <- source[
    !(
      source$Id %in% c("P01", "P04") &
        source$Datetime >= cutoff
    ),
    ,
    drop = FALSE
  ]
  record <- new_dataset_record(
    raw_data = source,
    display_name = "Unequal participant duration fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  snapshot <- dashboard_dataset_snapshot(record)
  selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    participants_per_page = 4L,
    time_basis = "participant",
    measurement_window = c(1L, 12L),
    view_mode = "detailed",
    time_page = 2L,
    days_per_page = 7L
  )
  preview <- dashboard_plot_preview(snapshot, selection)

  expect_identical(
    sort(unique(preview$data$Id)),
    c("P02", "P03")
  )
  expect_s3_class(preview$data[["Plot time"]], "POSIXct")

  plot <- plot_dashboard_timeline(
    snapshot,
    preview,
    scale = "symlog"
  )
  expect_identical(
    attr(plot, "llw_empty_facet_anchor_ids"),
    c("P01", "P04")
  )
  built <- ggplot2::ggplot_build(plot)
  expect_identical(nrow(built$layout$layout), 4L)
  panel_limits <- lapply(
    built$layout$panel_scales_x,
    function(scale) scale$get_limits()
  )
  expect_true(all(vapply(
    panel_limits,
    function(limits) isTRUE(all.equal(diff(limits), 7 * 86400)),
    logical(1L)
  )))
  expect_true(all(vapply(
    panel_limits,
    function(limits) {
      identical(
        format(
          as.POSIXct(limits[[1L]], origin = "1970-01-01", tz = "UTC"),
          "%H:%M:%S",
          tz = "UTC"
        ),
        "00:00:00"
      )
    },
    logical(1L)
  )))
})

test_that("plot height grows with visible participant slots", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 8L,
    days = 7L,
    epoch_seconds = 3600
  ))
  four_selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    participants_per_page = 4L,
    view_mode = "detailed"
  )
  eight_selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    participants_per_page = 8L,
    view_mode = "detailed"
  )
  four_preview <- dashboard_plot_preview(snapshot, four_selection)
  eight_preview <- dashboard_plot_preview(snapshot, eight_selection)

  expect_identical(dashboard_plot_height(four_preview), 620L)
  expect_identical(dashboard_plot_height(eight_preview), 960L)
  expect_gt(
    dashboard_plot_height(eight_preview),
    dashboard_plot_height(four_preview)
  )
})

test_that("symlog axes show zero, the linear threshold, and readable decades", {
  positive <- dashboard_symlog_axis(
    c(0, 0.2, 1, 20, 1200, 106840),
    threshold = 1
  )
  expect_identical(
    positive$breaks,
    c(0, 1, 10, 100, 1000, 10000, 100000)
  )
  expect_identical(
    positive$labels,
    c("0", "1", "10", "100", "1,000", "10,000", "100,000")
  )
  expect_equal(
    positive$minor_breaks[positive$minor_breaks > 0 &
      positive$minor_breaks < 1],
    seq(0.1, 0.9, by = 0.1)
  )
  expect_equal(
    positive$minor_breaks[positive$minor_breaks > 1 &
      positive$minor_breaks < 10],
    seq(1.9, 9.1, by = 0.9)
  )
  positive_transform <- LightLogR::symlog_trans(
    thr = 1,
    scale = 1
  )$transform
  central_spacing <- diff(positive_transform(c(
    0,
    positive$minor_breaks[positive$minor_breaks > 0 &
      positive$minor_breaks < 1],
    1
  )))
  outer_spacing <- diff(positive_transform(c(
    1,
    positive$minor_breaks[positive$minor_breaks > 1 &
      positive$minor_breaks < 10],
    10
  )))
  expect_equal(central_spacing, rep(0.1, 10L), tolerance = 1e-12)
  expect_gt(length(unique(round(outer_spacing, 8L))), 1L)

  signed <- dashboard_symlog_axis(
    c(-100000, -0.5, 0, 0.5, 100000),
    threshold = 1
  )
  expect_true(all(c(-1, 0, 1) %in% signed$breaks))
  expect_true(all(abs(signed$breaks[signed$breaks != 0]) %in% 10^(0:5)))
  expect_lte(length(signed$breaks), 11L)
  expect_true(any(startsWith(signed$labels, "\u2212")))

  sub_lux <- dashboard_symlog_axis(
    c(0, 0.001, 0.01, 1, 10000),
    threshold = 0.001
  )
  expect_true(all(c(0, 0.001, 0.01, 0.1, 1) %in% sub_lux$breaks))
  sub_lux_transform <- LightLogR::symlog_trans(
    thr = 0.001,
    scale = 0.001
  )$transform(c(0, 0.001, 0.01))
  expect_equal(
    diff(sub_lux_transform),
    c(0.001, 0.001),
    tolerance = 1e-12
  )
  expect_match(
    dashboard_scale_notice(
      structure(
        list(
          mode = "detailed",
          focus_unit = "lux",
          data = data.frame(Value = c(0, 1))
        ),
        class = "llw_dashboard_plot_preview"
      ),
      "symlog",
      symlog_threshold = 0.001
    ),
    "\u22120.001 to +0.001",
    fixed = TRUE
  )
  expect_match(
    dashboard_scale_notice(
      structure(
        list(
          mode = "detailed",
          focus_unit = "lux",
          data = data.frame(Value = c(0, 1))
        ),
        class = "llw_dashboard_plot_preview"
      ),
      "symlog",
      symlog_threshold = 0.001
    ),
    "equal axis spacing",
    fixed = TRUE
  )
})

test_that("dashboard showcase uses reviewed real-data records", {
  catalog <- dashboard_showcase_catalog()

  expect_true("sample" %in% catalog$key)
  expect_false("actlumus_synthetic" %in% catalog$key)
  reviewed_device_keys <- c(
    "actlumus_all",
    "speccy_all",
    "veet_02_als",
    "veet_02_pho"
  )
  development_root <- lightlogweb_development_root()
  if (dir.exists(file.path(development_root, "testdevices"))) {
    expect_true(all(reviewed_device_keys %in% catalog$key))
  }
  expect_match(catalog$label[catalog$key == "sample"], "testdata")
  if ("iztech" %in% catalog$key) {
    expect_match(catalog$label[catalog$key == "iztech"], "IZTECH")
  }
  if (all(reviewed_device_keys %in% catalog$key)) {
    expect_match(catalog$label[catalog$key == "actlumus_all"], "All ActLumus")
    expect_match(catalog$label[catalog$key == "speccy_all"], "All Speccy")
  }

  expected <- list(
    sample = list(rows = 69120L, participants = 2L, mode = "detailed"),
    iztech = list(rows = 151200L, participants = 17L, mode = "detailed"),
    actlumus_all = list(
      rows = 839607L,
      participants = 20L,
      mode = "availability"
    ),
    speccy_all = list(
      rows = 121060L,
      participants = 3L,
      mode = "availability"
    ),
    veet_02_als = list(rows = 173007L, participants = 1L, mode = "detailed"),
    veet_02_pho = list(rows = 173013L, participants = 1L, mode = "detailed")
  )
  loaded <- list()
  for (key in catalog$key) {
    record <- load_dataset_example(key)
    loaded[[key]] <- record
    snapshot <- dashboard_dataset_snapshot(record)
    selection <- dashboard_plot_selection(snapshot)
    recommendation <- dashboard_view_recommendation(snapshot)

    expect_identical(snapshot$raw_data, snapshot$prepared_data)
    expect_identical(nrow(snapshot$prepared_data), expected[[key]]$rows)
    expect_identical(
      length(snapshot$participants),
      expected[[key]]$participants
    )
    expect_identical(selection$mode, expected[[key]]$mode)
    expect_identical(
      recommendation$resolved_mode,
      expected[[key]]$mode
    )
    expect_identical(recommendation$time_basis, "participant")
    expect_identical(recommendation$view_mode, "auto")
    expect_identical(recommendation$days_per_page, 7L)
    expect_match(
      recommendation$summary,
      "full duration, up to 7 days/page",
      fixed = TRUE
    )
    expect_identical(snapshot$recipe$state, "empty_unchanged")
  }

  if ("speccy_all" %in% names(loaded)) {
    speccy <- loaded$speccy_all
    expect_setequal(
      unique(as.character(dataset_raw_data(speccy)$Id)),
      c("ID01", "ID02", "ID04")
    )
    expect_identical(
      speccy$source_manifest$details$development_example$participant_id_mapping$mapping_source,
      rep("extract", 8L)
    )
  }

  expect_s3_class(dataset_dashboard_app(), "shiny.appobj")
})

test_that("active-data tables declare the scalable server-side contract", {
  contract <- dashboard_table_contract()
  expect_true(contract$server)
  expect_true(contract$search)
  expect_true(contract$sort)
  expect_true(contract$column_visibility)
  expect_identical(contract$page_length, 10L)

  data <- data.frame(
    Id = c("P01", "P02"),
    Datetime = as.POSIXct(
      c("2026-01-01 00:00:00", "2026-01-01 00:01:00"),
      tz = "UTC"
    ),
    MEDI = c(1.23456789, 2.34567891),
    Missing = c(1L, 0L),
    Included = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  options <- dashboard_table_options(data)
  expect_true(options$processing)
  expect_true(options$deferRender)
  expect_true(options$searching)
  expect_true(options$ordering)
  expect_true(options$paging)
  expect_identical(options$pageLength, 10L)
  expect_identical(options$buttons[[1L]]$extend, "colvis")
  expect_null(options$lengthMenu)

  widget <- dashboard_datatable(data, "Typed test table")
  expect_s3_class(widget, "datatables")
  expect_identical(widget$x$filter, "none")
  expect_identical(widget$x$extensions[[1L]], "Buttons")
  expect_identical(widget$x$options$dom, "Bftip")

  compact <- dashboard_datatable(
    data,
    "Compact test table",
    column_visibility = FALSE
  )
  expect_identical(compact$x$options$dom, "ftip")
  expect_null(compact$x$extensions)

  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record())
  expect_identical(
    dashboard_main_columns(snapshot, "MEDI", "preprocessed"),
    c("Id", "Datetime", "MEDI")
  )
})

test_that("dashboard UI distinguishes pre-processed and source views", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html

  expect_match(html, "dashboard-date_window_ui", fixed = TRUE)
  expect_match(html, "Plot all participants", fixed = TRUE)
  expect_match(html, "dashboard-time_basis", fixed = TRUE)
  expect_match(html, "dashboard-view_recommendation", fixed = TRUE)
  expect_match(html, "Days from first measurement", fixed = TRUE)
  expect_match(html, "dashboard-view_mode", fixed = TRUE)
  expect_match(html, "Coverage overview", fixed = TRUE)
  expect_match(html, "dashboard-days_per_page", fixed = TRUE)
  expect_match(html, "Participant pages move up/down", fixed = TRUE)
  expect_match(html, "dashboard-participant_pagination", fixed = TRUE)
  expect_match(html, "dashboard-facet_pagination", fixed = TRUE)
  expect_match(html, "dashboard-focus_variable", fixed = TRUE)
  expect_match(html, "dashboard-data_stage", fixed = TRUE)
  expect_false(grepl(
    "dashboard-symlog_threshold_control",
    html,
    fixed = TRUE
  ))
  expect_match(html, "llw-dashboard-data-view--preprocessed", fixed = TRUE)
  expect_match(html, "llw-dashboard-data-view--source", fixed = TRUE)
  expect_match(html, "Pre-processed data", fixed = TRUE)
  expect_match(html, "Source data", fixed = TRUE)
  expect_match(html, "compact or full column view", fixed = TRUE)
  expect_match(html, "Missing values and exact measured zeros", fixed = TRUE)
  expect_match(html, "Source import checks", fixed = TRUE)
  expect_false(grepl(
    "Day of month (calendar months)",
    html,
    fixed = TRUE
  ))
  expect_false(grepl(
    "Day of year (calendar years)",
    html,
    fixed = TRUE
  ))

  quality_box <- htmltools::renderTags(dashboard_value_box(
    "Implicit gap epochs",
    "0",
    "arrows-alt",
    "No absent expected timestamps were detected.",
    tone = "success",
    outcome = "None detected"
  ))$html
  expect_match(
    quality_box,
    "llw-dashboard-value-box__icon",
    fixed = TRUE
  )
  expect_match(quality_box, "None detected", fixed = TRUE)
})

test_that("calendar controls refit to the selected participants", {
  participant_rows <- function(id, start) {
    data.frame(
      Id = id,
      Datetime = seq(
        as.POSIXct(paste(start, "00:00:00"), tz = "UTC"),
        by = "1 hour",
        length.out = 72L
      ),
      MEDI = rep(10, 72L),
      stringsAsFactors = FALSE
    )
  }
  record <- new_dataset_record(
    raw_data = rbind(
      participant_rows("P01", "2026-01-05"),
      participant_rows("P02", "2026-06-15")
    ),
    display_name = "Participant scope fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  dataset <- shiny::reactiveVal(record)

  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    {
      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "preprocessed",
        participants = "P02",
        show_all_participants = FALSE,
        time_basis = "calendar",
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1"
      )
      session$flushReact()
      selected_ui <- output$date_window_ui
      selected_html <- if (
        is.list(selected_ui) && !is.null(selected_ui$html)
      ) {
        selected_ui$html
      } else {
        paste(as.character(selected_ui), collapse = "")
      }
      expect_match(selected_html, "2026-06-15", fixed = TRUE)
      expect_match(selected_html, "2026-06-17", fixed = TRUE)

      session$setInputs(participants = c("P01", "P02"))
      session$flushReact()
      combined_ui <- output$date_window_ui
      combined_html <- if (
        is.list(combined_ui) && !is.null(combined_ui$html)
      ) {
        combined_ui$html
      } else {
        paste(as.character(combined_ui), collapse = "")
      }
      expect_match(combined_html, "2026-01-05", fixed = TRUE)
      expect_match(combined_html, "2026-06-17", fixed = TRUE)
    }
  )
})

test_that("dashboard module reports bounded reactive state", {
  dataset <- shiny::reactiveVal(dashboard_showcase_record())
  panel <- shiny::reactiveVal("dashboard")
  mode <- shiny::reactiveVal("light")
  recording_session <- shiny::MockShinySession$new()
  recording_session$userData$input_messages <- list()
  recording_session$sendInputMessage <- function(inputId, message) {
    recording_session$userData$input_messages <- c(
      recording_session$userData$input_messages,
      list(list(input_id = inputId, message = message))
    )
    invisible()
  }
  messages_for <- function(messages, target) {
    matches <- vapply(
      messages,
      function(message) {
        identical(message$input_id, target) ||
          endsWith(message$input_id, paste0("-", target))
      },
      logical(1)
    )
    messages[matches]
  }

  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = panel,
      color_mode = mode
    ),
    session = recording_session,
    {
      session$setInputs(
        focus_variable = "",
        data_stage = "preprocessed",
        participants = c("P01", "P02"),
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        date_window = as.Date(c("2026-01-01", "2026-01-02")),
        facet_page = "1"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_identical(returned$status()$focus_variable, "MEDI")
      expect_s3_class(
        returned$recommendation(),
        "llw_dashboard_recommendation"
      )
      expect_true(returned$status()$recommendation_active)
      expect_match(
        returned$status()$recommendation_summary,
        "Own recording dates",
        fixed = TRUE
      )
      recommendation_ui <- output$view_recommendation
      recommendation_html <- if (
        is.list(recommendation_ui) && !is.null(recommendation_ui$html)
      ) {
        recommendation_ui$html
      } else {
        paste(as.character(recommendation_ui), collapse = "")
      }
      expect_match(
        recommendation_html,
        "Recommended view",
        fixed = TRUE
      )
      expect_match(recommendation_html, "disabled", fixed = TRUE)
      initial_targets <- list(
        data_stage = "preprocessed",
        show_all_participants = FALSE,
        time_basis = "participant",
        view_mode = "auto",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1"
      )
      for (target in names(initial_targets)) {
        matching <- messages_for(
          recording_session$userData$input_messages,
          target
        )
        expect_identical(length(matching), 1L, info = target)
        if (length(matching) == 1L) {
          expect_identical(
            matching[[1L]][["message"]][["value"]],
            initial_targets[[target]],
            info = target
          )
        }
      }

      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "preprocessed",
        plot_scale = "symlog",
        symlog_threshold = "0.1",
        participants = c("P01", "P02"),
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        date_window = as.Date(c("2026-01-01", "2026-01-02")),
        facet_page = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()
      expect_true(returned$status()$ready)
      expect_identical(returned$status()$plot_mode, "detailed")
      expect_identical(returned$status()$time_basis, "participant")
      expect_identical(returned$status()$view_mode, "auto")
      expect_identical(returned$status()$participant_pages, 1L)
      expect_identical(returned$status()$time_pages, 1L)
      expect_identical(returned$status()$focus_variable, "MEDI")
      expect_identical(returned$status()$data_stage, "preprocessed")
      expect_identical(returned$status()$plot_scale, "symlog")
      expect_identical(returned$status()$symlog_threshold, 0.1)
      expect_false(returned$status()$recommendation_active)
      expect_identical(
        returned$status()$recommendation_differences,
        "symlog linear range"
      )
      adjusted_ui <- output$view_recommendation
      adjusted_html <- if (
        is.list(adjusted_ui) && !is.null(adjusted_ui$html)
      ) {
        adjusted_ui$html
      } else {
        paste(as.character(adjusted_ui), collapse = "")
      }
      expect_match(
        adjusted_html,
        "Current differences: symlog linear range",
        fixed = TRUE
      )
      expect_match(
        adjusted_html,
        "Recommended view",
        fixed = TRUE
      )
      expect_match(
        adjusted_html,
        "llw-recommended-view-action--available",
        fixed = TRUE
      )
      recording_session$userData$input_messages <- list()
      session$setInputs(apply_recommendation = 1)
      session$flushReact()
      reset_messages <- recording_session$userData$input_messages
      reset_targets <- c(
        time_basis = "participant",
        view_mode = "auto",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1"
      )
      for (target in names(reset_targets)) {
        matching <- messages_for(
          reset_messages,
          target
        )
        expect_identical(length(matching), 1L, info = target)
        if (length(matching) == 1L) {
          expect_identical(
            matching[[1L]][["message"]][["value"]],
            unname(reset_targets[[target]]),
            info = target
          )
        }
      }
      expect_true(returned$status()$recommendation_active)
      expect_identical(returned$status()$symlog_threshold, 1)
      expect_identical(returned$status()$recipe_state, "empty_unchanged")
      expect_identical(returned$status()$grouping_state, "empty")
      expect_lte(returned$status()$displayed_plot_rows, 12000L)
      expect_identical(output$raw_rows, "384")
      expect_identical(output$prepared_rows, "384")

      dataset(dashboard_showcase_record(
        participants = 10L,
        days = 30L,
        epoch_seconds = 3600
      ))
      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "source",
        participants = character(),
        show_all_participants = TRUE,
        time_basis = "participant",
        measurement_window = c(1L, 30L),
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        date_window = as.Date(c("2026-01-01", "2026-01-30")),
        facet_page = "1"
      )
      session$flushReact()
      expect_identical(returned$status()$plot_mode, "availability")
      expect_identical(returned$status()$data_stage, "source")
      expect_identical(returned$status()$facet_pages, 3L)
      expect_identical(returned$status()$participant_pages, 3L)
      expect_identical(returned$status()$time_pages, 5L)
      expect_lte(returned$status()$displayed_plot_rows, 2000L)
      pagination <- output$facet_pagination
      pagination_html <- if (
        is.list(pagination) && !is.null(pagination$html)
      ) {
        pagination$html
      } else {
        paste(as.character(pagination), collapse = "")
      }
      expect_false(grepl(
        "dashboard-participant_page",
        pagination_html,
        fixed = TRUE
      ))
      expect_match(pagination_html, "Time page", fixed = TRUE)
      expect_match(pagination_html, "Previous time page", fixed = TRUE)
      expect_match(pagination_html, "Next time page", fixed = TRUE)
      expect_match(pagination_html, "fa-calendar", fixed = TRUE)
      expect_match(pagination_html, "disabled", fixed = TRUE)
      participant_pagination <- output$participant_pagination
      participant_pagination_html <- if (
        is.list(participant_pagination) &&
          !is.null(participant_pagination$html)
      ) {
        participant_pagination$html
      } else {
        paste(as.character(participant_pagination), collapse = "")
      }
      expect_match(
        participant_pagination_html,
        "Participant page",
        fixed = TRUE
      )
      expect_match(
        participant_pagination_html,
        "llw-dashboard-page-option__icon",
        fixed = TRUE
      )
      expect_false(grepl("\U0001f4c4", participant_pagination_html, fixed = TRUE))
      participant_previous <- output$participant_previous_control
      participant_previous_html <- if (
        is.list(participant_previous) &&
          !is.null(participant_previous$html)
      ) {
        participant_previous$html
      } else {
        paste(as.character(participant_previous), collapse = "")
      }
      expect_match(
        participant_previous_html,
        "Show the previous participant page",
        fixed = TRUE
      )
      expect_match(participant_previous_html, "disabled", fixed = TRUE)
    }
  )
})
